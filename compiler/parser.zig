const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Ast = @import("ast.zig");
const Token = tokenizer.Token;
const Tokenizer = tokenizer.Tokenizer;

pub const Parser = struct {
    arena: std.heap.ArenaAllocator,
    tokenizer: Tokenizer,
    source: []const u8,
    current: ?Token,
    previous: ?Token,
    had_error: bool,
    panicking: bool,

    const Error = error{ ParseError, OutOfMemory };

    const Precedence = enum(u8) {
        lowest = 0,
        assignment = 1, // = += -= *= /=
        logic_or = 2, // or
        logic_and = 3, // and
        comparison = 4, // == != < > <= >=
        bitwise = 5, // & | ^
        shift = 6, // << >>
        term = 7, // + -
        factor = 8, // * / %
        unary = 9, // - ! ~ * & (address of)
        call = 10, // . () []
        primary = 11,

        pub fn next(prec: Precedence) Precedence {
            return @enumFromInt(@intFromEnum(prec) + 1);
        }

        pub fn greater(prec1: Precedence, prec2: Precedence) bool {
            return @intFromEnum(prec1) > @intFromEnum(prec2);
        }

        pub fn less(prec1: Precedence, prec2: Precedence) bool {
            return @intFromEnum(prec1) < @intFromEnum(prec2);
        }

        pub fn fromTag(tag: Token.Tag) Precedence {
            return switch (tag) {
                .equal, .plus_equal, .minus_equal, .asterisk_equal, .slash_equal => .assignment,

                .@"or" => .logic_or,
                .@"and" => .logic_and,

                .double_equal, .bang_equal, .less, .less_equal, .greater, .greater_equal => .comparison,

                .ampersand, .pipe, .caret => .bitwise,

                .shift_left, .shift_right => .shift,

                .plus, .minus => .term,

                .asterisk, .slash, .percent => .factor,

                .l_paren, .l_bracket, .l_brace, .dot => .call,

                else => .lowest,
            };
        }
    };

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        var parser: Parser = .{
            .arena = .init(allocator),
            .tokenizer = .init(source),
            .source = source,
            .current = null,
            .previous = null,
            .had_error = false,
            .panicking = false,
        };

        try parser.advance();
        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
    }

    pub fn parseProgram(self: *Parser) !Ast.Program {
        const allocator = self.arena.allocator();

        var decls: std.ArrayList(*Ast.Declaration) = .empty;

        while (!self.check(&.{.eof})) {
            const decl = self.parseDeclaration() catch |err| switch (err) {
                error.ParseError => {
                    try self.synchronize();
                    continue;
                },
                else => return err,
            };

            try decls.append(allocator, decl);
        }

        return .{ .decls = try decls.toOwnedSlice(allocator) };
    }

    fn parseDeclaration(self: *Parser) Parser.Error!*Ast.Declaration {
        const pub_token = if (try self.match(&.{.@"pub"})) self.previous else null;
        const extern_export_token = if (try self.match(&.{.@"export"}) or try self.match(&.{.@"extern"})) self.previous else null;

        var link_section_token: ?Token = null;
        if (try self.match(&.{.@"linksection"})) {
            try self.consume(.l_paren, "Expected '(' after 'linksection'.");
            link_section_token = try self.consumeReturn(.string_literal, "Expected string literal for linksection.");
            try self.consume(.r_paren, "Expected ')' after linksection string.");
        }

        if (!try self.match(&.{.@"var"}) and !try self.match(&.{.@"const"})) {
            return self.parseError("Expected 'var' or 'const'.");
        }
        const const_var_token = self.previous.?;

        if (!try self.match(&.{.identifier})) return self.parseError("Expected name identifier.");
        const decl_name = self.previous.?;

        const type_expr = if (try self.match(&.{.colon})) try self.parseTypeExpr() else null;

        var align_expr: ?*Ast.Expression = null;
        if (try self.match(&.{.@"align"})) {
            try self.consume(.l_paren, "Expected '(' after 'align'.");
            align_expr = try self.parseExpression();
            try self.consume(.r_paren, "Expected ')' after alignment expression.");
        }

        const init_expr = if (try self.match(&.{.equal})) try self.parseExpression() else null;

        try self.consume(.semicolon, "Expected ';' at end of declaration.");

        const decl = try self.create(Ast.Declaration, .{
            .pub_token = pub_token,
            .extern_export_token = extern_export_token,
            .link_section_token = link_section_token,
            .const_var_token = const_var_token,
            .name = decl_name,
            .type_expr = type_expr,
            .align_expr = align_expr,
            .init_expr = init_expr,
        });

        return decl;
    }

    fn parseTypeExpr(self: *Parser) Parser.Error!*Ast.TypeExpr {
        const allocator = self.arena.allocator();
        var prefixes: std.ArrayList(Ast.TypePrefix) = .empty;

        while (true) {
            if (try self.match(&.{.@"align"})) {
                try self.consume(.l_paren, "Expected '(' after 'align'.");
                const align_expr = try self.parseExpression();
                try self.consume(.r_paren, "Expected ')' after alignment expression.");

                try prefixes.append(allocator, .{ .align_prefix = align_expr });
            } else if (try self.match(&.{.asterisk})) {
                const token = self.previous.?;
                const allow_zero = if (try self.match(&.{.@"allowzero"})) self.previous.? else null;
                try prefixes.append(allocator, .{
                    .pointer = .{
                        .is_multi = false,
                        .is_volatile = false,
                        .allow_zero_token = allow_zero,
                        .token = token,
                    },
                });
            } else if (try self.match(&.{.tilde})) {
                const token = self.previous.?;
                const allow_zero = if (try self.match(&.{.@"allowzero"})) self.previous.? else null;
                try prefixes.append(allocator, .{
                    .pointer = .{
                        .is_multi = false,
                        .is_volatile = true,
                        .allow_zero_token = allow_zero,
                        .token = token,
                    },
                });
            } else if (try self.match(&.{.question_mark})) {
                try prefixes.append(allocator, .{ .optional = self.previous.? });
            } else if (try self.match(&.{.bang})) {
                try prefixes.append(allocator, .{ .error_union = self.previous.? });
            } else if (try self.match(&.{.l_bracket})) {
                if (try self.match(&.{.asterisk})) {
                    const token = self.previous.?;
                    try self.consume(.r_bracket, "Expected ']' after '[*'.");
                    const allow_zero = if (try self.match(&.{.@"allowzero"})) self.previous.? else null;
                    try prefixes.append(allocator, .{ .pointer = .{
                        .is_multi = true,
                        .is_volatile = false,
                        .allow_zero_token = allow_zero,
                        .token = token,
                    } });
                } else if (try self.match(&.{.tilde})) {
                    const token = self.previous.?;
                    try self.consume(.r_bracket, "Expected ']' after '[~'.");
                    const allow_zero = if (try self.match(&.{.@"allowzero"})) self.previous.? else null;
                    try prefixes.append(allocator, .{
                        .pointer = .{
                            .is_multi = true,
                            .is_volatile = true,
                            .allow_zero_token = allow_zero,
                            .token = token,
                        },
                    });
                } else {
                    // If we are here then the type started with '[', which means it's an array size we're parsing
                    const size_expr = try self.parseExpression();
                    try self.consume(.r_bracket, "Expected ']' after array size'.");

                    const array_node = try self.create(Ast.ArrayType, .{
                        .size = size_expr,
                        .child_type = try self.parseTypeExpr(),
                    });

                    return self.create(Ast.TypeExpr, .{
                        .prefixes = try prefixes.toOwnedSlice(allocator),
                        .core = .{ .array = array_node },
                    });
                }
            } else {
                break;
            }
        }

        const core: Ast.TypeCore = if (try self.match(&.{.identifier}))
            .{ .identifier = self.previous.? }
        else if (try self.match(&.{.@"struct"}))
            .{ .struct_def = try self.parseStruct() }
        else if (try self.match(&.{.@"enum"}))
            .{ .enum_def = try self.parseEnum() }
        else if (try self.match(&.{.@"union"}))
            .{ .union_def = try self.parseUnion() }
        else if (try self.match(&.{.@"fn"}))
            .{ .fn_type = try self.parseFnType() }
        else
            return self.parseError("Expected type.");

        return try self.create(Ast.TypeExpr, .{
            .prefixes = try prefixes.toOwnedSlice(allocator),
            .core = core,
        });
    }

    fn parseFnType(self: *Parser) !*Ast.FunctionType {
        const allocator = self.arena.allocator();
        try self.consume(.l_paren, "Expected '(' after 'fn' in type.");

        var params: std.ArrayList(*Ast.Param) = .empty;

        if (!self.check(&.{.r_paren})) {
            while (true) {
                // In function types, parameter names are optional in some langs, but required in ours per grammar?
                // Grammar says: param = identifier , ":" , type_expr;
                // So names are required.
                const name = try self.consumeReturn(.identifier, "Expected parameter name.");
                try self.consume(.colon, "Expected ':' after paramater name.");
                const type_expr = try self.parseTypeExpr();

                const param = try self.create(Ast.Param, .{
                    .name = name,
                    .type_expr = type_expr,
                });
                try params.append(allocator, param);

                if (!try self.match(&.{.comma})) break;
            }
        }
        try self.consume(.r_paren, "Expected ')' after parameters.");

        const return_type = try self.parseTypeExpr();

        return self.create(Ast.FunctionType, .{
            .params = try params.toOwnedSlice(allocator),
            .return_type = return_type,
        });
    }

    fn parseUnion(self: *Parser) !*Ast.UnionDef {
        const allocator = self.arena.allocator();
        const keyword = self.previous.?;

        var layout: Ast.StructLayout = .auto;
        if (try self.match(&.{.dot})) {
            if (try self.match(&.{.c_abi})) {
                layout = .c_abi;
            } else {
                return self.parseError("Expected 'c_abi' after 'union.'.");
            }
        }

        try self.consume(.l_brace, "Expected '{' before union body.");

        var members: std.ArrayList(*Ast.ContainerDecl) = .empty;
        while (!self.check(&.{ .r_brace, .eof })) {
            if (self.check(&.{ .@"pub", .@"const", .@"var", .@"extern", .@"export" })) {
                const decl = try self.parseDeclaration();
                const container_node = try self.create(Ast.ContainerDecl, .{ .declaration = decl });
                try members.append(allocator, container_node);
            } else {
                const name = try self.consumeReturn(.identifier, "Expected field name or declaration.");
                try self.consume(.colon, "Expected ':' after field name.");
                const type_expr = try self.parseTypeExpr();

                // Commas are required
                if (!try self.match(&.{.comma})) return self.parseError("Expected comma after field declaration.");

                const field = try self.create(Ast.FieldDecl, .{
                    .name = name,
                    .type_expr = type_expr,
                });

                const container_node = try self.create(Ast.ContainerDecl, .{ .field = field });
                try members.append(allocator, container_node);
            }
        }

        try self.consume(.r_brace, "Expected '}' after union body.");

        return try self.create(Ast.UnionDef, .{
            .keyword = keyword,
            .layout = layout,
            .members = try members.toOwnedSlice(allocator),
        });
    }

    fn parseExpression(self: *Parser) !*Ast.Expression {
        return try self.parsePrecedence(.lowest);
    }

    fn parsePrecedence(self: *Parser, curr_prec: Precedence) Parser.Error!*Ast.Expression {
        var left = try self.parsePrefix();

        while (true) {
            const next_prec: Precedence = .fromTag(self.current.?.tag);

            if (next_prec == .lowest or next_prec.less(curr_prec)) break;

            try self.advance();
            const operator = self.previous.?;

            left = try self.parseInfix(left, operator);
        }

        return left;
    }

    fn parsePrefix(self: *Parser) !*Ast.Expression {
        // Unary operators
        if (try self.match(&.{ .minus, .not, .bang, .ampersand })) {
            const op = self.previous.?;
            const right = try self.parsePrecedence(.unary);

            const unary_node = try self.create(Ast.UnaryExpr, .{
                .op = op,
                .operand = right,
            });

            return self.create(Ast.Expression, .{ .unary = unary_node });
        }

        // Pointer Type (*T, ~T)
        if (self.check(&.{ .asterisk, .tilde })) {
            const type_expr = try self.parseTypeExpr();
            const primary = try self.create(Ast.PrimaryExpr, .{ .type_expr = type_expr });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        // Array/Slice Type ([]T, [N]T, [*]T, [~]T)
        // parseTypeExpr handles l_bracket start.
        if (self.check(&.{.l_bracket})) {
            const type_expr = try self.parseTypeExpr();
            const primary = try self.create(Ast.PrimaryExpr, .{ .type_expr = type_expr });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        // Grouping
        if (try self.match(&.{.l_paren})) {
            const expr = try self.parseExpression();
            try self.consume(.r_paren, "Expected ')' after expression.");
            const primary = try self.create(Ast.PrimaryExpr, .{ .grouped = expr });

            return self.create(Ast.Expression, .{ .primary = primary });
        }

        // Struct Init (.{}) OR Enum Literal (.Identifier)
        if (try self.match(&.{.dot})) {
            if (try self.match(&.{.l_brace})) {
                // Struct Init (Anonymous)
                const fields = try self.parseFieldInits();
                const struct_init = try self.create(Ast.StructInit, .{
                    .type_expr = null,
                    .fields = fields,
                });
                const primary = try self.create(Ast.PrimaryExpr, .{ .struct_init = struct_init });
                return self.create(Ast.Expression, .{ .primary = primary });
            } else if (try self.match(&.{.identifier})) {
                // Enum Literal
                const primary = try self.create(Ast.PrimaryExpr, .{ .enum_literal = self.previous.? });
                return self.create(Ast.Expression, .{ .primary = primary });
            }
            return self.parseError("Expected '{' or identifier after '.'");
        }

        // Primary expressions
        if (try self.match(&.{.int_literal})) {
            const primary = try self.create(Ast.PrimaryExpr, .{ .int_literal = self.previous.? });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        if (try self.match(&.{.string_literal})) {
            const primary = try self.create(Ast.PrimaryExpr, .{ .string_literal = self.previous.? });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        if (try self.match(&.{.multiline_string_literal})) {
            const allocator = self.arena.allocator();
            var lines: std.ArrayList(Token) = .empty;
            try lines.append(allocator, self.previous.?);

            while (try self.match(&.{.multiline_string_literal})) {
                try lines.append(allocator, self.previous.?);
            }

            const primary = try self.create(Ast.PrimaryExpr, .{ .multiline_string_literal = try lines.toOwnedSlice(allocator) });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        if (try self.match(&.{.identifier})) {
            const primary = try self.create(Ast.PrimaryExpr, .{ .identifier = self.previous.? });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        if (try self.match(&.{ .true, .false })) {
            const primary = try self.create(Ast.PrimaryExpr, .{ .bool_literal = self.previous.? });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        if (try self.match(&.{.null})) {
            const primary = try self.create(Ast.PrimaryExpr, .{ .null_literal = self.previous.? });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        if (try self.match(&.{.undefined})) {
            const primary = try self.create(Ast.PrimaryExpr, .{ .undefined_literal = self.previous.? });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        // Function Literals OR Function Types
        if (try self.match(&.{.@"fn"})) {
            const allocator = self.arena.allocator();
            var attribute: ?Token = null;
            if (try self.match(&.{.dot})) {
                if (self.check(&.{ .naked, .interrupt })) {
                    try self.advance();
                    attribute = self.previous;
                } else {
                    return self.parseError("Expected function attribute (naked/interrupt) after 'fn.'.");
                }
            }
            try self.consume(.l_paren, "Expected '(' after 'fn'.");

            var params: std.ArrayList(*Ast.Param) = .empty;

            if (!self.check(&.{.r_paren})) {
                while (true) {
                    const name = try self.consumeReturn(.identifier, "Expected parameter name.");
                    try self.consume(.colon, "Expected ':' after paramater name.");
                    const type_expr = try self.parseTypeExpr();

                    const param = try self.create(Ast.Param, .{
                        .name = name,
                        .type_expr = type_expr,
                    });
                    try params.append(allocator, param);

                    if (!try self.match(&.{.comma})) break;
                }
            }
            try self.consume(.r_paren, "Expected ')' after parameters.");

            const return_type = try self.parseTypeExpr();

            if (self.check(&.{.l_brace})) {
                // It is a Function Literal with a Body
                const body = try self.parseBlock();
                const fn_literal = try self.create(Ast.FunctionLiteral, .{
                    .attribute = attribute,
                    .params = try params.toOwnedSlice(allocator),
                    .return_type = return_type,
                    .body = body,
                });
                const primary = try self.create(Ast.PrimaryExpr, .{ .fn_literal = fn_literal });
                return self.create(Ast.Expression, .{ .primary = primary });
            } else {
                // It is a Function Type
                if (attribute != null) return self.parseError("Function types cannot have attributes like .naked yet.");

                const fn_type = try self.create(Ast.FunctionType, .{
                    .params = try params.toOwnedSlice(allocator),
                    .return_type = return_type,
                });

                // Wrap in TypeExpr -> PrimaryExpr
                const type_expr = try self.create(Ast.TypeExpr, .{
                    .prefixes = &.{}, // No prefixes for bare fn type
                    .core = .{ .fn_type = fn_type },
                });

                const primary = try self.create(Ast.PrimaryExpr, .{ .type_expr = type_expr });
                return self.create(Ast.Expression, .{ .primary = primary });
            }
        }

        // Structs, Enums, Unions as Expressions (Types)
        if (self.check(&.{ .@"struct", .@"enum", .@"union", .@"extern" })) {
            const type_expr = try self.parseTypeExpr();
            const primary = try self.create(Ast.PrimaryExpr, .{ .type_expr = type_expr });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        // Asm Block
        if (try self.match(&.{.@"asm"})) {
            const asm_node = try self.parseAsmBlock();
            const primary = try self.create(Ast.PrimaryExpr, .{ .asm_block = asm_node });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        // Builtin Call
        if (try self.match(&.{.at_sign})) {
            const builtin = try self.parseBuiltinCall();
            const primary = try self.create(Ast.PrimaryExpr, .{ .builtin_call = builtin });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        return self.parseError("Expected expression.");
    }

    fn parseInfix(self: *Parser, left: *Ast.Expression, op: Token) !*Ast.Expression {
        const allocator = self.arena.allocator();

        switch (op.tag) {
            // Function call
            .l_paren => {
                var args: std.ArrayList(*Ast.Expression) = .empty;
                if (!self.check(&.{.r_paren})) {
                    while (true) {
                        try args.append(allocator, try self.parseExpression());
                        if (!try self.match(&.{.comma})) break;
                    }
                }

                try self.consume(.r_paren, "Expected ')' after arguments.");

                const call_node = try self.create(Ast.CallExpr, .{
                    .callee = left,
                    .args = try args.toOwnedSlice(allocator),
                });

                return self.create(Ast.Expression, .{ .call = call_node });
            },
            // Array indexing
            .l_bracket => {
                const index = try self.parseExpression();
                try self.consume(.r_bracket, "Expected ']' after index.");

                const index_node = try self.create(Ast.IndexExpr, .{
                    .target = left,
                    .index = index,
                });

                return self.create(Ast.Expression, .{ .index = index_node });
            },
            // Struct Initialization with Type (T { ... })
            .l_brace => {
                const fields = try self.parseFieldInits();

                const struct_init = try self.create(Ast.StructInit, .{
                    .type_expr = left,
                    .fields = fields,
                });

                const primary = try self.create(Ast.PrimaryExpr, .{ .struct_init = struct_init });
                return self.create(Ast.Expression, .{ .primary = primary });
            },
            // Dot access (foo.bar, foo.*, foo.!, foo.?)
            .dot => {
                if (try self.match(&.{.asterisk})) {
                    const unary_node = try self.create(Ast.UnaryExpr, .{
                        .op = self.previous.?,
                        .operand = left,
                    });

                    return self.create(Ast.Expression, .{ .dereference = unary_node });
                }

                if (try self.match(&.{.question_mark})) {
                    const unary_node = try self.create(Ast.UnaryExpr, .{
                        .op = self.previous.?,
                        .operand = left,
                    });

                    return self.create(Ast.Expression, .{ .unwrap_optional = unary_node });
                }

                if (try self.match(&.{.bang})) {
                    const unary_node = try self.create(Ast.UnaryExpr, .{
                        .op = self.previous.?,
                        .operand = left,
                    });

                    return self.create(Ast.Expression, .{ .unwrap_error = unary_node });
                }

                // If we're still here, it means we're doing a field access
                try self.consume(.identifier, "Expected field name after '.'.");

                const field_node = try self.create(Ast.FieldAccessExpr, .{
                    .target = left,
                    .field_name = self.previous.?,
                });

                return self.create(Ast.Expression, .{ .field_access = field_node });
            },
            else => {
                // if we're here it means this is a binary operator
                const curr_prec: Precedence = .fromTag(op.tag);

                const right = try self.parsePrecedence(curr_prec.next());

                const binary_node = try self.create(Ast.BinaryExpr, .{
                    .lhs = left,
                    .rhs = right,
                    .op = op,
                });

                return try self.create(Ast.Expression, .{ .binary = binary_node });
            },
        }
    }

    fn parseStruct(self: *Parser) !*Ast.StructDef {
        const allocator = self.arena.allocator();
        const keyword = self.previous.?;

        var layout: Ast.StructLayout = .auto;

        if (try self.match(&.{.dot})) {
            if (try self.match(&.{.@"packed"})) {
                layout = .@"packed";
            } else if (try self.match(&.{.c_abi})) {
                layout = .c_abi;
            } else {
                return self.parseError("Expected 'packed' or 'c_abi' after 'struct.'.");
            }
        }

        try self.consume(.l_brace, "Expected '{' before struct body.");

        var members: std.ArrayList(*Ast.ContainerDecl) = .empty;
        while (!self.check(&.{ .r_brace, .eof })) {
            // Check for associated declaration, if has to be a field
            if (self.check(&.{ .@"pub", .@"const", .@"var", .@"extern", .@"export" })) {
                const decl = try self.parseDeclaration();

                const container_node = try self.create(Ast.ContainerDecl, .{ .declaration = decl });
                try members.append(allocator, container_node);
            } else {
                const name = try self.consumeReturn(.identifier, "Expected field name or declaration.");

                try self.consume(.colon, "Expected ':' after field name.");

                const type_expr = try self.parseTypeExpr();

                if (!try self.match(&.{.comma})) return self.parseError("Expected comma after field declaration.");

                const field = try self.create(Ast.FieldDecl, .{
                    .name = name,
                    .type_expr = type_expr,
                });

                const container_node = try self.create(Ast.ContainerDecl, .{ .field = field });
                try members.append(allocator, container_node);
            }
        }

        try self.consume(.r_brace, "Expected '}' after struct body.");

        return try self.create(Ast.StructDef, .{
            .keyword = keyword,
            .layout = layout,
            .members = try members.toOwnedSlice(allocator),
        });
    }

    fn parseFnLiteral(self: *Parser) !*Ast.FunctionLiteral {
        const allocator = self.arena.allocator();

        var attribute: ?Token = null;
        if (try self.match(&.{.dot})) {
            if (self.check(&.{ .naked, .interrupt })) {
                try self.advance();
                attribute = self.previous;
            } else {
                return self.parseError("Expected function attribute (naked/interrupt) after 'fn.'.");
            }
        }
        try self.consume(.l_paren, "Expected '(' after 'fn'.");

        var params: std.ArrayList(*Ast.Param) = .empty;

        if (!self.check(&.{.r_paren})) {
            while (true) {
                const name = try self.consumeReturn(.identifier, "Expected parameter name.");
                try self.consume(.colon, "Expected ':' after paramater name.");
                const type_expr = try self.parseTypeExpr();

                const param = try self.create(Ast.Param, .{
                    .name = name,
                    .type_expr = type_expr,
                });
                try params.append(allocator, param);

                if (!try self.match(&.{.comma})) break;
            }
        }
        try self.consume(.r_paren, "Expected ')' after parameters.");

        const return_type = try self.parseTypeExpr();

        const body = try self.parseBlock();

        return self.create(Ast.FunctionLiteral, .{
            .attribute = attribute,
            .params = try params.toOwnedSlice(allocator),
            .return_type = return_type,
            .body = body,
        });
    }

    fn parseAsmBlock(self: *Parser) !*Ast.AsmBlock {
        const allocator = self.arena.allocator();
        const keyword = self.previous.?;
        var is_pure = false;

        if (try self.match(&.{.dot})) {
            const attr = try self.consumeReturn(.identifier, "Expected 'pure' after 'asm.'.");
            if (!std.mem.eql(u8, attr.lexeme(self.source), "pure")) {
                return self.parseError("Expected 'pure' attribute.");
            }
            is_pure = true;
        }

        try self.consume(.l_brace, "Expected '{' after asm.");

        var template_tokens: std.ArrayList(Token) = .empty;

        if (try self.match(&.{.string_literal})) {
            try template_tokens.append(allocator, self.previous.?);
        } else if (try self.match(&.{.multiline_string_literal})) {
            try template_tokens.append(allocator, self.previous.?);
            while (try self.match(&.{.multiline_string_literal})) {
                try template_tokens.append(allocator, self.previous.?);
            }
        } else {
            return self.parseError("Expected assembly template string.");
        }

        var outputs: std.ArrayList(Ast.AsmOutput) = .empty;
        var inputs: std.ArrayList(Ast.AsmInput) = .empty;
        var clobbers: std.ArrayList(Token) = .empty;

        // Outputs
        if (try self.match(&.{.colon})) {
            if (!self.check(&.{ .colon, .r_brace })) {
                while (true) {
                    try self.consume(.l_bracket, "Expected '[' before output operand name.");
                    const name = try self.consumeReturn(.identifier, "Expected output operand name.");
                    try self.consume(.r_bracket, "Expected ']' after output operand name.");

                    const constraint = try self.consumeReturn(.string_literal, "Expected output constraint string.");

                    try self.consume(.l_paren, "Expected '(' before output expression.");
                    const expr = try self.parseExpression();
                    try self.consume(.r_paren, "Expected ')' after output expression.");

                    try outputs.append(allocator, .{ .name = name, .constraint = constraint, .expr = expr });

                    if (!try self.match(&.{.comma})) break;
                }
            }

            // Inputs
            if (try self.match(&.{.colon})) {
                if (!self.check(&.{ .colon, .r_brace })) {
                    while (true) {
                        try self.consume(.l_bracket, "Expected '[' before input operand name.");
                        const name = try self.consumeReturn(.identifier, "Expected input operand name.");
                        try self.consume(.r_bracket, "Expected ']' after input operand name.");

                        const constraint = try self.consumeReturn(.string_literal, "Expected input constraint string.");

                        try self.consume(.l_paren, "Expected '(' before input expression.");
                        const expr = try self.parseExpression();
                        try self.consume(.r_paren, "Expected ')' after input expression.");

                        try inputs.append(allocator, .{ .name = name, .constraint = constraint, .expr = expr });

                        if (!try self.match(&.{.comma})) break;
                    }
                }

                // Clobbers
                if (try self.match(&.{.colon})) {
                    while (true) {
                        if (self.check(&.{.string_literal})) {
                            const clobber = try self.consumeReturn(.string_literal, "Expected clobber string.");
                            try clobbers.append(allocator, clobber);
                        }
                        if (!try self.match(&.{.comma})) break;
                    }
                }
            }
        }

        try self.consume(.r_brace, "Expected '}' after asm block.");

        return self.create(Ast.AsmBlock, .{
            .keyword = keyword,
            .is_pure = is_pure,
            .template = try template_tokens.toOwnedSlice(allocator),
            .outputs = try outputs.toOwnedSlice(allocator),
            .inputs = try inputs.toOwnedSlice(allocator),
            .clobbers = try clobbers.toOwnedSlice(allocator),
        });
    }

    fn parseBuiltinCall(self: *Parser) !*Ast.BuiltinCall {
        const allocator = self.arena.allocator();
        const name = try self.consumeReturn(.identifier, "Expected builtin identifier after '@'.");

        try self.consume(.l_paren, "Expected '(' after builtin name.");

        var args: std.ArrayList(*Ast.Expression) = .empty;
        if (!self.check(&.{.r_paren})) {
            while (true) {
                try args.append(allocator, try self.parseExpression());
                if (!try self.match(&.{.comma})) break;
            }
        }

        try self.consume(.r_paren, "Expected ')' after builtin arguments.");

        return self.create(Ast.BuiltinCall, .{
            .name = name,
            .args = try args.toOwnedSlice(allocator),
        });
    }

    fn parseFieldInits(self: *Parser) ![]const *Ast.FieldInit {
        const allocator = self.arena.allocator();
        var fields: std.ArrayList(*Ast.FieldInit) = .empty;

        while (!self.check(&.{ .r_brace, .eof })) {
            try self.consume(.dot, "Expected '.' before field name.");
            const name = try self.consumeReturn(.identifier, "Expected field name.");
            try self.consume(.equal, "Expected '=' after field name.");
            const value = try self.parseExpression();
            try self.consume(.comma, "Expected ',' after field initialization.");

            const field = try self.create(Ast.FieldInit, .{
                .name = name,
                .value = value,
            });
            try fields.append(allocator, field);
        }
        try self.consume(.r_brace, "Expected '}' after struct initialization.");
        return try fields.toOwnedSlice(allocator);
    }

    fn parseBlock(self: *Parser) Parser.Error!*Ast.Block {
        const allocator = self.arena.allocator();

        try self.consume(.l_brace, "Expected '{' to start block.");
        var stmts: std.ArrayList(*Ast.Statement) = .empty;

        while (!self.check(&.{ .r_brace, .eof })) {
            const stmt = try self.parseStatement();
            try stmts.append(allocator, stmt);
        }

        try self.consume(.r_brace, "Expected '}' after block.");

        return self.create(Ast.Block, .{
            .stmts = try stmts.toOwnedSlice(allocator),
        });
    }

    fn parseStatement(self: *Parser) Parser.Error!*Ast.Statement {
        if (try self.match(&.{.@"if"})) return self.parseIfStatement();
        if (try self.match(&.{.@"while"})) return self.parseWhileStatement();
        if (try self.match(&.{.@"for"})) return self.parseForStatement();
        if (try self.match(&.{.@"return"})) return self.parseReturnStatement();
        if (try self.match(&.{.@"switch"})) return self.parseSwitchStatement();
        if (try self.match(&.{ .@"defer", .@"errdefer" })) return self.parseDeferStatement();

        if (try self.match(&.{.@"break"})) {
            const token = self.previous.?;
            try self.consume(.semicolon, "Expected ';' after break.");
            return self.create(Ast.Statement, .{ .break_stmt = token });
        }

        if (try self.match(&.{.@"continue"})) {
            const token = self.previous.?;
            try self.consume(.semicolon, "Expected ';' after continue.");
            return self.create(Ast.Statement, .{ .continue_stmt = token });
        }

        if (self.check(&.{.l_brace})) {
            const block = try self.parseBlock();
            return self.create(Ast.Statement, .{ .block = block });
        }

        if (self.check(&.{ .@"var", .@"const" })) {
            const decl = try self.parseDeclaration();
            return self.create(Ast.Statement, .{ .declaration = decl });
        }

        const expr = try self.parseExpression();
        try self.consume(.semicolon, "Expected ';' after expression.");
        return self.create(Ast.Statement, .{ .expression = expr });
    }

    fn parseIfStatement(self: *Parser) !*Ast.Statement {
        try self.consume(.l_paren, "Expected '(' after 'if'.");
        const condition = try self.parseExpression();
        try self.consume(.r_paren, "Expected ')' after 'if' condition.");

        const then_block = try self.parseBlock();

        var else_branch: ?*Ast.Statement = null;

        if (try self.match(&.{.@"else"})) {
            if (self.check(&.{.@"if"})) {
                else_branch = try self.parseStatement();
            } else {
                const block = try self.parseBlock();
                else_branch = try self.create(Ast.Statement, .{
                    .block = block,
                });
            }
        }

        const if_node = try self.create(Ast.IfStmt, .{
            .condition = condition,
            .then_block = then_block,
            .else_branch = else_branch,
        });

        return self.create(Ast.Statement, .{ .if_stmt = if_node });
    }

    fn parseWhileStatement(self: *Parser) !*Ast.Statement {
        try self.consume(.l_paren, "Expected '(' after 'while'.");
        const condition = try self.parseExpression();
        try self.consume(.r_paren, "Expected ')' after 'while' condition.");

        const body = try self.parseBlock();

        const while_node = try self.create(Ast.WhileStmt, .{
            .condition = condition,
            .body = body,
        });

        return self.create(Ast.Statement, .{ .while_stmt = while_node });
    }

    fn parseReturnStatement(self: *Parser) !*Ast.Statement {
        const keyword = self.previous.?;
        var value: ?*Ast.Expression = null;

        if (!self.check(&.{.semicolon})) {
            value = try self.parseExpression();
        }
        try self.consume(.semicolon, "Expected ';' after 'return' statement.");

        const return_node = try self.create(Ast.ReturnStmt, .{
            .keyword = keyword,
            .value = value,
        });

        return self.create(Ast.Statement, .{ .return_stmt = return_node });
    }

    fn parseDeferStatement(self: *Parser) !*Ast.Statement {
        const keyword = self.previous.?;
        const payload = try self.parseStatement();

        const defer_node = try self.create(Ast.DeferStmt, .{
            .keyword = keyword,
            .payload = payload,
        });

        return self.create(Ast.Statement, .{ .defer_stmt = defer_node });
    }

    fn parseForStatement(self: *Parser) !*Ast.Statement {
        try self.consume(.l_paren, "Expected '(' after 'for'.");

        // Init: declaration or expression statement or empty (;)
        var init_stmt: ?*Ast.Statement = null;
        if (!try self.match(&.{.semicolon})) {
            if (self.check(&.{ .@"var", .@"const" })) {
                const decl = try self.parseDeclaration();
                init_stmt = try self.create(Ast.Statement, .{ .declaration = decl });
            } else {
                const expr = try self.parseExpression();
                try self.consume(.semicolon, "Expected ';' after initializer.");
                init_stmt = try self.create(Ast.Statement, .{ .expression = expr });
            }
        }

        // Condition
        var condition: ?*Ast.Expression = null;
        if (!try self.match(&.{.semicolon})) {
            condition = try self.parseExpression();
            try self.consume(.semicolon, "Expected ';' after condition.");
        }

        // Post
        var post: ?*Ast.Expression = null;
        if (!self.check(&.{.r_paren})) {
            post = try self.parseExpression();
        }

        try self.consume(.r_paren, "Expected ')' after for clauses.");

        const body = try self.parseBlock();

        const for_node = try self.create(Ast.ForStmt, .{
            .init = init_stmt,
            .condition = condition,
            .post = post,
            .body = body,
        });

        return self.create(Ast.Statement, .{ .for_stmt = for_node });
    }

    fn parseSwitchStatement(self: *Parser) !*Ast.Statement {
        const allocator = self.arena.allocator();
        try self.consume(.l_paren, "Expected '(' after 'switch'.");
        const condition = try self.parseExpression();
        try self.consume(.r_paren, "Expected ')' after switch condition.");
        try self.consume(.l_brace, "Expected '{' to start switch body.");

        var prongs: std.ArrayList(*Ast.SwitchProng) = .empty;

        while (!self.check(&.{ .r_brace, .eof })) {
            var items: std.ArrayList(Ast.SwitchItem) = .empty;
            var is_else = false;

            if (try self.match(&.{.@"else"})) {
                is_else = true;
            } else {
                while (true) {
                    const start_expr = try self.parseExpression();

                    if (try self.match(&.{.ellipsis})) {
                        const end_expr = try self.parseExpression();
                        const range = try self.create(Ast.SwitchRange, .{
                            .start = start_expr,
                            .end = end_expr,
                        });
                        try items.append(allocator, .{ .range = range });
                    } else {
                        try items.append(allocator, .{ .expression = start_expr });
                    }

                    if (!try self.match(&.{.comma})) break;
                }
            }

            try self.consume(.arrow, "Expected '=>' after switch patterns.");

            const body: *Ast.Statement = if (self.check(&.{.l_brace}))
                try self.create(Ast.Statement, .{ .block = try self.parseBlock() })
            else
                try self.parseStatement();

            // Optional comma after statement
            _ = try self.match(&.{.comma});

            const prong = try self.create(Ast.SwitchProng, .{
                .items = try items.toOwnedSlice(allocator),
                .is_else = is_else,
                .body = body,
            });

            try prongs.append(allocator, prong);
        }

        try self.consume(.r_brace, "Expected '}' after switch body.");

        const switch_node = try self.create(Ast.SwitchStmt, .{
            .condition = condition,
            .prongs = try prongs.toOwnedSlice(allocator),
        });

        return self.create(Ast.Statement, .{ .switch_stmt = switch_node });
    }

    fn parseEnum(self: *Parser) !*Ast.EnumDef {
        const allocator = self.arena.allocator();

        const keyword = self.previous.?;

        var backing_type: ?*Ast.TypeExpr = null;
        if (try self.match(&.{.l_paren})) {
            backing_type = try self.parseTypeExpr();
            try self.consume(.r_paren, "Expected ')' after enum backing type.");
        }

        try self.consume(.l_brace, "Expected '{' after 'enum'.");

        var members: std.ArrayList(Token) = .empty;

        while (!self.check(&.{ .r_brace, .eof })) {
            const name = try self.consumeReturn(.identifier, "Expected enum identifier");
            try members.append(allocator, name);
            _ = try self.match(&.{.comma});
        }

        try self.consume(.r_brace, "Expected '}' after 'enum' fields.");

        return self.create(Ast.EnumDef, .{
            .keyword = keyword,
            .backing_type = backing_type,
            .members = try members.toOwnedSlice(allocator),
        });
    }

    fn advance(self: *Parser) !void {
        self.previous = self.current;

        while (true) {
            self.current = self.tokenizer.next();

            if (self.current.?.tag != .invalid) break;

            return self.lexicalError("Invalid character.");
        }
    }

    fn consume(self: *Parser, expected_tag: Token.Tag, err_msg: ?[]const u8) !void {
        if (self.check(&.{expected_tag})) {
            return try self.advance();
        }

        return self.parseError(err_msg orelse "Unexpected error.");
    }

    fn match(self: *Parser, expected_tags: []const Token.Tag) !bool {
        const contains_tag = self.check(expected_tags);
        if (!contains_tag) return false;

        try self.advance();
        return true;
    }

    fn check(self: *Parser, expected_tags: []const Token.Tag) bool {
        const contains_tag = std.mem.containsAtLeastScalar2(Token.Tag, expected_tags, self.current.?.tag, 1);
        return contains_tag;
    }

    fn consumeReturn(self: *Parser, tag: Token.Tag, msg: []const u8) !Token {
        if (self.check(&.{tag})) {
            try self.advance();
            return self.previous.?;
        }
        return self.parseError(msg);
    }

    /// Try to recover from a panicking Parser by finding a statement,
    /// where we can start parsing normally again, without a mountain of
    /// errors on our way.
    fn synchronize(self: *Parser) !void {
        self.panicking = false;

        try self.advance();

        // While we aren't at the end or at a valid token, keep going
        while (self.current.?.tag != .eof) : (try self.advance()) {
            // If we are at a semicolon, there is a high probability that a statement starts
            if (self.previous.?.tag == .semicolon) return;

            // If the current token is a keyword that starts a statement, we are probably fine
            switch (self.current.?.tag) {
                .@"fn", .@"var", .@"const", .@"if", .@"while", .@"for", .@"switch", .@"return" => return,
                else => {},
            }
        }
    }

    /// Throw a ParseError at the current token
    fn parseError(self: *Parser, err_msg: []const u8) error{ParseError} {
        return self.errorAt(self.current.?, err_msg);
    }

    /// Used only for invalid characters (situations where we can immediately)
    /// keep on parsing instead of panicking
    fn lexicalError(self: *Parser, err_msg: []const u8) void {
        self.had_error = true;

        const token = self.current.?;
        const loc = token.loc;
        std.debug.print("[line {d}:{d}] Error at '{s}': {s}\n", .{
            loc.line,
            loc.column,
            token.lexeme(self.source),
            err_msg,
        });
    }

    /// The core error reporting logic with pretty printing.
    fn errorAt(self: *Parser, token: Token, msg: []const u8) error{ParseError} {
        // If we are already panicking, don't spam the user.
        if (self.panicking) return error.ParseError;

        self.panicking = true;
        self.had_error = true;

        const loc = token.loc;
        std.debug.print("[line {d}:{d}] Error", .{ loc.line, loc.column });

        if (token.tag == .eof) {
            std.debug.print(" at end", .{});
        } else if (token.tag == .invalid) {
            std.debug.print(" at '{s}'", .{token.lexeme(self.source)});
        } else {
            std.debug.print(" at '{s}'", .{token.lexeme(self.source)});
        }

        std.debug.print(": {s}\n", .{msg});

        // Print the source line and a pointer to the error in it
        self.printErrorPointer(token);

        return error.ParseError;
    }

    fn printErrorPointer(self: *Parser, token: Token) void {
        var line_start: usize = token.loc.start;
        while (line_start > 0 and self.source[line_start - 1] != '\n') {
            line_start -= 1;
        }

        var line_end: usize = token.loc.start;
        while (line_end < self.source.len and self.source[line_end] != '\n') {
            line_end += 1;
        }

        const line_slice = self.source[line_start..line_end];
        std.debug.print("    {s}\n", .{line_slice});

        std.debug.print("    ", .{}); // Indent to match code

        // Calculate column offset (column starts from 1)
        var i: usize = 1;
        while (i < token.loc.column) : (i += 1) {
            std.debug.print(" ", .{});
        }

        const len = if (token.loc.len == 0) 1 else token.loc.len;
        var j: usize = 0;
        while (j < len) : (j += 1) {
            std.debug.print("^", .{});
        }
        std.debug.print("\n", .{});
    }

    /// Helper for quickly allocating AST nodes
    fn create(self: *Parser, comptime T: type, val: T) !*T {
        const allocator = self.arena.allocator();

        const ptr = try allocator.create(T);
        ptr.* = val;
        return ptr;
    }
};
