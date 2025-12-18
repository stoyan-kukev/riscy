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

                .l_paren, .l_bracket, .dot => .call,

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

    pub fn parseProgram(self: *Parser) !Ast.Program {
        const allocator = self.arena.allocator();

        var decls: std.ArrayList(*Ast.Declaration) = .empty;

        while (!self.check(.eof)) {
            const decl = self.parseDeclaration() catch |err| switch (err) {
                error.ParseError => try self.synchronize(),
                else => return err,
            };

            try decls.append(allocator, decl);
        }

        return .{ .decls = try decls.toOwnedSlice(allocator) };
    }

    fn parseDeclaration(self: *Parser) !*Ast.Declaration {
        const pub_token = if (try self.match(&.{.@"pub"})) self.previous else null;
        const extern_export_token = if (try self.match(&.{.@"export"}) or try self.match(&.{.@"extern"})) self.previous else null;
        const link_section_token = if (try self.match(&.{.@"linksection"})) self.previous else null;

        if (!try self.match(&.{.@"var"}) and !try self.match(&.{.@"const"})) {
            return self.parseError("Expected 'var' or 'const'.");
        }
        const const_var_token = self.previous.?;

        if (!try self.match(&.{.identifier})) return self.parseError("Expected name identifier.");
        const decl_name = self.previous.?;

        const type_expr = if (try self.match(&.{.colon})) try self.parseTypeExpr() else null;

        const align_expr = if (try self.match(&.{.@"align"})) try self.parseExpression() else null;

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

    fn parseTypeExpr(self: *Parser) !*Ast.TypeExpr {
        const allocator = self.arena.allocator();
        var prefixes: std.ArrayList(Ast.TypePrefix) = .empty;

        while (true) {
            if (try self.match(&.{.asterisk})) {
                try prefixes.append(allocator, .{ .pointer = self.previous.? });
            } else if (try self.match(&.{.question_mark})) {
                try prefixes.append(allocator, .{ .optional = self.previous.? });
            } else if (try self.match(&.{.bang})) {
                try prefixes.append(allocator, .{ .error_union = self.previous.? });
            } else if (try self.match(&.{.l_bracket})) {
                if (try self.match(&.{.asterisk})) {
                    try self.consume(.r_bracket, "Expected ']' after '[*'.");
                    try prefixes.append(allocator, .{ .many_pointer = self.previous.? });
                } else {
                    // If we are here then the type started with '[', which means it's an array size we're parsing
                    const size_expr = try self.parseExpression();
                    try self.consume(.r_bracket, "Expected ']' after array size'.");

                    const array_node = try self.create(Ast.ArrayType, .{
                        .size = size_expr,
                        // recursively parse the child type
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
            try self.parseStruct()
        else if (try self.match(&.{.@"enum"}))
            try self.parseEnum()
        else if (try self.match(&.{.@"union"}))
            try self.parseUnion()
        else
            return self.parseError("Expected type.");

        return try self.create(Ast.TypeExpr, .{
            .prefixes = try prefixes.toOwnedSlice(allocator),
            .core = core,
        });
    }

    fn parseExpression(self: *Parser) !*Ast.Expression {
        return try self.parsePrecedence(.lowest);
    }

    fn parsePrecedence(self: *Parser, curr_prec: Precedence) Parser.Error!*Ast.Expression {
        var left = try self.parsePrefix();

        while (true) {
            const next_prec: Precedence = .fromTag(self.current.?.tag);

            if (next_prec.less(curr_prec)) break;

            try self.advance();
            const operator = self.previous.?;

            left = try self.parseInfix(left, operator);
        }
    }

    fn parsePrefix(self: *Parser) !*Ast.Expression {
        // Unary operators
        if (try self.match(&.{ .minus, .not, .bang, .tilde, .ampersand, .asterisk })) {
            const op = self.previous.?;
            const right = try self.parsePrecedence(.unary);

            const unary_node = try self.create(Ast.UnaryExpr, .{
                .op = op,
                .operand = right,
            });

            return self.create(Ast.Expression, .{ .unary = unary_node });
        }

        // Grouping
        if (try self.match(&.{.l_paren})) {
            const expr = try self.parseExpression();
            try self.consume(.r_paren, "Expected ')' after expression.");
            const primary = try self.create(Ast.PrimaryExpr, .{ .grouped = expr });

            return self.create(Ast.Expression, .{ .primary = primary });
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

        if (try self.match(&.{.identifier})) {
            const primary = try self.create(Ast.PrimaryExpr, .{ .identifier = self.previous.? });
            return self.create(Ast.Expression, .{ .primary = primary });
        }

        // Function Literals
        if (try self.match(&.{.@"fn"})) {
            return self.parseError("Function literals aren't implemented yet.");
        }

        return self.parseError("Expected expression.");
    }

    fn parseInfix(self: *Parser, left: *Ast.Expression, op: Token) !*Ast.Expression {
        const allocator = self.arena.allocator();

        switch (op.tag) {
            // Function call
            .l_paren => {
                var args: std.ArrayList(*Ast.Expression) = .empty;
                if (!self.check(.r_paren)) {
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
                try self.consume(.r_paren, "Expected ']' after index.");

                const index_node = try self.create(Ast.IndexExpr, .{
                    .target = left,
                    .index = index,
                });

                return self.create(Ast.Expression, .{ .index = index_node });
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

    fn advance(self: *Parser) !void {
        self.previous = self.current;

        while (true) {
            self.current = self.tokenizer.next();

            if (self.current.?.tag != .invalid) break;

            return self.lexicalError("Invalid character.");
        }
    }

    fn consume(self: *Parser, expected_tag: Token.Tag, err_msg: ?[]const u8) !void {
        if (self.check(expected_tag)) {
            return try self.advance();
        }

        return self.parseError(err_msg orelse "Unexpected error.");
    }

    fn match(self: *Parser, expected_tags: []const Token.Tag) !bool {
        const contains_tag = std.mem.containsAtLeastScalar2(Token.Tag, expected_tags, self.current.?.tag, 1);
        if (!contains_tag) return false;

        try self.advance();
        return true;
    }

    fn check(self: *Parser, expected_tag: Token.Tag) bool {
        return self.current.?.tag == expected_tag;
    }

    /// Try to recover from a panicking Parser by finding a statement,
    /// where we can start parsing normally again, without a mountain of
    /// errors on our way.
    fn synchronize(self: *Parser) !void {
        self.panicking = false;

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
