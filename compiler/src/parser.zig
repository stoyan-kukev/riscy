const std = @import("std");
const Token = @import("tokenizer.zig").Token;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Node = @import("ast.zig").Node;

pub const Parser = struct {
    arena: std.mem.Allocator,
    tokenizer: Tokenizer,
    curr: Token,
    peek: Token,

    pub const Error = error{ ParserError, NotImplemented, UnexpectedToken, OutOfMemory };

    pub const Precedence = enum(u8) {
        lowest,
        logical_or,
        logical_and,
        coalesce,
        equality,
        comparison,
        term,
        factor,
        prefix,
        postfix,
        call,

        pub fn lessThan(self: Precedence, other: Precedence) bool {
            return @intFromEnum(self) < @intFromEnum(other);
        }

        pub fn fromTag(tag: Token.Tag) Precedence {
            return switch (tag) {
                // Level 1: Logical OR
                .keyword_or => .logical_or,
                // Level 2: Logical AND
                .keyword_and => .logical_and,
                // Level 3: Coalescing operators
                .keyword_catch, .keyword_orelse => .coalesce,
                // Level 4: Equality
                .equal_equal, .bang_equal => .equality,
                // Level 5: Comparison
                .less, .greater, .less_equal, .greater_equal => .comparison,
                // Level 6: Terms (Addition, Subtraction, Bitwise OR, XOR)
                .plus, .minus, .pipe, .caret => .term,
                // Level 7: Factors (Mult, Div, Mod, Bitwise AND, Shifts)
                .star, .slash, .percent, .ampersand, .shift_left, .shift_right => .factor,
                // Level 8: Prefix (Not used in 'infix' loop usually, but good for reference)
                .bang, .tilde, .keyword_not => .prefix,
                // Level 9: Call / Postfix
                .l_paren, .l_bracket, .dot => .call,
                else => .lowest,
            };
        }
    };

    pub fn init(allocator: std.mem.Allocator, source: [:0]const u8) Parser {
        var tokenizer: Tokenizer = .init(source);

        const first = tokenizer.next();
        const second = tokenizer.next();

        return .{
            .tokenizer = tokenizer,
            .arena = allocator,
            .curr = first,
            .peek = second,
        };
    }

    fn advance(self: *Parser) void {
        self.curr = self.peek;
        self.peek = self.tokenizer.next();
    }

    fn check(self: *Parser, tags: []const Token.Tag) bool {
        return std.mem.containsAtLeastScalar(Token.Tag, tags, 1, self.curr.tag);
    }

    fn match(self: *Parser, tags: []const Token.Tag) bool {
        if (self.check(tags)) {
            self.advance();
            return true;
        }

        return false;
    }

    fn consume(self: *Parser, tag: Token.Tag) !Token {
        if (self.check(&.{tag})) {
            const token = self.curr;
            self.advance();
            return token;
        }

        return error.UnexpectedToken;
    }

    fn createNode(self: *Parser, tag: Node.Tag, token: Token, data: Node.Data) !*Node {
        const node = try self.arena.create(Node);

        node.* = .{
            .tag = tag,
            .token = token,
            .data = data,
        };

        return node;
    }

    pub fn parseRoot(self: *Parser) !*Node {
        var decls: std.ArrayList(*Node) = .empty;

        while (self.curr.tag != .eof) {
            const decl = try self.parseDeclaration();
            try decls.append(self.arena, decl);
        }

        return self.createNode(.root, self.curr, .{
            .root = .{ .decls = try decls.toOwnedSlice(self.arena) },
        });
    }

    fn parseDeclaration(self: *Parser) Parser.Error!*Node {
        const is_pub = self.match(&.{.keyword_pub});

        var linkage: Node.Linkage = .none;
        if (self.match(&.{.keyword_extern})) {
            linkage = .@"extern";
        } else if (self.match(&.{.keyword_export})) {
            linkage = .@"export";
        }

        var link_section: ?*Node = null;
        if (self.match(&.{.keyword_linksection})) {
            _ = try self.consume(.l_paren);
            const section_name = try self.consume(.string_literal);
            _ = try self.consume(.r_paren);

            link_section = try self.createNode(.string_literal, section_name, .{ .none = {} });
        }

        const is_const: bool = blk: {
            if (self.match(&.{.keyword_const})) break :blk true;
            if (self.match(&.{.keyword_var})) break :blk false;
            return error.UnexpectedToken;
        };

        const name_token = try self.consume(.identifier);

        var type_expr: ?*Node = null;
        if (self.match(&.{.colon})) {
            type_expr = try self.parseTypeExpr();
        }

        var align_expr: ?*Node = null;
        if (self.match(&.{.keyword_align})) {
            _ = try self.consume(.l_paren);
            align_expr = try self.parseExpression(.lowest);
            _ = try self.consume(.r_paren);
        }

        var init_expr: ?*Node = null;
        if (self.match(&.{.equal})) {
            init_expr = try self.parseExpression(.lowest);
        }

        _ = try self.consume(.semicolon);

        return try self.createNode(.declaration, name_token, .{ .declaration = .{
            .is_pub = is_pub,
            .linkage = linkage,
            .linksection_val = link_section,
            .is_const = is_const,
            .type_expr = type_expr,
            .align_expr = align_expr,
            .initial_value = init_expr,
        } });
    }

    fn parseExpression(self: *Parser, precedence: Precedence) Parser.Error!*Node {
        var left = try self.parsePrefix();

        if (precedence.lessThan(.fromTag(self.curr.tag))) {
            left = try self.parseInfix(left);
        }

        return left;
    }

    fn parsePrefix(self: *Parser) Parser.Error!*Node {
        switch (self.curr.tag) {
            .identifier => {
                const token = try self.consume(.identifier);
                return self.createNode(.identifier, token, .{ .none = {} });
            },
            .int_literal => {
                const token = try self.consume(.int_literal);
                return self.createNode(.int_literal, token, .{ .none = {} });
            },
            .string_literal => {
                const token = try self.consume(.string_literal);
                const raw = self.tokenizer.buffer[token.loc.start + 1 .. token.loc.end - 1];

                return self.createNode(.string_literal, token, .{
                    .string_literal = .{
                        .data = raw,
                    },
                });
            },
            .char_literal => {
                const token = try self.consume(.char_literal);
                const raw = self.tokenizer.buffer[token.loc.start + 1];
                return self.createNode(.char_literal, token, .{
                    .char_literal = .{
                        .char = raw,
                    },
                });
            },
            .l_paren => {
                self.advance();
                const expr = try self.parseExpression(.lowest);
                _ = try self.consume(.r_paren);
                return expr;
            },
            .multiline_string_literal => return self.parseMultilineStringLiteral(),
            .bang, .minus, .tilde => return self.parseUnary(),
            .keyword_struct => return self.parseStructLiteral(),
            .keyword_enum => return self.parseEnumLiteral(),
            .keyword_union => return self.parseUnionLiteral(),
            .keyword_fn => return self.parseFnLiteral(),
            .keyword_if => return self.parseIf(),
            .keyword_switch => return self.parseSwitch(),
            .keyword_asm => return self.parseAsm(),
            .l_brace => return self.parseBlock(),
            else => return error.UnexpectedToken,
        }
    }

    fn parseInfix(self: *Parser, left: *Node) !*Node {
        const token = self.curr;
        const tag = token.tag;

        switch (tag) {
            .plus,
            .minus,
            .star,
            .slash,
            .percent,
            .equal_equal,
            .bang_equal,
            .less,
            .greater,
            .less_equal,
            .greater_equal,
            .pipe,
            .ampersand,
            .caret,
            => {
                self.advance();
                const right = try self.parseExpression(.fromTag(tag));
                return self.createNode(.binary_expr, token, .{
                    .binary_expr = .{
                        .left = left,
                        .right = right,
                        .operator = tag,
                    },
                });
            },
            .l_paren => return self.parseCall(left),
            .l_bracket => return self.parseIndex(left),
            .dot => return self.parseDotAccess(left),
            else => return left,
        }
    }

    fn parseMultilineStringLiteral(self: *Parser) Parser.Error!*Node {
        var buffer: std.ArrayList(u8) = .empty;

        const start_token = self.curr;

        while (self.check(&.{.multiline_string_literal})) {
            const token = try self.consume(.multiline_string_literal);

            const raw_line = self.tokenizer.buffer[token.loc.start..token.loc.end];
            const content = if (raw_line.len >= 2) raw_line[2..] else raw_line;

            try buffer.appendSlice(self.arena, content);
            try buffer.append(self.arena, '\n');
        }

        return self.createNode(.string_literal, start_token, .{
            .string_literal = .{
                .data = try buffer.toOwnedSlice(self.arena),
            },
        });
    }

    fn parseUnary(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseTypeExpr(self: *Parser) Parser.Error!*Node {
        switch (self.curr.tag) {
            .star, .tilde => {
                const token = self.curr;
                self.advance();

                const is_const = self.match(&.{.keyword_const});
                const allow_zero = self.match(&.{.keyword_allowzero});

                var align_expr: ?*Node = null;
                if (self.match(&.{.keyword_align})) {
                    _ = try self.consume(.l_paren);
                    align_expr = try self.parseExpression(.lowest);
                    _ = try self.consume(.r_paren);
                }

                const child = try self.parseTypeExpr();

                return self.createNode(.pointer_type, token, .{
                    .pointer_type = .{
                        .child_type = child,
                        .kind = if (token.tag == .star) .normal else .@"volatile",
                        .align_expr = align_expr,
                        .allow_zero = allow_zero,
                        .is_const = is_const,
                    },
                });
            },
            .question_mark => {
                const token = self.curr;
                self.advance();

                const child = try self.parseTypeExpr();

                return self.createNode(.optional_type, token, .{
                    .optional_type = .{
                        .child_type = child,
                    },
                });
            },
            .bang => {
                const token = self.curr;
                self.advance();

                const child = try self.parseTypeExpr();

                return self.createNode(.error_union_type, token, .{
                    .error_union_type = .{
                        .child_type = child,
                    },
                });
            },
            .l_bracket => {
                const token = self.curr;
                self.advance();

                if (self.match(&.{.r_bracket})) {
                    const is_const = self.match(&.{.keyword_const});
                    const child = try self.parseTypeExpr();

                    return self.createNode(.slice_type, token, .{ .slice_type = .{
                        .child_type = child,
                        .is_const = is_const,
                    } });
                } else {
                    const size_expr = try self.parseExpression(.lowest);
                    _ = try self.consume(.r_bracket);

                    const is_const = self.match(&.{.keyword_const});
                    const child = try self.parseTypeExpr();

                    return self.createNode(.array_type, token, .{
                        .array_type = .{
                            .size_expr = size_expr,
                            .child_type = child,
                            .is_const = is_const,
                        },
                    });
                }
            },
            .identifier, .keyword_struct, .keyword_union, .keyword_enum, .keyword_fn => {
                return self.parsePrefix();
            },
            else => return error.UnexpectedToken,
        }
    }

    fn parseStructLiteral(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseEnumLiteral(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseUnionLiteral(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseFnLiteral(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseIf(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseWhile(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseFor(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseSwitch(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseReturn(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseBreak(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseContinue(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseDefer(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseAsm(self: *Parser) Parser.Error!*Node {
        _ = self;
        return error.NotImplemented;
    }

    fn parseStatement(self: *Parser) Parser.Error!*Node {
        switch (self.curr.tag) {
            .keyword_if => return self.parseIf(),
            .keyword_while => return self.parseWhile(),
            .keyword_for => return self.parseFor(),
            .keyword_switch => return self.parseSwitch(),

            .keyword_return => return self.parseReturn(),
            .keyword_break => return self.parseBreak(),
            .keyword_continue => return self.parseContinue(),

            .keyword_defer, .keyword_errdefer => return self.parseDefer(),

            .keyword_asm => return self.parseAsm(),

            .l_brace => return self.parseBlock(),

            else => return self.parseAssignmentOrExprStmt(),
        }
    }

    fn parseAssignmentOrExprStmt(self: *Parser) Parser.Error!*Node {
        const lhs = try self.parseExpression(.lowest);

        if (self.match(&.{.equal})) {
            const equal_token = self.curr;

            const rhs = try self.parseExpression(.lowest);

            _ = try self.consume(.semicolon);

            return self.createNode(.assignment, equal_token, .{
                .assignment = .{
                    .identifier = lhs,
                    .assignment_expr = rhs,
                },
            });
        }

        _ = try self.consume(.semicolon);

        return lhs;
    }

    fn parseBlock(self: *Parser) Parser.Error!*Node {
        const start_token = try self.consume(.l_brace);

        var statements: std.ArrayList(*Node) = .empty;

        while (!self.check(&.{ .eof, .r_brace })) {
            if (self.check(&.{
                .keyword_var,
                .keyword_const,
                .keyword_pub,
                .keyword_extern,
                .keyword_export,
                .keyword_linksection,
            })) {
                const decl = try self.parseDeclaration();
                try statements.append(self.arena, decl);
                continue;
            }

            if (self.check(&.{
                .keyword_if,
                .keyword_while,
                .keyword_for,
                .keyword_switch,
                .keyword_return,
                .keyword_break,
                .keyword_continue,
                .keyword_defer,
                .keyword_errdefer,
                .keyword_asm,
            })) {
                const stmt = try self.parseStatement();
                try statements.append(self.arena, stmt);
                continue;
            }

            const expr_stmt = try self.parseAssignmentOrExprStmt();
            try statements.append(self.arena, expr_stmt);
        }

        _ = try self.consume(.r_brace);

        return self.createNode(.block, start_token, .{
            .block = try statements.toOwnedSlice(self.arena),
        });
    }

    fn parseCall(self: *Parser, left: *Node) Parser.Error!*Node {
        _ = self;
        _ = left;
        return error.NotImplemented;
    }

    fn parseIndex(self: *Parser, left: *Node) Parser.Error!*Node {
        _ = self;
        _ = left;
        return error.NotImplemented;
    }

    fn parseDotAccess(self: *Parser, left: *Node) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        if (self.match(&.{.star})) {
            return self.createNode(.ptr_dereference, token, .{ .unary_suffix = .{ .lhs = left } });
        }
        if (self.match(&.{.tilde})) {
            return self.createNode(.volatile_dereference, token, .{ .unary_suffix = .{ .lhs = left } });
        }
        if (self.match(&.{.question_mark})) {
            return self.createNode(.optional_unwrap, token, .{ .unary_suffix = .{ .lhs = left } });
        }
        if (self.match(&.{.bang})) {
            return self.createNode(.error_unwrap, token, .{ .unary_suffix = .{ .lhs = left } });
        }

        const field = try self.consume(.identifier);

        return self.createNode(.field_access, token, .{
            .field_access = .{
                .lhs = left,
                .field_name = field,
            },
        });
    }
};
