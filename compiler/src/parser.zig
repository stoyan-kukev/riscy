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

    /// Checks if the current tag matches a set of tags.
    fn check(self: *Parser, tags: []const Token.Tag) bool {
        return std.mem.containsAtLeastScalar(Token.Tag, tags, 1, self.curr.tag);
    }

    /// Checks if the current tag matches a set of tags.
    /// If it does, advance past it and return true.
    /// If not, it just returns false without advancing.
    fn match(self: *Parser, tags: []const Token.Tag) bool {
        if (self.check(tags)) {
            self.advance();
            return true;
        }

        return false;
    }

    /// Attempts to consume an expected token and return it.
    /// Returns `error.UnexpectedToken` if the current token
    /// and the expected one don't match.
    fn consume(self: *Parser, tag: Token.Tag) !Token {
        if (self.check(&.{tag})) {
            const token = self.curr;
            self.advance();
            return token;
        }

        std.debug.print("Unexpected token: {t}\n", .{self.curr.tag});
        std.debug.print("{s}\n", .{self.tokenizer.buffer[self.curr.loc.start - 10 .. self.curr.loc.end + 10]});

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

            const raw = self.tokenizer.buffer[section_name.loc.start + 1 .. section_name.loc.end - 1];

            link_section = try self.createNode(.string_literal, section_name, .{
                .string_literal = .{
                    .data = raw,
                },
            });
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

        return try self.createNode(.declaration, name_token, .{
            .declaration = .{
                .is_pub = is_pub,
                .linkage = linkage,
                .linksection_val = link_section,
                .is_const = is_const,
                .type_expr = type_expr,
                .align_expr = align_expr,
                .initial_value = init_expr,
            },
        });
    }

    fn parseExpression(self: *Parser, precedence: Precedence) Parser.Error!*Node {
        var left = try self.parsePrefix();

        while (precedence.lessThan(.fromTag(self.curr.tag))) {
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
            .keyword_unreachable => {
                const name = self.curr;
                self.advance();
                return self.createNode(.unreachable_literal, name, .{ .none = {} });
            },
            .keyword_null => {
                const token = self.curr;
                self.advance();
                return self.createNode(.null_literal, token, .{ .none = {} });
            },
            .dot => return self.parseAnonymousInit(),
            .multiline_string_literal => return self.parseMultilineStringLiteral(),
            .bang, .minus, .tilde, .keyword_not => return self.parseUnary(),
            .star, .question_mark, .l_bracket => return self.parseTypeExpr(),
            .keyword_struct => return self.parseStructLiteral(),
            .keyword_enum => return self.parseEnumLiteral(),
            .keyword_union => return self.parseUnionLiteral(),
            .keyword_fn => return self.parseFnLiteral(),
            .keyword_if => return self.parseIf(),
            .keyword_switch => return self.parseSwitch(),
            .keyword_error => return self.parseErrorLiteral(),
            .keyword_asm => return self.parseAsm(),
            .builtin => return self.parseBuiltin(),
            .l_brace => return self.parseBlock(),
            else => {
                std.debug.print("Unexpected token: {t}\n", .{self.curr.tag});
                std.debug.print("{s}\n", .{self.tokenizer.buffer[self.curr.loc.start - 10 .. self.curr.loc.end + 10]});
                return error.UnexpectedToken;
            },
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
            .shift_left,
            .shift_right,
            .keyword_or,
            .keyword_and,
            .keyword_orelse,
            .keyword_catch,
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
        const token = self.curr;
        self.advance();

        const operand = try self.parseExpression(.prefix);

        return self.createNode(.unary_expr, token, .{
            .unary_expr = .{
                .operator = token.tag,
                .operand = operand,
            },
        });
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
        const token = self.curr;
        self.advance();

        var modifier: Node.StructModifier = .none;
        if (self.match(&.{.dot})) {
            if (self.match(&.{.keyword_packed})) {
                modifier = .@"packed";
            } else if (self.match(&.{.keyword_c_abi})) {
                modifier = .c_abi;
            }
        }

        _ = try self.consume(.l_brace);

        var field_decls: std.ArrayList(*Node) = .empty;
        var container_decls: std.ArrayList(*Node) = .empty;
        while (!self.check(&.{ .r_brace, .eof })) {
            if (self.check(&.{.identifier})) {
                const name = try self.consume(.identifier);

                _ = try self.consume(.colon);

                const type_expr = try self.parseTypeExpr();

                var align_expr: ?*Node = null;
                if (self.match(&.{.keyword_align})) {
                    _ = try self.consume(.l_paren);
                    align_expr = try self.parseExpression(.lowest);
                    _ = try self.consume(.r_paren);
                }

                var default_value: ?*Node = null;
                if (self.match(&.{.equal})) {
                    default_value = try self.parseExpression(.lowest);
                }

                const node = try self.createNode(.field_decl, name, .{
                    .field_decl = .{
                        .type_expr = type_expr,
                        .align_expr = align_expr,
                        .default_value = default_value,
                    },
                });

                _ = try self.consume(.comma);

                try field_decls.append(self.arena, node);
            } else {
                try container_decls.append(self.arena, try self.parseDeclaration());
            }
        }

        _ = try self.consume(.r_brace);

        return try self.createNode(.struct_literal, token, .{
            .struct_literal = .{
                .modifier = modifier,
                .field_decls = try field_decls.toOwnedSlice(self.arena),
                .container_decls = try container_decls.toOwnedSlice(self.arena),
            },
        });
    }

    fn parseEnumLiteral(self: *Parser) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        var type_expr: ?*Node = null;
        if (self.match(&.{.l_paren})) {
            type_expr = try self.parseTypeExpr();
            _ = try self.consume(.r_paren);
        }

        _ = try self.consume(.l_brace);

        var literal_list: std.ArrayList(*Node) = .empty;
        var container_decls: std.ArrayList(*Node) = .empty;
        while (!self.check(&.{ .r_brace, .eof })) {
            if (self.check(&.{.identifier})) {
                const identifier = try self.consume(.identifier);
                const node = try self.createNode(.enum_member, identifier, .{ .none = {} });
                try literal_list.append(self.arena, node);

                _ = try self.consume(.comma);
            } else {
                try container_decls.append(self.arena, try self.parseDeclaration());
            }
        }

        _ = try self.consume(.r_brace);

        return try self.createNode(.enum_literal, token, .{
            .enum_literal = .{
                .type_expr = type_expr,
                .literal_list = try literal_list.toOwnedSlice(self.arena),
                .container_decls = try container_decls.toOwnedSlice(self.arena),
            },
        });
    }

    fn parseUnionLiteral(self: *Parser) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        var union_modifier: Node.UnionModifier = .none;
        if (self.match(&.{.dot}) and self.match(&.{.keyword_c_abi})) {
            union_modifier = .c_abi;
        }

        _ = try self.consume(.l_brace);

        var field_decls: std.ArrayList(*Node) = .empty;
        var container_decls: std.ArrayList(*Node) = .empty;
        while (!self.check(&.{ .r_brace, .eof })) {
            if (self.check(&.{.identifier})) {
                const name = try self.consume(.identifier);

                _ = try self.consume(.colon);

                const type_expr = try self.parseTypeExpr();

                var align_expr: ?*Node = null;
                if (self.match(&.{.keyword_align})) {
                    _ = try self.consume(.l_paren);
                    align_expr = try self.parseExpression(.lowest);
                    _ = try self.consume(.r_paren);
                }

                var default_value: ?*Node = null;
                if (self.match(&.{.equal})) {
                    default_value = try self.parseExpression(.lowest);
                }

                const node = try self.createNode(.field_decl, name, .{
                    .field_decl = .{
                        .type_expr = type_expr,
                        .align_expr = align_expr,
                        .default_value = default_value,
                    },
                });

                _ = try self.consume(.comma);

                try field_decls.append(self.arena, node);
            } else {
                try container_decls.append(self.arena, try self.parseDeclaration());
            }
        }

        _ = try self.consume(.r_brace);

        return try self.createNode(.union_literal, token, .{
            .union_literal = .{
                .modifier = union_modifier,
                .field_decls = try field_decls.toOwnedSlice(self.arena),
                .container_decls = try container_decls.toOwnedSlice(self.arena),
            },
        });
    }

    fn parseFnLiteral(self: *Parser) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        var fn_modifier: Node.FnModifier = .none;
        if (self.match(&.{.dot})) {
            if (self.match(&.{.keyword_naked})) {
                fn_modifier = .naked;
            } else if (self.match(&.{.keyword_interrupt})) {
                fn_modifier = .interrupt;
            }
        }

        _ = try self.consume(.l_paren);

        var param_list: std.ArrayList(*Node) = .empty;
        while (!self.check(&.{ .r_paren, .eof })) {
            const name = try self.consume(.identifier);
            _ = try self.consume(.colon);

            const type_expr = try self.parseTypeExpr();

            var align_expr: ?*Node = null;
            if (self.match(&.{.keyword_align})) {
                _ = try self.consume(.l_paren);
                align_expr = try self.parseExpression(.lowest);
                _ = try self.consume(.r_paren);
            }

            _ = self.match(&.{.comma});

            const param = try self.createNode(.parameter, name, .{
                .parameter = .{
                    .type_expr = type_expr,
                    .align_expr = align_expr,
                },
            });

            try param_list.append(self.arena, param);
        }

        _ = try self.consume(.r_paren);

        const return_type_expr = try self.parseTypeExpr();

        const body = try self.parseBlock();

        return try self.createNode(.fn_literal, token, .{
            .fn_literal = .{
                .modifier = fn_modifier,
                .param_list = try param_list.toOwnedSlice(self.arena),
                .return_type_expr = return_type_expr,
                .body = body,
            },
        });
    }

    fn parseBuiltin(self: *Parser) Parser.Error!*Node {
        const name = self.curr;
        self.advance();

        _ = try self.consume(.l_paren);

        var args: std.ArrayList(*Node) = .empty;

        if (!self.check(&.{ .r_paren, .eof })) {
            while (true) {
                const arg = try self.parseExpression(.lowest);
                try args.append(self.arena, arg);

                if (!self.match(&.{.comma})) break;
            }
        }

        _ = try self.consume(.r_paren);

        return self.createNode(.builtin_call, name, .{
            .builtin_call = .{
                .name = name,
                .args = try args.toOwnedSlice(self.arena),
            },
        });
    }

    fn parseErrorLiteral(self: *Parser) Parser.Error!*Node {
        self.advance();

        _ = try self.consume(.dot);

        const error_name = try self.consume(.identifier);

        return try self.createNode(.error_literal, error_name, .{ .none = {} });
    }

    fn parseIf(self: *Parser) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        _ = try self.consume(.l_paren);
        const condition = try self.parseExpression(.lowest);
        _ = try self.consume(.r_paren);

        const then_branch = try self.parseStatement();

        var else_branch: ?*Node = null;
        if (self.match(&.{.keyword_else})) {
            else_branch = try self.parseStatement();
        }

        return self.createNode(.if_stmt, token, .{
            .if_stmt = .{
                .condition = condition,
                .then_branch = then_branch,
                .else_branch = else_branch,
            },
        });
    }

    fn parseWhile(self: *Parser) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        _ = try self.consume(.l_paren);
        const condition = try self.parseExpression(.lowest);
        _ = try self.consume(.r_paren);

        const body = try self.parseStatement();

        return self.createNode(.while_stmt, token, .{
            .while_stmt = .{
                .condition = condition,
                .body = body,
            },
        });
    }

    fn parseFor(self: *Parser) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        _ = try self.consume(.l_paren);
        const iterable = try self.parseExpression(.lowest);
        _ = try self.consume(.r_paren);

        _ = try self.consume(.pipe);

        const is_value_ptr = self.match(&.{.star});

        const value_capture = try self.consume(.identifier);
        var index_capture: ?Token = null;
        if (self.match(&.{.comma})) {
            index_capture = try self.consume(.identifier);
        }
        _ = try self.consume(.pipe);

        const body = try self.parseBlock();

        return try self.createNode(.for_stmt, token, .{
            .for_stmt = .{
                .iterable = iterable,
                .body = body,
                .value_capture = value_capture,
                .is_value_ptr = is_value_ptr,
                .index_capture = index_capture,
            },
        });
    }

    fn parseSwitch(self: *Parser) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        _ = try self.consume(.l_paren);
        const target = try self.parseExpression(.lowest);
        _ = try self.consume(.r_paren);

        var cases: std.ArrayList(Node.SwitchCase) = .empty;

        _ = try self.consume(.l_brace);
        while (!self.check(&.{ .r_brace, .eof })) {
            var variants: std.ArrayList(*Node) = .empty;

            const is_else = self.match(&.{.keyword_else});

            if (!is_else) while (true) {
                var variant = try self.parseExpression(.lowest);
                if (self.check(&.{.dot_dot})) {
                    const range_token = self.curr;
                    self.advance();

                    const r_expr = try self.parseExpression(.lowest);
                    const range = try self.createNode(.range_pattern, range_token, .{
                        .range_pattern = .{
                            .start = variant,
                            .end = r_expr,
                        },
                    });

                    variant = range;
                }

                try variants.append(self.arena, variant);

                if (!self.match(&.{.comma})) break;
            };

            _ = try self.consume(.arrow);

            const body = try self.parseExpression(.lowest);

            _ = try self.consume(.comma);

            try cases.append(self.arena, .{
                .variants = try variants.toOwnedSlice(self.arena),
                .body = body,
                .is_else = is_else,
            });
        }

        _ = try self.consume(.r_brace);

        return try self.createNode(.switch_stmt, token, .{
            .switch_stmt = .{
                .target = target,
                .cases = try cases.toOwnedSlice(self.arena),
            },
        });
    }

    fn parseReturn(self: *Parser) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        var value: ?*Node = null;

        if (!self.check(&.{.semicolon})) {
            value = try self.parseExpression(.lowest);
        }

        _ = try self.consume(.semicolon);

        return self.createNode(.return_stmt, token, .{
            .return_stmt = value,
        });
    }

    fn parseBreak(self: *Parser) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        _ = try self.consume(.semicolon);
        return self.createNode(.break_stmt, token, .{ .break_stmt = {} });
    }

    fn parseContinue(self: *Parser) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        _ = try self.consume(.semicolon);
        return self.createNode(.continue_stmt, token, .{ .continue_stmt = {} });
    }

    fn parseDefer(self: *Parser) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        const body = try self.parseStatement();

        return self.createNode(.defer_stmt, token, .{
            .defer_stmt = .{
                .body = body,
                .is_errdefer = (token.tag == .keyword_errdefer),
            },
        });
    }

    fn parseAsm(self: *Parser) Parser.Error!*Node {
        const start_token = self.curr;
        self.advance();

        var is_pure = false;

        if (self.match(&.{.dot})) {
            if (self.match(&.{.keyword_pure})) {
                is_pure = true;
            }
        }

        _ = try self.consume(.l_brace);

        const body = try self.parseExpression(.lowest);

        var operands: std.ArrayList(Node.AsmOperand) = .empty;
        var clobbers: std.ArrayList(Token) = .empty;

        if (self.match(&.{.colon})) {
            while (!self.check(&.{ .colon, .r_brace, .eof })) {
                _ = try self.consume(.l_bracket);
                const symbol = try self.consume(.identifier);
                _ = try self.consume(.r_bracket);

                const constraint = try self.consume(.string_literal);

                _ = try self.consume(.l_paren);
                const expr = try self.parseExpression(.lowest);
                _ = try self.consume(.r_paren);

                try operands.append(self.arena, .{
                    .symbol = symbol,
                    .constraint = constraint,
                    .expr = expr,
                });

                if (!self.match(&.{.comma})) break;
            }
        }

        if (self.match(&.{.colon})) {
            while (!self.check(&.{ .r_brace, .eof })) {
                const clobber = try self.consume(.string_literal);
                try clobbers.append(self.arena, clobber);

                if (!self.match(&.{.comma})) break;
            }
        }

        _ = try self.consume(.r_brace);

        _ = self.match(&.{.semicolon});

        return try self.createNode(.asm_block, start_token, .{
            .asm_block = .{
                .is_pure = is_pure,
                .body = body,
                .operands = try operands.toOwnedSlice(self.arena),
                .clobbers = try clobbers.toOwnedSlice(self.arena),
            },
        });
    }

    fn parseAnonymousInit(self: *Parser) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        if (self.match(&.{.l_brace})) {
            // Anonymous struct, union init
            var field_assignments: std.ArrayList(*Node) = .empty;
            while (!self.check(&.{ .r_brace, .eof })) {
                const tok = try self.consume(.dot);
                const identifier = try self.consume(.identifier);
                _ = try self.consume(.equal);
                const value = try self.parseExpression(.lowest);

                _ = try self.consume(.comma);

                try field_assignments.append(self.arena, try self.createNode(
                    .field_assignment,
                    tok,
                    .{
                        .field_assignment = .{
                            .identifier = identifier,
                            .value = value,
                        },
                    },
                ));
            }

            _ = try self.consume(.r_brace);

            return try self.createNode(.struct_init, token, .{
                .struct_init = .{
                    .fields = try field_assignments.toOwnedSlice(self.arena),
                },
            });
        } else {
            // Enum member
            const member = try self.consume(.identifier);
            return try self.createNode(.enum_member, member, .{ .none = {} });
        }
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

        if (self.check(Token.Tag.assignment_operators)) {
            const operator_token = self.curr;
            self.advance();

            const rhs = try self.parseExpression(.lowest);

            _ = try self.consume(.semicolon);

            return self.createNode(.assignment, operator_token, .{
                .assignment = .{
                    .identifier = lhs,
                    .assignment_expr = rhs,
                    .operator = operator_token,
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
        const token = self.curr;
        self.advance();

        var args: std.ArrayList(*Node) = .empty;

        if (!self.check(&.{.r_paren})) {
            while (true) {
                const arg = try self.parseExpression(.lowest);
                try args.append(self.arena, arg);

                if (!self.match(&.{.comma})) break;
            }
        }

        _ = try self.consume(.r_paren);

        return self.createNode(.fn_call, token, .{
            .fn_call = .{
                .lhs = left,
                .args = try args.toOwnedSlice(self.arena),
            },
        });
    }

    fn parseIndex(self: *Parser, left: *Node) Parser.Error!*Node {
        const token = self.curr;
        self.advance();

        const start = try self.parseExpression(.lowest);

        if (self.match(&.{.dot_dot})) {
            var end: ?*Node = null;
            if (!self.check(&.{.r_bracket})) {
                end = try self.parseExpression(.lowest);
            }

            _ = try self.consume(.r_bracket);

            return self.createNode(.slice_access, token, .{
                .slice_access = .{
                    .lhs = left,
                    .start = start,
                    .end = end,
                },
            });
        }

        _ = try self.consume(.r_bracket);

        return self.createNode(.index_access, token, .{
            .index_access = .{
                .lhs = left,
                .index = start,
            },
        });
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
