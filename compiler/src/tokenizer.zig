const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const keywords: std.StaticStringMap(Tag) = .initComptime(.{
        .{ "addrspace", .keyword_addrspace },
        .{ "align", .keyword_align },
        .{ "allowzero", .keyword_allowzero },
        .{ "and", .keyword_and },
        .{ "asm", .keyword_asm },
        .{ "break", .keyword_break },
        .{ "catch", .keyword_catch },
        .{ "var", .keyword_var },
        .{ "const", .keyword_const },
        .{ "continue", .keyword_continue },
        .{ "defer", .keyword_defer },
        .{ "else", .keyword_else },
        .{ "enum", .keyword_enum },
        .{ "errdefer", .keyword_errdefer },
        .{ "error", .keyword_error },
        .{ "export", .keyword_export },
        .{ "extern", .keyword_extern },
        .{ "fn", .keyword_fn },
        .{ "for", .keyword_for },
        .{ "if", .keyword_if },
        .{ "or", .keyword_or },
        .{ "pub", .keyword_pub },
        .{ "return", .keyword_return },
        .{ "linksection", .keyword_linksection },
        .{ "struct", .keyword_struct },
        .{ "switch", .keyword_switch },
        .{ "threadlocal", .keyword_threadlocal },
        .{ "union", .keyword_union },
        .{ "unreachable", .keyword_unreachable },
        .{ "while", .keyword_while },
        .{ "pure", .keyword_pure },
        .{ "true", .keyword_true },
        .{ "false", .keyword_false },
        .{ "packed", .keyword_packed },
        .{ "c_abi", .keyword_c_abi },
        .{ "naked", .keyword_naked },
        .{ "interrupt", .keyword_interrupt },
        .{ "orelse", .keyword_orelse },
        .{ "catch", .keyword_catch },
        .{ "not", .keyword_not },
        .{ "null", .keyword_null },
        .{ "undefined", .keyword_undefined },
    });

    const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const Tag = enum {
        invalid,
        identifier,
        string_literal,
        multiline_string_literal,
        char_literal,
        eof,
        builtin,
        bang,
        bang_equal,
        pipe,
        pipe_equal,
        equal,
        equal_equal,
        less,
        less_equal,
        greater,
        greater_equal,
        l_paren,
        r_paren,
        l_brace,
        r_brace,
        l_bracket,
        r_bracket,
        semicolon,
        percent,
        percent_equal,
        caret,
        caret_equal,
        plus,
        plus_equal,
        minus,
        minus_equal,
        star,
        star_equal,
        slash,
        slash_equal,
        ampersand,
        ampersand_equal,
        shift_left,
        shift_left_equal,
        shift_right,
        shift_right_equal,
        arrow,
        colon,
        comma,
        question_mark,
        dot,
        dot_dot,
        tilde,
        int_literal,
        keyword_addrspace,
        keyword_align,
        keyword_allowzero,
        keyword_and,
        keyword_asm,
        keyword_break,
        keyword_catch,
        keyword_var,
        keyword_const,
        keyword_continue,
        keyword_defer,
        keyword_else,
        keyword_enum,
        keyword_errdefer,
        keyword_error,
        keyword_export,
        keyword_extern,
        keyword_fn,
        keyword_naked,
        keyword_interrupt,
        keyword_for,
        keyword_if,
        keyword_or,
        keyword_pub,
        keyword_return,
        keyword_linksection,
        keyword_struct,
        keyword_switch,
        keyword_threadlocal,
        keyword_union,
        keyword_unreachable,
        keyword_while,
        keyword_pure,
        keyword_true,
        keyword_false,
        keyword_packed,
        keyword_c_abi,
        keyword_not,
        keyword_null,
        keyword_undefined,
        keyword_orelse,

        pub const assignment_operators: []const Tag = &.{
            .equal,
            .pipe_equal,
            .percent_equal,
            .caret_equal,
            .plus_equal,
            .minus_equal,
            .star_equal,
            .slash_equal,
            .ampersand_equal,
            .shift_left_equal,
            .shift_right_equal,
        };

        pub fn lexeme(self: Tag) ?[]const u8 {
            return switch (self) {
                .invalid,
                .identifier,
                .string_literal,
                .multiline_string_literal,
                .char_literal,
                .eof,
                .builtin,
                .int_literal,
                => null,

                .bang => "!",
                .bang_equal => "!=",
                .pipe => "|",
                .pipe_equal => "|=",
                .equal => "=",
                .equal_equal => "==",
                .less => "<",
                .less_equal => "<=",
                .greater => ">",
                .greater_equal => ">=",
                .l_paren => "(",
                .r_paren => ")",
                .l_brace => "{",
                .r_brace => "}",
                .l_bracket => "[",
                .r_bracket => "]",
                .semicolon => ";",
                .percent => "%",
                .percent_equal => "%=",
                .caret => "^",
                .caret_equal => "^=",
                .plus => "+",
                .plus_equal => "+=",
                .minus => "-",
                .minus_equal => "-=",
                .star => "*",
                .star_equal => "*=",
                .slash => "/",
                .slash_equal => "/=",
                .ampersand => "&",
                .ampersand_equal => "&=",
                .shift_left => "<<",
                .shift_left_equal => "<<=",
                .shift_right => ">>",
                .shift_right_equal => ">>=",
                .arrow => "=>",
                .colon => ":",
                .comma => ",",
                .question_mark => "?",
                .dot => ".",
                .dot_dot => "..",
                .tilde => "~",
                .keyword_addrspace => "addrspace",
                .keyword_align => "align",
                .keyword_allowzero => "allowzero",
                .keyword_and => "and",
                .keyword_asm => "asm",
                .keyword_break => "break",
                .keyword_catch => "catch",
                .keyword_var => "var",
                .keyword_const => "const",
                .keyword_continue => "continue",
                .keyword_defer => "defer",
                .keyword_else => "else",
                .keyword_enum => "enum",
                .keyword_errdefer => "errdefer",
                .keyword_error => "error",
                .keyword_export => "export",
                .keyword_extern => "extern",
                .keyword_fn => "fn",
                .keyword_for => "for",
                .keyword_if => "if",
                .keyword_or => "or",
                .keyword_pub => "pub",
                .keyword_return => "return",
                .keyword_linksection => "linksection",
                .keyword_struct => "struct",
                .keyword_switch => "switch",
                .keyword_threadlocal => "threadlocal",
                .keyword_union => "union",
                .keyword_unreachable => "unreachable",
                .keyword_while => "while",
                .keyword_pure => "pure",
                .keyword_true => "true",
                .keyword_false => "false",
                .keyword_packed => "packed",
                .keyword_c_abi => "c_abi",
                .keyword_not => "not",
                .keyword_null => "null",
                .keyword_undefined => "undefined",
                .keyword_orelse => "orelse",
            };
        }

        pub fn symbol(self: Tag) []const u8 {
            return self.lexeme() orelse switch (self) {
                .invalid => "invalid token",
                .identifier => "an identifier",
                .string_literal, .multiline_string_literal => "a string literal",
                .char_literal => "a character literal",
                .int_literal => "an int literal",
                .builtin => "a builtin function",
                .eof => "EOF",
                else => unreachable,
            };
        }
    };
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,

    const State = enum {
        start,
        identifier,
        invalid,
        string_literal,
        string_literal_backslash,
        char_literal,
        char_literal_backslash,
        multiline_string_literal,
        at_sign,
        angle_left,
        angle_left_left,
        angle_right,
        angle_right_right,
        dot,
        builtin,
        int,
        slash,
        equal,
        line_comment_start,
        line_comment,
        expect_newline,
    };

    pub fn init(buffer: [:0]const u8) Tokenizer {
        return .{
            .buffer = buffer,
            .index = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0,
        };
    }

    pub fn debugDump(self: *Tokenizer) void {
        std.debug.print("{s:<20} | {s:<10} | {s}\n", .{ "TAG", "LOC", "LEXEME" });
        std.debug.print("{s:-<20}-|-{s:-<10}-|-{s:-<20}\n", .{ "", "", "" });

        while (true) {
            const token = self.next();

            const lexeme = self.buffer[token.loc.start..token.loc.end];
            std.debug.print("{t:<20} | {d:<4}..{d:<4} | {s}\n", .{ token.tag, token.loc.start, token.loc.end, lexeme });

            if (token.tag == .eof) break;
        }
    }

    inline fn opEqual(self: *Tokenizer, base_tag: Token.Tag, equal_tag: Token.Tag) Token.Tag {
        self.index += 1;

        if (self.index < self.buffer.len and self.buffer[self.index] == '=') {
            self.index += 1;
            return equal_tag;
        }
        return base_tag;
    }

    pub fn next(self: *Tokenizer) Token {
        var result: Token = .{
            .tag = undefined,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };

        state: switch (State.start) {
            .start => switch (self.buffer[self.index]) {
                0 => if (self.index == self.buffer.len) {
                    return .{
                        .tag = .eof,
                        .loc = .{
                            .start = self.index,
                            .end = self.index,
                        },
                    };
                } else {
                    continue :state .invalid;
                },
                ' ', '\n', '\t', '\r' => {
                    self.index += 1;
                    result.loc.start = self.index;
                    continue :state .start;
                },
                '"' => {
                    result.tag = .string_literal;
                    continue :state .string_literal;
                },
                '\'' => {
                    result.tag = .char_literal;
                    continue :state .char_literal;
                },
                '\\' => {
                    result.tag = .multiline_string_literal;
                    continue :state .multiline_string_literal;
                },
                'a'...'z', 'A'...'Z', '_' => {
                    result.tag = .identifier;
                    continue :state .identifier;
                },
                '0'...'9' => {
                    result.tag = .int_literal;
                    self.index += 1;
                    continue :state .int;
                },
                '(', ')', '[', ']', ';', ',', '?', ':', '{', '}', '~' => |char| {
                    result.tag = switch (char) {
                        '(' => .l_paren,
                        ')' => .r_paren,
                        '[' => .l_bracket,
                        ']' => .r_bracket,
                        '{' => .l_brace,
                        '}' => .r_brace,
                        ';' => .semicolon,
                        ':' => .colon,
                        ',' => .comma,
                        '?' => .question_mark,
                        '~' => .tilde,
                        else => unreachable,
                    };
                    self.index += 1;
                },
                '!' => result.tag = self.opEqual(.bang, .bang_equal),
                '+' => result.tag = self.opEqual(.plus, .plus_equal),
                '-' => result.tag = self.opEqual(.minus, .minus_equal),
                '*' => result.tag = self.opEqual(.star, .star_equal),
                '%' => result.tag = self.opEqual(.percent, .percent_equal),
                '|' => result.tag = self.opEqual(.pipe, .pipe_equal),
                '^' => result.tag = self.opEqual(.caret, .caret_equal),
                '&' => result.tag = self.opEqual(.ampersand, .ampersand_equal),
                '<' => continue :state .angle_left,
                '>' => continue :state .angle_right,
                '/' => continue :state .slash,
                '.' => continue :state .dot,
                '=' => continue :state .equal,
                '@' => continue :state .at_sign,
                else => continue :state .invalid,
            },
            .invalid => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    } else {
                        continue :state .invalid;
                    },
                    '\n' => result.tag = .invalid,
                    else => continue :state .invalid,
                }
            },
            .string_literal => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else {
                            result.tag = .invalid;
                        }
                    },
                    '\n' => result.tag = .invalid,
                    '\\' => continue :state .string_literal_backslash,
                    '"' => self.index += 1,
                    0x01...0x08, 0x0b...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .string_literal,
                }
            },
            .string_literal_backslash => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0, '\n' => result.tag = .invalid,
                    else => continue :state .string_literal,
                }
            },
            .char_literal => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else {
                            result.tag = .invalid;
                        }
                    },
                    '\n' => result.tag = .invalid,
                    '\\' => continue :state .char_literal_backslash,
                    '\'' => self.index += 1,
                    0x01...0x08, 0x0b...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .char_literal,
                }
            },
            .char_literal_backslash => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else {
                            result.tag = .invalid;
                        }
                    },
                    '\n' => result.tag = .invalid,
                    0x01...0x08, 0x0b...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .char_literal,
                }
            },
            .multiline_string_literal => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        continue :state .invalid;
                    },
                    '\n' => {},
                    '\r' => if (self.buffer[self.index + 1] != '\n') {
                        continue :state .invalid;
                    },
                    0x01...0x08, 0x0b...0x0c, 0x0e...0x1f, 0x7f => continue :state .invalid,
                    else => continue :state .multiline_string_literal,
                }
            },
            .identifier => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .identifier,
                    else => {
                        const identifier = self.buffer[result.loc.start..self.index];
                        if (Token.keywords.get(identifier)) |tag| {
                            result.tag = tag;
                        }
                    },
                }
            },
            .dot => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '.' => {
                        result.tag = .dot_dot;
                        self.index += 1;
                    },
                    else => result.tag = .dot,
                }
            },
            .at_sign => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0, '\n' => result.tag = .invalid,
                    'a'...'z', 'A'...'Z', '_' => {
                        result.tag = .builtin;
                        continue :state .builtin;
                    },
                    else => continue :state .invalid,
                }
            },
            .builtin => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .builtin,
                    else => {},
                }
            },
            .int => switch (self.buffer[self.index]) {
                '_', '0'...'9', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z' => {
                    self.index += 1;
                    continue :state .int;
                },
                else => {},
            },
            .slash => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '/' => continue :state .line_comment_start,
                    '=' => {
                        result.tag = .slash_equal;
                        self.index += 1;
                    },
                    else => result.tag = .slash,
                }
            },
            .line_comment_start => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    },
                    '\n' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    '\r' => continue :state .expect_newline,
                    0x01...0x08, 0x0b...0x0c, 0x0e...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .line_comment,
                }
            },
            .line_comment => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    },
                    '\n' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    '\r' => continue :state .expect_newline,
                    0x01...0x08, 0x0b...0x0c, 0x0e...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .line_comment,
                }
            },
            .expect_newline => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index == self.buffer.len) {
                            result.tag = .invalid;
                        } else {
                            continue :state .invalid;
                        }
                    },
                    '\n' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    else => continue :state .invalid,
                }
            },
            .angle_left => {
                self.index += 1;
                if (self.index < self.buffer.len) switch (self.buffer[self.index]) {
                    '<' => continue :state .angle_left_left,
                    '=' => {
                        self.index += 1;
                        result.tag = .less_equal;
                    },
                    else => result.tag = .less,
                } else result.tag = .less;
            },
            .angle_right => {
                self.index += 1;
                if (self.index < self.buffer.len) switch (self.buffer[self.index]) {
                    '>' => continue :state .angle_right_right,
                    '=' => result.tag = .greater_equal,
                    else => result.tag = .greater,
                } else result.tag = .greater;
            },
            .angle_left_left => result.tag = self.opEqual(.shift_left, .shift_left_equal),
            .angle_right_right => result.tag = self.opEqual(.shift_right, .shift_right_equal),
            .equal => {
                self.index += 1;
                if (self.index < self.buffer.len) switch (self.buffer[self.index]) {
                    '>' => {
                        self.index += 1;
                        result.tag = .arrow;
                    },
                    '=' => {
                        self.index += 1;
                        result.tag = .equal_equal;
                    },
                    else => result.tag = .equal,
                } else result.tag = .equal;
            },
        }

        result.loc.end = self.index;
        return result;
    }
};
