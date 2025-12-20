const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub fn format(this: @This(), w: *std.Io.Writer) !void {
        try w.print("{s}", .{@tagName(this.tag)});
    }

    pub const Loc = struct {
        start: usize,
        len: usize,
        line: u32,
        column: u32,
    };

    /// Returns the slice of source code corresponding to this token.
    pub fn lexeme(self: Token, source: []const u8) []const u8 {
        return source[self.loc.start .. self.loc.start + self.loc.len];
    }

    pub const Tag = enum {
        // Storage & Visibility
        @"const",
        @"var",
        @"pub",

        // Control Flow
        @"fn",
        @"return",
        @"if",
        @"else",
        @"while",
        @"for",
        @"switch",
        @"break",
        @"continue",

        // Data Structures
        @"struct",
        @"enum",
        @"union",
        @"asm",

        // Values & Logic
        true,
        false,
        null,
        undefined,
        @"error",
        @"and",
        @"or",
        not,

        // Contextual Modifiers
        @"packed",
        @"volatile",
        @"allowzero",
        c_abi,
        naked,
        interrupt,
        @"align",
        @"linksection",
        @"extern",
        @"export",

        // Control Flow (Extended)
        @"defer",
        @"errdefer",

        // Punctuation
        l_brace,
        r_brace,
        l_paren,
        r_paren,
        l_bracket,
        r_bracket,
        semicolon,
        colon,
        comma,
        dot,
        ellipsis,
        arrow,

        // Operators
        equal,
        double_equal,
        bang,
        bang_equal,
        less,
        less_equal,
        shift_left,
        greater,
        greater_equal,
        shift_right,
        plus_equal,
        minus_equal,
        asterisk_equal,
        slash_equal,

        plus,
        minus,
        asterisk,
        slash,
        percent,
        ampersand,
        pipe,
        caret,
        tilde,
        question_mark,
        at_sign,

        // Literals
        identifier,
        string_literal,
        multiline_string_literal,
        int_literal,

        // Meta
        eof,
        invalid,
    };
};

pub const Tokenizer = struct {
    source: []const u8,
    index: usize,
    line: u32,
    column: u32,
    start_index: usize,
    start_col: u32,

    pub fn init(source: []const u8) Tokenizer {
        // Skip UTF-8 BOM if present
        const idx: usize = if (std.mem.startsWith(u8, source, "\xEF\xBB\xBF")) 3 else 0;

        return .{
            .source = source,
            .index = idx,
            .line = 1,
            .column = 1,
            .start_index = idx,
            .start_col = 1,
        };
    }

    /// Helper for tests/debugging to get all tokens at once
    pub fn tokenize(self: *Tokenizer, allocator: std.mem.Allocator) !std.ArrayList(Token) {
        var tokens: std.ArrayList(Token) = .empty;

        while (true) {
            const token = self.next();
            try tokens.append(allocator, token);
            if (token.tag == .eof) break;
        }

        return tokens;
    }

    pub fn next(self: *Tokenizer) Token {
        self.skipWhitespace();

        self.start_index = self.index;
        self.start_col = self.column;

        if (self.isAtEnd()) return self.makeToken(.eof);

        const c = self.advance();

        if (isAlpha(c)) return self.identifier();

        if (isDigit(c)) return self.number();

        switch (c) {
            '(' => return self.makeToken(.l_paren),
            ')' => return self.makeToken(.r_paren),
            '{' => return self.makeToken(.l_brace),
            '}' => return self.makeToken(.r_brace),
            '[' => return self.makeToken(.l_bracket),
            ']' => return self.makeToken(.r_bracket),
            ';' => return self.makeToken(.semicolon),
            ':' => return self.makeToken(.colon),
            ',' => return self.makeToken(.comma),
            '.' => {
                if (self.peek() == '.' and self.peekNext() == '.') {
                    _ = self.advance();
                    _ = self.advance();
                    return self.makeToken(.ellipsis);
                }
                return self.makeToken(.dot);
            },

            '+' => return self.makeToken(if (self.match('=')) .plus_equal else .plus),
            '-' => return self.makeToken(if (self.match('=')) .minus_equal else .minus),
            '*' => return self.makeToken(if (self.match('=')) .asterisk_equal else .asterisk),
            '/' => return self.makeToken(if (self.match('=')) .slash_equal else .slash),
            '%' => return self.makeToken(.percent),

            '!' => return self.makeToken(if (self.match('=')) .bang_equal else .bang),
            '=' => {
                if (self.match('=')) return self.makeToken(.double_equal);
                if (self.match('>')) return self.makeToken(.arrow);
                return self.makeToken(.equal);
            },
            '^' => return self.makeToken(.caret),
            '~' => return self.makeToken(.tilde),
            '?' => return self.makeToken(.question_mark),
            '@' => return self.makeToken(.at_sign),
            '&' => return self.makeToken(.ampersand),
            '|' => return self.makeToken(.pipe),

            '<' => {
                if (self.match('=')) return self.makeToken(.less_equal);
                if (self.match('<')) return self.makeToken(.shift_left);
                return self.makeToken(.less);
            },
            '>' => {
                if (self.match('=')) return self.makeToken(.greater_equal);
                if (self.match('>')) return self.makeToken(.shift_right);
                return self.makeToken(.greater);
            },

            '"' => return self.string(),

            '\\' => {
                if (self.match('\\')) return self.multilineString();
                return self.makeToken(.invalid);
            },

            else => return self.makeToken(.invalid),
        }
    }

    fn identifier(self: *Tokenizer) Token {
        while (isAlphaNumeric(self.peek())) {
            _ = self.advance();
        }

        const text = self.source[self.start_index..self.index];
        const tag = keywords.get(text) orelse .identifier;

        return self.makeToken(tag);
    }

    fn number(self: *Tokenizer) Token {
        // Hex detection: 0x...
        if (self.source[self.start_index] == '0' and self.peek() == 'x') {
            _ = self.advance(); // consume 'x'
            while (isHex(self.peek()) or self.peek() == '_') {
                _ = self.advance();
            }
            return self.makeToken(.int_literal);
        }

        // Binary detection: 0b...
        if (self.source[self.start_index] == '0' and self.peek() == 'b') {
            _ = self.advance(); // consume 'b'
            while (isBinary(self.peek()) or self.peek() == '_') {
                _ = self.advance();
            }
            return self.makeToken(.int_literal);
        }

        // Decimal
        while (isDigit(self.peek()) or self.peek() == '_') {
            _ = self.advance();
        }

        // We are ignoring floats (12.34) because real men only use whole numbers
        // All my homies hate IEEE-754

        return self.makeToken(.int_literal);
    }

    fn string(self: *Tokenizer) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
                self.column = 0;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) return self.makeToken(.invalid);

        _ = self.advance();
        return self.makeToken(.string_literal);
    }

    fn multilineString(self: *Tokenizer) Token {
        while (self.peek() != '\n' and !self.isAtEnd()) {
            _ = self.advance();
        }

        return self.makeToken(.multiline_string_literal);
    }

    fn skipWhitespace(self: *Tokenizer) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    self.column = 0;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // If we are here then this is a comment and we have to skip until it ends
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn makeToken(self: *Tokenizer, tag: Token.Tag) Token {
        return .{
            .tag = tag,
            .loc = .{
                .start = self.start_index,
                .len = self.index - self.start_index,
                .line = self.line,
                .column = self.start_col,
            },
        };
    }

    fn advance(self: *Tokenizer) u8 {
        self.index += 1;
        self.column += 1;
        return self.source[self.index - 1];
    }

    fn match(self: *Tokenizer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.index] != expected) return false;
        self.index += 1;
        self.column += 1;
        return true;
    }

    fn peek(self: *Tokenizer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.index];
    }

    fn peekNext(self: *Tokenizer) u8 {
        if (self.index + 1 >= self.source.len) return 0;
        return self.source[self.index + 1];
    }

    fn isAtEnd(self: *Tokenizer) bool {
        return self.index >= self.source.len;
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            c == '_';
    }

    fn isAlphaNumeric(c: u8) bool {
        return isAlpha(c) or isDigit(c);
    }

    fn isHex(c: u8) bool {
        return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
    }

    fn isBinary(c: u8) bool {
        return c == '0' or c == '1';
    }

    const keywords = std.StaticStringMap(Token.Tag).initComptime(.{
        .{ "const", .@"const" },
        .{ "var", .@"var" },
        .{ "pub", .@"pub" },
        .{ "fn", .@"fn" },
        .{ "return", .@"return" },
        .{ "if", .@"if" },
        .{ "else", .@"else" },
        .{ "while", .@"while" },
        .{ "for", .@"for" },
        .{ "switch", .@"switch" },
        .{ "break", .@"break" },
        .{ "continue", .@"continue" },
        .{ "struct", .@"struct" },
        .{ "enum", .@"enum" },
        .{ "union", .@"union" },
        .{ "asm", .@"asm" },
        .{ "true", .true },
        .{ "false", .false },
        .{ "null", .null },
        .{ "undefined", .undefined },
        .{ "error", .@"error" },
        .{ "and", .@"and" },
        .{ "or", .@"or" },
        .{ "not", .not },
        .{ "packed", .@"packed" },
        .{ "allowzero", .@"allowzero" },
        .{ "c_abi", .c_abi },
        .{ "naked", .naked },
        .{ "interrupt", .interrupt },
        .{ "align", .@"align" },
        .{ "linksection", .@"linksection" },
        .{ "extern", .@"extern" },
        .{ "export", .@"export" },
        .{ "defer", .@"defer" },
        .{ "errdefer", .@"errdefer" },
    });
};

const testing = std.testing;

test "tokenizer - basic declarations" {
    const source = "const x: u32 = 10;";
    var t = Tokenizer.init(source);

    try expectToken(&t, .@"const", "const");
    try expectToken(&t, .identifier, "x");
    try expectToken(&t, .colon, ":");
    try expectToken(&t, .identifier, "u32");
    try expectToken(&t, .equal, "=");
    try expectToken(&t, .int_literal, "10");
    try expectToken(&t, .semicolon, ";");
    try expectToken(&t, .eof, "");
}

test "tokenizer - hex and binary" {
    const source = "0xFF 0b101 123";
    var t = Tokenizer.init(source);

    try expectToken(&t, .int_literal, "0xFF");
    try expectToken(&t, .int_literal, "0b101");
    try expectToken(&t, .int_literal, "123");
}

test "tokenizer - symbols and operators" {
    const source = "!= == << >= .?";
    var t = Tokenizer.init(source);

    try expectToken(&t, .bang_equal, "!=");
    try expectToken(&t, .double_equal, "==");
    try expectToken(&t, .shift_left, "<<");
    try expectToken(&t, .greater_equal, ">=");
    try expectToken(&t, .dot, ".");
    try expectToken(&t, .question_mark, "?");
}

test "tokenizer - strings and comments" {
    const source =
        \\// This is a comment
        \\"hello world"
        \\// Another comment
    ;
    var t = Tokenizer.init(source);

    // Comments should be skipped entirely
    try expectToken(&t, .string_literal, "\"hello world\"");
    try expectToken(&t, .eof, "");
}

test "tokenizer - keywords vs identifiers" {
    const source = "while variable true @cast";
    var t = Tokenizer.init(source);

    try expectToken(&t, .@"while", "while");
    try expectToken(&t, .identifier, "variable");
    try expectToken(&t, .true, "true");
    try expectToken(&t, .at_sign, "@");
    try expectToken(&t, .identifier, "cast");
}

test "tokenizer - os keywords" {
    const source =
        \\ export linksection(".ram_text")
        \\ const fast_handler = fn() void {};
    ;
    var t = Tokenizer.init(source);

    try expectToken(&t, .@"export", "export");
    try expectToken(&t, .@"linksection", "linksection");
    try expectToken(&t, .l_paren, "(");
    try expectToken(&t, .string_literal, "\".ram_text\"");
    try expectToken(&t, .r_paren, ")");
    try expectToken(&t, .@"const", "const");
    try expectToken(&t, .identifier, "fast_handler");
    try expectToken(&t, .equal, "=");
    try expectToken(&t, .@"fn", "fn");
    try expectToken(&t, .l_paren, "(");
    try expectToken(&t, .r_paren, ")");
    try expectToken(&t, .identifier, "void");
    try expectToken(&t, .l_brace, "{");
    try expectToken(&t, .r_brace, "}");
    try expectToken(&t, .semicolon, ";");
}

// Helper function to keep tests clean
fn expectToken(t: *Tokenizer, expected_tag: Token.Tag, expected_lexeme: []const u8) !void {
    const token = t.next();
    try testing.expectEqual(expected_tag, token.tag);
    if (token.tag != .eof) {
        try testing.expectEqualSlices(u8, expected_lexeme, token.lexeme(t.source));
    }
}

test "tokenizer - multiline strings" {
    const source =
        \\\\line1
        \\\\line2
    ;
    var t = Tokenizer.init(source);

    try expectToken(&t, .multiline_string_literal, "\\\\line1");
    try expectToken(&t, .multiline_string_literal, "\\\\line2");
    try expectToken(&t, .eof, "");
}
