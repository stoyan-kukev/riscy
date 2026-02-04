const std = @import("std");
const Tokenizer = @import("tokenizer.zig").Tokenizer;

const test_source = @embedFile("test.riscy");

pub fn main() !void {
    var tokenizer: Tokenizer = .init(test_source);

    std.debug.print("{s:<20} | {s:<10} | {s}\n", .{ "TAG", "LOC", "LEXEME" });
    std.debug.print("{s:-<20}-|-{s:-<10}-|-{s:-<20}\n", .{ "", "", "" });

    while (true) {
        const token = tokenizer.next();

        if (token.tag == .eof) break;

        const lexeme = test_source[token.loc.start..token.loc.end];
        std.debug.print("{t:<20} | {d:<4}..{d:<4} | {s}\n", .{ token.tag, token.loc.start, token.loc.end, lexeme });
    }
}
