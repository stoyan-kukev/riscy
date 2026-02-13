const std = @import("std");
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Parser = @import("parser.zig").Parser;

const test_source = @embedFile("test.riscy");

pub fn main() !void {
    var da: std.heap.DebugAllocator(.{}) = .init;
    defer std.debug.assert(da.deinit() == .ok);
    const allocator = da.allocator();

    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();

    const parser_arena = arena.allocator();

    var parser: Parser = .init(parser_arena, test_source);
    const root_node = try parser.parseRoot();
    _ = root_node;
}
