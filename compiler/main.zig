const std = @import("std");
const Parser = @import("parser.zig").Parser;

pub fn main() !void {
    var da: std.heap.DebugAllocator(.{}) = .{};
    defer std.debug.assert(da.deinit() == .ok);
    const allocator = da.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.log.info("Enter the name of the .ry file", .{});
        return std.process.exit(1);
    } else {
        const source = try std.fs.cwd().readFileAlloc(args[1], allocator, .unlimited);
        defer allocator.free(source);

        var parser: Parser = try .init(allocator, source);
        const program_ast = try parser.parseProgram();
        _ = program_ast;
    }
}

test {
    _ = @import("tokenizer.zig");
}
