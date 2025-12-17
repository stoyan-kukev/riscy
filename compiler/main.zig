const std = @import("std");

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
        const source = try std.fs.cwd().readFileAlloc(allocator, args[1], std.math.maxInt(usize));
        defer allocator.free(source);

        std.debug.print("{s}\n", .{source});
    }
}
