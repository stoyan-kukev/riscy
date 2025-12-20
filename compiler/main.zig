const std = @import("std");
const Parser = @import("parser.zig").Parser;
const AstPrinter = @import("ast_printer.zig").AstPrinter;
const build_options = @import("build_options");

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
        defer parser.deinit();

        const program_ast = try parser.parseProgram();

        if (build_options.debug_logging) {
            var stderr_buffer: [4096]u8 = undefined;
            var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
            const stderr = &stderr_writer.interface;

            var printer: AstPrinter = .init(allocator, source, stderr);
            try printer.printProgram(program_ast);
            try stderr.flush();
        }
    }
}

test {
    _ = @import("tokenizer.zig");
}
