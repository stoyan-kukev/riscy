const std = @import("std");

pub fn build(b: *std.Build) !void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const compiler_module = b.createModule(.{
        .root_source_file = b.path("compiler/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const debug_logging_option = b.option(
        bool,
        "debug_logging",
        "Build option to display all debug logs while running.",
    ) orelse false;

    const options = b.addOptions();
    options.addOption(bool, "debug_logging", debug_logging_option);
    compiler_module.addOptions("build_options", options);

    const compiler_exe = b.addExecutable(.{
        .name = "riscy",
        .root_module = compiler_module,
    });
    b.installArtifact(compiler_exe);

    const compiler_check = b.addExecutable(.{
        .name = "riscy_check",
        .root_module = compiler_module,
    });
    const check = b.step("check", "Check if the compiler compiles");
    check.dependOn(&compiler_check.step);

    const tests = b.addTest(.{ .root_module = compiler_module });
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);
}
