const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "gb",
        .root_source_file  = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize
    });
    // const tracy_enable =
    //     b.option(bool, "tracy_enable", "Enable profiling") orelse
    //         if (optimize == .Debug) true else false;

    // const tracy = b.dependency("tracy", .{
    //     .target = target,
    //     .optimize = optimize,
    //     .tracy_enable = tracy_enable,
    // });

    // exe.root_module.addImport("tracy", tracy.module("tracy"));
    // if (tracy_enable) {
    //     exe.root_module.linkLibrary(tracy.artifact("tracy"));
    //     exe.root_module.link_libcpp = true;
    // }
    const sdl_dep = b.dependency("sdl", .{
    .target = target,
    .optimize = optimize,
    //.preferred_link_mode = .static, // or .dynamic
    });
    const sdl_lib = sdl_dep.artifact("SDL3");
    exe.root_module.linkLibrary(sdl_lib);

    // SDL_ttf Dependency
    const sdl_ttf_dep = b.dependency("sdl_ttf", .{
        .target = target,
        .optimize = optimize,
    });
    const sdl_ttf_lib = sdl_ttf_dep.artifact("SDL_ttf");
    exe.root_module.linkLibrary(sdl_ttf_lib);
    // const sdl_test_lib = sdl_dep.artifact("SDL3_test");
    exe.linkLibC();
    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const exe_unit_tests = b.addTest(.{
        .root_source_file  = b.path("src/tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}
