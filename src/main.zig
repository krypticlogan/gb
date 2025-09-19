const std = @import("std");
const print = std.debug.print;
const GB = @import("gb.zig");
pub fn main() !void {
    // print("ns per frame: {d}", .{Clock.ns_per_frame});
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer {
        const mem_leaks = gpa.deinit();
        print("Memory leaks: {any}", .{mem_leaks});
    }
    const allocator = gpa.allocator();

    const exe_path = try std.fs.selfExePathAlloc(allocator);
    const exe_dir = std.fs.path.dirname(exe_path).?;
    const project_root = std.fs.path.dirname(std.fs.path.dirname(exe_dir).?).?;
    defer allocator.free(exe_path);
    

    var gb = GB{.allocator = allocator, .root_path = project_root};

    gb.init() catch |err| {
        print("Couldn't inititalize GameBoy, Error: {any}\n", .{err});
        return;
    };
    print("GB init!\n", .{});
    gb.boot() catch |err| { // loads the boot rom and executes it
        print("Couldn't boot GameBoy, err: {any}\t", .{err});
        return;
    };
    defer {
        print("exiting gameboy...\nlast instruction: 0x{X} @ 0x{X}\n", .{ gb.cpu.executing_byte, gb.cpu.pc });
        gb.endGB();
    }
    gb.go() catch |err| {
        // gb.gpu.vram_dump();
        print("Error while running GameBoy: {any}\n", .{err});
    };
}
