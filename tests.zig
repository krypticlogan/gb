const std = @import("std");
const exe = @import("main.zig");
const testing = std.testing;
const print = std.debug.print;

var gb = exe.GB{};

test "LDSP16" {
        try gb.init();
        const program: [3]u8 = .{0x31, 0x01, 0x00};
        const expectN = 256;
        @memcpy(gb.memory[0..program.len], program[0..]);
        try gb.cycle();
        try std.testing.expect(gb.cpu.sp == expectN);
}
// test "simple test" {
//     var list = std.ArrayList(i32).init(std.testing.allocator);
//     defer list.deinit(); // Try commenting this out and see if zig detects the memory leak!
//     try list.append(42);
//     try std.testing.expectEqual(@as(i32, 42), list.pop());
// }

// test "add" {
//     const a = 254;
//     const b = 1;
//     const res: u8 = try exe.add(a, b);
//     // print("expect: {d}, res: {d}\n", .{@as(u64, res), @as(u64 ,@intCast(a + b))});
//     try testing.expect(@as(u64, res) == @as(u64 ,@intCast(a + b)));
// }

// test "fuzz example" {
//     const Context = struct {
//         fn testOne(context: @This(), input: []const u8) anyerror!void {
//             _ = context;
//             // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
//             try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
//         }
//     };
//     try std.testing.fuzz(Context{}, Context.testOne, .{});
// }
