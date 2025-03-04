const std = @import("std");
const exe = @import("main.zig");
const testing = std.testing;
const print = std.debug.print;
const expect = std.testing.expect;


fn startGB(program: []const u8) exe.GB {
    var gb = exe.GB{};
    try gb.init();
    @memcpy(gb.memory[0..program.len], program[0..]);
    return gb;
}
test "LDSP16" {
        const program: [3]u8 = .{0x31, 0x01, 0x00};
        const expectN = 256;
        var gb = startGB(&program);
        try gb.cycle();
        try expect(gb.cpu.sp == expectN);
}

test "XORA" {
    const program: [1]u8 = .{0xAF}; // testing register A ^ A |
    var gb = startGB(&program); // -- effectively 0 so flag should be set  |
    try gb.cycle();
    expect(gb.cpu.registers[@intFromEnum(exe.regID.a)] == 0 and gb.cpu.f.z) catch {
        if (!gb.cpu.f.z) return error.NullZeroFlag;
        if (gb.cpu.registers[@intFromEnum(exe.regID.a)] != 0) return error.XerrOR;
    };    
}
