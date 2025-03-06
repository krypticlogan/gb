const std = @import("std");
const exe = @import("main.zig");
const testing = std.testing;
const print = std.debug.print;
const expect = std.testing.expect;
const regID = exe.regID;

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

test "LD16" {
    try expect(true);
}

test "LDHLIA" {
    const program: [4]u8 = .{0x21, 0x01, 0x00, 0x22}; // testing load reg a into mem@hl and increment|
    var gb = startGB(&program); // |
    gb.cpu.set_byte(regID.a, 90);
    try gb.cycle(); 
    const priorHL = gb.cpu.get_word(regID.h);
    try gb.cycle();
    const hl = gb.cpu.get_word(regID.h);
    print("hl: {d} \t newHL: {d}\n", .{priorHL, hl});
    const value = gb.cpu.get_byte(regID.a);
    expect(gb.read_byte(priorHL) == value and hl == priorHL+1) catch {
        if (gb.read_byte(priorHL) != value) return error.ValueNotSet;
        if (hl != priorHL+1) return error.noIncrement;
    };
}

test "LDHLDA" {
    const program: [4]u8 = .{0x21, 0x01, 0x00, 0x32}; // testing load reg a into mem@hl and decrement|
    var gb = startGB(&program); // |
    try gb.cycle();
    const priorHL = gb.cpu.get_word(regID.h);
    try gb.cycle(); 
    const hl = gb.cpu.get_word(regID.h);
    const value = gb.cpu.get_byte(regID.a);
    expect(gb.read_byte(priorHL) == value and hl == priorHL-1) catch {
        if (gb.read_byte(priorHL) != value) return error.ValueNotSet;
        if (hl != priorHL-1) return error.noDecrement;
    };
} 


test "XORA" {
    const program: [1]u8 = .{0xAF}; // testing register A ^ A |
    var gb = startGB(&program); // -- effectively 0 so flag should be set  |
    try gb.cycle();
    expect(gb.cpu.registers[@intFromEnum(regID.a)] == 1 and gb.cpu.f.z) catch {
        if (!gb.cpu.f.z) return error.NullZeroFlag;
        if (gb.cpu.registers[@intFromEnum(regID.a)] != 0) return error.XerrOR;
    };    
}
