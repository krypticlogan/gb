const std = @import("std");
const exe = @import("main.zig");
const testing = std.testing;
const print = std.debug.print;
const expect = std.testing.expect;
const regID = exe.regID;

fn startGB(program: []const u8) !exe.GB {
    var gb = exe.GB{};
    try gb.init();
    @memcpy(gb.memory[0..program.len], program[0..]);
    return gb;
}
// test "LDSP16" {
//         const program: [3]u8 = .{0x31, 0x01, 0x00};
//         const expectN = 256;
//         var gb = startGB(&program);
//         try gb.cycle();
//         try expect(gb.cpu.sp == expectN);
// }
//
// test "LD16" {
//     try expect(true);
// }

// test "LDHLIA" {
//     const program: [4]u8 = .{0x21, 0x01, 0x00, 0x22}; // testing load reg a into mem@hl and increment|
//     var gb = startGB(&program); // |
//     gb.cpu.set_byte(regID.a, 90);
//     try gb.cycle();
//     const priorHL = gb.cpu.get_word(regID.h);
//     try gb.cycle();
//     const hl = gb.cpu.get_word(regID.h);
//     print("hl: {d} \t newHL: {d}\n", .{priorHL, hl});
//     const value = gb.cpu.get_byte(regID.a);
//     expect(gb.read_byte(priorHL) == value and hl == priorHL+1) catch {
//         if (gb.read_byte(priorHL) != value) return error.ValueNotSet;
//         if (hl != priorHL+1) return error.noIncrement;
//     };
// }

// rotate register
test "RLCA" {   // testing C <- [7 -> 0] <- C into A
    const program: [4]u8 = .{0x07, 0x0, 0x0, 0x0}; // 
    var gb = try startGB(&program); // |
    const prior_a = 0b0101_0101;
    gb.cpu.set_byte(regID.a, prior_a);

    _ = try gb.cpu.execute(&gb);
    const new_a = gb.cpu.get_byte(regID.a);

    const carried_valid = (prior_a >> 7) == @as(u1, @truncate(new_a));
    // print("a: 0b{b} \t new a: 0b{b}\ncarried valid: {any}", .{prior_a, new_a, carried_valid});
    try expect(new_a == 0b1010_1010 and carried_valid);
}

test "RRA" { // testing C -> [7 -> 0] -> C into A
    const program: [4]u8 = .{0x1F, 0x0, 0x0, 0x0};
    var gb = try startGB(&program);
    const prior_a = 0b0101_0101;
    gb.cpu.f.write(false, true, false, false);
    const prior_c = @intFromBool(gb.cpu.f.cFlag());
    gb.cpu.set_byte(regID.a, prior_a);

    _ = try gb.cpu.execute(&gb);
    const new_a = gb.cpu.get_byte(regID.a);
    //                          msb should be what c was             cFlag needs to be set by the lsb
    const carried_valid = (new_a >> 7) == prior_c and @intFromBool(gb.cpu.f.cFlag()) == @as(u1, @truncate(prior_a));
    // print("a: 0b{b} \t new a: 0b{b}\ncarried valid: {any}", .{prior_a, new_a, carried_valid});
    try expect(new_a == 0b1010_1010);
    try expect(carried_valid);
}

test "RRCA" {
    const program: [4]u8 = .{0x0F, 0x0, 0x0, 0x0}; // testing [0] -> [7 -> 0] -> C into A
    var gb = try startGB(&program); // |
    const prior_a = 0b0101_0101;
    gb.cpu.f.write(false, true, false, false);
    // const prior_c = @intFromBool(gb.cpu.f.cFlag());
    gb.cpu.set_byte(regID.a, prior_a);

    _ = try gb.cpu.execute(&gb);
    const new_a = gb.cpu.get_byte(regID.a);
    //                          msb should be 0             cFlag needs to be set by the lsb
    const carried_valid = (new_a >> 7) == 0 and @intFromBool(gb.cpu.f.cFlag()) == @as(u1, @truncate(prior_a));
    // print("a: 0b{b} \t new a: 0b{b}\ncarried valid: {any}", .{prior_a, new_a, carried_valid});
    try expect(new_a == 0b0010_1010);
    try expect(carried_valid);
}

// misc
test "DAA" {
    const program: [4]u8 = .{0x27, 0, 0, 0};
    var gb = try startGB(&program);

    const prior_a = 0xE4;
    gb.cpu.set_byte(.a, prior_a);

    const c = true;
    const z = false;
    const h = false;
    const s = true;
    gb.cpu.f.write(z, c, h, s);

    _ = try gb.cpu.execute(&gb);

    const new_a = gb.cpu.get_byte(.a);
    print("old a: 0x{X}, new_a: 0x{X}", .{prior_a, new_a});
    try expect(new_a == 0x84);
}

// test "LDHLDA" {
//     const program: [4]u8 = .{0x21, 0x01, 0x00, 0x32}; // testing load reg a into mem@hl and decrement|
//     var gb = startGB(&program); // |
//     try gb.cycle();
//     const priorHL = gb.cpu.get_word(regID.h);
//     try gb.cycle();
//     const hl = gb.cpu.get_word(regID.h);
//     const value = gb.cpu.get_byte(regID.a);
//     expect(gb.read_byte(priorHL) == value and hl == priorHL-1) catch {
//         if (gb.read_byte(priorHL) != value) return error.ValueNotSet;
//         if (hl != priorHL-1) return error.noDecrement;
//     };
// }


// test "XORA" {
//     const program: [1]u8 = .{0xAF}; // testing register A ^ A |
//     var gb = startGB(&program); // -- effectively 0 so flag should be set  |
//     try gb.cycle();
//     expect(gb.cpu.registers[@intFromEnum(regID.a)] == 1 and gb.cpu.f.z) catch {
//         if (!gb.cpu.f.z) return error.NullZeroFlag;
//         if (gb.cpu.registers[@intFromEnum(regID.a)] != 0) return error.XerrOR;
//     };
// }