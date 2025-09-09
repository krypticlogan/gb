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

// rotate register
test "RLA" { // C <- [7 <- 0] <- C]
    const program: [4]u8 = .{0x17, 0x0, 0x0, 0x0}; // 
    var gb = try startGB(&program); // |
    gb.cpu.f.write(false, true, false, false);
    const prior_a = 0b0101_0101;
    gb.cpu.set_byte(regID.a, prior_a);
    _ = try gb.cpu.execute(&gb);
    const carried_valid = @intFromBool(gb.cpu.f.cFlag()) == prior_a >> 7;
    try expect(carried_valid);
    print("RLA: a = 0b{b}\n", .{gb.cpu.get_byte(.a)});
    try expect(gb.cpu.get_byte(.a) == 0b1010_1011);
}
test "RLCA" {   // testing C <- [7 <- 0] <- 7 into A
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
test "RLCr8" {   // testing C <- [7 <- 0] <- 7 into A
    const program: [0]u8 = .{}; // 
    var gb = try startGB(&program); // |
    const prior = 0b0101_0101;
    gb.cpu.set_byte(regID.b, prior);

    _ = exe.InstructionSet.RLCr8(&gb, .{ .target = .b});
    const new = gb.cpu.get_byte(regID.b);

    const carried_valid = 0 == @as(u1, @truncate(new));
    print("RLCr8: prior: 0b{b} \t new: 0b{b}\ncarried valid: {any}", .{prior, new, carried_valid});
    try expect(new == 0b1010_1010);
    try expect(carried_valid);
}
test "RLr8" {   // testing C <- [7 <- 0] <- C into A
    const program: [0]u8 = .{}; // 
    var gb = try startGB(&program); // |
    const prior = 0b0101_0101;
    gb.cpu.set_byte(regID.b, prior);
    gb.cpu.f.write(false, true, false, false);
    _ = exe.InstructionSet.RLr8(&gb, .{ .target = .b});
    const new = gb.cpu.get_byte(regID.b);

    const carried_valid = 1 == @as(u1, @truncate(new));
    print("RLr8: prior: 0b{b} \t new: 0b{b}\ncarried valid: {any}", .{prior, new, carried_valid});
    try expect(new == 0b1010_1011);
    try expect(carried_valid);
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
    const carried_valid = (new_a >> 7) == @as(u1, @truncate(prior_a)) and @intFromBool(gb.cpu.f.cFlag()) == @as(u1, @truncate(prior_a));
    print("RRCA | a: 0b{b} \t new a: 0b{b}\ncarried valid: {any}\n", .{prior_a, new_a, carried_valid});
    try expect(new_a == 0b1010_1010);
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
    print("DAA | old a: 0x{X}, new_a: 0x{X}\n", .{prior_a, new_a});
    try expect(new_a == 0x84);
}