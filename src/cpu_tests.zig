const std = @import("std");
const exe = @import("main.zig");
const GB = @import("gb.zig");
const CPU = @import("cpu.zig");
const INS = @import("instruction_set.zig");
const regID = CPU.regID;
const testing = std.testing;
const print = std.debug.print;
const expect = std.testing.expect;

fn startGB(program: []const u8) !GB {
    var gpa = std.heap.DebugAllocator(.{}).init;
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();
    const exe_path = try std.fs.selfExePathAlloc(allocator);
    const exe_dir = std.fs.path.dirname(exe_path).?;
    const project_root = std.fs.path.dirname(std.fs.path.dirname(std.fs.path.dirname(exe_dir).?).?).?;
    defer allocator.free(exe_path);
    var gb = GB{.allocator = allocator, .root_path = project_root};
    try gb.init(true);
    @memcpy(gb.bus.memory[0..program.len], program[0..]);
    return gb;
}
// test loads
//
test "LD8" {
    const program = [_]u8{0x0, 0x17};
    var gb = try startGB(&program);
    _ = INS.LD8(&gb.cpu, . {.target = .b} );
    try expect(gb.cpu.get_byte(.b) == 0x17);
    print("LD8 passed\n", .{});
}
test "LDr8" {
    const program = [_]u8{};
    var gb = try startGB(&program);
    gb.cpu.set_byte(.b, 0x17);
    _ = INS.LDr8(&gb.cpu, . {.targets = .{.from = .b, .to = .d} } );
    try expect(gb.cpu.get_byte(.d) == 0x17);
    print("LDr8 passed\n", .{});
}
test "LDr8HL" {
    const program = [_]u8{};
    var gb = try startGB(&program);
    gb.cpu.set_byte(.b, 0x17);
    // _ = INS.LDr8HL(&gb.cpu, . {.targets = .{.from = .b, .to = .d} } );
    try expect(gb.cpu.get_byte(.d) == 0x17);
    print("LDr8 passed\n", .{});
}
// rotate register
test "RLA" { // C <- [7 <- 0] <- C]
    const program: [4]u8 = .{0x17, 0x0, 0x0, 0x0}; // 
    var gb = try startGB(&program); // |
    gb.cpu.f.write(false, true, false, false);
    const prior_a = 0b0101_0101;
    gb.cpu.set_byte(regID.a, prior_a);
    _ = try gb.cpu.execute();
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

    _ = try gb.cpu.execute();
    const new_a = gb.cpu.get_byte(regID.a);

    const carried_valid = (prior_a >> 7) == @as(u1, @truncate(new_a));
    // print("a: 0b{b} \t new a: 0b{b}\n carried valid: {any}", .{prior_a, new_a, carried_valid});
    try expect(new_a == 0b1010_1010 and carried_valid);
}
test "RLCr8" {   // testing C <- [7 <- 0] <- 7 into A
    const program: [0]u8 = .{}; // 
    var gb = try startGB(&program); // |
    const prior = 0b0101_0101;
    gb.cpu.set_byte(regID.b, prior);
    _ = INS.RLCr8(&gb.cpu, .{ .target = .b});
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
    _ = INS.RLr8(&gb.cpu, .{ .target = .b});
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

    _ = try gb.cpu.execute();
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

    _ = try gb.cpu.execute();
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

    _ = try gb.cpu.execute();

    const new_a = gb.cpu.get_byte(.a);
    print("DAA | old a: 0x{X}, new_a: 0x{X}\n", .{prior_a, new_a});
    try expect(new_a == 0x84);
}