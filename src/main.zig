const std = @import("std");
const tracy = @import("tracy");
const g = @cImport({
    @cDefine("SDL_DISABLE_OLD_NAMES", {});
    @cInclude("SDL3/SDL.h");
    @cInclude("SDL3/SDL_revision.h");
    @cDefine("SDL_MAIN_HANDLED", {});
    @cInclude("SDL3/SDL_main.h");
});

const DEBUG = true; // Set to false to disable debug prints
fn print(comptime fmt: []const u8, args: anytype) void {
    if (DEBUG) std.debug.print(fmt, args);
}
const allocator = std.heap.page_allocator;

const FlagRegister = struct {
    value: u8 = 0,
    z: bool = false,
    s: bool = false,
    h: bool = false,
    c: bool = false,
    const Conditions = union(enum) { z: bool, c: bool, s: bool, h: bool, none: bool };

    fn update(self: *@This()) void {
        self.z = (self.value >> 7) & 0b1 == 1;
        self.s = (self.value >> 6) & 0b1 == 1;
        self.h = (self.value >> 5) & 0b1 == 1;
        self.c = (self.value >> 4) & 0b1 == 1;
    }
    fn write(self: *@This()) void {
        self.value =
            @as(u8, @intFromBool(self.z)) << 7 |
            @as(u8, @intFromBool(self.s)) << 6 |
            @as(u8, @intFromBool(self.h)) << 5 |
            @as(u8, @intFromBool(self.c)) << 4;
        self.update();
        // print("new flags: z: {any}, c: {any}, s: {any}, h: {any}\n", .{ self.z, self.c, self.s, self.h });
    }
    fn check(self: *@This(), condition: Conditions) bool {
        var ret = true;
        switch (condition) {
            .h => |*h| {
                if (h.*) {
                    // print("h, flag:{any}\n", .{self.h});
                    if (!self.h) ret = false;
                } else {
                    // print("not h flag:{any}\n", .{self.h});
                    if (self.h) ret = false;
                }
            },
            .z => |*z| {
                if (z.*) {
                    // print("z flag:{any}\n", .{self.z});
                    if (!self.z) ret = false;
                } else {
                    // print("not z flag:{any}\n", .{self.z});
                    if (self.z) ret = false;
                }
            },
            .c => |*c| {
                if (c.*) {
                    // print("c flag:{any}\n", .{self.c});
                    if (!self.c) ret = false;
                } else {
                    // print("not c flag:{any}\n", .{self.c});
                    if (self.c) ret = false;
                }
            },
            .s => |*s| {
                if (s.*) {
                    // print("s flag:{any}\n", .{self.s});
                    if (!self.s) ret = false;
                } else {
                    // print("not s flag:{any}\n", .{self.s});
                    if (self.s) ret = false;
                }
            },
            .none => {
                // print("no condition, just jump\n", .{}),
            },
        }
        return ret;
    }
};

const InstructionSet = struct {
    const InstrFn = *const fn (*GB, InstrArgs) u8;
    const InstrArgs = union(enum) { none: void, target: regID, bit_target: struct { bit: u3, target: regID }, flagConditions: FlagRegister.Conditions, targets: struct { to: regID, from: regID } };
    fn NOP(gb: *GB, _: InstrArgs) u8 { // TODO test
        const zone = tracy.beginZone(@src(), .{ .name = "NOP" });
        defer zone.end();
        // print("NOP\n", .{});
        gb.cpu.pc += 1;
        return 1;
    }
    fn INCr8(gb: *GB, args: InstrArgs) u8 { // TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "INCr8" });
        defer zone.end();
        const value = gb.cpu.get_byte(args.target);
        // print("INCr8, target: {any}\n", .{args.target});
        gb.cpu.set_byte(args.target, @addWithOverflow(value, 1)[0]);
        gb.cpu.f.h = (value & 0xF + 1) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.z = gb.cpu.get_byte(args.target) == 0;
        gb.cpu.f.s = false;
        gb.cpu.f.write();
        gb.cpu.pc += 1;
        return 1;
    }
    fn INCr16(gb: *GB, args: InstrArgs) u8 { // TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "INCr16" });
        defer zone.end();
        const value = gb.cpu.get_word(args.target);
        const res = @addWithOverflow(value, 1)[0];
        // print("INCr16, target: {any}\n 0x{X} + 1 = 0x{X}", .{ args.target, value, res });
        gb.cpu.set_word(args.target, res);
        gb.cpu.pc += 1;
        return 2;
    }
    fn DECr8(gb: *GB, args: InstrArgs) u8 { // TODO TEST -- overflow? r16 too
        const zone = tracy.beginZone(@src(), .{ .name = "DECr8" });
        defer zone.end();
        const value = gb.cpu.get_byte(args.target);
        // print("DECr8, target: {any} ({d} -= 1)\n", .{ args.target, gb.cpu.get_byte(args.target) });
        const res = @subWithOverflow(value, 1)[0];
        gb.cpu.set_byte(args.target, res);
        gb.cpu.f.z = res == 0;
        gb.cpu.f.s = true;
        gb.cpu.f.h = (value & 0xF) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.write();
        gb.cpu.pc += 1;
        return 1;
    }
    fn DECr16(gb: *GB, args: InstrArgs) u8 { // TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "DECr16" });
        defer zone.end();
        const value = gb.cpu.get_byte(args.target);
        // print("DECr16, target: {any}\n", .{args.target});
        gb.cpu.set_word(args.target, @subWithOverflow(value, 1)[0]);
        gb.cpu.pc += 1;
        return 2;
    }
    fn LD8(gb: *GB, args: InstrArgs) u8 { // LD r8, n8 TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "LD8" });
        defer zone.end();
        const n: u8 = gb.read_byte(gb.cpu.pc + 1);
        // print("LD8, target {any}, n: Ox{X}\n", .{ @as(regID, args.target), n });
        // print("n b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.set_byte(args.target, n);
        gb.cpu.pc += 2;
        return 2;
    }
    fn LD16(gb: *GB, args: InstrArgs) u8 { // LD r16, n16 TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "LD16" });
        defer zone.end();
        const n: u16 = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
        // print("LD16, target {any}, n: Ox{X}\n", .{ @as(regID, args.target), n });
        // print("n b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.read_byte(gb.cpu.pc + 2) });
        // print("n b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.set_word(args.target, n);
        gb.cpu.pc += 3;
        return 3;
    }
    fn LDr8(gb: *GB, args: InstrArgs) u8 { // LD r8, r8 TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "LDr8" });
        defer zone.end();
        // print("LDr8, targets: from {any} --> to {any}\n", .{ args.targets.from, args.targets.to });
        // print("Values, from {any} --> to {any}\n", .{ gb.cpu.get_byte(args.targets.to), gb.cpu.get_byte(args.targets.from) });
        gb.cpu.set_byte(args.targets.to, gb.cpu.get_byte(args.targets.from));
        gb.cpu.pc += 1;
        return 1;
    }
    fn LDAHL(gb: *GB, _: InstrArgs) u8 { // LD r8, r8 TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "LDAHL" });
        defer zone.end();
        const mem_place = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(regID.a);
        // print("LDHL, mem@hl:0x{X} --> to A\n", .{mem_place});
        // print("Values, from {any} --> to {any}\n", .{ gb.cpu.get_byte(regID.a), gb.read_byte(mem_place) });
        gb.writeByte(mem_place, value);
        gb.cpu.pc += 1;
        return 2;
    }
    fn LDSP16(gb: *GB, _: InstrArgs) u8 { // LD r16, n16, 0x31
        const zone = tracy.beginZone(@src(), .{ .name = "LDSP16" });
        defer zone.end();
        // print("LD16SP\n", .{});
        const n: u16 = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
        // print("n b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.read_byte(gb.cpu.pc + 2) });
        // print("n b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.sp = n;
        // print("after op: sp: {d}\n", .{gb.cpu.sp});
        gb.cpu.pc += 3;
        return 3;
    }
    fn LDHL8(gb: *GB, _: InstrArgs) u8 { // LD[HL], n8
        const zone = tracy.beginZone(@src(), .{ .name = "LDHL8" });
        defer zone.end();
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.read_byte(gb.cpu.pc + 1);
        // print("LDHLDA\thl:0x{X}\tvalue:0x{x}\nmem@hl: 0x{x}\n", .{ hl, value, gb.read_byte(hl) });
        gb.writeByte(hl, value);
        // print("after op: mem@hl: 0x{X}\thl:{d}\n", .{ hl, gb.read_byte(hl), hl });
        gb.cpu.pc += 2;
        return 3;
    }
    fn LDHLR(gb: *GB, args: InstrArgs) u8 { // LD[HL],r8
        const zone = tracy.beginZone(@src(), .{ .name = "LDHLR" });
        defer zone.end();
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(args.target);
        // print("LDHLR, \nmem@0x{X}: 0x{X} --> 0x{X}\n", .{ hl, gb.read_byte(hl), value });
        gb.writeByte(hl, value);
        // print("after op: mem@0x{X}: 0x{X}\n", .{ hl, gb.read_byte(hl) });
        gb.cpu.pc += 1;
        return 2;
    }
    fn LDHLIA(gb: *GB, _: InstrArgs) u8 { // LD [HLI],A
        const zone = tracy.beginZone(@src(), .{ .name = "LDHLIA" });
        defer zone.end();
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(regID.a);
        // print("LDHLIA, \nmem@0x{X}: 0x{X} --> 0x{X}\n", .{ hl, gb.read_byte(hl), value });
        gb.writeByte(hl, value);
        gb.cpu.set_word(regID.h, hl + 1);
        // print("after op: hl+1= 0x{X}\t mem@0x{X}: 0x{X}\n", .{ gb.cpu.get_word(regID.h), hl, gb.read_byte(hl) });
        gb.cpu.pc += 1;
        return 2;
    }
    fn LDHLDA(gb: *GB, _: InstrArgs) u8 { // LD [HLD],A
        const zone = tracy.beginZone(@src(), .{ .name = "LDHLDA" });
        defer zone.end();
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(regID.a);
        // print("LDHLDA, \nmem@0x{X}: 0x{X} --> 0x{X}\n", .{ hl, gb.read_byte(hl), value });
        gb.writeByte(hl, value);
        gb.cpu.set_word(regID.h, hl - 1);
        // print("after op: mem@0x{X}: 0x{X}\n", .{ hl, gb.read_byte(hl) });
        gb.cpu.pc += 1;
        return 2;
    }
    fn LDHCA(gb: *GB, _: InstrArgs) u8 {
        const zone = tracy.beginZone(@src(), .{ .name = "LDHCA" });
        defer zone.end();
        const c = gb.cpu.get_byte(regID.c);
        const a = gb.cpu.get_byte(regID.a);
        const mem_place = 0xFF00 + @as(u16, c);
        // print("LDHCA, memplace@0x{X} --> 0x{X}\n", .{ mem_place, a });
        gb.writeByte(mem_place, a);
        gb.cpu.pc += 1;
        return 2;
    }
    fn LDHAC(gb: *GB, _: InstrArgs) u8 { // Load value in register A from the byte at address $FF00+c
        const zone = tracy.beginZone(@src(), .{ .name = "LDHAC" });
        defer zone.end();
        // const a = gb.cpu.get_byte(regID.a);
        const c = gb.cpu.get_byte(regID.c);
        const byte = gb.read_byte(0xFF00 + @as(u16, c));
        // print("LDHAC byte: 0x{X} --> A\n", .{byte});
        gb.cpu.set_byte(regID.a, byte);
        gb.cpu.pc += 1;
        return 2;
    }
    fn LDAn16(gb: *GB, _: InstrArgs) u8 { // TODO TEST Load value in register A from the byte at address n16.
        const zone = tracy.beginZone(@src(), .{ .name = "LDAn16" });
        defer zone.end();
        const memory_place = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
        const n = gb.read_byte(memory_place);
        // print("LDAn16, n: Ox{X} --> A\n", .{n});
        // print("memplace b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.read_byte(gb.cpu.pc + 2) });
        // print("memplace b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.set_byte(regID.a, n);
        gb.cpu.pc += 3;
        return 4;
    }
    fn LDHAn16(gb: *GB, _: InstrArgs) u8 { // TODO TEST same as above, provided the address is between $FF00 and $FFFF.
        const zone = tracy.beginZone(@src(), .{ .name = "LDHAn16" });
        defer zone.end();
        const memory_place = 0xFF00 + @as(u16, gb.read_byte(gb.cpu.pc + 1));
        // print("LDHAn16, \n", .{});
        // print("memplace b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, memory_place });
        // print("memplace b2: [pc]{d} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        if (memory_place >= 0xFF00 and memory_place <= 0xFFFF) {
            const n = gb.read_byte(memory_place);
            // print("n: 0x{X} --> A\n", .{n});
            gb.cpu.set_byte(regID.a, n);
        }
        gb.cpu.pc += 2;
        return 3;
    }
    fn LDn16A(gb: *GB, _: InstrArgs) u8 { // TODO TEST Store value in register A into the byte at address n16.
        const zone = tracy.beginZone(@src(), .{ .name = "LDn16A" });
        defer zone.end();
        const memory_place = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
        const n = gb.cpu.get_byte(regID.a);
        gb.writeByte(memory_place, n);
        // print("LDn16A, n: Ox{X} --> memplace@{X}", .{ n, memory_place });
        // print("memplace b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.read_byte(gb.cpu.pc + 2) });
        // print("memplace b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.pc += 3;
        return 4;
    }
    fn LDHn16A(gb: *GB, _: InstrArgs) u8 { // TODO TEST same as above, provided the address is between $FF00 and $FFFF.
        const zone = tracy.beginZone(@src(), .{ .name = "LDHn16A" });
        defer zone.end();
        // print("LDHn16A\n", .{});
        const memory_place = 0xFF00 + @as(u16, gb.read_byte(gb.cpu.pc + 1));
        // print("memplace b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, memory_place });
        // print("memplace b2: [pc]{d} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        if (memory_place >= 0xFF00 and memory_place <= 0xFFFF) {
            const n = gb.cpu.get_byte(regID.a);
            gb.writeByte(memory_place, n);
            // print("n: 0x{X} --> memplace@0x{X}\n", .{ n, memory_place });
        }
        gb.cpu.pc += 2;
        return 3;
    }
    fn LDAr16(gb: *GB, args: InstrArgs) u8 { // TODO TEST Load value in register A from the byte pointed to by register r16.
        const zone = tracy.beginZone(@src(), .{ .name = "LDAr16" });
        defer zone.end();
        const memory_place = gb.cpu.get_word(args.target);
        const n = gb.read_byte(memory_place);
        // print("LDAr16, n: 0x{X} --> A\n", .{n});
        gb.cpu.set_byte(regID.a, n);
        gb.cpu.pc += 1;
        return 2;
    }
    fn LDr16A(gb: *GB, args: InstrArgs) u8 { // TODO TEST Store value in register A into the byte pointed to by register r16.
        const zone = tracy.beginZone(@src(), .{ .name = "LDr16A" });
        defer zone.end();
        const memory_place = gb.cpu.get_word(args.target);
        const n = gb.cpu.get_byte(regID.a);
        gb.writeByte(memory_place, n);
        // print("LDr16A, n: 0x{X} --> memplace@0x{X}\n", .{ n, memory_place });
        gb.cpu.pc += 1;
        return 2;
    }

    fn XORA(gb: *GB, args: InstrArgs) u8 {
        const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
        defer zone.end();
        // print("XORA, target {any}\n", .{args.target});
        gb.cpu.registers[@intFromEnum(regID.a)] ^= gb.cpu.registers[@intFromEnum(args.target)];
        if (gb.cpu.registers[@intFromEnum(regID.a)] == 0) {
            gb.cpu.f.z = true;
            gb.cpu.f.write();
        }
        gb.cpu.pc += 1;
        return 1;
    }
    fn ADDAr8(gb: *GB, args: InstrArgs) u8 { // TODO finish, TEST, flags
        const zone = tracy.beginZone(@src(), .{ .name = "ADDAr8" });
        defer zone.end();
        const value = gb.cpu.get_byte(args.target);
        // print("ADDAr8 target: {any}, value: {d} \n", .{ args.target, value });
        const a = gb.cpu.get_byte(regID.a);
        const res: struct { u8, u1 } = @addWithOverflow(a, value);
        gb.cpu.f.s = false;
        gb.cpu.f.c = res[1] == 1;
        gb.cpu.f.h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.z = res[0] == 0;
        gb.cpu.f.write();
        gb.cpu.set_byte(regID.a, res[0]);
        gb.cpu.pc += 1;
        return 1;
    }
    fn SUBAr8(gb: *GB, args: InstrArgs) u8 { // TODO finish, TEST, flags
        const zone = tracy.beginZone(@src(), .{ .name = "SUBAr8" });
        defer zone.end();
        const value = gb.cpu.get_byte(args.target);
        // print("SUBA target: {any}, value: {d} \n", .{ args.target, value });
        const a = gb.cpu.get_byte(regID.a);
        const res: struct { u8, u1 } = @subWithOverflow(a, value);
        gb.cpu.f.c = value > a;
        gb.cpu.f.s = true;
        gb.cpu.f.h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.z = res[0] == 0;
        gb.cpu.f.write();
        gb.cpu.set_byte(regID.a, res[0]);
        gb.cpu.pc += 1;
        return 1;
    }
    fn ADDAHL(gb: *GB, _: InstrArgs) u8 { // TODO Add the byte pointed to by HL to A.
        const zone = tracy.beginZone(@src(), .{ .name = "ADDAHL" });
        defer zone.end();
        const mem_place = gb.cpu.get_word(regID.h);
        const value = gb.read_byte(mem_place);
        // print("ADDAHL:A + mem@0x{X}: value: {d} \n", .{ mem_place, value });
        const a = gb.cpu.get_byte(regID.a);
        const res: struct { u8, u1 } = @addWithOverflow(a, value);
        gb.cpu.f.s = false;
        gb.cpu.f.c = res[1] == 1;
        gb.cpu.f.h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.z = res[0] == 0;
        gb.cpu.f.write();
        gb.cpu.set_byte(regID.a, res[0]);
        gb.cpu.pc += 1;
        return 2;
    }
    fn ADDHLr16(gb: *GB, args: InstrArgs) u8 { // TODO finish, TEST, flags
        const zone = tracy.beginZone(@src(), .{ .name = "ADDHLr16" });
        defer zone.end();
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_word(args.target);
        // print("ADDHL {any} + hl, {d} + {d} \n", .{ args.target, value, hl });
        const res: u16 = @addWithOverflow(hl, value)[0];
        gb.cpu.f.s = false;
        gb.cpu.f.h = (((hl + value) >> 8) & 0xF) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.c = (((hl + value) >> 12) & 0xF) & 0x10 == 0x10;
        gb.cpu.f.write();
        gb.cpu.set_word(regID.h, res);
        gb.cpu.pc += 1;
        return 2;
    }
    fn EI(gb: *GB, _: InstrArgs) u8 { // TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "EI" });
        defer zone.end();
        // print("EI\n", .{});
        gb.cpu.pc += 1;
        return 1;
    }
    fn RRCA(gb: *GB, _: InstrArgs) u8 { // TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "RRCA" });
        defer zone.end();
        const a = gb.cpu.get_byte(regID.a);
        // print("RRCA\n", .{});
        gb.cpu.set_byte(regID.a, a << 7 | a >> 1);
        gb.cpu.f.c = (@as(u1, @truncate(a)) == 1);
        gb.cpu.f.write();
        gb.cpu.pc += 1;
        return 1;
    }
    fn PUSH(gb: *GB, args: InstrArgs) u8 {
        const zone = tracy.beginZone(@src(), .{ .name = "PUSH" });
        defer zone.end();
        var high: u8 = undefined;
        var low: u8 = undefined;
        if (args.target == regID.a) {
            high = gb.cpu.get_byte(regID.a);
            low = gb.cpu.f.value;
            // print("PUSH AF a: 0x{X}, f: 0x{X}\n", .{ high, low });
        } else {
            const value = gb.cpu.get_word(args.target);
            high = @truncate(value >> 8);
            low = @truncate(value);
            // print("PUSH 0x{X} from {any}\n", .{ value, args.target });
        }

        gb.cpu.sp -= 1;
        gb.writeByte(gb.cpu.sp, high);
        gb.cpu.sp -= 1;
        gb.writeByte(gb.cpu.sp, low);
        gb.cpu.pc += 1;
        return 4;
    }
    fn POP(gb: *GB, args: InstrArgs) u8 {
        const zone = tracy.beginZone(@src(), .{ .name = "POP" });
        defer zone.end();
        const low = gb.read_byte(gb.cpu.sp);
        gb.cpu.sp += 1;
        const high = gb.read_byte(gb.cpu.sp);
        gb.cpu.sp += 1;
        const value = @as(u16, high) << 8 | low;
        // print("POP 0x{X} --> {any}\n", .{ value, args.target });
        if (args.target == regID.a) {
            gb.cpu.set_byte(regID.a, high);
            gb.cpu.f.value = low;
            gb.cpu.f.update();
        } else gb.cpu.set_word(args.target, value);
        gb.cpu.pc += 1;
        return 3;
    }
    fn JP(gb: *GB, args: InstrArgs) u8 { // TODO ONLY 3 CYCLES IF NOT TAKEN OTHERWISE 4
        const zone = tracy.beginZone(@src(), .{ .name = "JP" });
        defer zone.end();
        // print("JP", .{});
        const jump = gb.cpu.f.check(args.flagConditions);
        if (jump) {
            const n = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
            // print("to 0x{X}\n", .{n});
            gb.cpu.pc = n;
            return 4; // 4 cycles when taken
        } else {
            gb.cpu.pc += 3;
            return 3; // 3 cycles when not taken
        }
    }
    fn JR(gb: *GB, args: InstrArgs) u8 { // TODO ONLY 2 CYCLES IF NOT TAKEN OTHERWISE 3
        const zone = tracy.beginZone(@src(), .{ .name = "JR" });
        defer zone.end();
        // print("JR, \tcondition:", .{});
        const dist: i8 = @bitCast(gb.read_byte(gb.cpu.pc + 1));
        const jump = gb.cpu.f.check(args.flagConditions);
        // print(" by dist: [pc]0x{X} \t0x{X} ({d}) bytes \n", .{ gb.cpu.pc + 1, dist, dist });

        // const byte = gb.read_byte(gb.cpu.pc);
        if (jump) {
            const new_mem: i17 = @as(i17, @intCast(gb.cpu.pc + 2)) + dist;
            gb.cpu.pc = @as(u8, @intCast(new_mem));
            // print("to pc:0x{X}\n", .{gb.cpu.pc});
            return 3; // 3 cycles when taken
        } else { // next instruction, condition failed
            // print("skipped jump, failed condition\n", .{});
            gb.cpu.pc += 2;
            return 2; // 2 cycles when not taken
        }
    }
    fn JPHL(gb: *GB, _: InstrArgs) u8 {
        const zone = tracy.beginZone(@src(), .{ .name = "JPHL" });
        defer zone.end();
        // print("JPHL\n", .{});
        gb.cpu.pc = gb.cpu.get_word(regID.h);
        return 1;
    }
    fn CALLn16(gb: *GB, args: InstrArgs) u8 { //
        const zone = tracy.beginZone(@src(), .{ .name = "CALLn16" });
        defer zone.end();
        const call = gb.cpu.f.check(args.flagConditions);
        // const byte = gb.read_byte(gb.cpu.pc);

        if (call) {
            const n = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
            const ret = gb.cpu.pc + 3;
            // print("CALL to 0x{X}, later RET to 0x{X}", .{ n, ret });
            gb.cpu.sp -= 1;
            gb.writeByte(gb.cpu.sp, @truncate(ret >> 8));
            gb.cpu.sp -= 1;
            gb.writeByte(gb.cpu.sp, @truncate(ret));
            gb.cpu.pc = n;
            return 6; // 6 cycles when taken
        } else { // next instruction, condition failed
            // print("skipped jump, failed condition\n", .{});
            gb.cpu.pc += 3;
            return 3; // 3 cycles when not taken
        }
    }
    fn RET(gb: *GB, args: InstrArgs) u8 { // TODO TEST, UPDATE cycles 5 if condition met, 2 if not met, 4 if no condition
        const zone = tracy.beginZone(@src(), .{ .name = "RET" });
        defer zone.end();
        // print("RET, condition:{any}", .{args.flagConditions});
        const ret = gb.cpu.f.check(args.flagConditions);
        // const byte = gb.read_byte(gb.cpu.pc);

        if (ret) {
            const low = gb.read_byte(gb.cpu.sp);
            gb.cpu.sp += 1;
            const high = gb.read_byte(gb.cpu.sp);
            gb.cpu.sp += 1;
            gb.cpu.pc = @as(u16, high) << 8 | low;
            return 5; // 5 cycles if condition met
        } else {
            gb.cpu.pc += 1;
            return 2; // 2 cycles when not taken
        }
    }
    fn CPAn8(gb: *GB, _: InstrArgs) u8 { // TODO TEST;
        const zone = tracy.beginZone(@src(), .{ .name = "CPAn8" });
        defer zone.end();
        // print("CPAn8, \n", .{});
        const n = gb.read_byte(gb.cpu.pc + 1);
        const reg = gb.cpu.get_byte(regID.a);
        // print("n b1: [pc]0x{X} \t(0x{X}), A: 0x{X}\n", .{ gb.cpu.pc + 1, n, gb.cpu.get_byte(regID.a) });
        const res = @subWithOverflow(reg, n);
        gb.cpu.f.z = res[0] == 0;
        gb.cpu.f.s = true;
        gb.cpu.f.h = (reg & 0xF) < (res[0] & 0xF); // half carry conditions
        gb.cpu.f.c = res[1] == 1;
        gb.cpu.f.write();
        gb.cpu.pc += 2;
        return 2;
    }
    fn CPAr8(gb: *GB, args: InstrArgs) u8 { // TODO TEST;
        const zone = tracy.beginZone(@src(), .{ .name = "CPAr8" });
        defer zone.end();
        const n = gb.cpu.get_byte(args.target);
        // print("CPAr8, target = {any}\n", .{args.target});
        const reg = gb.cpu.get_byte(regID.a);
        const res = @subWithOverflow(reg, n);
        gb.cpu.f.z = res[0] == 0;
        gb.cpu.f.s = true;
        gb.cpu.f.h = (reg & 0xF) < (res[0] & 0xF); // half carry conditions
        gb.cpu.f.c = res[1] == 1;
        gb.cpu.f.write();
        gb.cpu.pc += 1;
        return 1;
    }
    fn CPAHL(gb: *GB, _: InstrArgs) u8 { // TODO TEST;
        const zone = tracy.beginZone(@src(), .{ .name = "CPAHL" });
        defer zone.end();
        const hl = gb.cpu.get_word(regID.h);
        const reg = gb.cpu.get_byte(regID.a);
        // print("CPAHL, compare mem_place: 0x{X} ({d}) to A:{d}\n", .{ hl, gb.read_byte(hl), reg });
        const res = @subWithOverflow(reg, hl);
        gb.cpu.f.z = res[0] == 0;
        gb.cpu.f.s = true;
        gb.cpu.f.h = (reg & 0xF) < (res[0] & 0xF); // half carry conditions
        gb.cpu.f.c = res[1] == 1;
        gb.cpu.f.write();
        gb.cpu.pc += 1;
        return 2;
    }

    // PREFIX INSTRUCTIONS
    fn BITTEST(gb: *GB, args: InstrArgs) u8 { // TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "BITTEST" });
        defer zone.end();
        const bit: u3 = args.bit_target.bit;
        const target = gb.cpu.get_byte(args.bit_target.target);
        // print("BITTEST, target: {any}, bit: {any}, reg_bin: 0b{b}", .{ args.bit_target.target, args.bit_target.bit, target });
        // print("                                             ^", .{});
        gb.cpu.f.z = @as(u1, @truncate(target >> bit)) == 0; // set zero flag if the target bit is not set
        gb.cpu.f.h = true; // set half carry
        gb.cpu.f.write();
        gb.cpu.pc += 2;
        return 2;
    }
    fn BITTESTHL(gb: *GB, args: InstrArgs) u8 { // TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "BITTESTHL" });
        defer zone.end();
        const bit: u3 = args.bit_target.bit;
        // print("BITTESTHL, target: {any}, bit: {any}\n", .{ args.bit_target.target, args.bit_target.bit });
        const hl = gb.cpu.get_word(args.target);
        const byte = gb.read_byte(hl);
        gb.cpu.f.z = @as(u1, @truncate(byte >> bit)) == 0;
        gb.cpu.f.h = true;
        gb.cpu.f.write();
        gb.cpu.pc += 2;
        return 3;
    }
    fn RL(gb: *GB, args: InstrArgs) u8 { // TODO TEST Rotate bits in register r8 left through carry.
        const zone = tracy.beginZone(@src(), .{ .name = "RL" });
        defer zone.end();
        const carried = gb.cpu.f.c;
        const reg = gb.cpu.get_byte(args.target);
        // print("RL, target: {any}, prior: 0b{b}, carried = {d}\n", .{ args.target, reg, @intFromBool(carried) });
        gb.cpu.f.c = @as(u1, @truncate(reg >> 7)) == 1;
        const rotated: u8 = reg << 1 | @intFromBool(carried);
        gb.cpu.f.z = rotated == 0;
        gb.cpu.f.write();
        gb.cpu.set_byte(args.target, rotated);
        // print("after: 0b{b}\n", .{gb.cpu.get_byte(args.target)});
        gb.cpu.pc += 2;
        return 2;
    }
    fn UNDEF(_: *GB, _: InstrArgs) u8 {
        print("CRIT UNDEF INSTRUCTION\n", .{});
        return 255;
    }
    inline fn exe_from_byte(gb: *GB, byte: u8, prefixed: bool) u8 {
        return switch (prefixed) {
            false => switch (byte) {
                0x00 => NOP(gb, .{ .none = {} }),
                0x01 => LD16(gb, .{ .target = regID.b }),
                0x02 => UNDEF(gb, .{ .none = {} }),
                0x03 => UNDEF(gb, .{ .none = {} }),
                0x04 => INCr8(gb, .{ .target = regID.b }),
                0x05 => DECr8(gb, .{ .target = regID.b }),
                0x06 => LD8(gb, .{ .target = regID.b }),
                0x07 => UNDEF(gb, .{ .none = {} }),
                0x08 => UNDEF(gb, .{ .none = {} }),
                0x09 => UNDEF(gb, .{ .none = {} }),
                0x0A => UNDEF(gb, .{ .none = {} }),
                0x0B => UNDEF(gb, .{ .none = {} }),
                0x0C => INCr8(gb, .{ .target = regID.c }),
                0x0D => DECr8(gb, .{ .target = regID.c }),
                0x0E => LD8(gb, .{ .target = regID.c }),
                0x0F => RRCA(gb, .{ .none = {} }),
                0x10 => UNDEF(gb, .{ .none = {} }),
                0x11 => LD16(gb, .{ .target = regID.d }),
                0x12 => UNDEF(gb, .{ .none = {} }),
                0x13 => INCr16(gb, .{ .target = regID.d }),
                0x14 => INCr8(gb, .{ .target = regID.d }),
                0x15 => DECr8(gb, .{ .target = regID.d }),
                0x16 => LD8(gb, .{ .target = regID.d }),
                0x17 => UNDEF(gb, .{ .none = {} }),
                0x18 => JR(gb, .{ .flagConditions = .{ .none = true } }),
                0x19 => ADDHLr16(gb, .{ .target = regID.d }),
                0x1A => LDAr16(gb, .{ .target = regID.d }),
                0x1B => DECr16(gb, .{ .target = regID.d }),
                0x1C => INCr8(gb, .{ .target = regID.e }),
                0x1D => DECr8(gb, .{ .target = regID.e }),
                0x1E => LD8(gb, .{ .target = regID.e }),
                0x1F => UNDEF(gb, .{ .none = {} }),
                0x20 => JR(gb, .{ .flagConditions = .{ .z = false } }),
                0x21 => LD16(gb, .{ .target = regID.h }),
                0x22 => LDHLIA(gb, .{ .none = {} }),
                0x23 => INCr16(gb, .{ .target = regID.h }),
                0x24 => INCr8(gb, .{ .target = regID.h }),
                0x25 => UNDEF(gb, .{ .none = {} }),
                0x26 => UNDEF(gb, .{ .none = {} }),
                0x27 => UNDEF(gb, .{ .none = {} }),
                0x28 => JR(gb, .{ .flagConditions = .{ .z = true } }),
                0x29 => UNDEF(gb, .{ .none = {} }),
                0x2A => UNDEF(gb, .{ .none = {} }),
                0x2B => UNDEF(gb, .{ .none = {} }),
                0x2C => UNDEF(gb, .{ .none = {} }),
                0x2D => UNDEF(gb, .{ .none = {} }),
                0x2E => LD8(gb, .{ .target = regID.l }),
                0x2F => UNDEF(gb, .{ .none = {} }),
                0x30 => UNDEF(gb, .{ .none = {} }),
                0x31 => LDSP16(gb, .{ .none = {} }),
                0x32 => LDHLDA(gb, .{ .none = {} }),
                0x33 => UNDEF(gb, .{ .none = {} }),
                0x34 => UNDEF(gb, .{ .none = {} }),
                0x35 => UNDEF(gb, .{ .none = {} }),
                0x36 => UNDEF(gb, .{ .none = {} }),
                0x37 => UNDEF(gb, .{ .none = {} }),
                0x38 => UNDEF(gb, .{ .none = {} }),
                0x39 => UNDEF(gb, .{ .none = {} }),
                0x3A => UNDEF(gb, .{ .none = {} }),
                0x3B => UNDEF(gb, .{ .none = {} }),
                0x3C => UNDEF(gb, .{ .none = {} }),
                0x3D => DECr8(gb, .{ .target = regID.a }),
                0x3E => LD8(gb, .{ .target = regID.a }),
                0x3F => UNDEF(gb, .{ .none = {} }),
                0x40 => UNDEF(gb, .{ .none = {} }),
                0x41 => UNDEF(gb, .{ .none = {} }),
                0x42 => UNDEF(gb, .{ .none = {} }),
                0x43 => UNDEF(gb, .{ .none = {} }),
                0x44 => UNDEF(gb, .{ .none = {} }),
                0x45 => UNDEF(gb, .{ .none = {} }),
                0x46 => UNDEF(gb, .{ .none = {} }),
                0x47 => UNDEF(gb, .{ .none = {} }),
                0x48 => UNDEF(gb, .{ .none = {} }),
                0x49 => UNDEF(gb, .{ .none = {} }),
                0x4A => UNDEF(gb, .{ .none = {} }),
                0x4B => UNDEF(gb, .{ .none = {} }),
                0x4C => UNDEF(gb, .{ .none = {} }),
                0x4D => UNDEF(gb, .{ .none = {} }),
                0x4E => UNDEF(gb, .{ .none = {} }),
                0x4F => LDr8(gb, .{ .targets = .{ .to = regID.c, .from = regID.a } }),
                0x50 => UNDEF(gb, .{ .none = {} }),
                0x51 => UNDEF(gb, .{ .none = {} }),
                0x52 => UNDEF(gb, .{ .none = {} }),
                0x53 => UNDEF(gb, .{ .none = {} }),
                0x54 => UNDEF(gb, .{ .none = {} }),
                0x55 => UNDEF(gb, .{ .none = {} }),
                0x56 => UNDEF(gb, .{ .none = {} }),
                0x57 => LDr8(gb, .{ .targets = .{ .to = regID.d, .from = regID.a } }),
                0x58 => UNDEF(gb, .{ .none = {} }),
                0x59 => UNDEF(gb, .{ .none = {} }),
                0x5A => UNDEF(gb, .{ .none = {} }),
                0x5B => UNDEF(gb, .{ .none = {} }),
                0x5C => UNDEF(gb, .{ .none = {} }),
                0x5D => UNDEF(gb, .{ .none = {} }),
                0x5E => UNDEF(gb, .{ .none = {} }),
                0x5F => UNDEF(gb, .{ .none = {} }),
                0x60 => UNDEF(gb, .{ .none = {} }),
                0x61 => UNDEF(gb, .{ .none = {} }),
                0x62 => UNDEF(gb, .{ .none = {} }),
                0x63 => UNDEF(gb, .{ .none = {} }),
                0x64 => UNDEF(gb, .{ .none = {} }),
                0x65 => UNDEF(gb, .{ .none = {} }),
                0x66 => UNDEF(gb, .{ .none = {} }),
                0x67 => LDr8(gb, .{ .targets = .{ .to = regID.h, .from = regID.a } }),
                0x68 => UNDEF(gb, .{ .none = {} }),
                0x69 => UNDEF(gb, .{ .none = {} }),
                0x6A => UNDEF(gb, .{ .none = {} }),
                0x6B => UNDEF(gb, .{ .none = {} }),
                0x6C => UNDEF(gb, .{ .none = {} }),
                0x6D => UNDEF(gb, .{ .none = {} }),
                0x6E => UNDEF(gb, .{ .none = {} }),
                0x6F => UNDEF(gb, .{ .none = {} }),
                0x70 => UNDEF(gb, .{ .none = {} }),
                0x71 => UNDEF(gb, .{ .none = {} }),
                0x72 => UNDEF(gb, .{ .none = {} }),
                0x73 => UNDEF(gb, .{ .none = {} }),
                0x74 => UNDEF(gb, .{ .none = {} }),
                0x75 => UNDEF(gb, .{ .none = {} }),
                0x76 => UNDEF(gb, .{ .none = {} }),
                0x77 => LDHLR(gb, .{ .target = regID.a }),
                0x78 => LDr8(gb, .{ .targets = .{ .to = regID.a, .from = regID.b } }),
                0x79 => LDr8(gb, .{ .targets = .{ .to = regID.a, .from = regID.c } }),
                0x7A => LDr8(gb, .{ .targets = .{ .to = regID.a, .from = regID.d } }),
                0x7B => LDr8(gb, .{ .targets = .{ .to = regID.a, .from = regID.e } }),
                0x7C => LDr8(gb, .{ .targets = .{ .to = regID.a, .from = regID.h } }),
                0x7D => LDr8(gb, .{ .targets = .{ .to = regID.a, .from = regID.l } }),
                0x7E => LDAHL(gb, .{ .none = {} }),
                0x7F => LDr8(gb, .{ .targets = .{ .to = regID.a, .from = regID.a } }),
                0x80 => ADDAr8(gb, .{ .target = regID.b }),
                0x81 => ADDAr8(gb, .{ .target = regID.c }),
                0x82 => ADDAr8(gb, .{ .target = regID.d }),
                0x83 => ADDAr8(gb, .{ .target = regID.e }),
                0x84 => ADDAr8(gb, .{ .target = regID.h }),
                0x85 => ADDAr8(gb, .{ .target = regID.l }),
                0x86 => ADDAHL(gb, .{ .none = {} }),
                0x87 => UNDEF(gb, .{ .none = {} }),
                0x88 => UNDEF(gb, .{ .none = {} }),
                0x89 => UNDEF(gb, .{ .none = {} }),
                0x8A => UNDEF(gb, .{ .none = {} }),
                0x8B => UNDEF(gb, .{ .none = {} }),
                0x8C => UNDEF(gb, .{ .none = {} }),
                0x8D => UNDEF(gb, .{ .none = {} }),
                0x8E => UNDEF(gb, .{ .none = {} }),
                0x8F => UNDEF(gb, .{ .none = {} }),
                0x90 => SUBAr8(gb, .{ .target = regID.b }),
                0x91 => SUBAr8(gb, .{ .target = regID.c }),
                0x92 => SUBAr8(gb, .{ .target = regID.d }),
                0x93 => SUBAr8(gb, .{ .target = regID.e }),
                0x94 => SUBAr8(gb, .{ .target = regID.h }),
                0x95 => SUBAr8(gb, .{ .target = regID.l }),
                0x96 => UNDEF(gb, .{ .none = {} }),
                0x97 => SUBAr8(gb, .{ .target = regID.a }),
                0x98 => UNDEF(gb, .{ .none = {} }),
                0x99 => UNDEF(gb, .{ .none = {} }),
                0x9A => UNDEF(gb, .{ .none = {} }),
                0x9B => UNDEF(gb, .{ .none = {} }),
                0x9C => UNDEF(gb, .{ .none = {} }),
                0x9D => UNDEF(gb, .{ .none = {} }),
                0x9E => UNDEF(gb, .{ .none = {} }),
                0x9F => UNDEF(gb, .{ .none = {} }),
                0xA0 => UNDEF(gb, .{ .none = {} }),
                0xA1 => UNDEF(gb, .{ .none = {} }),
                0xA2 => UNDEF(gb, .{ .none = {} }),
                0xA3 => UNDEF(gb, .{ .none = {} }),
                0xA4 => UNDEF(gb, .{ .none = {} }),
                0xA5 => UNDEF(gb, .{ .none = {} }),
                0xA6 => UNDEF(gb, .{ .none = {} }),
                0xA7 => UNDEF(gb, .{ .none = {} }),
                0xA8 => UNDEF(gb, .{ .none = {} }),
                0xA9 => UNDEF(gb, .{ .none = {} }),
                0xAA => UNDEF(gb, .{ .none = {} }),
                0xAB => UNDEF(gb, .{ .none = {} }),
                0xAC => UNDEF(gb, .{ .none = {} }),
                0xAD => UNDEF(gb, .{ .none = {} }),
                0xAE => UNDEF(gb, .{ .none = {} }),
                0xAF => XORA(gb, .{ .target = regID.a }),
                0xB0 => UNDEF(gb, .{ .none = {} }),
                0xB1 => UNDEF(gb, .{ .none = {} }),
                0xB2 => UNDEF(gb, .{ .none = {} }),
                0xB3 => UNDEF(gb, .{ .none = {} }),
                0xB4 => UNDEF(gb, .{ .none = {} }),
                0xB5 => UNDEF(gb, .{ .none = {} }),
                0xB6 => UNDEF(gb, .{ .none = {} }),
                0xB7 => UNDEF(gb, .{ .none = {} }),
                0xB8 => UNDEF(gb, .{ .none = {} }),
                0xB9 => UNDEF(gb, .{ .none = {} }),
                0xBA => UNDEF(gb, .{ .none = {} }),
                0xBB => UNDEF(gb, .{ .none = {} }),
                0xBC => UNDEF(gb, .{ .none = {} }),
                0xBD => UNDEF(gb, .{ .none = {} }),
                0xBE => CPAHL(gb, .{ .none = {} }),
                0xBF => UNDEF(gb, .{ .none = {} }),
                0xC0 => UNDEF(gb, .{ .none = {} }),
                0xC1 => POP(gb, .{ .target = regID.b }),
                0xC2 => UNDEF(gb, .{ .none = {} }),
                0xC3 => JP(gb, .{ .flagConditions = .{ .none = true } }),
                0xC4 => UNDEF(gb, .{ .none = {} }),
                0xC5 => PUSH(gb, .{ .target = regID.b }),
                0xC6 => UNDEF(gb, .{ .none = {} }),
                0xC7 => UNDEF(gb, .{ .none = {} }),
                0xC8 => UNDEF(gb, .{ .none = {} }),
                0xC9 => RET(gb, .{ .flagConditions = .{ .none = true } }),
                0xCA => UNDEF(gb, .{ .none = {} }),
                0xCB => UNDEF(gb, .{ .none = {} }),
                0xCC => UNDEF(gb, .{ .none = {} }),
                0xCD => CALLn16(gb, .{ .flagConditions = .{ .none = true } }),
                0xCE => UNDEF(gb, .{ .none = {} }),
                0xCF => UNDEF(gb, .{ .none = {} }),
                0xD0 => UNDEF(gb, .{ .none = {} }),
                0xD1 => UNDEF(gb, .{ .none = {} }),
                0xD2 => UNDEF(gb, .{ .none = {} }),
                0xD3 => UNDEF(gb, .{ .none = {} }),
                0xD4 => UNDEF(gb, .{ .none = {} }),
                0xD5 => UNDEF(gb, .{ .none = {} }),
                0xD6 => UNDEF(gb, .{ .none = {} }),
                0xD7 => UNDEF(gb, .{ .none = {} }),
                0xD8 => UNDEF(gb, .{ .none = {} }),
                0xD9 => UNDEF(gb, .{ .none = {} }),
                0xDA => UNDEF(gb, .{ .none = {} }),
                0xDB => UNDEF(gb, .{ .none = {} }),
                0xDC => UNDEF(gb, .{ .none = {} }),
                0xDD => UNDEF(gb, .{ .none = {} }),
                0xDE => UNDEF(gb, .{ .none = {} }),
                0xDF => UNDEF(gb, .{ .none = {} }),
                0xE0 => LDHn16A(gb, .{ .none = {} }),
                0xE1 => UNDEF(gb, .{ .none = {} }),
                0xE2 => LDHCA(gb, .{ .none = {} }),
                0xE3 => UNDEF(gb, .{ .none = {} }),
                0xE4 => UNDEF(gb, .{ .none = {} }),
                0xE5 => UNDEF(gb, .{ .none = {} }),
                0xE6 => UNDEF(gb, .{ .none = {} }),
                0xE7 => UNDEF(gb, .{ .none = {} }),
                0xE8 => UNDEF(gb, .{ .none = {} }),
                0xE9 => JPHL(gb, .{ .none = {} }),
                0xEA => LDn16A(gb, .{ .none = {} }),
                0xEB => UNDEF(gb, .{ .none = {} }),
                0xEC => UNDEF(gb, .{ .none = {} }),
                0xED => UNDEF(gb, .{ .none = {} }),
                0xEE => UNDEF(gb, .{ .none = {} }),
                0xEF => UNDEF(gb, .{ .none = {} }),
                0xF0 => LDHAn16(gb, .{ .none = {} }),
                0xF1 => UNDEF(gb, .{ .none = {} }),
                0xF2 => LDHAC(gb, .{ .none = {} }),
                0xF3 => UNDEF(gb, .{ .none = {} }),
                0xF4 => UNDEF(gb, .{ .none = {} }),
                0xF5 => UNDEF(gb, .{ .none = {} }),
                0xF6 => UNDEF(gb, .{ .none = {} }),
                0xF7 => UNDEF(gb, .{ .none = {} }),
                0xF8 => UNDEF(gb, .{ .none = {} }),
                0xF9 => UNDEF(gb, .{ .none = {} }),
                0xFA => LDAn16(gb, .{ .none = {} }),
                0xFB => EI(gb, .{ .none = {} }),
                0xFC => UNDEF(gb, .{ .none = {} }),
                0xFD => UNDEF(gb, .{ .none = {} }),
                0xFE => CPAn8(gb, .{ .none = {} }),
                0xFF => UNDEF(gb, .{ .none = {} }),
            },
            true => switch (byte) {
                0x00 => UNDEF(gb, .{ .none = {} }),
                0x01 => UNDEF(gb, .{ .none = {} }),
                0x02 => UNDEF(gb, .{ .none = {} }),
                0x03 => UNDEF(gb, .{ .none = {} }),
                0x04 => UNDEF(gb, .{ .none = {} }),
                0x05 => UNDEF(gb, .{ .none = {} }),
                0x06 => UNDEF(gb, .{ .none = {} }),
                0x07 => UNDEF(gb, .{ .none = {} }),
                0x08 => UNDEF(gb, .{ .none = {} }),
                0x09 => UNDEF(gb, .{ .none = {} }),
                0x0A => UNDEF(gb, .{ .none = {} }),
                0x0B => UNDEF(gb, .{ .none = {} }),
                0x0C => UNDEF(gb, .{ .none = {} }),
                0x0D => UNDEF(gb, .{ .none = {} }),
                0x0E => UNDEF(gb, .{ .none = {} }),
                0x0F => UNDEF(gb, .{ .none = {} }),
                0x10 => UNDEF(gb, .{ .none = {} }),
                0x11 => RL(gb, .{ .target = regID.c }),
                0x12 => UNDEF(gb, .{ .none = {} }),
                0x13 => UNDEF(gb, .{ .none = {} }),
                0x14 => UNDEF(gb, .{ .none = {} }),
                0x15 => UNDEF(gb, .{ .none = {} }),
                0x16 => UNDEF(gb, .{ .none = {} }),
                0x17 => UNDEF(gb, .{ .none = {} }),
                0x18 => UNDEF(gb, .{ .none = {} }),
                0x19 => UNDEF(gb, .{ .none = {} }),
                0x1A => UNDEF(gb, .{ .none = {} }),
                0x1B => UNDEF(gb, .{ .none = {} }),
                0x1C => UNDEF(gb, .{ .none = {} }),
                0x1D => UNDEF(gb, .{ .none = {} }),
                0x1E => UNDEF(gb, .{ .none = {} }),
                0x1F => UNDEF(gb, .{ .none = {} }),
                0x20 => UNDEF(gb, .{ .none = {} }),
                0x21 => UNDEF(gb, .{ .none = {} }),
                0x22 => UNDEF(gb, .{ .none = {} }),
                0x23 => UNDEF(gb, .{ .none = {} }),
                0x24 => UNDEF(gb, .{ .none = {} }),
                0x25 => UNDEF(gb, .{ .none = {} }),
                0x26 => UNDEF(gb, .{ .none = {} }),
                0x27 => UNDEF(gb, .{ .none = {} }),
                0x28 => UNDEF(gb, .{ .none = {} }),
                0x29 => UNDEF(gb, .{ .none = {} }),
                0x2A => UNDEF(gb, .{ .none = {} }),
                0x2B => UNDEF(gb, .{ .none = {} }),
                0x2C => UNDEF(gb, .{ .none = {} }),
                0x2D => UNDEF(gb, .{ .none = {} }),
                0x2E => UNDEF(gb, .{ .none = {} }),
                0x2F => UNDEF(gb, .{ .none = {} }),
                0x30 => UNDEF(gb, .{ .none = {} }),
                0x31 => UNDEF(gb, .{ .none = {} }),
                0x32 => UNDEF(gb, .{ .none = {} }),
                0x33 => UNDEF(gb, .{ .none = {} }),
                0x34 => UNDEF(gb, .{ .none = {} }),
                0x35 => UNDEF(gb, .{ .none = {} }),
                0x36 => UNDEF(gb, .{ .none = {} }),
                0x37 => UNDEF(gb, .{ .none = {} }),
                0x38 => UNDEF(gb, .{ .none = {} }),
                0x39 => UNDEF(gb, .{ .none = {} }),
                0x3A => UNDEF(gb, .{ .none = {} }),
                0x3B => UNDEF(gb, .{ .none = {} }),
                0x3C => UNDEF(gb, .{ .none = {} }),
                0x3D => UNDEF(gb, .{ .none = {} }),
                0x3E => UNDEF(gb, .{ .none = {} }),
                0x3F => UNDEF(gb, .{ .none = {} }),
                0x40 => UNDEF(gb, .{ .none = {} }),
                0x41 => UNDEF(gb, .{ .none = {} }),
                0x42 => UNDEF(gb, .{ .none = {} }),
                0x43 => UNDEF(gb, .{ .none = {} }),
                0x44 => UNDEF(gb, .{ .none = {} }),
                0x45 => UNDEF(gb, .{ .none = {} }),
                0x46 => UNDEF(gb, .{ .none = {} }),
                0x47 => UNDEF(gb, .{ .none = {} }),
                0x48 => UNDEF(gb, .{ .none = {} }),
                0x49 => UNDEF(gb, .{ .none = {} }),
                0x4A => UNDEF(gb, .{ .none = {} }),
                0x4B => UNDEF(gb, .{ .none = {} }),
                0x4C => UNDEF(gb, .{ .none = {} }),
                0x4D => UNDEF(gb, .{ .none = {} }),
                0x4E => UNDEF(gb, .{ .none = {} }),
                0x4F => UNDEF(gb, .{ .none = {} }),
                0x50 => UNDEF(gb, .{ .none = {} }),
                0x51 => UNDEF(gb, .{ .none = {} }),
                0x52 => UNDEF(gb, .{ .none = {} }),
                0x53 => UNDEF(gb, .{ .none = {} }),
                0x54 => UNDEF(gb, .{ .none = {} }),
                0x55 => UNDEF(gb, .{ .none = {} }),
                0x56 => UNDEF(gb, .{ .none = {} }),
                0x57 => UNDEF(gb, .{ .none = {} }),
                0x58 => UNDEF(gb, .{ .none = {} }),
                0x59 => UNDEF(gb, .{ .none = {} }),
                0x5A => UNDEF(gb, .{ .none = {} }),
                0x5B => UNDEF(gb, .{ .none = {} }),
                0x5C => UNDEF(gb, .{ .none = {} }),
                0x5D => UNDEF(gb, .{ .none = {} }),
                0x5E => UNDEF(gb, .{ .none = {} }),
                0x5F => UNDEF(gb, .{ .none = {} }),
                0x60 => UNDEF(gb, .{ .none = {} }),
                0x61 => UNDEF(gb, .{ .none = {} }),
                0x62 => UNDEF(gb, .{ .none = {} }),
                0x63 => UNDEF(gb, .{ .none = {} }),
                0x64 => UNDEF(gb, .{ .none = {} }),
                0x65 => UNDEF(gb, .{ .none = {} }),
                0x66 => UNDEF(gb, .{ .none = {} }),
                0x67 => UNDEF(gb, .{ .none = {} }),
                0x68 => UNDEF(gb, .{ .none = {} }),
                0x69 => UNDEF(gb, .{ .none = {} }),
                0x6A => UNDEF(gb, .{ .none = {} }),
                0x6B => UNDEF(gb, .{ .none = {} }),
                0x6C => UNDEF(gb, .{ .none = {} }),
                0x6D => UNDEF(gb, .{ .none = {} }),
                0x6E => UNDEF(gb, .{ .none = {} }),
                0x6F => UNDEF(gb, .{ .none = {} }),
                0x70 => UNDEF(gb, .{ .none = {} }),
                0x71 => UNDEF(gb, .{ .none = {} }),
                0x72 => UNDEF(gb, .{ .none = {} }),
                0x73 => UNDEF(gb, .{ .none = {} }),
                0x74 => UNDEF(gb, .{ .none = {} }),
                0x75 => UNDEF(gb, .{ .none = {} }),
                0x76 => UNDEF(gb, .{ .none = {} }),
                0x77 => UNDEF(gb, .{ .none = {} }),
                0x78 => UNDEF(gb, .{ .none = {} }),
                0x79 => UNDEF(gb, .{ .none = {} }),
                0x7A => UNDEF(gb, .{ .none = {} }),
                0x7B => UNDEF(gb, .{ .none = {} }),
                0x7C => BITTEST(gb, .{ .bit_target = .{ .bit = 7, .target = regID.h } }),
                0x7D => UNDEF(gb, .{ .none = {} }),
                0x7E => UNDEF(gb, .{ .none = {} }),
                0x7F => UNDEF(gb, .{ .none = {} }),
                0x80 => UNDEF(gb, .{ .none = {} }),
                0x81 => UNDEF(gb, .{ .none = {} }),
                0x82 => UNDEF(gb, .{ .none = {} }),
                0x83 => UNDEF(gb, .{ .none = {} }),
                0x84 => UNDEF(gb, .{ .none = {} }),
                0x85 => UNDEF(gb, .{ .none = {} }),
                0x86 => UNDEF(gb, .{ .none = {} }),
                0x87 => UNDEF(gb, .{ .none = {} }),
                0x88 => UNDEF(gb, .{ .none = {} }),
                0x89 => UNDEF(gb, .{ .none = {} }),
                0x8A => UNDEF(gb, .{ .none = {} }),
                0x8B => UNDEF(gb, .{ .none = {} }),
                0x8C => UNDEF(gb, .{ .none = {} }),
                0x8D => UNDEF(gb, .{ .none = {} }),
                0x8E => UNDEF(gb, .{ .none = {} }),
                0x8F => UNDEF(gb, .{ .none = {} }),
                0x90 => UNDEF(gb, .{ .none = {} }),
                0x91 => UNDEF(gb, .{ .none = {} }),
                0x92 => UNDEF(gb, .{ .none = {} }),
                0x93 => UNDEF(gb, .{ .none = {} }),
                0x94 => UNDEF(gb, .{ .none = {} }),
                0x95 => UNDEF(gb, .{ .none = {} }),
                0x96 => UNDEF(gb, .{ .none = {} }),
                0x97 => UNDEF(gb, .{ .none = {} }),
                0x98 => UNDEF(gb, .{ .none = {} }),
                0x99 => UNDEF(gb, .{ .none = {} }),
                0x9A => UNDEF(gb, .{ .none = {} }),
                0x9B => UNDEF(gb, .{ .none = {} }),
                0x9C => UNDEF(gb, .{ .none = {} }),
                0x9D => UNDEF(gb, .{ .none = {} }),
                0x9E => UNDEF(gb, .{ .none = {} }),
                0x9F => UNDEF(gb, .{ .none = {} }),
                0xA0 => UNDEF(gb, .{ .none = {} }),
                0xA1 => UNDEF(gb, .{ .none = {} }),
                0xA2 => UNDEF(gb, .{ .none = {} }),
                0xA3 => UNDEF(gb, .{ .none = {} }),
                0xA4 => UNDEF(gb, .{ .none = {} }),
                0xA5 => UNDEF(gb, .{ .none = {} }),
                0xA6 => UNDEF(gb, .{ .none = {} }),
                0xA7 => UNDEF(gb, .{ .none = {} }),
                0xA8 => UNDEF(gb, .{ .none = {} }),
                0xA9 => UNDEF(gb, .{ .none = {} }),
                0xAA => UNDEF(gb, .{ .none = {} }),
                0xAB => UNDEF(gb, .{ .none = {} }),
                0xAC => UNDEF(gb, .{ .none = {} }),
                0xAD => UNDEF(gb, .{ .none = {} }),
                0xAE => UNDEF(gb, .{ .none = {} }),
                0xAF => UNDEF(gb, .{ .none = {} }),
                0xB0 => UNDEF(gb, .{ .none = {} }),
                0xB1 => UNDEF(gb, .{ .none = {} }),
                0xB2 => UNDEF(gb, .{ .none = {} }),
                0xB3 => UNDEF(gb, .{ .none = {} }),
                0xB4 => UNDEF(gb, .{ .none = {} }),
                0xB5 => UNDEF(gb, .{ .none = {} }),
                0xB6 => UNDEF(gb, .{ .none = {} }),
                0xB7 => UNDEF(gb, .{ .none = {} }),
                0xB8 => UNDEF(gb, .{ .none = {} }),
                0xB9 => UNDEF(gb, .{ .none = {} }),
                0xBA => UNDEF(gb, .{ .none = {} }),
                0xBB => UNDEF(gb, .{ .none = {} }),
                0xBC => UNDEF(gb, .{ .none = {} }),
                0xBD => UNDEF(gb, .{ .none = {} }),
                0xBE => UNDEF(gb, .{ .none = {} }),
                0xBF => UNDEF(gb, .{ .none = {} }),
                0xC0 => UNDEF(gb, .{ .none = {} }),
                0xC1 => UNDEF(gb, .{ .none = {} }),
                0xC2 => UNDEF(gb, .{ .none = {} }),
                0xC3 => UNDEF(gb, .{ .none = {} }),
                0xC4 => UNDEF(gb, .{ .none = {} }),
                0xC5 => UNDEF(gb, .{ .none = {} }),
                0xC6 => UNDEF(gb, .{ .none = {} }),
                0xC7 => UNDEF(gb, .{ .none = {} }),
                0xC8 => UNDEF(gb, .{ .none = {} }),
                0xC9 => UNDEF(gb, .{ .none = {} }),
                0xCA => UNDEF(gb, .{ .none = {} }),
                0xCB => UNDEF(gb, .{ .none = {} }),
                0xCC => UNDEF(gb, .{ .none = {} }),
                0xCD => UNDEF(gb, .{ .none = {} }),
                0xCE => UNDEF(gb, .{ .none = {} }),
                0xCF => UNDEF(gb, .{ .none = {} }),
                0xD0 => UNDEF(gb, .{ .none = {} }),
                0xD1 => UNDEF(gb, .{ .none = {} }),
                0xD2 => UNDEF(gb, .{ .none = {} }),
                0xD3 => UNDEF(gb, .{ .none = {} }),
                0xD4 => UNDEF(gb, .{ .none = {} }),
                0xD5 => UNDEF(gb, .{ .none = {} }),
                0xD6 => UNDEF(gb, .{ .none = {} }),
                0xD7 => UNDEF(gb, .{ .none = {} }),
                0xD8 => UNDEF(gb, .{ .none = {} }),
                0xD9 => UNDEF(gb, .{ .none = {} }),
                0xDA => UNDEF(gb, .{ .none = {} }),
                0xDB => UNDEF(gb, .{ .none = {} }),
                0xDC => UNDEF(gb, .{ .none = {} }),
                0xDD => UNDEF(gb, .{ .none = {} }),
                0xDE => UNDEF(gb, .{ .none = {} }),
                0xDF => UNDEF(gb, .{ .none = {} }),
                0xE0 => UNDEF(gb, .{ .none = {} }),
                0xE1 => UNDEF(gb, .{ .none = {} }),
                0xE2 => UNDEF(gb, .{ .none = {} }),
                0xE3 => UNDEF(gb, .{ .none = {} }),
                0xE4 => UNDEF(gb, .{ .none = {} }),
                0xE5 => UNDEF(gb, .{ .none = {} }),
                0xE6 => UNDEF(gb, .{ .none = {} }),
                0xE7 => UNDEF(gb, .{ .none = {} }),
                0xE8 => UNDEF(gb, .{ .none = {} }),
                0xE9 => UNDEF(gb, .{ .none = {} }),
                0xEA => UNDEF(gb, .{ .none = {} }),
                0xEB => UNDEF(gb, .{ .none = {} }),
                0xEC => UNDEF(gb, .{ .none = {} }),
                0xED => UNDEF(gb, .{ .none = {} }),
                0xEE => UNDEF(gb, .{ .none = {} }),
                0xEF => UNDEF(gb, .{ .none = {} }),
                0xF0 => UNDEF(gb, .{ .none = {} }),
                0xF1 => UNDEF(gb, .{ .none = {} }),
                0xF2 => UNDEF(gb, .{ .none = {} }),
                0xF3 => UNDEF(gb, .{ .none = {} }),
                0xF4 => UNDEF(gb, .{ .none = {} }),
                0xF5 => UNDEF(gb, .{ .none = {} }),
                0xF6 => UNDEF(gb, .{ .none = {} }),
                0xF7 => UNDEF(gb, .{ .none = {} }),
                0xF8 => UNDEF(gb, .{ .none = {} }),
                0xF9 => UNDEF(gb, .{ .none = {} }),
                0xFA => UNDEF(gb, .{ .none = {} }),
                0xFB => UNDEF(gb, .{ .none = {} }),
                0xFC => UNDEF(gb, .{ .none = {} }),
                0xFD => UNDEF(gb, .{ .none = {} }),
                0xFE => UNDEF(gb, .{ .none = {} }),
                0xFF => UNDEF(gb, .{ .none = {} }),
            },
        };
    }
};

pub const regID = enum(u3) {
    a,
    b,
    c,
    d,
    e,
    h,
    l,
};
/// Defines a GameBoy CPU: i8080 & Z80 hybrid chip
const CPU = struct {
    const mode = enum { DMG, CGB };
    registers: [7]u8 = undefined,
    f: FlagRegister = FlagRegister{},
    pc: u16 = undefined,
    sp: u16 = undefined,
    last_ins: u16 = 0x0,

    fn init(self: *@This()) !void {
        @memset(&self.registers, 0);
        self.pc = 0; //TODO: program start value
        self.sp = 0;
    }

    pub fn set_byte(self: *@This(), reg1: regID, value: u8) void {
        self.registers[@intFromEnum(reg1)] = value;
    }

    pub fn get_byte(self: *@This(), reg1: regID) u8 {
        return self.registers[@intFromEnum(reg1)];
    }

    pub fn set_word(self: *@This(), reg1: regID, value: u16) void {
        self.registers[@intFromEnum(reg1)] = @truncate((value & 0xFF00) >> 8);
        self.registers[@intFromEnum(reg1) + 1] = @truncate(value & 0x00FF);
    }

    pub fn get_word(self: *@This(), reg1: regID) u16 {
        return (@as(u16, self.registers[@intFromEnum(reg1)]) << 8) | self.registers[@intFromEnum(reg1) + 1];
    }

    fn execute(self: *@This(), gb: *GB) !u8 {
        const exe_zone = tracy.beginZone(@src(), .{ .name = "Execute Ins" });
        defer exe_zone.end();
        var byte = gb.read_byte(self.pc);
        var prefixed = false;
        if (byte == 0xCB) { // prefix byte
            prefixed = true;
            self.pc += 1;
            byte = gb.read_byte(self.pc);
        }
        // print("[pc]0x{X}\t(0x{X})\n", .{ self.pc, byte });
        const cycles_spent = InstructionSet.exe_from_byte(gb, byte, prefixed);
        if (cycles_spent == 255) return error.UNDEF_INSTRUCTION;
        // print("\n", .{});
        // if (self.last_ins == 0xFB) { // set IME flag after previous instruction
        //     // print("set IME\n", .{});
        // }
        return cycles_spent;
    }
};

const APU = struct {
    // TODO: implement sound

    fn init(self: *@This()) !void {
        _ = self;
        return;
    }
};

/// Defines a gameboy GPU(PPU)
/// - Handles writiing to vram and processing pixels from memory to the screen
const GPU = struct {
    const VRAM_BEGIN = 0x8000;
    const VRAM_END = 0x97FF;
    const VRAM_SIZE = VRAM_END - VRAM_BEGIN + 1;

    const OAM_BEGIN = 0xFE00;
    const OAM_END = 0xFE9F;
    const OAM_SIZE = OAM_END - OAM_BEGIN + 1;

    vram: *[VRAM_SIZE]u8 = undefined,
    oam: *[OAM_SIZE]u8 = undefined,
    // Stores sprite attributes (position, tile index, attributes).
    // Cannot be accessed during scanline rendering. -- writeOAM & read
    // Control rendering behavior.
    // Define which layers are enabled.
    special_registers: *[12]u8 = undefined,
    // LCD Control Registers (I/O Registers at $FF40–$FF4B)
    tile_set: [384]Tile = undefined,
    sprite_set: [10]Tile = undefined, // TODO actually sprite
    stat_reg: u8 = undefined,
    mode: Mode = undefined,
    lcd: LCD = undefined,

    scanline: [LCD.screenWidthPx]Color = undefined,
    mode_cycles_left: u16 = 456,
    // frame_cycles: usize = 0,
    frames_cycled: usize = 0,

    const Mode = enum { // modes specifying number of cycles per scanline
        HBLANK,
        VBLANK,
        SCAN,
        RENDER,
        const cycles: [4]u16 = .{ 204, 456, 80, 172 };
    };

    const Color = enum(u2) { black, dgray, lgray, white };
    const Tile = [8][8]Color;
    fn empty_tile(self: *@This()) Tile {
        _ = self;
        var tile: Tile = undefined;
        for (&tile) |*row| {
            @memset(row, .white);
        }
        return tile;
    }

    fn init(self: *@This(), gb: *GB) !void {
        self.vram = gb.memory[VRAM_BEGIN .. VRAM_END + 1];
        self.oam = gb.memory[OAM_BEGIN .. OAM_END + 1];
        self.special_registers = gb.memory[LCD.special_registers.start .. LCD.special_registers.end + 1];
        self.mode = .SCAN;
        self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.SCAN)];
        // try self.lcd.init();
        @memset(&self.tile_set, empty_tile(self));
    }

    fn setSpecialRegister(self: *@This(), register: LCD.special_registers, value: u8) void {
        self.special_registers[@intFromEnum(register)] = value;
    }
    fn getSpecialRegister(self: *@This(), register: LCD.special_registers) u8 {
        return self.special_registers[@intFromEnum(register)];
    }
    fn switchMode(self: *@This()) void {
        const zone = tracy.beginZone(@src(), .{ .name = "Mode switch" });
        defer zone.end();
        switch (self.mode) {
            .SCAN => {
                const scan_zone = tracy.beginZone(@src(), .{ .name = "SCAN" });
                defer scan_zone.end();
                self.mode = .RENDER;
                self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.RENDER)];
            },
            .RENDER => {
                // print("render!! \n\n\n\n\n", .{});
                const render_zone = tracy.beginZone(@src(), .{ .name = "RENDER" });
                defer render_zone.end();
                self.lcd.pushScanline(self.scanline, self.getSpecialRegister(.ly));
                self.mode = .HBLANK;
                self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.HBLANK)];
            },
            .HBLANK => {
                // Increment LY register
                const hblank_zone = tracy.beginZone(@src(), .{ .name = "HBLANK" });
                defer hblank_zone.end();
                const ly = self.getSpecialRegister(.ly);
                self.setSpecialRegister(.ly, ly + 1);
                if (ly + 1 == 144) {
                    // self.lcd.renderScreen(self.lcd.screen.len); // render at the last scanline
                    self.mode = .VBLANK;
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.VBLANK)]; // per scanline
                } else {
                    self.mode = .SCAN;
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.SCAN)];
                }
                // TODO INTERRUPT
            },
            .VBLANK => {
                const vblank_zone = tracy.beginZone(@src(), .{ .name = "VBLANK" });
                defer vblank_zone.end();
                const new_ly = self.getSpecialRegister(.ly) + 1;
                // self.setSpecialRegister(.ly, ly + 1);
                if (new_ly > 153) { // 153 is the end of VBLANK
                    self.setSpecialRegister(.ly, 0); // reset LY to 0
                    self.mode = .SCAN;
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.SCAN)];
                    self.frames_cycled += 1;
                    // print("fps: {d}\n", .{ @as(f64, @floatFromInt(self.frame_cycles)) / 70224});
                    // self.frame_cycles = 0;
                } else {
                    self.setSpecialRegister(.ly, new_ly);
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.VBLANK)];
                }
            },
        }
        // update STAT register
        var stat_reg = self.getSpecialRegister(.stat);
        stat_reg = (stat_reg & 0b1111_1100) | @intFromEnum(self.mode);
        self.setSpecialRegister(.stat, stat_reg);
    }

    fn do(self: *@This()) void {
        // Operate GPU here
        const zone = tracy.beginZone(@src(), .{ .name = "DO GPU CYCLES" });
        defer zone.end();
        switch (self.mode) {
            .SCAN => { // 2 searches OAM memory for sprites that should be rendered on the current scanline and stores them in a buffer
                // print("scanning\n", .{});
            },
            .RENDER => { // 3 transfers pixels to the LCD, one scanline at a time, duration variable
                // var scanline: [LCD.screenWidthPx]Color = undefined;
                // TODO: Generate the actual pixels for this scanline based on:
                // - Background tiles at the current scroll position
                // - Window tiles if enabled and visible on this line
                // - Sprites that were found during OAM scan
                @memset(&self.scanline, Color.white);
            },
            else => {}, // no action for hblank or vblank
        }
    }

    fn tick(self: *@This(), cycles: u16) void {
        // TODO the gpu should tick/cycle just as many
        // times as the cpu did, while being able to
        // process interrupts and continue on as well as changing modes midscanline when needed
        const zone = tracy.beginZone(@src(), .{ .name = "GPU TICK" });
        defer zone.end();
        var cycles_left = cycles; // amt of cycles spent by cpu
        while (cycles_left > 0) {
            const cycles_to_process: u16 = @min(cycles_left, self.mode_cycles_left);
            // self.frame_cycles += cycles_to_process;
            self.do();

            self.mode_cycles_left -= cycles_to_process;
            cycles_left -= cycles_to_process;

            if (self.mode_cycles_left == 0) {
                self.switchMode(); // handles drawing the screen, updating ly
                // print("Mode switch: {any}, LY: {d}, stat: {d}\n", .{ self.mode, self.getSpecialRegister(.ly), self.getSpecialRegister(.stat) });
                // TODO LYC CHECK
            }
        }
    }

    /// DMA Transfer from RAM to OAM mem
    /// - addresses (0xXX00 - 0xXX9F) are written to OAM
    fn writeOAM(self: *@This(), address: usize, value: u8) !void {
        if (self.mode == .RENDER or self.mode == .SCAN) {
            print("cannot access oam now\n", .{});
            return;
        }
        if (address <= 0xFE00 or address >= 0xFE9F) return error.OutOfOAMBounds;
        self.oam[address] = value;
    }

    fn readVram(self: *@This(), address: usize) u8 {
        return self.vram[address];
    }

    fn writeVram(self: *@This(), address: usize, value: u8) void {
        if (self.mode != .RENDER) {
            const fixed_address: u12 = @intCast(address - VRAM_BEGIN);
            self.vram[fixed_address] = value;
        }
    }

    fn vram_dump(self: *@This()) void {
        print("VRAM dump: \n", .{});
        for (self.vram, 0..VRAM_SIZE) |value, i| {
            print("0x{x}", .{value});
            if (i != 0 and i % 80 == 0) print("\n", .{});
        }
        print("\n", .{});
    }
};

///Contains the fields necessary to create a display,
///- Screen, Height, Width, Rendering
const LCD = struct {
    const initWinW: u16 = screenWidthPx + 2 * initSideScreen;
    const initWinH: u16 = screenHeightPx + aboveScreen + initBelowScreen;
    const aboveScreen = 20;
    const initSideScreen: u16 = 40;
    const initBelowScreen: u16 = 2 * screenHeightPx;
    // const pxScale: u8 = 1;
    const screenWidthPx = 160;
    const screenHeightPx = 144;
    var window_height: c_int = @intCast(initWinH);
    var window_width: c_int = @intCast(initWinW);
    var center: u16 = initWinW / 2;
    var belowScreen = initBelowScreen;
    var screenW = initWinW - 2 * initSideScreen;
    var screenH = initWinH - aboveScreen - initBelowScreen;
    var sidebar = initSideScreen;
    var pxSize: f32 = @as(f32, @floatFromInt(initWinW)) / screenWidthPx;

    screen: [screenHeightPx][screenWidthPx]GPU.Color = undefined,
    background: [32][32]GPU.Tile = undefined, // defines the background pixels of the gameboy
    window: [32][32]GPU.Tile = undefined, // defines the foreground and sprites
    renderer: *g.SDL_Renderer = undefined,
    win: *g.SDL_Window = undefined,
    grid_pixel_sz: u16 = undefined,

    const special_registers = enum(u8) {
        lcdc, // LCDC (LCD Control) Enables/disables layers, defines rendering mode
        stat, // $FF41 STAT (Status) Tracks PPU state
        scy, // $FF42 SCY (Scroll Y) Background vertical scroll
        scx, // $FF43 SCX (Scroll X) Background horizontal scroll
        ly, // current scanline
        lyc, // $FF45 LYC (Compare LY) Interrupt if LY matches LYC
        dma, // $FF46 DMA Transfers 160 bytes from RAM to OAM
        bgp, // $FF47 BGP (BG Palette) Defines colors for BG tiles
        obp0, // $FF48 OBP0 (OBJ Palette 0) Defines colors for sprite palette 0
        obp1, // $FF49 OBP1 (OBJ Palette 1) Defines colors for sprite palette 1
        wy, // $FF4A WY (Window Y) Window vertical position
        wx, // $FF4B WX (Window X) Window horizontal position

        const end = 0xFF4B;
        const start = 0xFF40;
        const size = 0xFF4B - 0xFF40 + 1;
    };

    fn init(self: *@This()) !void {
        var color: GPU.Color = undefined;
        for (&self.screen) |*row| {
            for (row) |*pixel| {
                var seed: u64 = undefined;
                try std.posix.getrandom(std.mem.asBytes(&seed));
                var prng = std.Random.DefaultPrng.init(seed);
                const rand = prng.random();
                color = rand.enumValue(GPU.Color);
                pixel.* = color;
            }
            // @memset(//row, color);
        }
        try self.startAndCreateRenderer(); // set window and renderer
    }

    fn setRect(self: *@This(), rect: *g.SDL_FRect, x: anytype, y: anytype, w: anytype, h: anytype) void {
        _ = self;
        rect.x = if (@TypeOf(x) == f32) x else @as(f32, @floatFromInt(x));
        rect.y = if (@TypeOf(y) == f32) y else @as(f32, @floatFromInt(y));
        rect.w = if (@TypeOf(w) == f32) w else @as(f32, @floatFromInt(w));
        rect.h = if (@TypeOf(h) == f32) h else @as(f32, @floatFromInt(h));
    }
    fn updateDimensions() void {
        screenH = @intFromFloat(@as(f32, @floatFromInt(window_height - aboveScreen)) * 0.4);
        screenW = screenH * screenWidthPx / screenHeightPx;
        if (screenW > window_width - 20) {
            screenW = @intCast(window_width - 20);
            screenH = screenW * screenHeightPx / screenWidthPx;
        }
        sidebar = center - screenW / 2;
        pxSize = @as(f32, @floatFromInt(screenH)) / screenHeightPx;
    }
    fn pushScanline(self: *@This(), new_scanline: [screenWidthPx]GPU.Color, ly: u8) void {
        @memcpy(&self.screen[ly], &new_scanline);
    }

    fn renderScreen(self: *@This(), frame_count: usize) void {
        // if (ly == 0) self.renderBody();
        _ = g.SDL_SetRenderDrawColor(self.renderer, 255, 192, 220, 255);
        _ = g.SDL_RenderClear(self.renderer);
        var rect = g.SDL_FRect{};
        // render the screen in the right place
        // SCREEN
        _ = g.SDL_SetRenderDrawColor(self.renderer, 230, 230, 230, 255);
        self.setRect(&rect, sidebar, aboveScreen - 5, screenW + 5, 5); // top of screen
        _ = g.SDL_RenderFillRect(self.renderer, &rect);

        self.setRect(&rect, sidebar - 5, aboveScreen - 5, 5, screenH + 10); // right side of screen
        _ = g.SDL_RenderFillRect(self.renderer, &rect);

        // SHADOW
        _ = g.SDL_SetRenderDrawColor(self.renderer, 50, 50, 50, 1);
        self.setRect(&rect, sidebar, aboveScreen + screenH, screenW, 5); // bottom of screen
        _ = g.SDL_RenderFillRect(self.renderer, &rect);

        self.setRect(&rect, sidebar + screenW, aboveScreen, 5, screenH + 5); // left side of screen
        _ = g.SDL_RenderFillRect(self.renderer, &rect);
        for (self.screen, 0..) |row, y| {
            for (row, 0..) |px, x| {
                _ = px;
                // _ = switch (px) {
                _ = switch (@as(GPU.Color, @enumFromInt(frame_count % 4))) {
                    .white => g.SDL_SetRenderDrawColor(self.renderer, 0, 200, 0, 255),
                    .lgray => g.SDL_SetRenderDrawColor(self.renderer, 0, 160, 0, 255),
                    .dgray => g.SDL_SetRenderDrawColor(self.renderer, 0, 120, 0, 255),
                    .black => g.SDL_SetRenderDrawColor(self.renderer, 0, 80, 0, 255),
                };
                const xPos = @as(f32, @floatFromInt(sidebar)) + @as(f32, @floatFromInt(x)) * pxSize;
                const yPos = @as(f32, @floatFromInt(aboveScreen)) + @as(f32, @floatFromInt(y)) * pxSize;
                self.setRect(&rect, xPos, yPos, pxSize, pxSize);
                _ = g.SDL_RenderFillRect(self.renderer, &rect);
            }
        }
        _ = g.SDL_RenderPresent(self.renderer);
    }

    fn startAndCreateRenderer(self: *@This()) !void {
        defer updateDimensions();
        if (!g.SDL_Init(g.SDL_INIT_VIDEO)) {
            print("SDL_Init failed: {s}\n", .{g.SDL_GetError()});
            return error.InitializationFailed;
        }
        var win: ?*g.SDL_Window = null;
        var renderer: ?*g.SDL_Renderer = null;
        if (!g.SDL_CreateWindowAndRenderer("gameboy!", initWinW, initWinH, 0, &win, &renderer)) {
            print("Failed to create window or renderer: {s}\n", .{g.SDL_GetError()});
            return error.CreationFailure;
        }
        if (win == null) {
            print("Failed to create window: {s}\n", .{g.SDL_GetError()});
            return error.WindowNull;
        }
        _ = g.SDL_SetWindowResizable(win.?, true);
        _ = g.SDL_SetWindowMinimumSize(win.?, initWinW, initWinH);

        if (renderer == null) {
            print("Failed to create renderer: {s}\n", .{g.SDL_GetError()});
            return error.RendererFailure;
        }
        self.renderer = renderer.?;
        self.win = win.?;
        if (!g.SDL_GetWindowSizeInPixels(self.win, &window_width, &window_height)) {
            print("err while getting window size: {s}\n", .{g.SDL_GetError()});
            return error.NoWinSize;
        } else {
            print("window size: {d}x{d}\n", .{ window_width, window_height });
            if (window_height == 0 or window_width == 0) {
                return error.DetectedZeroWidthWin;
            }
        }
    }
    pub fn screen_dump(self: *@This()) void {
        print("Actual memspace dump: \n", .{});
        for (self.screen) |row| {
            for (row) |pixel| {
                print("{any}", .{pixel});
            }
        }
        print("\n", .{});
    }
    fn endSDL(self: *@This()) void {
        g.SDL_Quit();
        g.SDL_DestroyWindow(self.win);
        g.SDL_DestroyRenderer(self.renderer);
    }
};
const Clock = struct {
    timer: std.time.Timer = undefined,
    ticks: usize = 0,
    ns_elapsed: usize = 0,
    fn start(self: *Clock) !void {
        self.timer = try std.time.Timer.start();
    }
    fn tick(self: *Clock) bool {
        const zone = tracy.beginZone(@src(), .{ .name = "Tick" });
        defer zone.end();
        const ns_passed = self.timer.lap();
        self.ns_elapsed += ns_passed;
        self.ticks += 1;
        // TODO tick at 239 ns per tick
        // if (self.ticks % ticks_per_frame == 0)
        //     print("fps: {d}\n", .{self.fpsCounter()});
        // print("ticked after: {d} ns\n", .{ns_passed});
        return true;
    }
    fn fpsCounter(self: *Clock) usize {
        return @intFromFloat(@as(f64, @floatFromInt(self.ticks / ticks_per_frame)) / (@as(f64, @floatFromInt(self.ns_elapsed)) / std.time.ns_per_s));
    }

    const ticks_per_s = 4.19 * @as(f64, std.math.pow(u64, 10, 6));
    const fps = 60;

    const ticks_per_ns = ticks_per_s / std.time.ns_per_s;
    const ns_per_tick = 1 / ticks_per_ns;
    const ticks_per_frame = 70224;
    const frames_per_s = 1 / ticks_per_frame * ticks_per_s;
};
/// Gameboy Machine, defer endGB
pub const GB = struct {
    cpu: CPU = CPU{},
    gpu: GPU = GPU{},
    apu: APU = APU{},
    memory: [0xFFFF]u8 = undefined,
    running: bool = undefined,
    cycles_spent: usize = 0,
    clock: Clock = Clock{},
    last_frame: std.time.Instant = undefined,

    const sr = LCD.special_registers;
    /// nintendo logo
    const LOGO: [48]u8 = .{ 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E };

    const freqMHz = @as(f64, 1) / 4.19;
    const cycle_time_ns = @round(freqMHz * std.math.pow(u16, 10, 3)); // tick at 4.19 MHz
    //
    fn fpsCounter(self: *GB) f64 {
        return (@as(f64, @floatFromInt(self.gpu.frames_cycled))) / (@as(f64, @floatFromInt(self.clock.ns_elapsed)) / std.time.ns_per_s);
    }

    pub fn init(self: *@This()) !void {
        @memset(&self.memory, 0);
        @memcpy(self.memory[0x104 .. 0x133 + 1], &LOGO);
        try self.cpu.init();
        try self.gpu.init(self);
        _ = InstructionSet.exe_from_byte(self, 0, false); // dummy op to init cache
        self.cpu.pc -= 1;
        self.running = true;
    }

    pub fn read_byte(self: *@This(), address: usize) u8 {
        const zone = tracy.beginZone(@src(), .{ .name = "read mem byte" });
        defer zone.end();
        // TODO: implement memory mapping based on address
        return self.memory[address];
    }

    pub fn writeByte(self: *@This(), address: usize, value: u8) void {
        const zone = tracy.beginZone(@src(), .{ .name = "write mem byte" });
        defer zone.end();
        switch (address) {
            GPU.VRAM_BEGIN...GPU.VRAM_END + 1 => {
                // println("mem before: mem@0x{X} = 0x{X}", .{ address, self.read_byte(address) });
                self.gpu.writeVram(address, value);
                // println("mem after: mem@0x{X} = 0x{X}", .{ address, self.read_byte(address) });
            },
            @intFromEnum(LCD.special_registers.dma) => {
                const prefix = address / 0x100;
                const ram_address: u16 = @as(u16, @intCast(prefix)) << 8;
                @memcpy(self.memory[GPU.OAM_BEGIN..GPU.OAM_END], self.memory[ram_address .. ram_address + GPU.OAM_SIZE]);
            },
            // 0xFF50 => { // Disable bootrom register
            //     // Unmap and replace the bootrom with cartridge data
            // },
            else => {
                self.memory[address] = value;
            },
        }
    }

    pub fn boot(self: *@This()) !void {
        const bootFile = try std.fs.cwd().openFile("roms/dmg_boot.bin", .{});
        defer bootFile.close();
        const bootFileStats = try bootFile.stat();
        const bootFileBuf: []u8 = try bootFile.readToEndAlloc(allocator, bootFileStats.size);
        for (0..bootFileBuf.len) |i| {
            self.memory[i] = bootFileBuf[i];
        }
    }

    pub fn load_game(self: *@This(), game_path: []const u8) !void {
        const romHeaderStart = 0x0100;
        const rom = try std.fs.cwd().openFile(game_path, .{});
        defer rom.close();
        const stats = try rom.stat();
        const buf: []u8 = try rom.readToEndAlloc(allocator, stats.size);
        print("Reading bytes...\n", .{});
        for (0..buf.len) |i| {
            self.memory[i + romHeaderStart] = buf[i];
            // print("0x{x} ", .{byte});
        }
    }

    pub fn go(self: *@This()) !void {
        try self.clock.start();
        while (self.cpu.pc < 0x100 and self.running) {
            // try self.getEvents()
            _ = self.clock.tick();
            try self.do();
            //TODO: Update peripherals & timing
        }
    }
    fn do(self: *@This()) !void {
        const cycles_spent = try self.cpu.execute(self);
        const ly = self.gpu.getSpecialRegister(.ly);
        if (ly == 0) print("fps: {d}\n", .{self.fpsCounter()});
        // print("ly: {d}", .{ly});
        self.cycles_spent += cycles_spent;
        self.gpu.tick(cycles_spent * 4);
    }
    fn getEvents(self: *@This()) !void {
        var event: g.SDL_Event = undefined;
        while (g.SDL_PollEvent(&event)) {
            switch (event.type) {
                g.SDL_EVENT_KEY_DOWN => {},
                g.SDL_EVENT_KEY_UP => {},
                g.SDL_EVENT_QUIT => {
                    self.running = false;
                },
                g.SDL_EVENT_WINDOW_RESIZED => {
                    print("RESIZED, NEW SIZE\n\n\n\n\n\n", .{});
                    _ = g.SDL_GetWindowSizeInPixels(self.gpu.lcd.win, &LCD.window_width, &LCD.window_height);
                    LCD.updateDimensions();
                },
                else => {},
            }
        }
    }
    pub fn mem_dump(self: *@This(), start: u16, end: u16) void {
        print("printing bytes:\n", .{});
        for (self.memory[start..end], start..end) |value, i| {
            if (i != 0 and i % 16 == 0) print("\n", .{});
            print("0x{x} ", .{value});
        }
        print("\n", .{});
    }

    pub fn gfx_dump(self: *@This()) void {
        print("Actual memspace dump: \n", .{});
        for (self.memory[0x8000 .. 0x97FF + 1], 0..(0x97FF - 0x8000 + 1)) |value, i| {
            print("0x{x}", .{value});
            if (i != 0 and i % 80 == 0) print("\n", .{});
        }
        print("\n", .{});
    }

    pub fn reg_dump(self: *@This()) void {
        print("Actual memspace dump:\n", .{});
        // for (self.memory[LCD.special_registers.start .. LCD.special_registers.end + 1], LCD.special_registers.start..LCD.special_registers.end + 1) |value, i| {
        //     println("register@0x{x}: 0x{x} ", .{i, value});
        // }
        const i = 0xFF44;
        print("register@0x{x}: 0x{x}\n", .{ i, self.gpu.getSpecialRegister(.ly) });
        // println("register@0x{x}: 0x{x} ", .{i, value});
        print("\n", .{});
    }

    fn endGB(self: *@This()) void {
        self.gpu.lcd.endSDL();
    }
};

pub fn main() !void {
    const zone = tracy.beginZone(@src(), .{ .name = "Main" });
    defer zone.end();
    var gb = GB{};
    print("ns per tick {d}\n", .{Clock.ns_per_tick});
    gb.init() catch |err| {
        print("Couldn't inititalize GameBoy, Error: {any}\n", .{err});
        return;
    };
    print("GB init!\n", .{});
    gb.boot() catch |err| { // loads the boot rom and executes it
        print("Couldn't boot GameBoy, err: {any}\t", .{err});
        return;
    };
    defer gb.endGB();

    // tracy.

    try gb.go();
}
