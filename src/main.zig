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
    std.debug.print(fmt, args);
}
const allocator = std.heap.page_allocator;

const InstructionSet = struct {
    const InstrFn = *const fn (*GB, InstrArgs) u8;
    const InstrArgs = union(enum) { none: void, target: regID, bit_target: struct { bit: u3, target: regID }, flagConditions: Condition, targets: struct { to: regID, from: regID } };
    const Condition = union(enum) { none, z, c, nz, nc };
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
        const h = (value & 0xF + 1) & 0x10 == 0x10; // half carry conditions
        const z = gb.cpu.get_byte(args.target) == 0;
        const s = false;
        const c = gb.cpu.f.cFlag();
        gb.cpu.f.write(z, c, h, s);
        gb.cpu.pc += 1;
        return 1;
    }
    fn INCr16(gb: *GB, args: InstrArgs) u8 { // TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "INCr16" });
        defer zone.end();
        const value = gb.cpu.get_word(args.target);
        const res = @addWithOverflow(value, 1)[0];
        // print("INCr16, target: {any}\n 0x{X} + 1 = 0x{X}\n", .{ args.target, value, res });
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
        const h = (value & 0xF) == 0x00; // half carry conditions
        const z = res == 0;
        const s = true;
        const c = gb.cpu.f.cFlag();
        gb.cpu.f.write(z, c, h, s);
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
        const n: u8 = gb.readByte(gb.cpu.pc + 1);
        // print("LD8, target {any}, n: Ox{X}\n", .{ @as(regID, args.target), n });
        // print("n b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.set_byte(args.target, n);
        gb.cpu.pc += 2;
        return 2;
    }
    fn LD16(gb: *GB, args: InstrArgs) u8 { // LD r16, n16 TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "LD16" });
        defer zone.end();
        // print("pc: 0x{X}\n", .{ gb.cpu.pc });
        const n: u16 = @as(u16, gb.readByte(gb.cpu.pc + 2)) << 8 | gb.readByte(gb.cpu.pc + 1);
        // print("LD16, target {any}, n: Ox{X}\n", .{ @as(regID, args.target), n });
        // print("n b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.readByte(gb.cpu.pc + 2) });
        // print("n b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.readByte(gb.cpu.pc + 1) });
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
        const n: u16 = @as(u16, gb.readByte(gb.cpu.pc + 2)) << 8 | gb.readByte(gb.cpu.pc + 1);
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
        const value = gb.readByte(gb.cpu.pc + 1);
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
        // print("[pc]:0x{X}\t", .{gb.cpu.pc});
        const zone = tracy.beginZone(@src(), .{ .name = "LDHLIA" });
        defer zone.end();
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(regID.a);
        // print("LDHLIA, \nmem@0x{X}: 0x{X} --> 0x{X}\n", .{ hl, gb.readByte(hl), value });
        gb.writeByte(hl, value);
        gb.cpu.set_word(regID.h, hl + 1);
        // print("after op: hl+1= 0x{X}\t mem@0x{X}: 0x{X}\n", .{ gb.cpu.get_word(regID.h), hl, gb.readByte(hl) });
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
        const byte = gb.readByte(0xFF00 + @as(u16, c));
        // print("LDHAC byte: 0x{X} --> A\n", .{byte});
        gb.cpu.set_byte(regID.a, byte);
        gb.cpu.pc += 1;
        return 2;
    }
    fn LDAn16(gb: *GB, _: InstrArgs) u8 { // TODO TEST Load value in register A from the byte at address n16.
        const zone = tracy.beginZone(@src(), .{ .name = "LDAn16" });
        defer zone.end();
        const memory_place = @as(u16, gb.readByte(gb.cpu.pc + 2)) << 8 | gb.readByte(gb.cpu.pc + 1);
        const n = gb.readByte(memory_place);
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
        const memory_place = 0xFF00 + @as(u16, gb.readByte(gb.cpu.pc + 1));
        // print("LDHAn16, \n", .{});
        // print("memplace b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, memory_place });
        // print("memplace b2: [pc]{d} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        const n = gb.readByte(memory_place);
        // print("n: 0x{X} --> A\n", .{n});
        gb.cpu.set_byte(regID.a, n);
        gb.cpu.pc += 2;
        return 3;
    }
    fn LDn16A(gb: *GB, _: InstrArgs) u8 { // TODO TEST Store value in register A into the byte at address n16.
        const zone = tracy.beginZone(@src(), .{ .name = "LDn16A" });
        defer zone.end();
        const memory_place = @as(u16, gb.readByte(gb.cpu.pc + 2)) << 8 | gb.readByte(gb.cpu.pc + 1);
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
        const memory_place = 0xFF00 + @as(u16, gb.readByte(gb.cpu.pc + 1));
        // print("memplace b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, memory_place });
        // print("memplace b2: [pc]{d} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        const n = gb.cpu.get_byte(regID.a);
        gb.writeByte(memory_place, n);
        // print("n: 0x{X} --> memplace@0x{X}\n", .{ n, memory_place });
        gb.cpu.pc += 2;
        return 3;
    }
    fn LDAr16(gb: *GB, args: InstrArgs) u8 { // TODO TEST Load value in register A from the byte pointed to by register r16.
        const zone = tracy.beginZone(@src(), .{ .name = "LDAr16" });
        defer zone.end();
        const memory_place = gb.cpu.get_word(args.target);
        const n = gb.readByte(memory_place);
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
            // const z = true;
            gb.cpu.f.write(true, gb.cpu.f.cFlag(), gb.cpu.f.hFlag(), gb.cpu.f.sFlag());
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
        const s = false;
        const c = res[1] == 1;
        const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
        const z = res[0] == 0;
        gb.cpu.f.write(z, c, h, s);
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
        const c = value > a;
        const s = true;
        const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
        const z = res[0] == 0;
        gb.cpu.f.write(z, c, h, s);
        gb.cpu.set_byte(regID.a, res[0]);
        gb.cpu.pc += 1;
        return 1;
    }
    fn ADDAHL(gb: *GB, _: InstrArgs) u8 { // TODO Add the byte pointed to by HL to A.
        const zone = tracy.beginZone(@src(), .{ .name = "ADDAHL" });
        defer zone.end();
        const mem_place = gb.cpu.get_word(regID.h);
        const value = gb.readByte(mem_place);
        // print("ADDAHL:A + mem@0x{X}: value: {d} \n", .{ mem_place, value });
        const a = gb.cpu.get_byte(regID.a);
        const res: struct { u8, u1 } = @addWithOverflow(a, value);
        const s = false;
        const c = res[1] == 1;
        const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
        const z = res[0] == 0;
        gb.cpu.f.write(z, c, h, s);
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
        const s = false;
        const h = (((hl + value) >> 8) & 0xF) & 0x10 == 0x10; // half carry conditions
        const c = (((hl + value) >> 12) & 0xF) & 0x10 == 0x10;
        const z = gb.cpu.f.zFlag();
        gb.cpu.f.write(z, c, h, s);
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
        const c = (@as(u1, @truncate(a)) == 1);
        const s = gb.cpu.f.sFlag();
        const h = gb.cpu.f.hFlag();
        const z = gb.cpu.f.zFlag();
        gb.cpu.f.write(z, c, h, s);
        gb.cpu.pc += 1;
        return 1;
    }
    fn RLA(gb: *GB, _: InstrArgs) u8 {
        const carried = gb.cpu.f.cFlag();
        const reg = gb.cpu.get_byte(regID.a);
        // print("[pc]:0x{X}\t", .{gb.cpu.pc});
        // print("RLA, prior: 0b{b}, carried = {d}\n", .{ reg, @intFromBool(carried) });
        const c = reg >> 7 == 1;
        const rotated: u8 = reg << 1 | @intFromBool(carried);
        const z = false;
        const h = false;
        const s = false;
        gb.cpu.f.write(z, c, h, s);
        gb.cpu.set_byte(regID.a, rotated);
        // print("rotated: 0b{b}\n", .{rotated});
        gb.cpu.pc += 1;
        return 1;
    }
    fn PUSH(gb: *GB, args: InstrArgs) u8 {
        const zone = tracy.beginZone(@src(), .{ .name = "PUSH" });
        defer zone.end();
        var high: u8 = undefined;
        var low: u8 = undefined;
        // print("[pc]:0x{X}\t", .{gb.cpu.pc});
        if (args.target == regID.a) {
            high = gb.cpu.get_byte(regID.a);
            low = gb.cpu.f.value & 0xF0; // only the upper 4 bits
            // print("PUSH AF a: 0x{X}, f: 0x{X}\n", .{ high, low });
        } else {
            const value = gb.cpu.get_word(args.target);
            high = @truncate(value >> 8);
            low = @truncate(value);
            // print("PUSH 0x{X} from {any}, hi 0x{X} lo 0x{X}\n", .{ value, args.target, high, low });
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
        const low = gb.readByte(gb.cpu.sp);
        gb.cpu.sp += 1;
        const high = gb.readByte(gb.cpu.sp);
        gb.cpu.sp += 1;
        // print("[pc]:0x{X}\t", .{gb.cpu.pc});
        const value = @as(u16, high) << 8 | low;
        // print("POP 0x{X} --> {any}\n", .{ value, args.target });
        if (args.target == regID.a) {
            gb.cpu.set_byte(regID.a, high);
            gb.cpu.f.value = low & 0xF0;
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
            const n = @as(u16, gb.readByte(gb.cpu.pc + 2)) << 8 | gb.readByte(gb.cpu.pc + 1);
            // print("to 0x{X}\n", .{n});
            gb.cpu.pc = n;
            return 4; // 4 cycles when taken
        } else {
            gb.cpu.pc += 3;
            return 3; // 3 cycles when not taken
        }
    }
    fn JR(gb: *GB, args: InstrArgs) u8 { // TODO ONLY 2 CYCLES IF NOT TAKEN OTHERWISE 3
        const zone = tracy.beginZone(@src(), .{ .name = "JR", .color = 0x00FF00 });
        defer zone.end();
        // print("JR, \tcondition:", .{});
        const dist: i8 = @bitCast(gb.readByte(gb.cpu.pc + 1));
        const jump = gb.cpu.f.check(args.flagConditions);
        // print(" by dist: [pc]0x{X} \t0x{X} ({d}) bytes \n", .{ gb.cpu.pc + 1, dist, dist });
        // const byte = gb.read_byte(gb.cpu.pc);
        if (jump) {
            const new_mem: u16 = @bitCast(@addWithOverflow(@as(i16, @intCast(gb.cpu.pc + 2)), dist)[0]);
            gb.cpu.pc = new_mem;
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
            // print("DE = 0x{X}, HL = 0x{X}\n", .{ gb.cpu.get_word(regID.d), gb.cpu.get_word(regID.h) });
            // print("[pc]:0x{X}\t", .{gb.cpu.pc});
            const n = @as(u16, gb.readByte(gb.cpu.pc + 2)) << 8 | gb.readByte(gb.cpu.pc + 1);
            const ret = gb.cpu.pc + 3;
            // print("CALL to 0x{X}, later RET to 0x{X}\n", .{ n, ret });
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
        // print("[pc]:0x{X}\t", .{gb.cpu.pc});
        // print("RET, condition:{any}", .{args.flagConditions});
        const ret = gb.cpu.f.check(args.flagConditions);
        if (ret) {
            const low = gb.readByte(gb.cpu.sp);
            gb.cpu.sp += 1;
            const high = gb.readByte(gb.cpu.sp);
            gb.cpu.sp += 1;
            gb.cpu.pc = @as(u16, high) << 8 | low;
            return 5; // 5 cycles if condition met
        } else {
            gb.cpu.pc += 1;
            return 2; // 2 cycles when not taken
        }
    }
    fn CPAn8(gb: *GB, _: InstrArgs) u8 { // TODO TEST;
        const zone = tracy.beginZone(@src(), .{ .name = "CPAn8", .color = 0x00FF00 });
        defer zone.end();
        // print("CPAn8, pc:[0x{X}] \n", .{gb.cpu.pc});
        const n = gb.readByte(gb.cpu.pc + 1);
        const reg = gb.cpu.get_byte(regID.a);
        // print("n b1: [pc]0x{X} \t(0x{X}), A: 0x{X}\n", .{ gb.cpu.pc + 1, n, gb.cpu.get_byte(regID.a) });
        // const res = @subWithOverflow(reg, n);
        const z = reg == n;
        const s = true;
        const h = (reg & 0xF) < (n & 0xF); // half carry conditions
        const c = reg < n;
        gb.cpu.f.write(z, c, h, s);
        gb.cpu.pc += 2;
        return 2;
    }
    fn CPAr8(gb: *GB, args: InstrArgs) u8 { // TODO TEST;
        const zone = tracy.beginZone(@src(), .{ .name = "CPAr8", .color = 0x00FF00 });
        defer zone.end();
        const n = gb.cpu.get_byte(args.target);
        // print("CPAr8, target = {any}\n", .{args.target});
        const reg = gb.cpu.get_byte(regID.a);
        // const res = @subWithOverflow(reg, n);
        const z = reg == n;
        const s = true;
        const h = (reg & 0xF) < (n & 0xF); // half carry conditions
        const c = reg < n;
        gb.cpu.f.write(z, c, h, s);
        gb.cpu.pc += 1;
        return 1;
    }
    fn CPAHL(gb: *GB, _: InstrArgs) u8 { // TODO TEST;
        const zone = tracy.beginZone(@src(), .{ .name = "CPAHL", .color = 0x00FF00 });
        defer zone.end();
        const hl = gb.cpu.get_word(regID.h);
        const reg = gb.cpu.get_byte(regID.a);
        const byte = gb.readByte(hl);
        // print("CPAHL, compare mem_place: 0x{X} ({d}) to A:{d}\n", .{ hl, gb.read_byte(hl), reg });
        // const res = @subWithOverflow(reg, hl);
        const z = reg == byte;
        const s = true;
        const h = (reg & 0xF) < (byte & 0xF); // half carry conditions
        const c = reg < byte;
        gb.cpu.f.write(z, c, h, s);
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
        const z = @as(u1, @truncate(target >> bit)) == 0; // set zero flag if the target bit is not set
        const h = true; // set half carry
        const s = false;
        const c = gb.cpu.f.cFlag();
        gb.cpu.f.write(z, c, h, s);
        gb.cpu.pc += 1;
        return 2;
    }
    fn BITTESTHL(gb: *GB, args: InstrArgs) u8 { // TODO TEST
        const zone = tracy.beginZone(@src(), .{ .name = "BITTESTHL" });
        defer zone.end();
        const bit: u3 = args.bit_target.bit;
        // print("BITTESTHL, target: {any}, bit: {any}\n", .{ args.bit_target.target, args.bit_target.bit });
        const hl = gb.cpu.get_word(args.target);
        const byte = gb.readByte(hl);
        const z = @as(u1, @truncate(byte >> bit)) == 0;
        const h = true;
        const c = gb.cpu.f.cFlag();
        const s = false;
        gb.cpu.f.write(z, c, h, s);
        gb.cpu.pc += 1;
        return 3;
    }
    fn RL(gb: *GB, args: InstrArgs) u8 { // TODO TEST Rotate bits in register r8 left through carry.
        const zone = tracy.beginZone(@src(), .{ .name = "RL" });
        defer zone.end();
        const carried = gb.cpu.f.cFlag();
        const reg = gb.cpu.get_byte(args.target);
        // print("[pc]:0x{X}\t", .{gb.cpu.pc});
        // print("RL, target: {any}, prior: 0b{b}, carried = {d}\n", .{ args.target, reg, @intFromBool(carried) });
        const c = reg >> 7 == 1;
        const rotated: u8 = reg << 1 | @intFromBool(carried);
        const z = rotated == 0;
        const h = false;
        const s = false;
        gb.cpu.f.write(z, c, h, s);
        gb.cpu.set_byte(args.target, rotated);
        // print("after: 0b{b}\n", .{gb.cpu.get_byte(args.target)});
        gb.cpu.pc += 1;
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
                0x17 => RLA(gb, .{ .none = {} }),
                0x18 => JR(gb, .{ .flagConditions = .none }),
                0x19 => ADDHLr16(gb, .{ .target = regID.d }),
                0x1A => LDAr16(gb, .{ .target = regID.d }),
                0x1B => DECr16(gb, .{ .target = regID.d }),
                0x1C => INCr8(gb, .{ .target = regID.e }),
                0x1D => DECr8(gb, .{ .target = regID.e }),
                0x1E => LD8(gb, .{ .target = regID.e }),
                0x1F => UNDEF(gb, .{ .none = {} }),
                0x20 => JR(gb, .{ .flagConditions = .nz }),
                0x21 => LD16(gb, .{ .target = regID.h }),
                0x22 => LDHLIA(gb, .{ .none = {} }),
                0x23 => INCr16(gb, .{ .target = regID.h }),
                0x24 => INCr8(gb, .{ .target = regID.h }),
                0x25 => UNDEF(gb, .{ .none = {} }),
                0x26 => UNDEF(gb, .{ .none = {} }),
                0x27 => UNDEF(gb, .{ .none = {} }),
                0x28 => JR(gb, .{ .flagConditions = .z }),
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
                0xC3 => JP(gb, .{ .flagConditions = .none }),
                0xC4 => UNDEF(gb, .{ .none = {} }),
                0xC5 => PUSH(gb, .{ .target = regID.b }),
                0xC6 => UNDEF(gb, .{ .none = {} }),
                0xC7 => UNDEF(gb, .{ .none = {} }),
                0xC8 => UNDEF(gb, .{ .none = {} }),
                0xC9 => RET(gb, .{ .flagConditions = .none }),
                0xCA => UNDEF(gb, .{ .none = {} }),
                0xCB => UNDEF(gb, .{ .none = {} }),
                0xCC => UNDEF(gb, .{ .none = {} }),
                0xCD => CALLn16(gb, .{ .flagConditions = .none }),
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
    pub fn init(self: *@This()) !void {
        @memset(&self.registers, 0);
        self.pc = 0; //TODO: program start value
        self.sp = 0;
    }
    // cpu execution
    pub fn execute(self: *@This(), gb: *GB) !u8 {
        const exe_zone = tracy.beginZone(@src(), .{ .name = "Execute Ins" });
        defer exe_zone.end();
        var byte = gb.readByte(self.pc);
        var prefixed = false;
        if (byte == 0xCB) { // prefix byte
            prefixed = true;
            self.pc += 1;
            byte = gb.readByte(self.pc);
        }
        // print("[pc]0x{X}\t(0x{X})\n", .{ self.pc, byte});
        const cycles_spent = InstructionSet.exe_from_byte(gb, byte, prefixed);
        if (cycles_spent == 255) return error.UNDEF_INSTRUCTION;
        // print("\n", .{});
        // if (self.last_ins == 0xFB) { // set IME flag after previous instruction
        //     // print("set IME\n", .{});
        // }
        return cycles_spent;
    }
    // memory ops
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
    // types & context
    const FlagRegister = struct {
        value: u8 = 0,
        inline fn cFlag(self: *FlagRegister) bool {
            return self.value & (1 << 4) != 0;
        }
        inline fn zFlag(self: *FlagRegister) bool {
            return self.value & (1 << 7) != 0;
        }
        inline fn hFlag(self: *FlagRegister) bool {
            return self.value & (1 << 5) != 0;
        }
        inline fn sFlag(self: *FlagRegister) bool {
            return self.value & (1 << 6) != 0;
        }
        inline fn write(self: *FlagRegister, z: bool, c: bool, h: bool, s: bool) void {
            self.value = (@as(u8, @intFromBool(z)) << 7) | // Z
                (@as(u8, @intFromBool(s)) << 6) | // N always set
                (@as(u8, @intFromBool(h)) << 5) |
                (@as(u8, @intFromBool(c)) << 4);
        }
        inline fn check(self: *FlagRegister, cond: InstructionSet.Condition) bool {
            return switch (cond) {
                .z => self.zFlag(), // Z
                .nz => !self.zFlag(),
                .c => self.cFlag(), // C
                .nc => !self.cFlag(),
                .none => true,
            };
        }
    };
    const WRAM_START = 0xC000;
    const WRAM_END = 0xDFFF;
};

const APU = struct {
    // TODO: implement sound
    // startup
    fn init(self: *@This()) !void {
        _ = self;
        return;
    }
    // apu execution
    // mem ops
    // types & context
};

/// Defines a gameboy GPU(PPU)
/// - Handles writiing to vram and processing pixels from memory to the screen
const GPU = struct {
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
    scanline_progress: u8 = 0,
    mode_cycles_left: u16 = 456,
    frames_cycled: usize = 0,
    frame_cycles_spent: u64 = 0,
    // rand: std.Random = undefined,

    fn empty_tile(self: *@This()) Tile {
        _ = self;
        var tile: Tile = undefined;
        for (&tile) |*row| {
            @memset(row, .white);
        }
        return tile;
    }
    // startup
    fn init(self: *@This(), gb: *GB) !void {
        self.vram = gb.memory[VRAM_BEGIN .. VRAM_END + 1];
        self.oam = gb.memory[OAM_BEGIN .. OAM_END + 1];
        self.special_registers = gb.memory[LCD.special_registers.start .. LCD.special_registers.end + 1];
        self.mode = .SCAN;
        self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.SCAN)];
        try self.lcd.init();
        @memset(&self.tile_set, empty_tile(self));
    }
    // gpu execution
    fn tick(self: *@This(), cycles: u16) void {
        // TODO the gpu should tick/cycle just as many
        // times as the cpu did, while being able to
        // process interrupts and continue on as well as changing modes midscanline when needed
        // const zone = tracy.beginZone(@src(), .{ .name = "GPU TICK" });
        // defer zone.end();
        var cycles_left = cycles; // amt of cycles spent by cpu
        self.frame_cycles_spent += cycles_left;
        while (cycles_left > 0) {
            const cycles_to_process: u8 = @intCast(@min(cycles_left, self.mode_cycles_left));
            // self.frame_cycles += cycles_to_process;
            self.do(cycles_to_process);
            self.mode_cycles_left -= cycles_to_process;
            cycles_left -= cycles_to_process;
            if (self.mode_cycles_left == 0) {
                self.switchMode(); // handles drawing the screen, updating ly
                // print("Mode switch: {any}, LY: {d}, stat: {d}\n", .{ self.mode, self.getSpecialRegister(.ly), self.getSpecialRegister(.stat) });
                // TODO LYC CHECK
            }
        }
    }
    fn do(self: *@This(), cycles: u8) void {
        // Operate GPU here
        // const zone = tracy.beginZone(@src(), .{ .name = "DO GPU CYCLES" });
        // defer zone.end();
        var cycles_to_spend: f16 = @floatFromInt(cycles);

        switch (self.mode) {
            .SCAN => { // 2 searches OAM memory for sprites that should be rendered on the current scanline and stores them in a buffer
                // decoding:
                // | byte 0 | y pos
                // | byte 1 | x pos
                // | byte 2 | tile number --  the Tile Number used for fetching the graphics data for the sprite
                // | byte 3 | sprite flags:
                // {bit 7} OBJ-BG priority 0 (priority) 1 (1-3 priority)
                // {bit 6} Y-flip
                // {bit 5} X-flip
                // {bit 4} pallete number
                // print("scanning\n", .{});
                // const sprite_buffer: u8[40] = .{};
                // var i: u8 = 0;
                // var buffer_i: u8 = 0;
                // while (buffer_i < 40 and i <= OAM_SIZE) : (i+=4) {
                //     const y_pos = self.oam[i];
                //     const x_pos = self.oam[i+1];
                //     if (x_pos > 0 and ly + 16 >= y_pos) {
                //         for(sprite_buffer[buffer_i]
                //     }
                // }
                return;
            },
            .RENDER => { // 3 transfers pixels to the LCD, one scanline at a time, duration variable
                // TODO: Generate the actual pixels for this scanline based on:
                // - Background tiles at the current scroll position
                // - Window tiles if enabled and visible on this line
                // - Sprites that were found during OAM scan
                if (!self.testSpecialRegister(.lcdc, 7)) return;
                const ly = self.getSpecialRegister(.ly);
                const bg_cpp = 1.0075; // cycles per pixel
                const use_signed = !self.testSpecialRegister(.lcdc, 4);
                const base: i32 = if (use_signed) 0x9000 else 0x8000;
                const tilemap_base: u16 = if (self.testSpecialRegister(.lcdc, 3)) 0x9C00 else 0x9800; // check lcdc bit 3
                const scx = self.getSpecialRegister(.scx);
                const scy = self.getSpecialRegister(.scy);
                var x = self.scanline_progress;
                while (x < self.scanline.len and cycles_to_spend > 0) {
                    const bg_y: u16 = (ly + scy) & 0xFF; // wraps at 256
                    const bg_x: u16 = @intCast((x + scx) & 0xFF);

                    const tile_y: u16 = bg_y / 8;
                    const tile_x: u16 = bg_x / 8;

                    const tile_index_addr: u16 = tilemap_base + tile_y * 32 + tile_x;

                    const tile_index: i16 = if (use_signed) @intCast(@as(i8, @bitCast(self.readVram(tile_index_addr)))) else @intCast(self.readVram(tile_index_addr));
                    const tile_line: u16 = bg_y % 8;

                    const tile_addr: u16 = @intCast(base + tile_index * 16 + @as(i32, @intCast(tile_line * 2)));
                    const low = self.readVram(tile_addr);
                    const high = self.readVram(tile_addr + 1);

                    const pixel: u3 = @intCast(bg_x % 8);
                    self.scanline[x] = self.tileDecoder(high, low, pixel);
                    cycles_to_spend -= bg_cpp;
                    x += 1;
                    self.scanline_progress = x;
                    if (cycles_to_spend <= 0) return;
                }
                return;
            },
            else => return, // no action for hblank or vblank
        }
    }
    fn randomStatic(self: *GPU) void { // random static
       for (0..self.lcd.screenBuf.len) |i| {
           const color: Color = GB.prng.random().enumValue(Color);
           LCD.writeToBuf(&self.lcd.screenBuf, color, i);
       }
    }
    fn tileDecoder(self: *GPU, high: u8, low: u8, pixel_index: u3) Color {
        const shift: u3 = 7 - pixel_index;
        const hi = @as(u1, @truncate(high >> shift));
        const lo = @as(u1, @truncate(low >> shift));
        const color_code: u2 = (@as(u2, hi) << 1) | lo;
        const bgp = self.getSpecialRegister(.bgp); // get the right color pallete (dmg)
        // print("color_code: {d}\n", .{color_code});
        const pallete_color: u2 = @truncate(bgp >> @as(u3, @intCast(color_code)) * 2); // selecting color
        return @as(Color, @enumFromInt(pallete_color));
    }
    fn switchMode(self: *@This()) void {
        const ly = self.getSpecialRegister(.ly);
        switch (self.mode) {
            .SCAN => {
                self.mode = .RENDER;
                self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.RENDER)];
            },
            .RENDER => {
                self.lcd.pushScanline(self.scanline, ly);
                self.mode = .HBLANK;
                self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.HBLANK)];
                self.scanline_progress = 0;
            },
            .HBLANK => {
                // Increment LY register
                self.setSpecialRegister(.ly, ly + 1);
                if (ly + 1 == 144) {
                    tracy.frameMark();
                    self.mode = .VBLANK;
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.VBLANK)]; // per scanline
                } else {
                    self.mode = .SCAN;
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.SCAN)];
                }
                // TODO INTERRUPT
            },
            .VBLANK => {
                const new_ly = ly + 1;
                if (new_ly > 153) { // 153 is the end of VBLANK
                    // fetching and writing background tiles to the lcd
                    const base: u16 = 0x8000; // start of tile data
                    var tile: u16 = 0;
                    while (tile < 384) : (tile += 1) {
                        const tileX = tile % 24;
                        const tileY = tile / 24;
                        const dstY = tileY * 8;
                        const dstX = tileX * 8;
                        const tile_addr: u16 = base + tile * 16;
                        for (0..8) |row| {
                            for (0..8) |col| {
                                const pixel_index: u3 = @intCast(col);
                                const low = self.readVram(tile_addr + row * 2);
                                const high = self.readVram(tile_addr + row * 2 + 1);
                                const color = self.tileDecoder(high, low, pixel_index);
                                // write to tilesheet buffer
                                const tilesheet_index = (dstY + row) * 192 + (dstX + col);
                                LCD.writeToBuf(&self.lcd.tilesheetBuf, color, tilesheet_index);
                            }
                        }
                    }
                    self.setSpecialRegister(.ly, 0); // reset LY to 0
                    self.mode = .SCAN;
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.SCAN)];
                    self.frames_cycled += 1;
                } else {
                    self.setSpecialRegister(.ly, new_ly);
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.VBLANK)];
                }
            },
        }
        var stat_reg = self.getSpecialRegister(.stat); // update STAT register vv
        stat_reg = (stat_reg & 0b1111_1100) | @intFromEnum(self.mode);
        self.setSpecialRegister(.stat, stat_reg);
    }
    // memory ops
    fn readVram(self: *@This(), address: usize) u8 {
        // print("address: 0x{X}", .{address});
        const fixed_address = address - VRAM_BEGIN;
        return self.vram[fixed_address];
    }
    fn writeVram(self: *@This(), address: usize, value: u8) void {
        const fixed_address = address - VRAM_BEGIN;
        self.vram[fixed_address] = value;
    }
    fn setSpecialRegister(self: *@This(), register: LCD.special_registers, value: u8) void {
        self.special_registers[@intFromEnum(register)] = value;
    }
    fn getSpecialRegister(self: *@This(), register: LCD.special_registers) u8 {
        return self.special_registers[@intFromEnum(register)];
    }
    fn testSpecialRegister(self: *@This(), register: LCD.special_registers, bit: u3) bool {
        return @as(u1, @truncate(self.special_registers[@intFromEnum(register)] >> bit)) == 1;
    }
    fn writeOAM(self: *@This(), address: usize, value: u8) !void {
        if (self.mode == .RENDER or self.mode == .SCAN) {
            print("cannot access oam now\n", .{});
            return;
        }
        if (address <= 0xFE00 or address >= 0xFE9F) return error.OutOfOAMBounds;
        self.oam[address] = value;
    }
    // mem dump
    fn vram_dump(self: *@This()) void {
        print("VRAM dump: \n", .{});
        for (self.vram, 0..VRAM_SIZE) |value, i| {
            const global_address = i + VRAM_BEGIN;
            if (global_address == 0x8000) print("\nentering vram\n", .{}) else if (global_address == 0x9800) print("\nentering tilemap one\n", .{}) else if (global_address == 0x9C00) print("\nentering tilemap two\n", .{});
            print("@0x{X}[", .{global_address});
            print("0x{x}]\t", .{value});
            if (i != 0 and i % 12 == 0) print("\n", .{});
        }
        print("\n", .{});
    }
    // context & types
    const Mode = enum { // modes specifying number of cycles per scanline
        HBLANK,
        VBLANK,
        SCAN,
        RENDER,
        const cycles: [4]u16 = .{ 204, 456, 80, 172 };
    };
    const Color = enum(u2) { black, dgray, lgray, white };
    const Tile = [8][8]Color;
    const VRAM_BEGIN = 0x8000;
    const VRAM_END = 0x9FFF;
    const VRAM_SIZE = VRAM_END - VRAM_BEGIN + 1;
    const tilemap_one = 0x9800;
    const tilemap_two = 0x9C00;
    const tilemap_size: u16 = tilemap_two - tilemap_one;
    const OAM_BEGIN = 0xFE00;
    const OAM_END = 0xFE9F;
    const OAM_SIZE = OAM_END - OAM_BEGIN + 1;
};

///Contains the fields necessary to create a display,
///- Screen, Height, Width, Rendering
const LCD = struct {
    screenBuf: [screenHeightPx * screenWidthPx]u32 = undefined,
    tilesheetBuf: [192 * 128]u32 = undefined,
    renderer: *g.SDL_Renderer = undefined,
    screen_texture: *g.SDL_Texture = undefined,
    bg_texture: *g.SDL_Texture = undefined,
    tilesheet_texture: *g.SDL_Texture = undefined,
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
    const gb_palette = [_]u32{
        0xFFFFFFFF, // white
        0xFFAAAAAA, // light gray
        0xFF555555, // dark gray
        0xFF000000, // transparent
    };
    // startup
    fn init(self: *@This()) !void {
        var color: GPU.Color = undefined;
        for (0..self.screenBuf.len) |i| {
            color = GB.prng.random().enumValue(GPU.Color);
            writeToBuf(&self.screenBuf, color, i);
        }
        for (0..self.tilesheetBuf.len) |i| {
            color = GB.prng.random().enumValue(GPU.Color);
            writeToBuf(&self.tilesheetBuf, color, i);
        }
        try self.startAndCreateRenderer(); // set window and renderer
    }
    fn startAndCreateRenderer(self: *@This()) !void {
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
        if (renderer == null) {
            print("Failed to create renderer: {s}\n", .{g.SDL_GetError()});
            return error.RendererFailure;
        }

        self.renderer = renderer.?;
        self.win = win.?;
        _ = g.SDL_SetWindowResizable(self.win, true);
        _ = g.SDL_SetWindowMinimumSize(self.win, initWinW, initWinH);
        if (!g.SDL_GetWindowSizeInPixels(self.win, &window_width, &window_height)) {
            print("err while getting window size: {s}\n", .{g.SDL_GetError()});
            return error.NoWinSize;
        } else {
            print("window size: {d}x{d}\n", .{ window_width, window_height });
            if (window_height == 0 or window_width == 0) {
                return error.DetectedZeroWidthWin;
            }
        }
        // creating textures
        self.screen_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_STREAMING, 160, 144);
        _ = g.SDL_SetTextureScaleMode(self.screen_texture, g.SDL_SCALEMODE_NEAREST);
        // TODO RESIZABLE
        self.bg_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, window_width, window_height);
        self.tilesheet_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, 192, 128);
        _ = g.SDL_SetTextureScaleMode(self.tilesheet_texture, g.SDL_SCALEMODE_NEAREST);
        self.createBG();
        self.bootScreen(); // wait for sdl to finish building the window to begin progression
    }
    fn bootScreen(self: *LCD) void {
        self.renderAll(); // Draw a dummy frame to force the window to initialize and show
        _ = g.SDL_PumpEvents();
        // Let the OS process events and show the window
        var window_ready = false;
        var event: g.SDL_Event = undefined;
        const timeout_ns = 500 * std.time.ns_per_ms;
        const start = Clock.Now();
        while (!window_ready and (Clock.Now() - start < timeout_ns)) {
            while (g.SDL_PollEvent(&event)) {
                if (event.type == g.SDL_EVENT_WINDOW_SHOWN) {
                    window_ready = true;
                }
            }
            std.time.sleep(1 * std.time.ns_per_ms);
        }
        std.time.sleep(17 * std.time.ns_per_ms);
    }
    // peripheral fns
    fn writeToBuf(buf: []u32, color: GPU.Color, index: usize) void {
        buf[index] = switch (color) {
            .white => 0xFFCCFFCC,
            .lgray => 0xFF99CC99,
            .dgray => 0xFF669966,
            .black => 0xFF336633,
        };
    }
    fn pushScanline(self: *@This(), new_scanline: [screenWidthPx]GPU.Color, ly: u8) void {
        for (0..screenWidthPx) |x| {
            writeToBuf(&self.screenBuf, new_scanline[x], @as(u32, @intCast(ly)) * screenWidthPx + x);
        }
    }
    // drawing
    fn setRect(self: *@This(), rect: *g.SDL_FRect, x: anytype, y: anytype, w: anytype, h: anytype) void {
        _ = self;
        rect.x = if (@TypeOf(x) == f32) x else @as(f32, @floatFromInt(x));
        rect.y = if (@TypeOf(y) == f32) y else @as(f32, @floatFromInt(y));
        rect.w = if (@TypeOf(w) == f32) w else @as(f32, @floatFromInt(w));
        rect.h = if (@TypeOf(h) == f32) h else @as(f32, @floatFromInt(h));
    }
    fn createBG(self: *LCD) void {
        _ = g.SDL_SetRenderTarget(self.renderer, self.bg_texture);
        defer _ = g.SDL_SetRenderTarget(self.renderer, null);
        _ = g.SDL_SetRenderDrawColor(self.renderer, 255, 192, 220, 255);
        _ = g.SDL_RenderClear(self.renderer);
    }
    fn renderAll(self: *@This()) void {
        _ = g.SDL_UpdateTexture(self.screen_texture, null, &self.screenBuf, 160 * @sizeOf(u32));
        _ = g.SDL_UpdateTexture(self.tilesheet_texture, null, &self.tilesheetBuf, 192 * @sizeOf(u32));
        // _ = g.SDL_RenderClear(self.renderer);
        const bg_rect = g.SDL_FRect{
            .x = screenWidthPx * pxSize + border * 2,
            .y = 0,
            .w = @as(f32, @floatFromInt(window_width)) - (screenWidthPx * pxSize + border * 2),
            .h = @as(f32, @floatFromInt(window_height)),
        };
        const screen_rect = g.SDL_FRect{
            .x = 10.0,
            .y = 0,
            .w = screenWidthPx * pxSize,
            .h = screenHeightPx * pxSize,
        };
        const bg_px_sz: f32 = (@as(f32, @floatFromInt(window_height)) - (screenHeightPx * pxSize) + 10) / 128;
        const bg_map_rect = g.SDL_FRect{
            .x = @as(f32, @floatFromInt(0)),
            .y = screenHeightPx * pxSize + 10,
            .w = 192 * bg_px_sz,
            .h = 128 * bg_px_sz,
        };
        _ = g.SDL_RenderTexture(self.renderer, self.bg_texture, null, &bg_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.screen_texture, null, &screen_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.tilesheet_texture, null, &bg_map_rect);
        _ = g.SDL_RenderPresent(self.renderer);
    }

    // mem dump
    pub fn screen_dump(self: *@This()) void {
        print("Actual memspace dump: \n", .{});
        for (self.screenBuf) |pixel| {
            print("{any}", .{@as(GPU.Color, @enumFromInt(pixel))});
        }
        print("\n", .{});
    }
    // end
    fn endSDL(self: *@This()) void {
        g.SDL_Quit();
        g.SDL_DestroyWindow(self.win);
        g.SDL_DestroyRenderer(self.renderer);
        g.SDL_DestroyTexture(self.bg_texture);
        g.SDL_DestroyTexture(self.screen_texture);
        g.SDL_DestroyTexture(self.tilesheet_texture);
    }
    const initWinW: u16 = 1000;
    const initWinH: u16 = 700;
    const border = 10;
    const screenWidthPx = 160;
    const screenHeightPx = 144;
    var window_height: c_int = @intCast(initWinH);
    var window_width: c_int = @intCast(initWinW);
    var center: u16 = initWinW / 2;
    var pxSize: f32 = @as(f32, @floatFromInt(initWinW)) / screenWidthPx / 2;
};

const Clock = struct {
    start: i128 = undefined,
    ns_elapsed: u64 = 0,
    last_frame_time: i128 = undefined, // TODO(logan) use 0 tracked time
    last_fps: f64 = 0,
    current_fps: f64 = 0,
    slept: u64 = 0,
    debt: i128 = 0,
    fn Start(self: *Clock) !void {
        self.start = Now();
    }
    const Now = std.time.nanoTimestamp;

    fn targetCycles(self: *Clock) u64 {
        const total_elapsed_ns: f64 = @floatFromInt(Now() - self.start);
        return @intFromFloat(total_elapsed_ns * ticks_per_ns);
    }
    fn tick(self: *Clock) void {
        const now = Now();
        const ns_passed = now - self.last_frame_time;
        if (ns_passed < ns_per_frame) {
            const wait_time_ns: u64 = @intCast(ns_per_frame - ns_passed);
            if (wait_time_ns > 100_000) {
                const start = Now();
                const sleeptime = std.math.cast(u64, wait_time_ns + self.debt - 100_000);
                std.time.sleep(sleeptime orelse 0); // sleep when necessary
                const waited = Now() - start;
                self.debt += wait_time_ns - waited;
                // print("leftover {d}\n", .{self.debt});
            }
        }
    }
    fn update(self: *Clock) void {
        const now = Now();
        const frame_time = now - self.last_frame_time;
        self.ns_elapsed += @intCast(frame_time);
        const fps_estimate = std.time.ns_per_s / @as(f64, @floatFromInt(frame_time));
        const smoothing = 0.7;
        self.current_fps = smoothing * self.last_fps + (1.0 - smoothing) * fps_estimate;
        if (self.ns_elapsed >= std.time.ns_per_s) {
            // print("{d:.2} fps\n", .{self.current_fps});
            self.ns_elapsed = 0;
        }
        self.last_fps = self.current_fps;
    }
    const ticks_per_s = 4.194304 * @as(f64, std.math.pow(u64, 10, 6));
    const ticks_per_ns = ticks_per_s / std.time.ns_per_s;
    const ns_per_tick = 1 / ticks_per_ns;
    const ns_per_frame: u64 = @intFromFloat(@round(1_000_000_000.0 / 59.744));
    const cycles_per_frame = 70224;
};
/// Gameboy Machine, defer endGB
pub const GB = struct {
    cpu: CPU = CPU{},
    gpu: GPU = GPU{},
    apu: APU = APU{},
    memory: [0xFFFF]u8 = undefined,
    running: bool = undefined,
    crash: bool = false,
    cycles_spent: usize = 0,
    clock: Clock = Clock{},
    last_frame: std.time.Instant = undefined,
    /// nintendo logo
    const LOGO: [48]u8 = .{ 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E };
    // startup
    var prng: std.Random.Xoshiro256 = undefined;
    pub fn init(self: *@This()) !void {
        @memset(&self.memory, 0);
        @memcpy(self.memory[0x104 .. 0x133 + 1], &LOGO);
        self.init_header_checksum(); // from loaded cartidge
        try initRandom(); // init random before gpu init
        try self.gpu.init(self);
        try self.cpu.init();
        _ = InstructionSet.exe_from_byte(self, 0, false); // dummy op to init cache
        self.cpu.pc -= 1;
        self.running = true;
    }

    fn init_header_checksum(self: *GB) void {
        const TITLE = "TEST GAME";
        @memcpy(self.memory[0x134 .. 0x134 + TITLE.len], TITLE);
        // Fill remaining header fields with dummy
        self.memory[0x144] = 0x00; // manufacturer code
        self.memory[0x145] = 0x00;
        self.memory[0x146] = 0x00; // CGB flag
        self.memory[0x147] = 0x00; // ROM ONLY
        self.memory[0x148] = 0x00; // 32KB ROM
        self.memory[0x149] = 0x00; // No RAM
        self.memory[0x14A] = 0x01; // Non-Japan
        self.memory[0x14B] = 0x33; // Old licensee code
        self.memory[0x14C] = 0x00; // Mask ROM version
        // Then compute checksum
        var checksum: u8 = 0;
        for (0x134..0x14D) |i| {
            checksum = @subWithOverflow(checksum, self.memory[i] + 1)[0];
        }
        self.memory[0x14D] = checksum;
    }
    /// Loads gameboy bootrom
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
    // gb execution
    pub fn go(self: *@This()) !void {
        print("GO!\n", .{});
        try self.clock.Start();
        while (self.running) {
            self.clock.last_frame_time = Clock.Now();
            while (self.gpu.frame_cycles_spent < Clock.cycles_per_frame and !self.crash) {
                self.do() catch {
                 self.crash = true;
                };
            }
            if (self.crash) {
                self.gpu.randomStatic();
            }
            try self.getEvents(); // poll events once per frame
            self.clock.tick();
            self.clock.update(); // calculates average fps
            self.gpu.lcd.renderAll(); // render at the last scanline
            self.gpu.frame_cycles_spent = 0;
        }
    }
    fn do(self: *@This()) !void {
        const cycles_spent = try self.cpu.execute(self);
        self.cycles_spent += cycles_spent;
        self.gpu.tick(cycles_spent * 4);
    }
    // helper
    fn initRandom() !void {
        var seed: u64 = undefined;
        try std.posix.getrandom(std.mem.asBytes(&seed));
        GB.prng = std.Random.DefaultPrng.init(seed);
    }
    // universal callers
    pub fn readByte(self: *@This(), address: u16) u8 {
        @setRuntimeSafety(false);
        if (address >= 0xFF00) return self.memory[address];
        // if (address >= LCD.special_registers.start and address <= LCD.special_registers.end) {
        //     return self.gpu.getSpecialRegister(@as(LCD.special_registers, @enumFromInt(address - LCD.special_registers.start)));
        // }
        if (address >= CPU.WRAM_START and address <= CPU.WRAM_END) {
            return self.memory[address];
        }
        if (address >= GPU.VRAM_BEGIN and address <= GPU.VRAM_END) {
            return self.gpu.readVram(address);
        }
        if (address >= GPU.OAM_BEGIN and address <= GPU.OAM_END) {
            return self.gpu.oam[address - GPU.OAM_BEGIN];
        }
        return self.memory[address];
    }
    pub fn writeByte(self: *@This(), address: u16, value: u8) void {
        @setRuntimeSafety(false);
        if (address < 0x8000) return; // no writes to ROM
        if (address >= LCD.special_registers.start and address <= LCD.special_registers.end) {
            const register = @as(LCD.special_registers, @enumFromInt(address - LCD.special_registers.start));
            self.gpu.setSpecialRegister(register, value);
            if (register == LCD.special_registers.dma) {
                const prefix = address / 0x100;
                const ram_address: u16 = @as(u16, @intCast(prefix)) << 8;
                @memcpy(self.memory[GPU.OAM_BEGIN..GPU.OAM_END], self.memory[ram_address .. ram_address + GPU.OAM_SIZE]);
            }
            return;
        }
        if (address >= GPU.VRAM_BEGIN and address <= GPU.VRAM_END) { // banks 0 & 1
            self.gpu.writeVram(address, value);
            return;
        }
        self.memory[address] = value;
    }
    pub fn getEvents(self: *@This()) !void {
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
                },
                else => {},
            }
        }
    }
    // memspace dumps
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
        for (self.memory[LCD.special_registers.start .. LCD.special_registers.end + 1], LCD.special_registers.start..LCD.special_registers.end + 1) |value, i| {
            print("register@0x{x}: 0x{x}\n", .{ i, value });
        }
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
    // print("ns per frame: {d}", .{Clock.ns_per_frame});
    var gb = GB{};
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
    gb.go() catch |err| {
        // gb.gpu.vram_dump();
        print("Error while running GameBoy: {any}\n", .{err});
    };
}
