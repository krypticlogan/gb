const InstrFn = fn (*CPU, InstrArgs) u8;
pub const InstrArgs = union(enum) { none: void, target: regID, bit: u3, bit_target: struct { bit: u3, target: regID }, flagConditions: Condition, targets: struct { to: regID, from: regID }, hl_mod: i2, where: u16 };
pub const Condition = union(enum) { none, z, c, nz, nc };
pub fn INVALID(cpu: *CPU, _: InstrArgs) u8 {
    // This instruction should never be called
    _ = cpu;
    @panic("Attempt to execute invalid instruction");
}
pub fn NOP(cpu: *CPU, _: InstrArgs) u8 {
    cpu.pushToExecutionChain("NOP", .{});
    cpu.pc += 1;
    return 1;
}
pub fn STOP(cpu: *CPU, _: InstrArgs) u8 {
    cpu.pushToExecutionChain("STOP", .{});
    cpu.halted = true;
    cpu.pc += 2;
    return 0;
}
pub fn HALT(cpu: *CPU, _: InstrArgs) u8 {
    const debug = "HALT @pc[{X}]";
    // print(debug ++ "\n", .{cpu.pc});
    cpu.pushToExecutionChain(debug, .{cpu.pc});
    const interrupt_pending = cpu.bus.handler.read(.enable) & cpu.bus.handler.read(.flag) != 0;
    // print("HALT on pc[{X}]: {s}\nInterrupt state\n -------------\n\t", .{cpu.pc, if (!cpu.halted) "first entry\n" else "returned" });
    // cpu.bus.handler.dump();
    switch (cpu.halted) {
        false => { // first entry
            cpu.halted = true;
            switch (cpu.bus.handler.ime) {
                true => {
                    if (interrupt_pending) {
                        cpu.bus.handler.handle(cpu);
                        cpu.halted = false;
                    }
                },
                false => {
                    if (interrupt_pending) {
                        cpu.halted = false;
                        cpu.halt_bug_state = 1;
                        print("Entered halt bug state, next PC / INSTR = pc[{X}] / 0x{X}\n", .{cpu.pc + 1, cpu.bus.readByte(cpu.pc + 1)});
                        cpu.pc += 1;
                    }
                },
            }
        },
        true => { // still halted, we have returned
            switch (cpu.bus.handler.ime) {
                true => {
                    if (interrupt_pending) {
                        cpu.bus.handler.handle(cpu);
                        cpu.halted = false;
                    }
                },
                false => {
                    if (interrupt_pending) {
                        print("no ime, no halt bug\n", .{});
                        cpu.halted = false;
                        cpu.pc += 1;
                    }
                },
            }
        },
    }
    return 1;
}
// LOAD
// 8 bit
//
pub fn LD8(cpu: *CPU, args: InstrArgs) u8 { // LD r8, n8
    const n: u8 = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("LD r8, n8 | {any} <-- 0x{X}", .{ @as(regID, args.target), n });
    cpu.set_byte(args.target, n);
    // if (args.target == .a and n == 0x1b) cpu.break_exe();
    cpu.pc += 2;
    return 2;
}
pub fn LDr8(cpu: *CPU, args: InstrArgs) u8 { // LD r8, r8
    cpu.pushToExecutionChain("LDr8 | {any} --> {any}", .{ args.targets.from, args.targets.to });
    cpu.set_byte(args.targets.to, cpu.get_byte(args.targets.from));
    cpu.pc += 1;
    return 1;
}
pub fn LDr8HL(cpu: *CPU, args: InstrArgs) u8 { // LD r8, [HL]
    // const hl = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(cpu.get_word(regID.h));
    if (args.target == .l and value == 0x1B) {
        print("HERE\n\n\t\tLD r8, [HL] | {any} <-- 0x{X} @(0x{X}) pc[{X}]\n", .{ args.target, value, cpu.get_word(regID.h), cpu.pc });
        // cpu.break_exe();
    }

    cpu.pushToExecutionChain("LD r8, [HL] | {any} <-- 0x{X}", .{ args.target, value });
    cpu.set_byte(args.target, value);
    cpu.pc += 1;
    return 2;
}
pub fn LDHLIA(cpu: *CPU, _: InstrArgs) u8 { // LD [HLI],A
    const hl = cpu.get_word(regID.h);
    const value = cpu.get_byte(regID.a);
    cpu.pushToExecutionChain("LD [HL+], A |  mem@0x{X} <-- 0x{X}", .{ hl, value });
    cpu.bus.writeByte(hl, value);
    cpu.set_word(regID.h, @addWithOverflow(hl, 1)[0]);
    cpu.pc += 1;
    return 2;
}
pub fn LDHLDA(cpu: *CPU, _: InstrArgs) u8 { // LD [HLD], A
    const hl = cpu.get_word(regID.h);
    const value = cpu.get_byte(regID.a);
    cpu.pushToExecutionChain("LD [HL-], A |  mem@0x{X} <-- 0x{X}", .{ hl, value });
    cpu.bus.writeByte(hl, value);
    cpu.set_word(regID.h, @subWithOverflow(hl, 1)[0]);
    cpu.pc += 1;
    return 2;
}
pub fn LDHCA(cpu: *CPU, _: InstrArgs) u8 {
    const c = cpu.get_byte(regID.c);
    const a = cpu.get_byte(regID.a);
    const mem_place = 0xFF00 + @as(u16, c);
    cpu.pushToExecutionChain("LDH [C], A | mem@0x{X} <-- 0x{X}", .{ mem_place, a });
    cpu.bus.writeByte(mem_place, a);
    cpu.pc += 1;
    return 2;
}
pub fn LDHAC(cpu: *CPU, _: InstrArgs) u8 { // Load value in register A from the byte at address $FF00+c
    const c = cpu.get_byte(regID.c);
    const byte = cpu.bus.readByte(0xFF00 + @as(u16, c));
    cpu.pushToExecutionChain("LDH A, [C] | A <-- 0x{X}", .{byte});
    cpu.set_byte(regID.a, byte);
    cpu.pc += 1;
    return 2;
}
// 16 bit
//
pub fn LD16(cpu: *CPU, args: InstrArgs) u8 { // LD r16, n16
    const n: u16 = @as(u16, cpu.bus.readByte(cpu.pc + 2)) << 8 | cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("LDr16, n16 | {any} <-- Ox{X}", .{ args.target, n });
    cpu.set_word(args.target, n);
    cpu.pc += 3;
    return 3;
}
pub fn LDAHL(cpu: *CPU, args: InstrArgs) u8 { // LD A, HL
    const mem_place = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(mem_place);
    cpu.pushToExecutionChain("LD A, [HL], mem@hl:0x{X} --> to A", .{mem_place});
    cpu.set_byte(.a, value);
    cpu.set_word(.h, @intCast(@addWithOverflow(@as(i17, args.hl_mod), @as(i17, mem_place))[0]));
    if (!(cpu.get_word(.h) == mem_place + 1 or cpu.get_word(.h) == mem_place - 1)) {
        @panic("we didnt change h");
    }
    cpu.pc += 1;
    return 2;
}
pub fn LDSP16(cpu: *CPU, _: InstrArgs) u8 { // LD SP, n16
    const n: u16 = @as(u16, cpu.bus.readByte(cpu.pc + 2)) << 8 | cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("LDSP16 | n (0x{X})", .{n});
    cpu.sp = n;
    // print("after op: sp: {d}\n", .{cpu.sp});
    cpu.pc += 3;
    return 3;
}
pub fn LDSPHL(cpu: *CPU, _: InstrArgs) u8 { // LD SP,HL
    cpu.pushToExecutionChain("LDSPHL", .{});
    cpu.sp = cpu.get_word(.h);
    // print("after op: sp: {d}\n", .{cpu.sp});
    cpu.pc += 1;
    return 2;
}
pub fn LDHLSPn8(cpu: *CPU, _: InstrArgs) u8 { // LD HL,SP+e8
    const n: i8 = @bitCast(cpu.bus.readByte(cpu.pc + 1));
    cpu.pushToExecutionChain("LDHLSPn8 | n (0x{X})", .{n});
    // cpu.pushToExecutionChain(", .{ cpu.pc + 1, cpu.bus.readByte(cpu.pc + 1) });
    cpu.set_word(.h, mixedSignArithmetic(cpu.sp, n, i17)[0]);
    // print("after op: sp: {d}\n", .{cpu.sp});
    const c = @addWithOverflow(@as(u8, @truncate(cpu.sp)), @as(u8, @bitCast(n)))[1] == 1;
    const h = detectHalfCarry(@as(u8, @truncate(cpu.sp)), @as(u8, @bitCast(n)), .add);
    cpu.f.write(false, c, h, false);
    //
    cpu.pc += 2;
    return 3;
}
pub fn LDn16SP(cpu: *CPU, _: InstrArgs) u8 { // Store SP & $FF at address n16 and SP >> 8 at address n16 + 1.
    const high: u8 = @truncate(cpu.sp >> 8);
    const low: u8 = @truncate(cpu.sp);
    const mem_place: u16 = @as(u16, cpu.bus.readByte(cpu.pc + 2)) << 8 | cpu.bus.readByte(cpu.pc + 1);
    cpu.bus.writeByte(mem_place, low);
    cpu.bus.writeByte(mem_place + 1, high);
    cpu.pc += 3;
    cpu.pushToExecutionChain("LDn16SP | SP:0x{X} --> mem@0x{X}{X}", .{ cpu.sp, high, low });
    return 5;
}
pub fn LDAn16(cpu: *CPU, _: InstrArgs) u8 {
    const memory_place = @as(u16, cpu.bus.readByte(cpu.pc + 2)) << 8 | cpu.bus.readByte(cpu.pc + 1);
    const n = cpu.bus.readByte(memory_place);
    cpu.pushToExecutionChain("LDAn16 | n: Ox{X} --> A", .{n});
    cpu.set_byte(regID.a, n);
    cpu.pc += 3;
    return 4;
}
pub fn LDHAn16(cpu: *CPU, _: InstrArgs) u8 { // same as above, provided the address is between $FF00 and $FFFF.
    // const zone = tracy.beginZone(@src(), .{ .name = "LDHAn16" });
    // defer zone.end();
    const memory_place = 0xFF00 + @as(u16, cpu.bus.readByte(cpu.pc + 1));
    const n = cpu.bus.readByte(memory_place);
    cpu.pushToExecutionChain("LDHAn16 | n: 0x{X} --> A", .{n});
    cpu.set_byte(regID.a, n);
    cpu.pc += 2;
    return 3;
}
pub fn LDn16A(cpu: *CPU, _: InstrArgs) u8 { // Store value in register A into the byte at address n16.
    // const zone = tracy.beginZone(@src(), .{ .name = "LDn16A" });
    // defer zone.end();
    const memory_place = (@as(u16, cpu.bus.readByte(cpu.pc + 2)) << 8) | cpu.bus.readByte(cpu.pc + 1);
    const n = cpu.get_byte(regID.a);
    cpu.bus.writeByte(memory_place, n);
    cpu.pushToExecutionChain("LDn16A | n: Ox{X} --> memplace@{X}", .{ n, memory_place });
    cpu.pc += 3;
    return 4;
}
pub fn LDHn16A(cpu: *CPU, _: InstrArgs) u8 { // same as above, provided the address is between $FF00 and $FFFF.
    // const zone = tracy.beginZone(@src(), .{ .name = "LDHn16A" });
    // defer zone.end();
    const memory_place = 0xFF00 + @as(u16, cpu.bus.readByte(cpu.pc + 1));
    const n = cpu.get_byte(regID.a);
    cpu.bus.writeByte(memory_place, n);
    cpu.pushToExecutionChain("LDHn16A | n: 0x{X} --> memplace@0x{X}", .{ n, memory_place });
    cpu.pc += 2;
    return 3;
}
pub fn LDAr16(cpu: *CPU, args: InstrArgs) u8 { // Load value in register A from the byte pointed to by register r16.
    // const zone = tracy.beginZone(@src(), .{ .name = "LDAr16" });
    // defer zone.end();
    const memory_place = cpu.get_word(args.target);
    const n = cpu.bus.readByte(memory_place);
    cpu.pushToExecutionChain("LDAr16 | n: 0x{X} --> A", .{n});
    cpu.set_byte(regID.a, n);
    cpu.pc += 1;
    return 2;
}
pub fn LDr16A(cpu: *CPU, args: InstrArgs) u8 { //  Store value in register A into the byte pointed to by register r16.
    // const zone = tracy.beginZone(@src(), .{ .name = "LDr16A" });
    // defer zone.end();
    const memory_place = cpu.get_word(args.target);
    const n = cpu.get_byte(regID.a);
    cpu.bus.writeByte(memory_place, n);
    cpu.pushToExecutionChain("LDr16A | n: 0x{X} --> memplace@0x{X}", .{ n, memory_place });
    cpu.pc += 1;
    return 2;
}
pub fn LDHL8(cpu: *CPU, _: InstrArgs) u8 { // LD[HL], n8
    const hl = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("LDHL8 | hl:0x{X}, value:0x{x}, mem@hl: 0x{x}", .{ hl, value, cpu.bus.readByte(hl) });
    cpu.bus.writeByte(hl, value);
    cpu.pc += 2;
    return 3;
}
pub fn LDHLr8(cpu: *CPU, args: InstrArgs) u8 { // LD[HL],r8
    const hl = cpu.get_word(regID.h);
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("LDHLR | mem@0x{X} 0:x{X} --> 0x{X}", .{ hl, cpu.bus.readByte(hl), value });
    cpu.bus.writeByte(hl, value);
    cpu.pc += 1;
    return 2;
}
// ALU & ARITHMETIC
// 8 bit
//
pub fn INCr8(cpu: *CPU, args: InstrArgs) u8 {
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("INCr8 | target: {any}", .{args.target});
    const res = @addWithOverflow(value, 1);
    cpu.set_byte(args.target, res[0]);
    const h = detectHalfCarry(value, 1, .add); // half carry conditions
    const z = cpu.get_byte(args.target) == 0;
    const s = false;
    const c = cpu.f.cFlag();
    cpu.f.write(z, c, h, s);
    cpu.pc += 1;
    return 1;
}
pub fn DECr8(cpu: *CPU, args: InstrArgs) u8 {
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("DECr8 | target: {any}", .{args.target});
    const res = @subWithOverflow(value, 1)[0];
    cpu.set_byte(args.target, res);
    const h = detectHalfCarry(value, 1, .sub);
    const z = res == 0;
    const s = true;
    const c = cpu.f.cFlag();
    cpu.f.write(z, c, h, s);
    cpu.pc += 1;
    return 1;
}
pub fn ORr8(cpu: *CPU, args: InstrArgs) u8 {
    cpu.pushToExecutionChain("ORr8 | target {any}", .{args.target});
    const a = cpu.get_byte(.a);
    const res = a | cpu.get_byte(args.target);
    cpu.set_byte(.a, res);
    cpu.f.write(res == 0, false, false, false);
    cpu.pc += 1;
    return 1;
}
pub fn ORn8(cpu: *CPU, _: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
    // defer zone.end();
    const n = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("ORn8 | n: {any}", .{n});
    const a = cpu.get_byte(.a);
    const res = a | n;
    cpu.set_byte(.a, res);
    cpu.f.write(res == 0, false, false, false);
    cpu.pc += 2;
    return 2;
}
pub fn ORHL(cpu: *CPU, _: InstrArgs) u8 {
    cpu.pushToExecutionChain("ORHL", .{});
    const a = cpu.get_byte(.a);
    const value = cpu.bus.readByte(cpu.get_word(.h));
    const res = a | value;
    cpu.set_byte(.a, res);
    cpu.f.write(res == 0, false, false, false);
    cpu.pc += 1;
    return 2;
}
pub fn XORr8(cpu: *CPU, args: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
    // defer zone.end();
    cpu.pushToExecutionChain("XORr8 | target {any}", .{args.target});
    const a = cpu.get_byte(.a);
    const res = a ^ cpu.get_byte(args.target);
    cpu.set_byte(.a, res);
    cpu.f.write(res == 0, false, false, false);
    cpu.pc += 1;
    return 1;
}
pub fn XORn8(cpu: *CPU, _: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
    // defer zone.end();
    const value = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("XORn8", .{});
    const a = cpu.get_byte(.a);
    const res = a ^ value;
    cpu.set_byte(.a, res);
    cpu.f.write(res == 0, false, false, false);
    cpu.pc += 2;
    return 2;
}
pub fn XORHL(cpu: *CPU, _: InstrArgs) u8 {
    cpu.pushToExecutionChain("XORHL", .{});
    const a = cpu.get_byte(.a);
    const value = cpu.bus.readByte(cpu.get_word(.h));
    const res = a ^ value;
    cpu.set_byte(.a, res);
    cpu.f.write(res == 0, false, false, false);
    cpu.pc += 1;
    return 2;
}
pub fn ANDn8(cpu: *CPU, _: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
    // defer zone.end();
    const n = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("ANDn8 | n: {any}", .{n});
    const a = cpu.get_byte(.a);
    const res = a & n;
    cpu.set_byte(.a, res);
    cpu.f.write(res == 0, false, true, false);
    cpu.pc += 2;
    return 2;
}
pub fn ANDHL(cpu: *CPU, _: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
    // defer zone.end();
    const reg = cpu.bus.readByte(cpu.get_word(.h));
    cpu.pushToExecutionChain("ANDr8 | A & HL", .{});
    const a = cpu.get_byte(.a);
    const res = a & reg;
    cpu.set_byte(.a, res);
    cpu.f.write(res == 0, false, true, false);
    cpu.pc += 1;
    return 2;
}
pub fn ANDr8(cpu: *CPU, args: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
    // defer zone.end();
    const reg = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("ANDr8 | A & {any}", .{args.target});
    const a = cpu.get_byte(.a);
    const res = a & reg;
    cpu.set_byte(.a, res);
    cpu.f.write(res == 0, false, true, false);
    cpu.pc += 1;
    return 1;
}
pub fn ADDAr8(cpu: *CPU, args: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "ADDAr8" });
    // defer zone.end();
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("ADDAr8 | target: {any}, value: {d}", .{ args.target, value });
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @addWithOverflow(a, value);
    const s = false;
    const c = res[1] == 1;
    const h = detectHalfCarry(a, value, .add);
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 1;
}
pub fn ADDAn8(cpu: *CPU, _: InstrArgs) u8 { //
    // const zone = tracy.beginZone(@src(), .{ .name = "ADDAr8" });
    // defer zone.end();
    const value = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("ADDAn8 | A + 0x{X}", .{value});
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @addWithOverflow(a, value);
    const s = false;
    const c = res[1] == 1;
    const h = detectHalfCarry(a, value, .add);
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 2;
    return 2;
}
pub fn ADDSPn8(cpu: *CPU, _: InstrArgs) u8 {
    const value: i8 = @bitCast(cpu.bus.readByte(cpu.pc + 1));
    const debug = "ADDSPn8 | SP 0x{X} + 0x{X}";
    cpu.pushToExecutionChain(debug, .{ cpu.sp, value });
    // print(debug ++ "\n", .{cpu.sp, value});
    const res = mixedSignArithmetic(cpu.sp, value, i17);
    const s = false;
    const c = @addWithOverflow(@as(u8, @truncate(cpu.sp)), @as(u8, @bitCast(value)))[1] == 1;
    const h = detectHalfCarry(@as(u8, @truncate(cpu.sp)), @as(u8, @bitCast(value)), .add);
    const z = false;
    cpu.f.write(z, c, h, s);
    cpu.sp = res[0];
    cpu.pc += 2;
    return 4;
}
pub fn ADCAr8(cpu: *CPU, args: InstrArgs) u8 { // add a to a register, plus the carry
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("ADCAr8 | target: {any}, value: {d}", .{ args.target, value });
    const a = cpu.get_byte(regID.a);
    const half_add = @addWithOverflow(a, value);
    const carry = @intFromBool(cpu.f.cFlag());
    const res: struct { u8, u1 } = @addWithOverflow(half_add[0], carry);
    const s = false;
    const c = res[1] == 1 or half_add[1] == 1;
    const h = detectHalfCarry(a, value, .add) or detectHalfCarry(half_add[0], @as(u8, @intCast(carry)), .add);
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 1;
}
pub fn ADCAn8(cpu: *CPU, _: InstrArgs) u8 { // add a to n8, plus the carry
    const value = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("ADCAn8 | value: {d}", .{value});
    const a = cpu.get_byte(regID.a);
    const carry = @intFromBool(cpu.f.cFlag());
    const half_add = @addWithOverflow(a, value);
    const res: struct { u8, u1 } = @addWithOverflow(carry, half_add[0]);
    const s = false;
    const c = res[1] == 1 or half_add[1] == 1;
    const h = detectHalfCarry(a, value, .add) or detectHalfCarry(half_add[0], @as(u8, @intCast(carry)), .add);
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 2;
    return 2;
}
pub fn ADCAHL(cpu: *CPU, _: InstrArgs) u8 {
    const value = cpu.bus.readByte(cpu.get_word(.h));
    cpu.pushToExecutionChain("ADCAHL | mem@hl: {d}", .{value});
    const a = cpu.get_byte(regID.a);
    const carry = @intFromBool(cpu.f.cFlag());
    const half_add = @addWithOverflow(a, value);
    const res: struct { u8, u1 } = @addWithOverflow(carry, half_add[0]);
    const s = false;
    const c = res[1] == 1 or half_add[1] == 1;
    const h = detectHalfCarry(a, value, .add) or detectHalfCarry(half_add[0], @as(u8, @intCast(carry)), .add);
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 2;
}
pub fn SUBAr8(cpu: *CPU, args: InstrArgs) u8 {
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("SUBA | target: {any}, value: {d}", .{ args.target, value });
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @subWithOverflow(a, value);
    const c = value > a;
    const s = true;
    const h = detectHalfCarry(a, value, .sub);
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 1;
}
pub fn SUBAn8(cpu: *CPU, _: InstrArgs) u8 {
    const value = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("SUBAn8 | value: {d}", .{value});
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @subWithOverflow(a, value);
    const c = value > a;
    const s = true;
    const h = detectHalfCarry(a, value, .sub);
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 2;
    return 2;
}
pub fn SUBAHL(cpu: *CPU, _: InstrArgs) u8 {
    const mem_place = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(mem_place);
    cpu.pushToExecutionChain("SUBAHL | A - mem@0x{X}: value: {d}", .{ mem_place, value });
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @subWithOverflow(a, value);
    const c = value > a;
    const s = true;
    const h = detectHalfCarry(a, value, .sub);
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 2;
}
pub fn SBCAr8(cpu: *CPU, args: InstrArgs) u8 {
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("SBCAr8 | target: {any}, value: {d}", .{ args.target, value });
    const a = cpu.get_byte(regID.a);
    const carry = @intFromBool(cpu.f.cFlag());
    const half_sub = @subWithOverflow(a, value);
    const res: struct { u8, u1 } = @subWithOverflow(half_sub[0], carry);
    const s = true;
    const c = res[1] == 1 or half_sub[1] == 1;
    const h = detectHalfCarry(a, value, .sub) or detectHalfCarry(half_sub[0], @as(u8, @intCast(carry)), .sub);
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 1;
}
pub fn SBCAn8(cpu: *CPU, _: InstrArgs) u8 {
    const value = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("SBCAn8 | value: {d}", .{value});
    const a = cpu.get_byte(regID.a);
    const carry = @intFromBool(cpu.f.cFlag());
    const half_sub = @subWithOverflow(a, value);
    const res: struct { u8, u1 } = @subWithOverflow(half_sub[0], carry);
    const s = true;
    const c = res[1] == 1 or half_sub[1] == 1;
    const h = detectHalfCarry(a, value, .sub) or detectHalfCarry(half_sub[0], @as(u8, @intCast(carry)), .sub);
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 2;
    return 2;
}
pub fn SBCAHL(cpu: *CPU, _: InstrArgs) u8 {
    const value = cpu.bus.readByte(cpu.get_word(.h));
    cpu.pushToExecutionChain("SBCAHL | mem@hl: {d}", .{value});
    const a = cpu.get_byte(regID.a);
    const carry = @intFromBool(cpu.f.cFlag());
    const half_sub = @subWithOverflow(a, value);
    const res: struct { u8, u1 } = @subWithOverflow(half_sub[0], carry);
    const s = true;
    const c = res[1] == 1 or half_sub[1] == 1;
    const h = detectHalfCarry(a, value, .sub) or detectHalfCarry(half_sub[0], @as(u8, @intCast(carry)), .sub);
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 2;
}
pub fn ADDAHL(cpu: *CPU, _: InstrArgs) u8 {
    const mem_place = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(mem_place);
    cpu.pushToExecutionChain("ADDAHL | A + mem@0x{X}: value: {d}", .{ mem_place, value });
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @addWithOverflow(a, value);
    const s = false;
    const c = res[1] == 1;
    const h = detectHalfCarry(a, value, .add);
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 2;
}
pub fn DAA(cpu: *CPU, _: InstrArgs) u8 {
    var a = cpu.get_byte(.a);
    var offset: u8 = 0;
    var c = cpu.f.cFlag();
    if (!cpu.f.sFlag()) {
        if ((a & 0xF > 0x9) or cpu.f.hFlag()) {
            offset |= 0x6;
        }
        if ((a > 0x99) or cpu.f.cFlag()) {
            offset |= 0x60;
            c = true;
        }
    } else {
        if (cpu.f.cFlag()) {
            offset |= 0x60;
        }
        if (cpu.f.hFlag()) {
            offset |= 0x6;
        }
    }
    const s = cpu.f.sFlag();
    // const c = !s and (a > 0x99 or cpu.f.cFlag()); // must check this before modification
    if (s) {
        a = @subWithOverflow(a, offset)[0];
    } else {
        a = @addWithOverflow(a, offset)[0];
    }

    const z = a == 0;
    const h = false;

    cpu.f.write(z, c, h, s);
    cpu.set_byte(.a, a);
    cpu.pc += 1;
    return 1;
}
// 16 bit
pub fn INCr16(cpu: *CPU, args: InstrArgs) u8 {
    const value = cpu.get_word(args.target);
    const res = @addWithOverflow(value, 1)[0];
    cpu.pushToExecutionChain("INCr16 | target: {any}, 0x{X} + 1 = 0x{X}", .{ args.target, value, res });
    cpu.set_word(args.target, res);
    cpu.pc += 1;
    return 2;
}
pub fn INCSP(cpu: *CPU, _: InstrArgs) u8 {
    const value = cpu.sp;
    const res = @addWithOverflow(value, 1)[0];
    cpu.pushToExecutionChain("INCSP | 0x{X} + 1 = 0x{X}", .{ value, res });
    cpu.sp = res;
    cpu.pc += 1;
    return 2;
}
pub fn INCHL(cpu: *CPU, _: InstrArgs) u8 { // increment the value of the byte pointed to by hl
    const mem_place = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(mem_place);
    const res = @addWithOverflow(value, 1)[0];
    cpu.bus.writeByte(mem_place, res);
    cpu.pushToExecutionChain("INCHL | mem@hl: 0x{X} + 1 = 0x{X}", .{ value, res });
    const h = detectHalfCarry(value, 1, .add);
    const z = res == 0;
    const s = false;
    const c = cpu.f.cFlag();
    cpu.f.write(z, c, h, s);
    cpu.pc += 1;
    return 3;
}
pub fn DECr16(cpu: *CPU, args: InstrArgs) u8 { // decrement any 16 bit register;
    const value = cpu.get_word(args.target);
    cpu.pushToExecutionChain("DECr16 | target: {any}", .{args.target});
    cpu.set_word(args.target, @subWithOverflow(value, 1)[0]);
    cpu.pc += 1;
    return 2;
}
pub fn DECSP(cpu: *CPU, _: InstrArgs) u8 { // decrement the stack pointer
    cpu.pushToExecutionChain("DECSP", .{});
    const value = cpu.sp;
    cpu.sp = @subWithOverflow(value, 1)[0];
    cpu.pc += 1;
    return 2;
}
pub fn DECHL(cpu: *CPU, _: InstrArgs) u8 { // decrement the value of the byte pointed to by hl
    const mem_place = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(mem_place);
    const res = @subWithOverflow(value, 1)[0];
    cpu.bus.writeByte(mem_place, res);
    const h = detectHalfCarry(value, 1, .sub);
    const z = res == 0;
    const s = true;
    const c = cpu.f.cFlag();
    cpu.f.write(z, c, h, s);
    cpu.pushToExecutionChain("DECHL | mem@hl: 0x{X} - 1 = 0x{X}", .{ value, res });
    cpu.pc += 1;
    return 3;
}
pub fn ADDHLr16(cpu: *CPU, args: InstrArgs) u8 {
    const hl = cpu.get_word(regID.h);
    const value = cpu.get_word(args.target);
    const debug = "ADDHLr16 | {any} + hl, {d} + {d}";
    cpu.pushToExecutionChain(debug, .{ args.target, value, hl });
    // print(debug, .{ args.target, value, hl });
    const res: struct { u16, u1 } = @addWithOverflow(hl, value);
    const s = false;
    const h = detectHalfCarry(hl, value, .add);
    const c = res[1] == 1;
    const z = cpu.f.zFlag();
    cpu.f.write(z, c, h, s);
    cpu.set_word(regID.h, res[0]);
    cpu.pc += 1;
    return 2;
}
pub fn ADDHLSP(cpu: *CPU, _: InstrArgs) u8 {
    //  0  0  0  0 _  0  0 0 0 _ 0 0 0 0 _ 0 0 0 0
    // 15 14 13 12   11 10
    const hl = cpu.get_word(regID.h);
    const value = cpu.sp;
    const debug = "ADDHLSP | SP + hl, 0x{X} + 0x{X}";
    cpu.pushToExecutionChain(debug, .{ value, hl });
    // print(debug, .{ value, hl });
    const res: struct { u16, u1 } = @addWithOverflow(hl, value);
    const s = false;
    const h = detectHalfCarry(hl, value, .add);
    const c = res[1] == 1;
    const z = cpu.f.zFlag();
    cpu.f.write(z, c, h, s);
    cpu.set_word(regID.h, res[0]);
    cpu.pc += 1;
    return 2;
}
// MISC
pub fn SCF(cpu: *CPU, _: InstrArgs) u8 { // set carry flag
    cpu.pushToExecutionChain("SCF", .{});
    cpu.f.write(cpu.f.zFlag(), true, false, false);
    cpu.pc += 1;
    return 1;
}
pub fn CCF(cpu: *CPU, _: InstrArgs) u8 { // complement carry flag
    cpu.pushToExecutionChain("SCF", .{});
    cpu.f.write(cpu.f.zFlag(), !cpu.f.cFlag(), false, false);
    cpu.pc += 1;
    return 1;
}
pub fn CPL(cpu: *CPU, _: InstrArgs) u8 { // sets the value in register A to its complement
    cpu.pushToExecutionChain("CPL", .{});
    cpu.set_byte(.a, cpu.get_byte(.a) ^ 0xFF);
    cpu.f.write(cpu.f.zFlag(), cpu.f.cFlag(), true, true);
    cpu.pc += 1;
    return 1;
}
pub fn EI(cpu: *CPU, _: InstrArgs) u8 {
    cpu.pushToExecutionChain("EI", .{});
    print("EI!\n\n pc = 0x{X}\n", .{cpu.pc});
    // cpu.break_exe();
    cpu.pc += 1;
    return 1;
}
pub fn DI(cpu: *CPU, _: InstrArgs) u8 {
    // print("DI!\n\n", .{});
    // cpu.break_exe();
    const prior = cpu.bus.handler.ime;
    const debug = "DI | ime prior: {any}, ime post op: {any}";
    print(debug ++ "\n", .{ prior, cpu.bus.handler.ime });
    cpu.bus.handler.ime = false;
    cpu.pushToExecutionChain(debug, .{ prior, cpu.bus.handler.ime });
    cpu.pc += 1;
    return 1;
}
pub fn PUSH(cpu: *CPU, args: InstrArgs) u8 {
    var high: u8 = undefined;
    var low: u8 = undefined;
    // print("[pc]:0x{X}\t", .{cpu.pc});
    if (args.target == regID.a) {
        high = cpu.get_byte(regID.a);
        low = cpu.f.value & 0xF0; // only the upper 4 bits
        cpu.pushToExecutionChain("PUSH AF a: 0x{X}, f: 0x{X}", .{ high, low });
    } else {
        const value = cpu.get_word(args.target);
        high = @truncate(value >> 8);
        low = @truncate(value);
        cpu.pushToExecutionChain("PUSH 0x{X} from {any}, hi 0x{X} lo 0x{X}", .{ value, args.target, high, low });
    }
    cpu.push_stack((@as(u16, high) << 8) | low);
    cpu.pc += 1;
    return 4;
}
pub fn POP(cpu: *CPU, args: InstrArgs) u8 {
    const popped = cpu.pop_stack();
    const low = popped[0];
    const high = popped[1];
    // print("[pc]:0x{X}\t", .{cpu.pc});
    const value = @as(u16, high) << 8 | low;
    cpu.pushToExecutionChain("POP 0x{X} --> {any}", .{ value, args.target });
    if (args.target == regID.a) {
        cpu.set_byte(regID.a, high);
        cpu.f.value = low & 0xF0;
    } else cpu.set_word(args.target, value);
    cpu.pc += 1;
    return 3;
}
pub fn CPAn8(cpu: *CPU, _: InstrArgs) u8 {
    const n = cpu.bus.readByte(cpu.pc + 1);
    const reg = cpu.get_byte(regID.a);
    const z = reg == n;
    const s = true;
    const h = detectHalfCarry(reg, n, .sub);
    const c = reg < n;
    cpu.f.write(z, c, h, s);
    cpu.pushToExecutionChain("CPAn8 | := reg.A, n := {d}, {d}", .{ reg, n });
    cpu.pc += 2;
    return 2;
}
pub fn CPAr8(cpu: *CPU, args: InstrArgs) u8 {
    const n = cpu.get_byte(args.target);
    // print("CPAr8, target = {any}\n", .{args.target});
    const reg = cpu.get_byte(regID.a);
    // const res = @subWithOverflow(reg, n);
    const z = reg == n;
    const s = true;
    const h = detectHalfCarry(reg, n, .sub);
    const c = reg < n;
    cpu.f.write(z, c, h, s);
    cpu.pushToExecutionChain("CPAr8 | reg.A, {any} := {d}, {d}", .{ args.target, reg, n });
    cpu.pc += 1;
    return 1;
}
pub fn CPAHL(cpu: *CPU, _: InstrArgs) u8 {
    const hl = cpu.get_word(regID.h);
    const reg = cpu.get_byte(regID.a);
    const byte = cpu.bus.readByte(hl);
    // print("CPAHL, compare mem_place: 0x{X} ({d}) to A:{d}\n", .{ hl, cpu.bus.readByte(hl), reg });
    const z = reg == byte;
    const s = true;
    const h = detectHalfCarry(reg, byte, .sub);
    const c = reg < byte;
    cpu.f.write(z, c, h, s);
    cpu.pushToExecutionChain("CPAHL | reg.A, mem[X.{X:04}] := {d}, {d}", .{ hl, reg, byte });
    cpu.pc += 1;
    return 2;
}
// ROTATES & SHIFTS
pub fn RRA(cpu: *CPU, _: InstrArgs) u8 { // C -> [7 -> 0] -> C into A
    const a = cpu.get_byte(regID.a);
    cpu.pushToExecutionChain("RRA", .{});
    cpu.set_byte(regID.a, @as(u8, @intFromBool(cpu.f.cFlag())) << 7 | a >> 1);
    const c = (@as(u1, @truncate(a)) == 1);
    const s = false;
    const h = false;
    const z = false;
    cpu.f.write(z, c, h, s);
    cpu.pc += 1;
    return 1;
}
pub fn RRCA(cpu: *CPU, _: InstrArgs) u8 {
    const a = cpu.get_byte(regID.a);
    cpu.pushToExecutionChain("RRCA", .{});
    cpu.set_byte(regID.a, (a << 7) | (a >> 1));
    const c = (@as(u1, @truncate(a)) == 1);
    const s = false;
    const h = false;
    const z = false;
    cpu.f.write(z, c, h, s);
    cpu.pc += 1;
    return 1;
}
pub fn RLA(cpu: *CPU, _: InstrArgs) u8 { // C <- [7 <- 0] <- C
    const carried = cpu.f.cFlag();
    const reg = cpu.get_byte(regID.a);
    cpu.pushToExecutionChain("RLA | prior: 0b{b}, carried = {d}", .{ reg, @intFromBool(carried) });
    const c = reg >> 7 == 1;
    const rotated: u8 = reg << 1 | @intFromBool(carried);
    const z = false;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, rotated);
    cpu.pc += 1;
    return 1;
}
pub fn RLCA(cpu: *CPU, _: InstrArgs) u8 { //Rotate register A left.
    cpu.pushToExecutionChain("RLCA | regID.a << 1", .{});
    const a = cpu.get_byte(.a);
    const c = (a >> 7) == 1;
    const z = false;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    const shifted = a << 1;
    cpu.set_byte(.a, shifted | (a >> 7));
    cpu.pc += 1;
    return 1;
}
// prefixed
//
pub fn RLCr8(cpu: *CPU, args: InstrArgs) u8 { //Rotate register left. C <- [7 <- 0] <- [7]
    cpu.pushToExecutionChain("RLCr8 | regID.a << 1", .{});
    const reg = cpu.get_byte(args.target);
    const rotated = reg << 1;
    const res = rotated | (reg >> 7);
    // const msb = reg >> 7;
    // print("RLC msb: {d}, rotated: 0b{b}\n", .{msb, rotated});
    const c = (reg >> 7) == 1;
    const z = res == 0;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(args.target, res);
    cpu.pc += 1;
    return 2;
}
pub fn RLCHL(cpu: *CPU, _: InstrArgs) u8 { //Rotate byte pointed to by hl left. C <- [7 <- 0] <- [7]
    cpu.pushToExecutionChain("RLCHL | regID.a << 1", .{});
    const mem_address = cpu.get_word(.h);
    const byte = cpu.bus.readByte(mem_address);
    const rotated = (byte << 1) | (byte >> 7);
    const res = rotated | (byte >> 7);
    const c = (byte >> 7) == 1;
    const z = rotated == 0;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.bus.writeByte(mem_address, res);
    cpu.pc += 1;
    return 4;
}
pub fn RLr8(cpu: *CPU, args: InstrArgs) u8 { // C <- [7 <- 0] <- C Rotate bits in register r8 left through carry.
    const carried = cpu.f.cFlag();
    const reg = cpu.get_byte(args.target);
    const c = reg >> 7 == 1;
    const rotated: u8 = reg << 1 | @intFromBool(carried);
    const z = rotated == 0;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(args.target, rotated);
    cpu.pushToExecutionChain("RLr8 | target: {any}, b.{b}", .{ args.target, reg });
    cpu.pc += 1;
    return 2;
}
pub fn RLHL(cpu: *CPU, _: InstrArgs) u8 { // C <- [7 <- 0] <- C Rotate bits in register r8 left through carry.
    const carried = cpu.f.cFlag();
    const mem_place = cpu.get_word(.h);
    const byte = cpu.bus.readByte(mem_place);
    const c = byte >> 7 == 1;
    const rotated: u8 = byte << 1 | @intFromBool(carried);
    const z = rotated == 0;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.bus.writeByte(mem_place, rotated);
    cpu.pushToExecutionChain("RLHL | b.{b}", .{byte});
    cpu.pc += 1;
    return 4;
}
pub fn RRCr8(cpu: *CPU, args: InstrArgs) u8 { //Rotate register right. 0 -> [7 -> 0] -> C
    cpu.pushToExecutionChain("RRCr8 | target {any} << 1", .{args.target});
    const reg = cpu.get_byte(args.target);
    const rotated = reg >> 1;
    const res = rotated | ((reg & 1) << 7);
    const c = (reg & 1) == 1;
    const z = res == 0;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(args.target, res);
    cpu.pc += 1;
    return 2;
}
pub fn RRCHL(cpu: *CPU, _: InstrArgs) u8 { //Rotate byte pointed to by hl right. 0 -> [7 -> 0] -> C
    cpu.pushToExecutionChain("RRCHL | HL >> 1", .{});
    const mem_address = cpu.get_word(.h);
    const byte = cpu.bus.readByte(mem_address);
    const rotated = byte >> 1;
    const res = rotated | (byte & 1) << 7;
    const c = (byte & 1) == 1;
    const z = res == 0;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.bus.writeByte(mem_address, res);
    cpu.pc += 1;
    return 4;
}
pub fn RRr8(cpu: *CPU, args: InstrArgs) u8 { // C -> [7 -> 0] -> C Rotate bits in register r8 left through carry.
    const carried = cpu.f.cFlag();
    const reg = cpu.get_byte(args.target);
    const c = reg & 1 == 1;
    const rotated: u8 = reg >> 1 | @as(u8, @intFromBool(carried)) << 7;
    const z = rotated == 0;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(args.target, rotated);
    cpu.pushToExecutionChain("RRr8 | target: {any}, b.{b}", .{ args.target, reg });
    cpu.pc += 1;
    return 2;
}
pub fn RRHL(cpu: *CPU, _: InstrArgs) u8 { // C -> [7 -> 0] -> C Rotate bits in register r8 left through carry.
    const carried = cpu.f.cFlag();
    const mem_place = cpu.get_word(.h);
    const byte = cpu.bus.readByte(mem_place);
    const c = byte & 1 == 1;
    const rotated: u8 = byte >> 1 | @as(u8, @intFromBool(carried)) << 7;
    const z = rotated == 0;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.bus.writeByte(mem_place, rotated);
    cpu.pushToExecutionChain("RRHL | b.{b}", .{byte});
    cpu.pc += 1;
    return 4;
}
pub fn SLAr8(cpu: *CPU, args: InstrArgs) u8 { // Shift Left Arithmetic register r8. C <- [7 <- 0] <- 0
    const reg = cpu.get_byte(args.target);
    const shifted = reg << 1;
    const z = shifted == 0;
    const c = reg >> 7 == 1;
    cpu.f.write(z, c, false, false);
    cpu.set_byte(args.target, shifted);
    cpu.pc += 1;
    return 2;
}
pub fn SLAHL(cpu: *CPU, _: InstrArgs) u8 { // Shift Left Arithmetic byte pointed to by hl. C <- [7 <- 0] <- 0
    const mem_address = cpu.get_word(.h);
    const byte = cpu.bus.readByte(mem_address);
    const shifted = byte << 1;
    const z = shifted == 0;
    const c = byte >> 7 == 1;
    cpu.f.write(z, c, false, false);
    cpu.bus.writeByte(mem_address, shifted);
    cpu.pc += 1;
    return 4;
}
pub fn SRAr8(cpu: *CPU, args: InstrArgs) u8 { // Shift Right Arithmetic register r8. 7 -> [7 -> 0] -> C
    const reg = cpu.get_byte(args.target);
    const shifted = ((reg >> 7 & 1) << 7) | reg >> 1;
    const z = shifted == 0;
    const c = reg & 1 == 1;
    cpu.f.write(z, c, false, false);
    cpu.set_byte(args.target, shifted);
    cpu.pc += 1;
    return 2;
}
pub fn SRAHL(cpu: *CPU, _: InstrArgs) u8 { // Shift Right Arithmetic byte pointed to by hl. 0 -> [7 -> 0] -> C
    const mem_address = cpu.get_word(.h);
    const byte = cpu.bus.readByte(mem_address);
    const shifted = ((byte >> 7 & 1) << 7) | byte >> 1;
    const z = shifted == 0;
    const c = byte & 1 == 1;
    cpu.f.write(z, c, false, false);
    cpu.bus.writeByte(mem_address, shifted);
    cpu.pc += 1;
    return 4;
}
pub fn SRLr8(cpu: *CPU, args: InstrArgs) u8 { // Shift Right Arithmetic register r8. 7 -> [7 -> 0] -> C
    const reg = cpu.get_byte(args.target);
    const shifted = reg >> 1;
    const z = shifted == 0;
    const c = reg & 1 == 1;
    cpu.f.write(z, c, false, false);
    cpu.set_byte(args.target, shifted);
    cpu.pc += 1;
    return 2;
}
pub fn SRLHL(cpu: *CPU, _: InstrArgs) u8 { // Shift Right Arithmetic byte pointed to by hl. 0 -> [7 -> 0] -> C
    const mem_address = cpu.get_word(.h);
    const byte = cpu.bus.readByte(mem_address);
    const shifted = byte >> 1;
    const z = shifted == 0;
    const c = byte & 1 == 1;
    cpu.f.write(z, c, false, false);
    cpu.bus.writeByte(mem_address, shifted);
    cpu.pc += 1;
    return 4;
}
pub fn SWAPr8(cpu: *CPU, args: InstrArgs) u8 { // Swap the upper 4 bits in register r8 and the lower 4 ones.
    const reg = cpu.get_byte(args.target);
    const high: u4 = @truncate(reg >> 4);
    const low: u4 = @truncate(reg);
    cpu.set_byte(args.target, (@as(u8, low) << 4) | high);
    const z = reg == 0;
    const c = false;
    const s = false;
    const h = false;
    cpu.f.write(z, c, h, s);
    cpu.pc += 1;
    return 2;
}
pub fn SWAPHL(cpu: *CPU, _: InstrArgs) u8 { // Swap the upper 4 bits in register r8 and the lower 4 ones.
    const mem_address = cpu.get_word(.h);
    const byte = cpu.bus.readByte(mem_address);
    const high: u4 = @truncate(byte >> 4);
    const low: u4 = @truncate(byte);
    cpu.bus.writeByte(mem_address, (@as(u8, low) << 4) | high);
    const z = byte == 0;
    const c = false;
    const s = false;
    const h = false;
    cpu.f.write(z, c, h, s);
    cpu.pc += 1;
    return 4;
}
// BIT MANIPULATION
//
pub fn BITTESTr8(cpu: *CPU, args: InstrArgs) u8 {
    const bit: u3 = args.bit_target.bit;
    const target = cpu.get_byte(args.bit_target.target);
    const z = @as(u1, @truncate(target >> bit)) == 0; // set zero flag if the target bit is not set
    const c = cpu.f.cFlag();
    cpu.f.write(z, c, true, false);
    cpu.pushToExecutionChain("BITTEST | {any} >> {d}", .{ target, bit }); // which register/bit
    cpu.pc += 1;
    return 2;
}
pub fn BITTESTHL(cpu: *CPU, args: InstrArgs) u8 {
    const bit: u3 = args.bit;
    const hl = cpu.get_word(.h);
    const byte = cpu.bus.readByte(hl);
    const z = @as(u1, @truncate(byte >> bit)) == 0;
    const c = cpu.f.cFlag();
    cpu.f.write(z, c, true, false);
    cpu.pushToExecutionChain("BITTEST HL | mem[X.{X:04}] = b.{b} >> {d}", .{ hl, byte, bit }); // which bit
    cpu.pc += 1;
    return 3;
}
pub fn RES(cpu: *CPU, args: InstrArgs) u8 { // Set bit u3 in register r8 to 0
    const bit: u3 = args.bit_target.bit;
    const target = cpu.get_byte(args.bit_target.target);
    const res = target & ~(@as(u8, 1) << bit); // target and everything but this bit
    cpu.set_byte(args.bit_target.target, res);
    cpu.pushToExecutionChain("RES | {any} >> {d} = 0", .{ target, bit }); // which register/bit
    cpu.pc += 1;
    return 2;
}
pub fn RESHL(cpu: *CPU, args: InstrArgs) u8 { // Set bit u3 in the byte pointed to by hl to 0.
    const bit: u3 = args.bit;
    const hl = cpu.get_word(.h);
    const byte = cpu.bus.readByte(hl);
    const res = byte & ~(@as(u8, 1) << bit); // target and everything but this bit
    cpu.bus.writeByte(hl, res);
    cpu.pushToExecutionChain("RES HL | mem[X.{X:04}] = b.{b} >> {d}", .{ hl, byte, bit }); // which bit
    cpu.pc += 1;
    return 4;
}
pub fn SET(cpu: *CPU, args: InstrArgs) u8 { // Set bit u3 in register r8 to 1. Bit 0 is the rightmost one, bit 7 the leftmost one.
    const bit: u3 = args.bit_target.bit;
    const target = cpu.get_byte(args.bit_target.target);
    const res = target | (@as(u8, 1) << bit); // everything and this bit
    cpu.set_byte(args.bit_target.target, res);
    cpu.pushToExecutionChain("RES | {any} >> {d} = 0", .{ target, bit }); // which register/bit
    cpu.pc += 1;
    return 2;
}
pub fn SETHL(cpu: *CPU, args: InstrArgs) u8 { // Set bit u3 in the byte pointed to by hl to 1.
    const bit: u3 = args.bit;
    const hl = cpu.get_word(.h);
    const byte = cpu.bus.readByte(hl);
    const res = byte | (@as(u8, 1) << bit); // everything and this bit
    cpu.bus.writeByte(hl, res);
    cpu.pushToExecutionChain("RES HL | mem[X.{X:04}] = b.{b} >> {d}", .{ hl, byte, bit }); // which bit
    cpu.pc += 1;
    return 4;
}

// JUMP
pub fn JP(cpu: *CPU, args: InstrArgs) u8 {
    const jump = cpu.f.check(args.flagConditions);
    if (jump) {
        const n = @as(u16, cpu.bus.readByte(cpu.pc + 2)) << 8 | cpu.bus.readByte(cpu.pc + 1);
        cpu.pushToExecutionChain("JP | to 0x{X}", .{n});
        cpu.pc = n;
        return 4; // 4 cycles when taken
    } else {
        cpu.pushToExecutionChain("JP | no jump", .{});
        cpu.pc += 3;
        return 3; // 3 cycles when not taken
    }
}
pub fn JPHL(cpu: *CPU, _: InstrArgs) u8 {
    cpu.pushToExecutionChain("JPHL", .{});
    cpu.pc = cpu.get_word(regID.h);
    return 1;
}
pub fn JR(cpu: *CPU, args: InstrArgs) u8 {
    const dist: i8 = @bitCast(cpu.bus.readByte(cpu.pc + 1));
    const jump = cpu.f.check(args.flagConditions);

    if (jump) {
        const new_mem: u16 = @intCast(@addWithOverflow(@as(i17, @intCast(cpu.pc + 2)), dist)[0]);
        cpu.pushToExecutionChain("JR | to pc:0x{X}", .{new_mem});
        cpu.pc = new_mem;
        return 3; // 3 cycles when taken
    } else { // next instruction, condition failed
        cpu.pushToExecutionChain("JR | skipped jump, failed condition", .{});
        cpu.pc += 2;
        return 2; // 2 cycles when not taken
    }
}
// CALL
pub fn CALLn16(cpu: *CPU, args: InstrArgs) u8 { //
    // const zone = tracy.beginZone(@src(), .{ .name = "CALLn16" });
    // defer zone.end();
    const call = cpu.f.check(args.flagConditions);
    // const byte = cpu.bus.readByte(cpu.pc);
    if (call) {
        // print("[pc]:0x{X}\t", .{cpu.pc});
        const n = @as(u16, cpu.bus.readByte(cpu.pc + 2)) << 8 | cpu.bus.readByte(cpu.pc + 1);
        const ret = cpu.pc + 3;
        cpu.pushToExecutionChain("CALL to 0x{X}, later RET to 0x{X}", .{ n, ret });
        cpu.sp -= 1;
        cpu.bus.writeByte(cpu.sp, @truncate(ret >> 8));
        cpu.sp -= 1;
        cpu.bus.writeByte(cpu.sp, @truncate(ret));
        cpu.pc = n;
        return 6; // 6 cycles when taken
    } else { // next instruction, condition failed
        cpu.pushToExecutionChain("skipped jump, failed condition", .{});
        cpu.pc += 3;
        return 3; // 3 cycles when not taken
    }
}
// RESTART
pub fn RST(cpu: *CPU, args: InstrArgs) u8 {
    const ret = cpu.pc + 1;
    cpu.push_stack(ret);
    cpu.pushToExecutionChain("RST | to 0x{X}, later RET to 0x{X}", .{ args.where, ret });
    cpu.pc = args.where;
    return 4;
}
// RETURN
pub fn RET(cpu: *CPU, args: InstrArgs) u8 {
    const ret = cpu.f.check(args.flagConditions);
    if (ret) {
        const low = cpu.bus.readByte(cpu.sp);
        cpu.sp += 1;
        const high = cpu.bus.readByte(cpu.sp);
        const jumpto = @as(u16, high) << 8 | low;
        cpu.pushToExecutionChain("RET | {any} met, jumpto pc[{X:04}]", .{ args.flagConditions, jumpto });
        cpu.sp += 1;
        cpu.pc = jumpto;
        return switch (args.flagConditions) {
            .none => 4,
            else => 5, // 5 cycles if condition met
        };
    } else {
        cpu.pushToExecutionChain("RET | if {any} not met", .{args.flagConditions});
        cpu.pc += 1;
        return 2; // 2 cycles when not taken
    }
}
pub fn RETI(cpu: *CPU, _: InstrArgs) u8 {
    // print("reti\n", .{});
    const popped = cpu.pop_stack();
    const low = popped[0];
    const high = popped[1];
    const jumpto = @as(u16, high) << 8 | low;
    cpu.pushToExecutionChain("RETI | jumpto pc[{X:04}]", .{jumpto});
    cpu.pc = jumpto;
    cpu.bus.handler.ime = true;
    return 4;
}
pub const DEBUG = true;
pub inline fn fmtInsDebug(string: []const u8, args: anytype) []const u8 {
    var buffer: [CPU.Log.MAX_CHAR]u8 = undefined;
    return std.fmt.bufPrint(&buffer, string, args) catch unreachable;
}

const Instr = struct {
    func: *const InstrFn,
    args: InstrArgs,
    cycles: u8,
    bytes: u8,

    pub inline fn call(self: *const Instr, cpu: *CPU) u8 {
        return self.func(cpu, self.args);
    }
};

pub const instrs = [256]Instr{
    // 0x00 - 0x0F
    .{ // 0x00 NOP
        .func = NOP,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x01 LD BC,d16
        .func = LD16,
        .args = .{ .target = regID.b },
        .cycles = 3,
        .bytes = 3,
    },
    .{ // 0x02 LD (BC),A
        .func = LDr16A,
        .args = .{ .target = regID.b },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x03 INC BC
        .func = INCr16,
        .args = .{ .target = regID.b },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x04 INC B
        .func = INCr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x05 DEC B
        .func = DECr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x06 LD B,d8
        .func = LD8,
        .args = .{ .target = regID.b },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0x07 RLCA
        .func = RLCA,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x08 LD (a16),SP
        .func = LDn16SP,
        .args = .{ .none = {} },
        .cycles = 3,
        .bytes = 3,
    },
    .{ // 0x09 ADD HL,BC
        .func = ADDHLr16,
        .args = .{ .target = regID.b },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x0A LD A,(BC)
        .func = LDAr16,
        .args = .{ .target = regID.b },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x0B DEC BC
        .func = DECr16,
        .args = .{ .target = regID.b },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x0C INC C
        .func = INCr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x0D DEC C
        .func = DECr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x0E LD C,d8
        .func = LD8,
        .args = .{ .target = regID.c },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0x0F RRCA
        .func = RRCA,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x10 STOP
        .func = STOP,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0x11 LD DE,d16
        .func = LD16,
        .args = .{ .target = regID.d },
        .cycles = 3,
        .bytes = 3,
    },
    .{ // 0x12 LD (DE),A
        .func = LDr16A,
        .args = .{ .target = regID.d },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x13 INC DE
        .func = INCr16,
        .args = .{ .target = regID.d },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x14 INC D
        .func = INCr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x15 DEC D
        .func = DECr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x16 LD D,d8
        .func = LD8,
        .args = .{ .target = regID.d },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0x17 RLA
        .func = RLA,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x18 JR r8
        .func = JR,
        .args = .{ .flagConditions = .none },
        .cycles = 3,
        .bytes = 2,
    },
    .{ // 0x19 ADD HL,DE
        .func = ADDHLr16,
        .args = .{ .target = regID.d },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x1A LD A,(DE)
        .func = LDAr16,
        .args = .{ .target = regID.d },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x1B DEC DE
        .func = DECr16,
        .args = .{ .target = regID.d },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x1C INC E
        .func = INCr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x1D DEC E
        .func = DECr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x1E LD E,d8
        .func = LD8,
        .args = .{ .target = regID.e },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0x1F RRA
        .func = RRA,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x20 JR NZ,r8
        .func = JR,
        .args = .{ .flagConditions = .nz },
        .cycles = 3,
        .bytes = 2,
    },
    .{ // 0x21 LD HL,d16
        .func = LD16,
        .args = .{ .target = regID.h },
        .cycles = 3,
        .bytes = 3,
    },
    .{ // 0x22 LD (HL+),A
        .func = LDHLIA,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x23 INC HL
        .func = INCr16,
        .args = .{ .target = regID.h },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x24 INC H
        .func = INCr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x25 DEC H
        .func = DECr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x26 LD H,d8
        .func = LD8,
        .args = .{ .target = regID.h },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0x27 DAA
        .func = DAA,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x28 JR Z,r8
        .func = JR,
        .args = .{ .flagConditions = .z },
        .cycles = 3,
        .bytes = 2,
    },
    .{ // 0x29 ADD HL,HL
        .func = ADDHLr16,
        .args = .{ .target = regID.h },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x2A LD A,(HL+)
        .func = LDAHL,
        .args = .{ .hl_mod = 1 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x2B DEC HL
        .func = DECr16,
        .args = .{ .target = regID.h },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x2C INC L
        .func = INCr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x2D DEC L
        .func = DECr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x2E LD L,d8
        .func = LD8,
        .args = .{ .target = regID.l },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0x2F CPL
        .func = CPL,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x30 JR NC,r8
        .func = JR,
        .args = .{ .flagConditions = .nc },
        .cycles = 3,
        .bytes = 2,
    },
    .{ // 0x31 LD SP,d16
        .func = LDSP16,
        .args = .{ .none = {} },
        .cycles = 3,
        .bytes = 3,
    },
    .{ // 0x32 LD (HL-),A
        .func = LDHLDA,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x33 INC SP
        .func = INCSP,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x34 INC (HL)
        .func = INCHL,
        .args = .{ .none = {} },
        .cycles = 3,
        .bytes = 1,
    },
    .{ // 0x35 DEC (HL)
        .func = DECHL,
        .args = .{ .none = {} },
        .cycles = 3,
        .bytes = 1,
    },
    .{ // 0x36 LD (HL),d8
        .func = LDHL8,
        .args = .{ .none = {} },
        .cycles = 3,
        .bytes = 2,
    },
    .{ // 0x37 SCF
        .func = SCF,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x38 JR C,r8
        .func = JR,
        .args = .{ .flagConditions = .c },
        .cycles = 3,
        .bytes = 2,
    },
    .{ // 0x39 ADD HL,SP
        .func = ADDHLSP,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x3A LD A,(HL-)
        .func = LDAHL,
        .args = .{ .hl_mod = -1 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x3B DEC SP
        .func = DECSP,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x3C INC A
        .func = INCr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x3D DEC A
        .func = DECr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x3E LD A,d8
        .func = LD8,
        .args = .{ .target = regID.a },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0x3F CCF
        .func = CCF,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x40 LD B,B
        .func = LDr8,
        .args = .{ .targets = .{ .to = .b, .from = .b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x41 LD B,C
        .func = LDr8,
        .args = .{ .targets = .{ .to = .b, .from = .c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x42 LD B,D
        .func = LDr8,
        .args = .{ .targets = .{ .to = .b, .from = .d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x43 LD B,E
        .func = LDr8,
        .args = .{ .targets = .{ .to = .b, .from = .e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x44 LD B,H
        .func = LDr8,
        .args = .{ .targets = .{ .to = .b, .from = .h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x45 LD B,L
        .func = LDr8,
        .args = .{ .targets = .{ .to = .b, .from = .l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x46 LD B,(HL)
        .func = LDr8HL,
        .args = .{ .target = regID.b },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x47 LD B,A
        .func = LDr8,
        .args = .{ .targets = .{ .to = .b, .from = .a } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x48 LD C,B
        .func = LDr8,
        .args = .{ .targets = .{ .to = .c, .from = .b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x49 LD C,C
        .func = LDr8,
        .args = .{ .targets = .{ .to = .c, .from = .c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x4A LD C,D
        .func = LDr8,
        .args = .{ .targets = .{ .to = .c, .from = .d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x4B LD C,E
        .func = LDr8,
        .args = .{ .targets = .{ .to = .c, .from = .e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x4C LD C,H
        .func = LDr8,
        .args = .{ .targets = .{ .to = .c, .from = .h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x4D LD C,L
        .func = LDr8,
        .args = .{ .targets = .{ .to = .c, .from = .l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x4E LD C,(HL)
        .func = LDr8HL,
        .args = .{ .target = regID.c },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x4F LD C,A
        .func = LDr8,
        .args = .{ .targets = .{ .to = .c, .from = .a } },
        .cycles = 1,
        .bytes = 1,
    },

    // 0x50–0x7F (D–A registers and HL/A transfers)
    .{ // 0x50 LD D,B
        .func = LDr8,
        .args = .{ .targets = .{ .to = .d, .from = .b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x51 LD D,C
        .func = LDr8,
        .args = .{ .targets = .{ .to = .d, .from = .c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x52 LD D,D
        .func = LDr8,
        .args = .{ .targets = .{ .to = .d, .from = .d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x53 LD D,E
        .func = LDr8,
        .args = .{ .targets = .{ .to = .d, .from = .e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x54 LD D,H
        .func = LDr8,
        .args = .{ .targets = .{ .to = .d, .from = .h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x55 LD D,L
        .func = LDr8,
        .args = .{ .targets = .{ .to = .d, .from = .l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x56 LD D,(HL)
        .func = LDr8HL,
        .args = .{ .target = regID.d },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x57 LD D,A
        .func = LDr8,
        .args = .{ .targets = .{ .to = .d, .from = .a } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x58 LD E,B
        .func = LDr8,
        .args = .{ .targets = .{ .to = .e, .from = .b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x59 LD E,C
        .func = LDr8,
        .args = .{ .targets = .{ .to = .e, .from = .c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x5A LD E,D
        .func = LDr8,
        .args = .{ .targets = .{ .to = .e, .from = .d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x5B LD E,E
        .func = LDr8,
        .args = .{ .targets = .{ .to = .e, .from = .e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x5C LD E,H
        .func = LDr8,
        .args = .{ .targets = .{ .to = .e, .from = .h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x5D LD E,L
        .func = LDr8,
        .args = .{ .targets = .{ .to = .e, .from = .l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x5E LD E,(HL)
        .func = LDr8HL,
        .args = .{ .target = regID.e },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x5F LD E,A
        .func = LDr8,
        .args = .{ .targets = .{ .to = .e, .from = .a } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x60 LD H,B
        .func = LDr8,
        .args = .{ .targets = .{ .to = .h, .from = .b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x61 LD H,C
        .func = LDr8,
        .args = .{ .targets = .{ .to = .h, .from = .c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x62 LD H,D
        .func = LDr8,
        .args = .{ .targets = .{ .to = .h, .from = .d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x63 LD H,E
        .func = LDr8,
        .args = .{ .targets = .{ .to = .h, .from = .e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x64 LD H,H
        .func = LDr8,
        .args = .{ .targets = .{ .to = .h, .from = .h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x65 LD H,L
        .func = LDr8,
        .args = .{ .targets = .{ .to = .h, .from = .l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x66 LD H,(HL)
        .func = LDr8HL,
        .args = .{ .target = regID.h },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x67 LD H,A
        .func = LDr8,
        .args = .{ .targets = .{ .to = .h, .from = .a } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x68 LD L,B
        .func = LDr8,
        .args = .{ .targets = .{ .to = .l, .from = .b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x69 LD L,C
        .func = LDr8,
        .args = .{ .targets = .{ .to = .l, .from = .c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x6A LD L,D
        .func = LDr8,
        .args = .{ .targets = .{ .to = .l, .from = .d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x6B LD L,E
        .func = LDr8,
        .args = .{ .targets = .{ .to = .l, .from = .e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x6C LD L,H
        .func = LDr8,
        .args = .{ .targets = .{ .to = .l, .from = .h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x6D LD L,L
        .func = LDr8,
        .args = .{ .targets = .{ .to = .l, .from = .l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x6E LD L,(HL)
        .func = LDr8HL,
        .args = .{ .target = regID.l },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x6F LD L,A
        .func = LDr8,
        .args = .{ .targets = .{ .to = .l, .from = .a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // 0x70 LD (HL),B
        .func = LDHLr8,
        .args = .{ .target = regID.b },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x71 LD (HL),C
        .func = LDHLr8,
        .args = .{ .target = regID.c },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x72 LD (HL),D
        .func = LDHLr8,
        .args = .{ .target = regID.d },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x73 LD (HL),E
        .func = LDHLr8,
        .args = .{ .target = regID.e },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x74 LD (HL),H
        .func = LDHLr8,
        .args = .{ .target = regID.h },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x75 LD (HL),L
        .func = LDHLr8,
        .args = .{ .target = regID.l },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x76 HALT
        .func = HALT,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x77 LD (HL),A
        .func = LDHLr8,
        .args = .{ .target = regID.a },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x78 LD A,B
        .func = LDr8,
        .args = .{ .targets = .{ .to = .a, .from = .b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x79 LD A,C
        .func = LDr8,
        .args = .{ .targets = .{ .to = .a, .from = .c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x7A LD A,D
        .func = LDr8,
        .args = .{ .targets = .{ .to = .a, .from = .d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x7B LD A,E
        .func = LDr8,
        .args = .{ .targets = .{ .to = .a, .from = .e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x7C LD A,H
        .func = LDr8,
        .args = .{ .targets = .{ .to = .a, .from = .h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x7D LD A,L
        .func = LDr8,
        .args = .{ .targets = .{ .to = .a, .from = .l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x7E LD A,(HL)
        .func = LDr8HL,
        .args = .{ .target = regID.a },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x7F LD A,A
        .func = LDr8,
        .args = .{ .targets = .{ .to = .a, .from = .a } },
        .cycles = 1,
        .bytes = 1,
    },

    // Arithmetic
    .{ // 0x80 ADD A,B
        .func = ADDAr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x81 ADD A,C
        .func = ADDAr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x82 ADD A,D
        .func = ADDAr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x83 ADD A,E
        .func = ADDAr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x84 ADD A,H
        .func = ADDAr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x85 ADD A,L
        .func = ADDAr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x86 ADD A,(HL)
        .func = ADDAHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x87 ADD A,A
        .func = ADDAr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x88 ADC A,B
        .func = ADCAr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x89 ADC A,C
        .func = ADCAr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x8A ADC A,D
        .func = ADCAr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x8B ADC A,E
        .func = ADCAr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x8C ADC A,H
        .func = ADCAr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x8D ADC A,L
        .func = ADCAr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x8E ADC A,(HL)
        .func = ADCAHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x8F ADC A,A
        .func = ADCAr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },
    // Subtraction
    .{ // 0x90 SUB A,B
        .func = SUBAr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x91 SUB A,C
        .func = SUBAr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x92 SUB A,D
        .func = SUBAr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x93 SUB A,E
        .func = SUBAr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x94 SUB A,H
        .func = SUBAr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x95 SUB A,L
        .func = SUBAr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x96 SUB A,(HL)
        .func = SUBAHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x97 SUB A,A
        .func = SUBAr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x98 SBC A,B
        .func = SBCAr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x99 SBC A,C
        .func = SBCAr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x9A SBC A,D
        .func = SBCAr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x9B SBC A,E
        .func = SBCAr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x9C SBC A,H
        .func = SBCAr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x9D SBC A,L
        .func = SBCAr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0x9E SBC A,(HL)
        .func = SBCAHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0x9F SBC A,A
        .func = SBCAr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },

    // AND
    .{ // 0xA0 AND B
        .func = ANDr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xA1 AND C
        .func = ANDr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xA2 AND D
        .func = ANDr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xA3 AND E
        .func = ANDr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xA4 AND H
        .func = ANDr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xA5 AND L
        .func = ANDr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xA6 AND (HL)
        .func = ANDHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0xA7 AND A
        .func = ANDr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },

    // XOR
    .{ // 0xA8 XOR B
        .func = XORr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xA9 XOR C
        .func = XORr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xAA XOR D
        .func = XORr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xAB XOR E
        .func = XORr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xAC XOR H
        .func = XORr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xAD XOR L
        .func = XORr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xAE XOR (HL)
        .func = XORHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0xAF XOR A
        .func = XORr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },

    // OR
    .{ // 0xB0 OR B
        .func = ORr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xB1 OR C
        .func = ORr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xB2 OR D
        .func = ORr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xB3 OR E
        .func = ORr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xB4 OR H
        .func = ORr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xB5 OR L
        .func = ORr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xB6 OR (HL)
        .func = ORHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0xB7 OR A
        .func = ORr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },

    // CP
    .{ // 0xB8 CP B
        .func = CPAr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xB9 CP C
        .func = CPAr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xBA CP D
        .func = CPAr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xBB CP E
        .func = CPAr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xBC CP H
        .func = CPAr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xBD CP L
        .func = CPAr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xBE CP (HL)
        .func = CPAHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0xBF CP A
        .func = CPAr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },

    // Control & Misc
    .{ // 0xC0 RET NZ
        .func = RET,
        .args = .{ .flagConditions = .nz },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0xC1 POP BC
        .func = POP,
        .args = .{ .target = regID.b },
        .cycles = 3,
        .bytes = 1,
    },
    .{ // 0xC2 JP NZ
        .func = JP,
        .args = .{ .flagConditions = .nz },
        .cycles = 3,
        .bytes = 3,
    },
    .{ // 0xC3 JP
        .func = JP,
        .args = .{ .flagConditions = .none },
        .cycles = 3,
        .bytes = 3,
    },
    .{ // 0xC4 CALL NZ
        .func = CALLn16,
        .args = .{ .flagConditions = .nz },
        .cycles = 6,
        .bytes = 3,
    },
    .{ // 0xC5 PUSH BC
        .func = PUSH,
        .args = .{ .target = regID.b },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xC6 ADD A,n
        .func = ADDAn8,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0xC7 RST 0x00
        .func = RST,
        .args = .{ .where = 0x00 },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xC8 RET Z
        .func = RET,
        .args = .{ .flagConditions = .z },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0xC9 RET
        .func = RET,
        .args = .{ .flagConditions = .none },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xCA JP Z
        .func = JP,
        .args = .{ .flagConditions = .z },
        .cycles = 3,
        .bytes = 3,
    },
    .{ // 0xCB PREFIX CB
        .func = INVALID, // handled separately as CB-prefixed table
        .args = .{ .none = {} },
        .cycles = 0,
        .bytes = 1,
    },
    .{ // 0xCC CALL Z
        .func = CALLn16,
        .args = .{ .flagConditions = .z },
        .cycles = 6,
        .bytes = 3,
    },
    .{ // 0xCD CALL
        .func = CALLn16,
        .args = .{ .flagConditions = .none },
        .cycles = 6,
        .bytes = 3,
    },
    .{ // 0xCE ADC A,n
        .func = ADCAn8,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0xCF RST 0x08
        .func = RST,
        .args = .{ .where = 0x08 },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xD0 RET NC
        .func = RET,
        .args = .{ .flagConditions = .nc },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0xD1 POP DE
        .func = POP,
        .args = .{ .target = regID.d },
        .cycles = 3,
        .bytes = 1,
    },
    .{ // 0xD2 JP NC
        .func = JP,
        .args = .{ .flagConditions = .nc },
        .cycles = 3,
        .bytes = 3,
    },
    .{ // 0xD3 INVALID
        .func = INVALID,
        .args = .{ .none = {} },
        .cycles = 0,
        .bytes = 1,
    },
    .{ // 0xD4 CALL NC
        .func = CALLn16,
        .args = .{ .flagConditions = .nc },
        .cycles = 6,
        .bytes = 3,
    },
    .{ // 0xD5 PUSH DE
        .func = PUSH,
        .args = .{ .target = regID.d },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xD6 SUB n
        .func = SUBAn8,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0xD7 RST 0x10
        .func = RST,
        .args = .{ .where = 0x10 },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xD8 RET C
        .func = RET,
        .args = .{ .flagConditions = .c },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0xD9 RETI
        .func = RETI,
        .args = .{ .none = {} },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xDA JP C
        .func = JP,
        .args = .{ .flagConditions = .c },
        .cycles = 3,
        .bytes = 3,
    },
    .{ // 0xDB INVALID
        .func = INVALID,
        .args = .{ .none = {} },
        .cycles = 0,
        .bytes = 1,
    },
    .{ // 0xDC CALL C
        .func = CALLn16,
        .args = .{ .flagConditions = .c },
        .cycles = 6,
        .bytes = 3,
    },
    .{ // 0xDD INVALID
        .func = INVALID,
        .args = .{ .none = {} },
        .cycles = 0,
        .bytes = 1,
    },
    .{ // 0xDE SBC A,n
        .func = SBCAn8,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0xDF RST 0x18
        .func = RST,
        .args = .{ .where = 0x18 },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xE0 LDH (n),A
        .func = LDHn16A,
        .args = .{ .none = {} },
        .cycles = 3,
        .bytes = 2,
    },
    .{ // 0xE1 POP HL
        .func = POP,
        .args = .{ .target = regID.h },
        .cycles = 3,
        .bytes = 1,
    },
    .{ // 0xE2 LD (C),A
        .func = LDHCA,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0xE3 INVALID
        .func = INVALID,
        .args = .{ .none = {} },
        .cycles = 0,
        .bytes = 1,
    },
    .{ // 0xE4 INVALID
        .func = INVALID,
        .args = .{ .none = {} },
        .cycles = 0,
        .bytes = 1,
    },
    .{ // 0xE5 PUSH HL
        .func = PUSH,
        .args = .{ .target = regID.h },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xE6 AND n
        .func = ANDn8,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0xE7 RST 0x20
        .func = RST,
        .args = .{ .where = 0x20 },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xE8 ADD SP,n
        .func = ADDSPn8,
        .args = .{ .none = {} },
        .cycles = 4,
        .bytes = 2,
    },
    .{ // 0xE9 JP (HL)
        .func = JPHL,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xEA LD (nn),A
        .func = LDn16A,
        .args = .{ .none = {} },
        .cycles = 4,
        .bytes = 3,
    },
    .{ // 0xEB INVALID
        .func = INVALID,
        .args = .{ .none = {} },
        .cycles = 0,
        .bytes = 1,
    },
    .{ // 0xEC INVALID
        .func = INVALID,
        .args = .{ .none = {} },
        .cycles = 0,
        .bytes = 1,
    },
    .{ // 0xED INVALID
        .func = INVALID,
        .args = .{ .none = {} },
        .cycles = 0,
        .bytes = 1,
    },
    .{ // 0xEE XOR n
        .func = XORn8,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0xEF RST 0x28
        .func = RST,
        .args = .{ .where = 0x28 },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xF0 LDH A,(n)
        .func = LDHAn16,
        .args = .{ .none = {} },
        .cycles = 3,
        .bytes = 2,
    },
    .{ // 0xF1 POP AF
        .func = POP,
        .args = .{ .target = regID.a },
        .cycles = 3,
        .bytes = 1,
    },
    .{ // 0xF2 LD A,(C)
        .func = LDHAC,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0xF3 DI
        .func = DI,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xF4 INVALID
        .func = INVALID,
        .args = .{ .none = {} },
        .cycles = 0,
        .bytes = 1,
    },
    .{ // 0xF5 PUSH AF
        .func = PUSH,
        .args = .{ .target = regID.a },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xF6 OR n
        .func = ORn8,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0xF7 RST 0x30
        .func = RST,
        .args = .{ .where = 0x30 },
        .cycles = 4,
        .bytes = 1,
    },
    .{ // 0xF8 LD HL,SP+n
        .func = LDHLSPn8,
        .args = .{ .none = {} },
        .cycles = 3,
        .bytes = 2,
    },
    .{ // 0xF9 LD SP,HL
        .func = LDSPHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // 0xFA LD A,(nn)
        .func = LDAn16,
        .args = .{ .none = {} },
        .cycles = 4,
        .bytes = 3,
    },
    .{ // 0xFB EI
        .func = EI,
        .args = .{ .none = {} },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // 0xFC INVALID
        .func = INVALID,
        .args = .{ .none = {} },
        .cycles = 0,
        .bytes = 1,
    },
    .{ // 0xFD INVALID
        .func = INVALID,
        .args = .{ .none = {} },
        .cycles = 0,
        .bytes = 1,
    },
    .{ // 0xFE CP n
        .func = CPAn8,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 2,
    },
    .{ // 0xFF RST 0x38
        .func = RST,
        .args = .{ .where = 0x38 },
        .cycles = 4,
        .bytes = 1,
    },
};

pub const prefix_instrs = [256]Instr{
    .{ // RLC B
        .func = RLCr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RLC C
        .func = RLCr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RLC D
        .func = RLCr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RLC E
        .func = RLCr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RLC H
        .func = RLCr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RLC L
        .func = RLCr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RLC (HL)
        .func = RLCHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // RLC A
        .func = RLCr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // RRC B
        .func = RRCr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RRC C
        .func = RRCr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RRC D
        .func = RRCr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RRC E
        .func = RRCr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RRC H
        .func = RRCr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RRC L
        .func = RRCr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RRC (HL)
        .func = RRCHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // RRC A
        .func = RRCr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // RL B
        .func = RLr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RL C
        .func = RLr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RL D
        .func = RLr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RL E
        .func = RLr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RL H
        .func = RLr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RL L
        .func = RLr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RL (HL)
        .func = RLHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // RL A
        .func = RLr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RR B
        .func = RRr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RR C
        .func = RRr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RR D
        .func = RRr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RR E
        .func = RRr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RR H
        .func = RRr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RR L
        .func = RRr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RR (HL)
        .func = RRHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // RR A
        .func = RRr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // SLA B
        .func = SLAr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SLA C
        .func = SLAr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SLA D
        .func = SLAr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SLA E
        .func = SLAr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SLA H
        .func = SLAr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SLA L
        .func = SLAr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SLA (HL)
        .func = SLAHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // SLA A
        .func = SLAr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // SRA B
        .func = SRAr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SRA C
        .func = SRAr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SRA D
        .func = SRAr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SRA E
        .func = SRAr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SRA H
        .func = SRAr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SRA L
        .func = SRAr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SRA (HL)
        .func = SRAHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // SRA A
        .func = SRAr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // SWAP B
        .func = SWAPr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SWAP C
        .func = SWAPr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SWAP D
        .func = SWAPr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SWAP E
        .func = SWAPr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SWAP H
        .func = SWAPr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SWAP L
        .func = SWAPr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SWAP (HL)
        .func = SWAPHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // SWAP A
        .func = SWAPr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // SRL B
        .func = SRLr8,
        .args = .{ .target = regID.b },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SRL C
        .func = SRLr8,
        .args = .{ .target = regID.c },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SRL D
        .func = SRLr8,
        .args = .{ .target = regID.d },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SRL E
        .func = SRLr8,
        .args = .{ .target = regID.e },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SRL H
        .func = SRLr8,
        .args = .{ .target = regID.h },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SRL L
        .func = SRLr8,
        .args = .{ .target = regID.l },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SRL (HL)
        .func = SRLHL,
        .args = .{ .none = {} },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // SRL A
        .func = SRLr8,
        .args = .{ .target = regID.a },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 0,B
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 0,C
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 0,D
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 0,E
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 0,H
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 0,L
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 0,(HL)
        .func = BITTESTHL,
        .args = .{ .bit = 0 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // BIT 0,A
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // BIT 1,B
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 1,C
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 1,D
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 1,E
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 1,H
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 1,L
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 1,(HL)
        .func = BITTESTHL,
        .args = .{ .bit = 1 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // BIT 1,A
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // BIT 2,B
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 2,C
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 2,D
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 2,E
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 2,H
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 2,L
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 2,(HL)
        .func = BITTESTHL,
        .args = .{ .bit = 2 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // BIT 2,A
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // BIT 3,B
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 3,C
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 3,D
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 3,E
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 3,H
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 3,L
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 3,(HL)
        .func = BITTESTHL,
        .args = .{ .bit = 3 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // BIT 3,A
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 4,B
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 4,C
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 4,D
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 4,E
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 4,H
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 4,L
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 4,(HL)
        .func = BITTESTHL,
        .args = .{ .bit = 4 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // BIT 4,A
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // BIT 5,B
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 5,C
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 5,D
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 5,E
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 5,H
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 5,L
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 5,(HL)
        .func = BITTESTHL,
        .args = .{ .bit = 5 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // BIT 5,A
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // BIT 6,B
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 6,C
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 6,D
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 6,E
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 6,H
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 6,L
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 6,(HL)
        .func = BITTESTHL,
        .args = .{ .bit = 6 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // BIT 6,A
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // BIT 7,B
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 7,C
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 7,D
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 7,E
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 7,H
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 7,L
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // BIT 7,(HL)
        .func = BITTESTHL,
        .args = .{ .bit = 7 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // BIT 7,A
        .func = BITTESTr8,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 0,B
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 0,C
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 0,D
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 0,E
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 0,H
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 0,L
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 0,(HL)
        .func = RESHL,
        .args = .{ .bit = 0 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // RES 0,A
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // RES 1,B
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 1,C
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 1,D
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 1,E
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 1,H
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 1,L
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 1,(HL)
        .func = RESHL,
        .args = .{ .bit = 1 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // RES 1,A
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // RES 2,B
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 2,C
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 2,D
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 2,E
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 2,H
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 2,L
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 2,(HL)
        .func = RESHL,
        .args = .{ .bit = 2 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // RES 2,A
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // RES 3,B
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 3,C
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 3,D
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 3,E
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 3,H
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 3,L
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 3,(HL)
        .func = RESHL,
        .args = .{ .bit = 3 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // RES 3,A
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // RES 4,B
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 4,C
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 4,D
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 4,E
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 4,H
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 4,L
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 4,(HL)
        .func = RESHL,
        .args = .{ .bit = 4 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // RES 4,A
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // RES 5,B
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 5,C
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 5,D
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 5,E
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 5,H
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 5,L
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 5,(HL)
        .func = RESHL,
        .args = .{ .bit = 5 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // RES 5,A
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 6,B
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 6,C
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 6,D
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 6,E
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 6,H
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 6,L
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 6,(HL)
        .func = RESHL,
        .args = .{ .bit = 6 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // RES 6,A
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // RES 7,B
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 7,C
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 7,D
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 7,E
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 7,H
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 7,L
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // RES 7,(HL)
        .func = RESHL,
        .args = .{ .bit = 7 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // RES 7,A
        .func = RES,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // SET 0,B
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 0,C
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 0,D
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 0,E
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 0,H
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 0,L
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 0,(HL)
        .func = SETHL,
        .args = .{ .bit = 0 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // SET 0,A
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 0, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // SET 1,B
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 1,C
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 1,D
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 1,E
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 1,H
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 1,L
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 1,(HL)
        .func = SETHL,
        .args = .{ .bit = 1 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // SET 1,A
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 1, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // SET 2,B
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 2,C
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 2,D
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 2,E
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 2,H
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 2,L
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 2,(HL)
        .func = SETHL,
        .args = .{ .bit = 2 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // SET 2,A
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 2, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // SET 3,B
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 3,C
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 3,D
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 3,E
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 3,H
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 3,L
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 3,(HL)
        .func = SETHL,
        .args = .{ .bit = 3 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // SET 3,A
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 3, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // SET 4,B
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 4,C
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 4,D
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 4,E
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 4,H
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 4,L
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 4,(HL)
        .func = SETHL,
        .args = .{ .bit = 4 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // SET 4,A
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 4, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // SET 5,B
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 5,C
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 5,D
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 5,E
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 5,H
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 5,L
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 5,(HL)
        .func = SETHL,
        .args = .{ .bit = 5 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // SET 5,A
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 5, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // SET 6,B
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 6,C
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 6,D
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 6,E
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 6,H
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 6,L
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 6,(HL)
        .func = SETHL,
        .args = .{ .bit = 6 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // SET 6,A
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 6, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },

    .{ // SET 7,B
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.b } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 7,C
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.c } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 7,D
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.d } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 7,E
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.e } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 7,H
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.h } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 7,L
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.l } },
        .cycles = 1,
        .bytes = 1,
    },
    .{ // SET 7,(HL)
        .func = SETHL,
        .args = .{ .bit = 7 },
        .cycles = 2,
        .bytes = 1,
    },
    .{ // SET 7,A
        .func = SET,
        .args = .{ .bit_target = .{ .bit = 7, .target = regID.a } },
        .cycles = 1,
        .bytes = 1,
    },
};
// helpers
fn detectHalfCarry(target: anytype, b: anytype, sign: union(enum(u1)) { add, sub }) bool {
    var target_high_byte: ?u8 = null;
    var b_high_byte: ?u8 = null;
    var lower_byte_carried = false;

    if (@TypeOf(target) == u16 or @TypeOf(b) == u16) {
        // print("")
        if (@TypeOf(target) == u16) {
            target_high_byte = @intCast(target >> 8);
            lower_byte_carried = switch (sign) {
                .add => @addWithOverflow(@as(u8, @truncate(target)), @as(u8, @truncate(b)))[1] == 1,
                .sub => @subWithOverflow(@as(u8, @truncate(target)), @as(u8, @truncate(b)))[1] == 1,
            };
            if (lower_byte_carried) {
                // print("Lower byte carried\n", .{});
                if (target_high_byte.? & 0xF == 0xF) {
                    // print("detected 0xF after carry\n", .{});
                    return true;
                }
            }
        }
        if (@TypeOf(b) == u16) {
            b_high_byte = @intCast(b >> 8);
            lower_byte_carried = switch (sign) {
                .add => @addWithOverflow(@as(u8, @truncate(target)), @as(u8, @truncate(b)))[1] == 1,
                .sub => @subWithOverflow(@as(u8, @truncate(target)), @as(u8, @truncate(b)))[1] == 1,
            };
            if (lower_byte_carried) {
                // print("Lower byte carried\n", .{});
                if (b_high_byte.? & 0xF == 0xF) {
                    // print("detected 0xF after carry\n", .{});
                    return true;
                }
            }
        }
    }
    return switch (sign) {
        .add => @addWithOverflow(@as(u4, @truncate(target_high_byte orelse target)), @as(u4, @truncate(b_high_byte orelse b)))[1] == 1,
        .sub => @subWithOverflow(@as(u4, @truncate(target_high_byte orelse target)), @as(u4, @truncate(b_high_byte orelse b)))[1] == 1,
    };
}
/// Allows for mixed sign arithmetic i.e. i8 + u16 with overflow
fn mixedSignArithmetic(target_value: anytype, signed_value: anytype, treat_as: type) struct { @TypeOf(target_value), u1 } {
    const res: struct { treat_as, u1 } = @addWithOverflow(@as(treat_as, target_value), signed_value);
    // print("result: 0x{X} (0b{b}), carry: 0b{b}\n", .{res[0], res[0], res[1]});
    var val: @TypeOf(target_value) = undefined;
    switch (res[0] >= 0) {
        true => val = @intCast(res[0]),
        false => {
            // distance from the min negative value is how much we overflowed by
            const dist = std.math.minInt(treat_as) - res[0];
            val = @intCast(@abs(dist));
        },
    }
    return .{ val, res[1] };
}
const CPU = @import("cpu.zig");
const regID = CPU.regID;

const std = @import("std");
const print = std.debug.print;
