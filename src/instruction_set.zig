const InstrFn = fn (*CPU, InstrArgs) u8;
pub const InstrArgs = union(enum) { none: void, target: regID, bit: u3, bit_target: struct { bit: u3, target: regID }, flagConditions: Condition, targets: struct { to: regID, from: regID }, hl_mod: i2, where: u16 };
pub const Condition = union(enum) { none, z, c, nz, nc };
fn INVALID(cpu: *CPU, _: InstrArgs) u8 {
    // This instruction should never be called
    _ = cpu;
    return 0;
}
fn NOP(cpu: *CPU, _: InstrArgs) u8 {
    cpu.pushToExecutionChain("NOP", .{});
    cpu.pc += 1;
    return 1;
}
fn STOP(cpu: *CPU, _: InstrArgs) u8 {
    cpu.pushToExecutionChain("STOP", .{});
    cpu.halted = true;
    cpu.pc += 2;
    return 0;
}
fn HALT(cpu: *CPU, _: InstrArgs) u8 {
    const debug = "HALT";
    print(debug ++ "\n", .{});
    cpu.pushToExecutionChain(debug, .{});
    switch(cpu.halted) {
        false => { // first entry
            cpu.halted = true; 
            return 1;
        },
        true => { // still halted, we have returned 
            switch (cpu.bus.handler.ime) {
                true => {
                    if (cpu.bus.handler.iE.* & cpu.bus.handler.iF.* != 0) { // interrupt pending
                        // cpu.pc += 1;
                        cpu.bus.handler.handle(cpu);
                        cpu.halted = false;
                    }
                },
                false => {
                    if (cpu.bus.handler.iE.* & cpu.bus.handler.iF.* != 0) { // interrupt pending
                        cpu.halt_bug = true;
                        cpu.halted = false;
                        cpu.pc += 1;
                    } else {
                        cpu.halted = false;
                        cpu.pc += 1;
                    }
                }
            }
        }   
    }
    return 1;
}
// LOAD
// 8 bit
//
fn LD8(cpu: *CPU, args: InstrArgs) u8 { // LD r8, n8
    const n: u8 = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("LD r8, n8 | {any} <-- 0x{X}", .{ @as(regID, args.target), n });
    cpu.set_byte(args.target, n);
    cpu.pc += 2;
    return 2;
}
fn LDr8(cpu: *CPU, args: InstrArgs) u8 { // LD r8, r8
    cpu.pushToExecutionChain("LDr8 | {any} --> {any}", .{ args.targets.from, args.targets.to });
    cpu.set_byte(args.targets.to, cpu.get_byte(args.targets.from));
    cpu.pc += 1;
    return 1;
}
fn LDr8HL(cpu: *CPU, args: InstrArgs) u8 { // LD r8, [HL] TODO: trying to encode ld [hl], [hl] instead yields the halt instruction:
    const hl = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(hl);
    cpu.pushToExecutionChain("LD r8, [HL] | {any} <-- 0x{X}", .{args.target, value});
    cpu.set_byte(args.target, value);
    cpu.pc += 1;
    return 2;
}
fn LDHLIA(cpu: *CPU, _: InstrArgs) u8 { // LD [HLI],A
    const hl = cpu.get_word(regID.h);
    const value = cpu.get_byte(regID.a);
    cpu.pushToExecutionChain("LD [HL+], A |  mem@0x{X} <-- 0x{X}", .{ hl, value });
    cpu.bus.writeByte(hl, value);
    cpu.set_word(regID.h, @addWithOverflow(hl, 1)[0]);
    cpu.pc += 1;
    return 2;
}
fn LDHLDA(cpu: *CPU, _: InstrArgs) u8 { // LD [HLD], A
    const hl = cpu.get_word(regID.h);
    const value = cpu.get_byte(regID.a);
    cpu.pushToExecutionChain("LD [HL-], A |  mem@0x{X} <-- 0x{X}", .{ hl, value });
    cpu.bus.writeByte(hl, value);
    cpu.set_word(regID.h, @subWithOverflow(hl, 1)[0]);
    cpu.pc += 1;
    return 2;
}
fn LDHCA(cpu: *CPU, _: InstrArgs) u8 {
    const c = cpu.get_byte(regID.c);
    const a = cpu.get_byte(regID.a);
    const mem_place = 0xFF00 + @as(u16, c);
    cpu.pushToExecutionChain("LDH [C], A | mem@0x{X} <-- 0x{X}", .{ mem_place, a });
    cpu.bus.writeByte(mem_place, a);
    cpu.pc += 1;
    return 2;
}
fn LDHAC(cpu: *CPU, _: InstrArgs) u8 { // Load value in register A from the byte at address $FF00+c
    const c = cpu.get_byte(regID.c);
    const byte = cpu.bus.readByte(0xFF00 + @as(u16, c));
    cpu.pushToExecutionChain("LDH A, [C] | A <-- 0x{X}", .{byte});
    cpu.set_byte(regID.a, byte);
    cpu.pc += 1;
    return 2;
}
// 16 bit
//
fn LD16(cpu: *CPU, args: InstrArgs) u8 { // LD r16, n16
    const n: u16 = @as(u16, cpu.bus.readByte(cpu.pc + 2)) << 8 | cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("LDr16, n16 | {any} <-- Ox{X}", .{ args.target, n });
    cpu.set_word(args.target, n);
    cpu.pc += 3;
    return 3;
}
fn LDAHL(cpu: *CPU, args: InstrArgs) u8 { // LD A, HL
    const mem_place = cpu.get_word(regID.h);
    const value = cpu.get_byte(regID.a);
    cpu.pushToExecutionChain("LD A, [HL], mem@hl:0x{X} --> to A", .{mem_place});
    cpu.bus.writeByte(mem_place, value);
    if (args.hl_mod != 0) cpu.set_word(.h, @intCast(@addWithOverflow(args.hl_mod, @as(i17, mem_place))[0]));
    cpu.pc += 1;
    return 2;
}
fn LDSP16(cpu: *CPU, _: InstrArgs) u8 { // LD SP, n16
    const n: u16 = @as(u16, cpu.bus.readByte(cpu.pc + 2)) << 8 | cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("LDSP16 | n (0x{X})", .{n});
    cpu.sp = n;
    // print("after op: sp: {d}\n", .{cpu.sp});
    cpu.pc += 3;
    return 3;
}
fn LDSPHL(cpu: *CPU, _: InstrArgs) u8 { // LD SP,HL
    cpu.pushToExecutionChain("LDSPHL", .{});
    cpu.sp = cpu.get_word(.h);
    // print("after op: sp: {d}\n", .{cpu.sp});
    cpu.pc += 1;
    return 2;
}
fn LDHLSPn8(cpu: *CPU, _: InstrArgs) u8 { // LD HL,SP+e8
    const n: i8 = @bitCast(cpu.bus.readByte(cpu.pc + 1));
    cpu.pushToExecutionChain("LDHLSPn8 | n (0x{X})", .{n});
    // cpu.pushToExecutionChain(", .{ cpu.pc + 1, cpu.bus.readByte(cpu.pc + 1) });
    cpu.set_word(.h, @intCast(@addWithOverflow(@as(i17, cpu.sp), n)[0]));
    // print("after op: sp: {d}\n", .{cpu.sp});
    cpu.pc += 2;
    return 3;
}
fn LDn16SP(cpu: *CPU, _: InstrArgs) u8 { // Store SP & $FF at address n16 and SP >> 8 at address n16 + 1.
    const high: u8 = @truncate(cpu.sp >> 8);
    const low: u8 = @truncate(cpu.sp);
    const mem_place: u16 = @as(u16, cpu.bus.readByte(cpu.pc + 2)) << 8 | cpu.bus.readByte(cpu.pc + 1);
    cpu.bus.writeByte(mem_place, low);
    cpu.bus.writeByte(mem_place + 1, high);
    cpu.pc += 3;
    cpu.pushToExecutionChain("LDn16SP | SP:0x{X} --> mem@0x{X}{X}", .{ cpu.sp, high, low });
    return 5;
}
fn LDAn16(cpu: *CPU, _: InstrArgs) u8 {
    const memory_place = @as(u16, cpu.bus.readByte(cpu.pc + 2)) << 8 | cpu.bus.readByte(cpu.pc + 1);
    const n = cpu.bus.readByte(memory_place);
    cpu.pushToExecutionChain("LDAn16 | n: Ox{X} --> A", .{n});
    cpu.set_byte(regID.a, n);
    cpu.pc += 3;
    return 4;
}
fn LDHAn16(cpu: *CPU, _: InstrArgs) u8 { // same as above, provided the address is between $FF00 and $FFFF.
    // const zone = tracy.beginZone(@src(), .{ .name = "LDHAn16" });
    // defer zone.end();
    const memory_place = 0xFF00 + @as(u16, cpu.bus.readByte(cpu.pc + 1));
    const n = cpu.bus.readByte(memory_place);
    cpu.pushToExecutionChain("LDHAn16 | n: 0x{X} --> A", .{n});
    cpu.set_byte(regID.a, n);
    cpu.pc += 2;
    return 3;
}
fn LDn16A(cpu: *CPU, _: InstrArgs) u8 { // Store value in register A into the byte at address n16.
    // const zone = tracy.beginZone(@src(), .{ .name = "LDn16A" });
    // defer zone.end();
    const memory_place = @as(u16, cpu.bus.readByte(cpu.pc + 2)) << 8 | cpu.bus.readByte(cpu.pc + 1);
    const n = cpu.get_byte(regID.a);
    cpu.bus.writeByte(memory_place, n);
    cpu.pushToExecutionChain("LDn16A | n: Ox{X} --> memplace@{X}", .{ n, memory_place });
    cpu.pc += 3;
    return 4;
}
fn LDHn16A(cpu: *CPU, _: InstrArgs) u8 { // same as above, provided the address is between $FF00 and $FFFF.
    // const zone = tracy.beginZone(@src(), .{ .name = "LDHn16A" });
    // defer zone.end();
    const memory_place = 0xFF00 + @as(u16, cpu.bus.readByte(cpu.pc + 1));
    const n = cpu.get_byte(regID.a);
    cpu.bus.writeByte(memory_place, n);
    cpu.pushToExecutionChain("LDHn16A | n: 0x{X} --> memplace@0x{X}", .{ n, memory_place });
    cpu.pc += 2;
    return 3;
}
fn LDAr16(cpu: *CPU, args: InstrArgs) u8 { // Load value in register A from the byte pointed to by register r16.
    // const zone = tracy.beginZone(@src(), .{ .name = "LDAr16" });
    // defer zone.end();
    const memory_place = cpu.get_word(args.target);
    const n = cpu.bus.readByte(memory_place);
    cpu.pushToExecutionChain("LDAr16 | n: 0x{X} --> A", .{n});
    cpu.set_byte(regID.a, n);
    cpu.pc += 1;
    return 2;
}
fn LDr16A(cpu: *CPU, args: InstrArgs) u8 { //  Store value in register A into the byte pointed to by register r16.
    // const zone = tracy.beginZone(@src(), .{ .name = "LDr16A" });
    // defer zone.end();
    const memory_place = cpu.get_word(args.target);
    const n = cpu.get_byte(regID.a);
    cpu.bus.writeByte(memory_place, n);
    cpu.pushToExecutionChain("LDr16A | n: 0x{X} --> memplace@0x{X}", .{ n, memory_place });
    cpu.pc += 1;
    return 2;
}
fn LDHL8(cpu: *CPU, _: InstrArgs) u8 { // LD[HL], n8
    const hl = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("LDHL8 | hl:0x{X}, value:0x{x}, mem@hl: 0x{x}", .{ hl, value, cpu.bus.readByte(hl) });
    cpu.bus.writeByte(hl, value);
    cpu.pc += 2;
    return 3;
}
fn LDHLr8(cpu: *CPU, args: InstrArgs) u8 { // LD[HL],r8
    const hl = cpu.get_word(regID.h);
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("LDHLR | mem@0x{X}: 0x{X} --> 0x{X}", .{ hl, cpu.bus.readByte(hl), value });
    cpu.bus.writeByte(hl, value);
    cpu.pc += 1;
    return 2;
}
// ALU & ARITHMETIC
// 8 bit
//
fn INCr8(cpu: *CPU, args: InstrArgs) u8 {
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("INCr8 | target: {any}", .{args.target});
    cpu.set_byte(args.target, @addWithOverflow(value, 1)[0]);
    const h = (value & 0xF + 1) & 0x10 == 0x10; // half carry conditions
    const z = cpu.get_byte(args.target) == 0;
    const s = false;
    const c = cpu.f.cFlag();
    cpu.f.write(z, c, h, s);
    cpu.pc += 1;
    return 1;
}
fn DECr8(cpu: *CPU, args: InstrArgs) u8 {
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("DECr8 | target: {any}", .{args.target});
    const res = @subWithOverflow(value, 1)[0];
    cpu.set_byte(args.target, res);
    const h = (value & 0xF) == 0x00; // half carry conditions
    const z = res == 0;
    const s = true;
    const c = cpu.f.cFlag();
    cpu.f.write(z, c, h, s);
    cpu.pc += 1;
    return 1;
}
fn ORr8(cpu: *CPU, args: InstrArgs) u8 {
    cpu.pushToExecutionChain("ORr8 | target {any}", .{args.target});
    const a = cpu.get_byte(.a);
    cpu.set_byte(.a, a | cpu.get_byte(args.target));
    cpu.f.write(a == 0, false, false, false);
    cpu.pc += 1;
    return 1;
}
fn ORn8(cpu: *CPU, _: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
    // defer zone.end();
    const n = cpu.bus.readByte(cpu.pc);
    cpu.pushToExecutionChain("ORn8 | n: {any}", .{n});
    const a = cpu.get_byte(.a);
    cpu.set_byte(.a, a | n);
    cpu.f.write(a == 0, false, true, false);
    cpu.pc += 2;
    return 2;
}
fn ORHL(cpu: *CPU, _: InstrArgs) u8 {
    cpu.pushToExecutionChain("ORHL", .{});
    const a = cpu.get_byte(.a);
    const value = cpu.bus.readByte(cpu.get_word(.h));
    cpu.set_byte(.a, a | value);
    cpu.f.write(a == 0, false, false, false);
    cpu.pc += 1;
    return 2;
}
fn XORr8(cpu: *CPU, args: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
    // defer zone.end();
    cpu.pushToExecutionChain("XORr8 | target {any}", .{args.target});
    const a = cpu.get_byte(.a);
    cpu.set_byte(.a, a ^ cpu.get_byte(args.target));
    cpu.f.write(a == 0, false, false, false);
    cpu.pc += 1;
    return 1;
}
fn XORn8(cpu: *CPU, _: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
    // defer zone.end();
    const value = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("XORn8", .{});
    const a = cpu.get_byte(.a);
    cpu.set_byte(.a, a ^ value);
    cpu.f.write(a == 0, false, false, false);
    cpu.pc += 2;
    return 2;
}
fn XORHL(cpu: *CPU, _: InstrArgs) u8 {
    cpu.pushToExecutionChain("XORHL", .{});
    const a = cpu.get_byte(.a);
    const value = cpu.bus.readByte(cpu.get_word(.h));
    cpu.set_byte(.a, a ^ value);
    cpu.f.write(a == 0, false, false, false);
    cpu.pc += 1;
    return 2;
}
fn ANDn8(cpu: *CPU, _: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
    // defer zone.end();
    const n = cpu.bus.readByte(cpu.pc);
    cpu.pushToExecutionChain("ANDn8 | n: {any}", .{n});
    const a = cpu.get_byte(.a);
    cpu.set_byte(.a, a & n);
    cpu.f.write(a == 0, false, true, false);
    cpu.pc += 2;
    return 2;
}
fn ANDHL(cpu: *CPU, _: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
    // defer zone.end();
    const reg = cpu.bus.readByte(cpu.get_word(.h));
    cpu.pushToExecutionChain("ANDr8 | A & HL", .{});
    const a = cpu.get_byte(.a);
    cpu.set_byte(.a, a & reg);
    cpu.f.write(a == 0, false, true, false);
    cpu.pc += 1;
    return 2;
}
fn ANDr8(cpu: *CPU, args: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "XORA" });
    // defer zone.end();
    const reg = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("ANDr8 | A & {any}", .{args.target});
    const a = cpu.get_byte(.a);
    cpu.set_byte(.a, a & reg);
    cpu.f.write(a == 0, false, true, false);
    cpu.pc += 1;
    return 1;
}
fn ADDAr8(cpu: *CPU, args: InstrArgs) u8 {
    // const zone = tracy.beginZone(@src(), .{ .name = "ADDAr8" });
    // defer zone.end();
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("ADDAr8 | target: {any}, value: {d}", .{ args.target, value });
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @addWithOverflow(a, value);
    const s = false;
    const c = res[1] == 1;
    const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 1;
}
fn ADDAn8(cpu: *CPU, _: InstrArgs) u8 { //
    // const zone = tracy.beginZone(@src(), .{ .name = "ADDAr8" });
    // defer zone.end();
    const value = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("ADDAn8 | A + 0x{X}", .{value});
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @addWithOverflow(a, value);
    const s = false;
    const c = res[1] == 1;
    const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 2;
    return 2;
}
fn ADDSPn8(cpu: *CPU, _: InstrArgs) u8 {
    const value: i8 = @bitCast(cpu.bus.readByte(cpu.pc + 1));
    cpu.pushToExecutionChain("ADDAn8 | SP + 0x{X}", .{value});
    const res: struct { i17, u1 } = @addWithOverflow(@as(i17, cpu.sp), value);
    const val: u8 = @intCast(res[0]);
    const s = false;
    const c = res[1] == 1;
    const h = (val & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = false;
    cpu.f.write(z, c, h, s);
    cpu.sp = val;
    cpu.pc += 2;
    return 4;
}
fn ADCAr8(cpu: *CPU, args: InstrArgs) u8 { // add a to a register, plus the carry
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("ADCAr8 | target: {any}, value: {d}", .{ args.target, value });
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @addWithOverflow(@intFromBool(cpu.f.cFlag()), @addWithOverflow(a, value)[0]);
    const s = false;
    const c = res[1] == 1;
    const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 1;
}
fn ADCAn8(cpu: *CPU, _: InstrArgs) u8 { // add a to a register, plus the carry
    const value = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("ADCAn8 | value: {d}", .{value});
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @addWithOverflow(@intFromBool(cpu.f.cFlag()), @addWithOverflow(a, value)[0]);
    const s = false;
    const c = res[1] == 1;
    const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 2;
    return 2;
}
fn ADCAHL(cpu: *CPU, _: InstrArgs) u8 {
    const value = cpu.bus.readByte(cpu.get_word(.h));
    cpu.pushToExecutionChain("ADCAHL | mem@hl: {d}", .{value});
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @addWithOverflow(@intFromBool(cpu.f.cFlag()), @addWithOverflow(a, value)[0]);
    const s = false;
    const c = res[1] == 1;
    const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 2;
}
fn SUBAr8(cpu: *CPU, args: InstrArgs) u8 {
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("SUBA | target: {any}, value: {d}", .{ args.target, value });
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @subWithOverflow(a, value);
    const c = value > a;
    const s = true;
    const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 1;
}
fn SUBAn8(cpu: *CPU, _: InstrArgs) u8 {
    const value = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("SUBAn8 | value: {d}", .{value});
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @subWithOverflow(a, value);
    const c = value > a;
    const s = true;
    const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 2;
    return 2;
}
fn SUBAHL(cpu: *CPU, _: InstrArgs) u8 {
    const mem_place = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(mem_place);
    cpu.pushToExecutionChain("SUBAHL | A - mem@0x{X}: value: {d}", .{ mem_place, value });
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @subWithOverflow(a, value);
    const c = value > a;
    const s = true;
    const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 2;
}
fn SBCAr8(cpu: *CPU, args: InstrArgs) u8 {
    const value = cpu.get_byte(args.target);
    cpu.pushToExecutionChain("SBCAr8 | target: {any}, value: {d}", .{ args.target, value });
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @subWithOverflow(@subWithOverflow(a, value)[0], @intFromBool(cpu.f.cFlag()));
    const s = true;
    const c = res[1] == 1;
    const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 1;
}
fn SBCAn8(cpu: *CPU, _: InstrArgs) u8 {
    const value = cpu.bus.readByte(cpu.pc + 1);
    cpu.pushToExecutionChain("SBCAn8 | value: {d}", .{value});
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @subWithOverflow(@subWithOverflow(a, value)[0], @intFromBool(cpu.f.cFlag()));
    const s = true;
    const c = res[1] == 1;
    const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 2;
    return 2;
}
fn SBCAHL(cpu: *CPU, _: InstrArgs) u8 {
    const value = cpu.bus.readByte(cpu.get_word(.h));
    cpu.pushToExecutionChain("SBCAHL | mem@hl: {d}", .{value});
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @subWithOverflow(@subWithOverflow(a, value)[0], @intFromBool(cpu.f.cFlag()));
    const s = true;
    const c = res[1] == 1;
    const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 2;
}
fn ADDAHL(cpu: *CPU, _: InstrArgs) u8 {
    const mem_place = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(mem_place);
    cpu.pushToExecutionChain("ADDAHL | A + mem@0x{X}: value: {d}", .{ mem_place, value });
    const a = cpu.get_byte(regID.a);
    const res: struct { u8, u1 } = @addWithOverflow(a, value);
    const s = false;
    const c = res[1] == 1;
    const h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
    const z = res[0] == 0;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(regID.a, res[0]);
    cpu.pc += 1;
    return 2;
}
fn DAA(cpu: *CPU, _: InstrArgs) u8 {
    var a = cpu.get_byte(.a);
    var offset: u8 = 0;
    if ((!cpu.f.sFlag() and a & 0xF > 0x9) or cpu.f.hFlag()) {
        offset |= 0x6;
    }
    if ((!cpu.f.sFlag() and a & 0xFF > 0x90) or cpu.f.cFlag()) {
        offset |= 0x60;
    }

    if (cpu.f.sFlag()) {
        a = @subWithOverflow(a, offset)[0];
    } else {
        a = @addWithOverflow(a, offset)[0];
    }

    const z = a == 0;
    const c = a > 0x99;
    const s = cpu.f.sFlag();
    const h = false;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(.a, a);
    cpu.pc += 1;
    return 1;
}
// 16 bit
fn INCr16(cpu: *CPU, args: InstrArgs) u8 {
    const value = cpu.get_word(args.target);
    const res = @addWithOverflow(value, 1)[0];
    cpu.pushToExecutionChain("INCr16 | target: {any}, 0x{X} + 1 = 0x{X}", .{ args.target, value, res });
    cpu.set_word(args.target, res);
    cpu.pc += 1;
    return 2;
}
fn INCSP(cpu: *CPU, _: InstrArgs) u8 {
    const value = cpu.sp;
    const res = @addWithOverflow(value, 1)[0];
    cpu.pushToExecutionChain("INCSP | 0x{X} + 1 = 0x{X}", .{ value, res });
    cpu.sp = res;
    cpu.pc += 1;
    return 2;
}
fn INCHL(cpu: *CPU, _: InstrArgs) u8 { // increment the value of the byte pointed to by hl
    const mem_place = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(mem_place);
    const res = @addWithOverflow(value, 1)[0];
    cpu.pushToExecutionChain("INCHL | mem@hl: 0x{X} + 1 = 0x{X}", .{ value, res });
    cpu.pc += 1;
    return 2;
}
fn DECr16(cpu: *CPU, args: InstrArgs) u8 { // decrement any 16 bit register;
    const value = cpu.get_word(args.target);
    cpu.pushToExecutionChain("DECr16 | target: {any}", .{args.target});
    cpu.set_word(args.target, @subWithOverflow(value, 1)[0]);
    cpu.pc += 1;
    return 2;
}
fn DECSP(cpu: *CPU, args: InstrArgs) u8 { // decrement the stack pointer
    const value = cpu.sp;
    cpu.pushToExecutionChain("DECSP | target: {any}", .{args.target});
    cpu.set_word(args.target, @subWithOverflow(value, 1)[0]);
    cpu.pc += 1;
    return 2;
}
fn DECHL(cpu: *CPU, _: InstrArgs) u8 { // decrement the value of the byte pointed to by hl
    const mem_place = cpu.get_word(regID.h);
    const value = cpu.bus.readByte(mem_place);
    const res = @subWithOverflow(value, 1)[0];
    cpu.pushToExecutionChain("DECHL | mem@hl: 0x{X} - 1 = 0x{X}", .{ value, res });
    cpu.pc += 1;
    return 2;
}
fn ADDHLr16(cpu: *CPU, args: InstrArgs) u8 {
    const hl = cpu.get_word(regID.h);
    const value = cpu.get_word(args.target);
    cpu.pushToExecutionChain("ADDHLr16 | {any} + hl, {d} + {d}", .{ args.target, value, hl });
    const res: u16 = @addWithOverflow(hl, value)[0];
    const s = false;
    const h = (((hl + value) >> 8) & 0xF) & 0x10 == 0x10; // half carry conditions
    const c = (((hl + value) >> 12) & 0xF) & 0x10 == 0x10;
    const z = cpu.f.zFlag();
    cpu.f.write(z, c, h, s);
    cpu.set_word(regID.h, res);
    cpu.pc += 1;
    return 2;
}
fn ADDHLSP(cpu: *CPU, args: InstrArgs) u8 {
    const hl = cpu.get_word(regID.h);
    const value = cpu.sp;
    cpu.pushToExecutionChain("ADDHLSP | {any} + hl, {d} + {d}", .{ args.target, value, hl });
    const res: u16 = @addWithOverflow(hl, value)[0];
    const s = false;
    const h = (((hl + value) >> 8) & 0xF) & 0x10 == 0x10; // half carry conditions
    const c = (((hl + value) >> 12) & 0xF) & 0x10 == 0x10;
    const z = cpu.f.zFlag();
    cpu.f.write(z, c, h, s);
    cpu.set_word(regID.h, res);
    cpu.pc += 1;
    return 2;
}
// MISC
fn SCF(cpu: *CPU, _: InstrArgs) u8 { // set carry flag
    cpu.pushToExecutionChain("SCF", .{});
    cpu.f.write(cpu.f.zFlag(), true, false, false);
    cpu.pc += 1;
    return 1;
}
fn CCF(cpu: *CPU, _: InstrArgs) u8 { // complement carry flag
    cpu.pushToExecutionChain("SCF", .{});
    cpu.f.write(cpu.f.zFlag(), !cpu.f.cFlag(), false, false);
    cpu.pc += 1;
    return 1;
}
fn CPL(cpu: *CPU, _: InstrArgs) u8 { // sets the value in register A to its complement
    cpu.pushToExecutionChain("CPL", .{});
    cpu.set_byte(.a, cpu.get_byte(.a) ^ 0xFF);
    cpu.f.write(cpu.f.zFlag(), cpu.f.cFlag(), true, true);
    cpu.pc += 1;
    return 1;
}
fn EI(cpu: *CPU, _: InstrArgs) u8 {
    cpu.pushToExecutionChain("EI", .{});
    print("EI!\n\n pc = 0x{X}\n", .{cpu.pc});
    cpu.pc += 1;
    return 1;
}
fn DI(cpu: *CPU, _: InstrArgs) u8 {
    // print("DI!\n\n", .{});
    const prior = cpu.bus.handler.ime;
    const debug = "DI | ime prior: {any}, ime post op: {any}";
    print(debug ++ "\n", .{prior, cpu.bus.handler.ime});
    cpu.bus.handler.ime = false;
    cpu.pushToExecutionChain(debug, .{prior, cpu.bus.handler.ime});
    cpu.pc += 1;
    return 1;
}
fn PUSH(cpu: *CPU, args: InstrArgs) u8 {
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
fn POP(cpu: *CPU, args: InstrArgs) u8 {
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
fn CPAn8(cpu: *CPU, _: InstrArgs) u8 {
    const n = cpu.bus.readByte(cpu.pc + 1);
    const reg = cpu.get_byte(regID.a);
    const z = reg == n;
    const s = true;
    const h = (reg & 0xF) < (n & 0xF); // half carry conditions
    const c = reg < n;
    cpu.f.write(z, c, h, s);
    cpu.pushToExecutionChain("CPAn8 | := reg.A, n := {d}, {d}", .{ reg, n });
    cpu.pc += 2;
    return 2;
}
fn CPAr8(cpu: *CPU, args: InstrArgs) u8 {
    const n = cpu.get_byte(args.target);
    // print("CPAr8, target = {any}\n", .{args.target});
    const reg = cpu.get_byte(regID.a);
    // const res = @subWithOverflow(reg, n);
    const z = reg == n;
    const s = true;
    const h = (reg & 0xF) < (n & 0xF); // half carry conditions
    const c = reg < n;
    cpu.f.write(z, c, h, s);
    cpu.pushToExecutionChain("CPAr8 | reg.A, {any} := {d}, {d}", .{ args.target, reg, n });
    cpu.pc += 1;
    return 1;
}
fn CPAHL(cpu: *CPU, _: InstrArgs) u8 {
    const hl = cpu.get_word(regID.h);
    const reg = cpu.get_byte(regID.a);
    const byte = cpu.bus.readByte(hl);
    // print("CPAHL, compare mem_place: 0x{X} ({d}) to A:{d}\n", .{ hl, cpu.bus.readByte(hl), reg });
    const z = reg == byte;
    const s = true;
    const h = (reg & 0xF) < (byte & 0xF); // half carry conditions
    const c = reg < byte;
    cpu.f.write(z, c, h, s);
    cpu.pushToExecutionChain("CPAHL | reg.A, mem[X.{X:04}] := {d}, {d}", .{ hl, reg, byte });
    cpu.pc += 1;
    return 2;
}
// ROTATES & SHIFTS
fn RRA(cpu: *CPU, _: InstrArgs) u8 { // C -> [7 -> 0] -> C into A
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
fn RRCA(cpu: *CPU, _: InstrArgs) u8 {
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
fn RLA(cpu: *CPU, _: InstrArgs) u8 { // C <- [7 <- 0] <- C
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
fn RLCA(cpu: *CPU, _: InstrArgs) u8 { //Rotate register A left.
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
fn RLCr8(cpu: *CPU, args: InstrArgs) u8 { //Rotate register left. C <- [7 <- 0] <- [7]
    cpu.pushToExecutionChain("RLCr8 | regID.a << 1", .{});
    const reg = cpu.get_byte(args.target);
    const rotated = reg << 1;
    // const msb = reg >> 7;
    // print("RLC msb: {d}, rotated: 0b{b}\n", .{msb, rotated});
    const c = (reg >> 7) == 1;
    const z = rotated == 0;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(args.target, rotated | (reg >> 7));
    cpu.pc += 1;
    return 2;
}
fn RLCHL(cpu: *CPU, _: InstrArgs) u8 { //Rotate byte pointed to by hl left. C <- [7 <- 0] <- [7]
    cpu.pushToExecutionChain("RLCHL | regID.a << 1", .{});
    const mem_address = cpu.get_word(.h);
    const byte = cpu.bus.readByte(mem_address);
    const rotated = (byte << 1) | (byte >> 7);
    const c = (byte >> 7) == 1;
    const z = rotated == 0;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.bus.writeByte(mem_address, rotated);
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
fn RLHL(cpu: *CPU, _: InstrArgs) u8 { // C <- [7 <- 0] <- C Rotate bits in register r8 left through carry.
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
    return 2;
}
fn RRCr8(cpu: *CPU, args: InstrArgs) u8 { //Rotate register right. 0 -> [7 -> 0] -> C
    cpu.pushToExecutionChain("RRCr8 | target {any} << 1", .{args.target});
    const reg = cpu.get_byte(args.target);
    const rotated = reg >> 1;
    const c = (reg & 1) == 1;
    const z = rotated == 0;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.set_byte(args.target, rotated | ((reg & 1) << 7));
    cpu.pc += 1;
    return 2;
}
fn RRCHL(cpu: *CPU, _: InstrArgs) u8 { //Rotate byte pointed to by hl right. 0 -> [7 -> 0] -> C
    cpu.pushToExecutionChain("RRCHL | HL >> 1", .{});
    const mem_address = cpu.get_word(.h);
    const byte = cpu.bus.readByte(mem_address);
    const rotated = byte >> 1;
    const c = (byte & 1) == 1;
    const z = rotated == 0;
    const h = false;
    const s = false;
    cpu.f.write(z, c, h, s);
    cpu.bus.writeByte(mem_address, rotated | (byte & 1) << 7);
    cpu.pc += 1;
    return 4;
}
fn RRr8(cpu: *CPU, args: InstrArgs) u8 { // C -> [7 -> 0] -> C Rotate bits in register r8 left through carry.
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
fn RRHL(cpu: *CPU, _: InstrArgs) u8 { // C -> [7 -> 0] -> C Rotate bits in register r8 left through carry.
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
    return 2;
}
fn SLAr8(cpu: *CPU, args: InstrArgs) u8 { // Shift Left Arithmetic register r8. C <- [7 <- 0] <- 0
    const reg = cpu.get_byte(args.target);
    const shifted = reg << 1;
    const z = shifted == 0;
    const c = reg >> 7 == 1;
    cpu.f.write(z, c, false, false);
    cpu.set_byte(args.target, shifted);
    cpu.pc += 1;
    return 2;
}
fn SLAHL(cpu: *CPU, _: InstrArgs) u8 { // Shift Left Arithmetic byte pointed to by hl. C <- [7 <- 0] <- 0
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
fn SRAr8(cpu: *CPU, args: InstrArgs) u8 { // Shift Right Arithmetic register r8. 7 -> [7 -> 0] -> C
    const reg = cpu.get_byte(args.target);
    const shifted = ((reg >> 7 & 1) << 7) | reg >> 1;
    const z = shifted == 0;
    const c = reg & 1 == 1;
    cpu.f.write(z, c, false, false);
    cpu.set_byte(args.target, shifted);
    cpu.pc += 1;
    return 2;
}
fn SRAHL(cpu: *CPU, _: InstrArgs) u8 { // Shift Right Arithmetic byte pointed to by hl. 0 -> [7 -> 0] -> C
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
fn SRLr8(cpu: *CPU, args: InstrArgs) u8 { // Shift Right Arithmetic register r8. 7 -> [7 -> 0] -> C
    const reg = cpu.get_byte(args.target);
    const shifted = reg >> 1;
    const z = shifted == 0;
    const c = reg & 1 == 1;
    cpu.f.write(z, c, false, false);
    cpu.set_byte(args.target, shifted);
    cpu.pc += 1;
    return 2;
}
fn SRLHL(cpu: *CPU, _: InstrArgs) u8 { // Shift Right Arithmetic byte pointed to by hl. 0 -> [7 -> 0] -> C
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
fn SWAPr8(cpu: *CPU, args: InstrArgs) u8 { // Swap the upper 4 bits in register r8 and the lower 4 ones.
    const reg = cpu.get_byte(args.target);
    const high: u4 = @truncate(reg >> 4);
    const low: u4 = @truncate(reg);
    cpu.set_byte(args.target, (@as(u8, low) << 4) | high);
    cpu.pc += 1;
    return 2;
}
fn SWAPHL(cpu: *CPU, _: InstrArgs) u8 { // Swap the upper 4 bits in register r8 and the lower 4 ones.
    const mem_address = cpu.get_word(.h);
    const byte = cpu.bus.readByte(mem_address);
    const high: u4 = @truncate(byte >> 4);
    const low: u4 = @truncate(byte);
    cpu.bus.writeByte(mem_address, (@as(u8, low) << 4) | high);
    cpu.pc += 1;
    return 4;
}
// BIT MANIPULATION
//
fn BITTESTr8(cpu: *CPU, args: InstrArgs) u8 {
    const bit: u3 = args.bit_target.bit;
    const target = cpu.get_byte(args.bit_target.target);
    const z = @as(u1, @truncate(target >> bit)) == 0; // set zero flag if the target bit is not set
    const c = cpu.f.cFlag();
    cpu.f.write(z, c, true, false);
    cpu.pushToExecutionChain("BITTEST | {any} >> {d}", .{ target, bit }); // which register/bit
    cpu.pc += 1;
    return 2;
}
fn BITTESTHL(cpu: *CPU, args: InstrArgs) u8 {
    const bit: u3 = args.bit_target.bit;
    const hl = cpu.get_word(.h);
    const byte = cpu.bus.readByte(hl);
    const z = @as(u1, @truncate(byte >> bit)) == 0;
    const c = cpu.f.cFlag();
    cpu.f.write(z, c, true, false);
    cpu.pushToExecutionChain("BITTEST HL | mem[X.{X:04}] = b.{b} >> {d}", .{ hl, byte, bit }); // which bit
    cpu.pc += 1;
    return 3;
}
fn RES(cpu: *CPU, args: InstrArgs) u8 { // Set bit u3 in register r8 to 0
    const bit: u3 = args.bit_target.bit;
    const target = cpu.get_byte(args.bit_target.target);
    const res = target & ~(@as(u8, 1) << bit); // target and everything but this bit
    cpu.set_byte(args.bit_target.target, res);
    cpu.pushToExecutionChain("RES | {any} >> {d} = 0", .{ target, bit }); // which register/bit
    cpu.pc += 1;
    return 2;
}
fn RESHL(cpu: *CPU, args: InstrArgs) u8 { // Set bit u3 in the byte pointed to by hl to 0.
    const bit: u3 = args.bit_target.bit;
    const hl = cpu.get_word(.h);
    const byte = cpu.bus.readByte(hl);
    const res = byte & ~(@as(u8, 1) << bit); // target and everything but this bit
    cpu.bus.writeByte(hl, res);
    cpu.pushToExecutionChain("RES HL | mem[X.{X:04}] = b.{b} >> {d}", .{ hl, byte, bit }); // which bit
    cpu.pc += 1;
    return 4;
}
fn SET(cpu: *CPU, args: InstrArgs) u8 { // Set bit u3 in register r8 to 1. Bit 0 is the rightmost one, bit 7 the leftmost one.
    const bit: u3 = args.bit_target.bit;
    const target = cpu.get_byte(args.bit_target.target);
    const res = target | (@as(u8, 1) << bit); // everything and this bit
    cpu.set_byte(args.bit_target.target, res);
    cpu.pushToExecutionChain("RES | {any} >> {d} = 0", .{ target, bit }); // which register/bit
    cpu.pc += 1;
    return 2;
}
fn SETHL(cpu: *CPU, args: InstrArgs) u8 { // Set bit u3 in the byte pointed to by hl to 1.
    const bit: u3 = args.bit_target.bit;
    const hl = cpu.get_word(.h);
    const byte = cpu.bus.readByte(hl);
    const res = byte | (@as(u8, 1) << bit); // everything and this bit
    cpu.bus.writeByte(hl, res);
    cpu.pushToExecutionChain("RES HL | mem[X.{X:04}] = b.{b} >> {d}", .{ hl, byte, bit }); // which bit
    cpu.pc += 1;
    return 4;
}

// JUMP
fn JP(cpu: *CPU, args: InstrArgs) u8 {
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
fn JPHL(cpu: *CPU, _: InstrArgs) u8 {
    cpu.pushToExecutionChain("JPHL", .{});
    cpu.pc = cpu.get_word(regID.h);
    return 1;
}
fn JR(cpu: *CPU, args: InstrArgs) u8 {
    const dist: i8 = @bitCast(cpu.bus.readByte(cpu.pc + 1));
    const jump = cpu.f.check(args.flagConditions);

    if (jump) {
        const new_mem: u16 = @bitCast(@addWithOverflow(@as(i16, @intCast(cpu.pc + 2)), dist)[0]);
        cpu.pc = new_mem;
        cpu.pushToExecutionChain("JR | to pc:0x{X}", .{new_mem});
        return 3; // 3 cycles when taken
    } else { // next instruction, condition failed
        cpu.pushToExecutionChain("JR | skipped jump, failed condition", .{});
        cpu.pc += 2;
        return 2; // 2 cycles when not taken
    }
}
// CALL
fn CALLn16(cpu: *CPU, args: InstrArgs) u8 { //
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
fn RST(cpu: *CPU, args: InstrArgs) u8 {
    const ret = cpu.pc + 1;
    cpu.push_stack(ret);
    cpu.pushToExecutionChain("RST | to 0x{X}, later RET to 0x{X}", .{ args.where, ret });
    cpu.pc = args.where;
    return 4;
}
// RETURN
fn RET(cpu: *CPU, args: InstrArgs) u8 {
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
fn RETI(cpu: *CPU, _: InstrArgs) u8 {
    print("reti\n", .{});
    const popped = cpu.pop_stack();
    const low = popped[0];
    const high = popped[1];
    const jumpto = @as(u16, high) << 8 | low;
    cpu.pushToExecutionChain("RETI | jumpto pc[{X:04}]", .{ jumpto });
    cpu.sp += 1;
    cpu.pc = jumpto;
    cpu.bus.handler.ime = true;
    return 4;
}
pub const DEBUG = true;
pub inline fn fmtInsDebug(string: []const u8, args: anytype) []const u8 {
    var buffer: [CPU.Log.MAX_CHAR]u8 = undefined;
    return std.fmt.bufPrint(&buffer, string, args) catch unreachable;
}
// opcode to exe
//
pub inline fn exe_from_byte(cpu: *CPU, prefixed: bool) u8 {
    return switch (prefixed) {
        false => switch (cpu.executing_byte) {
            0x00 => NOP(cpu, .{ .none = {} }),
            0x01 => LD16(cpu, .{ .target = regID.b }),
            0x02 => LDr16A(cpu, .{ .target = regID.b }),
            0x03 => INCr16(cpu, .{ .target = regID.b }),
            0x04 => INCr8(cpu, .{ .target = regID.b }),
            0x05 => DECr8(cpu, .{ .target = regID.b }),
            0x06 => LD8(cpu, .{ .target = regID.b }),
            0x07 => RLCA(cpu, .{ .none = {} }),
            0x08 => LDn16SP(cpu, .{ .none = {} }),
            0x09 => ADDHLr16(cpu, .{ .target = regID.b }),
            0x0A => LDAr16(cpu, .{ .target = regID.b }),
            0x0B => DECr16(cpu, .{ .target = regID.b }),
            0x0C => INCr8(cpu, .{ .target = regID.c }),
            0x0D => DECr8(cpu, .{ .target = regID.c }),
            0x0E => LD8(cpu, .{ .target = regID.c }),
            0x0F => RRCA(cpu, .{ .none = {} }),
            0x10 => STOP(cpu, .{ .none = {} }), // STOP
            0x11 => LD16(cpu, .{ .target = regID.d }),
            0x12 => LDr16A(cpu, .{ .target = regID.d }),
            0x13 => INCr16(cpu, .{ .target = regID.d }),
            0x14 => INCr8(cpu, .{ .target = regID.d }),
            0x15 => DECr8(cpu, .{ .target = regID.d }),
            0x16 => LD8(cpu, .{ .target = regID.d }),
            0x17 => RLA(cpu, .{ .none = {} }),
            0x18 => JR(cpu, .{ .flagConditions = .none }),
            0x19 => ADDHLr16(cpu, .{ .target = regID.d }),
            0x1A => LDAr16(cpu, .{ .target = regID.d }),
            0x1B => DECr16(cpu, .{ .target = regID.d }),
            0x1C => INCr8(cpu, .{ .target = regID.e }),
            0x1D => DECr8(cpu, .{ .target = regID.e }),
            0x1E => LD8(cpu, .{ .target = regID.e }),
            0x1F => RRA(cpu, .{ .none = {} }),
            0x20 => JR(cpu, .{ .flagConditions = .nz }),
            0x21 => LD16(cpu, .{ .target = regID.h }),
            0x22 => LDHLIA(cpu, .{ .none = {} }),
            0x23 => INCr16(cpu, .{ .target = regID.h }),
            0x24 => INCr8(cpu, .{ .target = regID.h }),
            0x25 => DECr8(cpu, .{ .target = regID.h }),
            0x26 => LD8(cpu, .{ .target = regID.h }),
            0x27 => DAA(cpu, .{ .none = {} }),
            0x28 => JR(cpu, .{ .flagConditions = .z }),
            0x29 => ADDHLr16(cpu, .{ .target = regID.h }),
            0x2A => LDAHL(cpu, .{ .hl_mod = 1 }),
            0x2B => DECr16(cpu, .{ .target = regID.h }),
            0x2C => INCr8(cpu, .{ .target = regID.l }),
            0x2D => DECr8(cpu, .{ .target = regID.l }),
            0x2E => LD8(cpu, .{ .target = regID.l }),
            0x2F => CPL(cpu, .{ .none = {} }),
            0x30 => JR(cpu, .{ .flagConditions = .nc }),
            0x31 => LDSP16(cpu, .{ .none = {} }),
            0x32 => LDHLDA(cpu, .{ .none = {} }),
            0x33 => INCSP(cpu, .{ .none = {} }),
            0x34 => INCHL(cpu, .{ .none = {} }),
            0x35 => DECHL(cpu, .{ .none = {} }),
            0x36 => LDHL8(cpu, .{ .none = {} }),
            0x37 => SCF(cpu, .{ .none = {} }),
            0x38 => JR(cpu, .{ .flagConditions = .c }),
            0x39 => ADDHLSP(cpu, .{ .none = {} }),
            0x3A => LDAHL(cpu, .{ .hl_mod = 0 }),
            0x3B => DECSP(cpu, .{ .none = {} }),
            0x3C => INCr8(cpu, .{ .target = regID.a }),
            0x3D => DECr8(cpu, .{ .target = regID.a }),
            0x3E => LD8(cpu, .{ .target = regID.a }),
            0x3F => CCF(cpu, .{ .none = {} }),
            0x40 => LDr8(cpu, .{ .targets = .{ .to = .b, .from = .b } }),
            0x41 => LDr8(cpu, .{ .targets = .{ .to = .b, .from = .c } }),
            0x42 => LDr8(cpu, .{ .targets = .{ .to = .b, .from = .d } }),
            0x43 => LDr8(cpu, .{ .targets = .{ .to = .b, .from = .e } }),
            0x44 => LDr8(cpu, .{ .targets = .{ .to = .b, .from = .h } }),
            0x45 => LDr8(cpu, .{ .targets = .{ .to = .b, .from = .l } }),
            0x46 => LDr8HL(cpu, .{ .target = regID.b }),
            0x47 => LDr8(cpu, .{ .targets = .{ .to = .b, .from = .a } }),
            0x48 => LDr8(cpu, .{ .targets = .{ .to = .c, .from = .b } }),
            0x49 => LDr8(cpu, .{ .targets = .{ .to = .c, .from = .c } }),
            0x4A => LDr8(cpu, .{ .targets = .{ .to = .c, .from = .d } }),
            0x4B => LDr8(cpu, .{ .targets = .{ .to = .c, .from = .e } }),
            0x4C => LDr8(cpu, .{ .targets = .{ .to = .c, .from = .h } }),
            0x4D => LDr8(cpu, .{ .targets = .{ .to = .c, .from = .l } }),
            0x4E => LDr8HL(cpu, .{ .target = regID.c }),
            0x4F => LDr8(cpu, .{ .targets = .{ .to = .c, .from = .a } }),
            0x50 => LDr8(cpu, .{ .targets = .{ .to = .d, .from = .b } }),
            0x51 => LDr8(cpu, .{ .targets = .{ .to = .d, .from = .c } }),
            0x52 => LDr8(cpu, .{ .targets = .{ .to = .d, .from = .d } }),
            0x53 => LDr8(cpu, .{ .targets = .{ .to = .d, .from = .e } }),
            0x54 => LDr8(cpu, .{ .targets = .{ .to = .d, .from = .h } }),
            0x55 => LDr8(cpu, .{ .targets = .{ .to = .d, .from = .l } }),
            0x56 => LDr8HL(cpu, .{ .target = regID.d }),
            0x57 => LDr8(cpu, .{ .targets = .{ .to = .d, .from = .a } }),
            0x58 => LDr8(cpu, .{ .targets = .{ .to = .e, .from = .b } }),
            0x59 => LDr8(cpu, .{ .targets = .{ .to = .e, .from = .c } }),
            0x5A => LDr8(cpu, .{ .targets = .{ .to = .e, .from = .d } }),
            0x5B => LDr8(cpu, .{ .targets = .{ .to = .e, .from = .e } }),
            0x5C => LDr8(cpu, .{ .targets = .{ .to = .e, .from = .h } }),
            0x5D => LDr8(cpu, .{ .targets = .{ .to = .e, .from = .l } }),
            0x5E => LDr8HL(cpu, .{ .target = regID.e }),
            0x5F => LDr8(cpu, .{ .targets = .{ .to = .e, .from = .a } }),
            0x60 => LDr8(cpu, .{ .targets = .{ .to = .h, .from = .b } }),
            0x61 => LDr8(cpu, .{ .targets = .{ .to = .h, .from = .c } }),
            0x62 => LDr8(cpu, .{ .targets = .{ .to = .h, .from = .d } }),
            0x63 => LDr8(cpu, .{ .targets = .{ .to = .h, .from = .e } }),
            0x64 => LDr8(cpu, .{ .targets = .{ .to = .h, .from = .h } }),
            0x65 => LDr8(cpu, .{ .targets = .{ .to = .h, .from = .l } }),
            0x66 => LDr8HL(cpu, .{ .target = regID.h }),
            0x67 => LDr8(cpu, .{ .targets = .{ .to = .h, .from = .a } }),
            0x68 => LDr8(cpu, .{ .targets = .{ .to = .l, .from = .b } }),
            0x69 => LDr8(cpu, .{ .targets = .{ .to = .l, .from = .c } }),
            0x6A => LDr8(cpu, .{ .targets = .{ .to = .l, .from = .d } }),
            0x6B => LDr8(cpu, .{ .targets = .{ .to = .l, .from = .e } }),
            0x6C => LDr8(cpu, .{ .targets = .{ .to = .l, .from = .h } }),
            0x6D => LDr8(cpu, .{ .targets = .{ .to = .l, .from = .l } }),
            0x6E => LDr8HL(cpu, .{ .target = regID.l }),
            0x6F => LDr8(cpu, .{ .targets = .{ .to = .l, .from = .a } }),
            0x70 => LDHLr8(cpu, .{ .target = regID.b }),
            0x71 => LDHLr8(cpu, .{ .target = regID.c }),
            0x72 => LDHLr8(cpu, .{ .target = regID.d }),
            0x73 => LDHLr8(cpu, .{ .target = regID.e }),
            0x74 => LDHLr8(cpu, .{ .target = regID.h }),
            0x75 => LDHLr8(cpu, .{ .target = regID.l }),
            0x76 => HALT(cpu, .{ .none = {} }), // HALT
            0x77 => LDHLr8(cpu, .{ .target = regID.a }),
            0x78 => LDr8(cpu, .{ .targets = .{ .to = .a, .from = .b } }),
            0x79 => LDr8(cpu, .{ .targets = .{ .to = .a, .from = .c } }),
            0x7A => LDr8(cpu, .{ .targets = .{ .to = .a, .from = .d } }),
            0x7B => LDr8(cpu, .{ .targets = .{ .to = .a, .from = .e } }),
            0x7C => LDr8(cpu, .{ .targets = .{ .to = .a, .from = .h } }),
            0x7D => LDr8(cpu, .{ .targets = .{ .to = .a, .from = .l } }),
            0x7E => LDr8HL(cpu, .{ .target = regID.a }),
            0x7F => LDr8(cpu, .{ .targets = .{ .to = regID.a, .from = regID.a } }),
            0x80 => ADDAr8(cpu, .{ .target = regID.b }),
            0x81 => ADDAr8(cpu, .{ .target = regID.c }),
            0x82 => ADDAr8(cpu, .{ .target = regID.d }),
            0x83 => ADDAr8(cpu, .{ .target = regID.e }),
            0x84 => ADDAr8(cpu, .{ .target = regID.h }),
            0x85 => ADDAr8(cpu, .{ .target = regID.l }),
            0x86 => ADDAHL(cpu, .{ .none = {} }),
            0x87 => ADDAr8(cpu, .{ .target = regID.a }),
            0x88 => ADCAr8(cpu, .{ .target = regID.b }),
            0x89 => ADCAr8(cpu, .{ .target = regID.c }),
            0x8A => ADCAr8(cpu, .{ .target = regID.d }),
            0x8B => ADCAr8(cpu, .{ .target = regID.e }),
            0x8C => ADCAr8(cpu, .{ .target = regID.h }),
            0x8D => ADCAr8(cpu, .{ .target = regID.l }),
            0x8E => ADCAHL(cpu, .{ .none = {} }),
            0x8F => ADCAr8(cpu, .{ .target = regID.a }),
            0x90 => SUBAr8(cpu, .{ .target = regID.b }),
            0x91 => SUBAr8(cpu, .{ .target = regID.c }),
            0x92 => SUBAr8(cpu, .{ .target = regID.d }),
            0x93 => SUBAr8(cpu, .{ .target = regID.e }),
            0x94 => SUBAr8(cpu, .{ .target = regID.h }),
            0x95 => SUBAr8(cpu, .{ .target = regID.l }),
            0x96 => SUBAHL(cpu, .{ .none = {} }),
            0x97 => SUBAr8(cpu, .{ .target = regID.a }),
            0x98 => SBCAr8(cpu, .{ .target = regID.b }),
            0x99 => SBCAr8(cpu, .{ .target = regID.c }),
            0x9A => SBCAr8(cpu, .{ .target = regID.d }),
            0x9B => SBCAr8(cpu, .{ .target = regID.e }),
            0x9C => SBCAr8(cpu, .{ .target = regID.h }),
            0x9D => SBCAr8(cpu, .{ .target = regID.l }),
            0x9E => SBCAHL(cpu, .{ .none = {} }),
            0x9F => SBCAr8(cpu, .{ .target = regID.a }),
            0xA0 => ANDr8(cpu, .{ .target = regID.b }),
            0xA1 => ANDr8(cpu, .{ .target = regID.c }),
            0xA2 => ANDr8(cpu, .{ .target = regID.d }),
            0xA3 => ANDr8(cpu, .{ .target = regID.e }),
            0xA4 => ANDr8(cpu, .{ .target = regID.h }),
            0xA5 => ANDr8(cpu, .{ .target = regID.l }),
            0xA6 => ANDHL(cpu, .{ .none = {} }),
            0xA7 => ANDr8(cpu, .{ .target = regID.a }),
            0xA8 => XORr8(cpu, .{ .target = regID.b }),
            0xA9 => XORr8(cpu, .{ .target = regID.c }),
            0xAA => XORr8(cpu, .{ .target = regID.d }),
            0xAB => XORr8(cpu, .{ .target = regID.e }),
            0xAC => XORr8(cpu, .{ .target = regID.h }),
            0xAD => XORr8(cpu, .{ .target = regID.l }),
            0xAE => XORHL(cpu, .{ .none = {} }),
            0xAF => XORr8(cpu, .{ .target = regID.a }),
            0xB0 => ORr8(cpu, .{ .target = regID.b }),
            0xB1 => ORr8(cpu, .{ .target = regID.c }),
            0xB2 => ORr8(cpu, .{ .target = regID.d }),
            0xB3 => ORr8(cpu, .{ .target = regID.e }),
            0xB4 => ORr8(cpu, .{ .target = regID.h }),
            0xB5 => ORr8(cpu, .{ .target = regID.l }),
            0xB6 => ORHL(cpu, .{ .none = {} }),
            0xB7 => ORr8(cpu, .{ .target = regID.a }),
            0xB8 => CPAr8(cpu, .{ .target = regID.b }),
            0xB9 => CPAr8(cpu, .{ .target = regID.c }),
            0xBA => CPAr8(cpu, .{ .target = regID.d }),
            0xBB => CPAr8(cpu, .{ .target = regID.e }),
            0xBC => CPAr8(cpu, .{ .target = regID.h }),
            0xBD => CPAr8(cpu, .{ .target = regID.l }),
            0xBE => CPAHL(cpu, .{ .none = {} }),
            0xBF => CPAr8(cpu, .{ .target = regID.a }),
            0xC0 => RET(cpu, .{ .flagConditions = .nz }),
            0xC1 => POP(cpu, .{ .target = regID.b }),
            0xC2 => JP(cpu, .{ .flagConditions = .nz }),
            0xC3 => JP(cpu, .{ .flagConditions = .none }),
            0xC4 => CALLn16(cpu, .{ .flagConditions = .nz }),
            0xC5 => PUSH(cpu, .{ .target = regID.b }),
            0xC6 => ADDAn8(cpu, .{ .none = {} }),
            0xC7 => RST(cpu, .{ .where = 0x0 }),
            0xC8 => RET(cpu, .{ .flagConditions = .z }),
            0xC9 => RET(cpu, .{ .flagConditions = .none }),
            0xCA => JP(cpu, .{ .flagConditions = .z }),
            0xCB => INVALID(cpu, .{ .none = {} }), // cb prefix
            0xCC => CALLn16(cpu, .{ .flagConditions = .z }),
            0xCD => CALLn16(cpu, .{ .flagConditions = .none }),
            0xCE => ADCAn8(cpu, .{ .none = {} }),
            0xCF => RST(cpu, .{ .where = 0x08 }),
            0xD0 => RET(cpu, .{ .flagConditions = .nc }),
            0xD1 => POP(cpu, .{ .target = regID.d }),
            0xD2 => JP(cpu, .{ .flagConditions = .nc }),
            0xD3 => INVALID(cpu, .{ .none = {} }), // undefined instruction
            0xD4 => CALLn16(cpu, .{ .flagConditions = .nc }),
            0xD5 => PUSH(cpu, .{ .target = regID.d }),
            0xD6 => SUBAn8(cpu, .{ .none = {} }),
            0xD7 => RST(cpu, .{ .where = 0x10 }),
            0xD8 => RET(cpu, .{ .flagConditions = .c }),
            0xD9 => RETI(cpu, .{ .none = {} }),
            0xDA => JP(cpu, .{ .flagConditions = .c }),
            0xDB => INVALID(cpu, .{ .none = {} }), // undefined instruction
            0xDC => CALLn16(cpu, .{ .flagConditions = .c }),
            0xDD => INVALID(cpu, .{ .none = {} }), // undefined instruction
            0xDE => SBCAn8(cpu, .{ .none = {} }),
            0xDF => RST(cpu, .{ .where = 0x18 }),
            0xE0 => LDHn16A(cpu, .{ .none = {} }),
            0xE1 => POP(cpu, .{ .target = regID.h }),
            0xE2 => LDHCA(cpu, .{ .none = {} }),
            0xE3 => INVALID(cpu, .{ .none = {} }), // undefined instruction
            0xE4 => INVALID(cpu, .{ .none = {} }), // undefined instruction
            0xE5 => PUSH(cpu, .{ .target = regID.h }),
            0xE6 => ANDn8(cpu, .{ .none = {} }),
            0xE7 => RST(cpu, .{ .where = 0x20 }),
            0xE8 => ADDSPn8(cpu, .{ .none = {} }),
            0xE9 => JPHL(cpu, .{ .none = {} }),
            0xEA => LDn16A(cpu, .{ .none = {} }),
            0xEB => INVALID(cpu, .{ .none = {} }), // undefined instruction
            0xEC => INVALID(cpu, .{ .none = {} }), // undefined instruction
            0xED => INVALID(cpu, .{ .none = {} }), // undefined instruction
            0xEE => XORn8(cpu, .{ .none = {} }),
            0xEF => RST(cpu, .{ .where = 0x28 }),
            0xF0 => LDHAn16(cpu, .{ .none = {} }),
            0xF1 => POP(cpu, .{ .target = regID.a }),
            0xF2 => LDHAC(cpu, .{ .none = {} }),
            0xF3 => DI(cpu, .{ .none = {} }),
            0xF4 => INVALID(cpu, .{ .none = {} }), // undefined instruction
            0xF5 => PUSH(cpu, .{ .target = regID.a }),
            0xF6 => ORn8(cpu, .{ .none = {} }),
            0xF7 => RST(cpu, .{ .where = 0x30 }),
            0xF8 => LDHLSPn8(cpu, .{ .none = {} }),
            0xF9 => LDSPHL(cpu, .{ .none = {} }),
            0xFA => LDAn16(cpu, .{ .none = {} }),
            0xFB => EI(cpu, .{ .none = {} }),
            0xFC => INVALID(cpu, .{ .none = {} }), // undefined instruction
            0xFD => INVALID(cpu, .{ .none = {} }), // undefined instruction
            0xFE => CPAn8(cpu, .{ .none = {} }),
            0xFF => RST(cpu, .{ .where = 0x38 }),
        },
        true => switch (cpu.executing_byte) {
            0x00 => RLCr8(cpu, .{ .target = regID.b }),
            0x01 => RLCr8(cpu, .{ .target = regID.c }),
            0x02 => RLCr8(cpu, .{ .target = regID.d }),
            0x03 => RLCr8(cpu, .{ .target = regID.e }),
            0x04 => RLCr8(cpu, .{ .target = regID.h }),
            0x05 => RLCr8(cpu, .{ .target = regID.l }),
            0x06 => RLCHL(cpu, .{ .none = {} }),
            0x07 => RLCr8(cpu, .{ .target = regID.a }),
            0x08 => RRCr8(cpu, .{ .target = regID.b }),
            0x09 => RRCr8(cpu, .{ .target = regID.c }),
            0x0A => RRCr8(cpu, .{ .target = regID.d }),
            0x0B => RRCr8(cpu, .{ .target = regID.e }),
            0x0C => RRCr8(cpu, .{ .target = regID.h }),
            0x0D => RRCr8(cpu, .{ .target = regID.l }),
            0x0E => RRCHL(cpu, .{ .none = {} }),
            0x0F => RRCr8(cpu, .{ .target = regID.a }),
            0x10 => RLr8(cpu, .{ .target = regID.b }),
            0x11 => RLr8(cpu, .{ .target = regID.c }),
            0x12 => RLr8(cpu, .{ .target = regID.d }),
            0x13 => RLr8(cpu, .{ .target = regID.e }),
            0x14 => RLr8(cpu, .{ .target = regID.h }),
            0x15 => RLr8(cpu, .{ .target = regID.l }),
            0x16 => RLHL(cpu, .{ .none = {} }),
            0x17 => RLr8(cpu, .{ .target = regID.a }),
            0x18 => RRr8(cpu, .{ .target = regID.b }),
            0x19 => RRr8(cpu, .{ .target = regID.c }),
            0x1A => RRr8(cpu, .{ .target = regID.d }),
            0x1B => RRr8(cpu, .{ .target = regID.e }),
            0x1C => RRr8(cpu, .{ .target = regID.h }),
            0x1D => RRr8(cpu, .{ .target = regID.l }),
            0x1E => RRHL(cpu, .{ .none = {} }),
            0x1F => RRr8(cpu, .{ .target = regID.a }),
            0x20 => SLAr8(cpu, .{ .target = regID.b }),
            0x21 => SLAr8(cpu, .{ .target = regID.c }),
            0x22 => SLAr8(cpu, .{ .target = regID.d }),
            0x23 => SLAr8(cpu, .{ .target = regID.e }),
            0x24 => SLAr8(cpu, .{ .target = regID.h }),
            0x25 => SLAr8(cpu, .{ .target = regID.l }),
            0x26 => SLAHL(cpu, .{ .none = {} }),
            0x27 => SLAr8(cpu, .{ .target = regID.a }),
            0x28 => SRAr8(cpu, .{ .target = regID.b }),
            0x29 => SRAr8(cpu, .{ .target = regID.c }),
            0x2A => SRAr8(cpu, .{ .target = regID.d }),
            0x2B => SRAr8(cpu, .{ .target = regID.e }),
            0x2C => SRAr8(cpu, .{ .target = regID.h }),
            0x2D => SRAr8(cpu, .{ .target = regID.l }),
            0x2E => SRAHL(cpu, .{ .none = {} }),
            0x2F => SWAPr8(cpu, .{ .target = regID.a }),
            0x30 => SWAPr8(cpu, .{ .target = regID.b }),
            0x31 => SWAPr8(cpu, .{ .target = regID.c }),
            0x32 => SWAPr8(cpu, .{ .target = regID.d }),
            0x33 => SWAPr8(cpu, .{ .target = regID.e }),
            0x34 => SWAPr8(cpu, .{ .target = regID.h }),
            0x35 => SWAPr8(cpu, .{ .target = regID.l }),
            0x36 => SWAPHL(cpu, .{ .none = {} }),
            0x37 => SWAPr8(cpu, .{ .target = regID.a }),
            0x38 => SRLr8(cpu, .{ .target = regID.b }),
            0x39 => SRLr8(cpu, .{ .target = regID.c }),
            0x3A => SRLr8(cpu, .{ .target = regID.d }),
            0x3B => SRLr8(cpu, .{ .target = regID.e }),
            0x3C => SRLr8(cpu, .{ .target = regID.h }),
            0x3D => SRLr8(cpu, .{ .target = regID.l }),
            0x3E => SRLHL(cpu, .{ .none = {} }),
            0x3F => SRLr8(cpu, .{ .target = regID.a }),
            0x40 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 0 } }),
            0x41 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 0 } }),
            0x42 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 0 } }),
            0x43 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 0 } }),
            0x44 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 0 } }),
            0x45 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 0 } }),
            0x46 => BITTESTHL(cpu, .{ .bit = 0 }),
            0x47 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 0 } }),
            0x48 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 1 } }),
            0x49 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 1 } }),
            0x4A => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 1 } }),
            0x4B => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 1 } }),
            0x4C => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 1 } }),
            0x4D => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 1 } }),
            0x4E => BITTESTHL(cpu, .{ .bit = 1 }),
            0x4F => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 1 } }),
            0x50 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 2 } }),
            0x51 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 2 } }),
            0x52 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 2 } }),
            0x53 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 2 } }),
            0x54 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 2 } }),
            0x55 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 2 } }),
            0x56 => BITTESTHL(cpu, .{ .bit = 2 }),
            0x57 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 2 } }),
            0x58 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 3 } }),
            0x59 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 3 } }),
            0x5A => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 3 } }),
            0x5B => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 3 } }),
            0x5C => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 3 } }),
            0x5D => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 3 } }),
            0x5E => BITTESTHL(cpu, .{ .bit = 3 }),
            0x5F => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 3 } }),
            0x60 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 4 } }),
            0x61 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 4 } }),
            0x62 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 4 } }),
            0x63 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 4 } }),
            0x64 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 4 } }),
            0x65 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 4 } }),
            0x66 => BITTESTHL(cpu, .{ .bit = 4 }),
            0x67 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 4 } }),
            0x68 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 5 } }),
            0x69 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 5 } }),
            0x6A => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 5 } }),
            0x6B => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 5 } }),
            0x6C => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 5 } }),
            0x6D => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 5 } }),
            0x6E => BITTESTHL(cpu, .{ .bit = 5 }),
            0x6F => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 5 } }),
            0x70 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 6 } }),
            0x71 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 6 } }),
            0x72 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 6 } }),
            0x73 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 6 } }),
            0x74 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 6 } }),
            0x75 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 6 } }),
            0x76 => BITTESTHL(cpu, .{ .bit = 6 }),
            0x77 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 6 } }),
            0x78 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 7 } }),
            0x79 => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 7 } }),
            0x7A => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 7 } }),
            0x7B => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 7 } }),
            0x7C => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 7 } }),
            0x7D => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 7 } }),
            0x7E => BITTESTHL(cpu, .{ .bit = 7 }),
            0x7F => BITTESTr8(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 7 } }),
            0x80 => RES(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 0 } }),
            0x81 => RES(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 0 } }),
            0x82 => RES(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 0 } }),
            0x83 => RES(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 0 } }),
            0x84 => RES(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 0 } }),
            0x85 => RES(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 0 } }),
            0x86 => RESHL(cpu, .{ .bit = 0 }),
            0x87 => RES(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 0 } }),
            0x88 => RES(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 1 } }),
            0x89 => RES(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 1 } }),
            0x8A => RES(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 1 } }),
            0x8B => RES(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 1 } }),
            0x8C => RES(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 1 } }),
            0x8D => RES(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 1 } }),
            0x8E => RESHL(cpu, .{ .bit = 1 }),
            0x8F => RES(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 1 } }),
            0x90 => RES(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 2 } }),
            0x91 => RES(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 2 } }),
            0x92 => RES(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 2 } }),
            0x93 => RES(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 2 } }),
            0x94 => RES(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 2 } }),
            0x95 => RES(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 2 } }),
            0x96 => RESHL(cpu, .{ .bit = 2 }),
            0x97 => RES(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 2 } }),
            0x98 => RES(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 3 } }),
            0x99 => RES(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 3 } }),
            0x9A => RES(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 3 } }),
            0x9B => RES(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 3 } }),
            0x9C => RES(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 3 } }),
            0x9D => RES(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 3 } }),
            0x9E => RESHL(cpu, .{ .bit = 3 }),
            0x9F => RES(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 3 } }),
            0xA0 => RES(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 4 } }),
            0xA1 => RES(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 4 } }),
            0xA2 => RES(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 4 } }),
            0xA3 => RES(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 4 } }),
            0xA4 => RES(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 4 } }),
            0xA5 => RES(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 4 } }),
            0xA6 => RESHL(cpu, .{ .bit = 4 }),
            0xA7 => RES(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 4 } }),
            0xA8 => RES(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 5 } }),
            0xA9 => RES(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 5 } }),
            0xAA => RES(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 5 } }),
            0xAB => RES(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 5 } }),
            0xAC => RES(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 5 } }),
            0xAD => RES(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 5 } }),
            0xAE => RESHL(cpu, .{ .bit = 5 }),
            0xAF => RES(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 5 } }),
            0xB0 => RES(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 6 } }),
            0xB1 => RES(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 6 } }),
            0xB2 => RES(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 6 } }),
            0xB3 => RES(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 6 } }),
            0xB4 => RES(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 6 } }),
            0xB5 => RES(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 6 } }),
            0xB6 => RESHL(cpu, .{ .bit = 6 }),
            0xB7 => RES(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 6 } }),
            0xB8 => RES(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 7 } }),
            0xB9 => RES(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 7 } }),
            0xBA => RES(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 7 } }),
            0xBB => RES(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 7 } }),
            0xBC => RES(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 7 } }),
            0xBD => RES(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 7 } }),
            0xBE => RESHL(cpu, .{.bit = 7,}),
            0xBF => RES(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 7 } }),
            0xC0 => SET(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 0 } }),
            0xC1 => SET(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 0 } }),
            0xC2 => SET(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 0 } }),
            0xC3 => SET(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 0 } }),
            0xC4 => SET(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 0 } }),
            0xC5 => SET(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 0 } }),
            0xC6 => SETHL(cpu, .{ .bit = 0 }),
            0xC7 => SET(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 0 } }),
            0xC8 => SET(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 1 } }),
            0xC9 => SET(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 1 } }),
            0xCA => SET(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 1 } }),
            0xCB => SET(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 1 } }),
            0xCC => SET(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 1 } }),
            0xCD => SET(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 1 } }),
            0xCE => SETHL(cpu, .{ .bit = 1 }),
            0xCF => SET(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 1 } }),
            0xD0 => SET(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 2 } }),
            0xD1 => SET(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 2 } }),
            0xD2 => SET(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 2 } }),
            0xD3 => SET(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 2 } }),
            0xD4 => SET(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 2 } }),
            0xD5 => SET(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 2 } }),
            0xD6 => SETHL(cpu, .{ .bit = 2 }),
            0xD7 => SET(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 2 } }),
            0xD8 => SET(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 3 } }),
            0xD9 => SET(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 3 } }),
            0xDA => SET(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 3 } }),
            0xDB => SET(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 3 } }),
            0xDC => SET(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 3 } }),
            0xDD => SET(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 3 } }),
            0xDE => SETHL(cpu, .{ .bit = 3 }),
            0xDF => SET(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 3 } }),
            0xE0 => SET(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 4 } }),
            0xE1 => SET(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 4 } }),
            0xE2 => SET(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 4 } }),
            0xE3 => SET(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 4 } }),
            0xE4 => SET(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 4 } }),
            0xE5 => SET(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 4 } }),
            0xE6 => SETHL(cpu, .{ .bit = 4 }),
            0xE7 => SET(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 4 } }),
            0xE8 => SET(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 5 } }),
            0xE9 => SET(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 5 } }),
            0xEA => SET(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 5 } }),
            0xEB => SET(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 5 } }),
            0xEC => SET(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 5 } }),
            0xED => SET(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 5 } }),
            0xEE => SETHL(cpu, .{ .bit = 5 }),
            0xEF => SET(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 5 } }),
            0xF0 => SET(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 6 } }),
            0xF1 => SET(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 6 } }),
            0xF2 => SET(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 6 } }),
            0xF3 => SET(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 6 } }),
            0xF4 => SET(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 6 } }),
            0xF5 => SET(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 6 } }),
            0xF6 => SETHL(cpu, .{ .bit = 6 }),
            0xF7 => SET(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 6 } }),
            0xF8 => SET(cpu, .{ .bit_target = .{ .target = regID.b, .bit = 7 } }),
            0xF9 => SET(cpu, .{ .bit_target = .{ .target = regID.c, .bit = 7 } }),
            0xFA => SET(cpu, .{ .bit_target = .{ .target = regID.d, .bit = 7 } }),
            0xFB => SET(cpu, .{ .bit_target = .{ .target = regID.e, .bit = 7 } }),
            0xFC => SET(cpu, .{ .bit_target = .{ .target = regID.h, .bit = 7 } }),
            0xFD => SET(cpu, .{ .bit_target = .{ .target = regID.l, .bit = 7 } }),
            0xFE => SETHL(cpu, .{ .bit = 7 }),
            0xFF => SET(cpu, .{ .bit_target = .{ .target = regID.a, .bit = 7 } }),
        },
    };
}

// const GB = @import("gb.zig"); // TODO GET RID OF THIS AND ALL REFERENCES
const CPU = @import("cpu.zig");
// const CPU = cpu;
const regID = CPU.regID;

const std = @import("std");
const print = std.debug.print;