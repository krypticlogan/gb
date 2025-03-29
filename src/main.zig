const std = @import("std");
const g = @cImport({
    @cDefine("SDL_DISABLE_OLD_NAMES", {});
    @cInclude("SDL3/SDL.h");
    @cInclude("SDL3/SDL_revision.h");
    @cDefine("SDL_MAIN_HANDLED", {});
    @cInclude("SDL3/SDL_main.h");
});
const print = std.debug.print;
fn println(comptime fmt: []const u8, args: anytype) void {
    print(fmt, args);
    print("\n", .{});
}
const allocator = std.heap.page_allocator;

const FlagRegister = struct {
    value: u8 = 0,
    z: bool = false,
    s: bool = false,
    h: bool = false,
    c: bool = false,
    const Conditions = union(enum) { z: bool, c: bool, s: bool, h: bool, none: bool };

    fn check(self: *@This()) void {
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
        self.check();
        println("new flags: z: {any}, c: {any}, s: {any}, h: {any}", .{ self.z, self.c, self.s, self.h });
    }
};

const InstructionSet = struct {
    const InstrFn = ?*const fn (*GB, InstrArgs) void;
    const InstrArgs = union(enum) { none: void, target: regID, bit_target: struct { bit: u3, target: regID }, flagConditions: FlagRegister.Conditions, targets: struct { to: regID, from: regID } };

    const table = [_]struct { u8, InstrFn, InstrArgs, u8 }{
        .{ 0x00, NOP, .{ .none = {} }, 1 }, // NOP
        .{ 0x01, LD16, .{ .target = regID.b }, 3 }, // LD BC, d16
        .{ 0x02, null, .{ .none = {} }, 8 }, // LD (BC), A
        .{ 0x03, null, .{ .none = {} }, 8 }, // INC BC
        .{ 0x04, INCr8, .{ .target = regID.b }, 1 }, // INC B
        .{ 0x05, DECr8, .{ .target = regID.b }, 1 }, // DEC B
        .{ 0x06, LD8, .{ .target = regID.b }, 2 }, // LD B, d8
        .{ 0x07, null, .{ .none = {} }, 4 }, // RLCA
        .{ 0x08, null, .{ .none = {} }, 20 }, // LD (a16), SP
        .{ 0x09, null, .{ .none = {} }, 8 }, // ADD HL, BC
        .{ 0x0A, null, .{ .none = {} }, 8 }, // LD A, (BC)
        .{ 0x0B, null, .{ .none = {} }, 8 }, // DEC BC
        .{ 0x0C, INCr8, .{ .target = regID.c }, 1 }, // INC C
        .{ 0x0D, DECr8, .{ .target = regID.c }, 1 }, // DEC C
        .{ 0x0E, LD8, .{ .target = regID.c }, 2 }, // LD C, d8
        .{ 0x0F, RRCA, .{ .none = {} }, 1 }, // RRCA
        .{ 0x10, null, .{ .none = {} }, 4 }, // STOP 0
        .{ 0x11, LD16, .{ .target = regID.d }, 3 }, // LD DE, d16
        .{ 0x12, null, .{ .none = {} }, 8 }, // LD (DE), A
        .{ 0x13, INCr16, .{ .target = regID.d }, 2 }, // INC DE
        .{ 0x14, INCr8, .{ .target = regID.d }, 1 }, // INC D
        .{ 0x15, DECr8, .{ .target = regID.d }, 1 }, // DEC D
        .{ 0x16, LD8, .{ .target = regID.d }, 2 }, // LD D, d8
        .{ 0x17, null, .{ .none = {} }, 4 }, // RLA
        .{ 0x18, JR, .{ .flagConditions = .{ .none = true } }, 3 }, // JR r8
        .{ 0x19, ADDHLr16, .{ .target = regID.d }, 2 }, // ADD HL, DE
        .{ 0x1A, LDAr16, .{ .target = regID.d }, 2 }, // LD A, (DE)
        .{ 0x1B, DECr16, .{ .target = regID.d }, 2 }, // DEC DE
        .{ 0x1C, INCr8, .{ .target = regID.e }, 1 }, // INC E
        .{ 0x1D, DECr8, .{ .target = regID.e }, 1 }, // DEC E
        .{ 0x1E, LD8, .{ .target = regID.e }, 2 }, // LD E, d8
        .{ 0x1F, null, .{ .none = {} }, 4 }, // RRA
        .{ 0x20, JR, .{ .flagConditions = .{ .z = false } }, 3 }, // JR NZ, r8
        .{ 0x21, LD16, .{ .target = regID.h }, 3 }, // LD HL, d16
        .{ 0x22, LDHLIA, .{ .none = {} }, 2 }, // LD (HL+), A
        .{ 0x23, INCr16, .{ .target = regID.h }, 2 }, // INC HL
        .{ 0x24, INCr8, .{ .target = regID.h }, 1 }, // INC H
        .{ 0x25, null, .{ .none = {} }, 4 }, // DEC H
        .{ 0x26, null, .{ .none = {} }, 8 }, // LD H, d8
        .{ 0x27, null, .{ .none = {} }, 4 }, // DAA
        .{ 0x28, JR, .{ .flagConditions = .{ .z = true } }, 3 }, // JR Z, r8
        .{ 0x29, null, .{ .none = {} }, 8 }, // ADD HL, HL
        .{ 0x2A, null, .{ .none = {} }, 8 }, // LD A, (HL+)
        .{ 0x2B, null, .{ .none = {} }, 8 }, // DEC HL
        .{ 0x2C, null, .{ .none = {} }, 4 }, // INC L
        .{ 0x2D, null, .{ .none = {} }, 4 }, // DEC L
        .{ 0x2E, LD8, .{ .target = regID.l }, 2 }, // LD L, d8
        .{ 0x2F, null, .{ .none = {} }, 4 }, // CPL
        .{ 0x30, null, .{ .none = {} }, 12 }, // JR NC, r8
        .{ 0x31, LDSP16, .{ .none = {} }, 3 }, // LD SP, d16
        .{ 0x32, LDHLDA, .{ .none = {} }, 2 }, // LD (HL-), A
        .{ 0x33, null, .{ .none = {} }, 8 }, // INC SP
        .{ 0x34, null, .{ .none = {} }, 12 }, // INC (HL)
        .{ 0x35, null, .{ .none = {} }, 12 }, // DEC (HL)
        .{ 0x36, null, .{ .none = {} }, 12 }, // LD (HL), d8
        .{ 0x37, null, .{ .none = {} }, 4 }, // SCF
        .{ 0x38, null, .{ .none = {} }, 12 }, // JR C, r8
        .{ 0x39, null, .{ .none = {} }, 8 }, // ADD HL, SP
        .{ 0x3A, null, .{ .none = {} }, 8 }, // LD A, (HL-)
        .{ 0x3B, null, .{ .none = {} }, 8 }, // DEC SP
        .{ 0x3C, null, .{ .none = {} }, 4 }, // INC A
        .{ 0x3D, DECr8, .{ .target = regID.a }, 1 }, // DEC A
        .{ 0x3E, LD8, .{ .target = regID.a }, 1 }, // LD A, d8
        .{ 0x3F, null, .{ .none = {} }, 4 }, // CCF
        .{ 0x40, null, .{ .none = {} }, 4 }, // LD B, B
        .{ 0x41, null, .{ .none = {} }, 4 }, // LD B, C
        .{ 0x42, null, .{ .none = {} }, 4 }, // LD B, D
        .{ 0x43, null, .{ .none = {} }, 4 }, // LD B, E
        .{ 0x44, null, .{ .none = {} }, 4 }, // LD B, H
        .{ 0x45, null, .{ .none = {} }, 4 }, // LD B, L
        .{ 0x46, null, .{ .none = {} }, 8 }, // LD B, (HL)
        .{ 0x47, null, .{ .none = {} }, 4 }, // LD B, A
        .{ 0x48, null, .{ .none = {} }, 4 }, // LD C, B
        .{ 0x49, null, .{ .none = {} }, 4 }, // LD C, C
        .{ 0x4A, null, .{ .none = {} }, 4 }, // LD C, D
        .{ 0x4B, null, .{ .none = {} }, 4 }, // LD C, E
        .{ 0x4C, null, .{ .none = {} }, 4 }, // LD C, H
        .{ 0x4D, null, .{ .none = {} }, 4 }, // LD C, L
        .{ 0x4E, null, .{ .none = {} }, 8 }, // LD C, (HL)
        .{ 0x4F, LDr8, .{ .targets = .{ .to = regID.c, .from = regID.a } }, 1 }, // LD C, A
        .{ 0x50, null, .{ .none = {} }, 4 }, // LD D, B
        .{ 0x51, null, .{ .none = {} }, 4 }, // LD D, C
        .{ 0x52, null, .{ .none = {} }, 4 }, // LD D, D
        .{ 0x53, null, .{ .none = {} }, 4 }, // LD D, E
        .{ 0x54, null, .{ .none = {} }, 4 }, // LD D, H
        .{ 0x55, null, .{ .none = {} }, 4 }, // LD D, L
        .{ 0x56, null, .{ .none = {} }, 8 }, // LD D, (HL)
        .{ 0x57, LDr8, .{ .targets = .{ .to = regID.d, .from = regID.a } }, 1 }, // LD D, A
        .{ 0x58, null, .{ .none = {} }, 4 }, // LD E, B
        .{ 0x59, null, .{ .none = {} }, 4 }, // LD E, C
        .{ 0x5A, null, .{ .none = {} }, 4 }, // LD E, D
        .{ 0x5B, null, .{ .none = {} }, 4 }, // LD E, E
        .{ 0x5C, null, .{ .none = {} }, 4 }, // LD E, H
        .{ 0x5D, null, .{ .none = {} }, 4 }, // LD E, L
        .{ 0x5E, null, .{ .none = {} }, 8 }, // LD E, (HL)
        .{ 0x5F, null, .{ .none = {} }, 4 }, // LD E, A
        .{ 0x60, null, .{ .none = {} }, 4 }, // LD H, B
        .{ 0x61, null, .{ .none = {} }, 4 }, // LD H, C
        .{ 0x62, null, .{ .none = {} }, 4 }, // LD H, D
        .{ 0x63, null, .{ .none = {} }, 4 }, // LD H, E
        .{ 0x64, null, .{ .none = {} }, 4 }, // LD H, H
        .{ 0x65, null, .{ .none = {} }, 4 }, // LD H, L
        .{ 0x66, null, .{ .none = {} }, 8 }, // LD H, (HL)
        .{ 0x67, LDr8, .{ .targets = .{ .to = regID.h, .from = regID.a } }, 1 }, // LD H, A
        .{ 0x68, null, .{ .none = {} }, 4 }, // LD L, B
        .{ 0x69, null, .{ .none = {} }, 4 }, // LD L, C
        .{ 0x6A, null, .{ .none = {} }, 4 }, // LD L, D
        .{ 0x6B, null, .{ .none = {} }, 4 }, // LD L, E
        .{ 0x6C, null, .{ .none = {} }, 4 }, // LD L, H
        .{ 0x6D, null, .{ .none = {} }, 4 }, // LD L, L
        .{ 0x6E, null, .{ .none = {} }, 8 }, // LD L, (HL)
        .{ 0x6F, null, .{ .none = {} }, 4 }, // LD L, A
        .{ 0x70, null, .{ .none = {} }, 8 }, // LD (HL), B
        .{ 0x71, null, .{ .none = {} }, 8 }, // LD (HL), C
        .{ 0x72, null, .{ .none = {} }, 8 }, // LD (HL), D
        .{ 0x73, null, .{ .none = {} }, 8 }, // LD (HL), E
        .{ 0x74, null, .{ .none = {} }, 8 }, // LD (HL), H
        .{ 0x75, null, .{ .none = {} }, 8 }, // LD (HL), L
        .{ 0x76, null, .{ .none = {} }, 4 }, // HALT
        .{ 0x77, LDHLR, .{ .target = regID.a }, 2 }, // LD (HL), A
        .{ 0x78, LDr8, .{ .targets = .{ .to = regID.a, .from = regID.b } }, 1 }, // LD A, B
        .{ 0x79, LDr8, .{ .targets = .{ .to = regID.a, .from = regID.c } }, 1 }, // LD A, C
        .{ 0x7A, LDr8, .{ .targets = .{ .to = regID.a, .from = regID.d } }, 1 }, // LD A, D
        .{ 0x7B, LDr8, .{ .targets = .{ .to = regID.a, .from = regID.e } }, 1 }, // LD A, E
        .{ 0x7C, LDr8, .{ .targets = .{ .to = regID.a, .from = regID.h } }, 1 }, // LD A, H
        .{ 0x7D, LDr8, .{ .targets = .{ .to = regID.a, .from = regID.l } }, 1 }, // LD A, L
        .{ 0x7E, LDAHL, .{ .none = {} }, 8 }, // LD A, (HL)
        .{ 0x7F, LDr8, .{ .targets = .{ .to = regID.a, .from = regID.a } }, 1 }, // LD A, A
        .{ 0x80, ADDAr8, .{ .target = regID.b }, 1 }, // ADD A, B
        .{ 0x81, ADDAr8, .{ .target = regID.c }, 1 }, // ADD A, C
        .{ 0x82, ADDAr8, .{ .target = regID.d }, 1 }, // ADD A, D
        .{ 0x83, ADDAr8, .{ .target = regID.e }, 1 }, // ADD A, E
        .{ 0x84, ADDAr8, .{ .target = regID.h }, 1 }, // ADD A, H
        .{ 0x85, ADDAr8, .{ .target = regID.l }, 1 }, // ADD A, L
        .{ 0x86, ADDAHL, .{ .none = {} }, 2 }, // ADD A, (HL)
        .{ 0x87, null, .{ .none = {} }, 4 }, // ADD A, A
        .{ 0x88, null, .{ .none = {} }, 4 }, // ADC A, B
        .{ 0x89, null, .{ .none = {} }, 4 }, // ADC A, C
        .{ 0x8A, null, .{ .none = {} }, 4 }, // ADC A, D
        .{ 0x8B, null, .{ .none = {} }, 4 }, // ADC A, E
        .{ 0x8C, null, .{ .none = {} }, 4 }, // ADC A, H
        .{ 0x8D, null, .{ .none = {} }, 4 }, // ADC A, L
        .{ 0x8E, null, .{ .none = {} }, 8 }, // ADC A, (HL)
        .{ 0x8F, null, .{ .none = {} }, 4 }, // ADC A, A
        .{ 0x90, SUBAr8, .{ .target = regID.b }, 1 }, // SUB B
        .{ 0x91, SUBAr8, .{ .target = regID.c }, 1 }, // SUB C
        .{ 0x92, SUBAr8, .{ .target = regID.d }, 1 }, // SUB D
        .{ 0x93, SUBAr8, .{ .target = regID.e }, 1 }, // SUB E
        .{ 0x94, SUBAr8, .{ .target = regID.h }, 1 }, // SUB H
        .{ 0x95, SUBAr8, .{ .target = regID.l }, 1 }, // SUB L
        .{ 0x96, null, .{ .none = {} }, 8 }, // SUB (HL)
        .{ 0x97, SUBAr8, .{ .target = regID.a }, 1 }, // SUB A
        .{ 0x98, null, .{ .none = {} }, 4 }, // SBC A, B
        .{ 0x99, null, .{ .none = {} }, 4 }, // SBC A, C
        .{ 0x9A, null, .{ .none = {} }, 4 }, // SBC A, D
        .{ 0x9B, null, .{ .none = {} }, 4 }, // SBC A, E
        .{ 0x9C, null, .{ .none = {} }, 4 }, // SBC A, H
        .{ 0x9D, null, .{ .none = {} }, 4 }, // SBC A, L
        .{ 0x9E, null, .{ .none = {} }, 8 }, // SBC A, (HL)
        .{ 0x9F, null, .{ .none = {} }, 4 }, // SBC A, A
        .{ 0xA0, null, .{ .none = {} }, 4 }, // AND B
        .{ 0xA1, null, .{ .none = {} }, 4 }, // AND C
        .{ 0xA2, null, .{ .none = {} }, 4 }, // AND D
        .{ 0xA3, null, .{ .none = {} }, 4 }, // AND E
        .{ 0xA4, null, .{ .none = {} }, 4 }, // AND H
        .{ 0xA5, null, .{ .none = {} }, 4 }, // AND L
        .{ 0xA6, null, .{ .none = {} }, 8 }, // AND (HL)
        .{ 0xA7, null, .{ .none = {} }, 4 }, // AND A
        .{ 0xA8, null, .{ .none = {} }, 4 }, // XOR B
        .{ 0xA9, null, .{ .none = {} }, 4 }, // XOR C
        .{ 0xAA, null, .{ .none = {} }, 4 }, // XOR D
        .{ 0xAB, null, .{ .none = {} }, 4 }, // XOR E
        .{ 0xAC, null, .{ .none = {} }, 4 }, // XOR H
        .{ 0xAD, null, .{ .none = {} }, 4 }, // XOR L
        .{ 0xAE, null, .{ .none = {} }, 8 }, // XOR (HL)
        .{ 0xAF, XORA, .{ .target = regID.a }, 1 }, // XOR A
        .{ 0xB0, null, .{ .none = {} }, 4 }, // OR B
        .{ 0xB1, null, .{ .none = {} }, 4 }, // OR C
        .{ 0xB2, null, .{ .none = {} }, 4 }, // OR D
        .{ 0xB3, null, .{ .none = {} }, 4 }, // OR E
        .{ 0xB4, null, .{ .none = {} }, 4 }, // OR H
        .{ 0xB5, null, .{ .none = {} }, 4 }, // OR L
        .{ 0xB6, null, .{ .none = {} }, 8 }, // OR (HL)
        .{ 0xB7, null, .{ .none = {} }, 4 }, // OR A
        .{ 0xB8, null, .{ .none = {} }, 4 }, // CP B
        .{ 0xB9, null, .{ .none = {} }, 4 }, // CP C
        .{ 0xBA, null, .{ .none = {} }, 4 }, // CP D
        .{ 0xBB, null, .{ .none = {} }, 4 }, // CP E
        .{ 0xBC, null, .{ .none = {} }, 4 }, // CP H
        .{ 0xBD, null, .{ .none = {} }, 4 }, // CP L
        .{ 0xBE, CPAHL, .{ .none = {} }, 2 }, // CP (HL)
        .{ 0xBF, null, .{ .none = {} }, 4 }, // CP A
        .{ 0xC0, null, .{ .none = {} }, 20 }, // RET NZ
        .{ 0xC1, POP, .{ .target = regID.b }, 3 }, // POP BC
        .{ 0xC2, null, .{ .none = {} }, 16 }, // JP NZ, a16
        .{ 0xC3, null, .{ .none = {} }, 16 }, // JP a16
        .{ 0xC4, null, .{ .none = {} }, 24 }, // CALL NZ, a16
        .{ 0xC5, PUSH, .{ .target = regID.b }, 1 }, // PUSH BC
        .{ 0xC6, null, .{ .none = {} }, 8 }, // ADD A, d8
        .{ 0xC7, null, .{ .none = {} }, 16 }, // RST 00H
        .{ 0xC8, null, .{ .none = {} }, 20 }, // RET Z
        .{ 0xC9, RET, .{ .flagConditions = .{ .none = true } }, 4 }, // RET
        .{ 0xCA, null, .{ .none = {} }, 16 }, // JP Z, a16
        .{ 0xCB, null, .{ .none = {} }, 4 }, // PREFIX CB
        .{ 0xCC, null, .{ .none = {} }, 24 }, // CALL Z, a16
        .{ 0xCD, CALLn16, .{ .flagConditions = .{ .none = true } }, 6 }, // CALL a16
        .{ 0xCE, null, .{ .none = {} }, 8 }, // ADC A, d8
        .{ 0xCF, null, .{ .none = {} }, 16 }, // RST 08H
        .{ 0xD0, null, .{ .none = {} }, 20 }, // RET NC
        .{ 0xD1, null, .{ .none = {} }, 12 }, // POP DE
        .{ 0xD2, null, .{ .none = {} }, 16 }, // JP NC, a16
        .{ 0xD3, null, .{ .none = {} }, 4 }, // NOP
        .{ 0xD4, null, .{ .none = {} }, 24 }, // CALL NC, a16
        .{ 0xD5, null, .{ .none = {} }, 16 }, // PUSH DE
        .{ 0xD6, null, .{ .none = {} }, 8 }, // SUB d8
        .{ 0xD7, null, .{ .none = {} }, 16 }, // RST 10H
        .{ 0xD8, null, .{ .none = {} }, 20 }, // RET C
        .{ 0xD9, null, .{ .none = {} }, 16 }, // RETI
        .{ 0xDA, null, .{ .none = {} }, 16 }, // JP C, a16
        .{ 0xDB, null, .{ .none = {} }, 4 }, // NOP
        .{ 0xDC, null, .{ .none = {} }, 24 }, // CALL C, a16
        .{ 0xDD, null, .{ .none = {} }, 4 }, // NOP
        .{ 0xDE, null, .{ .none = {} }, 8 }, // SBC A, d8
        .{ 0xDF, null, .{ .none = {} }, 16 }, // RST 18H
        .{ 0xE0, LDHn16A, .{ .none = {} }, 3 }, // LDH (a8), A
        .{ 0xE1, null, .{ .none = {} }, 12 }, // POP HL
        .{ 0xE2, LDHCA, .{ .none = {} }, 2 }, // LD (C), A
        .{ 0xE3, null, .{ .none = {} }, 4 }, // NOP
        .{ 0xE4, null, .{ .none = {} }, 4 }, // NOP
        .{ 0xE5, null, .{ .none = {} }, 16 }, // PUSH HL
        .{ 0xE6, null, .{ .none = {} }, 8 }, // AND d8
        .{ 0xE7, null, .{ .none = {} }, 16 }, // RST 20H
        .{ 0xE8, null, .{ .none = {} }, 16 }, // ADD SP, r8
        .{ 0xE9, null, .{ .none = {} }, 4 }, // JP (HL)
        .{ 0xEA, LDn16A, .{ .none = {} }, 4 }, // LD (a16), A
        .{ 0xEB, null, .{ .none = {} }, 4 }, // NOP
        .{ 0xEC, null, .{ .none = {} }, 4 }, // NOP
        .{ 0xED, null, .{ .none = {} }, 4 }, // NOP
        .{ 0xEE, null, .{ .none = {} }, 8 }, // XOR d8
        .{ 0xEF, null, .{ .none = {} }, 16 }, // RST 28H
        .{ 0xF0, LDHAn16, .{ .none = {} }, 3 }, // LDH A, (a8)
        .{ 0xF1, null, .{ .none = {} }, 12 }, // POP AF
        .{ 0xF2, null, .{ .none = {} }, 8 }, // LD A, (C)
        .{ 0xF3, null, .{ .none = {} }, 4 }, // DI
        .{ 0xF4, null, .{ .none = {} }, 4 }, // NOP
        .{ 0xF5, null, .{ .none = {} }, 16 }, // PUSH AF
        .{ 0xF6, null, .{ .none = {} }, 8 }, // OR d8
        .{ 0xF7, null, .{ .none = {} }, 16 }, // RST 30H
        .{ 0xF8, null, .{ .none = {} }, 12 }, // LD HL, SP+r8
        .{ 0xF9, null, .{ .none = {} }, 8 }, // LD SP, HL
        .{ 0xFA, null, .{ .none = {} }, 16 }, // LD A, (a16)
        .{ 0xFB, EI, .{ .none = {} }, 1 }, // EI
        .{ 0xFC, null, .{ .none = {} }, 4 }, // NOP
        .{ 0xFD, null, .{ .none = {} }, 4 }, // NOP
        .{ 0xFE, CPAn8, .{ .none = {} }, 2 }, // CP d8
        .{ 0xFF, null, .{ .none = {} }, 16 }, // RST 38H
    };

    const prefix_table = [_]struct { u8, InstrFn, InstrArgs, u8 }{
        .{ 0x00, null, .{ .none = {} }, 8 }, // RLC B
        .{ 0x01, null, .{ .none = {} }, 8 }, // RLC C
        .{ 0x02, null, .{ .none = {} }, 8 }, // RLC D
        .{ 0x03, null, .{ .none = {} }, 8 }, // RLC E
        .{ 0x04, null, .{ .none = {} }, 8 }, // RLC H
        .{ 0x05, null, .{ .none = {} }, 8 }, // RLC L
        .{ 0x06, null, .{ .none = {} }, 16 }, // RLC (HL)
        .{ 0x07, null, .{ .none = {} }, 8 }, // RLC A
        .{ 0x08, null, .{ .none = {} }, 8 }, // RRC B
        .{ 0x09, null, .{ .none = {} }, 8 }, // RRC C
        .{ 0x0A, null, .{ .none = {} }, 8 }, // RRC D
        .{ 0x0B, null, .{ .none = {} }, 8 }, // RRC E
        .{ 0x0C, null, .{ .none = {} }, 8 }, // RRC H
        .{ 0x0D, null, .{ .none = {} }, 8 }, // RRC L
        .{ 0x0E, null, .{ .none = {} }, 16 }, // RRC (HL)
        .{ 0x0F, null, .{ .none = {} }, 8 }, // RRC A
        .{ 0x10, null, .{ .none = {} }, 8 }, // RL B
        .{ 0x11, RL, .{ .target = regID.c }, 8 }, // RL C
        .{ 0x12, null, .{ .none = {} }, 8 }, // RL D
        .{ 0x13, null, .{ .none = {} }, 8 }, // RL E
        .{ 0x14, null, .{ .none = {} }, 8 }, // RL H
        .{ 0x15, null, .{ .none = {} }, 8 }, // RL L
        .{ 0x16, null, .{ .none = {} }, 16 }, // RL (HL)
        .{ 0x17, null, .{ .none = {} }, 8 }, // RL A
        .{ 0x18, null, .{ .none = {} }, 8 }, // RR B
        .{ 0x19, null, .{ .none = {} }, 8 }, // RR C
        .{ 0x1A, null, .{ .none = {} }, 8 }, // RR D
        .{ 0x1B, null, .{ .none = {} }, 8 }, // RR E
        .{ 0x1C, null, .{ .none = {} }, 8 }, // RR H
        .{ 0x1D, null, .{ .none = {} }, 8 }, // RR L
        .{ 0x1E, null, .{ .none = {} }, 16 }, // RR (HL)
        .{ 0x1F, null, .{ .none = {} }, 8 }, // RR A
        .{ 0x20, null, .{ .none = {} }, 8 }, // SLA B
        .{ 0x21, null, .{ .none = {} }, 8 }, // SLA C
        .{ 0x22, null, .{ .none = {} }, 8 }, // SLA D
        .{ 0x23, null, .{ .none = {} }, 8 }, // SLA E
        .{ 0x24, null, .{ .none = {} }, 8 }, // SLA H
        .{ 0x25, null, .{ .none = {} }, 8 }, // SLA L
        .{ 0x26, null, .{ .none = {} }, 16 }, // SLA (HL)
        .{ 0x27, null, .{ .none = {} }, 8 }, // SLA A
        .{ 0x28, null, .{ .none = {} }, 8 }, // SRA B
        .{ 0x29, null, .{ .none = {} }, 8 }, // SRA C
        .{ 0x2A, null, .{ .none = {} }, 8 }, // SRA D
        .{ 0x2B, null, .{ .none = {} }, 8 }, // SRA E
        .{ 0x2C, null, .{ .none = {} }, 8 }, // SRA H
        .{ 0x2D, null, .{ .none = {} }, 8 }, // SRA L
        .{ 0x2E, null, .{ .none = {} }, 16 }, // SRA (HL)
        .{ 0x2F, null, .{ .none = {} }, 8 }, // SRA A
        .{ 0x30, null, .{ .none = {} }, 8 }, // SWAP B
        .{ 0x31, null, .{ .none = {} }, 8 }, // SWAP C
        .{ 0x32, null, .{ .none = {} }, 8 }, // SWAP D
        .{ 0x33, null, .{ .none = {} }, 8 }, // SWAP E
        .{ 0x34, null, .{ .none = {} }, 8 }, // SWAP H
        .{ 0x35, null, .{ .none = {} }, 8 }, // SWAP L
        .{ 0x36, null, .{ .none = {} }, 16 }, // SWAP (HL)
        .{ 0x37, null, .{ .none = {} }, 8 }, // SWAP A
        .{ 0x38, null, .{ .none = {} }, 8 }, // SRL B
        .{ 0x39, null, .{ .none = {} }, 8 }, // SRL C
        .{ 0x3A, null, .{ .none = {} }, 8 }, // SRL D
        .{ 0x3B, null, .{ .none = {} }, 8 }, // SRL E
        .{ 0x3C, null, .{ .none = {} }, 8 }, // SRL H
        .{ 0x3D, null, .{ .none = {} }, 8 }, // SRL L
        .{ 0x3E, null, .{ .none = {} }, 16 }, // SRL (HL)
        .{ 0x3F, null, .{ .none = {} }, 8 }, // SRL A
        .{ 0x40, null, .{ .none = {} }, 8 }, // BIT 0, B
        .{ 0x41, null, .{ .none = {} }, 8 }, // BIT 0, C
        .{ 0x42, null, .{ .none = {} }, 8 }, // BIT 0, D
        .{ 0x43, null, .{ .none = {} }, 8 }, // BIT 0, E
        .{ 0x44, null, .{ .none = {} }, 8 }, // BIT 0, H
        .{ 0x45, null, .{ .none = {} }, 8 }, // BIT 0, L
        .{ 0x46, null, .{ .none = {} }, 12 }, // BIT 0, (HL)
        .{ 0x47, null, .{ .none = {} }, 8 }, // BIT 0, A
        .{ 0x48, null, .{ .none = {} }, 8 }, // BIT 1, B
        .{ 0x49, null, .{ .none = {} }, 8 }, // BIT 1, C
        .{ 0x4A, null, .{ .none = {} }, 8 }, // BIT 1, D
        .{ 0x4B, null, .{ .none = {} }, 8 }, // BIT 1, E
        .{ 0x4C, null, .{ .none = {} }, 8 }, // BIT 1, H
        .{ 0x4D, null, .{ .none = {} }, 8 }, // BIT 1, L
        .{ 0x4E, null, .{ .none = {} }, 12 }, // BIT 1, (HL)
        .{ 0x4F, null, .{ .none = {} }, 8 }, // BIT 1, A
        .{ 0x50, null, .{ .none = {} }, 8 }, // BIT 2, B
        .{ 0x51, null, .{ .none = {} }, 8 }, // BIT 2, C
        .{ 0x52, null, .{ .none = {} }, 8 }, // BIT 2, D
        .{ 0x53, null, .{ .none = {} }, 8 }, // BIT 2, E
        .{ 0x54, null, .{ .none = {} }, 8 }, // BIT 2, H
        .{ 0x55, null, .{ .none = {} }, 8 }, // BIT 2, L
        .{ 0x56, null, .{ .none = {} }, 12 }, // BIT 2, (HL)
        .{ 0x57, null, .{ .none = {} }, 8 }, // BIT 2, A
        .{ 0x58, null, .{ .none = {} }, 8 }, // BIT 3, B
        .{ 0x59, null, .{ .none = {} }, 8 }, // BIT 3, C
        .{ 0x5A, null, .{ .none = {} }, 8 }, // BIT 3, D
        .{ 0x5B, null, .{ .none = {} }, 8 }, // BIT 3, E
        .{ 0x5C, null, .{ .none = {} }, 8 }, // BIT 3, H
        .{ 0x5D, null, .{ .none = {} }, 8 }, // BIT 3, L
        .{ 0x5E, null, .{ .none = {} }, 12 }, // BIT 3, (HL)
        .{ 0x5F, null, .{ .none = {} }, 8 }, // BIT 3, A
        .{ 0x60, null, .{ .none = {} }, 8 }, // BIT 4, B
        .{ 0x61, null, .{ .none = {} }, 8 }, // BIT 4, C
        .{ 0x62, null, .{ .none = {} }, 8 }, // BIT 4, D
        .{ 0x63, null, .{ .none = {} }, 8 }, // BIT 4, E
        .{ 0x64, null, .{ .none = {} }, 8 }, // BIT 4, H
        .{ 0x65, null, .{ .none = {} }, 8 }, // BIT 4, L
        .{ 0x66, null, .{ .none = {} }, 12 }, // BIT 4, (HL)
        .{ 0x67, null, .{ .none = {} }, 8 }, // BIT 4, A
        .{ 0x68, null, .{ .none = {} }, 8 }, // BIT 5, B
        .{ 0x69, null, .{ .none = {} }, 8 }, // BIT 5, C
        .{ 0x6A, null, .{ .none = {} }, 8 }, // BIT 5, D
        .{ 0x6B, null, .{ .none = {} }, 8 }, // BIT 5, E
        .{ 0x6C, null, .{ .none = {} }, 8 }, // BIT 5, H
        .{ 0x6D, null, .{ .none = {} }, 8 }, // BIT 5, L
        .{ 0x6E, null, .{ .none = {} }, 12 }, // BIT 5, (HL)
        .{ 0x6F, null, .{ .none = {} }, 8 }, // BIT 5, A
        .{ 0x70, null, .{ .none = {} }, 8 }, // BIT 6, B
        .{ 0x71, null, .{ .none = {} }, 8 }, // BIT 6, C
        .{ 0x72, null, .{ .none = {} }, 8 }, // BIT 6, D
        .{ 0x73, null, .{ .none = {} }, 8 }, // BIT 6, E
        .{ 0x74, null, .{ .none = {} }, 8 }, // BIT 6, H
        .{ 0x75, null, .{ .none = {} }, 8 }, // BIT 6, L
        .{ 0x76, null, .{ .none = {} }, 12 }, // BIT 6, (HL)
        .{ 0x77, null, .{ .none = {} }, 8 }, // BIT 6, A
        .{ 0x78, null, .{ .none = {} }, 8 }, // BIT 7, B
        .{ 0x79, null, .{ .none = {} }, 8 }, // BIT 7, C
        .{ 0x7A, null, .{ .none = {} }, 8 }, // BIT 7, D
        .{ 0x7B, null, .{ .none = {} }, 8 }, // BIT 7, E
        .{ 0x7C, BITTEST, .{ .bit_target = .{ .bit = 7, .target = regID.h } }, 3 }, // BIT 7, H
        .{ 0x7D, null, .{ .none = {} }, 8 }, // BIT 7, L
        .{ 0x7E, null, .{ .none = {} }, 12 }, // BIT 7, (HL)
        .{ 0x7F, null, .{ .none = {} }, 8 }, // BIT 7, A
        .{ 0x80, null, .{ .none = {} }, 8 }, // RES 0, B
        .{ 0x81, null, .{ .none = {} }, 8 }, // RES 0, C
        .{ 0x82, null, .{ .none = {} }, 8 }, // RES 0, D
        .{ 0x83, null, .{ .none = {} }, 8 }, // RES 0, E
        .{ 0x84, null, .{ .none = {} }, 8 }, // RES 0, H
        .{ 0x85, null, .{ .none = {} }, 8 }, // RES 0, L
        .{ 0x86, null, .{ .none = {} }, 16 }, // RES 0, (HL)
        .{ 0x87, null, .{ .none = {} }, 8 }, // RES 0, A
        .{ 0x88, null, .{ .none = {} }, 8 }, // RES 1, B
        .{ 0x89, null, .{ .none = {} }, 8 }, // RES 1, C
        .{ 0x8A, null, .{ .none = {} }, 8 }, // RES 1, D
        .{ 0x8B, null, .{ .none = {} }, 8 }, // RES 1, E
        .{ 0x8C, null, .{ .none = {} }, 8 }, // RES 1, H
        .{ 0x8D, null, .{ .none = {} }, 8 }, // RES 1, L
        .{ 0x8E, null, .{ .none = {} }, 16 }, // RES 1, (HL)
        .{ 0x8F, null, .{ .none = {} }, 8 }, // RES 1, A
        .{ 0x90, null, .{ .none = {} }, 8 }, // RES 2, B
        .{ 0x91, null, .{ .none = {} }, 8 }, // RES 2, C
        .{ 0x92, null, .{ .none = {} }, 8 }, // RES 2, D
        .{ 0x93, null, .{ .none = {} }, 8 }, // RES 2, E
        .{ 0x94, null, .{ .none = {} }, 8 }, // RES 2, H
        .{ 0x95, null, .{ .none = {} }, 8 }, // RES 2, L
        .{ 0x96, null, .{ .none = {} }, 16 }, // RES 2, (HL)
        .{ 0x97, null, .{ .none = {} }, 8 }, // RES 2, A
        .{ 0x98, null, .{ .none = {} }, 8 }, // RES 3, B
        .{ 0x99, null, .{ .none = {} }, 8 }, // RES 3, C
        .{ 0x9A, null, .{ .none = {} }, 8 }, // RES 3, D
        .{ 0x9B, null, .{ .none = {} }, 8 }, // RES 3, E
        .{ 0x9C, null, .{ .none = {} }, 8 }, // RES 3, H
        .{ 0x9D, null, .{ .none = {} }, 8 }, // RES 3, L
        .{ 0x9E, null, .{ .none = {} }, 16 }, // RES 3, (HL)
        .{ 0x9F, null, .{ .none = {} }, 8 }, // RES 3, A
        .{ 0xA0, null, .{ .none = {} }, 8 }, // RES 4, B
        .{ 0xA1, null, .{ .none = {} }, 8 }, // RES 4, C
        .{ 0xA2, null, .{ .none = {} }, 8 }, // RES 4, D
        .{ 0xA3, null, .{ .none = {} }, 8 }, // RES 4, E
        .{ 0xA4, null, .{ .none = {} }, 8 }, // RES 4, H
        .{ 0xA5, null, .{ .none = {} }, 8 }, // RES 4, L
        .{ 0xA6, null, .{ .none = {} }, 16 }, // RES 4, (HL)
        .{ 0xA7, null, .{ .none = {} }, 8 }, // RES 4, A
        .{ 0xA8, null, .{ .none = {} }, 8 }, // RES 5, B
        .{ 0xA9, null, .{ .none = {} }, 8 }, // RES 5, C
        .{ 0xAA, null, .{ .none = {} }, 8 }, // RES 5, D
        .{ 0xAB, null, .{ .none = {} }, 8 }, // RES 5, E
        .{ 0xAC, null, .{ .none = {} }, 8 }, // RES 5, H
        .{ 0xAD, null, .{ .none = {} }, 8 }, // RES 5, L
        .{ 0xAE, null, .{ .none = {} }, 16 }, // RES 5, (HL)
        .{ 0xAF, null, .{ .none = {} }, 8 }, // RES 5, A
        .{ 0xB0, null, .{ .none = {} }, 8 }, // RES 6, B
        .{ 0xB1, null, .{ .none = {} }, 8 }, // RES 6, C
        .{ 0xB2, null, .{ .none = {} }, 8 }, // RES 6, D
        .{ 0xB3, null, .{ .none = {} }, 8 }, // RES 6, E
        .{ 0xB4, null, .{ .none = {} }, 8 }, // RES 6, H
        .{ 0xB5, null, .{ .none = {} }, 8 }, // RES 6, L
        .{ 0xB6, null, .{ .none = {} }, 16 }, // RES 6, (HL)
        .{ 0xB7, null, .{ .none = {} }, 8 }, // RES 6, A
        .{ 0xB8, null, .{ .none = {} }, 8 }, // RES 7, B
        .{ 0xB9, null, .{ .none = {} }, 8 }, // RES 7, C
        .{ 0xBA, null, .{ .none = {} }, 8 }, // RES 7, D
        .{ 0xBB, null, .{ .none = {} }, 8 }, // RES 7, E
        .{ 0xBC, null, .{ .none = {} }, 8 }, // RES 7, H
        .{ 0xBD, null, .{ .none = {} }, 8 }, // RES 7, L
        .{ 0xBE, null, .{ .none = {} }, 16 }, // RES 7, (HL)
        .{ 0xBF, null, .{ .none = {} }, 8 }, // RES 7, A
        .{ 0xC0, null, .{ .none = {} }, 8 }, // SET 0, B
        .{ 0xC1, null, .{ .none = {} }, 8 }, // SET 0, C
        .{ 0xC2, null, .{ .none = {} }, 8 }, // SET 0, D
        .{ 0xC3, null, .{ .none = {} }, 8 }, // SET 0, E
        .{ 0xC4, null, .{ .none = {} }, 8 }, // SET 0, H
        .{ 0xC5, null, .{ .none = {} }, 8 }, // SET 0, L
        .{ 0xC6, null, .{ .none = {} }, 16 }, // SET 0, (HL)
        .{ 0xC7, null, .{ .none = {} }, 8 }, // SET 0, A
        .{ 0xC8, null, .{ .none = {} }, 8 }, // SET 1, B
        .{ 0xC9, null, .{ .none = {} }, 8 }, // SET 1, C
        .{ 0xCA, null, .{ .none = {} }, 8 }, // SET 1, D
        .{ 0xCB, null, .{ .none = {} }, 8 }, // SET 1, E
        .{ 0xCC, null, .{ .none = {} }, 8 }, // SET 1, H
        .{ 0xCD, null, .{ .none = {} }, 8 }, // SET 1, L
        .{ 0xCE, null, .{ .none = {} }, 16 }, // SET 1, (HL)
        .{ 0xCF, null, .{ .none = {} }, 8 }, // SET 1, A
        .{ 0xD0, null, .{ .none = {} }, 8 }, // SET 2, B
        .{ 0xD1, null, .{ .none = {} }, 8 }, // SET 2, C
        .{ 0xD2, null, .{ .none = {} }, 8 }, // SET 2, D
        .{ 0xD3, null, .{ .none = {} }, 8 }, // SET 2, E
        .{ 0xD4, null, .{ .none = {} }, 8 }, // SET 2, H
        .{ 0xD5, null, .{ .none = {} }, 8 }, // SET 2, L
        .{ 0xD6, null, .{ .none = {} }, 16 }, // SET 2, (HL)
        .{ 0xD7, null, .{ .none = {} }, 8 }, // SET 2, A
        .{ 0xD8, null, .{ .none = {} }, 8 }, // SET 3, B
        .{ 0xD9, null, .{ .none = {} }, 8 }, // SET 3, C
        .{ 0xDA, null, .{ .none = {} }, 8 }, // SET 3, D
        .{ 0xDB, null, .{ .none = {} }, 8 }, // SET 3, E
        .{ 0xDC, null, .{ .none = {} }, 8 }, // SET 3, H
        .{ 0xDD, null, .{ .none = {} }, 8 }, // SET 3, L
        .{ 0xDE, null, .{ .none = {} }, 16 }, // SET 3, (HL)
        .{ 0xDF, null, .{ .none = {} }, 8 }, // SET 3, A
        .{ 0xE0, null, .{ .none = {} }, 8 }, // SET 4, B
        .{ 0xE1, null, .{ .none = {} }, 8 }, // SET 4, C
        .{ 0xE2, null, .{ .none = {} }, 8 }, // SET 4, D
        .{ 0xE3, null, .{ .none = {} }, 8 }, // SET 4, E
        .{ 0xE4, null, .{ .none = {} }, 8 }, // SET 4, H
        .{ 0xE5, null, .{ .none = {} }, 8 }, // SET 4, L
        .{ 0xE6, null, .{ .none = {} }, 16 }, // SET 4, (HL)
        .{ 0xE7, null, .{ .none = {} }, 8 }, // SET 4, A
        .{ 0xE8, null, .{ .none = {} }, 8 }, // SET 5, B
        .{ 0xE9, null, .{ .none = {} }, 8 }, // SET 5, C
        .{ 0xEA, null, .{ .none = {} }, 8 }, // SET 5, D
        .{ 0xEB, null, .{ .none = {} }, 8 }, // SET 5, E
        .{ 0xEC, null, .{ .none = {} }, 8 }, // SET 5, H
        .{ 0xED, null, .{ .none = {} }, 8 }, // SET 5, L
        .{ 0xEE, null, .{ .none = {} }, 16 }, // SET 5, (HL)
        .{ 0xEF, null, .{ .none = {} }, 8 }, // SET 5, A
        .{ 0xF0, null, .{ .none = {} }, 8 }, // SET 6, B
        .{ 0xF1, null, .{ .none = {} }, 8 }, // SET 6, C
        .{ 0xF2, null, .{ .none = {} }, 8 }, // SET 6, D
        .{ 0xF3, null, .{ .none = {} }, 8 }, // SET 6, E
        .{ 0xF4, null, .{ .none = {} }, 8 }, // SET 6, H
        .{ 0xF5, null, .{ .none = {} }, 8 }, // SET 6, L
        .{ 0xF6, null, .{ .none = {} }, 16 }, // SET 6, (HL)
        .{ 0xF7, null, .{ .none = {} }, 8 }, // SET 6, A
        .{ 0xF8, null, .{ .none = {} }, 8 }, // SET 7, B
        .{ 0xF9, null, .{ .none = {} }, 8 }, // SET 7, C
        .{ 0xFA, null, .{ .none = {} }, 8 }, // SET 7, D
        .{ 0xFB, null, .{ .none = {} }, 8 }, // SET 7, E
        .{ 0xFC, null, .{ .none = {} }, 8 }, // SET 7, H
        .{ 0xFD, null, .{ .none = {} }, 8 }, // SET 7, L
        .{ 0xFE, null, .{ .none = {} }, 16 }, // SET 7, (HL)
        .{ 0xFF, null, .{ .none = {} }, 8 }, // SET 7, A
    };

    fn from_byte(byte: u8, prefixed: bool) !struct { ins: *const fn (*GB, InstrArgs) void, args: InstrArgs, cycles: u8 } {
        var ins: ?*const fn (*GB, InstrArgs) void = undefined;
        var args: InstrArgs = undefined;
        var cycles: u8 = undefined;
        if ((byte >= 0 and byte < table.len)) {
            if (prefixed) {
                print("prefixed\n", .{});
                ins = prefix_table[byte][1];
                args = prefix_table[byte][2];
                cycles = prefix_table[byte][3];
            } else {
                ins = table[byte][1];
                args = table[byte][2];
                cycles = prefix_table[byte][3];
            }
            if (ins == null) return error.NullInstruction;
            return .{ .ins = ins.?, .args = args, .cycles = cycles};
        } else return error.IllegalByte;
    }

    fn NOP(gb: *GB, _: InstrArgs) void { // TODO test
        print("NOP\n", .{});
        gb.cpu.pc += 1;
    }
    fn INCr8(gb: *GB, args: InstrArgs) void { // TODO TEST
        const value = gb.cpu.get_byte(args.target);
        print("INCr8, target: {any}\n", .{args.target});
        gb.cpu.set_byte(args.target, @addWithOverflow(value, 1)[0]);
        gb.cpu.f.h = (value & 0xF + 1) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.z = gb.cpu.get_byte(args.target) == 0;
        gb.cpu.f.s = false;
        gb.cpu.f.write();
        gb.cpu.pc += 1;
    }
    fn INCr16(gb: *GB, args: InstrArgs) void { // TODO TEST
        const value = gb.cpu.get_word(args.target);
        const res = @addWithOverflow(value, 1)[0];
        print("INCr16, target: {any}\n 0x{X} + 1 = 0x{X}", .{args.target, value, res});
        gb.cpu.set_word(args.target, res);
        gb.cpu.pc += 1;
    }
    fn DECr8(gb: *GB, args: InstrArgs) void { // TODO TEST -- overflow? r16 too
        const value = gb.cpu.get_byte(args.target);
        print("DECr8, target: {any}\n", .{args.target});
        gb.cpu.set_byte(args.target, @subWithOverflow(value, 1)[0]);
        gb.cpu.f.z = value == 1;
        gb.cpu.f.s = true;
        gb.cpu.f.h = (value & 0xF) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.write();
        gb.cpu.pc += 1;
    }
    fn DECr16(gb: *GB, args: InstrArgs) void { // TODO TEST
        const value = gb.cpu.get_byte(args.target);
        print("DECr16, target: {any}\n", .{args.target});
        gb.cpu.set_word(args.target, @subWithOverflow(value, 1)[0]);
        gb.cpu.pc += 1;
    }
    fn LD8(gb: *GB, args: InstrArgs) void { // LD r8, n8 TODO TEST
        const n: u8 = gb.read_byte(gb.cpu.pc + 1);
        print("LD8, target {any}, n: Ox{X}\n", .{ @as(regID, args.target), n });
        print("n b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.set_byte(args.target, n);
        gb.cpu.pc += 2;
    }
    fn LD16(gb: *GB, args: InstrArgs) void { // LD r16, n16 TODO TEST
        const n: u16 = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
        print("LD16, target {any}, n: Ox{X}\n", .{ @as(regID, args.target), n });
        print("n b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.read_byte(gb.cpu.pc + 2) });
        print("n b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.set_word(args.target, n);
        gb.cpu.pc += 3;
    }
    fn LDr8(gb: *GB, args: InstrArgs) void { // LD r8, r8 TODO TEST
        print("LDr8, targets: from {any} --> to {any}\n", .{ args.targets.from, args.targets.to });
        print("Values, from {any} --> to {any}\n", .{ gb.cpu.get_byte(args.targets.to), gb.cpu.get_byte(args.targets.from) });
        gb.cpu.set_byte(args.targets.to, gb.cpu.get_byte(args.targets.from));
        gb.cpu.pc += 1;
    }
    fn LDAHL(gb: *GB, _: InstrArgs) void { // LD r8, r8 TODO TEST
        const mem_place = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(regID.a);
        print("LDHL, mem@hl:0x{X} --> to A\n", .{mem_place});
        print("Values, from {any} --> to {any}\n", .{ gb.cpu.get_byte(regID.a), gb.read_byte(mem_place) });
        gb.cpu.set_byte(regID.a, value);
        gb.cpu.pc += 1;
    }
    fn LDSP16(gb: *GB, _: InstrArgs) void { // LD r16, n16, 0x31
        print("LD16SP\n", .{});
        const n: u16 = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
        print("n b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.read_byte(gb.cpu.pc + 2) });
        print("n b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.sp = n;
        print("after op: sp: {d}\n", .{gb.cpu.sp});
        gb.cpu.pc += 3;
    }
    fn LDHL8(gb: *GB, _: InstrArgs) void { // LD[HL], n8
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.read_byte(gb.cpu.pc + 1);
        print("LDHLDA\thl:0x{X}\tvalue:0x{x}\nmem@hl: 0x{x}\n", .{ hl, value, gb.read_byte(hl) });
        gb.writeByte(hl, value);
        print("after op: mem@hl: 0x{X}\thl:{d}\n", .{ hl, gb.read_byte(hl), hl });
        gb.cpu.pc += 2;
    }
    fn LDHLR(gb: *GB, args: InstrArgs) void { // LD[HL],r8
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(args.target);
        print("LDHLR, \nmem@0x{X}: 0x{X} --> 0x{X}\n", .{ hl, gb.read_byte(hl), value });
        gb.writeByte(hl, value);
        print("after op: mem@0x{X}: 0x{X}\n", .{ hl, gb.read_byte(hl) });
        gb.cpu.pc += 1;
    }
    fn LDHLIA(gb: *GB, _: InstrArgs) void { // LD [HLI],A
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(regID.a);
        print("LDHLIA, \nmem@0x{X}: 0x{X} --> 0x{X}\n", .{ hl, gb.read_byte(hl), value });
        gb.writeByte(hl, value);
        gb.cpu.set_word(regID.h, hl + 1);
        print("after op: hl+1= 0x{X}\t mem@0x{X}: 0x{X}\n", .{ gb.cpu.get_word(regID.h), hl, gb.read_byte(hl) });
        gb.cpu.pc += 1;
    }
    fn LDHLDA(gb: *GB, _: InstrArgs) void { // LD [HLD],A
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(regID.a);
        print("LDHLDA, \nmem@0x{X}: 0x{X} --> 0x{X}\n", .{ hl, gb.read_byte(hl), value });
        gb.writeByte(hl, value);
        gb.cpu.set_word(regID.h, hl - 1);
        print("after op: mem@0x{X}: 0x{X}\n", .{ hl, gb.read_byte(hl) });
        gb.cpu.pc += 1;
    }
    fn LDHCA(gb: *GB, _: InstrArgs) void {
        const c = gb.cpu.get_byte(regID.c);
        const a = gb.cpu.get_byte(regID.a);
        const mem_place = 0xFF00 + @as(u16, c);
        print("LDHCA, memplace@0x{X} --> 0x{X}\n", .{ mem_place, a });
        gb.writeByte(mem_place, a);
        gb.cpu.pc += 1;
    }
    fn LDHAC(gb: *GB, _: InstrArgs) void { // Load value in register A from the byte at address $FF00+c
        const a = gb.cpu.get_byte(regID.a);
        const c = gb.cpu.get_byte(regID.c);
        const byte = gb.read_byte(0xFF00 + @as(u16, c));
        print("LDHAC byte: 0x{X} --> A\n", .{byte});
        gb.cpu.set_byte(a, byte);
        gb.cpu.pc += 1;
    }
    fn LDAn16(gb: *GB, _: InstrArgs) void { // TODO TEST Load value in register A from the byte at address n16.
        const memory_place = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
        const n = gb.read_byte(memory_place);
        print("LDAn16, n: Ox{X} --> A\n", .{n});
        print("memplace b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.read_byte(gb.cpu.pc + 2) });
        print("memplace b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.set_byte(regID.a, n);
        gb.cpu.pc += 3;
    }
    fn LDHAn16(gb: *GB, _: InstrArgs) void { // TODO TEST same as above, provided the address is between $FF00 and $FFFF.
        const memory_place = 0xFF00 + @as(u16, gb.read_byte(gb.cpu.pc + 1));
        print("LDHAn16, \n", .{});
        print("memplace b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, memory_place });
        // print("memplace b2: [pc]{d} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        if (memory_place >= 0xFF00 and memory_place <= 0xFFFF) {
            const n = gb.read_byte(memory_place);
            print("n: 0x{X} --> A\n", .{ n });
            gb.cpu.set_byte(regID.a, n);
        }
        gb.cpu.pc += 2;
    }
    fn LDn16A(gb: *GB, _: InstrArgs) void { // TODO TEST Store value in register A into the byte at address n16.
        const memory_place = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
        const n = gb.cpu.get_byte(regID.a);
        gb.writeByte(memory_place, n);
        print("LDn16A, n: Ox{X} --> memplace@{X}", .{ n, memory_place });
        print("memplace b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.read_byte(gb.cpu.pc + 2) });
        print("memplace b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.pc += 3;
    }
    fn LDHn16A(gb: *GB, _: InstrArgs) void { // TODO TEST same as above, provided the address is between $FF00 and $FFFF.
        print("LDHn16A\n", .{});
        const memory_place = 0xFF00 + @as(u16, gb.read_byte(gb.cpu.pc + 1));
        print("memplace b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, memory_place });
        // print("memplace b2: [pc]{d} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        if (memory_place >= 0xFF00 and memory_place <= 0xFFFF) {
            const n = gb.cpu.get_byte(regID.a);
            gb.writeByte(memory_place, n);
            print("n: 0x{X} --> memplace@0x{X}\n", .{ n, memory_place });
        }
        gb.cpu.pc += 2;
    }
    fn LDAr16(gb: *GB, args: InstrArgs) void { // TODO TEST Load value in register A from the byte pointed to by register r16.
        const memory_place = gb.cpu.get_word(args.target);
        const n = gb.read_byte(memory_place);
        print("LDAr16, n: 0x{X} --> A\n", .{n});
        gb.cpu.set_byte(regID.a, n);
        gb.cpu.pc += 1;
    }
    fn LDr16A(gb: *GB, args: InstrArgs) void { // TODO TEST Store value in register A into the byte pointed to by register r16.
        const memory_place = gb.cpu.get_word(args.target);
        const n = gb.cpu.get_byte(regID.a);
        gb.writeByte(memory_place, n);
        print("LDr16A, n: 0x{X} --> memplace@0x{X}\n", .{ n, memory_place });
        gb.cpu.pc += 1;
    }

    fn XORA(gb: *GB, args: InstrArgs) void {
        print("XORA, target {any}\n", .{args.target});
        gb.cpu.registers[@intFromEnum(regID.a)] ^= gb.cpu.registers[@intFromEnum(args.target)];
        if (gb.cpu.registers[@intFromEnum(regID.a)] == 0) {
            gb.cpu.f.z = true;
            gb.cpu.f.write();
        }
        gb.cpu.pc += 1;
    }
    fn ADDAr8(gb: *GB, args: InstrArgs) void { // TODO finish, TEST, flags
        const value = gb.cpu.get_byte(args.target);
        print("ADDAr8 target: {any}, value: {d} \n", .{ args.target, value });
        const a = gb.cpu.get_byte(regID.a);
        const res: struct { u8, u1 } = @addWithOverflow(a, value);
        gb.cpu.f.s = false;
        gb.cpu.f.c = res[1] == 1;
        gb.cpu.f.h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.z = res[0] == 0;
        gb.cpu.f.write();
        gb.cpu.set_byte(regID.a, res[0]);
        gb.cpu.pc += 1;
    }
    fn SUBAr8(gb: *GB, args: InstrArgs) void { // TODO finish, TEST, flags
        const value = gb.cpu.get_byte(args.target);
        print("SUBA target: {any}, value: {d} \n", .{ args.target, value });
        const a = gb.cpu.get_byte(regID.a);
        const res: struct { u8, u1 } = @subWithOverflow(a, value);
        gb.cpu.f.c = value > a;
        gb.cpu.f.s = true;
        gb.cpu.f.h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.z = res[0] == 0;
        gb.cpu.f.write();
        gb.cpu.set_byte(regID.a, res[0]);
        gb.cpu.pc += 1;
    }
    fn ADDAHL(gb: *GB, _: InstrArgs) void { // TODO Add the byte pointed to by HL to A.
        const mem_place = gb.cpu.get_word(regID.h);
        const value = gb.read_byte(mem_place);
        print("ADDAHL:A + mem@0x{X}: value: {d} \n", .{ mem_place, value });
        const a = gb.cpu.get_byte(regID.a);
        const res: struct { u8, u1 } = @addWithOverflow(a, value);
        gb.cpu.f.s = false;
        gb.cpu.f.c = res[1] == 1;
        gb.cpu.f.h = (res[0] & 0xF) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.z = res[0] == 0;
        gb.cpu.f.write();
        gb.cpu.set_byte(regID.a, res[0]);
        gb.cpu.pc += 1;
    }
    fn ADDHLr16(gb: *GB, args: InstrArgs) void { // TODO finish, TEST, flags
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_word(args.target);
        print("ADDHL {any} + hl, {d} + {d} \n", .{args.target, value, hl });
        const res: u16 = @addWithOverflow(hl, value)[0];
        gb.cpu.f.s = false;
        gb.cpu.f.h = (((hl + value) >> 8) & 0xF) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.c = (((hl + value) >> 12) & 0xF) & 0x10 == 0x10;
        gb.cpu.f.write();
        gb.cpu.set_word(regID.h, res);
        gb.cpu.pc += 1;
    }
    fn EI(gb: *GB, _: InstrArgs) void { // TODO TEST
        print("EI\n", .{});
        gb.cpu.pc += 1;
    }
    fn RRCA(gb: *GB, _: InstrArgs) void { // TODO TEST
        const a = gb.cpu.get_byte(regID.a);
        print("RRCA\n", .{});
        gb.cpu.set_byte(regID.a, a << 7 | a >> 1);
        gb.cpu.f.c = (@as(u1, @truncate(a)) == 1);
        gb.cpu.f.write();
        gb.cpu.pc += 1;
    }
    fn PUSH(gb: *GB, args: InstrArgs) void {
        var high: u8 = undefined;
        var low: u8 = undefined;
        if (args.target == regID.a) {
            high = gb.cpu.get_byte(regID.a);
            low = gb.cpu.f.value;
            print("PUSH AF a: 0x{X}, f: 0x{X}\n", .{ high, low });
        } else {
            const value = gb.cpu.get_word(args.target);
            high = @truncate(value >> 8);
            low = @truncate(value);
            print("PUSH 0x{X} from {any}\n", .{ value, args.target });
        }

        gb.cpu.sp -= 1;
        gb.writeByte(gb.cpu.sp, high);
        gb.cpu.sp -= 1;
        gb.writeByte(gb.cpu.sp, low);
        gb.cpu.pc += 1;
    }
    fn POP(gb: *GB, args: InstrArgs) void {
        const low = gb.read_byte(gb.cpu.sp);
        gb.cpu.sp += 1;
        const high = gb.read_byte(gb.cpu.sp);
        gb.cpu.sp += 1;
        const value = @as(u16, high) << 8 | low;
        print("POP 0x{X} --> {any}\n", .{ value, args.target });
        if (args.target == regID.a) {
            gb.cpu.set_byte(regID.a, high);
            gb.cpu.f.value = low;
            gb.cpu.f.check();
        } else gb.cpu.set_word(args.target, value);
        gb.cpu.pc += 1;
    }
    fn JP(gb: *GB, args: InstrArgs) void { // TODO ONLY 3 CYCLES IF NOT TAKEN OTHERWISE 4
        print("JP", .{});
        if (args.condition) {
            const n = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
            print("to 0x{X}\n", .{n});
            gb.cpu.pc = n;
        } else gb.cpu.pc += 3;
    }
    fn JR(gb: *GB, args: InstrArgs) void { // TODO ONLY 2 CYCLES IF NOT TAKEN OTHERWISE 3
        print("JR, \tcondition:", .{});
        const dist: i8 = @bitCast(gb.read_byte(gb.cpu.pc + 1));
        
        var jump = true;
        switch (args.flagConditions) { // switches on the carry flags, determining if we should jump
            .h => |*h| {
                if (h.*) {
                    println("h, flag:{any}", .{gb.cpu.f.h});
                    if (!gb.cpu.f.h) jump = false;
                } else {
                    println("not h flag:{any}", .{gb.cpu.f.h});
                    if (gb.cpu.f.h) jump = false;
                }
            },
            .z => |*z| {
                if (z.*) {
                    println("z flag:{any}", .{gb.cpu.f.z});
                    if (!gb.cpu.f.z) jump = false;
                } else {
                    println("not z flag:{any}", .{gb.cpu.f.z});
                    if (gb.cpu.f.z) jump = false;
                }
            },
            .c => |*c| {
                if (c.*) {
                    println("c flag:{any}", .{gb.cpu.f.c});
                    if (!gb.cpu.f.c) jump = false;
                } else {
                    println("not c flag:{any}", .{gb.cpu.f.c});
                    if (gb.cpu.f.c) jump = false;
                }
            },
            .s => |*s| {
                if (s.*) {
                    println("s flag:{any}", .{gb.cpu.f.s});
                    if (!gb.cpu.f.s) jump = false;
                } else {
                    println("not s flag:{any}", .{gb.cpu.f.s});
                    if (gb.cpu.f.s) jump = false;
                }
            },
            .none => println("no condition, just jump", .{}),
        }
        println(" by dist: [pc]0x{X} \t0x{X} ({d}) bytes ", .{ gb.cpu.pc + 1, dist, dist });
        if (jump) {
            const new_mem: i17 = @as(i17, @intCast(gb.cpu.pc + 2)) + dist;
            gb.cpu.pc = @as(u8, @intCast(new_mem));
            print("to pc:0x{X}\n", .{gb.cpu.pc});
            // if (dist == -2) {
            // }
        } else { // next instruction, condition failed
            println("skipped jump, failed condition", .{});
            gb.cpu.pc += 2;
        }
    }
    fn JPHL(gb: *GB, _: InstrArgs) void {
        print("JPHL\n", .{});
        gb.cpu.pc = gb.cpu.get_word(regID.h);
    }
    fn CALLn16(gb: *GB, args: InstrArgs) void { //
        var call = true;
        switch (args.flagConditions) {
            .h => |*h| {
                if (h.*) {
                    println("h, flag:{any}", .{gb.cpu.f.h});
                    if (!gb.cpu.f.h) call = false;
                } else {
                    println("not h flag:{any}", .{gb.cpu.f.h});
                    if (gb.cpu.f.h) call = false;
                }
            },
            .z => |*z| {
                if (z.*) {
                    println("z flag:{any}", .{gb.cpu.f.z});
                    if (!gb.cpu.f.z) call = false;
                } else {
                    println("not z flag:{any}", .{gb.cpu.f.z});
                    if (gb.cpu.f.z) call = false;
                }
            },
            .c => |*c| {
                if (c.*) {
                    println("c flag:{any}", .{gb.cpu.f.c});
                    if (!gb.cpu.f.c) call = false;
                } else {
                    println("not c flag:{any}", .{gb.cpu.f.c});
                    if (gb.cpu.f.c) call = false;
                }
            },
            .s => |*s| {
                if (s.*) {
                    println("s flag:{any}", .{gb.cpu.f.s});
                    if (!gb.cpu.f.s) call = false;
                } else {
                    println("not s flag:{any}", .{gb.cpu.f.s});
                    if (gb.cpu.f.s) call = false;
                }
            },
            .none => println("no condition, just jump", .{}),
        }
        if (call) {
            const n = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
            const ret = gb.cpu.pc + 3;
            print("CALL to 0x{X}, later RET to 0x{X}", .{ n, ret });
            gb.cpu.sp -= 1;
            gb.writeByte(gb.cpu.sp, @truncate(ret >> 8));
            gb.cpu.sp -= 1;
            gb.writeByte(gb.cpu.sp, @truncate(ret));
            gb.cpu.pc = n;
        } else { // next instruction, condition failed
            println("skipped jump, failed condition", .{});
            gb.cpu.pc += 2;
        }
    }
    fn RET(gb: *GB, args: InstrArgs) void { // TODO TEST, UPDATE cycles 5 if condition met, 2 if not met, 4 if no condition
        print("RET, condition:{any}", .{args.flagConditions});
        // var condition: bool = undefined;
        var ret = true;
        switch (args.flagConditions) {
            .h => |*h| {
                if (h.*) {
                    println("h, flag:{any}", .{gb.cpu.f.h});
                    if (!gb.cpu.f.h) ret = false;
                } else {
                    println("not h flag:{any}", .{gb.cpu.f.h});
                    if (gb.cpu.f.h) ret = false;
                }
            },
            .z => |*z| {
                if (z.*) {
                    println("z flag:{any}", .{gb.cpu.f.z});
                    if (!gb.cpu.f.z) ret = false;
                } else {
                    println("not z flag:{any}", .{gb.cpu.f.z});
                    if (gb.cpu.f.z) ret = false;
                }
            },
            .c => |*c| {
                if (c.*) {
                    println("c flag:{any}", .{gb.cpu.f.c});
                    if (!gb.cpu.f.c) ret = false;
                } else {
                    println("not c flag:{any}", .{gb.cpu.f.c});
                    if (gb.cpu.f.c) ret = false;
                }
            },
            .s => |*s| {
                if (s.*) {
                    println("s flag:{any}", .{gb.cpu.f.s});
                    if (!gb.cpu.f.s) ret = false;
                } else {
                    println("not s flag:{any}", .{gb.cpu.f.s});
                    if (gb.cpu.f.s) ret = false;
                }
            },
            .none => println("no condition, just jump", .{}),
        }
        if (ret) {
            const low = gb.read_byte(gb.cpu.sp);
            gb.cpu.sp += 1;
            const high = gb.read_byte(gb.cpu.sp);
            gb.cpu.sp += 1;
            gb.cpu.pc = @as(u16, high) << 8 | low;
        } else {
            gb.cpu.pc += 1;
        }
    }
    fn CPAn8(gb: *GB, _: InstrArgs) void { // TODO TEST;
        print("CPAn8, \n", .{});
        const n = gb.read_byte(gb.cpu.pc + 1);
        const reg = gb.cpu.get_byte(regID.a);
        print("n b1: [pc]0x{X} \t(0x{X}), A: 0x{X}\n", .{ gb.cpu.pc + 1, n, gb.cpu.get_byte(regID.a) });
        const res = @subWithOverflow(reg, n);
        gb.cpu.f.z = res[0] == 0; 
        gb.cpu.f.s = true;
        gb.cpu.f.h = (reg & 0xF) < (res[0] & 0xF); // half carry conditions
        gb.cpu.f.c = res[1] == 1;
        gb.cpu.f.write();
        gb.cpu.pc += 2;
    }
    fn CPAr8(gb: *GB, args: InstrArgs) void { // TODO TEST;
        const n = gb.cpu.get_byte(args.target);
        print("CPAr8, target = {any}\n", .{args.target});
        const reg = gb.cpu.get_byte(regID.a);
        const res = @subWithOverflow(reg, n);
        gb.cpu.f.z = res[0] == 0; 
        gb.cpu.f.s = true;
        gb.cpu.f.h = (reg & 0xF) < (res[0] & 0xF); // half carry conditions
        gb.cpu.f.c = res[1] == 1;
        gb.cpu.f.write();
        gb.cpu.pc += 1;
    }
    fn CPAHL(gb: *GB, _: InstrArgs) void { // TODO TEST;
        const hl = gb.cpu.get_word(regID.h);
        const reg = gb.cpu.get_byte(regID.a);
        print("CPAHL, compare mem_place: 0x{X} ({d}) to A:{d}\n", .{ hl, gb.read_byte(hl), reg });
        const res = @subWithOverflow(reg, hl);
        gb.cpu.f.z = res[0] == 0; 
        gb.cpu.f.s = true;
        gb.cpu.f.h = (reg & 0xF) < (res[0] & 0xF); // half carry conditions
        gb.cpu.f.c = res[1] == 1;
        gb.cpu.f.write();
        gb.cpu.pc += 1;
    }

    // PREFIX INSTRUCTIONS
    fn BITTEST(gb: *GB, args: InstrArgs) void { // TODO TEST
        const bit: u3 = args.bit_target.bit;
        const target = gb.cpu.get_byte(args.bit_target.target);
        print("BITTEST, target: {any}, bit: {any}, reg_bin: 0b{b}", .{ args.bit_target.target, args.bit_target.bit, target });
        // print("                                             ^", .{});
        gb.cpu.f.z = @as(u1, @truncate(target >> bit)) == 0; // set zero flag if the target bit is not set
        gb.cpu.f.h = true; // set half carry
        gb.cpu.f.write();
        gb.cpu.pc += 2;
    }
    fn BITTESTHL(gb: *GB, args: InstrArgs) void { // TODO TEST
        const bit: u3 = args.bit_target.bit;
        print("BITTESTHL, target: {any}, bit: {any}\n", .{ args.bit_target.target, args.bit_target.bit });
        const hl = gb.cpu.get_word(args.target);
        const byte = gb.read_byte(hl);
        gb.cpu.f.z = @as(u1, @truncate(byte >> bit)) == 0;
        gb.cpu.f.h = true;
        gb.cpu.f.write();
        gb.cpu.pc += 2;
    }
    fn RL(gb: *GB, args: InstrArgs) void { // TODO TEST Rotate bits in register r8 left through carry.
        const carried = gb.cpu.f.c;
        const reg = gb.cpu.get_byte(args.target);
        print("RL, target: {any}, prior: 0b{b}, carried = {d}\n", .{ args.target, reg, @intFromBool(carried) });
        gb.cpu.f.c = @as(u1, @truncate(reg >> 7)) == 1;
        const rotated: u8 = reg << 1 | @intFromBool(carried);
        gb.cpu.f.z = rotated == 0;
        gb.cpu.f.write();
        gb.cpu.set_byte(args.target, rotated);
        print("after: 0b{b}\n", .{gb.cpu.get_byte(args.target)});
        gb.cpu.pc += 2;
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
        var byte = gb.read_byte(self.pc);
        if (self.last_ins == byte) { // err on same instruction twice in a row
            return error.Repeat;
        }
        var prefixed = false;
        if (byte == 0xCB) { // prefix byte
            prefixed = true;
            self.pc += 1;
            byte = gb.read_byte(self.pc);
        }
        print("[pc]0x{X} \t(0x{X})\n", .{ self.pc, byte });
        const ins_args = try InstructionSet.from_byte(byte, prefixed);
        (ins_args.ins)(gb, ins_args.args);
        print("\n", .{});
        if (self.last_ins == 0xFB) { // set IME flag after previous instruction
            print("set IME\n", .{});
           
        }
        self.last_ins = byte;
        return ins_args.cycles;
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
    scanline_cycles_left: u16 = 456,
    frame_cycles_spent: u8 = 0,

    const Mode = enum { // modes specifying number of cycles
        HBLANK,
        VBLANK,
        SCAN,
        RENDER,
        const cycles: [4]u16 = .{204, 456, 80, 172};
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

    fn setSpecialRegister(self: *@This(), register: LCD.special_registers, value: u8) void {
        self.special_registers[@intFromEnum(register)] = value;
    }
    fn getSpecialRegister(self: *@This(), register: LCD.special_registers) u8 {
        return self.special_registers[@intFromEnum(register)];
    }

    fn switchMode(self: *@This()) void {
        if (self.getSpecialRegister(.ly) >= 144) {
            self.mode = .VBLANK;
            // TODO INTERRUPT
            return;
        }
        switch (self.mode) {
            .SCAN => {
                self.mode = .RENDER;
                self.scanline_cycles_left = Mode.cycles[@intFromEnum(Mode.RENDER)];
            },
            .RENDER => {
                self.mode = .HBLANK;
                self.scanline_cycles_left = Mode.cycles[@intFromEnum(Mode.HBLANK)];
            },
            .HBLANK => {
                self.mode = .SCAN;
                self.scanline_cycles_left = Mode.cycles[@intFromEnum(Mode.SCAN)];
                // self.setSpecialRegister(.ly, 144);
            },
            .VBLANK => {
                self.mode = .SCAN;
                self.scanline_cycles_left = Mode.cycles[@intFromEnum(Mode.SCAN)];                
            },
        }
        // update STAT register 
        var stat_reg = self.getSpecialRegister(.stat);
        // println("stat: {d}", .{stat_reg});
        stat_reg = (stat_reg & 0b1111_1100) | @intFromEnum(self.mode);
        self.setSpecialRegister(.stat, stat_reg);
    }

    fn init(self: *@This(), gb: *GB) !void {
        self.vram = gb.memory[VRAM_BEGIN .. VRAM_END + 1];
        self.oam = gb.memory[OAM_BEGIN .. OAM_END + 1];
        self.special_registers = gb.memory[LCD.special_registers.start..LCD.special_registers.end + 1];
        try self.lcd.init();
        @memset(&self.tile_set, empty_tile(self));
    }

    fn do(self: *@This()) void {
        // Operate GPU here 
        switch (self.mode) {
            .SCAN => { // 2 searches OAM memory for sprites that should be rendered on the current scanline and stores them in a buffer
                for (self.oam) |byte| {
                    _  = byte; // decode sprites
                }
            },
            .RENDER => { // 3 transfers pixels to the LCD, one scanline at a time, duration variable
                // var scanline: [LCD.screenWidthPx]Color = undefined;
                // TODO: Generate the actual pixels for this scanline based on:
                // - Background tiles at the current scroll position
                // - Window tiles if enabled and visible on this line
                // - Sprites that were found during OAM scan

                // if (self.scanline_cycles_left < cycles) {
                    
                // }
            },
            else => {} // no action for hblank or vblank
        }     
    }

    fn tick(self: *@This(), cycles: u16) void {
        // TODO the gpu should tick/cycle just as many 
            // times as the cpu did, while being able to 
            // process interrupts and continue on as well as changing modes midscanline when needed

        // time GPU here
        var cycles_left = cycles; // amt of cycles spent by cpu
        
        while (cycles_left > 0) {
            const cycles_to_process: u16 = @min(cycles_left, self.scanline_cycles_left);
            self.do();

            self.scanline_cycles_left -= cycles_to_process;
            cycles_left -= cycles_to_process;
            

            if (self.scanline_cycles_left == 0) {
                const ly = self.getSpecialRegister(.ly);
                if (self.mode == Mode.RENDER) {
                    println("Mode: {any}, stat update: {d}", .{self.mode, self.getSpecialRegister(.stat)});
                    println("scanline cycles left: {d}, cycles left: {d}", .{self.scanline_cycles_left, cycles_left});
                    println("ly: {d}, next: {d}\n", .{self.getSpecialRegister(.ly), self.getSpecialRegister(.ly) + 1});
                    @memset(&self.scanline, Color.white);
                    self.lcd.render_scanline(self.scanline, ly);
                }
                 if (ly >= 153) {
                    self.setSpecialRegister(.ly, 0);
                 }
                 else if (self.mode == .HBLANK or self.mode == .VBLANK) {
                    println("ly: {d}, next: {d}\n", .{self.getSpecialRegister(.ly), self.getSpecialRegister(.ly) + 1});
                    self.setSpecialRegister(.ly, ly + 1);
                 }
                self.switchMode();
                // TODO LYC CHECK 
            }
        }   
        // const normalized_address = fixed_address & 0xFFFE;
        //
        // const b1 = self.read_vram(normalized_address);
        // const b2 = self.read_vram(normalized_address + 1);
        //
        // const tile_index: u8 = fixed_address / 16; // 2 bpp * 8 rows of pixels per tile
        //         const row_index = (fixed_address % 16) / 2; // new row every two bytes
        //         // print("Row: {d}", .{row_index});
        //         for (0..8) |pixel_index| { // loop through pixels in current row
        //             const msb: u1 = @truncate(b2 >> @intCast(pixel_index)); // most significant bit
        //             const lsb: u1 = @truncate(b1 >> @intCast(pixel_index));
        //     print(" 0b{b}{b} ", .{ msb, lsb });
        //
        //     const color = switch (@as(u2, msb) << 1 | lsb) {
        //         0b00 => GPU.Color.white,
        //         0b01 => GPU.Color.lgray,
        //         0b10 => GPU.Color.dgray,
        //         0b11 => GPU.Color.black,
        //     };
        // }
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
        if (self.mode == .RENDER) {
            println("RENDER\n VRAM BLOCKED", .{});
        }
        const fixed_address: u12 = @intCast(address - VRAM_BEGIN);
        self.vram[fixed_address] = value;
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
    // var window_width: ?u16 = null;

    screen: [screenHeightPx][screenWidthPx]GPU.Color = undefined,
    background: [32][32]GPU.Tile = undefined, // defines the background pixels of the gameboy
    window: [32][32]GPU.Tile = undefined, // defines the foreground and sprites
    renderer: *g.SDL_Renderer = undefined,
    win: *g.SDL_Window = undefined,

    // gfxScale: u16 = 3,
    grid_pixel_sz: u16 = undefined,
    // grid_pixel_w: f32 = undefined,

    const special_registers = enum(u8) { 
        lcdc,
        stat,
        scy,
        scx,
        ly,
        lyc,
        dma,
        bgp,
        obp1,
        obp0,
        wy,
        wx,

        const end = 0xFF4B;
        const start = 0xFF40;
        const size = 0xFF4B - 0xFF40 + 1;

        // fn memPlace(register: special_registers) u16 {
        //     return switch (register) {
        //         .lcdc => 0xFF40, // LCDC (LCD Control) Enables/disables layers, defines rendering mode
        //         // Bit 7 | Bit 6 | Bit 5 | Bit 4 | Bit 3 | Bit 2 | Bit 1 | Bit 0
        //         // --------------------------------------------------------------
        //         // LCD  | Window | Window | BG Tile | BG & WIN | OBJ Size | OBJ Enable | BG Enable
        //         // Enable| Enable | Tile Map | Data Select | Tile Map Select | (8x8/8x16) | (Sprites) |
        //         .stat => 0xFF41, // $FF41 STAT (Status) Tracks PPU state
        //         // 6 LYC int select (Read/Write): If set, selects the LYC == LY condition for the STAT interrupt.
        //         // 5 Mode 2 int select (Read/Write): If set, selects the Mode 2 condition for the STAT interrupt.
        //         // 4 Mode 1 int select (Read/Write): If set, selects the Mode 1 condition for the STAT interrupt.
        //         // 3 Mode 0 int select (Read/Write): If set, selects the Mode 0 condition for the STAT interrupt.
        //         // 2 LYC == LY (Read-only): Set when LY contains the same value as LYC; it is constantly updated.
        //         // 1-0 PPU mode (Read-only): Indicates the PPU’s current status. Reports 0 instead when the PPU is disabled.
        //         .scy => 0xFF42, // $FF42 SCY (Scroll Y) Background vertical scroll
        //         .scx => 0xFF43, // $FF43 SCX (Scroll X) Background horizontal scroll
        //         .ly => 0xFF44,
        //         .lyc => 0xFF45, // $FF45 LYC (Compare LY) Interrupt if LY matches LYC
        //         .dma => 0xFF46, // $FF46 DMA Transfers 160 bytes from RAM to OAM
        //         .bgp => 0xFF47, // $FF47 BGP (BG Palette) Defines colors for BG tiles
        //         .obp0 => 0xFF48, // $FF48 OBP0 (OBJ Palette 0) Defines colors for sprite palette 0
        //         .obp1 => 0xFF49, // $FF49 OBP1 (OBJ Palette 1) Defines colors for sprite palette 1
        //         .wy => 0xFF4A, // $FF4A WY (Window Y) Window vertical position
        //         .wx => 0xFF4B // $FF4B WX (Window X) Window horizontal position
        //     };
        // }
        //
        // fn write(gb: GB, register: special_registers, byte: u8) void {
        //     const mem_place = memPlace(register);
        //     gb.writeByte(mem_place, byte);
        // }
        //
        // fn read(gb: GB, register: special_registers, byte: u8) u8 {
        //     const mem_place = memPlace(register);
        //     return gb.read_byte(mem_place, byte);
        // }
    };

    fn init(self: *@This()) !void {
        var color: GPU.Color = undefined;
        for (&self.screen) |*row| {
            for (row) |*pixel| {
                var seed: u64 = undefined;
                try std.posix.getrandom(std.mem.asBytes(&seed));
                var prng = std.Random.DefaultPrng.init(seed);
                const rand = prng.random();
                color = rand.enumValue(GPU.Color);                pixel.* = color;
            }
            // @memset(//row, color);
        }
        try self.startAndCreateRenderer(); // set window and renderer
    }

    fn setRect(self: *@This(), rect: *g.SDL_FRect, x: anytype, y: anytype, w: anytype, h: anytype) void {
        _ = self;
        rect.x = if (@TypeOf(x) == f32)  x else @as(f32, @floatFromInt(x));
        rect.y = if (@TypeOf(y) == f32) y else @as(f32, @floatFromInt(y));
        rect.w = if (@TypeOf(w) == f32) w else @as(f32, @floatFromInt(w));
        rect.h = if (@TypeOf(h) == f32) h else @as(f32, @floatFromInt(h));
    }

    fn updateDimensions() void {
        center = @as(u16, @intCast(window_width)) / 2;
        println("Center: {d}", .{center});
        screenH = @intFromFloat(@as(f32, @floatFromInt(window_height - aboveScreen)) * 0.4);

        screenW = screenH * screenWidthPx / screenHeightPx;
        if (screenW > window_width - 20) {
            screenW = @intCast(window_width - 20);
            screenH = screenW * screenHeightPx / screenWidthPx;
        }
        println("sH: {d}", .{screenH});
        println("sW: {d}", .{screenW});
        sidebar = center - screenW / 2;
        println("sidebar: {d}", .{sidebar});
        pxSize = @as(f32, @floatFromInt(screenH)) / screenHeightPx;
        println("pxSize: {any}", .{pxSize});

    }
    pub fn render_scanline(self: *@This(), scanline: [screenWidthPx]GPU.Color, y: u8 ) void {
        var rect = g.SDL_FRect{};
        // SCANLINE 
        for (scanline, 0..) |px, x| {
            _ = switch (px) {
                .white => g.SDL_SetRenderDrawColor(self.renderer, 0, 200, 0, 255),
                .lgray => g.SDL_SetRenderDrawColor(self.renderer, 0, 160, 0, 255),
                .dgray => g.SDL_SetRenderDrawColor(self.renderer, 0, 120, 0, 255),
                .black => g.SDL_SetRenderDrawColor(self.renderer, 0, 80, 0, 255)
            };
            const xPos = @as(f32, @floatFromInt(sidebar)) + @as(f32, @floatFromInt(x)) * pxSize;
            const yPos = @as(f32, @floatFromInt(aboveScreen)) + @as(f32, @floatFromInt(y)) * pxSize;
            self.setRect(&rect, xPos, yPos, pxSize, pxSize);
            _ = g.SDL_RenderFillRect(self.renderer, &rect);
        }
        _ = g.SDL_RenderPresent(self.renderer);
    }
    fn renderBody(self: *@This()) void {
        _ = g.SDL_SetRenderDrawColor(self.renderer, 255, 192, 220, 255);
        _ = g.SDL_RenderClear(self.renderer);
        _ = g.SDL_SetRenderDrawColor(self.renderer, 0, 255, 0, 255);
        println("screen: {d}x{d}\npxSize: {any}\nrendered screen width: {d}\nrendered screen height: {d}", .{screenW, screenH, pxSize, pxSize * screenWidthPx, pxSize * screenHeightPx});

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
        _ = g.SDL_RenderPresent(self.renderer);
    }
    // pub fn render(self: *@This()) void {
    //     _ = g.SDL_SetRenderDrawColor(self.renderer, 255, 192, 220, 255);
    //     _ = g.SDL_RenderClear(self.renderer);

    //     println("rendering {d}x{d}\n", .{window_width, window_height});
    //     _ = g.SDL_RenderPresent(self.renderer);
    // }
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
        _ =  g.SDL_SetWindowMinimumSize(win.?, initWinW, initWinH);
        // _ = g.SDL_MaximizeWindow(win.?);

        if (renderer == null) {
            print("Failed to create renderer: {s}\n", .{g.SDL_GetError()});
            return error.RendererFailure;
        }
        self.renderer = renderer.?;
        self.win = win.?;
        if (!g.SDL_GetWindowSizeInPixels(self.win, &window_width, &window_height)) {
            println("err while getting window size: {s}", .{g.SDL_GetError()});
            return error.NoWinSize;
        } else {
            println("window size: {d}x{d}", .{window_width, window_height});
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
            // if (i != 0 and i % 80 == 0) print("\n", .{});
        }
        print("\n", .{});
    }
    fn endSDL(self: *@This()) void {
        g.SDL_Quit();
        g.SDL_DestroyWindow(self.win);
        g.SDL_DestroyRenderer(self.renderer);
    }
};

/// Gameboy Machine, defer endGB
pub const GB = struct {
    cpu: CPU = CPU{},
    gpu: GPU = GPU{},
    apu: APU = APU{},
    memory: [0xFFFF]u8 = undefined,
    running: bool = undefined,
    cycles_spent: usize = 0,
    const sr = LCD.special_registers;
    /// nintendo logo
    const LOGO: [48]u8 = .{ 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E };

    pub fn init(self: *@This()) !void {
        @memset(&self.memory, 0);
        @memcpy(self.memory[0x104 .. 0x133 + 1], &LOGO);
        try self.cpu.init();
        try self.gpu.init(self);
        self.running = true;
    }

    pub fn read_byte(self: *@This(), address: usize) u8 {
        // TODO: implement memory mapping based on address
            return self.memory[address];
        }

    
    pub fn writeByte(self: *@This(), address: usize, value: u8) void {
        switch (address) {
            GPU.VRAM_BEGIN...GPU.VRAM_END + 1 => {
                println("mem before: mem@0x{X} = 0x{X}", .{ address, self.read_byte(address) });
                self.gpu.writeVram(address, value);
                println("mem after: mem@0x{X} = 0x{X}", .{ address, self.read_byte(address) });
            },
            @intFromEnum(LCD.special_registers.dma) => {
                const prefix = address / 0x100;
                const ram_address: u16 = @as(u16, @intCast(prefix)) << 8;
                @memcpy( self.memory[GPU.OAM_BEGIN .. GPU.OAM_END], self.memory[ram_address..ram_address + GPU.OAM_END]);
            },
            0xFF50 => { // Disable bootrom register
                // Unmap and replace the bootrom with cartridge data
            },
            else => {
                self.memory[address] = value;
            },
        }
        // if (address == 0xFF50) {
        //     // end boot rom, map cartridge
        // }

    }

    pub fn boot(self: *@This()) !void {
        const bootFile = try std.fs.cwd().openFile("roms/dmg_boot.bin", .{});
        defer bootFile.close();
        const bootFileStats = try bootFile.stat();
        const bootFileBuf: []u8 = try bootFile.readToEndAlloc(allocator, bootFileStats.size);
        for (0..bootFileBuf.len) |i| {
            self.memory[i] = bootFileBuf[i];
        }
        // var cycles: u8 = undefined;
        // while (self.cpu.pc < 0x100) {
        // // while (self.cpu.pc < 0xA7) {
        //     cycles = try self.cpu.execute(self);
        // }
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
        while (self.running) {
            try self.getEvents();
            try self.do();
        //TODO: Update peripherals & timing
        }
    }
    fn do(self: *@This()) !void {
        const cycles = try self.cpu.execute(self);
        self.cycles_spent += cycles;
        self.gpu.tick(cycles);
        self.reg_dump();
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
                // g.eventwindow
                // g.SDL_EVENT_WINDOW_ENTER_FULLSCREEN => {
                //     print("FULLSCREEN, NEW SIZE\n\n\n\n\n\n", .{});
                //     _ = g.SDL_GetWindowSizeInPixels(self.gpu.lcd.win, &LCD.window_width, &LCD.window_height);
                //     LCD.updateDimensions();
                // },
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
        print("Actual memspace dump: \n", .{});
        for (self.memory[LCD.special_registers.start .. LCD.special_registers.end + 1], LCD.special_registers.start..LCD.special_registers.end + 1) |value, i| {
            println("register@0x{x}: 0x{x} ", .{i, value});
        }
        print("\n", .{});
    }

    fn endGB(self: *@This()) void {
        self.gpu.lcd.endSDL();
    }
};

pub fn main() !void {
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
    try gb.go();
}



