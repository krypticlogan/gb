//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.
const std = @import("std");
const print = std.debug.print;
const allocator = std.heap.page_allocator;
const regID = enum(u3) {
    a,
    b,
    c,
    d,
    e,
    h,
    l,
};
const FlagRegister = struct {
    value: u8 = 0,
    zero: bool = false,
    subtract: bool = false,
    half_carry: bool = false,
    carry: bool = false,

    ZERO_FLAG_BYTE_POSITION: u8 = 7,
    SUBTRACT_FLAG_BYTE_POSITION: u8 = 6,
    HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5,
    CARRY_FLAG_BYTE_POSITION: u8 = 4,

    fn check(self: *@This(), byte: u8) void {
        self.zero = (byte >> self.ZERO_FLAG_BYTE_POSITION) & 0b1;
        self.subtract = (byte >> self.SUBTRACT_FLAG_BYTE_POSITION) & 0b1;
        self.half_carry = (byte >> self.HALF_CARRY_FLAG_BYTE_POSITION) & 0b1;
        self.carry = (byte >> self.CARRY_FLAG_BYTE_POSITION) & 0b1;
    }

    fn write(self: *@This()) u8 {
        self.value = (if (self.zero) 1 else 0) << self.ZERO_FLAG_BYTE_POSITION |
            (if (self.subtract) 1 else 0) << self.SUBTRACT_FLAG_BYTE_POSITION |
            (if (self.half_carry) 1 else 0) << self.HALF_CARRY_FLAG_BYTE_POSITION |
            (if (self.carry) 1 else 0 << self.CARRY_FLAG_BYTE_POSITION);
    }
};

const InstructionSet = struct {
    const table = [_]struct { u8, ?*const fn (*CPU) void }{
        .{ 0x00, InstructionSet.NOP }, // NOP
        .{ 0x01, null }, // LD BC, d16
        .{ 0x02, null }, // LD (BC), A
        .{ 0x03, null }, // INC BC
        .{ 0x04, null }, // INC B
        .{ 0x05, null }, // DEC B
        .{ 0x06, null }, // LD B, d8
        .{ 0x07, null }, // RLCA
        .{ 0x08, null }, // LD (a16), SP
        .{ 0x09, null }, // ADD HL, BC
        .{ 0x0A, null }, // LD A, (BC)
        .{ 0x0B, null }, // DEC BC
        .{ 0x0C, null }, // INC C
        .{ 0x0D, null }, // DEC C
        .{ 0x0E, null }, // LD C, d8
        .{ 0x0F, null }, // RRCA
        .{ 0x10, null }, // STOP 0
        .{ 0x11, null }, // LD DE, d16
        .{ 0x12, null }, // LD (DE), A
        .{ 0x13, null }, // INC DE
        .{ 0x14, null }, // INC D
        .{ 0x15, null }, // DEC D
        .{ 0x16, null }, // LD D, d8
        .{ 0x17, null }, // RLA
        .{ 0x18, null }, // JR r8
        .{ 0x19, null }, // ADD HL, DE
        .{ 0x1A, null }, // LD A, (DE)
        .{ 0x1B, null }, // DEC DE
        .{ 0x1C, null }, // INC E
        .{ 0x1D, null }, // DEC E
        .{ 0x1E, null }, // LD E, d8
        .{ 0x1F, null }, // RRA
        .{ 0x20, null }, // JR NZ, r8
        .{ 0x21, null }, // LD HL, d16
        .{ 0x22, null }, // LD (HL+), A
        .{ 0x23, null }, // INC HL
        .{ 0x24, null }, // INC H
        .{ 0x25, null }, // DEC H
        .{ 0x26, null }, // LD H, d8
        .{ 0x27, null }, // DAA
        .{ 0x28, null }, // JR Z, r8
        .{ 0x29, null }, // ADD HL, HL
        .{ 0x2A, null }, // LD A, (HL+)
        .{ 0x2B, null }, // DEC HL
        .{ 0x2C, null }, // INC L
        .{ 0x2D, null }, // DEC L
        .{ 0x2E, null }, // LD L, d8
        .{ 0x2F, null }, // CPL
        .{ 0x30, null }, // JR NC, r8
        .{ 0x31, null }, // LD SP, d16
        .{ 0x32, null }, // LD (HL-), A
        .{ 0x33, null }, // INC SP
        .{ 0x34, null }, // INC (HL)
        .{ 0x35, null }, // DEC (HL)
        .{ 0x36, null }, // LD (HL), d8
        .{ 0x37, null }, // SCF
        .{ 0x38, null }, // JR C, r8
        .{ 0x39, null }, // ADD HL, SP
        .{ 0x3A, null }, // LD A, (HL-)
        .{ 0x3B, null }, // DEC SP
        .{ 0x3C, null }, // INC A
        .{ 0x3D, null }, // DEC A
        .{ 0x3E, null }, // LD A, d8
        .{ 0x3F, null }, // CCF
        .{ 0x40, null }, // LD B, B
        .{ 0x41, null }, // LD B, C
        .{ 0x42, null }, // LD B, D
        .{ 0x43, null }, // LD B, E
        .{ 0x44, null }, // LD B, H
        .{ 0x45, null }, // LD B, L
        .{ 0x46, null }, // LD B, (HL)
        .{ 0x47, null }, // LD B, A
        .{ 0x48, null }, // LD C, B
        .{ 0x49, null }, // LD C, C
        .{ 0x4A, null }, // LD C, D
        .{ 0x4B, null }, // LD C, E
        .{ 0x4C, null }, // LD C, H
        .{ 0x4D, null }, // LD C, L
        .{ 0x4E, null }, // LD C, (HL)
        .{ 0x4F, null }, // LD C, A
        .{ 0x50, null }, // LD D, B
        .{ 0x51, null }, // LD D, C
        .{ 0x52, null }, // LD D, D
        .{ 0x53, null }, // LD D, E
        .{ 0x54, null }, // LD D, H
        .{ 0x55, null }, // LD D, L
        .{ 0x56, null }, // LD D, (HL)
        .{ 0x57, null }, // LD D, A
        .{ 0x58, null }, // LD E, B
        .{ 0x59, null }, // LD E, C
        .{ 0x5A, null }, // LD E, D
        .{ 0x5B, null }, // LD E, E
        .{ 0x5C, null }, // LD E, H
        .{ 0x5D, null }, // LD E, L
        .{ 0x5E, null }, // LD E, (HL)
        .{ 0x5F, null }, // LD E, A
        .{ 0x60, null }, // LD H, B
        .{ 0x61, null }, // LD H, C
        .{ 0x62, null }, // LD H, D
        .{ 0x63, null }, // LD H, E
        .{ 0x64, null }, // LD H, H
        .{ 0x65, null }, // LD H, L
        .{ 0x66, null }, // LD H, (HL)
        .{ 0x67, null }, // LD H, A
        .{ 0x68, null }, // LD L, B
        .{ 0x69, null }, // LD L, C
        .{ 0x6A, null }, // LD L, D
        .{ 0x6B, null }, // LD L, E
        .{ 0x6C, null }, // LD L, H
        .{ 0x6D, null }, // LD L, L
        .{ 0x6E, null }, // LD L, (HL)
        .{ 0x6F, null }, // LD L, A
        .{ 0x70, null }, // LD (HL), B
        .{ 0x71, null }, // LD (HL), C
        .{ 0x72, null }, // LD (HL), D
        .{ 0x73, null }, // LD (HL), E
        .{ 0x74, null }, // LD (HL), H
        .{ 0x75, null }, // LD (HL), L
        .{ 0x76, null }, // HALT
        .{ 0x77, null }, // LD (HL), A
        .{ 0x78, null }, // LD A, B
        .{ 0x79, null }, // LD A, C
        .{ 0x7A, null }, // LD A, D
        .{ 0x7B, null }, // LD A, E
        .{ 0x7C, null }, // LD A, H
        .{ 0x7D, null }, // LD A, L
        .{ 0x7E, null }, // LD A, (HL)
        .{ 0x7F, null }, // LD A, A
        .{ 0x80, InstructionSet.ADDB }, // ADD A, B
        .{ 0x81, null }, // ADD A, C
        .{ 0x82, null }, // ADD A, D
        .{ 0x83, null }, // ADD A, E
        .{ 0x84, null }, // ADD A, H
        .{ 0x85, null }, // ADD A, L
        .{ 0x86, null }, // ADD A, (HL)
        .{ 0x87, null }, // ADD A, A
        .{ 0x88, null }, // ADC A, B
        .{ 0x89, null }, // ADC A, C
        .{ 0x8A, null }, // ADC A, D
        .{ 0x8B, null }, // ADC A, E
        .{ 0x8C, null }, // ADC A, H
        .{ 0x8D, null }, // ADC A, L
        .{ 0x8E, null }, // ADC A, (HL)
        .{ 0x8F, null }, // ADC A, A
        .{ 0x90, null }, // SUB B
        .{ 0x91, null }, // SUB C
        .{ 0x92, null }, // SUB D
        .{ 0x93, null }, // SUB E
        .{ 0x94, null }, // SUB H
        .{ 0x95, null }, // SUB L
        .{ 0x96, null }, // SUB (HL)
        .{ 0x97, null }, // SUB A
        .{ 0x98, null }, // SBC A, B
        .{ 0x99, null }, // SBC A, C
        .{ 0x9A, null }, // SBC A, D
        .{ 0x9B, null }, // SBC A, E
        .{ 0x9C, null }, // SBC A, H
        .{ 0x9D, null }, // SBC A, L
        .{ 0x9E, null }, // SBC A, (HL)
        .{ 0x9F, null }, // SBC A, A
        .{ 0xA0, null }, // AND B
        .{ 0xA1, null }, // AND C
        .{ 0xA2, null }, // AND D
        .{ 0xA3, null }, // AND E
        .{ 0xA4, null }, // AND H
        .{ 0xA5, null }, // AND L
        .{ 0xA6, null }, // AND (HL)
        .{ 0xA7, null }, // AND A
        .{ 0xA8, null }, // XOR B
        .{ 0xA9, null }, // XOR C
        .{ 0xAA, null }, // XOR D
        .{ 0xAB, null }, // XOR E
        .{ 0xAC, null }, // XOR H
        .{ 0xAD, null }, // XOR L
        .{ 0xAE, null }, // XOR (HL)
        .{ 0xAF, null }, // XOR A
        .{ 0xB0, null }, // OR B
        .{ 0xB1, null }, // OR C
        .{ 0xB2, null }, // OR D
        .{ 0xB3, null }, // OR E
        .{ 0xB4, null }, // OR H
        .{ 0xB5, null }, // OR L
        .{ 0xB6, null }, // OR (HL)
        .{ 0xB7, null }, // OR A
        .{ 0xB8, null }, // CP B
        .{ 0xB9, null }, // CP C
        .{ 0xBA, null }, // CP D
        .{ 0xBB, null }, // CP E
        .{ 0xBC, null }, // CP H
        .{ 0xBD, null }, // CP L
        .{ 0xBE, null }, // CP (HL)
        .{ 0xBF, null }, // CP A
        .{ 0xC0, null }, // RET NZ
        .{ 0xC1, null }, // POP BC
        .{ 0xC2, null }, // JP NZ, a16
        .{ 0xC3, null }, // JP a16
        .{ 0xC4, null }, // CALL NZ, a16
        .{ 0xC5, null }, // PUSH BC
        .{ 0xC6, null }, // ADD A, d8
        .{ 0xC7, null }, // RST 00H
        .{ 0xC8, null }, // RET Z
        .{ 0xC9, null }, // RET
        .{ 0xCA, null }, // JP Z, a16
        .{ 0xCB, null }, // PREFIX CB
        .{ 0xCC, null }, // CALL Z, a16
        .{ 0xCD, null }, // CALL a16
        .{ 0xCE, null }, // ADC A, d8
        .{ 0xCF, null }, // RST 08H
        .{ 0xD0, null }, // RET NC
        .{ 0xD1, null }, // POP DE
        .{ 0xD2, null }, // JP NC, a16
        .{ 0xD3, null }, // NOP
        .{ 0xD4, null }, // CALL NC, a16
        .{ 0xD5, null }, // PUSH DE
        .{ 0xD6, null }, // SUB d8
        .{ 0xD7, null }, // RST 10H
        .{ 0xD8, null }, // RET C
        .{ 0xD9, null }, // RETI
        .{ 0xDA, null }, // JP C, a16
        .{ 0xDB, null }, // NOP
        .{ 0xDC, null }, // CALL C, a16
        .{ 0xDD, null }, // NOP
        .{ 0xDE, null }, // SBC A, d8
        .{ 0xDF, null }, // RST 18H
        .{ 0xE0, null }, // LDH (a8), A
        .{ 0xE1, null }, // POP HL
        .{ 0xE2, null }, // LD (C), A
        .{ 0xE3, null }, // NOP
        .{ 0xE4, null }, // NOP
        .{ 0xE5, null }, // PUSH HL
        .{ 0xE6, null }, // AND d8
        .{ 0xE7, null }, // RST 20H
        .{ 0xE8, null }, // ADD SP, r8
        .{ 0xE9, null }, // JP (HL)
        .{ 0xEA, null }, // LD (a16), A
        .{ 0xEB, null }, // NOP
        .{ 0xEC, null }, // NOP
        .{ 0xED, null }, // NOP
        .{ 0xEE, null }, // XOR d8
        .{ 0xEF, null }, // RST 28H
        .{ 0xF0, null }, // LDH A, (a8)
        .{ 0xF1, null }, // POP AF
        .{ 0xF2, null }, // LD A, (C)
        .{ 0xF3, null }, // DI
        .{ 0xF4, null }, // NOP
        .{ 0xF5, null }, // PUSH AF
        .{ 0xF6, null }, // OR d8
        .{ 0xF7, null }, // RST 30H
        .{ 0xF8, null }, // LD HL, SP+r8
        .{ 0xF9, null }, // LD SP, HL
        .{ 0xFA, null }, // LD A, (a16)
        .{ 0xFB, null },
        .{ 0xFC, null },
        .{ 0xFD, null },
        .{ 0xFE, null },
        .{ 0xFF, null },
    };

    const prefix_table = [_]struct { u8, ?*const fn (*CPU) void }{
        .{ 0x00, InstructionSet.NOP }, // NOP
        .{ 0x01, null }, // LD BC, d16
        .{ 0x02, null }, // LD (BC), A
        .{ 0x03, null }, // INC BC
        .{ 0x04, null }, // INC B
        .{ 0x05, null }, // DEC B
        .{ 0x06, null }, // LD B, d8
        .{ 0x07, null }, // RLCA
        .{ 0x08, null }, // LD (a16), SP
        .{ 0x09, null }, // ADD HL, BC
        .{ 0x0A, null }, // LD A, (BC)
        .{ 0x0B, null }, // DEC BC
        .{ 0x0C, null }, // INC C
        .{ 0x0D, null }, // DEC C
        .{ 0x0E, null }, // LD C, d8
        .{ 0x0F, null }, // RRCA
        .{ 0x10, null }, // STOP 0
        .{ 0x11, null }, // LD DE, d16
        .{ 0x12, null }, // LD (DE), A
        .{ 0x13, null }, // INC DE
        .{ 0x14, null }, // INC D
        .{ 0x15, null }, // DEC D
        .{ 0x16, null }, // LD D, d8
        .{ 0x17, null }, // RLA
        .{ 0x18, null }, // JR r8
        .{ 0x19, null }, // ADD HL, DE
        .{ 0x1A, null }, // LD A, (DE)
        .{ 0x1B, null }, // DEC DE
        .{ 0x1C, null }, // INC E
        .{ 0x1D, null }, // DEC E
        .{ 0x1E, null }, // LD E, d8
        .{ 0x1F, null }, // RRA
        .{ 0x20, null }, // JR NZ, r8
        .{ 0x21, null }, // LD HL, d16
        .{ 0x22, null }, // LD (HL+), A
        .{ 0x23, null }, // INC HL
        .{ 0x24, null }, // INC H
        .{ 0x25, null }, // DEC H
        .{ 0x26, null }, // LD H, d8
        .{ 0x27, null }, // DAA
        .{ 0x28, null }, // JR Z, r8
        .{ 0x29, null }, // ADD HL, HL
        .{ 0x2A, null }, // LD A, (HL+)
        .{ 0x2B, null }, // DEC HL
        .{ 0x2C, null }, // INC L
        .{ 0x2D, null }, // DEC L
        .{ 0x2E, null }, // LD L, d8
        .{ 0x2F, null }, // CPL
        .{ 0x30, null }, // JR NC, r8
        .{ 0x31, null }, // LD SP, d16
        .{ 0x32, null }, // LD (HL-), A
        .{ 0x33, null }, // INC SP
        .{ 0x34, null }, // INC (HL)
        .{ 0x35, null }, // DEC (HL)
        .{ 0x36, null }, // LD (HL), d8
        .{ 0x37, null }, // SCF
        .{ 0x38, null }, // JR C, r8
        .{ 0x39, null }, // ADD HL, SP
        .{ 0x3A, null }, // LD A, (HL-)
        .{ 0x3B, null }, // DEC SP
        .{ 0x3C, null }, // INC A
        .{ 0x3D, null }, // DEC A
        .{ 0x3E, null }, // LD A, d8
        .{ 0x3F, null }, // CCF
        .{ 0x40, null }, // LD B, B
        .{ 0x41, null }, // LD B, C
        .{ 0x42, null }, // LD B, D
        .{ 0x43, null }, // LD B, E
        .{ 0x44, null }, // LD B, H
        .{ 0x45, null }, // LD B, L
        .{ 0x46, null }, // LD B, (HL)
        .{ 0x47, null }, // LD B, A
        .{ 0x48, null }, // LD C, B
        .{ 0x49, null }, // LD C, C
        .{ 0x4A, null }, // LD C, D
        .{ 0x4B, null }, // LD C, E
        .{ 0x4C, null }, // LD C, H
        .{ 0x4D, null }, // LD C, L
        .{ 0x4E, null }, // LD C, (HL)
        .{ 0x4F, null }, // LD C, A
        .{ 0x50, null }, // LD D, B
        .{ 0x51, null }, // LD D, C
        .{ 0x52, null }, // LD D, D
        .{ 0x53, null }, // LD D, E
        .{ 0x54, null }, // LD D, H
        .{ 0x55, null }, // LD D, L
        .{ 0x56, null }, // LD D, (HL)
        .{ 0x57, null }, // LD D, A
        .{ 0x58, null }, // LD E, B
        .{ 0x59, null }, // LD E, C
        .{ 0x5A, null }, // LD E, D
        .{ 0x5B, null }, // LD E, E
        .{ 0x5C, null }, // LD E, H
        .{ 0x5D, null }, // LD E, L
        .{ 0x5E, null }, // LD E, (HL)
        .{ 0x5F, null }, // LD E, A
        .{ 0x60, null }, // LD H, B
        .{ 0x61, null }, // LD H, C
        .{ 0x62, null }, // LD H, D
        .{ 0x63, null }, // LD H, E
        .{ 0x64, null }, // LD H, H
        .{ 0x65, null }, // LD H, L
        .{ 0x66, null }, // LD H, (HL)
        .{ 0x67, null }, // LD H, A
        .{ 0x68, null }, // LD L, B
        .{ 0x69, null }, // LD L, C
        .{ 0x6A, null }, // LD L, D
        .{ 0x6B, null }, // LD L, E
        .{ 0x6C, null }, // LD L, H
        .{ 0x6D, null }, // LD L, L
        .{ 0x6E, null }, // LD L, (HL)
        .{ 0x6F, null }, // LD L, A
        .{ 0x70, null }, // LD (HL), B
        .{ 0x71, null }, // LD (HL), C
        .{ 0x72, null }, // LD (HL), D
        .{ 0x73, null }, // LD (HL), E
        .{ 0x74, null }, // LD (HL), H
        .{ 0x75, null }, // LD (HL), L
        .{ 0x76, null }, // HALT
        .{ 0x77, null }, // LD (HL), A
        .{ 0x78, null }, // LD A, B
        .{ 0x79, null }, // LD A, C
        .{ 0x7A, null }, // LD A, D
        .{ 0x7B, null }, // LD A, E
        .{ 0x7C, null }, // LD A, H
        .{ 0x7D, null }, // LD A, L
        .{ 0x7E, null }, // LD A, (HL)
        .{ 0x7F, null }, // LD A, A
        .{ 0x80, null }, // ADD A, B
        .{ 0x81, null }, // ADD A, C
        .{ 0x82, null }, // ADD A, D
        .{ 0x83, null }, // ADD A, E
        .{ 0x84, null }, // ADD A, H
        .{ 0x85, null }, // ADD A, L
        .{ 0x86, null }, // ADD A, (HL)
        .{ 0x87, null }, // ADD A, A
        .{ 0x88, null }, // ADC A, B
        .{ 0x89, null }, // ADC A, C
        .{ 0x8A, null }, // ADC A, D
        .{ 0x8B, null }, // ADC A, E
        .{ 0x8C, null }, // ADC A, H
        .{ 0x8D, null }, // ADC A, L
        .{ 0x8E, null }, // ADC A, (HL)
        .{ 0x8F, null }, // ADC A, A
        .{ 0x90, null }, // SUB B
        .{ 0x91, null }, // SUB C
        .{ 0x92, null }, // SUB D
        .{ 0x93, null }, // SUB E
        .{ 0x94, null }, // SUB H
        .{ 0x95, null }, // SUB L
        .{ 0x96, null }, // SUB (HL)
        .{ 0x97, null }, // SUB A
        .{ 0x98, null }, // SBC A, B
        .{ 0x99, null }, // SBC A, C
        .{ 0x9A, null }, // SBC A, D
        .{ 0x9B, null }, // SBC A, E
        .{ 0x9C, null }, // SBC A, H
        .{ 0x9D, null }, // SBC A, L
        .{ 0x9E, null }, // SBC A, (HL)
        .{ 0x9F, null }, // SBC A, A
        .{ 0xA0, null }, // AND B
        .{ 0xA1, null }, // AND C
        .{ 0xA2, null }, // AND D
        .{ 0xA3, null }, // AND E
        .{ 0xA4, null }, // AND H
        .{ 0xA5, null }, // AND L
        .{ 0xA6, null }, // AND (HL)
        .{ 0xA7, null }, // AND A
        .{ 0xA8, null }, // XOR B
        .{ 0xA9, null }, // XOR C
        .{ 0xAA, null }, // XOR D
        .{ 0xAB, null }, // XOR E
        .{ 0xAC, null }, // XOR H
        .{ 0xAD, null }, // XOR L
        .{ 0xAE, null }, // XOR (HL)
        .{ 0xAF, null }, // XOR A
        .{ 0xB0, null }, // OR B
        .{ 0xB1, null }, // OR C
        .{ 0xB2, null }, // OR D
        .{ 0xB3, null }, // OR E
        .{ 0xB4, null }, // OR H
        .{ 0xB5, null }, // OR L
        .{ 0xB6, null }, // OR (HL)
        .{ 0xB7, null }, // OR A
        .{ 0xB8, null }, // CP B
        .{ 0xB9, null }, // CP C
        .{ 0xBA, null }, // CP D
        .{ 0xBB, null }, // CP E
        .{ 0xBC, null }, // CP H
        .{ 0xBD, null }, // CP L
        .{ 0xBE, null }, // CP (HL)
        .{ 0xBF, null }, // CP A
        .{ 0xC0, null }, // RET NZ
        .{ 0xC1, null }, // POP BC
        .{ 0xC2, null }, // JP NZ, a16
        .{ 0xC3, null }, // JP a16
        .{ 0xC4, null }, // CALL NZ, a16
        .{ 0xC5, null }, // PUSH BC
        .{ 0xC6, null }, // ADD A, d8
        .{ 0xC7, null }, // RST 00H
        .{ 0xC8, null }, // RET Z
        .{ 0xC9, null }, // RET
        .{ 0xCA, null }, // JP Z, a16
        .{ 0xCB, null }, // PREFIX CB
        .{ 0xCC, null }, // CALL Z, a16
        .{ 0xCD, null }, // CALL a16
        .{ 0xCE, null }, // ADC A, d8
        .{ 0xCF, null }, // RST 08H
        .{ 0xD0, null }, // RET NC
        .{ 0xD1, null }, // POP DE
        .{ 0xD2, null }, // JP NC, a16
        .{ 0xD3, null }, // NOP
        .{ 0xD4, null }, // CALL NC, a16
        .{ 0xD5, null }, // PUSH DE
        .{ 0xD6, null }, // SUB d8
        .{ 0xD7, null }, // RST 10H
        .{ 0xD8, null }, // RET C
        .{ 0xD9, null }, // RETI
        .{ 0xDA, null }, // JP C, a16
        .{ 0xDB, null }, // NOP
        .{ 0xDC, null }, // CALL C, a16
        .{ 0xDD, null }, // NOP
        .{ 0xDE, null }, // SBC A, d8
        .{ 0xDF, null }, // RST 18H
        .{ 0xE0, null }, // LDH (a8), A
        .{ 0xE1, null }, // POP HL
        .{ 0xE2, null }, // LD (C), A
        .{ 0xE3, null }, // NOP
        .{ 0xE4, null }, // NOP
        .{ 0xE5, null }, // PUSH HL
        .{ 0xE6, null }, // AND d8
        .{ 0xE7, null }, // RST 20H
        .{ 0xE8, null }, // ADD SP, r8
        .{ 0xE9, null }, // JP (HL)
        .{ 0xEA, null }, // LD (a16), A
        .{ 0xEB, null }, // NOP
        .{ 0xEC, null }, // NOP
        .{ 0xED, null }, // NOP
        .{ 0xEE, null }, // XOR d8
        .{ 0xEF, null }, // RST 28H
        .{ 0xF0, null }, // LDH A, (a8)
        .{ 0xF1, null }, // POP AF
        .{ 0xF2, null }, // LD A, (C)
        .{ 0xF3, null }, // DI
        .{ 0xF4, null }, // NOP
        .{ 0xF5, null }, // PUSH AF
        .{ 0xF6, null }, // OR d8
        .{ 0xF7, null }, // RST 30H
        .{ 0xF8, null }, // LD HL, SP+r8
        .{ 0xF9, null }, // LD SP, HL
        .{ 0xFA, null }, // LD A, (a16)
        .{ 0xFB, null },
        .{ 0xFC, null },
        .{ 0xFD, null },
        .{ 0xFE, null },
        .{ 0xFF, null },
    };

    fn from_byte(byte: u8) !*const fn (*CPU) void {
        const ins = table[byte][1];
        if ((byte >= 0 and byte < table.len)) {
            if (ins == null) return error.NullInstruction;
            return ins.?;
        } else return error.IllegalByte;
    }

    fn from_prefixed_byte(byte: u8) !*const fn (*CPU) void {
        const ins = prefix_table[byte][1];
        if ((byte >= 0 and byte < table.len)) {
            if (ins == null) return error.NullInstruction;
            return ins.?;
        } else return error.IllegalByte;
    }

    fn NOP(cpu: *CPU) void {
        print("NOP", .{});
        cpu.pc += 1;
    }
    fn ADDB(cpu: *CPU) void {
        print("ADDB", .{});
        cpu.pc += 1;
    }
};
/// Defines a GameBoy CPU: i8080 & Z80 hybrid chip
const CPU = struct {
    registers: [7]u8 = undefined,
    f: FlagRegister = FlagRegister{},
    pc: u16 = undefined,
    sp: u16 = undefined,

    fn init(self: *@This()) !void {
        @memset(&self.registers, 0);
        self.pc = 0; //TODO: program start value
        self.sp = 0;
    }


    fn set_byte(self: *@This(), reg1: regID, value: u8) void {
        self.registers[reg1] = value;
    }

    fn get_byte(self: *@This(), reg1: regID) u8 {
        return self.registers[reg1];
    }

    fn set_word(self: *@This(), reg1: regID, value: u16) void {
        self.registers[reg1] = @truncate((value & 0xFF00) >> 8);
        self.registers[reg1 + 1] = @truncate(value & 0x00FF);
    }

    fn get_word(self: *@This(), reg1: regID) u16 {
        return (@as(u16, self.registers[reg1]) << 8) | self.registers[reg1 + 1];
    }

    fn execute(self: *@This(), gb: *GB) !void {
        const byte = gb.memory[self.pc];
        const prefixed = byte == 0xCB;
        var ins: *const fn (*CPU) void = undefined;
        if (prefixed) {
            self.pc += 1;
            ins = InstructionSet.from_prefixed_byte(byte) catch |err| {
                print("\t opcode: 0x{x} Error: {any}\n", .{ byte, err });
                return err;
            };
        } else {
            ins = InstructionSet.from_byte(byte) catch |err| {
                print("\t opcode: 0x{x} Error: {any}\n", .{ byte, err });
                return err;
            };
        }
        ins(self);
        print("\n", .{});
    }
};

const APU = struct {
    // TODO: implement sound
};

const GPU = struct {
    const VRAM_BEGIN = 0x8000;
    const VRAM_END = 0x97FF;
    const VRAM_SIZE = VRAM_END - VRAM_BEGIN + 1;

    const Color = enum { black, dgray, lgray, white };

    const Tile = [8][8]Color;
    fn empty_tile(self: *@This()) Tile {
        _ = self;
        var tile: Tile = undefined;
        for (&tile) |*row| {
            @memset(row, Color.black);
        }
        return tile;
    }

    vram: *[VRAM_SIZE]u8 = undefined,
    tile_set: [384]Tile = undefined,

    fn init(self: *@This(), gb: *GB) !void {
        self.vram = gb.memory[VRAM_BEGIN .. VRAM_END + 1];
        @memset(&self.tile_set, empty_tile(self));
    }

    fn read_vram(self: *@This(), address: usize) u8 {
        return self.vram[address];
    }

    fn write_vram(self: *@This(), address: usize, value: u8) void {
        self.vram[address] = value;
        if (address >= 0x1800) return;
        // const normalized_address
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

const GB = struct {
    cpu: CPU = CPU{},
    gpu: GPU = GPU{},
    apu: APU = APU{},
    memory: [0xFFFF]u8 = undefined,

    fn init(self: *@This()) !void {
        @memset(&self.memory, 0);
        try self.cpu.init();
        try self.gpu.init(self);
        try self.boot();
    }

    fn boot(self: *@This()) !void {
        const bootFile = try std.fs.cwd().openFile("roms/dmg_boot.bin", .{});
        defer bootFile.close();
        const bootFileStats = try bootFile.stat();
        const bootFileBuf: []u8 = try bootFile.readToEndAlloc(allocator, bootFileStats.size);
        for (0..bootFileBuf.len) |i| {
            self.memory[i] = bootFileBuf[i];
        }
        while (self.cpu.pc < 0x100) {
            try self.cpu.execute(self);
        }
    }

    fn loadGame(self: *@This(), game_path: []const u8) !void {
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

    fn cycle(self: *@This()) !void {
        try self.cpu.execute(self);
        //TODO: Update peripherals & timing
    }

    fn mem_dump(self: *@This()) void {
        print("printing bytes:\n", .{});
        for (self.memory, 0..self.memory.len) |value, i| {
            print("0x{x}", .{value});
            if (i != 0 and i % 64 == 0) print("\n", .{}) else continue;
        }
        print("\n", .{});
    }

    fn gfx_dump(self: *@This()) void {
        print("Actual memspace dump: \n", .{});
        for (self.memory[0x8000 .. 0x97FF + 1], 0..(0x97FF - 0x8000 + 1)) |value, i| {
            print("0x{x}", .{value});
            if (i != 0 and i % 80 == 0) print("\n", .{});
        }
        print("\n", .{});
    }

    // specs
    // CPU freq (TIMING)	4.194304 MHz1,	Up to 8.388608 MHz
    // Work RAM	8 KiB,	32 KiB3 (4 + 7 × 4 KiB)
    // Video RAM	8 KiB,	16 KiB3 (2 × 8 KiB)

    // OBJ ("sprites")	8 × 8 or 8 × 16 ; max 40 per screen, 10 per line
    // Palettes	BG: 1 × 4, OBJ: 2 × 3,	BG: 8 × 4, OBJ: 8 × 33
    // Colors	4 shades of green,	32768 colors (15-bit RGB)
    // Horizontal sync	9.198 KHz, 9.198 KHz
    // Vertical sync	59.73 Hz, 59.73 Hz
    // Sound	4 channels with stereo output
    // memory
    // The Game Boy has a 16-bit address bus, which is used to address ROM, RAM, and I/O.
};

pub fn main() !void {
    var gb = GB{};
    gb.init() catch |err| {
        print("Couldn't inititalize GameBoy, Error: {any}\n", .{err});
        return;
    };

    print("GB init!\n", .{});

    gb.boot() catch |err| {
        print("Couldn't boot GameBoy, Error: {any}\n", .{err});
        return err;
    };
    // gb.memory[0] = 0x00;
    // // gb.memory[1] = 0x80;
    // // gb.memory[0] = 0x00;
    // gb.memory[1] = 0x01;

    // // gb.gpu.vram_dump();
    // // gb.gfx_dump();

    // print("\n", .{});
    // try gb.cycle();
    // try gb.cycle();

    // const tile = gb.gpu.empty_tile();
    // print("{any}", .{gb.gpu.tile_set});
}
