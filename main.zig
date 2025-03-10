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
    const table = [_]struct { u8, ?*const fn (*GB) void, u16 }{
        .{ 0x00, InstructionSet.NOP, 1 }, // NOP
        .{ 0x01, null, 12 }, // LD BC, d16
        .{ 0x02, null, 8 }, // LD (BC), A
        .{ 0x03, null, 8 }, // INC BC
        .{ 0x04, null, 4 }, // INC B
        .{ 0x05, null, 4 }, // DEC B
        .{ 0x06, null, 8 }, // LD B, d8
        .{ 0x07, null, 4 }, // RLCA
        .{ 0x08, null, 20 }, // LD (a16), SP
        .{ 0x09, null, 8 }, // ADD HL, BC
        .{ 0x0A, null, 8 }, // LD A, (BC)
        .{ 0x0B, null, 8 }, // DEC BC
        .{ 0x0C, null, 4 }, // INC C
        .{ 0x0D, null, 4 }, // DEC C
        .{ 0x0E, null, 8 }, // LD C, d8
        .{ 0x0F, null, 4 }, // RRCA
        .{ 0x10, null, 4 }, // STOP 0
        .{ 0x11, null, 12 }, // LD DE, d16
        .{ 0x12, null, 8 }, // LD (DE), A
        .{ 0x13, null, 8 }, // INC DE
        .{ 0x14, null, 4 }, // INC D
        .{ 0x15, null, 4 }, // DEC D
        .{ 0x16, null, 8 }, // LD D, d8
        .{ 0x17, null, 4 }, // RLA
        .{ 0x18, null, 12 }, // JR r8
        .{ 0x19, null, 8 }, // ADD HL, DE
        .{ 0x1A, null, 8 }, // LD A, (DE)
        .{ 0x1B, null, 8 }, // DEC DE
        .{ 0x1C, null, 4 }, // INC E
        .{ 0x1D, null, 4 }, // DEC E
        .{ 0x1E, null, 8 }, // LD E, d8
        .{ 0x1F, null, 4 }, // RRA
        .{ 0x20, null, 12 }, // JR NZ, r8
        .{ 0x21, null, 12 }, // LD HL, d16
        .{ 0x22, null, 8 }, // LD (HL+), A
        .{ 0x23, null, 8 }, // INC HL
        .{ 0x24, null, 4 }, // INC H
        .{ 0x25, null, 4 }, // DEC H
        .{ 0x26, null, 8 }, // LD H, d8
        .{ 0x27, null, 4 }, // DAA
        .{ 0x28, null, 12 }, // JR Z, r8
        .{ 0x29, null, 8 }, // ADD HL, HL
        .{ 0x2A, null, 8 }, // LD A, (HL+)
        .{ 0x2B, null, 8 }, // DEC HL
        .{ 0x2C, null, 4 }, // INC L
        .{ 0x2D, null, 4 }, // DEC L
        .{ 0x2E, null, 8 }, // LD L, d8
        .{ 0x2F, null, 4 }, // CPL
        .{ 0x30, null, 12 }, // JR NC, r8
        .{ 0x31, InstructionSet.LDSP16, 12 }, // LD SP, d16
        .{ 0x32, null, 8 }, // LD (HL-), A
        .{ 0x33, null, 8 }, // INC SP
        .{ 0x34, null, 12 }, // INC (HL)
        .{ 0x35, null, 12 }, // DEC (HL)
        .{ 0x36, null, 12 }, // LD (HL), d8
        .{ 0x37, null, 4 }, // SCF
        .{ 0x38, null, 12 }, // JR C, r8
        .{ 0x39, null, 8 }, // ADD HL, SP
        .{ 0x3A, null, 8 }, // LD A, (HL-)
        .{ 0x3B, null, 8 }, // DEC SP
        .{ 0x3C, null, 4 }, // INC A
        .{ 0x3D, null, 4 }, // DEC A
        .{ 0x3E, null, 8 }, // LD A, d8
        .{ 0x3F, null, 4 }, // CCF
        .{ 0x40, null, 4 }, // LD B, B
        .{ 0x41, null, 4 }, // LD B, C
        .{ 0x42, null, 4 }, // LD B, D
        .{ 0x43, null, 4 }, // LD B, E
        .{ 0x44, null, 4 }, // LD B, H
        .{ 0x45, null, 4 }, // LD B, L
        .{ 0x46, null, 8 }, // LD B, (HL)
        .{ 0x47, null, 4 }, // LD B, A
        .{ 0x48, null, 4 }, // LD C, B
        .{ 0x49, null, 4 }, // LD C, C
        .{ 0x4A, null, 4 }, // LD C, D
        .{ 0x4B, null, 4 }, // LD C, E
        .{ 0x4C, null, 4 }, // LD C, H
        .{ 0x4D, null, 4 }, // LD C, L
        .{ 0x4E, null, 8 }, // LD C, (HL)
        .{ 0x4F, null, 4 }, // LD C, A
        .{ 0x50, null, 4 }, // LD D, B
        .{ 0x51, null, 4 }, // LD D, C
        .{ 0x52, null, 4 }, // LD D, D
        .{ 0x53, null, 4 }, // LD D, E
        .{ 0x54, null, 4 }, // LD D, H
        .{ 0x55, null, 4 }, // LD D, L
        .{ 0x56, null, 8 }, // LD D, (HL)
        .{ 0x57, null, 4 }, // LD D, A
        .{ 0x58, null, 4 }, // LD E, B
        .{ 0x59, null, 4 }, // LD E, C
        .{ 0x5A, null, 4 }, // LD E, D
        .{ 0x5B, null, 4 }, // LD E, E
        .{ 0x5C, null, 4 }, // LD E, H
        .{ 0x5D, null, 4 }, // LD E, L
        .{ 0x5E, null, 8 }, // LD E, (HL)
        .{ 0x5F, null, 4 }, // LD E, A
        .{ 0x60, null, 4 }, // LD H, B
        .{ 0x61, null, 4 }, // LD H, C
        .{ 0x62, null, 4 }, // LD H, D
        .{ 0x63, null, 4 }, // LD H, E
        .{ 0x64, null, 4 }, // LD H, H
        .{ 0x65, null, 4 }, // LD H, L
        .{ 0x66, null, 8 }, // LD H, (HL)
        .{ 0x67, null, 4 }, // LD H, A
        .{ 0x68, null, 4 }, // LD L, B
        .{ 0x69, null, 4 }, // LD L, C
        .{ 0x6A, null, 4 }, // LD L, D
        .{ 0x6B, null, 4 }, // LD L, E
        .{ 0x6C, null, 4 }, // LD L, H
        .{ 0x6D, null, 4 }, // LD L, L
        .{ 0x6E, null, 8 }, // LD L, (HL)
        .{ 0x6F, null, 4 }, // LD L, A
        .{ 0x70, null, 8 }, // LD (HL), B
        .{ 0x71, null, 8 }, // LD (HL), C
        .{ 0x72, null, 8 }, // LD (HL), D
        .{ 0x73, null, 8 }, // LD (HL), E
        .{ 0x74, null, 8 }, // LD (HL), H
        .{ 0x75, null, 8 }, // LD (HL), L
        .{ 0x76, null, 4 }, // HALT
        .{ 0x77, null, 8 }, // LD (HL), A
        .{ 0x78, null, 4 }, // LD A, B
        .{ 0x79, null, 4 }, // LD A, C
        .{ 0x7A, null, 4 }, // LD A, D
        .{ 0x7B, null, 4 }, // LD A, E
        .{ 0x7C, null, 4 }, // LD A, H
        .{ 0x7D, null, 4 }, // LD A, L
        .{ 0x7E, null, 8 }, // LD A, (HL)
        .{ 0x7F, null, 4 }, // LD A, A
        .{ 0x80, InstructionSet.ADDB, 4 }, // ADD A, B
        .{ 0x81, null, 4 }, // ADD A, C
        .{ 0x82, null, 4 }, // ADD A, D
        .{ 0x83, null, 4 }, // ADD A, E
        .{ 0x84, null, 4 }, // ADD A, H
        .{ 0x85, null, 4 }, // ADD A, L
        .{ 0x86, null, 8 }, // ADD A, (HL)
        .{ 0x87, null, 4 }, // ADD A, A
        .{ 0x88, null, 4 }, // ADC A, B
        .{ 0x89, null, 4 }, // ADC A, C
        .{ 0x8A, null, 4 }, // ADC A, D
        .{ 0x8B, null, 4 }, // ADC A, E
        .{ 0x8C, null, 4 }, // ADC A, H
        .{ 0x8D, null, 4 }, // ADC A, L
        .{ 0x8E, null, 8 }, // ADC A, (HL)
        .{ 0x8F, null, 4 }, // ADC A, A
        .{ 0x90, null, 4 }, // SUB B
        .{ 0x91, null, 4 }, // SUB C
        .{ 0x92, null, 4 }, // SUB D
        .{ 0x93, null, 4 }, // SUB E
        .{ 0x94, null, 4 }, // SUB H
        .{ 0x95, null, 4 }, // SUB L
        .{ 0x96, null, 8 }, // SUB (HL)
        .{ 0x97, null, 4 }, // SUB A
        .{ 0x98, null, 4 }, // SBC A, B
        .{ 0x99, null, 4 }, // SBC A, C
        .{ 0x9A, null, 4 }, // SBC A, D
        .{ 0x9B, null, 4 }, // SBC A, E
        .{ 0x9C, null, 4 }, // SBC A, H
        .{ 0x9D, null, 4 }, // SBC A, L
        .{ 0x9E, null, 8 }, // SBC A, (HL)
        .{ 0x9F, null, 4 }, // SBC A, A
        .{ 0xA0, null, 4 }, // AND B
        .{ 0xA1, null, 4 }, // AND C
        .{ 0xA2, null, 4 }, // AND D
        .{ 0xA3, null, 4 }, // AND E
        .{ 0xA4, null, 4 }, // AND H
        .{ 0xA5, null, 4 }, // AND L
        .{ 0xA6, null, 8 }, // AND (HL)
        .{ 0xA7, null, 4 }, // AND A
        .{ 0xA8, null, 4 }, // XOR B
        .{ 0xA9, null, 4 }, // XOR C
        .{ 0xAA, null, 4 }, // XOR D
        .{ 0xAB, null, 4 }, // XOR E
        .{ 0xAC, null, 4 }, // XOR H
        .{ 0xAD, null, 4 }, // XOR L
        .{ 0xAE, null, 8 }, // XOR (HL)
        .{ 0xAF, null, 4 }, // XOR A
        .{ 0xB0, null, 4 }, // OR B
        .{ 0xB1, null, 4 }, // OR C
        .{ 0xB2, null, 4 }, // OR D
        .{ 0xB3, null, 4 }, // OR E
        .{ 0xB4, null, 4 }, // OR H
        .{ 0xB5, null, 4 }, // OR L
        .{ 0xB6, null, 8 }, // OR (HL)
        .{ 0xB7, null, 4 }, // OR A
        .{ 0xB8, null, 4 }, // CP B
        .{ 0xB9, null, 4 }, // CP C
        .{ 0xBA, null, 4 }, // CP D
        .{ 0xBB, null, 4 }, // CP E
        .{ 0xBC, null, 4 }, // CP H
        .{ 0xBD, null, 4 }, // CP L
        .{ 0xBE, null, 8 }, // CP (HL)
        .{ 0xBF, null, 4 }, // CP A
        .{ 0xC0, null, 20 }, // RET NZ
        .{ 0xC1, null, 12 }, // POP BC
        .{ 0xC2, null, 16 }, // JP NZ, a16
        .{ 0xC3, null, 16 }, // JP a16
        .{ 0xC4, null, 24 }, // CALL NZ, a16
        .{ 0xC5, null, 16 }, // PUSH BC
        .{ 0xC6, null, 8 }, // ADD A, d8
        .{ 0xC7, null, 16 }, // RST 00H
        .{ 0xC8, null, 20 }, // RET Z
        .{ 0xC9, null, 16 }, // RET
        .{ 0xCA, null, 16 }, // JP Z, a16
        .{ 0xCB, null, 4 }, // PREFIX CB
        .{ 0xCC, null, 24 }, // CALL Z, a16
        .{ 0xCD, null, 24 }, // CALL a16
        .{ 0xCE, null, 8 }, // ADC A, d8
        .{ 0xCF, null, 16 }, // RST 08H
        .{ 0xD0, null, 20 }, // RET NC
        .{ 0xD1, null, 12 }, // POP DE
        .{ 0xD2, null, 16 }, // JP NC, a16
        .{ 0xD3, null, 4 }, // NOP
        .{ 0xD4, null, 24 }, // CALL NC, a16
        .{ 0xD5, null, 16 }, // PUSH DE
        .{ 0xD6, null, 8 }, // SUB d8
        .{ 0xD7, null, 16 }, // RST 10H
        .{ 0xD8, null, 20 }, // RET C
        .{ 0xD9, null, 16 }, // RETI
        .{ 0xDA, null, 16 }, // JP C, a16
        .{ 0xDB, null, 4 }, // NOP
        .{ 0xDC, null, 24 }, // CALL C, a16
        .{ 0xDD, null, 4 }, // NOP
        .{ 0xDE, null, 8 }, // SBC A, d8
        .{ 0xDF, null, 16 }, // RST 18H
        .{ 0xE0, null, 12 }, // LDH (a8), A
        .{ 0xE1, null, 12 }, // POP HL
        .{ 0xE2, null, 8 }, // LD (C), A
        .{ 0xE3, null, 4 }, // NOP
        .{ 0xE4, null, 4 }, // NOP
        .{ 0xE5, null, 16 }, // PUSH HL
        .{ 0xE6, null, 8 }, // AND d8
        .{ 0xE7, null, 16 }, // RST 20H
        .{ 0xE8, null, 16 }, // ADD SP, r8
        .{ 0xE9, null, 4 }, // JP (HL)
        .{ 0xEA, null, 16 }, // LD (a16), A
        .{ 0xEB, null, 4 }, // NOP
        .{ 0xEC, null, 4 }, // NOP
        .{ 0xED, null, 4 }, // NOP
        .{ 0xEE, null, 8 }, // XOR d8
        .{ 0xEF, null, 16 }, // RST 28H
        .{ 0xF0, null, 12 }, // LDH A, (a8)
        .{ 0xF1, null, 12 }, // POP AF
        .{ 0xF2, null, 8 }, // LD A, (C)
        .{ 0xF3, null, 4 }, // DI
        .{ 0xF4, null, 4 }, // NOP
        .{ 0xF5, null, 16 }, // PUSH AF
        .{ 0xF6, null, 8 }, // OR d8
        .{ 0xF7, null, 16 }, // RST 30H
        .{ 0xF8, null, 12 }, // LD HL, SP+r8
        .{ 0xF9, null, 8 }, // LD SP, HL
        .{ 0xFA, null, 16 }, // LD A, (a16)
        .{ 0xFB, null, 4 }, // EI
        .{ 0xFC, null, 4 }, // NOP
        .{ 0xFD, null, 4 }, // NOP
        .{ 0xFE, null, 8 }, // CP d8
        .{ 0xFF, null, 16 }, // RST 38H
    };

    const prefix_table = [_]struct { u8, ?*const fn (*GB) void, u16 }{
        .{ 0x00, null, 8 }, // RLC B
        .{ 0x01, null, 8 }, // RLC C
        .{ 0x02, null, 8 }, // RLC D
        .{ 0x03, null, 8 }, // RLC E
        .{ 0x04, null, 8 }, // RLC H
        .{ 0x05, null, 8 }, // RLC L
        .{ 0x06, null, 16 }, // RLC (HL)
        .{ 0x07, null, 8 }, // RLC A
        .{ 0x08, null, 8 }, // RRC B
        .{ 0x09, null, 8 }, // RRC C
        .{ 0x0A, null, 8 }, // RRC D
        .{ 0x0B, null, 8 }, // RRC E
        .{ 0x0C, null, 8 }, // RRC H
        .{ 0x0D, null, 8 }, // RRC L
        .{ 0x0E, null, 16 }, // RRC (HL)
        .{ 0x0F, null, 8 }, // RRC A
        .{ 0x10, null, 8 }, // RL B
        .{ 0x11, null, 8 }, // RL C
        .{ 0x12, null, 8 }, // RL D
        .{ 0x13, null, 8 }, // RL E
        .{ 0x14, null, 8 }, // RL H
        .{ 0x15, null, 8 }, // RL L
        .{ 0x16, null, 16 }, // RL (HL)
        .{ 0x17, null, 8 }, // RL A
        .{ 0x18, null, 8 }, // RR B
        .{ 0x19, null, 8 }, // RR C
        .{ 0x1A, null, 8 }, // RR D
        .{ 0x1B, null, 8 }, // RR E
        .{ 0x1C, null, 8 }, // RR H
        .{ 0x1D, null, 8 }, // RR L
        .{ 0x1E, null, 16 }, // RR (HL)
        .{ 0x1F, null, 8 }, // RR A
        .{ 0x20, null, 8 }, // SLA B
        .{ 0x21, null, 8 }, // SLA C
        .{ 0x22, null, 8 }, // SLA D
        .{ 0x23, null, 8 }, // SLA E
        .{ 0x24, null, 8 }, // SLA H
        .{ 0x25, null, 8 }, // SLA L
        .{ 0x26, null, 16 }, // SLA (HL)
        .{ 0x27, null, 8 }, // SLA A
        .{ 0x28, null, 8 }, // SRA B
        .{ 0x29, null, 8 }, // SRA C
        .{ 0x2A, null, 8 }, // SRA D
        .{ 0x2B, null, 8 }, // SRA E
        .{ 0x2C, null, 8 }, // SRA H
        .{ 0x2D, null, 8 }, // SRA L
        .{ 0x2E, null, 16 }, // SRA (HL)
        .{ 0x2F, null, 8 }, // SRA A
        .{ 0x30, null, 8 }, // SWAP B
        .{ 0x31, null, 8 }, // SWAP C
        .{ 0x32, null, 8 }, // SWAP D
        .{ 0x33, null, 8 }, // SWAP E
        .{ 0x34, null, 8 }, // SWAP H
        .{ 0x35, null, 8 }, // SWAP L
        .{ 0x36, null, 16 }, // SWAP (HL)
        .{ 0x37, null, 8 }, // SWAP A
        .{ 0x38, null, 8 }, // SRL B
        .{ 0x39, null, 8 }, // SRL C
        .{ 0x3A, null, 8 }, // SRL D
        .{ 0x3B, null, 8 }, // SRL E
        .{ 0x3C, null, 8 }, // SRL H
        .{ 0x3D, null, 8 }, // SRL L
        .{ 0x3E, null, 16 }, // SRL (HL)
        .{ 0x3F, null, 8 }, // SRL A
        .{ 0x40, null, 8 }, // BIT 0, B
        .{ 0x41, null, 8 }, // BIT 0, C
        .{ 0x42, null, 8 }, // BIT 0, D
        .{ 0x43, null, 8 }, // BIT 0, E
        .{ 0x44, null, 8 }, // BIT 0, H
        .{ 0x45, null, 8 }, // BIT 0, L
        .{ 0x46, null, 12 }, // BIT 0, (HL)
        .{ 0x47, null, 8 }, // BIT 0, A
        .{ 0x48, null, 8 }, // BIT 1, B
        .{ 0x49, null, 8 }, // BIT 1, C
        .{ 0x4A, null, 8 }, // BIT 1, D
        .{ 0x4B, null, 8 }, // BIT 1, E
        .{ 0x4C, null, 8 }, // BIT 1, H
        .{ 0x4D, null, 8 }, // BIT 1, L
        .{ 0x4E, null, 12 }, // BIT 1, (HL)
        .{ 0x4F, null, 8 }, // BIT 1, A
        .{ 0x50, null, 8 }, // BIT 2, B
        .{ 0x51, null, 8 }, // BIT 2, C
        .{ 0x52, null, 8 }, // BIT 2, D
        .{ 0x53, null, 8 }, // BIT 2, E
        .{ 0x54, null, 8 }, // BIT 2, H
        .{ 0x55, null, 8 }, // BIT 2, L
        .{ 0x56, null, 12 }, // BIT 2, (HL)
        .{ 0x57, null, 8 }, // BIT 2, A
        .{ 0x58, null, 8 }, // BIT 3, B
        .{ 0x59, null, 8 }, // BIT 3, C
        .{ 0x5A, null, 8 }, // BIT 3, D
        .{ 0x5B, null, 8 }, // BIT 3, E
        .{ 0x5C, null, 8 }, // BIT 3, H
        .{ 0x5D, null, 8 }, // BIT 3, L
        .{ 0x5E, null, 12 }, // BIT 3, (HL)
        .{ 0x5F, null, 8 }, // BIT 3, A
        .{ 0x60, null, 8 }, // BIT 4, B
        .{ 0x61, null, 8 }, // BIT 4, C
        .{ 0x62, null, 8 }, // BIT 4, D
        .{ 0x63, null, 8 }, // BIT 4, E
        .{ 0x64, null, 8 }, // BIT 4, H
        .{ 0x65, null, 8 }, // BIT 4, L
        .{ 0x66, null, 12 }, // BIT 4, (HL)
        .{ 0x67, null, 8 }, // BIT 4, A
        .{ 0x68, null, 8 }, // BIT 5, B
        .{ 0x69, null, 8 }, // BIT 5, C
        .{ 0x6A, null, 8 }, // BIT 5, D
        .{ 0x6B, null, 8 }, // BIT 5, E
        .{ 0x6C, null, 8 }, // BIT 5, H
        .{ 0x6D, null, 8 }, // BIT 5, L
        .{ 0x6E, null, 12 }, // BIT 5, (HL)
        .{ 0x6F, null, 8 }, // BIT 5, A
        .{ 0x70, null, 8 }, // BIT 6, B
        .{ 0x71, null, 8 }, // BIT 6, C
        .{ 0x72, null, 8 }, // BIT 6, D
        .{ 0x73, null, 8 }, // BIT 6, E
        .{ 0x74, null, 8 }, // BIT 6, H
        .{ 0x75, null, 8 }, // BIT 6, L
        .{ 0x76, null, 12 }, // BIT 6, (HL)
        .{ 0x77, null, 8 }, // BIT 6, A
        .{ 0x78, null, 8 }, // BIT 7, B
        .{ 0x79, null, 8 }, // BIT 7, C
        .{ 0x7A, null, 8 }, // BIT 7, D
        .{ 0x7B, null, 8 }, // BIT 7, E
        .{ 0x7C, null, 8 }, // BIT 7, H
        .{ 0x7D, null, 8 }, // BIT 7, L
        .{ 0x7E, null, 12 }, // BIT 7, (HL)
        .{ 0x7F, null, 8 }, // BIT 7, A
        .{ 0x80, null, 8 }, // RES 0, B
        .{ 0x81, null, 8 }, // RES 0, C
        .{ 0x82, null, 8 }, // RES 0, D
        .{ 0x83, null, 8 }, // RES 0, E
        .{ 0x84, null, 8 }, // RES 0, H
        .{ 0x85, null, 8 }, // RES 0, L
        .{ 0x86, null, 16 }, // RES 0, (HL)
        .{ 0x87, null, 8 }, // RES 0, A
        .{ 0x88, null, 8 }, // RES 1, B
        .{ 0x89, null, 8 }, // RES 1, C
        .{ 0x8A, null, 8 }, // RES 1, D
        .{ 0x8B, null, 8 }, // RES 1, E
        .{ 0x8C, null, 8 }, // RES 1, H
        .{ 0x8D, null, 8 }, // RES 1, L
        .{ 0x8E, null, 16 }, // RES 1, (HL)
        .{ 0x8F, null, 8 }, // RES 1, A
        .{ 0x90, null, 8 }, // RES 2, B
        .{ 0x91, null, 8 }, // RES 2, C
        .{ 0x92, null, 8 }, // RES 2, D
        .{ 0x93, null, 8 }, // RES 2, E
        .{ 0x94, null, 8 }, // RES 2, H
        .{ 0x95, null, 8 }, // RES 2, L
        .{ 0x96, null, 16 }, // RES 2, (HL)
        .{ 0x97, null, 8 }, // RES 2, A
        .{ 0x98, null, 8 }, // RES 3, B
        .{ 0x99, null, 8 }, // RES 3, C
        .{ 0x9A, null, 8 }, // RES 3, D
        .{ 0x9B, null, 8 }, // RES 3, E
        .{ 0x9C, null, 8 }, // RES 3, H
        .{ 0x9D, null, 8 }, // RES 3, L
        .{ 0x9E, null, 16 }, // RES 3, (HL)
        .{ 0x9F, null, 8 }, // RES 3, A
        .{ 0xA0, null, 8 }, // RES 4, B
        .{ 0xA1, null, 8 }, // RES 4, C
        .{ 0xA2, null, 8 }, // RES 4, D
        .{ 0xA3, null, 8 }, // RES 4, E
        .{ 0xA4, null, 8 }, // RES 4, H
        .{ 0xA5, null, 8 }, // RES 4, L
        .{ 0xA6, null, 16 }, // RES 4, (HL)
        .{ 0xA7, null, 8 }, // RES 4, A
        .{ 0xA8, null, 8 }, // RES 5, B
        .{ 0xA9, null, 8 }, // RES 5, C
        .{ 0xAA, null, 8 }, // RES 5, D
        .{ 0xAB, null, 8 }, // RES 5, E
        .{ 0xAC, null, 8 }, // RES 5, H
        .{ 0xAD, null, 8 }, // RES 5, L
        .{ 0xAE, null, 16 }, // RES 5, (HL)
        .{ 0xAF, null, 8 }, // RES 5, A
        .{ 0xB0, null, 8 }, // RES 6, B
        .{ 0xB1, null, 8 }, // RES 6, C
        .{ 0xB2, null, 8 }, // RES 6, D
        .{ 0xB3, null, 8 }, // RES 6, E
        .{ 0xB4, null, 8 }, // RES 6, H
        .{ 0xB5, null, 8 }, // RES 6, L
        .{ 0xB6, null, 16 }, // RES 6, (HL)
        .{ 0xB7, null, 8 }, // RES 6, A
        .{ 0xB8, null, 8 }, // RES 7, B
        .{ 0xB9, null, 8 }, // RES 7, C
        .{ 0xBA, null, 8 }, // RES 7, D
        .{ 0xBB, null, 8 }, // RES 7, E
        .{ 0xBC, null, 8 }, // RES 7, H
        .{ 0xBD, null, 8 }, // RES 7, L
        .{ 0xBE, null, 16 }, // RES 7, (HL)
        .{ 0xBF, null, 8 }, // RES 7, A
        .{ 0xC0, null, 8 }, // SET 0, B
        .{ 0xC1, null, 8 }, // SET 0, C
        .{ 0xC2, null, 8 }, // SET 0, D
        .{ 0xC3, null, 8 }, // SET 0, E
        .{ 0xC4, null, 8 }, // SET 0, H
        .{ 0xC5, null, 8 }, // SET 0, L
        .{ 0xC6, null, 16 }, // SET 0, (HL)
        .{ 0xC7, null, 8 }, // SET 0, A
        .{ 0xC8, null, 8 }, // SET 1, B
        .{ 0xC9, null, 8 }, // SET 1, C
        .{ 0xCA, null, 8 }, // SET 1, D
        .{ 0xCB, null, 8 }, // SET 1, E
        .{ 0xCC, null, 8 }, // SET 1, H
        .{ 0xCD, null, 8 }, // SET 1, L
        .{ 0xCE, null, 16 }, // SET 1, (HL)
        .{ 0xCF, null, 8 }, // SET 1, A
        .{ 0xD0, null, 8 }, // SET 2, B
        .{ 0xD1, null, 8 }, // SET 2, C
        .{ 0xD2, null, 8 }, // SET 2, D
        .{ 0xD3, null, 8 }, // SET 2, E
        .{ 0xD4, null, 8 }, // SET 2, H
        .{ 0xD5, null, 8 }, // SET 2, L
        .{ 0xD6, null, 16 }, // SET 2, (HL)
        .{ 0xD7, null, 8 }, // SET 2, A
        .{ 0xD8, null, 8 }, // SET 3, B
        .{ 0xD9, null, 8 }, // SET 3, C
        .{ 0xDA, null, 8 }, // SET 3, D
        .{ 0xDB, null, 8 }, // SET 3, E
        .{ 0xDC, null, 8 }, // SET 3, H
        .{ 0xDD, null, 8 }, // SET 3, L
        .{ 0xDE, null, 16 }, // SET 3, (HL)
        .{ 0xDF, null, 8 }, // SET 3, A
        .{ 0xE0, null, 8 }, // SET 4, B
        .{ 0xE1, null, 8 }, // SET 4, C
        .{ 0xE2, null, 8 }, // SET 4, D
        .{ 0xE3, null, 8 }, // SET 4, E
        .{ 0xE4, null, 8 }, // SET 4, H
        .{ 0xE5, null, 8 }, // SET 4, L
        .{ 0xE6, null, 16 }, // SET 4, (HL)
        .{ 0xE7, null, 8 }, // SET 4, A
        .{ 0xE8, null, 8 }, // SET 5, B
        .{ 0xE9, null, 8 }, // SET 5, C
        .{ 0xEA, null, 8 }, // SET 5, D
        .{ 0xEB, null, 8 }, // SET 5, E
        .{ 0xEC, null, 8 }, // SET 5, H
        .{ 0xED, null, 8 }, // SET 5, L
        .{ 0xEE, null, 16 }, // SET 5, (HL)
        .{ 0xEF, null, 8 }, // SET 5, A
        .{ 0xF0, null, 8 }, // SET 6, B
        .{ 0xF1, null, 8 }, // SET 6, C
        .{ 0xF2, null, 8 }, // SET 6, D
        .{ 0xF3, null, 8 }, // SET 6, E
        .{ 0xF4, null, 8 }, // SET 6, H
        .{ 0xF5, null, 8 }, // SET 6, L
        .{ 0xF6, null, 16 }, // SET 6, (HL)
        .{ 0xF7, null, 8 }, // SET 6, A
        .{ 0xF8, null, 8 }, // SET 7, B
        .{ 0xF9, null, 8 }, // SET 7, C
        .{ 0xFA, null, 8 }, // SET 7, D
        .{ 0xFB, null, 8 }, // SET 7, E
        .{ 0xFC, null, 8 }, // SET 7, H
        .{ 0xFD, null, 8 }, // SET 7, L
        .{ 0xFE, null, 16 }, // SET 7, (HL)
        .{ 0xFF, null, 8 }, // SET 7, A
    };

    fn from_byte(byte: u8, prefixed: bool) !*const fn (*GB) void {
        var ins: ?*const fn (*GB) void = undefined;
        if ((byte >= 0 and byte < table.len)) {
            if (prefixed) {
                print("prefixed\n", .{});
                ins = prefix_table[byte][1];
            } else {
                ins = table[byte][1];
            }
            if (ins == null) return error.NullInstruction;
            return ins.?;
        } else return error.IllegalByte;
    }

    // fn from_prefixed_byte(byte: u8) !*const fn (*CPU) void {
    //     const ins = prefix_table[byte][1];
    //     if ((byte >= 0 and byte < table.len)) {
    //         if (ins == null) return error.NullInstruction;
    //         return ins.?;
    //     } else return error.IllegalByte;
    // }
    fn LD16(gb: *GB, target: regID) u16 { // LD r16, n16
        print("LD16", .{});
        const n: u16 = @as(u16, gb.memory[gb.cpu.pc + 1] << 8) | gb.memory[gb.cpu.pc + 2];
        gb.cpu.set_word(target, n);
        gb.cpu.pc += 3;
    }
    fn LDSP16(gb: *GB) void { // LD r16, n16, 0x31
        print("LD16SP", .{});
        const n: u16 = @as(u16, gb.memory[gb.cpu.pc + 1]) << 8 | gb.memory[gb.cpu.pc + 2];
        print("\n n: {d} to sp: {d}\n", .{n , gb.cpu.sp});
        gb.cpu.sp = n;
        print("after op: sp: {d}\n", .{gb.cpu.sp});
        gb.cpu.pc += 3;
    }
    fn NOP(gb: *GB) void {
        print("NOP", .{});
        gb.cpu.pc += 1;
    }
    fn ADDB(gb: *GB) void {
        print("ADDB", .{});
        gb.cpu.pc += 1;
    }
};
/// Defines a GameBoy CPU: i8080 & Z80 hybrid chip
const CPU = struct {
    const mode = enum { DMG, CGB };
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
        var byte = gb.memory[self.pc];
        var prefixed = false;
        if (byte == 0xCB) {
            prefixed = true;
            self.pc += 1;
            byte = gb.memory[self.pc];
        }
        print("[pc]0x{x} \t(0x{x})\n", .{ self.pc, byte });
        const ins = InstructionSet.from_byte(byte, prefixed) catch |err| {
            return err;
        };
        ins(gb);
        print("\n", .{});
    }
};

const APU = struct {
    // TODO: implement sound

    fn init(self: *@This()) !void {
        _ = self;
        return;
    }
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

pub const GB = struct {
    cpu: CPU = CPU{},
    gpu: GPU = GPU{},
    apu: APU = APU{},
    memory: [0xFFFF]u8 = undefined,

    pub fn init(self: *@This()) !void {
        @memset(&self.memory, 0);
        try self.cpu.init();
        try self.gpu.init(self);
    }

    pub fn readByte(self: *@This(), address: usize) u8 {
        // TODO: implement memory mapping based on address
        return self.memory[address];
    }

    pub fn boot(self: *@This()) !void {
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

    pub fn loadGame(self: *@This(), game_path: []const u8) !void {
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

    pub fn cycle(self: *@This()) !void {
        try self.cpu.execute(self);
        //TODO: Update peripherals & timing
    }

    pub fn mem_dump(self: *@This()) void {
        print("printing bytes:\n", .{});
        for (self.memory, 0..self.memory.len) |value, i| {
            print("0x{x}", .{value});
            if (i != 0 and i % 64 == 0) print("\n", .{}) else continue;
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

    gb.boot() catch |err| { // loads the boot rom and executes it
        print("Couldn't boot GameBoy,\t", .{});
        return err;
    };

    print("\n", .{});

    // const tile = gb.gpu.empty_tile();
    // print("{any}", .{gb.gpu.tile_set});
}
