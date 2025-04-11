const std = @import("std");
const tracy = @import("tracy");
const g = @cImport({
    @cDefine("SDL_DISABLE_OLD_NAMES", {});
    @cInclude("SDL3/SDL.h");
    @cInclude("SDL3/SDL_revision.h");
    @cDefine("SDL_MAIN_HANDLED", {});
    @cInclude("SDL3/SDL_main.h");
});

var DEBUG = true; // Set to false to disable debug prints
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
    const InstrInfo = struct { op: u8, ins: InstrFn, args: InstrArgs, cycles: u8 };
    const table = [_]InstrInfo{
        InstrInfo{ .op = 0x00, .ins = NOP, .args = .{ .none = {} }, .cycles = 1 }, // NOP
        InstrInfo{ .op = 0x01, .ins = LD16, .args = .{ .target = regID.b }, .cycles = 3 }, // LD BC, d16
        InstrInfo{ .op = 0x02, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD (BC), A
        InstrInfo{ .op = 0x03, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // INC BC
        InstrInfo{ .op = 0x04, .ins = INCr8, .args = .{ .target = regID.b }, .cycles = 1 }, // INC B
        InstrInfo{ .op = 0x05, .ins = DECr8, .args = .{ .target = regID.b }, .cycles = 1 }, // DEC B
        InstrInfo{ .op = 0x06, .ins = LD8, .args = .{ .target = regID.b }, .cycles = 2 }, // LD B, d8
        InstrInfo{ .op = 0x07, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // RLCA
        InstrInfo{ .op = 0x08, .ins = undefined, .args = .{ .none = {} }, .cycles = 20 }, // LD (a16), SP
        InstrInfo{ .op = 0x09, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // ADD HL, BC
        InstrInfo{ .op = 0x0A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD A, (BC)
        InstrInfo{ .op = 0x0B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // DEC BC
        InstrInfo{ .op = 0x0C, .ins = INCr8, .args = .{ .target = regID.c }, .cycles = 1 }, // INC C
        InstrInfo{ .op = 0x0D, .ins = DECr8, .args = .{ .target = regID.c }, .cycles = 1 }, // DEC C
        InstrInfo{ .op = 0x0E, .ins = LD8, .args = .{ .target = regID.c }, .cycles = 2 }, // LD C, d8
        InstrInfo{ .op = 0x0F, .ins = RRCA, .args = .{ .none = {} }, .cycles = 1 }, // RRCA
        InstrInfo{ .op = 0x10, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // STOP 0
        InstrInfo{ .op = 0x11, .ins = LD16, .args = .{ .target = regID.d }, .cycles = 3 }, // LD DE, d16
        InstrInfo{ .op = 0x12, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD (DE), A
        InstrInfo{ .op = 0x13, .ins = INCr16, .args = .{ .target = regID.d }, .cycles = 2 }, // INC DE
        InstrInfo{ .op = 0x14, .ins = INCr8, .args = .{ .target = regID.d }, .cycles = 1 }, // INC D
        InstrInfo{ .op = 0x15, .ins = DECr8, .args = .{ .target = regID.d }, .cycles = 1 }, // DEC D
        InstrInfo{ .op = 0x16, .ins = LD8, .args = .{ .target = regID.d }, .cycles = 2 }, // LD D, d8
        InstrInfo{ .op = 0x17, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // RLA
        InstrInfo{ .op = 0x18, .ins = JR, .args = .{ .flagConditions = .{ .none = true } }, .cycles = 2 }, // JR r8
        InstrInfo{ .op = 0x19, .ins = ADDHLr16, .args = .{ .target = regID.d }, .cycles = 2 }, // ADD HL, DE
        InstrInfo{ .op = 0x1A, .ins = LDAr16, .args = .{ .target = regID.d }, .cycles = 2 }, // LD A, (DE)
        InstrInfo{ .op = 0x1B, .ins = DECr16, .args = .{ .target = regID.d }, .cycles = 2 }, // DEC DE
        InstrInfo{ .op = 0x1C, .ins = INCr8, .args = .{ .target = regID.e }, .cycles = 1 }, // INC E
        InstrInfo{ .op = 0x1D, .ins = DECr8, .args = .{ .target = regID.e }, .cycles = 1 }, // DEC E
        InstrInfo{ .op = 0x1E, .ins = LD8, .args = .{ .target = regID.e }, .cycles = 2 }, // LD E, d8
        InstrInfo{ .op = 0x1F, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // RRA
        InstrInfo{ .op = 0x20, .ins = JR, .args = .{ .flagConditions = .{ .z = false } }, .cycles = 3 }, // JR NZ, r8
        InstrInfo{ .op = 0x21, .ins = LD16, .args = .{ .target = regID.h }, .cycles = 3 }, // LD HL, d16
        InstrInfo{ .op = 0x22, .ins = LDHLIA, .args = .{ .none = {} }, .cycles = 2 }, // LD (HL+), A
        InstrInfo{ .op = 0x23, .ins = INCr16, .args = .{ .target = regID.h }, .cycles = 2 }, // INC HL
        InstrInfo{ .op = 0x24, .ins = INCr8, .args = .{ .target = regID.h }, .cycles = 1 }, // INC H
        InstrInfo{ .op = 0x25, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // DEC H
        InstrInfo{ .op = 0x26, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD H, d8
        InstrInfo{ .op = 0x27, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // DAA
        InstrInfo{ .op = 0x28, .ins = JR, .args = .{ .flagConditions = .{ .z = true } }, .cycles = 3 }, // JR Z, r8
        InstrInfo{ .op = 0x29, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // ADD HL, HL
        InstrInfo{ .op = 0x2A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD A, (HL+)
        InstrInfo{ .op = 0x2B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // DEC HL
        InstrInfo{ .op = 0x2C, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // INC L
        InstrInfo{ .op = 0x2D, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // DEC L
        InstrInfo{ .op = 0x2E, .ins = LD8, .args = .{ .target = regID.l }, .cycles = 2 }, // LD L, d8
        InstrInfo{ .op = 0x2F, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // CPL
        InstrInfo{ .op = 0x30, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // JR NC, r8
        InstrInfo{ .op = 0x31, .ins = LDSP16, .args = .{ .none = {} }, .cycles = 3 }, // LD SP, d16
        InstrInfo{ .op = 0x32, .ins = LDHLDA, .args = .{ .none = {} }, .cycles = 2 }, // LD (HL-), A
        InstrInfo{ .op = 0x33, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // INC SP
        InstrInfo{ .op = 0x34, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // INC (HL)
        InstrInfo{ .op = 0x35, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // DEC (HL)
        InstrInfo{ .op = 0x36, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // LD (HL), d8
        InstrInfo{ .op = 0x37, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // SCF
        InstrInfo{ .op = 0x38, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // JR C, r8
        InstrInfo{ .op = 0x39, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // ADD HL, SP
        InstrInfo{ .op = 0x3A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD A, (HL-)
        InstrInfo{ .op = 0x3B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // DEC SP
        InstrInfo{ .op = 0x3C, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // INC A
        InstrInfo{ .op = 0x3D, .ins = DECr8, .args = .{ .target = regID.a }, .cycles = 1 }, // DEC A
        InstrInfo{ .op = 0x3E, .ins = LD8, .args = .{ .target = regID.a }, .cycles = 1 }, // LD A, d8
        InstrInfo{ .op = 0x3F, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // CCF
        InstrInfo{ .op = 0x40, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD B, B
        InstrInfo{ .op = 0x41, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD B, C
        InstrInfo{ .op = 0x42, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD B, D
        InstrInfo{ .op = 0x43, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD B, E
        InstrInfo{ .op = 0x44, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD B, H
        InstrInfo{ .op = 0x45, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD B, L
        InstrInfo{ .op = 0x46, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD B, (HL)
        InstrInfo{ .op = 0x47, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD B, A
        InstrInfo{ .op = 0x48, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD C, B
        InstrInfo{ .op = 0x49, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD C, C
        InstrInfo{ .op = 0x4A, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD C, D
        InstrInfo{ .op = 0x4B, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD C, E
        InstrInfo{ .op = 0x4C, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD C, H
        InstrInfo{ .op = 0x4D, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD C, L
        InstrInfo{ .op = 0x4E, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD C, (HL)
        InstrInfo{ .op = 0x4F, .ins = LDr8, .args = .{ .targets = .{ .to = regID.c, .from = regID.a } }, .cycles = 1 }, // LD C, A
        InstrInfo{ .op = 0x50, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD D, B
        InstrInfo{ .op = 0x51, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD D, C
        InstrInfo{ .op = 0x52, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD D, D
        InstrInfo{ .op = 0x53, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD D, E
        InstrInfo{ .op = 0x54, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD D, H
        InstrInfo{ .op = 0x55, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD D, L
        InstrInfo{ .op = 0x56, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD D, (HL)
        InstrInfo{ .op = 0x57, .ins = LDr8, .args = .{ .targets = .{ .to = regID.d, .from = regID.a } }, .cycles = 1 }, // LD D, A
        InstrInfo{ .op = 0x58, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD E, B
        InstrInfo{ .op = 0x59, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD E, C
        InstrInfo{ .op = 0x5A, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD E, D
        InstrInfo{ .op = 0x5B, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD E, E
        InstrInfo{ .op = 0x5C, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD E, H
        InstrInfo{ .op = 0x5D, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD E, L
        InstrInfo{ .op = 0x5E, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD E, (HL)
        InstrInfo{ .op = 0x5F, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD E, A
        InstrInfo{ .op = 0x60, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD H, B
        InstrInfo{ .op = 0x61, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD H, C
        InstrInfo{ .op = 0x62, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD H, D
        InstrInfo{ .op = 0x63, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD H, E
        InstrInfo{ .op = 0x64, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD H, H
        InstrInfo{ .op = 0x65, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD H, L
        InstrInfo{ .op = 0x66, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD H, (HL)
        InstrInfo{ .op = 0x67, .ins = LDr8, .args = .{ .targets = .{ .to = regID.h, .from = regID.a } }, .cycles = 1 }, // LD H, A
        InstrInfo{ .op = 0x68, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD L, B
        InstrInfo{ .op = 0x69, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD L, C
        InstrInfo{ .op = 0x6A, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD L, D
        InstrInfo{ .op = 0x6B, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD L, E
        InstrInfo{ .op = 0x6C, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD L, H
        InstrInfo{ .op = 0x6D, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD L, L
        InstrInfo{ .op = 0x6E, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD L, (HL)
        InstrInfo{ .op = 0x6F, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // LD L, A
        InstrInfo{ .op = 0x70, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD (HL), B
        InstrInfo{ .op = 0x71, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD (HL), C
        InstrInfo{ .op = 0x72, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD (HL), D
        InstrInfo{ .op = 0x73, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD (HL), E
        InstrInfo{ .op = 0x74, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD (HL), H
        InstrInfo{ .op = 0x75, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD (HL), L
        InstrInfo{ .op = 0x76, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // HALT
        InstrInfo{ .op = 0x77, .ins = LDHLR, .args = .{ .target = regID.a }, .cycles = 2 }, // LD (HL), A
        InstrInfo{ .op = 0x78, .ins = LDr8, .args = .{ .targets = .{ .to = regID.a, .from = regID.b } }, .cycles = 1 }, // LD A, B
        InstrInfo{ .op = 0x79, .ins = LDr8, .args = .{ .targets = .{ .to = regID.a, .from = regID.c } }, .cycles = 1 }, // LD A, C
        InstrInfo{ .op = 0x7A, .ins = LDr8, .args = .{ .targets = .{ .to = regID.a, .from = regID.d } }, .cycles = 1 }, // LD A, D
        InstrInfo{ .op = 0x7B, .ins = LDr8, .args = .{ .targets = .{ .to = regID.a, .from = regID.e } }, .cycles = 1 }, // LD A, E
        InstrInfo{ .op = 0x7C, .ins = LDr8, .args = .{ .targets = .{ .to = regID.a, .from = regID.h } }, .cycles = 1 }, // LD A, H
        InstrInfo{ .op = 0x7D, .ins = LDr8, .args = .{ .targets = .{ .to = regID.a, .from = regID.l } }, .cycles = 1 }, // LD A, L
        InstrInfo{ .op = 0x7E, .ins = LDAHL, .args = .{ .none = {} }, .cycles = 8 }, // LD A, (HL)
        InstrInfo{ .op = 0x7F, .ins = LDr8, .args = .{ .targets = .{ .to = regID.a, .from = regID.a } }, .cycles = 1 }, // LD A, A
        InstrInfo{ .op = 0x80, .ins = ADDAr8, .args = .{ .target = regID.b }, .cycles = 1 }, // ADD A, B
        InstrInfo{ .op = 0x81, .ins = ADDAr8, .args = .{ .target = regID.c }, .cycles = 1 }, // ADD A, C
        InstrInfo{ .op = 0x82, .ins = ADDAr8, .args = .{ .target = regID.d }, .cycles = 1 }, // ADD A, D
        InstrInfo{ .op = 0x83, .ins = ADDAr8, .args = .{ .target = regID.e }, .cycles = 1 }, // ADD A, E
        InstrInfo{ .op = 0x84, .ins = ADDAr8, .args = .{ .target = regID.h }, .cycles = 1 }, // ADD A, H
        InstrInfo{ .op = 0x85, .ins = ADDAr8, .args = .{ .target = regID.l }, .cycles = 1 }, // ADD A, L
        InstrInfo{ .op = 0x86, .ins = ADDAHL, .args = .{ .none = {} }, .cycles = 2 }, // ADD A, (HL)
        InstrInfo{ .op = 0x87, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // ADD A, A
        InstrInfo{ .op = 0x88, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // ADC A, B
        InstrInfo{ .op = 0x89, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // ADC A, C
        InstrInfo{ .op = 0x8A, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // ADC A, D
        InstrInfo{ .op = 0x8B, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // ADC A, E
        InstrInfo{ .op = 0x8C, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // ADC A, H
        InstrInfo{ .op = 0x8D, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // ADC A, L
        InstrInfo{ .op = 0x8E, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // ADC A, (HL)
        InstrInfo{ .op = 0x8F, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // ADC A, A
        InstrInfo{ .op = 0x90, .ins = SUBAr8, .args = .{ .target = regID.b }, .cycles = 1 }, // SUB B
        InstrInfo{ .op = 0x91, .ins = SUBAr8, .args = .{ .target = regID.c }, .cycles = 1 }, // SUB C
        InstrInfo{ .op = 0x92, .ins = SUBAr8, .args = .{ .target = regID.d }, .cycles = 1 }, // SUB D
        InstrInfo{ .op = 0x93, .ins = SUBAr8, .args = .{ .target = regID.e }, .cycles = 1 }, // SUB E
        InstrInfo{ .op = 0x94, .ins = SUBAr8, .args = .{ .target = regID.h }, .cycles = 1 }, // SUB H
        InstrInfo{ .op = 0x95, .ins = SUBAr8, .args = .{ .target = regID.l }, .cycles = 1 }, // SUB L
        InstrInfo{ .op = 0x96, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SUB (HL)
        InstrInfo{ .op = 0x97, .ins = SUBAr8, .args = .{ .target = regID.a }, .cycles = 1 }, // SUB A
        InstrInfo{ .op = 0x98, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // SBC A, B
        InstrInfo{ .op = 0x99, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // SBC A, C
        InstrInfo{ .op = 0x9A, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // SBC A, D
        InstrInfo{ .op = 0x9B, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // SBC A, E
        InstrInfo{ .op = 0x9C, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // SBC A, H
        InstrInfo{ .op = 0x9D, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // SBC A, L
        InstrInfo{ .op = 0x9E, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SBC A, (HL)
        InstrInfo{ .op = 0x9F, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // SBC A, A
        InstrInfo{ .op = 0xA0, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // AND B
        InstrInfo{ .op = 0xA1, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // AND C
        InstrInfo{ .op = 0xA2, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // AND D
        InstrInfo{ .op = 0xA3, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // AND E
        InstrInfo{ .op = 0xA4, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // AND H
        InstrInfo{ .op = 0xA5, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // AND L
        InstrInfo{ .op = 0xA6, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // AND (HL)
        InstrInfo{ .op = 0xA7, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // AND A
        InstrInfo{ .op = 0xA8, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // XOR B
        InstrInfo{ .op = 0xA9, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // XOR C
        InstrInfo{ .op = 0xAA, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // XOR D
        InstrInfo{ .op = 0xAB, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // XOR E
        InstrInfo{ .op = 0xAC, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // XOR H
        InstrInfo{ .op = 0xAD, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // XOR L
        InstrInfo{ .op = 0xAE, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // XOR (HL)
        InstrInfo{ .op = 0xAF, .ins = XORA, .args = .{ .target = regID.a }, .cycles = 1 }, // XOR A
        InstrInfo{ .op = 0xB0, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // OR B
        InstrInfo{ .op = 0xB1, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // OR C
        InstrInfo{ .op = 0xB2, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // OR D
        InstrInfo{ .op = 0xB3, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // OR E
        InstrInfo{ .op = 0xB4, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // OR H
        InstrInfo{ .op = 0xB5, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // OR L
        InstrInfo{ .op = 0xB6, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // OR (HL)
        InstrInfo{ .op = 0xB7, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // OR A
        InstrInfo{ .op = 0xB8, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // CP B
        InstrInfo{ .op = 0xB9, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // CP C
        InstrInfo{ .op = 0xBA, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // CP D
        InstrInfo{ .op = 0xBB, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // CP E
        InstrInfo{ .op = 0xBC, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // CP H
        InstrInfo{ .op = 0xBD, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // CP L
        InstrInfo{ .op = 0xBE, .ins = CPAHL, .args = .{ .none = {} }, .cycles = 2 }, // CP (HL)
        InstrInfo{ .op = 0xBF, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // CP A
        InstrInfo{ .op = 0xC0, .ins = undefined, .args = .{ .none = {} }, .cycles = 20 }, // RET NZ
        InstrInfo{ .op = 0xC1, .ins = POP, .args = .{ .target = regID.b }, .cycles = 3 }, // POP BC
        InstrInfo{ .op = 0xC2, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // JP NZ, a16
        InstrInfo{ .op = 0xC3, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // JP a16
        InstrInfo{ .op = 0xC4, .ins = undefined, .args = .{ .none = {} }, .cycles = 24 }, // CALL NZ, a16
        InstrInfo{ .op = 0xC5, .ins = PUSH, .args = .{ .target = regID.b }, .cycles = 1 }, // PUSH BC
        InstrInfo{ .op = 0xC6, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // ADD A, d8
        InstrInfo{ .op = 0xC7, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RST 00H
        InstrInfo{ .op = 0xC8, .ins = undefined, .args = .{ .none = {} }, .cycles = 20 }, // RET Z
        InstrInfo{ .op = 0xC9, .ins = RET, .args = .{ .flagConditions = .{ .none = true } }, .cycles = 2 }, // RET
        InstrInfo{ .op = 0xCA, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // JP Z, a16
        InstrInfo{ .op = 0xCB, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // PREFIX CB
        InstrInfo{ .op = 0xCC, .ins = undefined, .args = .{ .none = {} }, .cycles = 24 }, // CALL Z, a16
        InstrInfo{ .op = 0xCD, .ins = CALLn16, .args = .{ .flagConditions = .{ .none = true } }, .cycles = 3 }, // CALL a16
        InstrInfo{ .op = 0xCE, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // ADC A, d8
        InstrInfo{ .op = 0xCF, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RST 08H
        InstrInfo{ .op = 0xD0, .ins = undefined, .args = .{ .none = {} }, .cycles = 20 }, // RET NC
        InstrInfo{ .op = 0xD1, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // POP DE
        InstrInfo{ .op = 0xD2, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // JP NC, a16
        InstrInfo{ .op = 0xD3, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // NOP
        InstrInfo{ .op = 0xD4, .ins = undefined, .args = .{ .none = {} }, .cycles = 24 }, // CALL NC, a16
        InstrInfo{ .op = 0xD5, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // PUSH DE
        InstrInfo{ .op = 0xD6, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SUB d8
        InstrInfo{ .op = 0xD7, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RST 10H
        InstrInfo{ .op = 0xD8, .ins = undefined, .args = .{ .none = {} }, .cycles = 20 }, // RET C
        InstrInfo{ .op = 0xD9, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RETI
        InstrInfo{ .op = 0xDA, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // JP C, a16
        InstrInfo{ .op = 0xDB, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // NOP
        InstrInfo{ .op = 0xDC, .ins = undefined, .args = .{ .none = {} }, .cycles = 24 }, // CALL C, a16
        InstrInfo{ .op = 0xDD, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // NOP
        InstrInfo{ .op = 0xDE, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SBC A, d8
        InstrInfo{ .op = 0xDF, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RST 18H
        InstrInfo{ .op = 0xE0, .ins = LDHn16A, .args = .{ .none = {} }, .cycles = 3 }, // LDH (a8), A
        InstrInfo{ .op = 0xE1, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // POP HL
        InstrInfo{ .op = 0xE2, .ins = LDHCA, .args = .{ .none = {} }, .cycles = 2 }, // LD (C), A
        InstrInfo{ .op = 0xE3, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // NOP
        InstrInfo{ .op = 0xE4, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // NOP
        InstrInfo{ .op = 0xE5, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // PUSH HL
        InstrInfo{ .op = 0xE6, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // AND d8
        InstrInfo{ .op = 0xE7, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RST 20H
        InstrInfo{ .op = 0xE8, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // ADD SP, r8
        InstrInfo{ .op = 0xE9, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // JP (HL)
        InstrInfo{ .op = 0xEA, .ins = LDn16A, .args = .{ .none = {} }, .cycles = 4 }, // LD (a16), A
        InstrInfo{ .op = 0xEB, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // NOP
        InstrInfo{ .op = 0xEC, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // NOP
        InstrInfo{ .op = 0xED, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // NOP
        InstrInfo{ .op = 0xEE, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // XOR d8
        InstrInfo{ .op = 0xEF, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RST 28H
        InstrInfo{ .op = 0xF0, .ins = LDHAn16, .args = .{ .none = {} }, .cycles = 3 }, // LDH A, (a8)
        InstrInfo{ .op = 0xF1, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // POP AF
        InstrInfo{ .op = 0xF2, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD A, (C)
        InstrInfo{ .op = 0xF3, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // DI
        InstrInfo{ .op = 0xF4, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // NOP
        InstrInfo{ .op = 0xF5, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // PUSH AF
        InstrInfo{ .op = 0xF6, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // OR d8
        InstrInfo{ .op = 0xF7, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RST 30H
        InstrInfo{ .op = 0xF8, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // LD HL, SP+r8
        InstrInfo{ .op = 0xF9, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // LD SP, HL
        InstrInfo{ .op = 0xFA, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // LD A, (a16)
        InstrInfo{ .op = 0xFB, .ins = EI, .args = .{ .none = {} }, .cycles = 1 }, // EI
        InstrInfo{ .op = 0xFC, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // NOP
        InstrInfo{ .op = 0xFD, .ins = undefined, .args = .{ .none = {} }, .cycles = 4 }, // NOP
        InstrInfo{ .op = 0xFE, .ins = CPAn8, .args = .{ .none = {} }, .cycles = 2 }, // CP d8
        InstrInfo{ .op = 0xFF, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RST 38H
    };

    const prefix_table = [_]InstrInfo{
        InstrInfo{ .op = 0x00, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RLC B
        InstrInfo{ .op = 0x01, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RLC C
        InstrInfo{ .op = 0x02, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RLC D
        InstrInfo{ .op = 0x03, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RLC E
        InstrInfo{ .op = 0x04, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RLC H
        InstrInfo{ .op = 0x05, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RLC L
        InstrInfo{ .op = 0x06, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RLC (HL)
        InstrInfo{ .op = 0x07, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RLC A
        InstrInfo{ .op = 0x08, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RRC B
        InstrInfo{ .op = 0x09, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RRC C
        InstrInfo{ .op = 0x0A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RRC D
        InstrInfo{ .op = 0x0B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RRC E
        InstrInfo{ .op = 0x0C, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RRC H
        InstrInfo{ .op = 0x0D, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RRC L
        InstrInfo{ .op = 0x0E, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RRC (HL)
        InstrInfo{ .op = 0x0F, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RRC A
        InstrInfo{ .op = 0x10, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RL B
        InstrInfo{ .op = 0x11, .ins = RL, .args = .{ .target = regID.c }, .cycles = 8 }, // RL C
        InstrInfo{ .op = 0x12, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RL D
        InstrInfo{ .op = 0x13, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RL E
        InstrInfo{ .op = 0x14, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RL H
        InstrInfo{ .op = 0x15, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RL L
        InstrInfo{ .op = 0x16, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RL (HL)
        InstrInfo{ .op = 0x17, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RL A
        InstrInfo{ .op = 0x18, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RR B
        InstrInfo{ .op = 0x19, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RR C
        InstrInfo{ .op = 0x1A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RR D
        InstrInfo{ .op = 0x1B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RR E
        InstrInfo{ .op = 0x1C, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RR H
        InstrInfo{ .op = 0x1D, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RR L
        InstrInfo{ .op = 0x1E, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RR (HL)
        InstrInfo{ .op = 0x1F, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RR A
        InstrInfo{ .op = 0x20, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SLA B
        InstrInfo{ .op = 0x21, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SLA C
        InstrInfo{ .op = 0x22, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SLA D
        InstrInfo{ .op = 0x23, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SLA E
        InstrInfo{ .op = 0x24, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SLA H
        InstrInfo{ .op = 0x25, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SLA L
        InstrInfo{ .op = 0x26, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // SLA (HL)
        InstrInfo{ .op = 0x27, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SLA A
        InstrInfo{ .op = 0x28, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRA B
        InstrInfo{ .op = 0x29, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRA C
        InstrInfo{ .op = 0x2A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRA D
        InstrInfo{ .op = 0x2B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRA E
        InstrInfo{ .op = 0x2C, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRA H
        InstrInfo{ .op = 0x2D, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRA L
        InstrInfo{ .op = 0x2E, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // SRA (HL)
        InstrInfo{ .op = 0x2F, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRA A
        InstrInfo{ .op = 0x30, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SWAP B
        InstrInfo{ .op = 0x31, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SWAP C
        InstrInfo{ .op = 0x32, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SWAP D
        InstrInfo{ .op = 0x33, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SWAP E
        InstrInfo{ .op = 0x34, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SWAP H
        InstrInfo{ .op = 0x35, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SWAP L
        InstrInfo{ .op = 0x36, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // SWAP (HL)
        InstrInfo{ .op = 0x37, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SWAP A
        InstrInfo{ .op = 0x38, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRL B
        InstrInfo{ .op = 0x39, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRL C
        InstrInfo{ .op = 0x3A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRL D
        InstrInfo{ .op = 0x3B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRL E
        InstrInfo{ .op = 0x3C, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRL H
        InstrInfo{ .op = 0x3D, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRL L
        InstrInfo{ .op = 0x3E, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // SRL (HL)
        InstrInfo{ .op = 0x3F, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SRL A
        InstrInfo{ .op = 0x40, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 0, B
        InstrInfo{ .op = 0x41, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 0, C
        InstrInfo{ .op = 0x42, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 0, D
        InstrInfo{ .op = 0x43, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 0, E
        InstrInfo{ .op = 0x44, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 0, H
        InstrInfo{ .op = 0x45, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 0, L
        InstrInfo{ .op = 0x46, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // BIT 0, (HL)
        InstrInfo{ .op = 0x47, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 0, A
        InstrInfo{ .op = 0x48, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 1, B
        InstrInfo{ .op = 0x49, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 1, C
        InstrInfo{ .op = 0x4A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 1, D
        InstrInfo{ .op = 0x4B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 1, E
        InstrInfo{ .op = 0x4C, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 1, H
        InstrInfo{ .op = 0x4D, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 1, L
        InstrInfo{ .op = 0x4E, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // BIT 1, (HL)
        InstrInfo{ .op = 0x4F, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 1, A
        InstrInfo{ .op = 0x50, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 2, B
        InstrInfo{ .op = 0x51, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 2, C
        InstrInfo{ .op = 0x52, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 2, D
        InstrInfo{ .op = 0x53, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 2, E
        InstrInfo{ .op = 0x54, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 2, H
        InstrInfo{ .op = 0x55, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 2, L
        InstrInfo{ .op = 0x56, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // BIT 2, (HL)
        InstrInfo{ .op = 0x57, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 2, A
        InstrInfo{ .op = 0x58, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 3, B
        InstrInfo{ .op = 0x59, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 3, C
        InstrInfo{ .op = 0x5A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 3, D
        InstrInfo{ .op = 0x5B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 3, E
        InstrInfo{ .op = 0x5C, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 3, H
        InstrInfo{ .op = 0x5D, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 3, L
        InstrInfo{ .op = 0x5E, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // BIT 3, (HL)
        InstrInfo{ .op = 0x5F, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 3, A
        InstrInfo{ .op = 0x60, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 4, B
        InstrInfo{ .op = 0x61, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 4, C
        InstrInfo{ .op = 0x62, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 4, D
        InstrInfo{ .op = 0x63, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 4, E
        InstrInfo{ .op = 0x64, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 4, H
        InstrInfo{ .op = 0x65, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 4, L
        InstrInfo{ .op = 0x66, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // BIT 4, (HL)
        InstrInfo{ .op = 0x67, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 4, A
        InstrInfo{ .op = 0x68, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 5, B
        InstrInfo{ .op = 0x69, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 5, C
        InstrInfo{ .op = 0x6A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 5, D
        InstrInfo{ .op = 0x6B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 5, E
        InstrInfo{ .op = 0x6C, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 5, H
        InstrInfo{ .op = 0x6D, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 5, L
        InstrInfo{ .op = 0x6E, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // BIT 5, (HL)
        InstrInfo{ .op = 0x6F, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 5, A
        InstrInfo{ .op = 0x70, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 6, B
        InstrInfo{ .op = 0x71, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 6, C
        InstrInfo{ .op = 0x72, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 6, D
        InstrInfo{ .op = 0x73, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 6, E
        InstrInfo{ .op = 0x74, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 6, H
        InstrInfo{ .op = 0x75, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 6, L
        InstrInfo{ .op = 0x76, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // BIT 6, (HL)
        InstrInfo{ .op = 0x77, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 6, A
        InstrInfo{ .op = 0x78, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 7, B
        InstrInfo{ .op = 0x79, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 7, C
        InstrInfo{ .op = 0x7A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 7, D
        InstrInfo{ .op = 0x7B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 7, E
        InstrInfo{ .op = 0x7C, .ins = BITTEST, .args = .{ .bit_target = .{ .bit = 7, .target = regID.h } }, .cycles = 3 }, // BIT 7, H
        InstrInfo{ .op = 0x7D, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 7, L
        InstrInfo{ .op = 0x7E, .ins = undefined, .args = .{ .none = {} }, .cycles = 12 }, // BIT 7, (HL)
        InstrInfo{ .op = 0x7F, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // BIT 7, A
        InstrInfo{ .op = 0x80, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 0, B
        InstrInfo{ .op = 0x81, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 0, C
        InstrInfo{ .op = 0x82, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 0, D
        InstrInfo{ .op = 0x83, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 0, E
        InstrInfo{ .op = 0x84, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 0, H
        InstrInfo{ .op = 0x85, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 0, L
        InstrInfo{ .op = 0x86, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RES 0, (HL)
        InstrInfo{ .op = 0x87, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 0, A
        InstrInfo{ .op = 0x88, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 1, B
        InstrInfo{ .op = 0x89, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 1, C
        InstrInfo{ .op = 0x8A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 1, D
        InstrInfo{ .op = 0x8B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 1, E
        InstrInfo{ .op = 0x8C, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 1, H
        InstrInfo{ .op = 0x8D, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 1, L
        InstrInfo{ .op = 0x8E, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RES 1, (HL)
        InstrInfo{ .op = 0x8F, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 1, A
        InstrInfo{ .op = 0x90, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 2, B
        InstrInfo{ .op = 0x91, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 2, C
        InstrInfo{ .op = 0x92, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 2, D
        InstrInfo{ .op = 0x93, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 2, E
        InstrInfo{ .op = 0x94, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 2, H
        InstrInfo{ .op = 0x95, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 2, L
        InstrInfo{ .op = 0x96, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RES 2, (HL)
        InstrInfo{ .op = 0x97, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 2, A
        InstrInfo{ .op = 0x98, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 3, B
        InstrInfo{ .op = 0x99, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 3, C
        InstrInfo{ .op = 0x9A, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 3, D
        InstrInfo{ .op = 0x9B, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 3, E
        InstrInfo{ .op = 0x9C, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 3, H
        InstrInfo{ .op = 0x9D, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 3, L
        InstrInfo{ .op = 0x9E, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RES 3, (HL)
        InstrInfo{ .op = 0x9F, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 3, A
        InstrInfo{ .op = 0xA0, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 4, B
        InstrInfo{ .op = 0xA1, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 4, C
        InstrInfo{ .op = 0xA2, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 4, D
        InstrInfo{ .op = 0xA3, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 4, E
        InstrInfo{ .op = 0xA4, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 4, H
        InstrInfo{ .op = 0xA5, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 4, L
        InstrInfo{ .op = 0xA6, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RES 4, (HL)
        InstrInfo{ .op = 0xA7, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 4, A
        InstrInfo{ .op = 0xA8, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 5, B
        InstrInfo{ .op = 0xA9, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 5, C
        InstrInfo{ .op = 0xAA, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 5, D
        InstrInfo{ .op = 0xAB, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 5, E
        InstrInfo{ .op = 0xAC, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 5, H
        InstrInfo{ .op = 0xAD, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 5, L
        InstrInfo{ .op = 0xAE, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RES 5, (HL)
        InstrInfo{ .op = 0xAF, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 5, A
        InstrInfo{ .op = 0xB0, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 6, B
        InstrInfo{ .op = 0xB1, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 6, C
        InstrInfo{ .op = 0xB2, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 6, D
        InstrInfo{ .op = 0xB3, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 6, E
        InstrInfo{ .op = 0xB4, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 6, H
        InstrInfo{ .op = 0xB5, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 6, L
        InstrInfo{ .op = 0xB6, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RES 6, (HL)
        InstrInfo{ .op = 0xB7, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 6, A
        InstrInfo{ .op = 0xB8, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 7, B
        InstrInfo{ .op = 0xB9, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 7, C
        InstrInfo{ .op = 0xBA, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 7, D
        InstrInfo{ .op = 0xBB, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 7, E
        InstrInfo{ .op = 0xBC, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 7, H
        InstrInfo{ .op = 0xBD, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 7, L
        InstrInfo{ .op = 0xBE, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // RES 7, (HL)
        InstrInfo{ .op = 0xBF, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // RES 7, A
        InstrInfo{ .op = 0xC0, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 0, B
        InstrInfo{ .op = 0xC1, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 0, C
        InstrInfo{ .op = 0xC2, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 0, D
        InstrInfo{ .op = 0xC3, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 0, E
        InstrInfo{ .op = 0xC4, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 0, H
        InstrInfo{ .op = 0xC5, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 0, L
        InstrInfo{ .op = 0xC6, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // SET 0, (HL)
        InstrInfo{ .op = 0xC7, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 0, A
        InstrInfo{ .op = 0xC8, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 1, B
        InstrInfo{ .op = 0xC9, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 1, C
        InstrInfo{ .op = 0xCA, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 1, D
        InstrInfo{ .op = 0xCB, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 1, E
        InstrInfo{ .op = 0xCC, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 1, H
        InstrInfo{ .op = 0xCD, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 1, L
        InstrInfo{ .op = 0xCE, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // SET 1, (HL)
        InstrInfo{ .op = 0xCF, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 1, A
        InstrInfo{ .op = 0xD0, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 2, B
        InstrInfo{ .op = 0xD1, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 2, C
        InstrInfo{ .op = 0xD2, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 2, D
        InstrInfo{ .op = 0xD3, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 2, E
        InstrInfo{ .op = 0xD4, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 2, H
        InstrInfo{ .op = 0xD5, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 2, L
        InstrInfo{ .op = 0xD6, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // SET 2, (HL)
        InstrInfo{ .op = 0xD7, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 2, A
        InstrInfo{ .op = 0xD8, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 3, B
        InstrInfo{ .op = 0xD9, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 3, C
        InstrInfo{ .op = 0xDA, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 3, D
        InstrInfo{ .op = 0xDB, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 3, E
        InstrInfo{ .op = 0xDC, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 3, H
        InstrInfo{ .op = 0xDD, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 3, L
        InstrInfo{ .op = 0xDE, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // SET 3, (HL)
        InstrInfo{ .op = 0xDF, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 3, A
        InstrInfo{ .op = 0xE0, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 4, B
        InstrInfo{ .op = 0xE1, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 4, C
        InstrInfo{ .op = 0xE2, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 4, D
        InstrInfo{ .op = 0xE3, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 4, E
        InstrInfo{ .op = 0xE4, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 4, H
        InstrInfo{ .op = 0xE5, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 4, L
        InstrInfo{ .op = 0xE6, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // SET 4, (HL)
        InstrInfo{ .op = 0xE7, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 4, A
        InstrInfo{ .op = 0xE8, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 5, B
        InstrInfo{ .op = 0xE9, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 5, C
        InstrInfo{ .op = 0xEA, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 5, D
        InstrInfo{ .op = 0xEB, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 5, E
        InstrInfo{ .op = 0xEC, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 5, H
        InstrInfo{ .op = 0xED, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 5, L
        InstrInfo{ .op = 0xEE, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // SET 5, (HL)
        InstrInfo{ .op = 0xEF, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 5, A
        InstrInfo{ .op = 0xF0, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 6, B
        InstrInfo{ .op = 0xF1, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 6, C
        InstrInfo{ .op = 0xF2, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 6, D
        InstrInfo{ .op = 0xF3, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 6, E
        InstrInfo{ .op = 0xF4, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 6, H
        InstrInfo{ .op = 0xF5, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 6, L
        InstrInfo{ .op = 0xF6, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // SET 6, (HL)
        InstrInfo{ .op = 0xF7, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 6, A
        InstrInfo{ .op = 0xF8, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 7, B
        InstrInfo{ .op = 0xF9, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 7, C
        InstrInfo{ .op = 0xFA, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 7, D
        InstrInfo{ .op = 0xFB, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 7, E
        InstrInfo{ .op = 0xFC, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 7, H
        InstrInfo{ .op = 0xFD, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 7, L
        InstrInfo{ .op = 0xFE, .ins = undefined, .args = .{ .none = {} }, .cycles = 16 }, // SET 7, (HL)
        InstrInfo{ .op = 0xFF, .ins = undefined, .args = .{ .none = {} }, .cycles = 8 }, // SET 7, A
    };

    fn NOP(gb: *GB, _: InstrArgs) u8 { // TODO test
        // print("NOP\n", .{});
        gb.cpu.pc += 1;
        return 0;
    }
    fn INCr8(gb: *GB, args: InstrArgs) u8 { // TODO TEST
        const value = gb.cpu.get_byte(args.target);
        // print("INCr8, target: {any}\n", .{args.target});
        gb.cpu.set_byte(args.target, @addWithOverflow(value, 1)[0]);
        gb.cpu.f.h = (value & 0xF + 1) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.z = gb.cpu.get_byte(args.target) == 0;
        gb.cpu.f.s = false;
        gb.cpu.f.write();
        gb.cpu.pc += 1;
        return 0;
    }
    fn INCr16(gb: *GB, args: InstrArgs) u8 { // TODO TEST
        const value = gb.cpu.get_word(args.target);
        const res = @addWithOverflow(value, 1)[0];
        // print("INCr16, target: {any}\n 0x{X} + 1 = 0x{X}", .{ args.target, value, res });
        gb.cpu.set_word(args.target, res);
        gb.cpu.pc += 1;
        return 0;
    }
    fn DECr8(gb: *GB, args: InstrArgs) u8 { // TODO TEST -- overflow? r16 too
        const value = gb.cpu.get_byte(args.target);
        // print("DECr8, target: {any} ({d} -= 1)\n", .{ args.target, gb.cpu.get_byte(args.target) });
        const res = @subWithOverflow(value, 1)[0];
        gb.cpu.set_byte(args.target, res);
        gb.cpu.f.z = res == 0;
        gb.cpu.f.s = true;
        gb.cpu.f.h = (value & 0xF) & 0x10 == 0x10; // half carry conditions
        gb.cpu.f.write();
        gb.cpu.pc += 1;
        return 0;
    }
    fn DECr16(gb: *GB, args: InstrArgs) u8 { // TODO TEST
        const value = gb.cpu.get_byte(args.target);
        // print("DECr16, target: {any}\n", .{args.target});
        gb.cpu.set_word(args.target, @subWithOverflow(value, 1)[0]);
        gb.cpu.pc += 1;
        return 0;
    }
    fn LD8(gb: *GB, args: InstrArgs) u8 { // LD r8, n8 TODO TEST
        const n: u8 = gb.read_byte(gb.cpu.pc + 1);
        // print("LD8, target {any}, n: Ox{X}\n", .{ @as(regID, args.target), n });
        // print("n b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.set_byte(args.target, n);
        gb.cpu.pc += 2;
        return 0;
    }
    fn LD16(gb: *GB, args: InstrArgs) u8 { // LD r16, n16 TODO TEST
        const n: u16 = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
        // print("LD16, target {any}, n: Ox{X}\n", .{ @as(regID, args.target), n });
        // print("n b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.read_byte(gb.cpu.pc + 2) });
        // print("n b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.set_word(args.target, n);
        gb.cpu.pc += 3;
        return 0;
    }
    fn LDr8(gb: *GB, args: InstrArgs) u8 { // LD r8, r8 TODO TEST
        // print("LDr8, targets: from {any} --> to {any}\n", .{ args.targets.from, args.targets.to });
        // print("Values, from {any} --> to {any}\n", .{ gb.cpu.get_byte(args.targets.to), gb.cpu.get_byte(args.targets.from) });
        gb.cpu.set_byte(args.targets.to, gb.cpu.get_byte(args.targets.from));
        gb.cpu.pc += 1;
        return 0;
    }
    fn LDAHL(gb: *GB, _: InstrArgs) u8 { // LD r8, r8 TODO TEST
        const mem_place = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(regID.a);
        // print("LDHL, mem@hl:0x{X} --> to A\n", .{mem_place});
        // print("Values, from {any} --> to {any}\n", .{ gb.cpu.get_byte(regID.a), gb.read_byte(mem_place) });
        gb.writeByte(mem_place, value);
        gb.cpu.pc += 1;
        return 0;
    }
    fn LDSP16(gb: *GB, _: InstrArgs) u8 { // LD r16, n16, 0x31
        // print("LD16SP\n", .{});
        const n: u16 = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
        // print("n b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.read_byte(gb.cpu.pc + 2) });
        // print("n b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.sp = n;
        // print("after op: sp: {d}\n", .{gb.cpu.sp});
        gb.cpu.pc += 3;
        return 0;
    }
    fn LDHL8(gb: *GB, _: InstrArgs) u8 { // LD[HL], n8
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.read_byte(gb.cpu.pc + 1);
        // print("LDHLDA\thl:0x{X}\tvalue:0x{x}\nmem@hl: 0x{x}\n", .{ hl, value, gb.read_byte(hl) });
        gb.writeByte(hl, value);
        // print("after op: mem@hl: 0x{X}\thl:{d}\n", .{ hl, gb.read_byte(hl), hl });
        gb.cpu.pc += 2;
        return 0;
    }
    fn LDHLR(gb: *GB, args: InstrArgs) u8 { // LD[HL],r8
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(args.target);
        // print("LDHLR, \nmem@0x{X}: 0x{X} --> 0x{X}\n", .{ hl, gb.read_byte(hl), value });
        gb.writeByte(hl, value);
        // print("after op: mem@0x{X}: 0x{X}\n", .{ hl, gb.read_byte(hl) });
        gb.cpu.pc += 1;
        return 0;
    }
    fn LDHLIA(gb: *GB, _: InstrArgs) u8 { // LD [HLI],A
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(regID.a);
        // print("LDHLIA, \nmem@0x{X}: 0x{X} --> 0x{X}\n", .{ hl, gb.read_byte(hl), value });
        gb.writeByte(hl, value);
        gb.cpu.set_word(regID.h, hl + 1);
        // print("after op: hl+1= 0x{X}\t mem@0x{X}: 0x{X}\n", .{ gb.cpu.get_word(regID.h), hl, gb.read_byte(hl) });
        gb.cpu.pc += 1;
        return 0;
    }
    fn LDHLDA(gb: *GB, _: InstrArgs) u8 { // LD [HLD],A
        const hl = gb.cpu.get_word(regID.h);
        const value = gb.cpu.get_byte(regID.a);
        // print("LDHLDA, \nmem@0x{X}: 0x{X} --> 0x{X}\n", .{ hl, gb.read_byte(hl), value });
        gb.writeByte(hl, value);
        gb.cpu.set_word(regID.h, hl - 1);
        // print("after op: mem@0x{X}: 0x{X}\n", .{ hl, gb.read_byte(hl) });
        gb.cpu.pc += 1;
        return 0;
    }
    fn LDHCA(gb: *GB, _: InstrArgs) u8 {
        const c = gb.cpu.get_byte(regID.c);
        const a = gb.cpu.get_byte(regID.a);
        const mem_place = 0xFF00 + @as(u16, c);
        // print("LDHCA, memplace@0x{X} --> 0x{X}\n", .{ mem_place, a });
        gb.writeByte(mem_place, a);
        gb.cpu.pc += 1;
        return 0;
    }
    fn LDHAC(gb: *GB, _: InstrArgs) u8 { // Load value in register A from the byte at address $FF00+c
        const a = gb.cpu.get_byte(regID.a);
        const c = gb.cpu.get_byte(regID.c);
        const byte = gb.read_byte(0xFF00 + @as(u16, c));
        // print("LDHAC byte: 0x{X} --> A\n", .{byte});
        gb.cpu.set_byte(a, byte);
        gb.cpu.pc += 1;
        return 0;
    }
    fn LDAn16(gb: *GB, _: InstrArgs) u8 { // TODO TEST Load value in register A from the byte at address n16.
        const memory_place = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
        const n = gb.read_byte(memory_place);
        // print("LDAn16, n: Ox{X} --> A\n", .{n});
        // print("memplace b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.read_byte(gb.cpu.pc + 2) });
        // print("memplace b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.set_byte(regID.a, n);
        gb.cpu.pc += 3;
        return 0;
    }
    fn LDHAn16(gb: *GB, _: InstrArgs) u8 { // TODO TEST same as above, provided the address is between $FF00 and $FFFF.
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
        return 0;
    }
    fn LDn16A(gb: *GB, _: InstrArgs) u8 { // TODO TEST Store value in register A into the byte at address n16.
        const memory_place = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
        const n = gb.cpu.get_byte(regID.a);
        gb.writeByte(memory_place, n);
        // print("LDn16A, n: Ox{X} --> memplace@{X}", .{ n, memory_place });
        // print("memplace b1: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 2, gb.read_byte(gb.cpu.pc + 2) });
        // print("memplace b2: [pc]0x{X} \t(0x{X})\n", .{ gb.cpu.pc + 1, gb.read_byte(gb.cpu.pc + 1) });
        gb.cpu.pc += 3;
        return 0;
    }
    fn LDHn16A(gb: *GB, _: InstrArgs) u8 { // TODO TEST same as above, provided the address is between $FF00 and $FFFF.
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
        return 0;
    }
    fn LDAr16(gb: *GB, args: InstrArgs) u8 { // TODO TEST Load value in register A from the byte pointed to by register r16.
        const memory_place = gb.cpu.get_word(args.target);
        const n = gb.read_byte(memory_place);
        // print("LDAr16, n: 0x{X} --> A\n", .{n});
        gb.cpu.set_byte(regID.a, n);
        gb.cpu.pc += 1;
        return 0;
    }
    fn LDr16A(gb: *GB, args: InstrArgs) u8 { // TODO TEST Store value in register A into the byte pointed to by register r16.
        const memory_place = gb.cpu.get_word(args.target);
        const n = gb.cpu.get_byte(regID.a);
        gb.writeByte(memory_place, n);
        // print("LDr16A, n: 0x{X} --> memplace@0x{X}\n", .{ n, memory_place });
        gb.cpu.pc += 1;
        return 0;
    }

    fn XORA(gb: *GB, args: InstrArgs) u8 {
        // print("XORA, target {any}\n", .{args.target});
        gb.cpu.registers[@intFromEnum(regID.a)] ^= gb.cpu.registers[@intFromEnum(args.target)];
        if (gb.cpu.registers[@intFromEnum(regID.a)] == 0) {
            gb.cpu.f.z = true;
            gb.cpu.f.write();
        }
        gb.cpu.pc += 1;
        return 0;
    }
    fn ADDAr8(gb: *GB, args: InstrArgs) u8 { // TODO finish, TEST, flags
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
        return 0;
    }
    fn SUBAr8(gb: *GB, args: InstrArgs) u8 { // TODO finish, TEST, flags
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
        return 0;
    }
    fn ADDAHL(gb: *GB, _: InstrArgs) u8 { // TODO Add the byte pointed to by HL to A.
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
        return 0;
    }
    fn ADDHLr16(gb: *GB, args: InstrArgs) u8 { // TODO finish, TEST, flags
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
        return 0;
    }
    fn EI(gb: *GB, _: InstrArgs) u8 { // TODO TEST
        // print("EI\n", .{});
        gb.cpu.pc += 1;
        return 0;
    }
    fn RRCA(gb: *GB, _: InstrArgs) u8 { // TODO TEST
        const a = gb.cpu.get_byte(regID.a);
        // print("RRCA\n", .{});
        gb.cpu.set_byte(regID.a, a << 7 | a >> 1);
        gb.cpu.f.c = (@as(u1, @truncate(a)) == 1);
        gb.cpu.f.write();
        gb.cpu.pc += 1;
        return 0;
    }
    fn PUSH(gb: *GB, args: InstrArgs) u8 {
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
        return 0;
    }
    fn POP(gb: *GB, args: InstrArgs) u8 {
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
        return 0;
    }
    fn JP(gb: *GB, args: InstrArgs) u8 { // TODO ONLY 3 CYCLES IF NOT TAKEN OTHERWISE 4
        // print("JP", .{});
        const jump = gb.cpu.f.check(args.flagConditions);
        if (jump) {
            const n = @as(u16, gb.read_byte(gb.cpu.pc + 2)) << 8 | gb.read_byte(gb.cpu.pc + 1);
            // print("to 0x{X}\n", .{n});
            gb.cpu.pc = n;
            return 1; // Extra cycle when taken
        } else {
            gb.cpu.pc += 3;
            return 0; // No extra cycles when not taken
        }
    }
    fn JR(gb: *GB, args: InstrArgs) u8 { // TODO ONLY 2 CYCLES IF NOT TAKEN OTHERWISE 3
        // print("JR, \tcondition:", .{});
        const dist: i8 = @bitCast(gb.read_byte(gb.cpu.pc + 1));
        const jump = gb.cpu.f.check(args.flagConditions);
        // print(" by dist: [pc]0x{X} \t0x{X} ({d}) bytes \n", .{ gb.cpu.pc + 1, dist, dist });

        // const byte = gb.read_byte(gb.cpu.pc);
        if (jump) {
            const new_mem: i17 = @as(i17, @intCast(gb.cpu.pc + 2)) + dist;
            gb.cpu.pc = @as(u8, @intCast(new_mem));
            // print("to pc:0x{X}\n", .{gb.cpu.pc});
            return 1; // Extra cycle when taken
        } else { // next instruction, condition failed
            // print("skipped jump, failed condition\n", .{});
            gb.cpu.pc += 2;
            return 0; // No extra cycles needed
        }
    }
    fn JPHL(gb: *GB, _: InstrArgs) u8 {
        // print("JPHL\n", .{});
        gb.cpu.pc = gb.cpu.get_word(regID.h);
        return 0;
    }
    fn CALLn16(gb: *GB, args: InstrArgs) u8 { //
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
            return 3;
        } else { // next instruction, condition failed
            // print("skipped jump, failed condition\n", .{});
            gb.cpu.pc += 3;
            return 0; // No extra cycles needed
        }
    }
    fn RET(gb: *GB, args: InstrArgs) u8 { // TODO TEST, UPDATE cycles 5 if condition met, 2 if not met, 4 if no condition
        // print("RET, condition:{any}", .{args.flagConditions});
        const ret = gb.cpu.f.check(args.flagConditions);
        // const byte = gb.read_byte(gb.cpu.pc);

        if (ret) {
            const low = gb.read_byte(gb.cpu.sp);
            gb.cpu.sp += 1;
            const high = gb.read_byte(gb.cpu.sp);
            gb.cpu.sp += 1;
            gb.cpu.pc = @as(u16, high) << 8 | low;
            return 3; // 3 cycles if condition met
        } else {
            gb.cpu.pc += 1;
            return 0; // No extra cycles when not taken
        }
    }
    fn CPAn8(gb: *GB, _: InstrArgs) u8 { // TODO TEST;
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
        return 0;
    }
    fn CPAr8(gb: *GB, args: InstrArgs) u8 { // TODO TEST;
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
        return 0;
    }
    fn CPAHL(gb: *GB, _: InstrArgs) u8 { // TODO TEST;
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
        return 0;
    }

    // PREFIX INSTRUCTIONS
    fn BITTEST(gb: *GB, args: InstrArgs) u8 { // TODO TEST
        const bit: u3 = args.bit_target.bit;
        const target = gb.cpu.get_byte(args.bit_target.target);
        // print("BITTEST, target: {any}, bit: {any}, reg_bin: 0b{b}", .{ args.bit_target.target, args.bit_target.bit, target });
        // print("                                             ^", .{});
        gb.cpu.f.z = @as(u1, @truncate(target >> bit)) == 0; // set zero flag if the target bit is not set
        gb.cpu.f.h = true; // set half carry
        gb.cpu.f.write();
        gb.cpu.pc += 2;
        return 0;
    }
    fn BITTESTHL(gb: *GB, args: InstrArgs) u8 { // TODO TEST
        const bit: u3 = args.bit_target.bit;
        // print("BITTESTHL, target: {any}, bit: {any}\n", .{ args.bit_target.target, args.bit_target.bit });
        const hl = gb.cpu.get_word(args.target);
        const byte = gb.read_byte(hl);
        gb.cpu.f.z = @as(u1, @truncate(byte >> bit)) == 0;
        gb.cpu.f.h = true;
        gb.cpu.f.write();
        gb.cpu.pc += 2;
        return 0;
    }
    fn RL(gb: *GB, args: InstrArgs) u8 { // TODO TEST Rotate bits in register r8 left through carry.
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
        return 0;
    }
    fn from_byte(byte: u8, prefixed: bool) InstrInfo {
        return switch (prefixed) {
            true => prefix_table[byte],
            false => table[byte],
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
        const exe_zone = tracy.beginZone(@src(), .{.name = "Execute Ins"});
        defer exe_zone.end();
        var byte = gb.read_byte(self.pc);
        var prefixed = false;
        if (byte == 0xCB) { // prefix byte
            prefixed = true;
            self.pc += 1;
            byte = gb.read_byte(self.pc);
        }
        // print("[pc]0x{X}\t(0x{X})\n", .{ self.pc, byte });
        const ins_args = InstructionSet.from_byte(byte, prefixed);
        const extra_cycles = (ins_args.ins)(gb, ins_args.args);
        // print("\n", .{});
        // if (self.last_ins == 0xFB) { // set IME flag after previous instruction
        //     // print("set IME\n", .{});
        // }
        return ins_args.cycles + extra_cycles;
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
    frames_cycled: u16 = 0,

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
        try self.lcd.init();
        @memset(&self.tile_set, empty_tile(self));
    }

    fn setSpecialRegister(self: *@This(), register: LCD.special_registers, value: u8) void {
        self.special_registers[@intFromEnum(register)] = value;
    }
    fn getSpecialRegister(self: *@This(), register: LCD.special_registers) u8 {
        return self.special_registers[@intFromEnum(register)];
    }
    fn switchMode(self: *@This()) void {
        switch (self.mode) {
            .SCAN => {
                self.mode = .RENDER;
                self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.RENDER)];
            },
            .RENDER => {
                // print("render!! \n\n\n\n\n", .{});
                self.lcd.pushScanline(self.scanline, self.getSpecialRegister(.ly));
                self.mode = .HBLANK;
                self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.HBLANK)];
            },
            .HBLANK => {
                // Increment LY register
                const ly = self.getSpecialRegister(.ly);
                self.setSpecialRegister(.ly, ly + 1);

                if (self.getSpecialRegister(.ly) == 144) {
                    self.lcd.renderScreen(self.lcd.screen.len); // render at the last scanline
                    self.mode = .VBLANK;
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.VBLANK)]; // per scanline
                } else {
                    self.mode = .SCAN;
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.SCAN)];
                }
                // TODO INTERRUPT
            },
            .VBLANK => {
                const new_ly = self.getSpecialRegister(.ly) + 1;
                // self.setSpecialRegister(.ly, ly + 1);
                if (new_ly > 153) { // 153 is the end of VBLANK
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
        // update STAT register
        var stat_reg = self.getSpecialRegister(.stat);
        stat_reg = (stat_reg & 0b1111_1100) | @intFromEnum(self.mode);
        self.setSpecialRegister(.stat, stat_reg);
    }

    fn do(self: *@This()) void {
        // Operate GPU here
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

        var cycles_left = cycles; // amt of cycles spent by cpu
        while (cycles_left > 0) {
            const cycles_to_process: u16 = @min(cycles_left, self.mode_cycles_left);
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
        if (self.mode == .RENDER) {
            return;
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
        // try self.startAndCreateRenderer(); // set window and renderer
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
    ticks: usize = ticks_per_frame * 60,
    ns_elapsed: usize = std.time.ns_per_s,
    fn start(self: *Clock) !void {
        self.timer = try std.time.Timer.start();
    }
    fn tick(self: *Clock) bool {
        const zone = tracy.beginZone(@src(),
            .{.name = "Tick"});
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

    pub fn init(self: *@This()) !void {
        @memset(&self.memory, 0);
        @memcpy(self.memory[0x104 .. 0x133 + 1], &LOGO);
        try self.cpu.init();
        try self.gpu.init(self);
        _ = InstructionSet.NOP(self, .{ .none = {} }); // dummy op to init cache
        self.cpu.pc -= 1;
        self.running = true;
    }

    pub fn read_byte(self: *@This(), address: usize) u8 {
        // TODO: implement memory mapping based on address
        return self.memory[address];
    }

    pub fn writeByte(self: *@This(), address: usize, value: u8) void {
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
            0xFF50 => { // Disable bootrom register
                // Unmap and replace the bootrom with cartridge data
            },
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
            try self.getEvents();
            try self.do();
            _ = self.clock.tick();
            //TODO: Update peripherals & timing
        }
    }
    fn do(self: *@This()) !void {
        const cycles_spent = try self.cpu.execute(self);
        self.cycles_spent += cycles_spent;
        self.gpu.tick(cycles_spent);
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
    const zone = tracy.beginZone(@src(),.{.name = "Main"});
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
