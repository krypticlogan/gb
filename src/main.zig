//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.
const std = @import("std");
const print = std.debug.print;

const regID = enum(u3) {
    a,
    b,
    c,
    d,
    e,
    f,
    h,
    l,
};
const FlagRegister = struct {
    zero: bool = false,
    subtract: bool = false,
    half_carry: bool = false,
    carry: bool = false,

    ZERO_FLAG_BYTE_POSITION: u8 = 7,
    SUBTRACT_FLAG_BYTE_POSITION: u8 = 6,
    HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5,
    CARRY_FLAG_BYTE_POSITION: u8 = 4,

    fn check(self: *@This(), byte: u8) @This() {
        self.zero = (byte >> self.ZERO_FLAG_BYTE_POSITION) & 0b1;
        self.subtract = (byte >> self.SUBTRACT_FLAG_BYTE_POSITION) & 0b1;
        self.half_carry = (byte >> self.HALF_CARRY_FLAG_BYTE_POSITION) & 0b1;
        self.carry = (byte >> self.CARRY_FLAG_BYTE_POSITION) & 0b1;
    }

    fn tou8(self: *@This()) u8 {
        return (if (self.zero) 1 else 0) << self.ZERO_FLAG_BYTE_POSITION |
            (if (self.subtract) 1 else 0) << self.SUBTRACT_FLAG_BYTE_POSITION |
            (if (self.half_carry) 1 else 0) << self.HALF_CARRY_FLAG_BYTE_POSITION |
            (if (self.carry) 1 else 0 << self.CARRY_FLAG_BYTE_POSITION);
    }
};

const Instruction = enum { ADD };
/// Defines a GameBoy CPU: i8080 & Z80 hybrid chip
const CPU = struct {
    registers: [8]u8 = undefined,
    flagRegister: FlagRegister = FlagRegister{},
    pc: u16 = undefined,
    sp: u16 = undefined,

    fn init(self: *@This()) !void {
        @memset(&self.registers, 0);
        self.pc = 0; //TODO: program start value
        self.sp = 0;
    }

    fn two_bit_set(self: *@This(), reg1: regID, value: u16) void {
        self.registers[reg1] = @truncate((value & 0xFF00) >> 8);
        self.registers[reg1 + 1] = @truncate(value & 0x00FF);
    }

    fn two_bit_get(self: *@This(), reg1: regID) u16 {
        return @as(u16, (self.registers[reg1] << 8)) | self.registers[reg1 + 1];
    }
};

const GB = struct {
    cpu: CPU = CPU{},

    fn init(self: *@This()) !void {
        try self.cpu.init();
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
}
