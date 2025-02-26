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
    const table = [_]struct { u8, *const fn (*CPU) void }{
        .{ 0x00, InstructionSet.NOP },
        .{ 0x80, InstructionSet.ADDB },
    };

    const prefix_table = [_]struct { u8, *const fn (*CPU) void }{
        .{ 0x00, InstructionSet.NOP },
        .{ 0x80, InstructionSet.ADDB },
    };

    fn from_byte(byte: u8) !*const fn (*CPU) void {
        inline for (table) |mapping| {
            const code = mapping[0];
            const ins = mapping[1];
            if (code == byte) {
                return ins;
            }
        }
        // if (byte > 0 and byte < table.len) return table[byte][1]  else
        print("\tbyte: 0x{x}\t", .{byte});
        return error.IllegalByte;
    }

    fn from_prefixed_byte(byte: u8) !*const fn (*CPU) void {
        inline for (prefix_table) |mapping| {
            const code = mapping[0];
            const ins = mapping[1];
            if (code == byte) {
                return ins;
            }
        }
        // if (byte > 0 and byte < table.len) return table[byte][1]  else
        print("\tbyte: 0x{x}\t", .{byte});
        return error.IllegalByte;
    }

    fn NOP(cpu: *CPU) void {
        print("NOP", .{});
        cpu.pc += 1;
    }
    fn ADDB(cpu: *CPU) void {
        print("ADDB", .{});
        cpu.pc += 1;
        // return 8;
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

    fn load(self: *@This()) !void {
        _ = self;
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
            ins = try InstructionSet.from_prefixed_byte(byte);
        } else {
            ins = try InstructionSet.from_byte(byte);
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

    const Color = enum {
        black,
        dgray,
        lgray,
        white
    };

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
        self.vram = gb.memory[VRAM_BEGIN..VRAM_END+1];
        @memset(&self.tile_set, empty_tile(self));
    } 

    fn read_vram (self: *@This(), address: usize) u8 {
        return self.vram[address];
    }

     fn write_vram (self: *@This(), address: usize, value: u8) void {
        self.vram[address] = value;
        if (address >= 0x1800) return;

        // const normalized_address
    }

    fn vram_dump(self: *@This()) void {
        print("VRAM dump: \n", .{});
        for (self.vram, 0..VRAM_SIZE) |value, i| {
            print("0x{x}", .{value});
            if (i!=0 and i % 80 == 0) print("\n", .{});
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
    }

    fn cycle(self: *@This()) !void {
        try self.cpu.execute(self);
        //TODO: Update peripherals & timing
    }

    fn mem_dump(self: *@This()) void {
        print("printing bytes:\n", .{});
        for (self.memory, 0..self.memory.len) |value, i| {
            print("0x{x}", .{value});
            if (i!=0 and i % 64 == 0) print("\n", .{}) else continue;
        }
        print("\n", .{});
    }

    fn gfx_dump(self: *@This()) void {
        print("Actual memspace dump: \n", .{});
        for (self.memory[0x8000..0x97FF+1], 0..(0x97FF - 0x8000+1)) |value, i| {
            print("0x{x}", .{value});
            if (i!=0 and i % 80 == 0) print("\n", .{});
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
    gb.memory[0] = 0x00;
    gb.memory[1] = 0x80;

    // gb.gpu.vram_dump();
    // gb.gfx_dump();


    print("\n", .{});
    try gb.cycle();
    try gb.cycle();

    // const tile = gb.gpu.empty_tile();
    print("{any}", .{gb.gpu.tile_set});
}
