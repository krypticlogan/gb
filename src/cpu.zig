/// Defines a GameBoy CPU: i8080 & Z80 hybrid chip
const mode = enum { DMG, CGB };
bus: *Bus = undefined,
registers: [7]u8 = undefined,
f: FlagRegister = FlagRegister{},
pc: u16 = undefined,
sp: u16 = undefined,
ime: bool = false,
halted: bool = false,
handler: InterruptHandler = InterruptHandler{},
executing_byte: u8 = 0x0,
log: Log = Log{},
// TODO instruction cache?
pub fn init(self: *@This(), gb: *GB) !void { // TODO MOVE THE BUS INTO HERE
    @memset(&self.registers, 0);
    self.pc = 0;
    self.sp = 0;
    self.bus = &gb.bus;
    self.handler.init(self.bus.memory[0xFFFF..0xFFFF+1], self.bus.memory[0xFF0F..0xFF0F+1]);
}
// cpu execution
pub fn execute(self: *@This()) !u8 {
    const set_ime = self.executing_byte == 0xFB; // set the ime flag after this instruction
    self.executing_byte = self.bus.readByte(self.pc);

    var prefixed = false;
    if (self.executing_byte == 0xCB) { // prefix byte
        prefixed = true;
        self.pc += 1;
        self.executing_byte = self.bus.readByte(self.pc);
    }
    const cycles_spent = InstructionSet.exe_from_byte(self, prefixed);
    if (set_ime) { // set ime flag if needed
        self.ime = true;
    }

    if (cycles_spent == 255) {
        print("crashed @[pc]0x{X}\tbyte:(0x{X}), prefixed? {any}\n", .{ self.pc, self.executing_byte, prefixed });
        return error.UNDEF_INSTRUCTION;
    } else if (self.executing_byte == 0xFF) {
        print("tried to execute RST 38 @ 0x{X}... shouldn't happen,\ntraceback: \n", .{self.pc});
        self.log.dump();
        // return error.NO_RST;
    }
    return cycles_spent;
}

pub inline fn pushToExecutionChain(self: *@This(), debug: []const u8, args: anytype) void {
    if (InstructionSet.DEBUG) {
        const fmt = InstructionSet.fmtInsDebug(debug, args);
        self.log.write(.{ fmt, self.pc, self.executing_byte });
    }
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
pub fn read_imm_16(self: *@This()) u16 {
    return @as(u16, self.bus.memory[self.pc + 2]) << 8 | self.bus[self.pc + 1];
}
pub fn read_imm_8(self: *@This()) u16 {
    return self.bus.memory[self.pc + 1];
}
/// Dumps all register values, previous instruction and program counter to the command line
pub fn state_dump(self: *@This()) void {
    const register_labels = [_]u8{ "a", "b", "c", "d", "e", "h", "l" };
    for (register_labels, self.registers) |label, register| {
        print("REGISTER {s}: {s}", .{ label, register });
    }

    // print("Register values: a, b, c, d, e, h, l", .{});

}
// types & context
pub const regID = enum(u3) {
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
    pub inline fn cFlag(self: *FlagRegister) bool {
        return self.value & (1 << 4) != 0;
    }
    pub inline fn zFlag(self: *FlagRegister) bool {
        return self.value & (1 << 7) != 0;
    }
    pub inline fn hFlag(self: *FlagRegister) bool {
        return self.value & (1 << 5) != 0;
    }
    pub inline fn sFlag(self: *FlagRegister) bool {
        return self.value & (1 << 6) != 0;
    }
    pub inline fn write(self: *FlagRegister, z: bool, c: bool, h: bool, s: bool) void {
        self.value = (@as(u8, @intFromBool(z)) << 7) |
            (@as(u8, @intFromBool(s)) << 6) |
            (@as(u8, @intFromBool(h)) << 5) |
            (@as(u8, @intFromBool(c)) << 4);
    }
    pub inline fn check(self: *FlagRegister, cond: InstructionSet.Condition) bool {
        return switch (cond) {
            .z => self.zFlag(), // Z
            .nz => !self.zFlag(),
            .c => self.cFlag(), // C
            .nc => !self.cFlag(),
            .none => true,
        };
    }
};
const InterruptHandler = struct {
    iE: []u8 = undefined,
    iF: []u8 = undefined,
    fn init(self: *InterruptHandler, ie_ptr: []u8, if_ptr: []u8) void {
        self.iE = ie_ptr;
        self.iF = if_ptr;
    }
};
pub const Log = struct {
    const MAX_LINES = 20;
    pub const MAX_CHAR = 128;
    var buffer: [MAX_LINES * MAX_CHAR]u8 = undefined;
    log: [MAX_LINES]?[]const u8 = [_]?[]const u8{null} ** MAX_LINES,
    ring_idx: u8 = 0,

    inline fn write(self: *Log, debug: struct { []const u8, u16, u8 }) void {
        // debug: string, pc, byte
        const offset = MAX_CHAR * @as(usize, self.ring_idx);
        const buf = buffer[offset .. offset + MAX_CHAR];
        const fmt_debug = std.fmt.bufPrint(buf[0 .. MAX_CHAR - 1], "pc[X.{1X:04}]: X.{2X} -> {0s}\n", debug) catch unreachable;
        buf[fmt_debug.len] = 0;
        self.log[self.ring_idx] = buf[0..fmt_debug.len];
        self.ring_idx = (self.ring_idx + 1) % MAX_LINES;
    }

    pub inline fn writeAll(self: *Log) []const u8 {
        var text_buffer: [MAX_LINES * MAX_CHAR]u8 = undefined;
        var stream = std.io.fixedBufferStream(&text_buffer);
        const writer = stream.writer();
        var i: u8 = 0;
        while (i < MAX_LINES) : (i += 1) {
            const idx = (self.ring_idx + i) % MAX_LINES;
            if (self.log[idx]) |debug| {
                writer.print("{s}", .{debug}) catch unreachable;
            }
        }
        return stream.getWritten();
    }
    fn dump(self: *Log) void {
        for (self.log) |info| {
            if (info) |str| {
                print("{s}", .{str});
            }
        }
    }
};
pub const WRAM_START = 0xC000;
pub const WRAM_END = 0xDFFF;


const std = @import("std");
const GB = @import("gb.zig");
const Bus = GB.Bus;
const InstructionSet = GB.InstructionSet;
const print = std.debug.print;