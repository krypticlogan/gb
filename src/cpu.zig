/// Defines a GameBoy CPU: i8080 & Z80 hybrid chip
const mode = enum { DMG, CGB };
bus: *Bus = undefined, // memory
registers: [7]u8 = undefined, // registers vv
f: FlagRegister = FlagRegister{},
pc: u16 = undefined, // program counter
sp: u16 = undefined, // stack pointer
halted: bool = false, // stops all execution when true
halt_bug_state: u2 = 0,
// state
step: bool = false,
paused: bool = false,
executing_byte: u8 = 0x0,
// prefixed: bool = false,
log: Log = Log{},
booted: bool = false,
// TODO instruction cache?
pub fn init(self: *@This(), gb: *GB) !void {
    @memset(&self.registers, 0);
    self.pc = 0;
    self.sp = 0;
    self.bus = &gb.bus;
    // if (std.fs.cwd().deleteFile(sub_path: []const u8))
    // Log.out_file = std.fs.cwd().createFile(Log.out_file_path, .{.truncate = true}) catch {
    //     @panic("Unable to create log file, aborting");
    // };
}
// cpu execution
pub fn execute(self: *@This()) struct{u8, bool} {
    const set_ime = self.executing_byte == 0xFB; // set the ime flag after this instruction
    switch (self.halt_bug_state) {
        0 => {}, // normal operation
        1 => { // this is the instruction to be repeated
            self.halt_bug_state += 1;
        },
        2 => { // this is our repeat of the instruction
            self.jump_to_prev_instr();
            self.halt_bug_state = 0;
        },
        else => unreachable
    }
    self.executing_byte = self.bus.readByte(self.pc);
    // const byte = self.bus.readByte(self.executing_pc);
    // const cycles_spent = InstructionSet.exe_from_byte(self, prefixed);
    const instr = switch (self.executing_byte == 0xCB) { // prefix byte
        false => InstructionSet.instrs[self.executing_byte],
        true => blk: {
            self.pc += 1;
            self.executing_byte = self.bus.readByte(self.pc);
            break :blk InstructionSet.prefix_instrs[self.executing_byte];
        }
    };

    const cycles_spent = instr.call(self);
    if (set_ime) self.bus.handler.enable();
    if (self.step) {
        self.break_exe();
        return .{cycles_spent, true};
    }
    return .{cycles_spent, self.paused};
}

pub inline fn pushToExecutionChain(self: *@This(), comptime debug: []const u8, args: anytype) void {
    if (InstructionSet.DEBUG) {
        const fmt = InstructionSet.fmtInsDebug(debug, args);
        self.log.write(.{ fmt, self.pc, self.executing_byte });
    }
}
// memory ops
pub fn set_byte(self: *@This(), reg1: regID, value: u8) void {
    // if (reg1 == .a and (value == 0x1b or value == 0x1a)) {
    //  print("A register modified at pc[0x{X}] by [{X}]; new a = 0x{X}\n", .{self.pc, self.executing_byte, value});
    // }
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
pub inline fn push_stack(cpu: *CPU, val: u16) void {
    cpu.sp = @subWithOverflow(cpu.sp, 1)[0];
    cpu.bus.writeByte(cpu.sp, @truncate(val >> 8));
    cpu.sp = @subWithOverflow(cpu.sp, 1)[0];
    cpu.bus.writeByte(cpu.sp, @truncate(val));
}
pub inline fn pop_stack(cpu: *CPU) struct { u8, u8 } {
    const low = cpu.bus.readByte(cpu.sp);
    cpu.sp = @addWithOverflow(cpu.sp, 1)[0];
    const high = cpu.bus.readByte(cpu.sp);
    cpu.sp = @addWithOverflow(cpu.sp, 1)[0];
    return .{ low, high };
}
/// Sets the pc to the location of the previously executed instruction
pub inline fn jump_to_prev_instr(self: *CPU) void {
    self.pc -= InstructionSet.instrs[self.executing_byte].bytes;
    self.pc = switch (self.pc > 0 and self.bus.readByte(self.pc - 1) == 0xCB) {
        false => self.pc,
        true => self.pc - 1
    };
}
// debug
pub inline fn break_exe(self: *CPU) void {
    if (!self.booted) return;
    print("break_exe()\n", .{});
    self.paused = true;
}
pub inline fn resume_exe(self: *CPU) void {
    // print("resume execution\n", .{});
    self.paused = false;
}
/// Dumps all register values, previous instruction and program counter to the command line
pub fn state_dump(self: *@This()) void {
    print("\n[CPU STATE]\n----------------------\n", .{});
    // program counter & stack pointer
    print("PC: 0x{X}\tSP: 0x{X}\n", .{self.pc, self.sp});
    // registers
    print("REGISTERS\n", .{});
    for (std.enums.values(regID), self.registers) |label, register| {
        print("{any}[ 0x{X} / ({d}) ]\n", .{ label, register, register });
    }
    print("\n", .{});
    // flags and state
    print("Flags: C: {any}, S: {any}, H: {any}, Z: {any}\n", .{self.f.cFlag(), self.f.sFlag(), self.f.hFlag(), self.f.zFlag()});
    print("Current Instruction: 0x{X}\n", .{self.executing_byte});
    print("Previous Instructions:\n", .{});
    self.log.dump();
    print("Upcoming Instructions: TODO\n", .{});
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
        var i = self.ring_idx;
        const ring_start = switch (self.ring_idx) {
            0 => 19,
            else => self.ring_idx - 1
        };
        // print("log! ring idx {d}\n", .{self.ring_idx});
        while (i != ring_start) : (i = (i+1) % MAX_LINES) {
            // print("{d} ", .{i});
            if (self.log[i]) |debug| {
                writer.print("{s}", .{debug}) catch unreachable;
            }
        }
        // print("{d}\n", .{ring_start});
        writer.print("   {s}", .{self.log[ring_start].?}) catch unreachable;
        // while (i < MAX_LINES) : (i += 1) {
        //     const idx = (self.ring_idx + i) % MAX_LINES;
        //     if (self.log[idx]) |debug| {
        //         writer.print("{s}", .{debug}) catch unreachable;
        //     }
        // }
        return stream.getWritten();
    }
    /// Dumps the log state from oldest to most recent instruction
    pub fn dump(self: *Log) void {
        var i = self.ring_idx + 1;
        while (i != self.ring_idx) {
            if (i + 1 >= MAX_LINES) {
                i = 0;
            } else {
                i += 1;
            }
            if (self.log[i]) |debug| {
                print("{s}", .{debug});
            }
        }
    }
    const out_file_path = "gameboy-doctor/gb.log";
    pub var out_file: std.fs.File = undefined;

    pub inline fn write_to_file(state: struct {u8, u8, u8, u8, u8, u8, u8, u8, u16, u16, u8, u8, u8, u8}) void {// a   f   b   c   d   e   h   l   sp   pc   pc_mem -->
        // out_file.seekFromEnd(0) catch {
        //     @panic("Issue with seekFromEnd");
        // };
        const log_format = "A:{X:0>2} F:{X:0>2} B:{X:0>2} C:{X:0>2} D:{X:0>2} E:{X:0>2} H:{X:0>2} L:{X:0>2} SP:{X:0>4} PC:{X:0>4} PCMEM:{X:0>2},{X:0>2},{X:0>2},{X:0>2}\n";
        var log_buffer: [1024]u8 = undefined;
        // const state: []const u8 =

        _ = out_file.write(
            std.fmt.bufPrint(&log_buffer, log_format, state) catch {
                @panic("Unable to format log");
        }) catch {
            @panic("Unable to write log");
        };
    }
};
pub const WRAM_START = 0xC000;
pub const WRAM_END = 0xDFFF;


const std = @import("std");
const GB = @import("gb.zig");
const Bus = GB.Bus;
const CPU = @This();
const InstructionSet = GB.InstructionSet;
const print = std.debug.print;