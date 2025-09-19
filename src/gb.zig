/// Gameboy Machine, defer endGB
const GB = *@This();
cpu: CPU = CPU{},
gpu: GPU = GPU{},
apu: APU = APU{},
timer: Timer = Timer{},
bus: Bus = Bus{},
rom_file_path: []const u8 = undefined,
running: bool = undefined,
booted: bool = false,
crashed: bool = false,
cycles_spent: usize = 0,
clock: Clock = Clock{},
last_frame: std.time.Instant = undefined,
allocator: std.mem.Allocator,
root_path: []const u8,

// containers
const interrupts = enum {};
/// nintendo logo
const LOGO: [48]u8 = .{ 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E };
// startup
pub var prng: std.Random.Xoshiro256 = undefined;
pub fn init(self: *@This()) !void {
    self.bus.init(self);
    try initRandom(); // init random before gpu init
    try self.gpu.init(self);
    self.timer.init(self);
    try self.cpu.init(self);
    _ = InstructionSet.exe_from_byte(&self.cpu, false); // dummy op to init cache
    self.cpu.pc -= 1;
    self.running = true;
}
pub fn load_game(self: *@This()) !void {
    var args = try std.process.argsWithAllocator(self.allocator);
    defer args.deinit();
    _ = args.next();
    const rom_file = try std.mem.concat(self.allocator, u8, &[_][]const u8{args.next() orelse "cpu_instrs", ".gb" });
    const rom_file_path = try std.fs.path.join(self.allocator, &.{self.root_path, "roms", rom_file});
    defer {
        self.allocator.free(rom_file);
        self.allocator.free(rom_file_path);
    }
    const rom = try std.fs.openFileAbsolute(rom_file_path, .{});
    defer rom.close();
    const stats = try rom.stat();
    const buf: []u8 = try rom.readToEndAlloc(self.allocator, stats.size);
    defer self.allocator.free(buf);
    print("Reading {d} bytes...\n", .{buf.len});
    if (self.booted) {
        for (0x0..0x100) |i| { // replace the bootrom after completed
            self.bus.memory[i] = buf[i];
        }
    } else {
        for (0x100..buf.len) |i| {
            self.bus.memory[i] = buf[i];
        }
    }
}
/// Loads gameboy bootrom and the rom to be used
pub fn boot(self: *@This()) !void {
    const bootFilePath = try std.fs.path.join(self.allocator, &.{self.root_path, "roms", "dmg_boot.bin"});
    defer self.allocator.free(bootFilePath);
    const bootFile = try std.fs.openFileAbsolute(bootFilePath, .{});
    defer bootFile.close();
    const bootFileStats = try bootFile.stat();
    const bootFileBuf: []u8 = try bootFile.readToEndAlloc(self.allocator, bootFileStats.size);
    defer self.allocator.free(bootFileBuf);
    for (0..bootFileBuf.len) |i| {
        self.bus.memory[i] = bootFileBuf[i];
    }
    try self.load_game();
}
// gb execution
pub fn go(self: *@This()) !void {
    print("GO!\n", .{});
    try self.clock.Start();
    while (self.running) {
        self.clock.last_frame_time = Clock.Now();
        if (!self.cpu.halted) {
            while (self.gpu.frame_cycles_spent < Clock.cycles_per_frame and !self.crashed) {
                self.do() catch {
                    self.crashed = true;
                };
            }
            if (self.crashed) { // debug
                self.gpu.randomStatic();
            }
        }
        try self.getEvents(); // poll events once per frame
        self.clock.tick();
        self.clock.update(); // calculates average fps
        self.gpu.lcd.renderAll(self.cpu.log.writeAll()); // render at the last scanline
        self.gpu.frame_cycles_spent = 0;
    }
}
fn do(self: *@This()) !void {
    const cycles_spent = try self.cpu.execute();
    self.cycles_spent += cycles_spent;
    self.gpu.tick(cycles_spent * 4);
    if (self.cpu.pc > 0xFF and !self.booted) {
        self.booted = true;
        try self.load_game();
    }
}
// helper
fn initRandom() !void {
    var seed: u64 = undefined;
    try std.posix.getrandom(std.mem.asBytes(&seed));
    prng = std.Random.DefaultPrng.init(seed);
}
// universal callers

pub fn getEvents(self: *@This()) !void {
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
            },
            else => {},
        }
    }
}
// memspace dumps
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
    for (self.memory[LCD.special_registers.start .. LCD.special_registers.end + 1], LCD.special_registers.start..LCD.special_registers.end + 1) |value, i| {
        print("register@0x{x}: 0x{x}\n", .{ i, value });
    }
    const i = 0xFF44;
    print("register@0x{x}: 0x{x}\n", .{ i, self.gpu.getSpecialRegister(.ly) });
    // println("register@0x{x}: 0x{x} ", .{i, value});
    print("\n", .{});
}
pub fn endGB(self: *@This()) void {
    self.gpu.lcd.endSDL();
}

pub const Bus = struct {
    const SIZE = 0xFFFF + 1;
    memory: [SIZE]u8 = undefined,
    // peripherals
    cpu: *CPU = undefined,
    gpu: *GPU = undefined,
    apu: *APU = undefined,
    timer: *Timer = undefined,

    pub fn init(self: *@This(), gb: GB) void {
        @memset(&self.memory, 0);
        self.cpu = &gb.cpu;
        self.gpu = &gb.gpu;
        self.apu = &gb.apu;
        self.timer = &gb.timer;
    }
    pub fn readByte(self: *@This(), address: u16) u8 {
        @setRuntimeSafety(false);
        // if (address >= 0xFF00) return self.memory[address];
        if (address >= Timer.START and address <= Timer.END) {
            return self.timer.read(address);
        }
        if (address >= LCD.special_registers.start and address <= LCD.special_registers.end) {
            return self.gpu.getSpecialRegister(@as(LCD.special_registers, @enumFromInt(address - LCD.special_registers.start)));
        }
        if (address >= CPU.WRAM_START and address <= CPU.WRAM_END) {
            return self.memory[address];
        }
        if (address >= GPU.VRAM_BEGIN and address <= GPU.VRAM_END) {
            return self.gpu.readVram(address);
        }
        if (address >= GPU.OAM_BEGIN and address <= GPU.OAM_END) {
            return self.gpu.oam[address - GPU.OAM_BEGIN];
        }
        return self.memory[address];
    }
    pub fn writeByte(self: *@This(), address: u16, value: u8) void {
        @setRuntimeSafety(false);
        if (address < 0x8000) return; // no writes to ROM
        if (address >= Timer.START and address <= Timer.END) {
            // print("address : 0x{X}", .{address});
            self.timer.write(address, value);
        } else if (address >= LCD.special_registers.start and address <= LCD.special_registers.end) {
            const register = @as(LCD.special_registers, @enumFromInt(address - LCD.special_registers.start));
            self.gpu.setSpecialRegister(register, value);
            if (register == LCD.special_registers.dma) {
                const prefix = address / 0x100;
                const ram_address: u16 = @as(u16, @intCast(prefix)) << 8;
                @memcpy(self.memory[GPU.OAM_BEGIN..GPU.OAM_END], self.memory[ram_address .. ram_address + GPU.OAM_SIZE]);
            }
        } else if (address >= GPU.VRAM_BEGIN and address <= GPU.VRAM_END) { // banks 0 & 1
            self.gpu.writeVram(address, value);
        } else self.memory[address] = value;
    }
};

pub const Clock = struct {
    start: i128 = undefined,
    ns_elapsed: u64 = 0,
    last_frame_time: i128 = undefined, // TODO use 0 tracked time
    last_fps: f64 = 0,
    current_fps: f64 = 0,
    slept: u64 = 0,
    debt: i128 = 0,
    fn Start(self: *Clock) !void {
        self.start = Now();
    }
    pub const Now = std.time.nanoTimestamp;

    fn targetCycles(self: *Clock) u64 {
        const total_elapsed_ns: f64 = @floatFromInt(Now() - self.start);
        return @intFromFloat(total_elapsed_ns * ticks_per_ns);
    }
    fn tick(self: *Clock) void {
        const now = Now();
        const ns_passed = now - self.last_frame_time;
        // print("frame in: {d}", .{@divFloor(ns_passed , std.time.ns_per_ms)});
        if (ns_passed < ns_per_frame) {
            const wait_time_ns: u64 = @intCast(ns_per_frame - ns_passed);
            if (wait_time_ns > 100_000) {
                const start = Now();
                const sleeptime = std.math.cast(u64, wait_time_ns + self.debt - 100_000);
                std.Thread.sleep(sleeptime orelse 0); // sleep when necessary
                const waited = Now() - start;
                self.debt += wait_time_ns - waited;
                // print("leftover {d}\n", .{self.debt});
            }
        }
    }
    fn update(self: *Clock) void {
        const now = Now();
        const frame_time = now - self.last_frame_time;
        self.ns_elapsed += @intCast(frame_time);
        const fps_estimate = std.time.ns_per_s / @as(f64, @floatFromInt(frame_time));
        const smoothing = 0.7;
        self.current_fps = smoothing * self.last_fps + (1.0 - smoothing) * fps_estimate;
        if (self.ns_elapsed >= std.time.ns_per_s) {
            print("{d:.2} fps\n", .{self.current_fps});
            self.ns_elapsed = 0;
        }
        self.last_fps = self.current_fps;
    }
    const ticks_per_s = 4.194304 * @as(f64, std.math.pow(u64, 10, 6));
    const ticks_per_ns = ticks_per_s / std.time.ns_per_s;
    const ns_per_tick = 1 / ticks_per_ns;
    const ns_per_frame: u64 = @intFromFloat(@round(1_000_000_000.0 / 59.744));
    const cycles_per_frame = 70224;
};
const Timer = struct {
    registers: *[4]u8 = undefined,
    counter: u16 = 0,
    prev_enabled: bool = false,
    cycles_since_overflow: ?u8 = null,
    const START = 0xFF04;
    const END = 0xFF07;
    const timer_reg = enum {
        div, // $FF04 - Divider Register (DIV)
        tima, // $FF05 - Timer Counter (TIMA)
        tma, // $FF06 - Timer Modulo (TMA)
        tac, // $FF07 - Timer Control (TAC)
        // |bit 2| timer enable |bit 1-0| clock select
        // 0b00 : CPU Clock / 1024
        // 0b01 : CPU Clock / 16
        // 0b10 : CPU Clock / 64
        // 0b11 : CPU Clock / 256
    };
    fn init(self: *Timer, gb: GB) void {
        self.registers = gb.bus.memory[START .. END + 1];
    }
    fn tick(self: *Timer, cycles: u8) void {
        const timer_enable: u1 = @truncate(self.registers[@intFromEnum(.tac)] >> 2 & 1);
        const bit_pos = switch (@as(u2, @truncate(self.registers[@intFromEnum(.tac)]))) {
            0b00 => 9,
            0b01 => 3,
            0b10 => 5,
            0b11 => 7,
        };
        var bit: u1 = undefined;
        const cycles_ticked = 0;
        while (cycles_ticked < cycles) : (cycles_ticked += 1) {
            self.counter += 1;
            if (self.cycles_since_overflow) |*cycles_since| {
                cycles_since += 1;
                if (cycles_since == 4) {
                    // TODO timer interrupt
                    self.registers[@intFromEnum(.tima)];
                }
            }
            bit = @truncate(self.registers[@intFromEnum(.div)] >> bit_pos);
            const edge = (bit & timer_enable) == 1;
            if (!edge and self.prev_enabled) {
                const res = @addWithOverflow(self.registers[@intFromEnum(.tima)], 1);
                if (res[0] != 0) {
                    self.cycles_since_overflow = 0;
                    self.registers[@intFromEnum(.tima)] = 0;
                }
            }
        }
    }
    fn read(self: *Timer, address: u16) u8 {
        const fixed_address = address - 0xFF04;
        return if (@as(timer_reg, @enumFromInt(fixed_address)) == .div)
            @truncate(self.counter >> 8)
        else
            self.registers[fixed_address];
    }
    fn write(self: *Timer, address: u16, value: u8) void {
        const fixed_address: u3 = @intCast(address - 0xFF04);
        // print("timer reg len: {d}, index: {d}", .{self.registers.len, fixed_address});
        if (@as(timer_reg, @enumFromInt(fixed_address)) == .div) { // writing here resets the counter to 0
            self.counter = 0;
        } else self.registers[fixed_address] = value;
    }
};
pub const InstructionSet = @import("instruction_set.zig");
pub const CPU = @import("cpu.zig");
pub const display = @import("display.zig");
pub const APU = @import("apu.zig");
pub const GPU = display.GPU;
pub const LCD = display.LCD;
pub const g = display.g;

const std = @import("std");
const print = std.debug.print;
