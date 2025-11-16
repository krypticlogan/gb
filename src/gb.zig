/// GameBoy Machine, defer endGB
const GB = @This();
cpu: CPU = CPU{}, // peripherals
gpu: GPU = GPU{},
apu: APU = APU{},
timer: Timer = Timer{},
joypad: Joypad = Joypad{},
bus: Bus = Bus{},
mbc: Bus.MBC = Bus.MBC{},
clock: Clock = Clock{},
cartridge_rom: []u8 = undefined,
rom_file_path: []const u8 = undefined,
running: bool = false, // state
crashed: bool = false,
cycles_spent: usize = 0,
last_frame: std.time.Instant = undefined,
allocator: std.mem.Allocator, // allocator
root_path: []const u8,
// startup
pub var prng: std.Random.Xoshiro256 = undefined;
pub fn init(self: *GB, testing: bool) !void {
    if (!testing) {
        try self.load_cartridge(); // load the game to be played
        try initRandom(); // init random before gpu init
        try self.gpu.init(self);
    }
    self.joypad.init(self);
    self.bus.init(self); // inits memory, connects peripherals
    self.timer.init(self);
    try self.cpu.init(self);
    const NOP = InstructionSet.instrs[0x0];
    _ = NOP.call(&self.cpu); // dummy op to init cache
    self.cpu.jump_to_prev_instr();
    self.running = true;
}
pub fn load_cartridge(self: *GB) !void {
    var args = try std.process.argsWithAllocator(self.allocator);
    defer args.deinit();
    _ = args.next(); // skip past the first arg
    const rom_file = try std.mem.concat(self.allocator, u8, &[_][]const u8{args.next() orelse "cpu_instrs", ".gb"});
    const rom_file_path = try std.fs.path.join(self.allocator, &.{self.root_path, "roms", rom_file});
    defer self.allocator.free(rom_file);
    defer self.allocator.free(rom_file_path);
    print("Rom path | {s}", .{rom_file_path});
    const rom = try std.fs.openFileAbsolute(rom_file_path, .{});
    defer rom.close();
    const stats = try rom.stat();
    self.cartridge_rom = try rom.readToEndAlloc(self.allocator, stats.size);
    print("Reading {d} bytes ({d}kb)...\n", .{self.cartridge_rom.len, self.cartridge_rom.len / 1024});

}
fn load_cartridge_to_rom(self: *GB) void {
    // load first 32kb to memory
    if (self.cpu.booted) {
        print("Boot state: ", .{});
        self.cpu.state_dump();
        for (0x0..0x100) |i| { // replace the bootrom after completed
            self.bus.memory[i] = self.cartridge_rom[i];
        }
    } else {
        const end = @min(0x8000, self.cartridge_rom.len);
        for (0x100..end) |i| {
            self.bus.memory[i] = self.cartridge_rom[i];
        }
    }
}
/// Loads GameBoy bootrom and the rom to be used
pub fn boot(self: *GB) !void {
    const bootFilePath = try std.fs.path.join(self.allocator, &.{self.root_path, "roms", "boot.bin"});
    defer self.allocator.free(bootFilePath);
    const bootFile = try std.fs.openFileAbsolute(bootFilePath, .{});
    defer bootFile.close();
    const bootFileStats = try bootFile.stat();
    const bootFileBuf: []u8 = try bootFile.readToEndAlloc(self.allocator, bootFileStats.size);
    defer self.allocator.free(bootFileBuf);
    for (0..bootFileBuf.len) |i| {
        self.bus.memory[i] = bootFileBuf[i];
    }
    self.load_cartridge_to_rom();

    self.mbc.header_setup(&self.bus);
    if (self.mbc.type != .unset) {
        const bank = self.mbc.get_rom_bank();
        const address = Bus.MBC.get_bank_address(bank);
        print("Bank {d} @ address 0x{X}\n", .{bank, address});
        self.bus.remap_bank(address);
    }
    // const ram_byte = self.cartridge_rom[0x149];


    // self.ext_ram =
}
// gb execution
pub fn go(self: *GB) !void {
    print("GO!\n", .{});
    try self.clock.Start();
    while (self.running) {
        self.clock.last_frame_time = Clock.Now();
        if (!self.cpu.paused) { // pause at breakpoints (debug)
            while (self.gpu.frame_cycles_spent < Clock.cycles_per_frame) {
                self.do() catch {
                    break;
                };
            }
        }
        // debug
        // if (self.crashed) {
        //     self.gpu.randomStatic();
        // }
        // print("frame cycles {d}", .{self.gpu.frame_cycles_spent});
        try self.getEvents(); // poll events once per frame
        self.clock.tick();
        self.clock.update(); // calculates average fps
        self.gpu.lcd.renderAll(self.cpu.log.writeAll()); // render at the last scanline
        if (self.gpu.frame_cycles_spent >= Clock.cycles_per_frame) self.gpu.frame_cycles_spent = 0;
    }
}
fn do(self: *GB) !void {
    // if (self.cpu.booted) {
    //     CPU.Log.write_to_file(.{
    //         self.cpu.get_byte(.a),
    //         self.cpu.f.value,
    //         self.cpu.get_byte(.b),
    //         self.cpu.get_byte(.c),
    //         self.cpu.get_byte(.d),
    //         self.cpu.get_byte(.e),
    //         self.cpu.get_byte(.h),
    //         self.cpu.get_byte(.l),
    //         self.cpu.sp,
    //         self.cpu.pc,
    //         self.cpu.bus.readByte(self.cpu.pc),
    //         self.cpu.bus.readByte(self.cpu.pc + 1),
    //         self.cpu.bus.readByte(self.cpu.pc + 2),
    //         self.cpu.bus.readByte(self.cpu.pc + 3),
    //     });
    // }
    const res = self.cpu.execute();
    const cycles_spent = res[0];
    self.cycles_spent += cycles_spent;
    const cycles_to_spend: u8 = @max(1, cycles_spent);
    self.gpu.tick(cycles_to_spend * 4);
    self.timer.tick(cycles_to_spend * 4);
    self.bus.handler.handle(&self.cpu);
    if (self.cpu.pc > 0xFF and !self.cpu.booted) {
        self.cpu.booted = true;
        self.load_cartridge_to_rom();
    }
    if (res[1]) return error.StepMode;
}
// helper
fn initRandom() !void {
    var seed: u64 = undefined;
    try std.posix.getrandom(std.mem.asBytes(&seed));
    prng = std.Random.DefaultPrng.init(seed);
}
// universal callers

pub fn getEvents(self: *GB) !void {
    var event: g.SDL_Event = undefined;
    while (g.SDL_PollEvent(&event)) {
        switch (event.type) {
            g.SDL_EVENT_KEY_DOWN => {
                switch (event.key.key) {
                    g.SDLK_1 => {
                       LCD.nextPalette();
                    },
                    g.SDLK_P => {
                        switch (self.cpu.paused) {
                            false => self.cpu.break_exe(),
                            true => {
                                self.cpu.resume_exe();
                                self.cpu.step = false;
                            }
                        }
                    },
                    g.SDLK_SPACE => {
                        if (self.cpu.paused) {
                            // self.state_dump();
                            self.cpu.step = true;
                            self.cpu.resume_exe();
                        }
                    },
                    else => continue
                }
            },
            g.SDL_EVENT_KEY_UP => {},
            g.SDL_EVENT_QUIT => {
                self.running = false;
            },
            g.SDL_EVENT_WINDOW_RESIZED => { // TODO
                print("RESIZED, NEW SIZE\n\n\n\n\n\n", .{});
            },
            else => {},
        }
    }
}
// memspace dumps
pub fn mem_dump(self: *GB, start: u16, end: u16) void {
    print("printing bytes:\n", .{});
    for (self.memory[start..end], start..end) |value, i| {
        if (i != 0 and i % 16 == 0) print("\n", .{});
        print("0x{x} ", .{value});
    }
    print("\n", .{});
}
pub fn gfx_dump(self: *GB) void {
    print("Actual memspace dump: \n", .{});
    for (self.memory[0x8000 .. 0x97FF + 1], 0..(0x97FF - 0x8000 + 1)) |value, i| {
        print("0x{x}", .{value});
        if (i != 0 and i % 80 == 0) print("\n", .{});
    }
    print("\n", .{});
}
pub fn reg_dump(self: *GB) void {
    print("Actual memspace dump:\n", .{});
    for (self.memory[GPU.special_register.start .. GPU.special_register.end + 1], GPU.special_register.start..GPU.special_register.end + 1) |value, i| {
        print("register@0x{x}: 0x{x}\n", .{ i, value });
    }
    const i = 0xFF44;
    print("register@0x{x}: 0x{x}\n", .{ i, self.gpu.getSpecialRegister(.ly) });
    print("\n", .{});
}
pub fn state_dump(self: *GB) void {
    self.cpu.state_dump();
    self.gpu.spec_register_dump();
    self.bus.handler.dump(); // interrupts state
}
pub fn endGB(self: *GB) void {
    print("final pc: 0x{X}\n", .{self.cpu.pc});
    print("Serial output: {s}", .{serialBuf[0..serialIndex]});
    self.allocator.free(self.cartridge_rom);
    self.gpu.lcd.endSDL();
}

//
var serialBuf: [1024]u8 = undefined;
var serialIndex: u8 = 0;
pub const Bus = struct {
    const SIZE = 0xFFFF + 1;
    memory: [SIZE]u8 = undefined,
    // peripherals
    cpu: *CPU = undefined,
    gpu: *GPU = undefined,
    apu: *APU = undefined,
    timer: *Timer = undefined,
    joypad: *Joypad = undefined,
    mbc: *MBC = undefined,
    rom: []u8 = undefined,
    ext_ram: []u8 = undefined,
    handler: InterruptHandler = InterruptHandler{},

    pub fn init(self: *@This(), gb: *GB) void {
        @memset(&self.memory, 0);
        self.handler.init(self);
        self.cpu = &gb.cpu;
        self.gpu = &gb.gpu;
        self.apu = &gb.apu;
        self.joypad = &gb.joypad;
        self.timer = &gb.timer;
        self.mbc = &gb.mbc;
        self.rom = gb.cartridge_rom;
    }
    fn remap_bank(self: *Bus, address: u32) void {
        // print("attempting to map\n", .{});
        // print("prior: {any}\n\n", .{self.memory[0x4000 .. 0x4000 + 20]});
        @memcpy(
            self.memory[0x4000 .. 0x4000 + MBC.ROM_BANK_SIZE], // 0x4000 - 0x7FFF
            self.rom[address .. address + MBC.ROM_BANK_SIZE]
        );
        // print("new: {any}\n\n", .{self.memory[0x4000 .. 0x4000 + 20]});
    }
    pub fn readByte(self: *Bus, address: u16) u8 {
        @setRuntimeSafety(false);
        // if (address >= 0xFF00) return self.memory[address];
        if (address >= CPU.WRAM_START and address <= CPU.WRAM_END) {
            return self.memory[address];
        }
        if (address >= GPU.VRAM_BEGIN and address <= GPU.VRAM_END) {
            return self.gpu.readVram(address);
        }
        if (address >= GPU.special_register.start and address <= GPU.special_register.end) {
            // if (self.cpu.booted and address == 0xFF44) return 0x90;
            return self.gpu.getSpecialRegister(@as(GPU.special_register, @enumFromInt(address - GPU.special_register.start)));
        }
        if (address >= GPU.OAM_BEGIN and address <= GPU.OAM_END) {
            return self.gpu.oam[address - GPU.OAM_BEGIN];
        }
        if (address >= Timer.START and address <= Timer.END) {
            // print("read timer @0x{X}, got 0x{X}\n", .{address, self.timer.read(address)});
            return self.timer.read(address);
        }
        if (address == 0xFF0F) {
            return self.handler.read(.flag);
        }
        if (address == 0xFFFF) {
            return self.handler.read(.enable);
        }
        if (address == 0xFF00) {
            // print("read joypad\n", .{});
            return self.joypad.check();
        }
        return self.memory[address];
    }
    pub fn writeByte(self: *Bus, address: u16, value: u8) void {
        // @setRuntimeSafety(false);
        // switch (address) { // TODO test speed
        //     address >= Timer.START and address <= Timer.END => {
        //
        //     },
        // }
        if (address >= Timer.START and address <= Timer.END) {
            // print("write to timer @0x{X}, value 0x{X}\n", .{address, value});
            // self.cpu.break_exe();
            self.timer.write(address, value);
        } else if (address >= GPU.special_register.start and address <= GPU.special_register.end) {
            const register = @as(GPU.special_register, @enumFromInt(address - GPU.special_register.start));
            switch (register) {
                .ly => {}, // no writes
                .stat => self.gpu.setSpecialRegister(register, value & 0b1111_1000), // bottom 3 bytes are read only
                else => self.gpu.setSpecialRegister(register, value)
            }
            self.gpu.setSpecialRegister(register, value);
            // handle dma transfers
            if (register == GPU.special_register.dma) {
                const prefix = address / 0x100;
                const ram_address: u16 = @as(u16, @intCast(prefix)) << 8;
                // print("RAM ADDR: [0x{X}], copy size\n\t source: 0x{X}\n\t dest: 0x{X}\n", .{ram_address, GPU.OAM_SIZE, GPU.OAM_END - GPU.OAM_BEGIN});
                @memcpy(self.memory[GPU.OAM_BEGIN..GPU.OAM_END + 1], self.memory[ram_address .. ram_address + GPU.OAM_SIZE]);
            }
        } else if (address >= GPU.VRAM_BEGIN and address <= GPU.VRAM_END) { // VRAM banks 0 & 1
            self.gpu.writeVram(address, value);
        } else if (address < 0x8000) { // route rom writes to mbc
            const new_address = self.mbc.rom_trap(address, value);
            if (new_address) |map_to| { // if we received a new anchor address, remap
                self.remap_bank(map_to);
            }
        } else if (address == 0xFF01) { // serial byte
            // print("serial byte: 0x{X} ({d}): {c}\n", .{value, value, value});
            // self.cpu.state_dump();
            self.memory[address] = value;
        } else if (address == 0xFF02) { // serial control
            self.memory[address] = value;
            if (value == 0x81) {
                var buf: [1]u8 = undefined;
                const byte = std.fmt.bufPrint(&buf, "{c}", .{self.memory[0xFF01]}) catch {
                  @panic("No space");
                };
                serialBuf[serialIndex] = byte[0];
                serialIndex += 1;
                // print("SERIAL: [{c}]\n", .{self.memory[0xFF01]});
                // const char = self.memory[0xFF01];
                // print("byte: 0x{X}, '{c}'\n", .{char, char});
                self.memory[address] &= ~@as(u8, 0x80);
            }
        }
        // else if (address == 0xFFFF) {
        //     self.memory[address] = value;
        //     // print("wrote to the iF flag = 0b{b}\n does it show?\t handler IE flag = 0b{b}\n", .{value, self.handler.iE.*});
        //     // self.handler.dump();
        // } else if (address == 0xFF0F) {
        //     self.memory[address] = value;
        //     // print("wrote to the iF flag = 0b{b} @pc[{X}]\n does it show?\t handler iF flag = 0b{b}\n", .{value, self.cpu.pc, self.handler.iF.*});
        //     // self.handler.dump();
        // }
        else self.memory[address] = value;
    }
    /// Memory Bank Controller
    const MBC = struct {
        const Type = union(enum) {
            unset,
            mbc1,
            mbc2
        };
        const ROM_BANK_SIZE = 0x4000;
        const RAM_BANK_SIZE = 0x2000;
        rom_bank_low: u5 = 0, // 5 bit register to identify rom_bank
        bank_high: u2 = 0, // this can refer to RAM or ROM high bits
        type: Type = .unset,
        ram_enable: bool = false,
        ram_banking_mode: bool = false,
        ram_size_in_KiB: u7 = undefined,
        rom_size_in_KiB: u7 = undefined,

        pub fn header_setup(self: *MBC, bus: *Bus) void {
            self.setup_type(bus.memory[0x147]);
            self.get_rom_size_in_KiB(bus.memory[0x148]);
            self.get_ram_size_in_KiB(bus.memory[0x149]);
        }
        pub fn setup_type(self: *MBC, byte: u8) void {
            self.type = switch (byte) {
                0 => .unset,
                1, 2, 3 => .mbc1,
                else => .unset // TODO: add more mbc support
            };
            if (self.type != .unset) { // set the rom bank to use
                self.rom_bank_low = 1;
            }
            print("Set MBC to type {any}\n", .{self.type});
        }
        pub fn get_ram_size_in_KiB(self: *MBC, byte: u8) void {
            self.ram_size_in_KiB = switch (byte) {
                0 => 0,
                1 => 2,
                2 => 8,
                3 => 32,
                else => 0
            };
            print("Detected {d}KiB of RAM, 0x149 value: 0x{X}\n", .{self.ram_size_in_KiB, byte});
        }
        pub fn get_rom_size_in_KiB(self: *MBC, byte: u8) void {
            self.rom_size_in_KiB = switch (byte) {
                0 => 32,
                1 => 64,
                else => 127
            };
            print("Detected {d}KiB of R0M, 0x148 value: 0x{X}\n", .{self.rom_size_in_KiB, byte});
        }
        fn get_rom_bank(self: *MBC) u7 { // 2^7 - 3 (125) addressable banks
            // print("High bits: 0b{b}, low bits: 0b{b}\n", .{self.bank_high, self.rom_bank_low});
            const bank = (@as(u7, self.bank_high) << 5) | self.rom_bank_low;
            return @intCast(bank % ((@as(usize, self.rom_size_in_KiB) * 1024) / ROM_BANK_SIZE));
        }
        fn get_bank_address(bank: u7) u32 {
            // print("bank: 0x{X}, 0b{b}\n", .{bank, bank});
            return @as(u32, bank) * ROM_BANK_SIZE;
        }
        pub fn rom_trap(self: *MBC, address: u16, byte: u8) ?u32 {
            var new_address: ?u32 = null;
            switch (self.type) {
                .mbc1 => {
                    switch (address) {
                        0x0000...0x1FFF => { // RAM enable/disable (MBC1, MBC2, etc.).
                            if (self.ram_size_in_KiB > 0) print("ram enable\n", .{});
                            if (byte == 0x0) {
                                self.ram_enable = false;
                            } else if (@as(u4, @truncate(byte)) == 0xA) {
                                self.ram_enable = true;
                            }
                        },
                        0x2000...0x3FFF => { // ROM bank number (low bits). Writing here selects which ROM bank gets mapped into 0x4000–0x7FFF. The written value is masked depending on the MBC (e.g., only lower 5 or 7 bits are used).
                            // print("bank switch | wrote 0x{X}, 0b{b}\n", .{byte, byte});
                            var bank: u5 = @truncate(byte);
                            if (self.rom_size_in_KiB <= 64) {
                                bank = @as(u3, @truncate(bank));
                            }
                            self.rom_bank_low = if (bank != 0) bank else 1;
                            const new_bank = self.get_rom_bank();
                            new_address = get_bank_address(new_bank);
                        },
                        0x4000...0x5FFF => { // Upper bits of ROM bank or RAM bank number depending on mode.
                            // print("bank switch (high)\n", .{});
                            self.bank_high = @truncate(byte);
                            const new_bank = self.get_rom_bank();
                            new_address = get_bank_address(new_bank);
                        },
                        0x6000...0x7FFF => { // Banking mode select (switch between ROM banking mode and RAM banking mode).
                            // print("mode switch\n", .{});
                            self.ram_banking_mode = switch (@as(u1, @truncate(byte))) {
                                0 => false,
                                1 => true
                            };
                        },
                        else => return null // out of ROM range
                    }
                },
                else => return null
            }
            return new_address;
        }
    };

    /// Handles/Services interrupts sent to the GameBoy from various devices
    pub const InterruptHandler = struct {
        const InterruptBit = enum(u3) {
            vblank,
            lcd,
            timer,
            serial,
            joypad,
            _
        };
        const Interrupt = struct {
            bit: InterruptBit,
            source: u16
        };
        const Interrupts = [_]Interrupt{
            .{.bit = .vblank, .source = 0x40}, // highest priority
            .{.bit = .lcd, .source = 0x48},
            .{.bit = .timer, .source = 0x50},
            .{.bit = .serial, .source = 0x58},
            .{.bit = .joypad, .source = 0x60} // least priority
         };
        iE: *u8 = undefined, // interrupt enable
        iF: *u8 = undefined, // interrupt flag
        ime: bool = false, // interrupt master enable
        fn init(self: *InterruptHandler, bus: *Bus) void {
            self.iE = &bus.memory[0xFFFF];
            self.iF = &bus.memory[0xFF0F];
        }
        fn check(self: *InterruptHandler, target: enum {enable, flag}, bit: InterruptBit) bool {
            return switch (target) {
                .enable => @as(u1, @truncate(self.iE.* >> @intFromEnum(bit))) != 0,
                .flag => @as(u1, @truncate(self.iF.* >> @intFromEnum(bit))) != 0
            };
        }
        pub fn set(self: *InterruptHandler, target: enum {enable, flag}, bit: InterruptBit) void {
            const mask = @as(u8, 1) << @intFromEnum(bit);
            switch (target) {
                .enable => self.iE.* |= mask,
                .flag => self.iF.* |= mask
            }
        }
        pub fn clear(self: *InterruptHandler, target: enum {enable, flag}, bit: InterruptBit) void {
            const mask: u8 = ~(@as(u8, 1) << @intFromEnum(bit));
            switch (target) {
                .enable => self.iE.* &= mask,
                .flag => self.iF.* &= mask
            }
        }
        pub inline fn enable(self: *InterruptHandler) void {
            self.ime = true;
        }
        pub inline fn handle(self: *InterruptHandler, cpu: *CPU) void {
            // which interrupt do we need to handle (highest priority first)
            if (self.ime) {
                // print("interrupt handle @pc[{X}]\n", .{cpu.pc});
                // if (self.check(.flag, .lcd)) {
                    // self.dump();
                // }
                for (Interrupts) |interrupt| {
                    if (self.check(.enable, interrupt.bit) and self.check(.flag, interrupt.bit)) {
                        self.ime = false;
                        self.clear(.flag, interrupt.bit);
                        cpu.push_stack(cpu.pc);
                        cpu.pc = interrupt.source;
                        // print("\thandled {any}, set pc to 0x{X}\n", .{interrupt, cpu.pc});
                        break;
                    }
                } // 5 M cycles
            }
        }
        pub fn read(self: *InterruptHandler, target: enum { enable, flag }) u8 {
            return @intCast(
                switch (target) {
                    .enable => @as(u5, @truncate(self.iE.*)),
                    .flag =>  @as(u5, @truncate(self.iF.*)),
                }
            );
        }
        pub fn dump(self: *InterruptHandler) void {
            print("IME: {d}\n", .{@intFromBool(self.ime)});
            for (std.enums.values(InterruptBit)) |interrupt| {
                print("{any}[ E: ({d})\tF: ({d}) ]\n", .{interrupt, @intFromBool(self.check(.enable, interrupt)), @intFromBool(self.check(.flag, interrupt))});
            }
        }
    };
};

/// Joypad logic
pub const Joypad = struct {
    const button = enum {
        select, start, // options
        a, b, // gameplay
        up, down, left, right // d-pad
    };

    button_field: *u8 = undefined,

    fn init(self: *Joypad, gb: *GB) void {
        self.button_field = &gb.bus.memory[0xFF00];
    }

    fn check(self: *Joypad) u8 {
        _ = self;
        return 0xF;
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
    bus: *Bus = undefined,
    div_counter: u16 = 0, // master counter (div)
    tima_counter: u16 = 0,
    prev_and_res: bool = false,
    cycles_since_overflow: ?u8 = null,
    const START = 0xFF04;
    const END = 0xFF07;
    fn init(self: *Timer, gb: *GB) void {
        self.registers = gb.bus.memory[START .. END + 1];
        self.bus = &gb.bus;
    }
    const timer_reg = enum {
        div,    // $FF04 - Divider Register (DIV)
        tima,   // $FF05 - Timer Counter (TIMA)
        tma,    // $FF06 - Timer Modulo (TMA)
        tac,    // $FF07 - Timer Control (TAC)
        //          |bit 2| timer enable (on/off)  bool?
        //          |bit 1-0| clock select enum?
        //          0b00 : CPU Clock / 1024
        //          0b01 : CPU Clock / 16
        //          0b10 : CPU Clock / 64
        //          0b11 : CPU Clock / 256
    };
    fn get_reg(timer: *Timer, reg: timer_reg) u8 {
        return timer.registers[@intFromEnum(reg)];
    }
    fn set_reg(timer: *Timer, reg: timer_reg, value: u8) void {
        timer.registers[@intFromEnum(reg)] = value;
    }
    // This tick function proves to be a tricky area. But it eventually started passing test roms
    // You should probably return to this and look into it some more (tima is never zero, interrupts are immediate)
    fn tick(self: *Timer, t_cycles: u8) void {
        // We should tick the div register once for every t-cycle
        self.div_counter = @addWithOverflow(self.div_counter, t_cycles)[0];

        const enabled: bool = (self.get_reg(.tac) >> 2) == 1;
        const clock_sel: u16 = switch (@as(u2, @truncate(self.get_reg(.tac)))){
            0b00 => 1024,
            0b01 => 16,
            0b10 => 64,
            0b11 => 256
        };
        // const bit_pos: u4 = @intCast(@ctz(clock_sel) - 1); // our bit position is the same as one less than the number of trailing zeros
        // const bit: bool = self.div_counter & clock_sel != 0;
        // const and_res = enabled and bit;
        if (enabled) {
        self.tima_counter += t_cycles;
        while (self.tima_counter >= clock_sel) : (self.tima_counter -= clock_sel) {
            // const should_tick = tima_cycles_spent % clock_sel == 0;
            // if (self.prev_and_res and !and_res) { // falling edge
                const tima = self.get_reg(.tima);
            if (tima < 0xFF) {
                self.set_reg(.tima, tima + 1);
            } else { // overflow
                    self.set_reg(.tima, self.get_reg(.tma));
                self.cycles_since_overflow = 0;
                self.bus.handler.set(.flag, .timer);
            }
            // }

            // self.prev_and_res = and_res;
            //
            // if (self.cycles_since_overflow) |cycles_since_overflow| {
            //     switch (cycles_since_overflow) {
            //         4 => { // interrupt
            //             self.set_reg(.tima, self.get_reg(.tma));
            //             self.bus.handler.set(.flag, .timer);
            //         },
            //         else => self.cycles_since_overflow.? += 1
            //     }
            // }
            }
        }

        // Source: https://github.com/feo-boy/feo-boy/blob/master/src/bus/timer.rs#L56-L66
            // NB: This is the source of a very common bug in timer implementations.
            //
            // Here, we need to increment the timer's internal counter relative to the tick size. The
            // counter may have to be incremented multiple times for a given tick. While this
            // technically could happen for the div internal counter, in practice it doesn't: no
            // instruction takes longer to execute than it takes to increment DIV once. However, it
            // _is_ possible to have the timer internal counter increment multiple times during a given
            // instruction.
            //
            // Notably, getting this wrong will cause blargg's instr_timing test ROM to fail with
            // the cryptic "Failure #255" message.;

            // imagine we dont complete the required 4 t-cycles before we exit to complete another instruction
            // in the case that cycles since overflow is '3' and we exit, imagine we execute a 1 m-cycle instruction
            // in this case, we should have 4 more t-cycles to execute, meaning that after the first one,
            // we should immediately reload the value in the tima register and execute a timer interrupt
            // execution continues, and we execute the other 3 t-cycles that we're meant to.
            // an interrupt should not happen in this time
    }
    fn read(self: *Timer, address: u16) u8 {
        const fixed_address: u3 = @intCast(address - START);
        return switch (@as(timer_reg, @enumFromInt(fixed_address))) {
            .div => @truncate(self.div_counter >> 8),
            else => self.registers[fixed_address]
        };
    }
    fn write(self: *Timer, address: u16, value: u8) void {
        const fixed_address: u3 = @intCast(address - START);
        // print("timer reg len: {d}, index: {d}", .{self.registers.len, fixed_address});
        switch (@as(timer_reg, @enumFromInt(fixed_address))) { // writing here resets the counter to 0
            .div => self.div_counter = 0,
            else => self.registers[fixed_address] = value
        }
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
