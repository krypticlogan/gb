/// Defines a gameboy GPU(PPU)
/// - Handles writing to vram and processing pixels from memory to the screen
pub const GPU = struct {
    pub const VRAM_BEGIN = 0x8000;
    pub const VRAM_END = 0x9FFF;
    pub const VRAM_SIZE = VRAM_END - VRAM_BEGIN + 1;
    pub const OAM_BEGIN = 0xFE00;
    pub const OAM_END = 0xFE9F;
    pub const OAM_SIZE = OAM_END - OAM_BEGIN + 1;

    const tilemap_one = 0x9800;
    const tilemap_two = 0x9C00;
    const tilemap_size: u16 = tilemap_two - tilemap_one;

    const Mode = enum { // modes specifying number of cycles per scanline
        HBLANK,
        VBLANK,
        SCAN,
        RENDER,
        // const cycles: [4]u16 = .{ 204, 456, 80, 172 };
        fn min_cycles(self: Mode) u16 {
            return switch (self) {
                .HBLANK => 204,
                .VBLANK => 456,
                .SCAN => 80,
                .RENDER => 172
            };
        }
    };
    const Color = enum(u2) { transparent, dgray, lgray, white };
    const Sprite = struct {
        y: u8 = 0,
        x: u8 = 0,
        tile_no: u8 = 0,
        flags: u8 = 0,
        const len = 4;
    };

    bus: *Bus = undefined,
    vram: *[VRAM_SIZE]u8 = undefined,
    /// Stores sprite attributes (position, tile index, attributes).
    oam: *[OAM_SIZE]u8 = undefined,
    /// Mid-scanline tracker for the how far into OAM we have searched
    oam_i: u8 = 0,
    /// LCD Control Registers (I/O Registers at $FF40–$FF4B)
    special_registers: *[12]u8 = undefined,
    /// Holds sprites collected within .SCAN Mode
    sprite_buffer: [10]?Sprite = undefined,
    /// Mid-scanline tracker for the how far into Sprite buffer we have inserted
    sprite_buffer_i: u8 = 0,
    /// Counts the number of used sprites, MAX of 3 per scanline
    sprites_used: u8 = 0,

    // Fifos that the fetcher uses to process pixels
    sprite_pixel_fifo: std.ArrayList(Color) = undefined,
    sprite_pixel_fifo_buffer: [8]Color = undefined,
    bg_pixel_fifo: std.ArrayList(Color) = undefined,
    bg_pixel_fifo_buffer: [8]Color = undefined,

    // STATE
    mode: Mode = undefined,
    stat_reg: u8 = undefined,
    interrupt_pending: bool = false,
    scanline: [LCD.screenWidthPx]Color = undefined,
    scanline_fetched: u8 = 0,
    scanline_displayed: u8 = 0,
    scx_discard: u8 = 0,
    reached_window: bool = false,
    mode_cycles_left: u16 = 0,
    frames_cycled: usize = 0,
    frame_cycles_spent: u64 = 0,

    // Peripherals
    lcd: LCD = undefined,
    /// Combined BG/Sprite pixel fetcher
    fetcher: pixel_fetcher = undefined,

    // rand: std.Random = undefined,
    // startup
    pub fn init(self: *@This(), gb: *GB) !void {
        self.vram = gb.bus.memory[VRAM_BEGIN .. VRAM_END + 1];
        self.oam = gb.bus.memory[OAM_BEGIN .. OAM_END + 1];
        self.bus = &gb.bus;
        self.special_registers = gb.bus.memory[special_register.start .. special_register.end + 1];
        self.mode = .SCAN;
        self.mode_cycles_left = self.mode.min_cycles();
        self.sprite_pixel_fifo = std.ArrayList(Color).initBuffer(&self.sprite_pixel_fifo_buffer);
        self.bg_pixel_fifo = std.ArrayList(Color).initBuffer(&self.bg_pixel_fifo_buffer);
        @memset(&self.scanline, GB.prng.random().enumValue(Color));
        try self.lcd.init(gb.allocator, gb.root_path);
        self.fetcher.init(self);
        // @memset(&self.sprite_buffer, Sprite{});
    }
    /// gpu execution
    pub fn tick(self: *@This(), cycles: u16) void {
        // the gpu should tick/cycle just as many
        // times as the cpu did, while being able to
        // process interrupts and continue on as well as changing modes mid-scanline when needed
        var cycles_left = cycles; // amt of cycles spent by cpu
        self.frame_cycles_spent += cycles_left;
        while (cycles_left > 0) {
            if (self.mode_cycles_left == 0) {
                self.switchMode(); // handles drawing the screen, updating ly
                // print("Mode switch: {any}, LY: {d}, stat: {d}\n", .{ self.mode, self.getSpecialRegister(.ly), self.getSpecialRegister(.stat) });
            }
            const cycles_to_process: u8 = @intCast(@min(cycles_left, self.mode_cycles_left));

            self.do(cycles_to_process);
            self.mode_cycles_left -= cycles_to_process;
            cycles_left -= cycles_to_process;
        }
    }
    fn do(self: *@This(), cycles: u8) void {
        // Operate GPU here
        // // const zone = tracy.beginZone(@src(), .{ .name = "DO GPU CYCLES" });
        // // defer zone.end();
        var cycles_to_spend: i16 = @intCast(cycles);
        switch (self.mode) {
            .SCAN => { // 2 searches OAM memory for sprites that should be rendered on the current scanline and stores them in a buffer
                while (self.sprite_buffer_i < 10 and self.oam_i < OAM_SIZE) : (self.oam_i+=4) {
                    const ly = self.getSpecialRegister(.ly);
                    const x_pos = self.oam[self.oam_i+1];
                    const y_pos = self.oam[self.oam_i];
                    const tile_no = self.oam[self.oam_i+2];
                    const flags = self.oam[self.oam_i+3];

                    const height: u8 = switch (BIT(2, self.getSpecialRegister(.lcdc)) == 1) {
                        true => 16,
                        false => 8
                    };
                    const horizontally_visible = x_pos > 0;
                    const vertically_visible =  y_pos <= ly + 16 and y_pos + height > ly + 16; // Within our current scanline?

                    if (horizontally_visible and vertically_visible) {
                        self.sprite_buffer[self.sprite_buffer_i] = Sprite { .y = y_pos, .x = x_pos, .tile_no = tile_no, .flags = flags };
                        self.sprite_buffer_i += 1;
                    }
                    cycles_to_spend -= 2;
                    if (cycles_to_spend <= 0) return;
                }
                // return;
            },
            .RENDER => { // 3 transfers pixels to the LCD, one scanline at a time, duration variable
                // TODO: Generate the actual pixels for this scanline based on:
                // - Background tiles at the current scroll position
                // - Window tiles if enabled and visible on this line
                // - Sprites that were found during OAM scan
                if (!self.testSpecialRegister(.lcdc, 7)) return;

                while (self.scanline_displayed < self.scanline.len and cycles_to_spend > 0) {
                    if (self.scx_discard > 0) {
                        print("discarding, progress: {d}, fetched: {d}\n", .{self.scanline_fetched, self.scanline_displayed});
                        _ = self.bg_pixel_fifo.pop();
                        self.scx_discard -= 1;
                        // self.scanline_fetched += 1;
                        cycles_to_spend -= 1;
                        continue;
                    }

                    const cycles_spent = self.fetcher.step_forward();
                    cycles_to_spend -= cycles_spent;

                    var pixels_pushed: u8 = 0;
                    // push pixels to scanline at one cycle per pixel
                    while (pixels_pushed < cycles_spent) : (pixels_pushed += 1) {
                        if (self.bg_pixel_fifo.pop()) |bg_pixel| {
                            const sprite_pixel = self.sprite_pixel_fifo.pop() orelse .transparent;
                            self.scanline[self.scanline_displayed] = pixelMixer(sprite_pixel, bg_pixel, false);
                            self.scanline_displayed += 1;
                            // print("scanline px color: {any} (pushed {d} pixels)\n", .{self.scanline[self.scanline_displayed], self.scanline_displayed});
                        } else break;
                    }
                }       
            },
            else => return, // no action for hblank or vblank
        }
    }

    fn detect_window_reached(self: *GPU) void {
        if (self.reached_window) return; // once reached, we do not need to update this anymore
        self.reached_window = (
            self.testSpecialRegister(.lcdc, 5) == 1 and // win enabled
            self.getSpecialRegister(.wy) == self.getSpecialRegister(.ly) and // and we have reached the window vertically
            self.scanline_displayed >= self.getSpecialRegister(.wx) - 7 // and we have reached the window horizontally
        );

    }
    fn detect_sprite_fetch(self: *GPU) void {
        // sprite fetch check
        for (self.sprites_used..self.sprite_buffer_i) |cur_sprite_i| {
            const sprite = self.sprite_buffer[cur_sprite_i] orelse unreachable;
            const sprite_x = sprite.x - 8;
            if (sprite_x + 8 < self.scanline_fetched + 8) {
                continue;
            }
            const pixel_i = self.scanline_fetched - sprite_x;

            if (pixel_i < 0 or pixel_i > 7) {
                continue;
            }
            self.fetcher.active_fetcher = .sprite;
            self.fetcher.reset_state();
        }
    }
    const pixel_fetcher = struct {
        const fetcher_state = struct {
            tile_addr: ?u16 = null,
            tile_low: ?u8 = null,
            tile_high: ?u8 = null,
            step: step = .tile_no,

            fn reset(self: *fetcher_state) void {
                self.tile_addr = null;
                self.tile_high = null;
                self.tile_low = null;
                self.step = .tile_no;
            }
        };
        const fetcher_type = enum {
            bg,
            win,
            sprite
        };
        const step = enum {
            tile_no,
            tile_low,
            tile_high,
            push
        };
        gpu: *GPU = undefined,

        bg_fetcher: fetcher_state = fetcher_state{},
        win_fetcher: fetcher_state = fetcher_state{},
        sprite_fetcher: fetcher_state = fetcher_state{},

        active_fetcher: fetcher_type = .bg,
        sprite_data: ?Sprite = null,



        fn init(self: *pixel_fetcher, gpu: *GPU) void {
            self.gpu = gpu;
        }
        fn active_fetcher_state(self: *pixel_fetcher) *fetcher_state {
            return switch (self.active_fetcher) {
                .bg => &self.bg_fetcher,
                .win => &self.win_fetcher,
                .sprite => &self.sprite_fetcher
            };
        }
        fn reset_state(self: *pixel_fetcher) void {
            if (self.active_fetcher == .sprite) self.sprite_fetcher.reset();
            if (self.active_fetcher == .win) self.win_fetcher.reset();
            self.bg_fetcher.reset();
            self.active_fetcher = .bg;
        }
        fn step_forward(self: *pixel_fetcher) u8 {
            // print("step {d}: {any}\n", .{@intFromEnum(self.state), self.state});
            var state = self.active_fetcher_state();
            return cycles: switch (state.step) {
                .tile_no => {
                    // file tile position
                    const ly = self.gpu.getSpecialRegister(.ly);
                    // only scroll on bg
                    const scx = if (self.active_fetcher == .bg) self.gpu.getSpecialRegister(.scx) else 0;
                    const scy = if (self.active_fetcher == .bg) self.gpu.getSpecialRegister(.scy) else 0;

                    state.tile_addr = addr: switch (self.active_fetcher) {
                        .bg, .win => {
                            const use_signed = !self.gpu.testSpecialRegister(.lcdc, 4);
                            const base: i32 = if (use_signed) 0x9000 else 0x8000;

                            const tilemap_base: u16 = switch (self.active_fetcher) { // check lcdc bit 3 for bg, bit 6 for win}
                                .bg => if (self.gpu.testSpecialRegister(.lcdc, 3)) 0x9C00 else 0x9800,
                                .win => if (self.gpu.testSpecialRegister(.lcdc, 6)) 0x9C00 else 0x9800,
                                else => unreachable
                            };

                            const bg_y: u16 = (@as(u16, ly) + scy) & 0xFF; // wraps at 256
                            const bg_x: u16 = (@as(u16, self.gpu.scanline_fetched) + scx) & 0xFF;

                            // fetch bg pixel (maybe window tile)
                            const tile_y: u16 = bg_y / 8;
                            const tile_x: u16 = bg_x / 8;

                            const tile_index_addr: u16 = tilemap_base + tile_y * 32 + tile_x;

                            const tile_index: i16 = if (use_signed) @intCast(@as(i8, @bitCast(self.gpu.readVram(tile_index_addr)))) else @intCast(self.gpu.readVram(tile_index_addr));
                            const tile_line: u16 = bg_y % 8;

                            break :addr @intCast(base + tile_index * 16 + @as(i32, @intCast(tile_line * 2)));
                            // print("LY={d}, scanline_fetched={d}, bg_x={d}, tile_x={d}\n", .{ly, self.gpu.scanline_fetched, bg_x, tile_x});
                        },
                        .sprite => {
                            const sprite_tile_line: u16 = (ly - self.sprite_data.?.y) % 8;
                            break :addr 0x8000 + @as(u16, @intCast(self.sprite_data.?.tile_no)) * 16 + sprite_tile_line * 2;
                        }
                    };
                    state.step = .tile_low;
                    break :cycles 2;
                },
                .tile_low => {
                    state.tile_low = self.gpu.readVram(state.tile_addr.?);
                    state.step = .tile_high;
                    break :cycles 2;
                },
                .tile_high => {
                    state.tile_high = self.gpu.readVram(state.tile_addr.? + 1);
                    state.step = .push;
                    break :cycles 2;
                },
                .push => {
                    if (self.gpu.bg_pixel_fifo.items.len == 0) {
                        var tile_i: u4 = 8;
                        while (tile_i > 0) {
                            tile_i -= 1;
                            const rendering_sprites = self.active_fetcher == .sprite;
                            const bg_win_enabled = self.gpu.testSpecialRegister(.lcdc, 0);
                            const pixel = color: {
                              if (rendering_sprites or bg_win_enabled) {
                                  break: color self.gpu.tilePixelDecoder(state.tile_high.?, state.tile_low.?, @intCast(tile_i));
                              } else break: color .transparent;
                            };
                            // fetch_sprite_pixels()
                            switch (self.active_fetcher) {
                                .bg, .win => self.gpu.bg_pixel_fifo.appendAssumeCapacity(pixel),
                                .sprite => self.gpu.sprite_pixel_fifo.appendAssumeCapacity(pixel)
                            }
                        }
                        if (self.active_fetcher == .sprite) self.gpu.sprites_used += 1
                            else self.gpu.scanline_fetched += 8;
                        self.reset_state(); // reset the fetcher state as this is the last step
                        break :cycles 2;
                    } else break :cycles 1;
                }
            };
        }
    };

    pub fn randomStatic(self: *GPU) void { // random static
        for (0..self.lcd.screenBuf.len) |i| {
            const color: Color = GB.prng.random().enumValue(Color);
            LCD.writeToBuf(&self.lcd.screenBuf, color, i);
        }
    }
    // fn createTile()
    fn tilePixelDecoder(self: *GPU, high: u8, low: u8, pixel_index: u3) Color {
        const shift: u3 = 7 - pixel_index;
        const hi = @as(u1, @truncate(high >> shift));
        const lo = @as(u1, @truncate(low >> shift));
        const color_code: u2 = (@as(u2, hi) << 1) | lo;
        const bgp = self.getSpecialRegister(.bgp); // get the right color pallete (dmg)
        const palette_color: u2 = @truncate(bgp >> @as(u3, @intCast(color_code)) * 2); // selecting color
        return @as(Color, @enumFromInt(palette_color));
    }
    fn pixelMixer(sprite_color: Color, bg_color: Color, bg_priority: bool) Color {
        if (sprite_color == .transparent) {
            return bg_color;
        }
        if (bg_priority and bg_color != .transparent) {
            return bg_color;
        }
        return sprite_color;
        // if (bg_priority and bg_color != .transparent) return bg_color;
    }
    fn spriteDecoder(self: *GPU) void {
        _ = self;
    }
    fn switchMode(self: *@This()) void {
        const ly = self.getSpecialRegister(.ly);
        self.mode = mode: switch (self.mode) {
            .SCAN => {
                self.mode_cycles_left = Mode.RENDER.min_cycles();
                break :mode .RENDER;
            },
            .RENDER => {
                self.lcd.pushScanline(self.scanline, ly);
                self.mode_cycles_left = Mode.HBLANK.min_cycles();
                break :mode .HBLANK;
            },
            .HBLANK => {
                // Increment LY register
                self.setSpecialRegister(.ly, ly + 1);
                if (ly + 1 == 144) { // send vblank interrupt
                    self.bus.handler.set(.flag, .vblank);
                    self.mode_cycles_left = Mode.VBLANK.min_cycles(); // per scanline
                    break :mode .VBLANK;
                } else {
                    self.mode_cycles_left = Mode.SCAN.min_cycles();
                    break :mode .SCAN;
                }
            },
            .VBLANK => {
                const new_ly = ly + 1;
                if (new_ly > 153) { // 153 is the end of VBLANK
                    // send the tilesheet data to the buffer for debug
                    var tile: u16 = 0;
                    while (tile < 384) : (tile += 1) {
                        const tiles_per_row = 24;
                        const tileX = tile % tiles_per_row;
                        const tileY = tile / tiles_per_row;
                        const dstY = tileY * 8;
                        const dstX = tileX * 8;

                        const base: u16 = 0x8000; // start of tile data
                        const tile_addr: u16 = base + tile * 16;
                        for (0..8) |row| {
                            for (0..8) |col| {
                                const pixel_index: u3 = @intCast(col);
                                const low = self.readVram(tile_addr + row * 2);
                                const high = self.readVram(tile_addr + row * 2 + 1);
                                const color = self.tilePixelDecoder(high, low, pixel_index);
                                // write to tilesheet buffer
                                const tilesheet_index = (dstY + row) * 192 + (dstX + col);
                                LCD.writeToBuf(&self.lcd.tilesheetBuf, color, tilesheet_index);
                            }
                        }
                    }
                    // send background tile data to lcd for debug
                    tile = 0;
                    while (tile < 1024) : (tile+=1) {
                        const tiles_per_row = 32;
                        const tileX = tile % tiles_per_row;
                        const tileY = tile / tiles_per_row;
                        const dstY = tileY * 8;
                        const dstX = tileX * 8;
                        const base = 0x9800;
                        const tile_no: u8 = self.readVram(base + tile);
                        const tile_addr: u16 = 0x8000 + @as(u16, @intCast(tile_no)) * 16;
                        for (0..8) |row| {
                            for (0..8) |col| {
                                const pixel_index: u3 = @intCast(col);
                                const low = self.readVram(tile_addr + row * 2);
                                const high = self.readVram(tile_addr + row * 2 + 1);
                                const color = self.tilePixelDecoder(high, low, pixel_index);
                                // write to tilesheet buffer
                                const tilesheet_index = (dstY + row) * 256 + (dstX + col);
                                LCD.writeToBuf(&self.lcd.bgBuf, color, tilesheet_index);
                            }
                        }
                    }
                    tile = 0;
                    // send oam tile data (sprites) to lcd for debug
                    while (tile < self.sprite_buffer.len) : (tile+=1) {
                        if (self.sprite_buffer[tile] == null) {
                            continue;
                        }
                        const tiles_per_row = 20;
                        const tileX = tile % tiles_per_row;
                        const tileY = tile / tiles_per_row;
                        const dstY = tileY * 8;
                        const dstX = tileX * 8;

                        // const sprite_index = tile * 4;
                        // const base = 0xFE00;
                        const tile_no = self.sprite_buffer[tile].?.tile_no;

                        const tile_addr: u16 = 0x8000 + @as(u16, @intCast(tile_no)) * 16;
                        for (0..8) |row| {
                            for (0..8) |col| {
                                const pixel_index: u3 = @intCast(col);
                                const low = self.readVram(tile_addr + row * 2);
                                const high = self.readVram(tile_addr + row * 2 + 1);
                                const color = self.tilePixelDecoder(high, low, pixel_index);
                                // write to tilesheet buffer
                                const tilesheet_index = (dstY + row) * 160 + (dstX + col);
                                LCD.writeToBuf(&self.lcd.spriteBuf, color, tilesheet_index);
                            }
                        }
                    }
                    tile = 0;
                    while (tile < 40) : (tile+=1) {
                        const tiles_per_row = 20;
                        const tileX = tile % tiles_per_row;
                        const tileY = tile / tiles_per_row;
                        const dstY = tileY * 8;
                        const dstX = tileX * 8;

                        const tile_no = self.readOAM(OAM_BEGIN + tile * 4 + 2);

                        const tile_addr: u16 = 0x8000 + @as(u16, @intCast(tile_no)) * 16;
                        for (0..8) |row| {
                            for (0..8) |col| {
                                const pixel_index: u3 = @intCast(col);
                                const low = self.readVram(tile_addr + row * 2);
                                const high = self.readVram(tile_addr + row * 2 + 1);
                                const color = self.tilePixelDecoder(high, low, pixel_index);
                                // write to tilesheet buffer
                                const tilesheet_index = (dstY + row) * 160 + (dstX + col);
                                LCD.writeToBuf(&self.lcd.oamBuf, color, tilesheet_index);
                            }
                        }
                    }
                    self.setSpecialRegister(.ly, 0); // reset LY to 0
                    self.mode_cycles_left = Mode.SCAN.min_cycles();
                    self.frames_cycled += 1;
                    break :mode .SCAN;
                } else {
                    self.setSpecialRegister(.ly, new_ly);
                    self.mode_cycles_left = Mode.VBLANK.min_cycles();
                    break:mode .VBLANK;
                }
            },
        };
        if (self.mode == .SCAN) { // we just started a new scanline, reset all per scanline variables
            self.fetcher.reset_state();
            self.sprite_buffer_i = 0;
            self.oam_i = 0;
            self.scx_discard = self.getSpecialRegister(.scx) % 8;
            self.scanline_fetched = 0;
            self.scanline_displayed = 0;
            self.sprites_used = 0;
            self.sprite_pixel_fifo.clearRetainingCapacity();
            self.bg_pixel_fifo.clearRetainingCapacity();
            self.reached_window = false;
            // print("we just started a new scanline, reset all per scanline variables. SCANLINE FETCHED = {d}\n", .{self.scanline_fetched});
        }

        // update the stat register after mode switch
        const lyc_check = self.getSpecialRegister(.ly) == self.getSpecialRegister(.lyc);
        var stat_reg = self.getSpecialRegister(.stat); // update STAT register
        stat_reg |= (@as(u3, @intFromBool(lyc_check)) << @intFromEnum(stat_bit.lyc_res)) | @intFromEnum(self.mode);
        self.setSpecialRegister(.stat, stat_reg);

        const mode_interrupt_bit: ?stat_bit = switch (self.mode) {
            .HBLANK => stat_bit.enable_mode0,
            .VBLANK => stat_bit.enable_mode1,
            .SCAN => stat_bit.enable_mode2,
            .RENDER => null
        };
        const line_was_set = set: {
            if (mode_interrupt_bit != null and interrupt_is_enabled(self.stat_reg, mode_interrupt_bit.?)) {
                break :set self.set_interrupt_line();
            }
            if (lyc_check and interrupt_is_enabled(self.stat_reg, .enable_lyc_check)) {
                break :set self.set_interrupt_line();
            }
            break :set false;
        };
        if (line_was_set) { // send lcd interrupt
            // print("lcd interrupt\n", .{});
            self.bus.handler.set(.flag, .lcd);
        }
    }

    const stat_bit = enum(u3) {
        // read-only
        ppu_mode = 0, // 2 wide
        lyc_res = 2,
        // read and write -- interrupts
        enable_mode0 = 3,
        enable_mode1 = 4,
        enable_mode2 = 5,
        enable_lyc_check = 6
    };
    fn set_interrupt_line(self: *GPU) bool { // TODO clean this up
        if (!self.interrupt_pending) {
            self.interrupt_pending = true;
            return true;
        }
        return false;
    }
    fn interrupt_is_enabled(stat: u8, bit: stat_bit) bool {
        return (stat >> @intFromEnum(bit) & 1) == 1;
    }
    // memory ops
    pub fn readVram(self: *@This(), address: usize) u8 {
        // print("address: 0x{X}", .{address});
        const fixed_address = address - VRAM_BEGIN;
        return self.vram[fixed_address];
    }
    pub fn writeVram(self: *@This(), address: usize, value: u8) void {
        if (!(self.testSpecialRegister(.lcdc, 7) and self.mode == .RENDER)) {
            const fixed_address = address - VRAM_BEGIN;
            self.vram[fixed_address] = value;
        }
    }
    pub const special_register = enum {
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
        pub const end = 0xFF4B;
        pub const start = 0xFF40;
        const size = 0xFF4B - 0xFF40 + 1;
    };
    pub fn setSpecialRegister(self: *GPU, register: special_register, value: u8) void {
        self.special_registers[@intFromEnum(register)] = value;
    }
    pub fn getSpecialRegister(self: *GPU, register: special_register) u8 {
        return self.special_registers[@intFromEnum(register)];
    }
    pub fn testSpecialRegister(self: *GPU, register: special_register, bit: u3) bool {
        return @as(u1, @truncate(self.special_registers[@intFromEnum(register)] >> bit)) == 1;
    }
    pub fn readOAM(self: *GPU, address: usize) u8 {
        if (self.mode == .RENDER or self.mode == .SCAN) {
            print("cannot access oam now\n", .{});
            return 0xF;
        }
        // if (address <= 0xFE00 or address >= 0xFE9F) return error.OutOfOAMBounds;
        const fixed_address = address - 0xFE00;
        return self.oam[fixed_address];
    }
    pub fn writeOAM(self: *GPU, address: usize, value: u8) void {
        if (self.mode == .RENDER or self.mode == .SCAN) {
            print("cannot access oam now\n", .{});
            return;
        }
        // if (address <= 0xFE00 or address >= 0xFE9F) return error.OutOfOAMBounds;
        const fixed_address = address - 0xFE00;
        self.oam[fixed_address] = value;
    }
    // mem dump
    fn vram_dump(self: *GPU) void {
        print("VRAM dump: \n", .{});
        for (self.vram, 0..VRAM_SIZE) |value, i| {
            const global_address = i + VRAM_BEGIN;
            if (global_address == 0x8000) print("\nentering vram\n", .{}) else if (global_address == 0x9800) print("\nentering tilemap one\n", .{}) else if (global_address == 0x9C00) print("\nentering tilemap two\n", .{});
            print("@0x{X}[", .{global_address});
            print("0x{x}]\t", .{value});
            if (i != 0 and i % 12 == 0) print("\n", .{});
        }
        print("\n", .{});
    }
    pub fn spec_register_dump(self: *GPU) void {
        print("Special Registers:\n", .{});
        for (std.enums.values(special_register), self.special_registers[0..]) |reg, value| {
            print("{any}[ 0x{x} ]\n", .{ reg, value });
        }
        print("\n", .{});
    }
};

///Contains the fields necessary to create a display,
///- Screen, Height, Width, Rendering
pub const LCD = struct {
    screenBuf: [screenHeightPx * screenWidthPx]u32 = undefined,
    tilesheetBuf: [192 * 128]u32 = undefined,
    bgBuf: [256 * 256]u32 = undefined,
    spriteBuf: [8 * 5 * 64]u32 = undefined,
    oamBuf: [8 * 5 * 64]u32 = undefined,
    renderer: *g.SDL_Renderer = undefined,
    screen_texture: *g.SDL_Texture = undefined,
    bg_texture: *g.SDL_Texture = undefined,
    tilesheet_texture: *g.SDL_Texture = undefined,
    oam_texture: *g.SDL_Texture = undefined,
    sprites_texture: *g.SDL_Texture = undefined,
    // text_surface: *g.SDL_Surface = undefined,
    text_texture: *g.SDL_Texture = undefined,
    font: *g.TTF_Font = undefined,
    win: *g.SDL_Window = undefined,
    // debug_win: *g.SDL_Window = undefined,
    grid_pixel_sz: u16 = undefined,
    allocator: std.mem.Allocator = undefined,
    root_path: []const u8 = undefined,

    fn initPixelBuffer(buf: []u32) void {
        for (0..buf.len) |i| {
            const color = GB.prng.random().enumValue(GPU.Color);
            writeToBuf(buf, color, i);
        }
    }
    const texture_buffer = enum(u16) { // enum value denotes the width of the buffer, as it is flat 2-dim
        screen = 160,
        tilesheet = 192,
        bg = 256,
        oam = 160,

        fn len(self: *texture_buffer) u16 {
            return @intFromEnum(self.*);
        }
    };
    // startup
    fn init(self: *@This(), allocator: std.mem.Allocator, root_path: []const u8) !void {
        initPixelBuffer(&self.screenBuf);
        initPixelBuffer(&self.tilesheetBuf);
        initPixelBuffer(&self.bgBuf);
        initPixelBuffer(&self.oamBuf);
        initPixelBuffer(&self.spriteBuf);
        self.allocator = allocator;
        self.root_path = root_path;
        try self.startAndCreateRendererAndTextures(); // set window and renderer and initialize textures
    }
    fn startAndCreateRendererAndTextures(self: *@This()) !void {
        if (!g.SDL_Init(g.SDL_INIT_VIDEO)) {
            print("SDL_Init failed: {s}\n", .{g.SDL_GetError()});
            return error.InitializationFailed;
        }
        var win: ?*g.SDL_Window = null;
        var renderer: ?*g.SDL_Renderer = null;
        if (!g.SDL_CreateWindowAndRenderer("zi(g)ameboy!", initWinW, initWinH, 0, &win, &renderer)) {
            print("Failed to create window or renderer: {s}\n", .{g.SDL_GetError()});
            return error.CreationFailure;
        }
        if (win == null) {
            print("Failed to create window: {s}\n", .{g.SDL_GetError()});
            return error.WindowNull;
        }
        if (renderer == null) {
            print("Failed to create renderer: {s}\n", .{g.SDL_GetError()});
            return error.RendererFailure;
        }

        self.renderer = renderer.?;
        self.win = win.?;
        _ = g.SDL_SetWindowResizable(self.win, true);
        // _ = g.SDL_SetWindowFullscreenMode(self.win, );
        _ = g.SDL_SetWindowMinimumSize(self.win, initWinW, initWinH);
        if (!g.SDL_GetWindowSizeInPixels(self.win, &window_width, &window_height)) {
            print("err while getting window size: {s}\n", .{g.SDL_GetError()});
            return error.NoWinSize;
        } else {
            print("window size: {d}x{d}\n", .{ window_width, window_height });
            if (window_height == 0 or window_width == 0) {
                return error.DetectedZeroWidthWin;
            }
        }
        // Font & Text init
        if (!g.TTF_Init()) {
            print("TTF Init failed", .{});
            return error.TTF_Init;
        }

        const font_path = try std.fs.path.join(self.allocator, &.{ self.root_path, "assets", "fonts", "Minecraft.ttf" });
        defer self.allocator.free(font_path);
        const font = g.TTF_OpenFont(font_path.ptr, 18);
        self.font = font orelse {
            print("Font Loading Error", .{});
            return error.FontInit;
        };
        // creating textures
        self.screen_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_STREAMING, 160, 144);
        _ = g.SDL_SetTextureScaleMode(self.screen_texture, g.SDL_SCALEMODE_NEAREST);
        self.bg_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, 256, 256);
        _ = g.SDL_SetTextureScaleMode(self.bg_texture, g.SDL_SCALEMODE_NEAREST);
        self.tilesheet_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, 192, 128);
        _ = g.SDL_SetTextureScaleMode(self.tilesheet_texture, g.SDL_SCALEMODE_NEAREST);
        self.oam_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, 160, 16);
        _ = g.SDL_SetTextureScaleMode(self.oam_texture, g.SDL_SCALEMODE_NEAREST);
        self.sprites_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, 160, 16);
        _ = g.SDL_SetTextureScaleMode(self.sprites_texture, g.SDL_SCALEMODE_NEAREST);
        // self.createBG();
        self.bootScreen(); // wait for sdl to finish building the window to begin progression
    }
    fn bootScreen(self: *LCD) void {
        self.renderAll("GAMEBOY\n"); // Draw a dummy frame to force the window to initialize and show
        _ = g.SDL_PumpEvents(); // Let the OS process events and show the window
        var window_ready = false;
        var event: g.SDL_Event = undefined;
        const timeout_ns = 500 * std.time.ns_per_ms;
        const start = Clock.Now();
        while (!window_ready and (Clock.Now() - start < timeout_ns)) { // fallback
            while (g.SDL_PollEvent(&event)) {
            if (event.type == g.SDL_EVENT_WINDOW_SHOWN) {
                window_ready = true;
            }
        }
            std.Thread.sleep(1 * std.time.ns_per_ms);
        }
        std.Thread.sleep(17 * std.time.ns_per_ms);
    }
    var palette_index: u8 = 0;
    const Palette = [4]u32;
    const palettes = [_]Palette {
        .{ // b/w
            0xFF_FF_FF_FF,
            0xFF_AA_AA_AA,
            0xFF_55_55_55,
            0xFF_00_00_00,
        },
        .{ // green
            0xFF_CC_FF_CC,
            0xFF_99_CC_99,
            0xFF_66_99_66,
            0xFF_33_66_33
        },
        .{ // pink
            0xFF_FF_CC_CC,
            0xFF_CC_99_99,
            0xFF_99_66_66,
            0xFF_66_33_33
        },
        .{ // blue
            0xFF_CC_CC_FF,
            0xFF_99_99_CC,
            0xFF_66_66_99,
            0xFF_33_33_66
        },
        .{ // sand
            0xFF_FB_EF_E8,
            0xFF_F2_B4_9C,
            0xFF_C3_7A_60,
            0xFF_6A_5C_41
        },
        .{ // forest
            0xFF_A4_F3_97,
            0xFF_88_75_4E,
            0xFF_60_51_3A,
            0xFF_22_2B_22
        },
        .{ // fairy
            0xFF_FF_D2_D4,
            0xFF_F8_D1_A5,
            0xFF_B7_B1_F6,
            0xFF_F5_83_9A
        },
        .{ // funk
            0xFF_FF_F8_61,
            0xFF_FF_79_C6,
            0xFF_B1_4B_F0,
            0xFF_6A_00_F4
        },
        .{ // ocean
            0xFF_A0_FF_FF,
            0xFF_4D_CB_FF,
            0xFF_00_89_C0,
            0xFF_00_3F_63
        },
        .{ // sunset
            0xFF_FF_C1_47,
            0xFF_FF_70_67,
            0xFF_C0_3A_6B,
            0xFF_4C_1A_45
        },
        .{ // moss and ember
            0xFF_D9_EC_D2,
            0xFF_9C_CB_7A,
            0xFF_75_54_2E,
            0xFF_A6_38_1F
        },
        .{ // bubblegum
            0xFF_FF_E6_F7,
            0xFF_FF_AE_EC,
            0xFF_D8_7C_F4,
            0xFF_53_2D_8F
        },
        .{ // frostbyte
            0xFF_E0_FF_FB,
            0xFF_8A_FF_F1,
            0xFF_30_C9_CF,
            0xFF_0F_3E_4F
        },
    };
    inline fn getCurrentPalette() Palette {
        return palettes[palette_index];
    }
    pub inline fn nextPalette() void {
        palette_index = (palette_index + 1) % @as(u8, palettes.len);
    }
    // peripheral fns
    inline fn writeToBuf(buf: []u32, color: GPU.Color, index: usize) void {
        buf[index] = getCurrentPalette()[@intFromEnum(color)];
    }
    fn pushScanline(self: *@This(), new_scanline: [screenWidthPx]GPU.Color, ly: u8) void {
        for (0..screenWidthPx) |x| {
            writeToBuf(&self.screenBuf, new_scanline[x], @as(u32, @intCast(ly)) * screenWidthPx + x);
        }
    }
    // drawing
    fn setRect(self: *@This(), rect: *g.SDL_FRect, x: anytype, y: anytype, w: anytype, h: anytype) void {
        _ = self;
        rect.x = if (@TypeOf(x) == f32) x else @as(f32, @floatFromInt(x));
        rect.y = if (@TypeOf(y) == f32) y else @as(f32, @floatFromInt(y));
        rect.w = if (@TypeOf(w) == f32) w else @as(f32, @floatFromInt(w));
        rect.h = if (@TypeOf(h) == f32) h else @as(f32, @floatFromInt(h));
    }
    // fn createBG(self: *LCD) void {
    //     _ = g.SDL_SetRenderTarget(self.renderer, self.bg_texture);
    //     defer _ = g.SDL_SetRenderTarget(self.renderer, null);
    //     _ = g.SDL_SetRenderDrawColor(self.renderer, 255, 192, 220, 255);
    //     _ = g.SDL_RenderClear(self.renderer);
    // }
    pub fn renderAll(self: *@This(), debug: []const u8) void {
        _ = g.SDL_RenderClear(self.renderer);
        _ = g.SDL_UpdateTexture(self.screen_texture, null, &self.screenBuf, 160 * @sizeOf(u32));
        _ = g.SDL_UpdateTexture(self.tilesheet_texture, null, &self.tilesheetBuf, 192 * @sizeOf(u32));
        _ = g.SDL_UpdateTexture(self.bg_texture, null, &self.bgBuf, 256 * @sizeOf(u32));
        _ = g.SDL_UpdateTexture(self.oam_texture, null, &self.oamBuf, 160 * @sizeOf(u32));
        _ = g.SDL_UpdateTexture(self.sprites_texture, null, &self.spriteBuf, 160 * @sizeOf(u32));
        // const dgray = g.SDL_Color{ .r = 36, .g = 36, .b = 36, .a = 255 };
        _ = debug;
        // if (debug.len > 0) {
        //     self.text_surface = g.TTF_RenderText_Solid_Wrapped(self.font, debug.ptr, debug.len, dgray, 0);
        //     self.text_texture = g.SDL_CreateTextureFromSurface(self.renderer, self.text_surface);
        // }
        // defer g.SDL_DestroySurface(self.text_surface);
        // defer g.SDL_DestroyTexture(self.text_texture);
        // _ = g.SDL_RenderClear(self.renderer);
        const gb_screen_w = screenWidthPx * pxSize;
        const gb_screen_h = screenHeightPx * pxSize;
        const screen_rect = g.SDL_FRect{
            .x = 0,
            // .x = @as(f32, @floatFromInt(center)) - screenWidthPx / 2 * pxSize,
            .y = border / 2,
            .w = gb_screen_w,
            .h = gb_screen_h
        };
        // const bg_width: f32 = @as(f32, @floatFromInt(window_width)) - gb_screen_w;
        // const bg_rect = g.SDL_FRect{
        //     .x = gb_screen_w + 10,
        //     .y = 0,
        //     .w = bg_width,
        //     .h = @as(f32, @floatFromInt(window_height)),
        // };
        // const text_rect = g.SDL_FRect{
        //     .x = bg_x + 15,
        //     .y = 30,
        //     .w = @as(f32, @floatFromInt(self.text_surface.w)),
        //     .h = @as(f32, @floatFromInt(self.text_surface.h))
        // };
        //

        const background_len = gb_screen_h / 2; // background is a square
        const background_map_rect = g.SDL_FRect{
            .x = gb_screen_w,
            .y = 0,
            .w = background_len,
            .h = background_len
        };

        const tilesheet_h = gb_screen_h / 2;
        const tilesheet_h_px = 128;
        const tilesheet_w_px = 192;
        const tilesheet_w = tilesheet_w_px * tilesheet_h / tilesheet_h_px;
        const tilesheet_rect = g.SDL_FRect{
            .x = gb_screen_w + background_len,
            .y = 0,
            .w = tilesheet_w,
            .h = tilesheet_h,
        };
        //
        const oam_w = @as(f32, @floatFromInt(window_width)) - gb_screen_w;
        const oam_h_px = 16;
        const oam_w_px = 160;
        const oam_h = oam_h_px * oam_w / oam_w_px;
        const oam_rect = g.SDL_FRect{
            .x = gb_screen_w,
            .y = background_len,
            .w = oam_w,
            .h = oam_h
        };
        const sprites_rect = g.SDL_FRect{
            .x = gb_screen_w,
            .y = background_len + oam_h,
            .w = oam_w,
            .h = oam_h
        };
        // _ = bg_rect;
        // _ = screen_rect;

        // _ = g.SDL_RenderTexture(self.renderer, self.text_texture, null, &text_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.screen_texture, null, &screen_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.tilesheet_texture, null, &tilesheet_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.bg_texture, null, &background_map_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.oam_texture, null, &oam_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.sprites_texture, null, &sprites_rect);
        _ = g.SDL_RenderPresent(self.renderer);
    }

    // mem dump
    pub fn screen_dump(self: *@This()) void {
        print("Actual memspace dump: \n", .{});
        for (self.screenBuf) |pixel| {
            print("{any}", .{@as(GPU.Color, @enumFromInt(pixel))});
        }
        print("\n", .{});
    }
    // end
    pub fn endSDL(self: *@This()) void {
        g.SDL_DestroyWindow(self.win);
        g.SDL_DestroyRenderer(self.renderer);
        g.SDL_DestroyTexture(self.bg_texture);
        g.SDL_DestroyTexture(self.screen_texture);
        g.SDL_DestroyTexture(self.tilesheet_texture);
        g.SDL_DestroyTexture(self.text_texture);
        g.SDL_DestroyTexture(self.oam_texture);
        // g.SDL_DestroySurface(self.text_surface);
        g.TTF_CloseFont(self.font);
        g.SDL_Quit();
        g.TTF_Quit();
    }
    pub fn toggleFullscreen(self: *LCD) void {
        _ = g.SDL_SetWindowFullscreen(self.win, !is_fullscreen);
        is_fullscreen = !is_fullscreen;
        // if (win_flags & g.SDL_WINDOW_FULLSCREEN != g.SDL_WINDOW_FULLSCREEN) {
        //     g.SDL_SetWindowFullscreen(self.win, true);
        // } else {
        //     g.SDL_SetWindowFullscreen(self.win, false);
        // }
    }
    pub fn resize(self: *LCD) void {
        // window_height = height;
        // window_width = width;
        _ = g.SDL_GetWindowSizeInPixels(self.win, &window_width, &window_height);
        center = @intCast(@divFloor(window_width, 2));
        pxSize = switch (window_height > window_width) {
            true => @as(f32, @floatFromInt(window_width - border)) / screenWidthPx,
            false => @as(f32, @floatFromInt(window_height - border)) / screenHeightPx,
        };

    }
    const initWinW: u32 = initWinH * screenWidthPx / screenHeightPx;
    const initWinH: u32 = 500;
    const border = 0;
    const screenWidthPx = 160;
    const screenHeightPx = 144;
    var window_height: c_int = @intCast(initWinH);
    var window_width: c_int = @intCast(initWinW);
    var center: u16 = initWinW / 2;
    var pxSize: f32 = @as(f32, @floatFromInt(initWinH - border)) / screenHeightPx;
    var is_fullscreen = false;
};
const GB = @import("gb.zig");
const Clock = GB.Clock;
const Bus = GB.Bus;
const std = @import("std");
const print = std.debug.print;
pub const g = @cImport({
    @cDefine("SDL_DISABLE_OLD_NAMES", {});
    @cInclude("SDL3/SDL.h");
    @cInclude("SDL3/SDL_revision.h");
    @cDefine("SDL_MAIN_HANDLED", {});
    @cInclude("SDL3/SDL_main.h");
    @cInclude("SDL3_ttf/SDL_ttf.h");
});

fn BIT(bit: u6, int: usize) u1 {
    return @truncate(int >> bit);
}