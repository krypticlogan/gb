/// Defines a Game Boy GPU(PPU)
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

    const Mode = enum { // 4 modes that the PPU cycles through
        HBLANK,
        VBLANK,
        SCAN,
        RENDER,
        fn max_cycles(self: Mode) u16 {
            return switch (self) {
                .HBLANK => 204,
                .VBLANK => 456,
                .SCAN => 80,
                .RENDER => 289
            };
        }
    };

    const Color = enum(u2) { transparent, dgray, lgray, white };
    const Sprite = struct { // A sprite object
        y: u8 = 0,
        x: u8 = 0,
        tile_no: u8 = 0,
        flags: Flags, // constructed from bitfield in detect_sprite_fetch through the inspect_flags(flags: u8) method
        const Flags = struct {bg_priority: bool, y_flip: bool, x_flip: bool, palette: bool};    
        fn inspect_flags(flags: u8) Flags {
            return .{
                // Bit 7    OBJ-to-BG Priority
                // 0 = Sprite is always rendered above background
                // 1 = Background colors 1-3 overlay sprite, sprite is still rendered above color 0
                .bg_priority = BIT(7, flags) == 1,
                // Bit 6    Y-Flip
                // If set to 1 the sprite is flipped vertically, otherwise rendered as normal
                .y_flip = BIT(6, flags) ==  1,
                // Bit 5    X-Flip
                // If set to 1 the sprite is flipped horizontally, otherwise rendered as normal
                .x_flip = BIT(5, flags) ==  1,
                // Bit 4    Palette Number
                // If set to 0, the OBP0 register is used as the palette, otherwise OBP1
                .palette = BIT(4,flags) ==  1,
            };
        }
        fn comparePriority(context: void, lhs: ?Sprite, rhs: ?Sprite) bool {
            _ = context;
            if (rhs == null or lhs == null) {
                return lhs != null;
            }
            const lhs_x = lhs.?.x;
            const rhs_x = rhs.?.x;
            if (lhs_x == rhs_x) return true; // preserve order for equal sprites
            // if the sprites occupy the same space, put the one with the lower x value after the one with higher
            // we occupy the same space iff the abs(lhs - rhs) is less than 8
            // const same_space = @abs(@as(i16, lhs_x) - rhs_x) < 8;
            // if (same_space) return lhs_x > rhs_x;
            return lhs_x < rhs_x; // lhs should come before rhs (they do not occupy the same space)
        }
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
    sprite_pixel_fifo: std.ArrayList(struct{Color, bool}) = undefined,
    sprite_pixel_fifo_buffer: [16]struct{Color, bool} = undefined,
    bg_pixel_fifo: std.ArrayList(Color) = undefined,
    bg_pixel_fifo_buffer: [16]Color = undefined,

    // STATE
    mode: Mode = undefined,
    mode_cycles: u16 = 0,
    mode_cycles_spent: u16 = 0,
    stat_reg: u8 = undefined,
    interrupt_pending: bool = false,
    scanline: [LCD.screenWidthPx]Color = undefined,
    scanline_displayed: u8 = 0,
    scx_discard: u8 = 0,
    frames_cycled: usize = 0,
    frame_cycles_spent: u64 = 0,
    stall: u8 = 0,

    // Peripherals
    lcd: LCD = undefined,
    /// Combined BG/Sprite pixel fetcher
    fetcher: pixel_fetcher = undefined,

    // startup
    pub fn init(self: *@This(), gb: *GB) !void {
        self.vram = gb.bus.memory[VRAM_BEGIN .. VRAM_END + 1];
        self.oam = gb.bus.memory[OAM_BEGIN .. OAM_END + 1];
        self.bus = &gb.bus;
        self.special_registers = gb.bus.memory[special_register.start .. special_register.end + 1];
        self.mode = .SCAN;
        self.mode_cycles = self.mode.max_cycles();
        self.sprite_pixel_fifo = std.ArrayList(struct{Color, bool}).initBuffer(&self.sprite_pixel_fifo_buffer);
        self.bg_pixel_fifo = std.ArrayList(Color).initBuffer(&self.bg_pixel_fifo_buffer);
        @memset(&self.scanline, GB.prng.random().enumValue(Color));
        try self.lcd.init(gb.allocator, gb.root_path);
        self.fetcher.init(self);
    }
    /// gpu execution
    pub fn tick(self: *@This(), cycles: u16) void {
        // the gpu should tick/cycle just as many
        // times as the cpu did, while being able to
        // process interrupts and continue on as well as changing modes mid-scanline when needed
        var cycles_left = cycles; // amt of cycles spent by cpu
        self.frame_cycles_spent += cycles_left;

        while (cycles_left > 0) {
            switch (self.mode) {
                .RENDER => {
                    if (self.scanline_displayed == self.scanline.len) {
                        self.switchMode();
                    }
                    else if (self.mode_cycles_spent >= self.mode_cycles) {
                        self.switchMode();
                    }
                },
                else => {
                    // print("switching mode {any}\n", .{self.mode});
                    if (self.mode_cycles_spent >= self.mode_cycles) self.switchMode(); // handles drawing the screen, updating ly
                }
            }

            const cycles_to_process: u16 = @intCast(@min(cycles_left, self.mode_cycles));
            // print("processing cycles\n", .{});
            self.do(cycles_to_process);
            cycles_left -= cycles_to_process;
        }
    }
    fn do(self: *@This(), cycles: u16) void {
        // Operate GPU here
        // // const zone = tracy.beginZone(@src(), .{ .name = "DO GPU CYCLES" });
        // // defer zone.end();
        self.mode_cycles_spent += cycles;
        var cycles_spent: u16 = 0;
        switch (self.mode) {
            .SCAN => { // 2 searches OAM memory for sprites that should be rendered on the current scanline and stores them in a buffer
                while (self.oam_i < OAM_SIZE and cycles_spent < cycles) : (self.oam_i+=4) {
                    // this needs to sort/add sprites to the buffer by x value
                    // using pdq sort
                    cycles_spent += 2;
                    if (self.sprite_buffer_i == 10) continue;
                    const ly = self.getSpecialRegister(.ly);
                    const y_pos = self.oam[self.oam_i];
                    const x_pos = self.oam[self.oam_i+1];
                    var tile_no = self.oam[self.oam_i+2];
                    const flags = self.oam[self.oam_i+3];
                    const height: u8 = h: switch (self.check_lcdc(.sprite_size)) {
                        true => {
                            tile_no &= 0xFE; // ignore the last bit for tall sprites
                            break :h 16;
                        },
                        false => 8
                    };
                    const horizontally_visible = x_pos > 0;
                    const vertically_visible =  y_pos <= ly + 16 and y_pos + height > ly + 16; // Within our current scanline?

                    if (horizontally_visible and vertically_visible) {
                        const sprite = Sprite { .y = y_pos, .x = x_pos, .tile_no = tile_no, .flags = Sprite.inspect_flags(flags) };
                        print("putting sprite into sprite buffer: {any}\nRead from oam mem[0x{X} .. 0x{X} + 4]\n", .{sprite, self.oam_i, self.oam_i});
                        self.sprite_buffer[self.sprite_buffer_i] = sprite;
                        self.sprite_buffer_i += 1;
                    }
                }
                if (self.oam_i == self.oam.len) {
                    std.sort.insertion(?Sprite, self.sprite_buffer[0..], {}, Sprite.comparePriority);
                    print("Sorted sprite data\n==============\n", .{});
                    for (self.sprite_buffer) |sprite| {
                        print("{any}\n", .{sprite});
                    }
                    // self.bus.cpu.break_exe();
                }
                // return;
            },
            .RENDER => { // 3 transfers pixels to the LCD, one scanline at a time, duration variable
                // TODO: Generate the actual pixels for this scanline based on:
                // - Background tiles at the current scroll position
                // - Window tiles if enabled and visible on this line
                // - Sprites that were found during OAM scan
                if (!self.check_lcdc(.lcd_enable)) return;

                while (self.scanline_displayed < self.scanline.len and cycles_spent < cycles) exit: {
                    if (self.scx_discard > 0) {
                        // print("discarding, progress: {d}, fetched: {d}\n", .{self.scanline_fetched, self.scanline_displayed});
                        if (self.bg_pixel_fifo.items.len > 0)
                            _ = self.bg_pixel_fifo.orderedRemove(0);
                        self.scx_discard -= 1;
                        self.fetcher.pixels_fetched += 1;
                        cycles_spent += 1;
                        continue;
                    }

                    const fetcher_cycles_spent = self.fetcher.step_forward(); // returns 2 if it successfully completed a step, 1 if it is waiting to push out and the bg fifo is not empty
                    var push_cycles_left = fetcher_cycles_spent;
                    // push pixels to scanline at one cycle per pixel
                    const fetching_sprite = self.fetcher.active_fetcher == .sprite; // if we are fetching a sprite, we should wait until it is done
                    while (!fetching_sprite and push_cycles_left > 0) : (push_cycles_left -= 1) { // shift pixels out to the buffer
                        if (self.bus.cpu.booted) {
                            if (self.fetcher.detect_window_reached()) {
                                break;
                            }
                            if (self.fetcher.detect_sprite_fetch()) {
                                break;
                            }
                        }
                        if (self.bg_pixel_fifo.items.len < 8) break;
                        if (self.bus.cpu.booted) print("shifting out pixel {d} | ", .{self.scanline_displayed});
                        const bg_pixel= self.bg_pixel_fifo.orderedRemove(0); // TODO change this to a different structure for 0(1)
                        if (self.bus.cpu.booted) print("bg color {any} | ", .{bg_pixel});

                        var sprite_pixel: struct{Color, bool} = undefined;
                        if (self.sprite_pixel_fifo.items.len >= 1) {
                            sprite_pixel = self.sprite_pixel_fifo.orderedRemove(0);
                        } else sprite_pixel = .{.transparent, false};
                        if (self.bus.cpu.booted) print("sprite color {any} | bg priority? {any}\n ", .{sprite_pixel[0], sprite_pixel[1]});

                        self.scanline[self.scanline_displayed] = pixelMixer(sprite_pixel, bg_pixel);
                        // print("scanline px color: {any} (pushed {d} pixels)\n", .{self.scanline[self.scanline_displayed], self.scanline_displayed});
                        self.scanline_displayed += 1;
                        if (self.scanline_displayed == self.scanline.len) {
                            break :exit;
                        }
                    }
                    // print("Fetcher cycles: {d}, pushing cycles: {d}", .{fetcher_cycles_spent, pushing_cycles_spent});
                    // if (fetcher_cycles_spent != pushing_cycles_spent) @panic("Mismatch between fetcher cycles and pushing cycles");
                    cycles_spent += fetcher_cycles_spent;

                    // print("cycles spent: {d} | cycles to spend: {d}\n", .{cycles_spent, cycles});
                }
                if (cycles_spent < cycles) print("lost cycles: {d}", .{cycles - cycles_spent});
                if (self.mode == .SCAN) print("cycles spent: {d}", .{self.mode_cycles_spent});
            },
            else => return, // no action for hblank or vblank
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
        render_window: bool = false,
        window_reached: bool = false,
        sprite_data: ?Sprite = null,

        pixels_fetched: u8 = 0, // counter
        window_line_counter: u8 = 0,
        window_x: u8 = 0,


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
            self.sprite_fetcher.reset();
            self.win_fetcher.reset();
            self.bg_fetcher.reset();
            self.active_fetcher = if (!self.render_window) .bg else .win;
        }
        fn step_forward(self: *pixel_fetcher) u8 {
            // print("step {d}: {any}\n", .{@intFromEnum(self.state), self.state});
            var state = self.active_fetcher_state();
            return cycles: switch (state.step) {
                .tile_no => {
                    // file tile position
                    const ly: u16 = self.gpu.getSpecialRegister(.ly);

                    // debug
                        if (self.active_fetcher == .sprite) {
                            const x = self.pixels_fetched;
                            self.gpu.sprites_used += 1;
                            print("\nLY: {d}, X: {d} | sprites used? {d}\n", .{ly, x, self.gpu.sprites_used});
                            // print("sprite buffer: {any}", .{self.gpu.sprite_buffer});
                        }

                    // only scroll on bg
                    const scx: u16 = if (self.active_fetcher == .bg) self.gpu.getSpecialRegister(.scx) else 0;
                    const scy: u16 = if (self.active_fetcher == .bg) self.gpu.getSpecialRegister(.scy) else 0;

                    state.tile_addr = addr: switch (self.active_fetcher) {
                        .bg, .win => { // bg / window tile addr
                            const use_signed = !self.gpu.check_lcdc(.tile_data_sel);
                            const base: i32 = if (use_signed) 0x9000 else 0x8000;

                            var tilemap_base: u16 = undefined; // check lcdc bit 3 for bg, bit 6 for win}
                            
                            const bg_y: u8 = y: switch (self.active_fetcher) {
                                .bg => {
                                    tilemap_base = if (self.gpu.check_lcdc(.bg_tilemap)) 0x9C00 else 0x9800;
                                    break: y @intCast((ly + scy) & 0xFF); // wraps at 256
                                },
                                .win => {
                                    tilemap_base = if (self.gpu.check_lcdc(.window_tilemap)) 0x9C00 else 0x9800;
                                    break: y self.window_line_counter;
                                },
                                else => unreachable
                            };
        
                            const bg_x: u8 = switch (self.active_fetcher) {
                                .bg => @intCast((self.pixels_fetched + scx) & 0xFF),
                                .win => self.window_x,
                                else => unreachable
                            };

                            // fetch bg pixel (maybe window tile)
                            const tile_y: u8 = bg_y / 8;
                            const tile_x: u8 = bg_x / 8;

                            const tile_index_addr: u16 = tilemap_base + @as(u16, tile_y) * 32 + tile_x;

                            const tile_index: i16 = if (use_signed) @intCast(@as(i8, @bitCast(self.gpu.readVram(tile_index_addr)))) else @intCast(self.gpu.readVram(tile_index_addr));
                            const tile_line: u16 = bg_y % 8;

                            break :addr @intCast(base + tile_index * 16 + @as(i32, @intCast(tile_line * 2)));
                            // print("LY={d}, scanline_fetched={d}, bg_x={d}, tile_x={d}\n", .{ly, self.gpu.scanline_fetched, bg_x, tile_x});
                        },
                        .sprite => { // sprite tile addr
                            const is_tall = self.gpu.check_lcdc(.sprite_size);
                            print("Beginning rendering {s} sprite: {any}\n", .{if (is_tall) "tall" else "not tall", self.sprite_data.?});
                            const height: u8 = if (is_tall) 16 else 8;
                            var sprite_tile_line: u16 = (ly -% self.sprite_data.?.y -% 16) % height;
                            if (self.sprite_data.?.flags.y_flip) sprite_tile_line = (height - 1) - sprite_tile_line;
                            break :addr 0x8000 + @as(u16, @intCast(self.sprite_data.?.tile_no + (sprite_tile_line >> 3)))  * 16 + sprite_tile_line * 2;
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
                    const can_push: bool = switch(self.active_fetcher) {
                        .bg, .win => self.gpu.bg_pixel_fifo.items.len < 8,
                        .sprite => self.gpu.sprite_pixel_fifo.items.len < 8,
                    };
                    if (can_push) {
                        const rendering_sprites = self.active_fetcher == .sprite;
                        // we only push a new set of pixels in the instant that the background is
                        // const pixel_i: u3 = @intCast(self.pixels_fetched % 8); //
                        var pixel_i: u4  = 0;
                        while (pixel_i < 8) : (pixel_i += 1) {
                            if (self.gpu.bus.cpu.booted) { // debug
                                print("Pushing pixel {d} (tile index {d}) | Rendering sprites? {any}, BG/WIN enabled? {any}\n", .{if (!self.render_window) self.pixels_fetched else self.window_x, pixel_i, rendering_sprites, self.gpu.check_lcdc(.bg_win_enable)});
                            }
                            const pixel = color: {
                                if (rendering_sprites) {
                                    if (self.gpu.check_lcdc(.sprite_enable)) {
                                        print("sprite data: {any}\n", .{self.sprite_data});
                                        const palette: special_register = if (self.sprite_data.?.flags.palette) .obp1 else .obp0;
                                        // const tall = self.sprite_data.?.flags.

                                        break: color self.gpu.tilePixelDecoder(
                                            state.tile_high.?,
                                            state.tile_low.?,
                                            @intCast(pixel_i),
                                            self.sprite_data.?.flags,
                                            palette
                                        );
                                    } else break: color .transparent;
                                }

                                if (self.gpu.check_lcdc(.bg_win_enable)) {
                                    break: color self.gpu.tilePixelDecoder(
                                        state.tile_high.?,
                                        state.tile_low.?,
                                        @intCast(pixel_i),
                                        .{ .bg_priority = false, .x_flip = false, .y_flip = false, .palette = false},
                                        .bgp
                                    );
                                } else break: color .transparent;
                            };

                            switch (self.active_fetcher) {
                                .bg, .win => self.gpu.bg_pixel_fifo.appendAssumeCapacity(pixel),
                                .sprite => {
                                    if (self.gpu.sprite_pixel_fifo.items.len > pixel_i) {
                                        const next = self.gpu.sprite_pixel_fifo.items[pixel_i][0];
                                        if (next != .transparent) continue;
                                        self.gpu.sprite_pixel_fifo.items[pixel_i] = .{pixel, self.sprite_data.?.flags.bg_priority};
                                    } else self.gpu.sprite_pixel_fifo.appendAssumeCapacity(.{pixel, self.sprite_data.?.flags.bg_priority});
                                }
                            }

                            switch (self.active_fetcher) {
                                .bg => self.pixels_fetched += 1,
                                .win => self.window_x += 1,
                                .sprite => {}
                            }
                        }
                        // if (pixel_i == 0) {
                            self.reset_state(); // reset the fetcher state as we have pushed the last pixel
                            if (self.gpu.bus.cpu.booted) print("finished pushing {s} pixel: active fetcher: {any} | state {any}\n", .{if (rendering_sprites) "sprite" else "bg/window" , self.active_fetcher, self.active_fetcher_state()});
                        // }
                        break :cycles 2;
                    } else break :cycles 1;
                }
            };
        }

        fn detect_window_reached(self: *pixel_fetcher) bool {
            // print("detect_window_reached()\n", .{});
            if (!self.render_window and self.gpu.check_lcdc(.display_window) and self.window_reached) {
                // print("we should display the window and we've reached it vertically...\n", .{});
                const wx = self.gpu.getSpecialRegister(.wx);
                const window_x_trigger = if (wx >= 7) wx - 7 else 0;
                const window_max_trigger = 167;
                if (window_x_trigger <= window_max_trigger and self.gpu.scanline_displayed >= window_x_trigger) {
                    // print("we've also reached it horizontally, begin window rendering...\n", .{});
                    self.render_window = true;
                    self.gpu.bg_pixel_fifo.clearRetainingCapacity();
                    self.window_x = 0;
                    // self.pixels_fetched = 0;
                    self.active_fetcher = .win;
                    self.reset_state();
                    return true;
                }
            } return false;
        }

        fn detect_sprite_fetch(self: *pixel_fetcher) bool {
            for (self.gpu.sprite_buffer[self.gpu.sprites_used..self.gpu.sprite_buffer_i]) |maybe_sprite| {
                const sprite = maybe_sprite orelse unreachable;
                const sprite_x = if (sprite.x >= 8) sprite.x - 8 else continue;
                if (sprite_x != self.gpu.scanline_displayed) continue;
                // sprite must be here
                // self.gpu.bg_pixel_fifo.clearRetainingCapacity();
                print("deteced a sprite fetch at pixel {d} | {any}", .{self.gpu.scanline_displayed, sprite});
                self.reset_state();
                self.active_fetcher = .sprite;
                self.sprite_data = sprite;
                return true;
            } return false;
        }
    };

    // fn createTile()
    fn tilePixelDecoder(self: *GPU, high: u8, low: u8, pixel_index: u3, flags: Sprite.Flags, palette: special_register) Color {
        // inspect the sprite obj data if there is one
        if (palette != .bgp and palette != .obp0 and palette != .obp1) @panic("you passed a invalid register as the palette"); // remove in release builds
        const shift: u3 = if (!flags.x_flip) 7 - pixel_index else pixel_index;
        const hi = @as(u1, @truncate(high >> shift));
        const lo = @as(u1, @truncate(low >> shift));
        const color_code: u2 = (@as(u2, hi) << 1) | lo;
        const palette_color: u2 = @truncate(self.getSpecialRegister(palette) >> @as(u3, @intCast(color_code)) * 2); // selecting color
        return @as(Color, @enumFromInt(palette_color));
    }
    fn pixelMixer(sprite_pixel: struct{Color, bool}, bg_color: Color) Color {
        const sprite_color = sprite_pixel[0];
        const bg_priority = sprite_pixel[1];
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
        // print("Mode switch: {any}, LY: {d}, stat: {d}\n", .{ self.mode, ly, self.getSpecialRegister(.stat) });
        self.mode = mode: switch (self.mode) {
            .SCAN => {
                self.mode_cycles = Mode.RENDER.max_cycles(); // this may vary
                break :mode .RENDER;
            },
            .RENDER => {
                // debug
                self.lcd.renderAll("", self.getSpecialRegister(.ly)); // render at the last scanline (for debug!! should happen at the end of each frame in gb.go())
                if (self.bus.cpu.booted) {
                    for (self.sprite_buffer[0..]) |*maybe_sprite| {
                        const sprite = maybe_sprite.* orelse break;
                        print("Scanline sprite set data: {any}\n", .{sprite});
                        maybe_sprite.* = null;
                    }
                    print("\n", .{});
                    // self.bus.cpu.break_exe();
                }

                self.lcd.pushScanline(self.scanline, ly);
                print("entering hblank, cycles spent rendering? {d}\n", .{self.mode_cycles_spent});
                self.mode_cycles = Mode.HBLANK.max_cycles() - (self.mode_cycles_spent - 172);
                break :mode .HBLANK;
            },
            .HBLANK => {
                // Increment LY register
                self.setSpecialRegister(.ly, ly + 1);
                if (self.fetcher.render_window) {
                    self.fetcher.window_line_counter += 1;
                }
                self.fetcher.render_window = false;
                if (ly + 1 == 144) { // send vblank interrupt
                    self.bus.handler.set(.flag, .vblank);
                    self.mode_cycles = Mode.VBLANK.max_cycles(); // per scanline
                    break :mode .VBLANK;
                } else {
                    self.mode_cycles = Mode.SCAN.max_cycles();
                    break :mode .SCAN;
                }
            },
            .VBLANK => {
                // leaving vblank, reset all per frame variables
                self.fetcher.window_reached = false;
                self.fetcher.window_line_counter = 0;
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
                                const color = self.tilePixelDecoder(high, low, pixel_index, .{ .bg_priority = false, .x_flip = false, .y_flip = false, .palette = false}, .bgp);
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
                                // const is_edge = (row == 0 or row == 7) or (col == 0 or col == 7);
                                const color: Color = self.tilePixelDecoder(high, low, pixel_index, .{ .bg_priority = false, .x_flip = false, .y_flip = false, .palette = false}, .bgp);

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

                                const color = self.tilePixelDecoder(high, low, pixel_index,.{ .bg_priority = false, .x_flip = false, .y_flip = false, .palette = false}, .obp0);
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
                                const color = self.tilePixelDecoder(high, low, pixel_index, .{ .bg_priority = false, .x_flip = false, .y_flip = false, .palette = false}, .obp0);
                                // write to tilesheet buffer
                                const tilesheet_index = (dstY + row) * 160 + (dstX + col);
                                LCD.writeToBuf(&self.lcd.oamBuf, color, tilesheet_index);
                            }
                        }
                    }
                    self.setSpecialRegister(.ly, 0); // reset LY to 0
                    self.mode_cycles = Mode.SCAN.max_cycles();
                    self.frames_cycled += 1;
                    break :mode .SCAN;
                } else {
                    self.setSpecialRegister(.ly, new_ly);
                    self.mode_cycles = Mode.VBLANK.max_cycles();
                    break:mode .VBLANK;
                }
            },
        };
        if (self.mode == .SCAN) { // we just started a new scanline, reset all per scanline variables
            self.fetcher.reset_state();
            self.sprite_buffer_i = 0;
            self.oam_i = 0;
            self.scx_discard = self.getSpecialRegister(.scx) % 8;
            self.fetcher.pixels_fetched = 0;
            // self.fetcher.window_fetched = 0;
            self.scanline_displayed = 0;
            self.sprites_used = 0;
            self.sprite_pixel_fifo.clearRetainingCapacity();
            self.bg_pixel_fifo.clearRetainingCapacity();
            if (self.getSpecialRegister(.ly) == self.getSpecialRegister(.wy)) {
                self.fetcher.window_reached = true;
            }
        }
        self.mode_cycles_spent = 0;
        // update the stat register after mode switch
        const lyc_check = self.getSpecialRegister(.ly) == self.getSpecialRegister(.lyc);
        var stat_reg = self.getSpecialRegister(.stat); // update STAT register
        stat_reg &= 0b1111_1000;
        stat_reg |= (@as(u3, @intFromBool(lyc_check)) << @intFromEnum(stat_bit.lyc_res)) | @intFromEnum(self.mode);
        self.setSpecialRegister(.stat, stat_reg);

        const mode_interrupt_bit: ?stat_bit = switch (self.mode) {
            .HBLANK => stat_bit.enable_mode0,
            .VBLANK => stat_bit.enable_mode1,
            .SCAN => stat_bit.enable_mode2,
            .RENDER => null
        };

        if (mode_interrupt_bit) |b| {
            if (interrupt_is_enabled(stat_reg, b)) self.bus.handler.set(.flag, .lcd);
        }
        if (lyc_check and interrupt_is_enabled(stat_reg, .enable_lyc_check)) {
            self.bus.handler.set(.flag, .lcd);
        }
    }

    pub fn randomStatic(self: *GPU) void { // random static
        for (0..self.lcd.screenBuf.len) |i| {
            const color: Color = GB.prng.random().enumValue(Color);
            LCD.writeToBuf(&self.lcd.screenBuf, color, i);
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
        if (!(self.check_lcdc(.lcd_enable) and self.mode == .RENDER)) {
            const fixed_address = address - VRAM_BEGIN;
            self.vram[fixed_address] = value;
        }
    }
    pub const lcdc_bit = enum {
        bg_win_enable, // 0
        sprite_enable, // 1
        sprite_size,   // 2
        bg_tilemap,    // 3
        tile_data_sel, // 4
        display_window,// 5
        window_tilemap,// 6
        lcd_enable     // 7
    };
    pub fn check_lcdc(self: *GPU, bit: lcdc_bit) bool {
        return BIT(@intFromEnum(bit), self.getSpecialRegister(.lcdc)) == 1;
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
        if (register == .lcdc) {
            // if ((value & 0x20) != 0) @panic("the window got enabled");
            if (self.check_lcdc(.lcd_enable) ^ (BIT(@intFromEnum(lcdc_bit.lcd_enable), value) == 1)) { // lcd enable changed
                // print("lcd enable changed", .{});
                self.fetcher.reset_state();
                self.mode = .VBLANK;
                self.switchMode();
            }
        }
        self.special_registers[@intFromEnum(register)] = value;
    }
    pub fn getSpecialRegister(self: *GPU, register: special_register) u8 {
        return self.special_registers[@intFromEnum(register)];
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
    // buffers
    screenBuf: [screenHeightPx * screenWidthPx]u32 = undefined,

    // debug buffers
    tilesheetBuf: [192 * 128]u32 = undefined,
    bgBuf: [256 * 256]u32 = undefined,
    spriteBuf: [8 * 5 * 64]u32 = undefined,
    oamBuf: [8 * 5 * 64]u32 = undefined,

    // SDL
    win: *g.SDL_Window = undefined,
    renderer: *g.SDL_Renderer = undefined,
    screen_texture: *g.SDL_Texture = undefined,

    // debug textures
    bg_texture: *g.SDL_Texture = undefined,
    tilesheet_texture: *g.SDL_Texture = undefined,
    oam_texture: *g.SDL_Texture = undefined,
    sprites_texture: *g.SDL_Texture = undefined,
    ly_texture: *g.SDL_Texture = undefined,
    text_texture: *g.SDL_Texture = undefined,

    font: *g.TTF_Font = undefined,
    
    // debug_win: *g.SDL_Window = undefined,
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
        // Font & Text init (debug)
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

        // debug
        self.bg_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, 256, 256);
        _ = g.SDL_SetTextureScaleMode(self.bg_texture, g.SDL_SCALEMODE_NEAREST);
        self.tilesheet_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, 192, 128);
        _ = g.SDL_SetTextureScaleMode(self.tilesheet_texture, g.SDL_SCALEMODE_NEAREST);
        self.oam_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, 160, 16);
        _ = g.SDL_SetTextureScaleMode(self.oam_texture, g.SDL_SCALEMODE_NEAREST);
        self.sprites_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, 160, 16);
        _ = g.SDL_SetTextureScaleMode(self.sprites_texture, g.SDL_SCALEMODE_NEAREST);

        self.ly_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, 160, 1);
        _ = g.SDL_SetTextureScaleMode(self.sprites_texture, g.SDL_SCALEMODE_NEAREST);
        
        self.bootScreen(); // wait for sdl to finish building the window to begin progression
    }
    fn bootScreen(self: *LCD) void {
        self.renderAll("GAMEBOY\n", 0); // Draw a dummy frame to force the window to initialize and show
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
        .{ // frostbite
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

    const ly_texture_buf: [screenWidthPx]u32 = .{0xFF_FF_00_00} ** screenWidthPx;
    pub fn renderAll(self: *@This(), debug: []const u8, ly: u8) void {
        _ = g.SDL_RenderClear(self.renderer);
        _ = g.SDL_UpdateTexture(self.screen_texture, null, &self.screenBuf, 160 * @sizeOf(u32));

        // debug
        _ = debug;
        _ = g.SDL_UpdateTexture(self.tilesheet_texture, null, &self.tilesheetBuf, 192 * @sizeOf(u32));
        _ = g.SDL_UpdateTexture(self.bg_texture, null, &self.bgBuf, 256 * @sizeOf(u32));
        _ = g.SDL_UpdateTexture(self.oam_texture, null, &self.oamBuf, 160 * @sizeOf(u32));
        _ = g.SDL_UpdateTexture(self.sprites_texture, null, &self.spriteBuf, 160 * @sizeOf(u32));
        _ = g.SDL_UpdateTexture(self.ly_texture, null, &ly_texture_buf, 160 * @sizeOf(u32));

        // _ = g.SDL
        const gb_screen_w = screenWidthPx * pxSize;
        const gb_screen_h = screenHeightPx * pxSize;
        const screen_rect = g.SDL_FRect{
            .x = 0,
            // .x = @as(f32, @floatFromInt(center)) - screenWidthPx / 2 * pxSize,
            .y = border / 2,
            .w = gb_screen_w,
            .h = gb_screen_h
        };

        // debugs
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

        const ly_rect = g.SDL_FRect{
            .x = 0,
            .y = @as(f32, @floatFromInt(ly)) * pxSize,
            .w = gb_screen_w,
            .h = pxSize
        };

        _ = g.SDL_RenderTexture(self.renderer, self.screen_texture, null, &screen_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.tilesheet_texture, null, &tilesheet_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.bg_texture, null, &background_map_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.oam_texture, null, &oam_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.sprites_texture, null, &sprites_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.ly_texture, null, &ly_rect);
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

pub fn BIT(bit: u6, int: usize) u1 {
    return @truncate(int >> bit);
}