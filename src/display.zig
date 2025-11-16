/// Defines a gameboy GPU(PPU)
/// - Handles writing to vram and processing pixels from memory to the screen
pub const GPU = struct {
    vram: *[VRAM_SIZE]u8 = undefined,
    oam: *[OAM_SIZE]u8 = undefined,
    bus: *Bus = undefined,
    // Stores sprite attributes (position, tile index, attributes).
    // Cannot be accessed during scanline rendering. -- writeOAM & read
    // Control rendering behavior.
    // Define which layers are enabled.
    special_registers: *[12]u8 = undefined,
    // LCD Control Registers (I/O Registers at $FF40–$FF4B)
    tile_set: [384]Tile = undefined,
    sprite_set: [10]Tile = undefined, // TODO actually sprite
    stat_reg: u8 = undefined,
    interrupt_pending: bool = false,
    mode: Mode = undefined,
    lcd: LCD = undefined,
    scanline: [LCD.screenWidthPx]Color = undefined,
    scanline_progress: u8 = 0,
    mode_cycles_left: u16 = 456,
    frames_cycled: usize = 0,
    frame_cycles_spent: u64 = 0,
    // rand: std.Random = undefined,

    fn empty_tile(self: *@This()) Tile {
        _ = self;
        var tile: Tile = undefined;
        for (&tile) |*row| {
            @memset(row, .white);
        }
        return tile;
    }
    // startup
    pub fn init(self: *@This(), gb: *GB) !void {
        self.vram = gb.bus.memory[VRAM_BEGIN .. VRAM_END + 1];
        self.oam = gb.bus.memory[OAM_BEGIN .. OAM_END + 1];
        self.bus = &gb.bus;
        self.special_registers = gb.bus.memory[special_register.start .. special_register.end + 1];
        self.mode = .SCAN;
        self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.SCAN)];
        try self.lcd.init(gb.allocator, gb.root_path);
        @memset(&self.tile_set, empty_tile(self));
    }
    // gpu execution
    pub fn tick(self: *@This(), cycles: u16) void {
        // TODO the gpu should tick/cycle just as many
        // times as the cpu did, while being able to
        // process interrupts and continue on as well as changing modes mid-scanline when needed
        var cycles_left = cycles; // amt of cycles spent by cpu
        self.frame_cycles_spent += cycles_left;
        while (cycles_left > 0) {
            const cycles_to_process: u8 = @intCast(@min(cycles_left, self.mode_cycles_left));
            // self.frame_cycles += cycles_to_process;
            self.do(cycles_to_process);
            self.mode_cycles_left -= cycles_to_process;
            cycles_left -= cycles_to_process;
            if (self.mode_cycles_left == 0) {
                self.switchMode(); // handles drawing the screen, updating ly
                // update the stat register after mode switch
                const lyc_check = self.getSpecialRegister(.ly) == self.getSpecialRegister(.lyc);
                // lyc check here
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
                if (line_was_set) {
                    // print("lcd interrupt\n", .{});
                    self.bus.handler.set(.flag, .lcd);
                }
                // print("Mode switch: {any}, LY: {d}, stat: {d}\n", .{ self.mode, self.getSpecialRegister(.ly), self.getSpecialRegister(.stat) });
            }
        }
    }
    fn do(self: *@This(), cycles: u8) void {
        // Operate GPU here
        // // const zone = tracy.beginZone(@src(), .{ .name = "DO GPU CYCLES" });
        // // defer zone.end();
        var cycles_to_spend: f16 = @floatFromInt(cycles);
        switch (self.mode) {
            .SCAN => { // 2 searches OAM memory for sprites that should be rendered on the current scanline and stores them in a buffer
                // decoding:
                // | byte 0 | y pos
                // | byte 1 | x pos
                // | byte 2 | tile number --  the Tile Number used for fetching the graphics data for the sprite
                // | byte 3 | sprite flags:
                // {bit 7} OBJ-BG priority 0 (priority) 1 (1-3 priority)
                // {bit 6} Y-flip
                // {bit 5} X-flip
                // {bit 4} pallete number
                // print("scanning\n", .{});
                // const sprite_buffer: u8[40] = .{};
                // var i: u8 = 0;
                // var buffer_i: u8 = 0;
                // while (buffer_i < 40 and i <= OAM_SIZE) : (i+=4) {
                //     const y_pos = self.oam[i];
                //     const x_pos = self.oam[i+1];
                //     if (x_pos > 0 and ly + 16 >= y_pos) {
                //         for(sprite_buffer[buffer_i]
                //     }
                // }
                return;
            },
            .RENDER => { // 3 transfers pixels to the LCD, one scanline at a time, duration variable
                // TODO: Generate the actual pixels for this scanline based on:
                // - Background tiles at the current scroll position
                // - Window tiles if enabled and visible on this line
                // - Sprites that were found during OAM scan
                if (!self.testSpecialRegister(.lcdc, 7)) return;
                const ly = self.getSpecialRegister(.ly);
                const bg_cpp = 1.0075; // cycles per pixel
                const use_signed = !self.testSpecialRegister(.lcdc, 4);
                const base: i32 = if (use_signed) 0x9000 else 0x8000;
                const tilemap_base: u16 = if (self.testSpecialRegister(.lcdc, 3)) 0x9C00 else 0x9800; // check lcdc bit 3
                const scx = self.getSpecialRegister(.scx);
                const scy = self.getSpecialRegister(.scy);
                var x = self.scanline_progress;
                while (x < self.scanline.len and cycles_to_spend > 0) {
                    const bg_y: u16 = (@as(u16, ly) + scy) & 0xFF; // wraps at 256
                    const bg_x: u16 = (@as(u16, x) + scx) & 0xFF;

                    const tile_y: u16 = bg_y / 8;
                    const tile_x: u16 = bg_x / 8;

                    const tile_index_addr: u16 = tilemap_base + tile_y * 32 + tile_x;

                    const tile_index: i16 = if (use_signed) @intCast(@as(i8, @bitCast(self.readVram(tile_index_addr)))) else @intCast(self.readVram(tile_index_addr));
                    const tile_line: u16 = bg_y % 8;

                    const tile_addr: u16 = @intCast(base + tile_index * 16 + @as(i32, @intCast(tile_line * 2)));
                    const low = self.readVram(tile_addr);
                    const high = self.readVram(tile_addr + 1);

                    const pixel: u3 = @intCast(bg_x % 8);
                    self.scanline[x] = self.tileDecoder(high, low, pixel);
                    cycles_to_spend -= bg_cpp;
                    x += 1;
                    self.scanline_progress = x;
                    if (cycles_to_spend <= 0) return;
                }
                return;
            },
            else => return, // no action for hblank or vblank
        }
    }
    pub fn randomStatic(self: *GPU) void { // random static
        for (0..self.lcd.screenBuf.len) |i| {
        const color: Color = GB.prng.random().enumValue(Color);
        LCD.writeToBuf(&self.lcd.screenBuf, color, i);
    }
    }
    fn tileDecoder(self: *GPU, high: u8, low: u8, pixel_index: u3) Color {
        const shift: u3 = 7 - pixel_index;
        const hi = @as(u1, @truncate(high >> shift));
        const lo = @as(u1, @truncate(low >> shift));
        const color_code: u2 = (@as(u2, hi) << 1) | lo;
        const bgp = self.getSpecialRegister(.bgp); // get the right color pallete (dmg)
        const pallete_color: u2 = @truncate(bgp >> @as(u3, @intCast(color_code)) * 2); // selecting color
        return @as(Color, @enumFromInt(pallete_color));
    }
    fn switchMode(self: *@This()) void {
        const ly = self.getSpecialRegister(.ly);
        self.mode = mode: switch (self.mode) {
            .SCAN => {
                self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.RENDER)];
                break :mode .RENDER;
            },
            .RENDER => {
                self.lcd.pushScanline(self.scanline, ly);
                self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.HBLANK)];
                self.scanline_progress = 0;
                break :mode .HBLANK;
            },
            .HBLANK => {
                // Increment LY register
                self.setSpecialRegister(.ly, ly + 1);
                if (ly + 1 == 144) { // send vblank interrupt
                    self.bus.handler.set(.flag, .vblank);
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.VBLANK)]; // per scanline
                    break :mode .VBLANK;
                } else {
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.SCAN)];
                    break :mode .SCAN;
                }
            },
            .VBLANK => {
                const new_ly = ly + 1;
                if (new_ly > 153) { // 153 is the end of VBLANK
                    // fetching and writing background tiles to the lcd
                    const base: u16 = 0x8000; // start of tile data
                    var tile: u16 = 0;
                    while (tile < 384) : (tile += 1) {
                        const tileX = tile % 24;
                        const tileY = tile / 24;
                        const dstY = tileY * 8;
                        const dstX = tileX * 8;
                        const tile_addr: u16 = base + tile * 16;
                        for (0..8) |row| {
                            for (0..8) |col| {
                                const pixel_index: u3 = @intCast(col);
                                const low = self.readVram(tile_addr + row * 2);
                                const high = self.readVram(tile_addr + row * 2 + 1);
                                const color = self.tileDecoder(high, low, pixel_index);
                                // write to tilesheet buffer
                                const tilesheet_index = (dstY + row) * 192 + (dstX + col);
                                LCD.writeToBuf(&self.lcd.tilesheetBuf, color, tilesheet_index);
                            }
                        }
                    }
                    self.setSpecialRegister(.ly, 0); // reset LY to 0
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.SCAN)];
                    self.frames_cycled += 1;
                    break :mode .SCAN;
                } else {
                    self.setSpecialRegister(.ly, new_ly);
                    self.mode_cycles_left = Mode.cycles[@intFromEnum(Mode.VBLANK)];
                    break:mode .VBLANK;
                }
            },
        };
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
    fn set_interrupt_line(self: *GPU) bool {
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
    pub fn writeOAM(self: *GPU, address: usize, value: u8) !void {
        if (self.mode == .RENDER or self.mode == .SCAN) {
            print("cannot access oam now\n", .{});
            return;
        }
        if (address <= 0xFE00 or address >= 0xFE9F) return error.OutOfOAMBounds;
        self.oam[address] = value;
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
    // context & types
    const Mode = enum { // modes specifying number of cycles per scanline
        HBLANK,
        VBLANK,
        SCAN,
        RENDER,
    const cycles: [4]u16 = .{ 204, 456, 80, 172 };
    };
    const Color = enum(u2) { black, dgray, lgray, white };
    const Tile = [8][8]Color;
    pub const VRAM_BEGIN = 0x8000;
    pub const VRAM_END = 0x9FFF;
    pub const VRAM_SIZE = VRAM_END - VRAM_BEGIN + 1;
    const tilemap_one = 0x9800;
    const tilemap_two = 0x9C00;
    const tilemap_size: u16 = tilemap_two - tilemap_one;
    pub const OAM_BEGIN = 0xFE00;
    pub const OAM_END = 0xFE9F;
    pub const OAM_SIZE = OAM_END - OAM_BEGIN + 1;
};

///Contains the fields necessary to create a display,
///- Screen, Height, Width, Rendering
pub const LCD = struct {
    screenBuf: [screenHeightPx * screenWidthPx]u32 = undefined,
    tilesheetBuf: [192 * 128]u32 = undefined,
    renderer: *g.SDL_Renderer = undefined,
    screen_texture: *g.SDL_Texture = undefined,
    bg_texture: *g.SDL_Texture = undefined,
    tilesheet_texture: *g.SDL_Texture = undefined,
    text_surface: *g.SDL_Surface = undefined,
    text_texture: *g.SDL_Texture = undefined,
    font: *g.TTF_Font = undefined,
    win: *g.SDL_Window = undefined,
    grid_pixel_sz: u16 = undefined,
    allocator: std.mem.Allocator = undefined,
    root_path: []const u8 = undefined,
    const gb_palette = [_]u32{ // actually greens
        0xFFFFFFFF, // white
        0xFFAAAAAA, // light gray
        0xFF555555, // dark gray
        0xFF000000, // transparent
    };

    // startup
    fn init(self: *@This(), allocator: std.mem.Allocator, root_path: []const u8) !void {
        var color: GPU.Color = undefined;
        for (0..self.screenBuf.len) |i| {
            color = GB.prng.random().enumValue(GPU.Color);
            writeToBuf(&self.screenBuf, color, i);
        }
        for (0..self.tilesheetBuf.len) |i| {
            color = GB.prng.random().enumValue(GPU.Color);
            writeToBuf(&self.tilesheetBuf, color, i);
        }
        self.allocator = allocator;
        self.root_path = root_path;
        try self.startAndCreateRenderer(); // set window and renderer
    }
    fn startAndCreateRenderer(self: *@This()) !void {
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
        if (renderer == null) {
            print("Failed to create renderer: {s}\n", .{g.SDL_GetError()});
            return error.RendererFailure;
        }

        self.renderer = renderer.?;
        self.win = win.?;
        _ = g.SDL_SetWindowResizable(self.win, true);
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
        // TODO RESIZABLE
        self.bg_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, window_width, window_height);
        self.tilesheet_texture = g.SDL_CreateTexture(self.renderer, g.SDL_PIXELFORMAT_ARGB8888, g.SDL_TEXTUREACCESS_TARGET, 192, 128);
        _ = g.SDL_SetTextureScaleMode(self.tilesheet_texture, g.SDL_SCALEMODE_NEAREST);
        self.createBG();
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
    // peripheral fns
    fn writeToBuf(buf: []u32, color: GPU.Color, index: usize) void {
        if (@intFromEnum(color) > 3) {
            print("enum Color(0-3): {d}\n", .{color});
        }
        buf[index] = switch (color) {
            .white => 0xFFCCFFCC,
            .lgray => 0xFF99CC99,
            .dgray => 0xFF669966,
            .black => 0xFF336633,
        };
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
    fn createBG(self: *LCD) void {
        _ = g.SDL_SetRenderTarget(self.renderer, self.bg_texture);
        defer _ = g.SDL_SetRenderTarget(self.renderer, null);
        _ = g.SDL_SetRenderDrawColor(self.renderer, 255, 192, 220, 255);
        _ = g.SDL_RenderClear(self.renderer);
    }
    pub fn renderAll(self: *@This(), debug: []const u8) void {
        _ = g.SDL_UpdateTexture(self.screen_texture, null, &self.screenBuf, 160 * @sizeOf(u32));
        _ = g.SDL_UpdateTexture(self.tilesheet_texture, null, &self.tilesheetBuf, 192 * @sizeOf(u32));
        const dgray = g.SDL_Color{ .r = 36, .g = 36, .b = 36, .a = 255 };
        if (debug.len > 0) {
            self.text_surface = g.TTF_RenderText_Solid_Wrapped(self.font, debug.ptr, debug.len, dgray, 0);
            self.text_texture = g.SDL_CreateTextureFromSurface(self.renderer, self.text_surface);
        }
        defer g.SDL_DestroySurface(self.text_surface);
        defer g.SDL_DestroyTexture(self.text_texture);
        // _ = g.SDL_RenderClear(self.renderer);
        const bg_x = screenWidthPx * pxSize + border * 2;
        const bg_width: f32 = @as(f32, @floatFromInt(window_width)) - (screenWidthPx * pxSize + border * 2);
        const bg_rect = g.SDL_FRect{
            .x = bg_x,
            .y = 0,
            .w = bg_width,
            .h = @as(f32, @floatFromInt(window_height)),
        };
        const text_rect = g.SDL_FRect{ .x = bg_x + 15, .y = 30, .w = @as(f32, @floatFromInt(self.text_surface.w)), .h = @as(f32, @floatFromInt(self.text_surface.h)) };
        const screen_rect = g.SDL_FRect{
            .x = 10.0,
            .y = 0,
            .w = screenWidthPx * pxSize,
            .h = screenHeightPx * pxSize,
        };
        const bg_px_sz: f32 = (@as(f32, @floatFromInt(window_height)) - (screenHeightPx * pxSize) + 10) / 128;
        const bg_map_rect = g.SDL_FRect{
            .x = @as(f32, @floatFromInt(0)),
            .y = screenHeightPx * pxSize + 10,
            .w = 192 * bg_px_sz,
            .h = 128 * bg_px_sz,
        };
        _ = g.SDL_RenderTexture(self.renderer, self.bg_texture, null, &bg_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.text_texture, null, &text_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.screen_texture, null, &screen_rect);
        _ = g.SDL_RenderTexture(self.renderer, self.tilesheet_texture, null, &bg_map_rect);
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
        g.SDL_DestroySurface(self.text_surface);
        g.TTF_CloseFont(self.font);
        g.SDL_Quit();
        g.TTF_Quit();
    }
    const initWinW: u16 = 1280;
    const initWinH: u16 = 700;
    const border = 10;
    const screenWidthPx = 160;
    const screenHeightPx = 144;
    var window_height: c_int = @intCast(initWinH);
    var window_width: c_int = @intCast(initWinW);
    var center: u16 = initWinW / 2;
    var pxSize: f32 = @as(f32, @floatFromInt(initWinW)) / screenWidthPx / 2;
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