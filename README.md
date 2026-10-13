# zigboy

zigboy is a Game Boy emulator written in Zig. It runs Game Boy ROMs in an SDL window and shows the game screen alongside graphics debugging views.

## Run

Build and launch any rom from the `roms/` directory with:

```sh
zig build run -- <rom-title>
```

The ROM argument is the file name without `.gb`.  The emulator expects `roms/boot.bin`.

## Controls

| Action | Keys |
| --- | --- |
| D-pad up, left, down, right | W, A, S, D or arrow keys |
| Game Boy A | O or Z |
| Game Boy B | P or X |
| Start | F |
| Select | G |
| Cycle color palette | 1 |
| Pause emulation | 0 |
| Step one instruction | Space |
| Resume emulation | Enter |
| Toggle fullscreen | F11 |

Close the window to quit. The pause and step keys are debugging controls; pausing is available after the boot ROM finishes.
