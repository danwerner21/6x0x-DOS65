# Wyrmhold

An original top-down fantasy RPG for the **6502PC** computer running **DOS/65**,
written in 6502 assembly (ca65 syntax).

It uses the **memory-mapped video card** directly (paged into bank `$A` like the
SpeedScript port) for a colorful 80x24 display with custom graphics tiles, and
the **AY-3-8910 PSG** for a title-screen melody and in-game sound effects.

This is original work — our own maps, monsters, text, tiles, and engine.

## Gameplay

- A scrolling overworld viewport keeps your hero centered while the world scrolls.
- A right-hand panel shows HP, level, XP, gold, food, weapon and armor.
- A two-line message log at the bottom narrates what happens.
- **Movement:** `W` `A` `S` `D` (also `H` `J` `K` `L`).
- **Bump combat:** walk into a monster to attack it; it strikes back.
- **Towns** (the house tile): enter to reach a shop. Buy healing, provisions,
  better weapons and armor with gold. Press the listed number keys to buy; `X`
  to leave. Step on the bright-green door to leave a town.
- **Dungeon** (the cave tile): descend, grab treasure, and defeat the **Dragon
  boss** to win. Step on the bright-green stairs to climb back out.
- **`Q`** quits back to the DOS/65 prompt.

## Building

> **NOTE:** The cc65 toolchain (`ca65`/`ld65`) is **not** installed on the
> original development machine — only `srec_cat` is. Build this on a machine
> with cc65 + the CP/M tools (`cpmcp`/`cpmrm`) installed.

```
cd software/wyrmhold
make
```

The `Makefile` assembles with `ca65`, links with `ld65` against `dos65.cfg`
(program loads at `$0800`, the DOS/65 TEA), runs the `srec_cat` offset chain to
produce `WYRMHOLD.COM`, and copies it into the 6502PC DOS/65 disk image
(`../../bin/6502PC/DOS65_6502PC.IMG`).

## Running

Boot the DOS/65 image on the 6502PC (hardware or emulator) and run:

```
WYRMHOLD
```

## Source layout

| File           | Purpose                                                      |
|----------------|--------------------------------------------------------------|
| `wyrmhold.asm` | Entry point, title screen, main turn loop, win/lose, includes|
| `defines.asm`  | System equates, color/tile constants, zero-page & game state |
| `macro.asm`    | 16-bit helper macros and a `FARCALL` macro                   |
| `tiles.asm`    | Custom 8x8 character-generator bitmaps (terrain/monsters)    |
| `video.asm`    | Video paging, frame/panel/viewport rendering, UI text output |
| `world.asm`    | Overworld / town / dungeon map data and tile-property tables |
| `rng.asm`      | Small xorshift pseudo-random generator                       |
| `ui.asm`       | Message log, stat panel, prompts, decimal number printing    |
| `entity.asm`   | Monster table, spawning and simple AI                        |
| `player.asm`   | Player state and movement / map transitions                  |
| `combat.asm`   | Bump-combat resolution, rewards and leveling                 |
| `town.asm`     | Town interior and shop logic                                 |

The video and PSG access patterns follow the existing repo code
(`software/speedscript/screen.asm` and `software/dbasic/ay38910.asm`), and the
custom tiles use the character generator the way `V_PATTERN` does in
`software/dbasic/screencmds.asm`.
