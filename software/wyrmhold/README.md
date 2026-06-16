# Wyrmhold

An original top-down fantasy RPG for the **6502PC** computer running **DOS/65**,
written in 6502 assembly (ca65 syntax).

It uses the **memory-mapped video card** directly (paged into bank `$A` like the
SpeedScript port) for a colorful 80x24 display with custom graphics tiles, and
the **AY-3-8910 PSG** for a title-screen melody and in-game sound effects.
Wyrmhold explicitly selects 80-column text mode at startup before using its
direct-VRAM renderer.

This is original work — our own maps, monsters, text, tiles, and engine.

## Gameplay

- A scrolling `22x10`-tile overworld viewport keeps your hero centered while
  the world scrolls. Each world tile uses four custom characters for clean,
  high-contrast 16x16 artwork.
- Restrained foreground detail and strongly separated background colors make
  fields, forests, water, mountains, roads, and interiors easy to distinguish.
- Hills soften mountain ranges into the surrounding grasslands, while marshes
  add wetland detail around rivers and coasts.
- Terrain affects travel and combat: roads conserve provisions, forests conceal
  the player from distant monsters, hills grant a combat advantage, and
  marshes are costly and may poison the unwary.
- Authored overworld discoveries reward careful exploration with supplies,
  permanent vitality, useful route information, and a risky Sunken March
  shortcut.
- A right-hand panel shows HP, level, XP, gold, food, weapon and armor.
- The panel names the current overworld region: Northreach, Wyrmhold Vale, or
  the Sunken March.
- The objective panel guides the opening quest and later progression.
- A two-line message log at the bottom narrates what happens.
- **Movement:** `W` `A` `S` `D` (also `H` `J` `K` `L`).
- **Bump combat:** walk into a monster to attack it; it strikes back.
- **Guard:** press `G` to wait one turn and reduce incoming melee damage.
- **Help:** press `?` to view controls, terrain hints, and the current objective
  without consuming a turn.
- Weapons have distinct styles: daggers can critically strike, swords deal
  reliable damage, and axes have a powerful but unpredictable swing.
- Armor also has tradeoffs: leather resists snake venom, chain is balanced,
  and plate provides the best defense but consumes provisions faster.
- Monsters now behave differently: snakes surge and poison, skeletons guard
  until approached, thieves steal and flee, and trolls regenerate but move
  slowly.
- Overworld encounter mixes vary by region and become more dangerous after the
  dragon's lair is opened.
- Roaming overworld monsters persist while visiting interiors; depleted groups
  receive limited regional reinforcements instead of resetting completely.
- **Eastmere** is a dense coastal town whose outfitter sells the realm's
  cheapest weapon and armor upgrades.
- **Valehaven** is an open canal town whose market specializes in inexpensive
  healing and larger provision bundles.
- In either town, press the listed number keys to buy, `X` to leave the shop,
  and step on the bright-green door to return to the overworld.
- **Wyrmhold Castle** is an enterable audience chamber. Press `T` beside the
  ruler to receive the opening commission.
- The **Sunken Shrine** lies in the southern marshes. Its fast-moving Wyrm
  Warden guards the key needed to open the dragon's lair.
- Press `T` at the Sunken March reed ford to cross a dangerous shortcut. It
  saves travel, but costs health and provisions.
- **Dungeon** (the cave tile): cross ancient halls and flooded passages to
  confront the **Dragon**. Its glowing fire lane warns where its next breath
  will strike. After defeating it, return to King Aldren for the ending.
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

Run source and linked-image validation separately with:

```
make validate
```

The validator checks map dimensions and characters, tile-table lengths,
metatile records, glyph collisions, discovery coordinates, and the linked
`$9000` memory-budget target. Existing non-64-character overworld source rows
are reported as warnings because the decoder currently pads or truncates them.
The Makefile defaults to `python3`; override `PYTHON` when using another
launcher.

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
| `metatiles.asm` | 2x2 gameplay terrain, landmark, player and monster artwork  |
| `video.asm`    | Video paging, frame/panel/viewport rendering, UI text output |
| `world.asm`    | Overworld / town / dungeon map data and tile-property tables |
| `rng.asm`      | Small xorshift pseudo-random generator                       |
| `ui.asm`       | Message log, stat panel, prompts, decimal number printing    |
| `entity.asm`   | Monster table, spawning and simple AI                        |
| `player.asm`   | Player state and movement / map transitions                  |
| `combat.asm`   | Bump-combat resolution, rewards and leveling                 |
| `town.asm`     | Town interior and shop logic                                 |
| `castle.asm`   | Castle audience chamber, ruler interaction and quest opening |
| `shrine.asm`   | Sunken Shrine transition and Wyrm Key quest reward            |
| `HARDWARE_TESTS.md` | Running real-hardware verification and balance backlog |
| `tools/validate.py` | Static source, graphics allocation, map, and memory checks |

The video and PSG access patterns follow the existing repo code
(`software/speedscript/screen.asm` and `software/dbasic/ay38910.asm`), and the
custom tiles use the character generator the way `V_PATTERN` does in
`software/dbasic/screencmds.asm`.
Tile bitmap sources use bit 7 for the left edge; the upload routine reverses
each scanline for the video card's bit-0-left character generator.
