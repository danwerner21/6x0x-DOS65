# Wyrmhold Improvement Plan

## Product Direction

Wyrmhold should become a compact, authored fantasy adventure rather than a
larger collection of loosely connected RPG systems.

The current engine is a strong foundation: it is fast, readable, colorful, and
already supports scrolling maps, interiors, combat, progression, shops, custom
graphics, and AY-3-8910 audio. The main weakness is that nearly every system
currently has only one meaningful outcome. The player explores, buys linear
upgrades, bumps monsters that all behave alike, enters one dungeon, and kills
one boss.

The target should be a polished 30-45 minute adventure with:

- A clear three-act quest and a world whose landmarks have purposes.
- Several meaningful player decisions without turning the game into a menu-heavy
  RPG.
- Monsters distinguished by behavior as well as graphics and statistics.
- Strong audiovisual feedback for movement, danger, discovery, and victory.
- Enough secrets and alternate approaches to reward a second playthrough.

The existing 80-column, 2x2 metatile engine should remain the primary renderer.
It is already fast and gives Wyrmhold a distinct identity. The hires experiment
can remain an optional research branch, but changing renderers would not solve
the game's central problem: it needs more authored situations and stronger
feedback.

## Current Review

### What Already Works

- The game starts quickly and the turn-based controls are responsive.
- The scrolling 22x10 world-tile viewport is a good fit for the machine.
- The custom 16x16 player, monster, terrain, and landmark art is readable.
- Colored terrain backgrounds make the world easy to parse.
- The title screen has a real three-channel musical theme.
- The panel, message log, shop overlay, and map transitions form a complete UI.
- Source modules have clear responsibilities and are straightforward to extend.
- The current image ends around `$551C`, leaving roughly 19 KB before the
  `$A000` video window.

### Why It Feels Flat

- The entire objective is stated immediately and never develops: find the
  dungeon and kill the dragon.
- The two overworld towns lead to the same town map and the same shop.
- The castle is decorative and cannot be entered or used.
- Roads, forests, hills, marshes, and bridges look different but have almost no
  gameplay identity.
- Every normal monster uses the same pursue-or-attack AI.
- Weapons and armor are linear numeric upgrades with no distinct play styles.
- Treasure is always a small random amount of gold.
- The dungeon is one maze floor with treasure and a stationary final goal.
- The boss is statistically stronger but does not behave like a boss encounter.
- The game is silent during normal exploration except for blocking sound effects.
- Combat feedback is mostly message text plus the same hit sound for both sides.
- There is no save/load support, quest log, dialog system, or contextual help.
- The RNG always starts from the same fixed seed, making runs more predictable
  than intended.

### Technical and Content Risks to Clean Up

- The main folder, `wyrm-hires`, and `wyrm-onetile` contain duplicate engine
  sources that can drift. The main folder should be declared canonical.
- Authored overworld rows are silently padded or truncated. Currently 55
  overworld source rows are not exactly 64 characters wide.
- The current town and castle metatile records are cross-labeled, and the
  mountain alternate appears identical to its base artwork. These may be
  deliberate, but they should be audited.
- `sfx_move` exists but is not used.
- Sound effects use busy waits, temporarily stopping all other activity.
- Only 8 permanent gameplay glyphs remain free at `$F8..$FF`; future visual
  expansion needs deliberate allocation or location-specific glyph loading.
- There are no automated static checks for map widths, tile tables, metatile
  records, glyph collisions, or memory budget.

## Design Principles

1. **Depth before breadth.** Improve the meaning of existing towns, castle,
   terrain, monsters, and dungeon before expanding the map.
2. **Every landmark does something.** A visually important place should provide
   story, service, danger, reward, or progression.
3. **Behavior creates identity.** A snake should feel different from a thief
   before either receives more statistics or artwork.
4. **Keep turns fast.** New decisions should use contextual actions and short
   prompts, not frequent large menus.
5. **Feedback should be immediate.** Important actions need a visual cue, a
   distinct sound, and concise text.
6. **Author for the hardware.** Preserve the readable palette, responsive text
   renderer, and simple controls.
7. **Build reusable content systems.** Add data-driven quest, dialog, encounter,
   and location tables instead of hard-coding every new event.

## Recommended Core Adventure

Give the existing world a simple three-act structure:

### Act I: The Wyrmhold Commission

- The castle becomes an enterable quest hub.
- The ruler explains that the dragon's lair is sealed by an ancient ward.
- The player receives a clear objective: recover the Wyrm Key from a dangerous
  landmark.
- The panel displays a short current objective.
- The first town provides provisions and healing; the second specializes in
  weapons and armor.

### Act II: Recover the Wyrm Key

- Convert one existing landmark or add one compact interior as the key location.
- Place a named miniboss there with unique behavior.
- Reward the player with the Wyrm Key and a permanent benefit, not just gold.
- Optionally place two clues in different towns so exploration has direction
  without requiring a large quest log.

### Act III: Enter the Wyrmhold

- The dungeon entrance opens only after the key is acquired.
- The dungeon gains two or three distinct zones, even if they remain in one map:
  entrance halls, flooded passages, and the dragon chamber.
- The dragon uses multiple behaviors and receives a short introduction.
- Victory returns to the castle for a proper ending rather than ending
  immediately on the killing blow.

**Implemented:** The dungeon now has three authored zones, the dragon warns and
fires down visible breath lanes, and victory requires returning to King Aldren.

This structure reuses almost every existing system while giving the player a
reason to care about the world.

The original dungeon and boss limitations above are now resolved by the
three-zone dungeon, warned breath lanes, and castle-return finale.

## Phased Roadmap

### Phase 0: Establish a Reliable Baseline

Do this before adding content so later changes remain easy to verify.

- Declare `software/wyrmhold` the canonical game source.
- Document the purpose and status of `wyrm-hires` and `wyrm-onetile`.
- Add a static validation script that checks:
  - Exact map row widths and row counts.
  - Tile IDs, color entries, properties, and variant-table lengths.
  - Metatile record order, four quadrants, and eight rows per glyph.
  - Duplicate or cross-labeled metatile artwork.
  - Glyph allocation collisions and remaining glyph capacity.
  - Final linked end address, with a target of staying below `$9000`.
- Fix the fixed RNG seed by mixing in title-screen key timing.
- Create a short hardware smoke-test checklist.
- Capture baseline screenshots or photographs for title, overworld, town,
  dungeon, combat, shop, victory, and defeat.

**Exit criteria:** The remote build is reproducible, malformed map/art data
cannot silently enter the game, and the current game has a documented baseline.

### Phase 1: Build One Professional Vertical Slice

This is the highest-priority phase. It should establish the quality bar before
adding many new features.

- Make the castle interactive and enterable.
- Add a small dialog/prompt system using reusable speaker and text tables.
- Add quest state flags and an `Objective` line to the side panel.
- Introduce the Wyrm Key quest with a clear beginning, destination, and reward.
- Add one compact quest interior or encounter area.
- Add one named miniboss with a unique movement or attack pattern.
- Add a distinct quest-start cue, discovery cue, and quest-complete cue.
- Add brief visual feedback for receiving an item and defeating the miniboss.
- Return to the castle for acknowledgment and permission to enter the dungeon.

**Exit criteria:** A new player can understand the objective without reading the
README, complete a satisfying 10-minute quest, and describe at least one
memorable event afterward.

### Phase 2: Give Combat and Monsters Identity

Keep bump combat as the fast default, but make encounters require observation.

- Add behavior flags or behavior routines per monster type:
  - **Snake:** fast, fragile, and may inflict short poison.
  - **Orc:** straightforward pursuer and baseline enemy.
  - **Skeleton:** guards rooms or wakes when approached.
  - **Thief:** steals gold, then tries to flee.
  - **Troll:** slow, powerful, and regenerates unless finished quickly.
  - **Dragon:** breath attack at range plus a close-combat phase. **Implemented.**
- Add a `Wait/Guard` action so the player can control engagement distance.
- Make weapons distinct rather than purely linear:
  - Dagger: low damage with a critical-hit chance.
  - Sword: reliable damage.
  - Axe: high damage with greater variance.
- Give armor a visible tradeoff or specialty instead of only flat reduction.
- Add separate sounds and brief color flashes for:
  - Player attack.
  - Player taking damage.
  - Critical hit.
  - Miss or blocked attack.
  - Poison/status effect.
  - Boss breath attack.
- Improve combat messages so important events are visible without becoming
  verbose.
- Balance encounters by region and progression rather than selecting every
  overworld monster from the same random table.

**Exit criteria:** Every monster can be identified by behavior without looking
at its glyph, and the dragon encounter requires more than repeated bump attacks.

### Phase 3: Make the World Feel Authored

- Give each town a unique map, name, visual palette, and service.
- Turn decorative landmarks into useful locations, encounters, or clues.
- Add named regions and show the current region in the panel.
- Give terrain modest gameplay identity:
  - Roads reduce food use or avoid random encounters.
  - Forests limit sight or favor ambushes.
  - Hills reveal landmarks or affect encounters.
  - Marshes cost extra food or risk poison.
- Add a small number of handcrafted discoveries:
  - A hidden cache.
  - A healing shrine.
  - A dangerous shortcut.
  - A traveler or signpost with useful information.
- Replace generic treasure with a data-driven reward table containing gold,
  food, healing items, equipment, and quest items.
- Add controlled encounter respawning so the world does not empty immediately
  or reset unnaturally whenever an interior is left.
- Reshape the overworld around routes, decisions, and sightlines after all
  landmarks have defined purposes.

**Exit criteria:** Every major landmark has a reason to visit, the two towns are
meaningfully different, and exploration regularly produces information or
rewards rather than only travel time.

### Phase 4: Presentation and Audio Polish

#### Graphics

- Perform a consistency pass over all gameplay metatiles at actual hardware
  scale and color.
- Reserve the strongest contrast for the player, enemies, exits, and objectives.
- Add location-specific glyph loading when entering towns or the dungeon,
  allowing richer interior art without consuming permanent glyph slots.
- Add a few composite map features made from multiple tiles: gates, ruins,
  bridges, shrines, and the dragon chamber.
- Add restrained turn-based animation:
  - Alternate water or torch art periodically.
  - Brief attack and damage flashes.
  - Door/chest state changes.
- Improve title, victory, and defeat screens with a consistent logo, framing,
  and short narrative text.

#### UI

- Add the current objective and important status effects to the side panel.
- Use modal bordered panels for dialog, discoveries, and major choices.
- Add an in-game help panel listing controls and current objective.
- Show equipment effects when purchasing or viewing equipment.
- Use consistent terminology, capitalization, border style, and color roles.
- Replace instructional map messages such as references to `'+'`, `'<'`, and
  `'S'` with natural descriptions once the graphics are clear enough.

#### Sound

- Keep the existing three-channel title theme.
- Refactor sound playback into a small cooperative sequencer shared by music and
  effects, avoiding long busy waits.
- Add short location themes or ambient loops for overworld, town, dungeon, and
  boss encounter.
- Duck or temporarily simplify music while an important effect plays.
- Give discoveries, quest progress, enemy attacks, purchases, and errors
  distinct cues.
- Add a sound on/off option if continuous music is implemented.

**Exit criteria:** Important events can be understood from visual and audio
feedback before reading the message log, and each major location has a distinct
atmosphere.

### Phase 5: Progression, Persistence, and Release Quality

- Rebalance food, gold, healing, XP, equipment costs, and encounter strength
  around the intended 30-45 minute run.
- Add one or two consumable item types, keeping inventory deliberately small.
- Add save/load support through DOS/65 if hardware testing shows it is reliable.
  Save only compact state: player data, quest flags, collected treasures, and
  location.
- Add a proper ending sequence and brief credits.
- Add version text to the title or help screen.
- Update the README with the final controls, premise, screenshots, build steps,
  and gameplay description.
- Run complete hardware playthroughs from new game to every ending/failure mode.

**Exit criteria:** A complete run has a clear arc, no dead ends, stable balance,
and enough polish to present as a finished 6502PC game rather than a technology
demo.

## Suggested Data Architecture

The new systems should remain small and table-driven.

- `quest_flags`: bitfield for quest start, key acquired, dungeon unlocked,
  miniboss defeated, and ending state.
- `location_table`: map pointer, dimensions, palette/theme, spawn table, entry
  point, and exit behavior.
- `dialog_table`: speaker/color plus pointers to short text pages and optional
  condition/action fields.
- `encounter_table`: monster type weights by region or location.
- `monster_behavior`: behavior ID or flags indexed by monster type.
- `reward_table`: reward type, amount, message, and optional quest flag.
- `objective_table`: short panel text selected from quest state.

Town, dungeon, and quest interiors can continue sharing `locmap`; decode the
required map when entering a location. Permanent RAM use should remain modest.

Because permanent custom glyph space is nearly full, use one of these strategies
for new visuals:

1. Prefer composite landmarks built from existing terrain tiles.
2. Reuse glyph IDs for location-specific art loaded on entry.
3. Reserve the remaining `$F8..$FF` range for universally visible high-value
   assets.

## Priority Backlog

### Must Have

- Interactive castle and clear quest structure.
- Distinct monster behaviors.
- A real dragon encounter. **Implemented.**
- Unique towns or clearly differentiated town services.
- Objective display and contextual dialog.
- Better combat and discovery feedback.
- Static map/metatile/memory validation.
- Hardware-tested balance and complete ending sequence.

### Should Have

- Region-based encounters and meaningful terrain effects.
- Location-specific music or ambience.
- Non-blocking/cooperative sound effects.
- Data-driven rewards and a very small consumable inventory.
- Location-specific glyph loading.
- Save/load support.

### Could Have

- Additional dungeon floor or optional ruin.
- Day/night palette changes.
- Alternate ending or optional superboss.
- Hires presentation experiment after the main game is complete.

## First Implementation Milestone

Implement **The Wyrmhold Commission** before any other large feature:

1. Add quest flags and objective text. **Implemented.**
2. Make the castle enterable. **Implemented.**
3. Add a compact castle audience chamber and ruler interaction. **Implemented.**
4. Give the player the Wyrm Key objective. **Implemented.**
5. Add one quest location and one unique miniboss. **Implemented.**
6. Return to the castle for a reward and unlock the dungeon. **Implemented.**
7. Add the associated sounds, messages, and visual feedback. **Implemented.**
8. Playtest the slice on hardware and use it to set the quality bar.
   **Implemented for the Wyrm Key quest; dragon finale playtest pending.**

This milestone touches story, world interaction, UI, combat, graphics, sound,
and progression in a controlled scope. If it feels good, the rest of the
roadmap becomes expansion and refinement of proven systems.

## Definition of Done

Wyrmhold is ready to call professional when:

- A first-time player understands the premise, controls, and current objective
  from the game itself.
- The first meaningful discovery or decision occurs within two minutes.
- Each major landmark and monster has a distinct gameplay purpose.
- The player receives clear visual, audio, and textual feedback for important
  actions.
- The complete adventure has a deliberate beginning, escalation, boss encounter,
  and ending.
- The game remains responsive on real 6502PC hardware.
- Static validation passes, the linked image remains below `$9000`, and a full
  hardware playthrough completes without defects or dead ends.
