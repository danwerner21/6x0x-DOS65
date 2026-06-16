# Wyrmhold Hardware Test Backlog

Use this file as the running verification list while development continues
without local 6502PC access.

Status values:

- **Passed:** verified by a remote build or on real hardware.
- **Pending:** implemented but not yet exercised on real hardware.
- **Retest:** previously passed behavior affected by later changes.
- **Failed:** defect reproduced; add a short note and leave it listed.

## Build And Startup

| Status | Test |
|--------|------|
| **Passed** | Persistent-encounter source built successfully remotely (June 15, 2026). |
| **Passed** | Title-timing RNG build link map ends at `$7337`, below the `$9000` target. |
| **Passed** | Local static validator completes with no errors; known legacy conditions remain warnings. |
| **Passed** | Run `make validate` on the remote Linux build host. |
| **Passed** | Title-timing RNG revision built successfully remotely (June 15, 2026). |
| **Passed** | In-game-help revision built successfully remotely (June 15, 2026). |
| **Pending** | Build the current reed-ford shortcut revision remotely. |
| **Pending** | Launch from DOS/65, view title, dismiss title, and reach the overworld. |
| **Pending** | Launch twice with noticeably different title-screen wait times; confirm initial monster placement differs. |
| **Pending** | Confirm both a rapid title keypress and a long wait begin normally without a frozen or repeating RNG. |
| **Pending** | Using the same title key after different delays produces different encounter or damage sequences. |
| **Pending** | Quit with `Q`, confirm both Yes and No paths, and verify a clean DOS/65 return. |

## Quest And Locations

| Status | Test |
|--------|------|
| **Passed** | Receive the Wyrm Key quest from King Aldren. |
| **Retest** | Defeat the Wyrm Warden, receive the key, and return to Aldren to open the lair. |
| **Pending** | Enter and leave Eastmere, Valehaven, the castle, Sunken Shrine, and dragon dungeon. |
| **Pending** | Defeat the dragon, see the return-to-Aldren objective, and trigger the ending at the castle. |
| **Pending** | Verify treasure, shop purchases, healing, provisions, and equipment upgrades. |
| **Pending** | Eastmere uses its dense coastal map and displays `Eastmere` in the panel. |
| **Pending** | Valehaven uses its open canal map and displays `Valehaven` in the panel. |
| **Pending** | Eastmere sells cheaper equipment; Valehaven sells cheaper healing and more provisions. |

## Regions And Encounters

| Status | Test |
|--------|------|
| **Pending** | `Where` shows `Northreach` in rows 0-20, `Wyrmhold Vale` in rows 21-43, and `Sunken March` in rows 44-63. |
| **Pending** | Crossing rows 20/21 and 43/44 updates the displayed region without visual corruption. |
| **Pending** | Northreach favors orcs and skeletons; Wyrmhold Vale is mixed; Sunken March favors snakes, thieves, and trolls. |
| **Pending** | After Aldren opens the dragon's lair, newly spawned overworld groups feel noticeably tougher. |
| **Pending** | Entering and leaving an interior restores surviving overworld monsters at the same coordinates, with their previous health and behavior state. |
| **Pending** | Returning from an interior only adds reinforcements when fewer than three overworld monsters remain. |
| **Pending** | Repeatedly entering and leaving locations does not create a fresh full encounter group each time. |

## Terrain Identity

| Status | Test |
|--------|------|
| **Pending** | Roads and bridges display `Road: saves ration` and do not consume the normal travel ration. |
| **Pending** | Forests display `Forest cover`; monsters more than five cells away stop pursuing until the player leaves cover or comes closer. |
| **Pending** | Hills display `High ground +2` and add exactly two damage to player attacks. |
| **Pending** | Marshes display `Marsh: costly`, consume two base rations, and can poison the player. |
| **Pending** | Leather reduces marsh poison frequency; plate adds its extra ration cost on every terrain. |
| **Pending** | Poison status overrides terrain text until cured or expired. |

## Handcrafted Discoveries

| Status | Test |
|--------|------|
| **Pending** | Reaching the northern forest cache at `(30,4)` awards 40 gold and 75 provisions once. |
| **Pending** | Reaching the hilltop cairn at `(31,26)` grants five maximum health, fully heals, and cures poison once. |
| **Pending** | Reaching the old waystone at `(29,40)` displays its route clue once; pressing `T` there displays it again. |
| **Pending** | Pressing `T` at the reed ford endpoints `(20,57)` and `(32,59)` crosses to the other side, costs 6 HP and up to 20 provisions, and consumes a turn. |
| **Pending** | The reed ford refuses crossing at 6 HP or less and refuses if a monster occupies the destination. |
| **Pending** | Discovery rewards persist after entering and leaving interiors and do not trigger a second time. |
| **Pending** | Discovery messages and sounds are noticeable without disrupting the turn loop. |

## Combat And Monster Identity

| Status | Test |
|--------|------|
| **Pending** | Snake surges, can poison, and leather armor sometimes resists venom. |
| **Pending** | Skeleton guards until approached, thief steals then flees, and troll moves slowly and regenerates. |
| **Pending** | `G` guard consumes a turn and reduces melee damage without blocking theft, poison, or dragon breath. |
| **Pending** | Dagger critical hits, sword reliability, and axe damage variance are perceptibly distinct. |
| **Pending** | Plate armor consumes an extra provision per successful step. |
| **Pending** | Dragon breath warning lane renders clearly, can be dodged, deals damage, and clears correctly. |

## Presentation And Regression

| Status | Test |
|--------|------|
| **Pending** | Press `?` from the overworld and every interior; the field guide opens without consuming a turn and closes on the next key. |
| **Pending** | The field guide border, controls, terrain hints, and current objective fit cleanly with no stale characters. |
| **Pending** | Closing the field guide fully restores the viewport, panel, monsters, breath warning, and message log. |
| **Pending** | Player, monsters, terrain, landmarks, and breath glyphs render in the correct order and orientation. |
| **Pending** | Panel values, region names, objective text, status text, messages, and shop text fit without stale characters. |
| **Pending** | Attack, hurt, critical, poison, door, treasure, quest, victory, and defeat sounds are distinct and not excessively slow. |
| **Pending** | Death by wounds and starvation both show the correct defeat screen. |
| **Pending** | Complete a new-game-to-victory playthrough without a dead end or crash. |

## Balance Notes

Record playthrough observations here before changing values.

| Date | Build | Area/System | Observation | Follow-up |
|------|-------|-------------|-------------|-----------|
| | | | | |
