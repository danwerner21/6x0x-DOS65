;______________________________________________________________________________
;  player.asm - player movement, map transitions, treasure
;______________________________________________________________________________

;----------------------------------------------------------------
; Player starting state on the overworld.
;----------------------------------------------------------------
PLAYER_START_X  = 20
PLAYER_START_Y  = 9

player_init:
        LDA     #PLAYER_START_X
        STA     px
        LDA     #PLAYER_START_Y
        STA     py
        LDA     #30
        STA     phealth
        STA     pmaxhp
        LDA     #1
        STA     plevel
        LDA     #0
        STA     pxp
        STA     pxp+1
        LDA     #50
        STA     pgold
        LDA     #0
        STA     pgold+1
        SETW16  pfood, 250
        LDA     #1
        STA     pweapon         ; dagger
        LDA     #0
        STA     parmor          ; clothes
        LDA     #LOC_WORLD
        STA     loc
        LDA     #0
        STA     town_id
        STA     discovery_flags
        STA     bosskilled
        STA     queststate
        STA     boss_breath_dir
        STA     poison_turns
        STA     turn_phase
        STA     guard_active
        STA     attack_flags
        RTS

;----------------------------------------------------------------
; try_move - attempt to move the player by (dx,dy).
;   IN: dx, dy = signed -1/0/+1 deltas
;   Effects: combat on bump, terrain block, town/dungeon entry,
;   treasure pickup, exit interiors, food cost.  Monsters act after
;   a successful step (handled by caller via did_move flag).
;   OUT: did_move (game var) = 1 if a turn was consumed.
;----------------------------------------------------------------
try_move:
        LDA     #0
        STA     did_move
; target = (px+dx, py+dy)
        CLC
        LDA     px
        ADC     dx
        STA     tgtx
        CLC
        LDA     py
        ADC     dy
        STA     tgty

; monster there? -> attack (consumes the turn)
        JSR     mon_at
        BCC     @noenemy
; X = monster slot ; resolve combat
        JSR     player_attacks_monster
        LDA     #1
        STA     did_move
        RTS
@noenemy:
; look at the terrain
        JSR     tileat          ; -> tgttile
        LDX     tgttile
        LDA     tile_prop,X
        STA     tmp0            ; property bits
        AND     #P_PASS
        BNE     @passable
; blocked
        JSR     sfx_blocked
        PRINTMSG_MSG m_blocked
        RTS                     ; no turn consumed on a wall bump
@passable:
; check special tiles
        LDA     tmp0
        AND     #P_TOWN
        BEQ     @ck_castle
        JSR     save_overworld_monsters
        JMP     enter_town
@ck_castle:
        LDA     tmp0
        AND     #P_CASTLE
        BEQ     @ck_dung
        JSR     save_overworld_monsters
        JMP     enter_castle
@ck_dung:
        LDA     tmp0
        AND     #P_DUNG
        BEQ     @ck_exit
        JSR     save_overworld_monsters
        JMP     enter_dungeon
@ck_exit:
        LDA     tmp0
        AND     #P_EXIT
        BEQ     @ck_treas
        JMP     leave_interior
@ck_treas:
; commit the move first
        LDA     tgtx
        STA     px
        LDA     tgty
        STA     py
        LDA     #1
        STA     did_move
; treasure?
        LDA     tmp0
        AND     #P_TREAS
        BEQ     @food
        JSR     collect_treasure
@food:
; terrain sets the base travel cost: roads and bridges are free,
; ordinary ground costs one ration, and marshes cost two.
        LDA     #1
        STA     tmp2
        LDA     loc
        BNE     @armor_cost
        LDA     tgttile
        CMP     #T_ROAD
        BEQ     @easy_travel
        CMP     #T_BRIDGE
        BEQ     @easy_travel
        CMP     #T_MARSH
        BNE     @armor_cost
        LDA     #2
        STA     tmp2
        JMP     @armor_cost
@easy_travel:
        LDA     #0
        STA     tmp2
@armor_cost:
; plate armor adds one ration to any successful step
        LDX     parmor
        CPX     #3
        BNE     @consume_food
        INC     tmp2
@consume_food:
        LDA     tmp2
        BEQ     @terrain_effect
        JSR     consume_food_unit
        DEC     tmp2
        JMP     @consume_food
@terrain_effect:
; marshes may poison the traveler after movement is committed
        LDA     loc
        BNE     @discoveries
        LDA     tgttile
        CMP     #T_MARSH
        BNE     @discoveries
        JSR     marsh_hazard
@discoveries:
        JSR     check_overworld_discovery
        RTS

;----------------------------------------------------------------
; consume_food_unit - consume one provision if any remain.
;----------------------------------------------------------------
consume_food_unit:
        LDA     pfood
        ORA     pfood+1
        BEQ     @done
        SEC
        LDA     pfood
        SBC     #1
        STA     pfood
        LDA     pfood+1
        SBC     #0
        STA     pfood+1
@done:
        RTS

;----------------------------------------------------------------
; marsh_hazard - marsh travel can inflict poison. Leather armor
; reduces the chance from one-in-eight to one-in-sixteen.
;----------------------------------------------------------------
marsh_hazard:
        LDX     parmor
        CPX     #1
        BNE     @normal_risk
        LDA     #16
        BNE     @roll
@normal_risk:
        LDA     #8
@roll:
        JSR     rng_d
        CMP     #1
        BNE     @safe
        LDA     poison_turns
        BNE     @refresh
        LDA     #$84            ; skip the immediate status tick
        STA     poison_turns
        JMP     @poisoned
@refresh:
        LDA     #4
        STA     poison_turns
@poisoned:
        JSR     sfx_poison
        PRINTMSG_MSG m_marsh_poison
@safe:
        RTS

;----------------------------------------------------------------
; check_overworld_discovery - award one-time authored discoveries
; when the player reaches their overworld coordinates.
;----------------------------------------------------------------
check_overworld_discovery:
        LDA     loc
        BEQ     :+
        RTS
:
        LDA     discovery_flags
        AND     #DISC_CACHE
        BNE     @cairn_check
        LDA     px
        CMP     #CACHE_X
        BNE     @cairn_check
        LDA     py
        CMP     #CACHE_Y
        BNE     @cairn_check
        JMP     discover_cache
@cairn_check:
        LDA     discovery_flags
        AND     #DISC_CAIRN
        BNE     @waystone_check
        LDA     px
        CMP     #CAIRN_X
        BNE     @waystone_check
        LDA     py
        CMP     #CAIRN_Y
        BNE     @waystone_check
        JMP     discover_cairn
@waystone_check:
        LDA     discovery_flags
        AND     #DISC_WAYSTONE
        BNE     @done
        LDA     px
        CMP     #WAYSTONE_X
        BNE     @done
        LDA     py
        CMP     #WAYSTONE_Y
        BNE     @done
        JMP     discover_waystone
@done:
        RTS

; A hunter's hidden cache provides both money and travel supplies.
discover_cache:
        LDA     discovery_flags
        ORA     #DISC_CACHE
        STA     discovery_flags
        CLC
        LDA     pgold
        ADC     #40
        STA     pgold
        LDA     pgold+1
        ADC     #0
        STA     pgold+1
        CLC
        LDA     pfood
        ADC     #75
        STA     pfood
        LDA     pfood+1
        ADC     #0
        STA     pfood+1
        JSR     sfx_treasure
        PRINTMSG_MSG m_discover_cache
        RTS

; The hilltop cairn grants a permanent vitality blessing.
discover_cairn:
        LDA     discovery_flags
        ORA     #DISC_CAIRN
        STA     discovery_flags
        CLC
        LDA     pmaxhp
        ADC     #5
        STA     pmaxhp
        STA     phealth
        LDA     #0
        STA     poison_turns
        JSR     sfx_levelup
        PRINTMSG_MSG m_discover_cairn
        RTS

; The old waystone gives a concise route clue.
discover_waystone:
        LDA     discovery_flags
        ORA     #DISC_WAYSTONE
        STA     discovery_flags
        JSR     sfx_treasure
        PRINTMSG_MSG m_discover_waystone
        RTS

;----------------------------------------------------------------
; overworld_use - contextual overworld interaction. The discovered
; waystone can be read again with T so its route clue is not lost.
;----------------------------------------------------------------
overworld_use:
        LDA     px
        CMP     #WAYSTONE_X
        BNE     @shortcut_a
        LDA     py
        CMP     #WAYSTONE_Y
        BNE     @shortcut_a
        PRINTMSG_MSG m_discover_waystone
        RTS
@shortcut_a:
        LDA     px
        CMP     #SHORTCUT_A_X
        BNE     @shortcut_b
        LDA     py
        CMP     #SHORTCUT_A_Y
        BNE     @shortcut_b
        LDA     #SHORTCUT_B_X
        STA     tgtx
        LDA     #SHORTCUT_B_Y
        STA     tgty
        JMP     shortcut_cross
@shortcut_b:
        LDA     px
        CMP     #SHORTCUT_B_X
        BNE     @nothing
        LDA     py
        CMP     #SHORTCUT_B_Y
        BNE     @nothing
        LDA     #SHORTCUT_A_X
        STA     tgtx
        LDA     #SHORTCUT_A_Y
        STA     tgty
        JMP     shortcut_cross
@nothing:
        PRINTMSG_MSG t_nothing
        RTS

; The reed ford is a deliberate shortcut through the Sunken March:
; useful, but costly enough that using it wounded is a bad idea.
shortcut_cross:
        LDA     phealth
        CMP     #SHORTCUT_HP_COST+1
        BCS     @can_cross
        JSR     sfx_blocked
        PRINTMSG_MSG m_shortcut_weak
        RTS
@can_cross:
        JSR     mon_at
        BCC     @destination_clear
        JSR     sfx_blocked
        PRINTMSG_MSG m_shortcut_blocked
        RTS
@destination_clear:
        SEC
        LDA     phealth
        SBC     #SHORTCUT_HP_COST
        STA     phealth

        LDX     #SHORTCUT_FOOD_COST
@food:
        LDA     pfood
        ORA     pfood+1
        BEQ     @move
        JSR     consume_food_unit
        DEX
        BNE     @food
@move:
        LDA     tgtx
        STA     px
        LDA     tgty
        STA     py
        LDA     #1
        STA     did_move
        JSR     sfx_move
        PRINTMSG_MSG m_shortcut_cross
        RTS

;----------------------------------------------------------------
; collect_treasure - pick up a chest at (tgtx,tgty): gold + maybe
; XP, then clear the tile to floor.
;----------------------------------------------------------------
collect_treasure:
; gold 5..20
        LDA     #16
        JSR     rng_d
        CLC
        ADC     #4
; add to gold (16-bit)
        CLC
        ADC     pgold
        STA     pgold
        LDA     pgold+1
        ADC     #0
        STA     pgold+1
; clear the chest tile -> floor
        LDA     #T_FLOOR
        JSR     settile
        JSR     sfx_treasure
        PRINTMSG_MSG m_treasure
        RTS

;----------------------------------------------------------------
; enter_town - switch to the town interior map.
;----------------------------------------------------------------
enter_town:
; identify the landmark before effects or map decoding use scratch state
        LDA     #TOWN_EASTMERE
        LDX     tgty
        CPX     #VALEHAVEN_Y
        BNE     @town_selected
        LDA     #TOWN_VALEHAVEN
@town_selected:
        STA     town_id
        JSR     sfx_door
        JSR     decode_town
        LDA     #LOC_TOWN
        STA     loc
        LDA     #TOWNW
        STA     locw
        LDA     #TOWNH
        STA     loch
; remember overworld position to restore on exit
        LDA     px
        STA     owretx
        LDA     py
        STA     owrety
; place player just inside, one tile NORTH of the door so the
; exit '+' is visible directly south of the player on entry.
        LDA     #14
        STA     px
        LDA     #TOWNH-3
        STA     py
; towns have no monsters
        JSR     mon_clear_all
        LDA     #1
        STA     did_move
        LDA     town_id
        BEQ     @eastmere_msg
        PRINTMSG_MSG m_valehaven
        RTS
@eastmere_msg:
        PRINTMSG_MSG m_eastmere
        RTS

;----------------------------------------------------------------
; enter_castle - switch to Wyrmhold Castle's audience chamber.
;----------------------------------------------------------------
enter_castle:
        JSR     sfx_door
        JSR     decode_castle
        LDA     #LOC_CASTLE
        STA     loc
        LDA     #CASTLEW
        STA     locw
        LDA     #CASTLEH
        STA     loch
        LDA     px
        STA     owretx
        LDA     py
        STA     owrety
; enter on the carpet, immediately north of the exit
        LDA     #15
        STA     px
        LDA     #CASTLEH-3
        STA     py
        JSR     mon_clear_all
        LDA     #1
        STA     did_move
        PRINTMSG_MSG m_castle
        RTS

;----------------------------------------------------------------
; enter_dungeon - dispatch cave landmarks. The southern-marsh cave
; is the Sunken Shrine; the dragon's lair stays sealed until the
; Wyrm Key has been returned to King Aldren.
;----------------------------------------------------------------
enter_dungeon:
        LDA     tgtx
        CMP     #KEY_SITE_X
        BNE     @dragon
        LDA     tgty
        CMP     #KEY_SITE_Y
        BNE     @dragon
        LDA     queststate
        CMP     #QUEST_FIND_KEY
        BCC     @shrine_dormant
        JMP     enter_shrine
@shrine_dormant:
        JSR     sfx_blocked
        PRINTMSG_MSG m_shrine_dormant
        RTS
@dragon:
        LDA     queststate
        CMP     #QUEST_DUNG_OPEN
        BCS     @enter
        JSR     sfx_blocked
        PRINTMSG_MSG m_dung_sealed
        RTS
@enter:
        JSR     sfx_door
        JSR     decode_dung
        LDA     #LOC_DUNG
        STA     loc
        LDA     #DUNGW
        STA     locw
        LDA     #DUNGH
        STA     loch
        LDA     px
        STA     owretx
        LDA     py
        STA     owrety
; start just east of the stairs-up '<' (at 1,1) so the exit
; is visible immediately to the west of the player.
        LDA     #2
        STA     px
        LDA     #1
        STA     py
        JSR     spawn_dungeon_monsters
        LDA     #1
        STA     did_move
        LDA     queststate
        CMP     #QUEST_DRAGON_DEAD
        BCS     @silent_lair
        PRINTMSG_MSG m_dungeon
        PRINTMSG_MSG m_dragon_wakes
        RTS
@silent_lair:
        PRINTMSG_MSG m_dungeon_empty
        RTS

;----------------------------------------------------------------
; leave_interior - return to the overworld at the saved position.
;----------------------------------------------------------------
leave_interior:
        JSR     sfx_door
        LDA     #BREATH_NONE
        STA     boss_breath_dir
        JSR     decode_world
        LDA     #LOC_WORLD
        STA     loc
        LDA     owretx
        STA     px
        LDA     owrety
        STA     py
        JSR     restore_overworld_monsters
        LDA     #1
        STA     did_move
        PRINTMSG_MSG m_world
        RTS
