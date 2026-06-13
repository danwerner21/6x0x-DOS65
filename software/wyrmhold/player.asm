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
        STA     bosskilled
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
        BEQ     @ck_dung
        JMP     enter_town
@ck_dung:
        LDA     tmp0
        AND     #P_DUNG
        BEQ     @ck_exit
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
; consume a unit of food per step
        LDA     pfood
        ORA     pfood+1
        BEQ     @nofood
        SEC
        LDA     pfood
        SBC     #1
        STA     pfood
        LDA     pfood+1
        SBC     #0
        STA     pfood+1
@nofood:
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
        PRINTMSG_MSG m_town
        RTS

;----------------------------------------------------------------
; enter_dungeon - switch to the dungeon map and spawn its monsters.
;----------------------------------------------------------------
enter_dungeon:
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
        PRINTMSG_MSG m_dungeon
        RTS

;----------------------------------------------------------------
; leave_interior - return to the overworld at the saved position.
;----------------------------------------------------------------
leave_interior:
        JSR     sfx_door
        JSR     decode_world
        LDA     #LOC_WORLD
        STA     loc
        LDA     owretx
        STA     px
        LDA     owrety
        STA     py
        JSR     spawn_overworld_monsters
        LDA     #1
        STA     did_move
        PRINTMSG_MSG m_world
        RTS
