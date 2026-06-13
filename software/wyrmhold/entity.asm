;______________________________________________________________________________
;  entity.asm - monsters: table, spawning, simple AI, rendering
;
;  Parallel arrays of MAXMON slots.  A slot with mon_type == M_NONE
;  is empty.  Monsters live on the currently active map (overworld
;  or dungeon).  Towns have no monsters.
;______________________________________________________________________________

;----------------------------------------------------------------
; Per-monster-type stat tables (indexed by M_* id)
;   mtype_glyph : ASCII glyph
;   mtype_color : color cell
;   mtype_hp    : starting HP
;   mtype_atk   : attack dice sides (damage 1..atk)
;   mtype_xp    : XP awarded on kill
;   mtype_gold  : gold dropped (max; rolled 0..gold)
;----------------------------------------------------------------
mtype_glyph:
        .BYTE   ' '             ; 0 none
        .BYTE   G_ORC           ; 1 orc
        .BYTE   G_SNAKE         ; 2 snake
        .BYTE   G_SKELETON      ; 3 skeleton
        .BYTE   G_THIEF         ; 4 thief
        .BYTE   G_TROLL         ; 5 troll
        .BYTE   G_BOSS          ; 6 boss (dragon)
mtype_color:
        .BYTE   C_MONST         ; 0
        .BYTE   COLOR(CO_BRGREEN,CO_BLACK)  ; orc
        .BYTE   COLOR(CO_GREEN, CO_BLACK)   ; snake
        .BYTE   COLOR(CO_BRWHITE,CO_BLACK)  ; skeleton
        .BYTE   COLOR(CO_BRYELLOW,CO_BLACK) ; thief
        .BYTE   COLOR(CO_BRCYAN,CO_BLACK)   ; troll
        .BYTE   C_BOSS                      ; boss
mtype_hp:
        .BYTE   0, 6, 4, 8, 5, 14, 40
mtype_atk:
        .BYTE   0, 4, 3, 5, 3, 7, 12
mtype_xp:
        .BYTE   0, 3, 2, 5, 4, 10, 50
mtype_gold:
        .BYTE   0, 4, 1, 3, 12, 8, 40

;----------------------------------------------------------------
; mon_clear_all - empty every monster slot.
;----------------------------------------------------------------
mon_clear_all:
        LDX     #0
        LDA     #M_NONE
@c:
        STA     mon_type,X
        INX
        CPX     #MAXMON
        BNE     @c
        RTS

;----------------------------------------------------------------
; mon_find_free - find an empty slot.  Returns X = slot, C=1 ok,
;                 C=0 if table full.
;----------------------------------------------------------------
mon_find_free:
        LDX     #0
@f:
        LDA     mon_type,X
        CMP     #M_NONE
        BEQ     @found
        INX
        CPX     #MAXMON
        BNE     @f
        CLC
        RTS
@found:
        SEC
        RTS

;----------------------------------------------------------------
; mon_spawn - create a monster.
;   IN: A=type, tgtx=x, tgty=y
;   Sets hp from the type table.  No-op if table full.
;----------------------------------------------------------------
mon_spawn:
        STA     tmp0            ; type
        JSR     mon_find_free
        BCC     @full
        LDA     tmp0
        STA     mon_type,X
        LDA     tgtx
        STA     mon_x,X
        LDA     tgty
        STA     mon_y,X
        LDY     tmp0
        LDA     mtype_hp,Y
        STA     mon_hp,X
@full:
        RTS

;----------------------------------------------------------------
; mon_at - is there a live monster at (tgtx,tgty)?
;          Returns X = slot and C=1 if found, else C=0.
;----------------------------------------------------------------
mon_at:
        LDX     #0
@a:
        LDA     mon_type,X
        CMP     #M_NONE
        BEQ     @nextslot
        LDA     mon_x,X
        CMP     tgtx
        BNE     @nextslot
        LDA     mon_y,X
        CMP     tgty
        BNE     @nextslot
        SEC
        RTS
@nextslot:
        INX
        CPX     #MAXMON
        BNE     @a
        CLC
        RTS

;----------------------------------------------------------------
; mon_kill - remove monster in slot X.
;----------------------------------------------------------------
mon_kill:
        LDA     #M_NONE
        STA     mon_type,X
        RTS

;----------------------------------------------------------------
; spawn_overworld_monsters - scatter a handful of monsters on the
; overworld at random passable land tiles near the centre.
;----------------------------------------------------------------
spawn_overworld_monsters:
        JSR     mon_clear_all
        LDA     #4              ; only a few roaming monsters at a time
        STA     cnt0
@loop:
        ; place in a ring roughly 6..21 tiles from the player: far
        ; enough not to crowd you, near enough to be encountered.
        ; offset = (rand(0..15) + 6), sign random per axis.
        LDA     #16
        JSR     rng_mod
        CLC
        ADC     #6              ; 6..21
        STA     tmp1            ; magnitude
        JSR     rng_next        ; random sign bit
        AND     #1
        BEQ     @xplus
        LDA     px
        SEC
        SBC     tmp1
        JMP     @setx
@xplus:
        LDA     px
        CLC
        ADC     tmp1
@setx:
        STA     tgtx
        LDA     #16
        JSR     rng_mod
        CLC
        ADC     #6
        STA     tmp1
        JSR     rng_next
        AND     #1
        BEQ     @yplus
        LDA     py
        SEC
        SBC     tmp1
        JMP     @sety
@yplus:
        LDA     py
        CLC
        ADC     tmp1
@sety:
        STA     tgty
        ; reject the player's own tile (don't spawn on top of you)
        LDA     tgtx
        CMP     px
        BNE     @okpos
        LDA     tgty
        CMP     py
        BEQ     @skip
@okpos:
        ; must be passable grass/forest (not water/mountain/town...)
        JSR     tileat
        LDX     tgttile
        LDA     tile_prop,X
        AND     #P_PASS
        BEQ     @skip
        LDA     tile_prop,X
        AND     #(P_TOWN|P_DUNG)
        BNE     @skip           ; don't sit on a town/dungeon tile
        ; not already occupied by a monster?
        JSR     mon_at
        BCS     @skip
        ; pick a random monster type 1..5
        LDA     #5
        JSR     rng_d           ; 1..5
        JSR     mon_spawn
@skip:
        DEC     cnt0
        BNE     @loop
        RTS

;----------------------------------------------------------------
; spawn_dungeon_monsters - place a few monsters and the boss.
;----------------------------------------------------------------
spawn_dungeon_monsters:
        JSR     mon_clear_all
        ; the boss waits at the far corner treasure room (bottom-right)
        LDA     #29
        STA     tgtx
        LDA     #18
        STA     tgty
        LDA     #M_BOSS
        JSR     mon_spawn
        ; a couple of guards on floor tiles
        LDA     #4
        STA     cnt0
@loop:
        LDA     #28
        JSR     rng_mod
        CLC
        ADC     #2
        STA     tgtx
        LDA     #16
        JSR     rng_mod
        CLC
        ADC     #2
        STA     tgty
        JSR     tileat
        LDX     tgttile
        CPX     #T_FLOOR
        BNE     @skip
        LDA     #2              ; orc or skeleton
        JSR     rng_d
        CLC
        ADC     #2              ; 3..4 -> skeleton/thief mix
        JSR     mon_spawn
@skip:
        DEC     cnt0
        BNE     @loop
        RTS

;----------------------------------------------------------------
; draw_monsters_vram - overlay all visible monsters in the viewport.
; Called by render_view while video is paged in.
;----------------------------------------------------------------
draw_monsters_vram:
        LDX     #0
        STX     monidx
@dm:
        LDX     monidx
        LDA     mon_type,X
        CMP     #M_NONE
        BEQ     @next
        ; set world coords + glyph/color, then plot if visible
        LDA     mon_x,X
        STA     tgtx
        LDA     mon_y,X
        STA     tgty
        LDY     mon_type,X
        LDA     mtype_glyph,Y
        STA     cnt0            ; glyph (cnt0/cnt1 survive rowbase)
        LDA     mtype_color,Y
        STA     cnt1            ; color
        JSR     plot_view_cell
@next:
        INC     monidx
        LDA     monidx
        CMP     #MAXMON
        BNE     @dm
        RTS

;----------------------------------------------------------------
; mon_act - move/attack for every monster (called once per turn).
; Monsters in LOC_TOWN never act (towns are safe).
;   A monster adjacent to the player attacks; otherwise it steps
;   one tile toward the player if the destination is passable and
;   unoccupied.
;----------------------------------------------------------------
mon_act:
        LDA     loc
        CMP     #LOC_TOWN
        BEQ     @ret            ; safe in town
        LDX     #0
        STX     monidx
@ma:
        LDX     monidx
        LDA     mon_type,X
        CMP     #M_NONE
        BEQ     @next
        ; distance to player: dxv = px - mon_x ; dyv = py - mon_y
        ; adjacency test (|dx|<=1 && |dy|<=1 && not both 0)
        JSR     mon_step_or_attack
@next:
        INC     monidx
        LDA     monidx
        CMP     #MAXMON
        BNE     @ma
@ret:
        RTS

; mon_step_or_attack - for monster in slot monidx (X on entry).
mon_step_or_attack:
        ; compute signed dx = px - mon_x  -> tmp0 ; dy -> tmp1
        LDX     monidx
        SEC
        LDA     px
        SBC     mon_x,X
        STA     tmp0            ; dx (signed)
        SEC
        LDA     py
        SBC     mon_y,X
        STA     tmp1            ; dy (signed)
        ; |dx| <= 1 ?
        LDA     tmp0
        JSR     abs_a
        STA     tmp2            ; |dx|
        LDA     tmp1
        JSR     abs_a
        STA     tmp3            ; |dy|
        ; adjacency: |dx|<=1 and |dy|<=1
        LDA     tmp2
        CMP     #2
        BCS     @move
        LDA     tmp3
        CMP     #2
        BCS     @move
        ; adjacent (and since one of them is the player, not same cell)
        ; -> attack the player
        JMP     monster_attacks_player          ; in combat.asm
@move:
        ; step one tile toward player along the larger axis
        ; choose step in x: sign of dx
        LDX     monidx
        LDA     mon_x,X
        STA     tgtx
        LDA     mon_y,X
        STA     tgty
        ; prefer horizontal if |dx|>=|dy|
        LDA     tmp2
        CMP     tmp3
        BCC     @vert
        ; horizontal step
        LDA     tmp0
        BEQ     @vert           ; dx==0 -> try vertical
        BMI     @left
        INC     tgtx
        JMP     @trymove
@left:
        DEC     tgtx
        JMP     @trymove
@vert:
        LDA     tmp1
        BEQ     @done           ; dy==0 too -> no move
        BMI     @up
        INC     tgty
        JMP     @trymove
@up:
        DEC     tgty
@trymove:
        ; passable terrain?
        JSR     tileat
        LDX     tgttile
        LDA     tile_prop,X
        AND     #P_PASS
        BEQ     @done           ; blocked terrain
        ; not onto the player?
        LDA     tgtx
        CMP     px
        BNE     @okcell
        LDA     tgty
        CMP     py
        BEQ     @done           ; would land on player (shouldn't: adjacency handled)
@okcell:
        ; not onto another monster?
        JSR     mon_at
        BCS     @done           ; occupied
        ; commit move
        LDX     monidx
        LDA     tgtx
        STA     mon_x,X
        LDA     tgty
        STA     mon_y,X
@done:
        RTS

;----------------------------------------------------------------
; abs_a - A = |A| treating A as signed 8-bit.
;----------------------------------------------------------------
abs_a:
        CMP     #$80
        BCC     @pos
        EOR     #$FF
        CLC
        ADC     #1
@pos:
        RTS
