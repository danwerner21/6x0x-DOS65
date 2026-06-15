;______________________________________________________________________________
;  entity.asm - monsters: table, spawning, simple AI, rendering
;
;  Parallel arrays of MAXMON slots.  A slot with mon_type == M_NONE
;  is empty.  Monsters live on the currently active map (overworld
;  or dungeon).  Towns have no monsters.
;______________________________________________________________________________

;----------------------------------------------------------------
; Per-monster-type stat tables (indexed by M_* id)
;   mtype_glyph : 2x2 metatile base glyph
;   mtype_color : color cell
;   mtype_hp    : starting HP
;   mtype_atk   : attack dice sides (damage 1..atk)
;   mtype_xp    : XP awarded on kill
;   mtype_gold  : gold dropped (max; rolled 0..gold)
;----------------------------------------------------------------
mtype_glyph:
        .BYTE   ' '             ; 0 none
        .BYTE   MG_ORC          ; 1 orc
        .BYTE   MG_SNAKE        ; 2 snake
        .BYTE   MG_SKELETON     ; 3 skeleton
        .BYTE   MG_THIEF        ; 4 thief
        .BYTE   MG_TROLL        ; 5 troll
        .BYTE   MG_BOSS         ; 6 boss (dragon)
        .BYTE   MG_WARDEN       ; 7 Wyrm Warden
mtype_color:
        .BYTE   C_MONST         ; 0
        .BYTE   COLOR(CO_BRRED, CO_BLACK); orc
        .BYTE   COLOR(CO_BRYELLOW,CO_BLACK); snake
        .BYTE   COLOR(CO_BRWHITE,CO_BLACK); skeleton
        .BYTE   COLOR(CO_VIOLET,CO_BLACK); thief
        .BYTE   COLOR(CO_BRTURQ,CO_BLACK); troll
        .BYTE   C_BOSS          ; boss
        .BYTE   COLOR(CO_BRPURPLE,CO_BLACK); Wyrm Warden
mtype_hp:
        .BYTE   0, 6, 4, 8, 5, 14, 40, 24
mtype_atk:
        .BYTE   0, 4, 3, 5, 3, 7, 12, 8
mtype_xp:
        .BYTE   0, 3, 2, 5, 4, 10, 50, 25
mtype_gold:
        .BYTE   0, 4, 1, 3, 12, 8, 40, 0

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
        AND     #(P_TOWN|P_DUNG|P_CASTLE)
        BNE     @skip           ; don't sit on a location entrance
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
; spawn_dungeon_monsters - populate the three authored dungeon zones.
;----------------------------------------------------------------
spawn_dungeon_monsters:
        JSR     mon_clear_all
        LDA     #BREATH_NONE
        STA     boss_breath_dir
; once slain, the dragon and its guards do not return
        LDA     queststate
        CMP     #QUEST_DRAGON_DEAD
        BCS     @done
; the dragon waits in the open southern chamber
        LDA     #DRAGON_X
        STA     tgtx
        LDA     #DRAGON_Y
        STA     tgty
        LDA     #M_BOSS
        JSR     mon_spawn
; one authored guard in each approach zone
        LDA     #9
        STA     tgtx
        LDA     #5
        STA     tgty
        LDA     #M_SKELETON
        JSR     mon_spawn
        LDA     #14
        STA     tgtx
        LDA     #11
        STA     tgty
        LDA     #M_THIEF
        JSR     mon_spawn
        LDA     #23
        STA     tgtx
        LDA     #15
        STA     tgty
        LDA     #M_TROLL
        JSR     mon_spawn
@done:
        RTS

;----------------------------------------------------------------
; spawn_shrine_monsters - the Wyrm Warden appears only while the
; key quest is active. Once defeated, the shrine remains cleared.
;----------------------------------------------------------------
spawn_shrine_monsters:
        JSR     mon_clear_all
        LDA     queststate
        CMP     #QUEST_FIND_KEY
        BNE     @done
        LDA     #WARDEN_X
        STA     tgtx
        LDA     #WARDEN_Y
        STA     tgty
        LDA     #M_WARDEN
        JSR     mon_spawn
@done:
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
; draw_dragon_telegraph_vram - overlay the warned fire lane while
; the dragon is preparing a breath attack.
; Called by render_view while video is paged in.
;----------------------------------------------------------------
draw_dragon_telegraph_vram:
        LDA     loc
        CMP     #LOC_DUNG
        BNE     @done
        LDA     boss_breath_dir
        BEQ     @done
; find the live dragon
        LDX     #0
@find:
        LDA     mon_type,X
        CMP     #M_BOSS
        BEQ     @found
        INX
        CPX     #MAXMON
        BNE     @find
        RTS
@found:
        LDA     mon_x,X
        STA     tgtx
        LDA     mon_y,X
        STA     tgty
@lane:
        JSR     dragon_advance_target
        JSR     tileat
        LDX     tgttile
        LDA     tile_prop,X
        AND     #P_PASS
        BEQ     @done
        LDA     #MG_BREATH
        STA     cnt0
        LDA     #C_BREATH
        STA     cnt1
        JSR     plot_view_cell
        JMP     @lane
@done:
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
        CMP     #M_WARDEN
        BNE     @ckdragon
        JSR     warden_act
        JMP     @next
@ckdragon:
        CMP     #M_BOSS
        BNE     @normal
        JSR     dragon_act
        JMP     @next
@normal:
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

;----------------------------------------------------------------
; dragon_act - breathe down a warned straight lane on the following
; turn, otherwise warn when the player is in clear line of sight.
; The dragon uses normal melee and pursuit when no breath is ready.
;----------------------------------------------------------------
dragon_act:
        LDA     boss_breath_dir
        BEQ     @ready
        JMP     dragon_fire_breath
@ready:
        JSR     mon_is_adjacent
        BCS     @normal
        JSR     dragon_prepare_breath
        BCS     @done
@normal:
        JMP     mon_step_or_attack
@done:
        RTS

; dragon_prepare_breath - arm a breath attack if the player and
; dragon share a clear row or column. Returns C=1 when armed.
dragon_prepare_breath:
        LDX     monidx
        LDA     mon_y,X
        CMP     py
        BNE     @vertical
; same row
        LDA     mon_x,X
        CMP     px
        BCC     @right
        LDA     #BREATH_LEFT
        BNE     @armed
@right:
        LDA     #BREATH_RIGHT
        BNE     @armed
@vertical:
        LDA     mon_x,X
        CMP     px
        BNE     @no
        LDA     mon_y,X
        CMP     py
        BCC     @down
        LDA     #BREATH_UP
        BNE     @armed
@down:
        LDA     #BREATH_DOWN
@armed:
        STA     boss_breath_dir
        JSR     dragon_line_hits_player
        BCC     @blocked
        JSR     sfx_breath_warn
        PRINTMSG_MSG m_breath_warn
        SEC
        RTS
@blocked:
        LDA     #BREATH_NONE
        STA     boss_breath_dir
@no:
        CLC
        RTS

; dragon_fire_breath - resolve the prepared lane after the player
; has had one turn to step clear.
dragon_fire_breath:
        JSR     dragon_line_hits_player
        BCC     @miss
        LDA     #BREATH_NONE
        STA     boss_breath_dir
        JMP     dragon_fire_player
@miss:
        LDA     #BREATH_NONE
        STA     boss_breath_dir
        JSR     sfx_breath
        PRINTMSG_MSG m_breath_miss
        RTS

; dragon_line_hits_player - trace the prepared lane from the dragon
; until blocked. Returns C=1 if the player is currently in it.
dragon_line_hits_player:
        LDX     monidx
        LDA     mon_x,X
        STA     tgtx
        LDA     mon_y,X
        STA     tgty
@trace:
        JSR     dragon_advance_target
        JSR     tileat
        LDX     tgttile
        LDA     tile_prop,X
        AND     #P_PASS
        BEQ     @no
        LDA     tgtx
        CMP     px
        BNE     @trace
        LDA     tgty
        CMP     py
        BNE     @trace
        SEC
        RTS
@no:
        CLC
        RTS

; dragon_advance_target - move tgtx/tgty one cell in the prepared
; breath direction.
dragon_advance_target:
        LDA     boss_breath_dir
        CMP     #BREATH_UP
        BEQ     @up
        CMP     #BREATH_DOWN
        BEQ     @down
        CMP     #BREATH_LEFT
        BEQ     @left
        INC     tgtx
        RTS
@up:
        DEC     tgty
        RTS
@down:
        INC     tgty
        RTS
@left:
        DEC     tgtx
        RTS

;----------------------------------------------------------------
; warden_act - the Wyrm Warden surges up to two tiles per turn.
; It stops after closing to melee range, so it attacks at most once
; per turn while remaining much harder to outrun than normal foes.
;----------------------------------------------------------------
warden_act:
        JSR     mon_is_adjacent
        BCS     @attack
        JSR     mon_step_or_attack
        JSR     mon_is_adjacent
        BCS     @done
        JMP     mon_step_or_attack
@attack:
        JMP     mon_step_or_attack
@done:
        RTS

; mon_is_adjacent - C=1 if monster in monidx is beside the player.
mon_is_adjacent:
        LDX     monidx
        SEC
        LDA     px
        SBC     mon_x,X
        JSR     abs_a
        CMP     #2
        BCS     @no
        LDX     monidx
        SEC
        LDA     py
        SBC     mon_y,X
        JSR     abs_a
        CMP     #2
        BCS     @no
        SEC
        RTS
@no:
        CLC
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
        JMP     monster_attacks_player; in combat.asm
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
; monsters do not occupy map-transition landmarks
        LDA     tile_prop,X
        AND     #(P_TOWN|P_DUNG|P_CASTLE)
        BNE     @done
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
