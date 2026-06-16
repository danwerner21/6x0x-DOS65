;______________________________________________________________________________
;  combat.asm - bump combat resolution, leveling, message builder
;______________________________________________________________________________

;----------------------------------------------------------------
; Message builder.  Compose a line in mbuf then push to the log.
;   mb_reset            - start a new line
;   mb_str  (A=lo,Y=hi) - append a 0-terminated string
;   mb_num  (A=byte)    - append a 0..255 value in decimal
;   mb_mname(A=type)    - append a monster type name
;   mb_flush            - push mbuf into the scrolling log
;----------------------------------------------------------------
mb_reset:
        LDA     #0
        STA     mblen
        RTS

mb_str:
        STA     strp
        STY     strp+1
        LDY     #0
@s:
        LDA     (strp),Y
        BEQ     @done
        LDX     mblen
        STA     mbuf,X
        INC     mblen
        INY
        BNE     @s
@done:
        RTS

; append A (0..255) as decimal
mb_num:
        STA     numarg
        LDA     #0
        STA     numarg+1
mb_num16:
; find highest place
        LDY     #8
@find:
        LDA     numarg+1
        CMP     nums+1,Y
        BCC     @nx
        BNE     @start
        LDA     numarg
        CMP     nums,Y
        BCS     @start
@nx:
        DEY
        DEY
        BNE     @find
@start:
        LDA     #'0'
        STA     numspace
@dig:
        LDA     numarg+1
        CMP     nums+1,Y
        BCC     @out
        BNE     @noc
        LDA     numarg
        SBC     nums,Y
        BCC     @out
        BCS     @sub
@noc:
        LDA     numarg
        SBC     nums,Y
@sub:
        INC     numspace
        STA     numarg
        LDA     numarg+1
        SBC     nums+1,Y
        STA     numarg+1
        JMP     @dig
@out:
        LDA     numspace
        LDX     mblen
        STA     mbuf,X
        INC     mblen
        DEY
        DEY
        BPL     @start
        RTS

; append a monster type name (A = type id)
mb_mname:
        ASL     A
        TAX
        LDA     mon_names,X
        LDY     mon_names+1,X
        JMP     mb_str

mb_flush:
        LDX     mblen
        LDA     #0
        STA     mbuf,X          ; NUL terminate
        LDA     #<mbuf
        LDY     #>mbuf
        JMP     msg_print

;----------------------------------------------------------------
; player_attacks_monster - X = monster slot.
;   Fists are weak, daggers can critically strike, swords are
;   reliable, and axes have the widest damage range.
;----------------------------------------------------------------
player_attacks_monster:
        STX     monidx
        LDA     #0
        STA     attack_flags
        LDX     pweapon
        CPX     #1
        BEQ     @dagger
        CPX     #2
        BEQ     @sword
        CPX     #3
        BEQ     @axe
; fists: 1..2
        LDA     #2
        JSR     rng_d
        JMP     @rolled
@dagger:
; dagger: 1..4, with a one-in-four critical strike for +4
        LDA     #4
        JSR     rng_d
        STA     tmp0
        LDA     #4
        JSR     rng_d
        CMP     #1
        BNE     @dagger_normal
        LDA     #ATTACK_CRITICAL
        STA     attack_flags
        LDA     tmp0
        CLC
        ADC     #4
        JMP     @rolled
@dagger_normal:
        LDA     tmp0
        JMP     @rolled
@sword:
; sword: reliable 4..7
        LDA     #4
        JSR     rng_d
        CLC
        ADC     #3
        JMP     @rolled
@axe:
; axe: volatile 1..12
        LDA     #12
        JSR     rng_d
@rolled:
        STA     cnt0            ; preserve damage while checking terrain
        LDA     loc
        BNE     @damage_ready
        LDA     px
        STA     tgtx
        LDA     py
        STA     tgty
        JSR     tileat
        LDA     tgttile
        CMP     #T_HILLS
        BNE     @damage_ready
        CLC
        LDA     cnt0
        ADC     #2
        STA     cnt0
        LDA     attack_flags
        ORA     #ATTACK_HIGH
        STA     attack_flags
@damage_ready:
        LDA     cnt0
        STA     tmp0            ; final damage
; subtract from monster hp
        LDX     monidx
        SEC
        LDA     mon_hp,X
        SBC     tmp0
        STA     mon_hp,X
; build "You hit the Orc for N!"
        JSR     mb_reset
        LDA     #<c_youhit
        LDY     #>c_youhit
        JSR     mb_str
        LDX     monidx
        LDA     mon_type,X
        JSR     mb_mname
        LDA     #<c_for
        LDY     #>c_for
        JSR     mb_str
        LDA     tmp0
        JSR     mb_num
        LDA     #<c_bang
        LDY     #>c_bang
        JSR     mb_str
        JSR     mb_flush
        LDA     attack_flags
        AND     #ATTACK_CRITICAL
        BEQ     @normal_hit
        JSR     sfx_critical
        PRINTMSG_MSG m_critical
        JMP     @terrain_feedback
@normal_hit:
        JSR     sfx_hit
@terrain_feedback:
        LDA     attack_flags
        AND     #ATTACK_HIGH
        BEQ     @deadcheck
        PRINTMSG_MSG m_high_ground
@deadcheck:
; dead?  (hp <= 0 i.e. hp was <= damage -> result negative/zero)
        LDX     monidx
        LDA     mon_hp,X
        BEQ     @dead
        BMI     @dead
        RTS
@dead:
        JMP     monster_dies

;----------------------------------------------------------------
; monster_dies - award XP/gold, drop the slot, maybe win, maybe
; level up.  Slot in monidx.
;----------------------------------------------------------------
monster_dies:
        LDX     monidx
        LDA     mon_type,X
        STA     tmp1            ; type
; XP
        TAY
        LDA     mtype_xp,Y
        CLC
        ADC     pxp
        STA     pxp
        LDA     pxp+1
        ADC     #0
        STA     pxp+1
; gold drop 0..mtype_gold
        LDY     tmp1
        LDA     mtype_gold,Y
        BEQ     @nogold
        JSR     rng_mod         ; 0..gold-1
        CLC
        ADC     pgold
        STA     pgold
        LDA     pgold+1
        ADC     #0
        STA     pgold+1
@nogold:
; message "The Orc dies!"
        JSR     mb_reset
        LDA     #<c_the
        LDY     #>c_the
        JSR     mb_str
        LDA     tmp1
        JSR     mb_mname
        LDA     #<c_dies
        LDY     #>c_dies
        JSR     mb_str
        JSR     mb_flush
        JSR     sfx_killed
; remove monster
        LDX     monidx
        JSR     mon_kill
; quest miniboss?
        LDA     tmp1
        CMP     #M_WARDEN
        BNE     @boss
        JSR     award_wyrm_key
        JMP     @lvl
; final boss?
@boss:
        LDA     tmp1
        CMP     #M_BOSS
        BNE     @lvl
        LDA     #BREATH_NONE
        STA     boss_breath_dir
        LDA     #QUEST_DRAGON_DEAD
        STA     queststate
        PRINTMSG_MSG m_dragon_fallen
@lvl:
        JSR     check_levelup
        RTS

;----------------------------------------------------------------
; check_levelup - if xp >= level*20, level up (raise maxhp, heal).
;----------------------------------------------------------------
check_levelup:
; threshold = plevel * 20  (8-bit is enough up to lvl 12)
        LDA     plevel
        STA     tmp0
        LDA     #0
        STA     tmp1
; tmp = level*20 = level*16 + level*4
        LDA     plevel
        ASL     A
        ASL     A               ; *4
        STA     tmp2
        LDA     plevel
        ASL     A
        ASL     A
        ASL     A
        ASL     A               ; *16
        CLC
        ADC     tmp2
        STA     tmp2            ; level*20 (low); ignore >255 (caps fine)
; compare xp (16-bit) >= tmp2
        LDA     pxp+1
        BNE     @levelup        ; xp >= 256 definitely past early thresholds
        LDA     pxp
        CMP     tmp2
        BCC     @done
@levelup:
        INC     plevel
; maxhp += 8, full heal
        LDA     pmaxhp
        CLC
        ADC     #8
        STA     pmaxhp
        STA     phealth
        LDA     #0
        STA     poison_turns
        JSR     sfx_levelup
        JSR     mb_reset
        LDA     #<c_levelup
        LDY     #>c_levelup
        JSR     mb_str
        LDA     plevel
        JSR     mb_num
        LDA     #<c_bang
        LDY     #>c_bang
        JSR     mb_str
        JSR     mb_flush
@done:
        RTS

;----------------------------------------------------------------
; monster_attacks_player - monster in slot monidx strikes.
;   Damage = 1..mtype_atk, reduced by armor.  Apply to phealth.
;----------------------------------------------------------------
monster_attacks_player:
        LDX     monidx
        LDA     mon_type,X
        STA     tmp1            ; type
        TAY
        LDA     mtype_atk,Y
        JSR     rng_d           ; 1..atk
        STA     tmp0            ; raw damage
; armor reduces by armor index (0..3)
        LDX     parmor
        LDA     arm_def,X
        STA     tmp2
        SEC
        LDA     tmp0
        SBC     tmp2
        BPL     @ok
        LDA     #1              ; minimum 1 damage
@ok:
        BNE     @guard
        LDA     #1
@guard:
        LDX     guard_active
        BEQ     @apply
        SEC
        SBC     #2
        BPL     :+
        LDA     #1
:
        BNE     @apply
        LDA     #1
@apply:
        STA     tmp0
; subtract from phealth (floor at 0)
        SEC
        LDA     phealth
        SBC     tmp0
        BCS     @sethp
        LDA     #0
@sethp:
        STA     phealth
; message "Orc hits you for N!"
        JSR     mb_reset
        LDA     #<c_the
        LDY     #>c_the
        JSR     mb_str
        LDA     tmp1
        JSR     mb_mname
        LDA     #<c_hitsyou
        LDY     #>c_hitsyou
        JSR     mb_str
        LDA     tmp0
        JSR     mb_num
        LDA     #<c_bang
        LDY     #>c_bang
        JSR     mb_str
        JSR     mb_flush
; snake bites may inflict four turns of poison
        LDA     tmp1
        CMP     #M_SNAKE
        BNE     @normal_sfx
        LDX     parmor
        CPX     #1
        BNE     @normal_venom
; leather armor reduces the poison chance from one-in-three to one-in-six
        LDA     #6
        BNE     @roll_venom
@normal_venom:
        LDA     #3
@roll_venom:
        JSR     rng_d
        CMP     #1
        BNE     @normal_sfx
        LDA     poison_turns
        BNE     @refresh_poison
        LDA     #$84            ; high bit skips the immediate status tick
        STA     poison_turns
        JMP     @poisoned
@refresh_poison:
        LDA     #4              ; refreshed poison still ticks this turn
        STA     poison_turns
@poisoned:
        JSR     sfx_poison
        PRINTMSG_MSG m_poisoned
        RTS
@normal_sfx:
        JSR     sfx_hurt
        RTS

;----------------------------------------------------------------
; thief_steals_player - steal up to eight gold, then mark this
; thief as fleeing. If the player is broke, attack normally.
;----------------------------------------------------------------
thief_steals_player:
        LDA     pgold
        ORA     pgold+1
        BNE     @hasgold
        JMP     monster_attacks_player
@hasgold:
        LDA     #8
        JSR     rng_d
        STA     tmp0
        LDA     pgold+1
        BNE     @subtract
        LDA     pgold
        CMP     tmp0
        BCS     @subtract
        STA     tmp0
@subtract:
        SEC
        LDA     pgold
        SBC     tmp0
        STA     pgold
        LDA     pgold+1
        SBC     #0
        STA     pgold+1
        LDX     monidx
        LDA     #1
        STA     mon_state,X
        JSR     mb_reset
        LDA     #<c_thiefsteals
        LDY     #>c_thiefsteals
        JSR     mb_str
        LDA     tmp0
        JSR     mb_num
        LDA     #<c_goldflees
        LDY     #>c_goldflees
        JSR     mb_str
        JSR     mb_flush
        JSR     sfx_blocked
        RTS

;----------------------------------------------------------------
; process_status - apply player status effects after a consumed turn.
;----------------------------------------------------------------
process_status:
        LDA     poison_turns
        BEQ     @done
        BMI     @fresh
        DEC     poison_turns
        LDA     phealth
        BEQ     @done
        DEC     phealth
        JSR     sfx_poison
        PRINTMSG_MSG m_poison_tick
        RTS
@fresh:
        AND     #$7F
        STA     poison_turns
@done:
        RTS

;----------------------------------------------------------------
; dragon_fire_player - apply the dragon's telegraphed breath attack.
; Fire ignores armor but is avoidable by leaving the warned lane.
;----------------------------------------------------------------
dragon_fire_player:
        LDA     #7
        JSR     rng_d           ; 1..7
        CLC
        ADC     #5              ; 6..12 damage
        STA     tmp0
        SEC
        LDA     phealth
        SBC     tmp0
        BCS     @sethp
        LDA     #0
@sethp:
        STA     phealth
        JSR     mb_reset
        LDA     #<c_dragonfire
        LDY     #>c_dragonfire
        JSR     mb_str
        LDA     tmp0
        JSR     mb_num
        LDA     #<c_bang
        LDY     #>c_bang
        JSR     mb_str
        JSR     mb_flush
        JSR     sfx_breath
        RTS

;----------------------------------------------------------------
; Armor defense table
;----------------------------------------------------------------
arm_def:
        .BYTE   0               ; 0 clothes
        .BYTE   1               ; 1 leather
        .BYTE   2               ; 2 chain
        .BYTE   4               ; 3 plate

;----------------------------------------------------------------
; Monster name table (for messages), indexed by type id
;----------------------------------------------------------------
mon_names:
        .WORD   mn_none, mn_orc, mn_snake, mn_skel, mn_thief, mn_troll, mn_boss, mn_warden
mn_none:
        .BYTE   "thing",0
mn_orc:
        .BYTE   "Orc",0
mn_snake:
        .BYTE   "Snake",0
mn_skel:
        .BYTE   "Skeleton",0
mn_thief:
        .BYTE   "Thief",0
mn_troll:
        .BYTE   "Troll",0
mn_boss:
        .BYTE   "Dragon",0
mn_warden:
        .BYTE   "Wyrm Warden",0

;----------------------------------------------------------------
; Combat / event message fragments
;----------------------------------------------------------------
c_youhit:
        .BYTE   "You hit the ",0
c_for:
        .BYTE   " for ",0
c_bang:
        .BYTE   "!",0
c_the:
        .BYTE   "The ",0
c_dies:
        .BYTE   " dies!",0
c_hitsyou:
        .BYTE   " hits you for ",0
c_dragonfire:
        .BYTE   "Dragon fire burns you for ",0
c_thiefsteals:
        .BYTE   "The Thief steals ",0
c_goldflees:
        .BYTE   " gold and flees!",0
c_levelup:
        .BYTE   "Welcome to level ",0

;----------------------------------------------------------------
; Map / event messages (shared with player.asm)
;----------------------------------------------------------------
m_blocked:
        .BYTE   "Blocked!",0
m_treasure:
        .BYTE   "You found gold in a chest!",0
m_eastmere:
        .BYTE   "Eastmere. Its outfitter offers the realm's best equipment.",0
m_valehaven:
        .BYTE   "Valehaven. Its market offers cheap healing and provisions.",0
m_dungeon:
        .BYTE   "Ancient halls descend through black water toward the dragon.",0
m_dragon_wakes:
        .BYTE   "A furious roar rolls from the southern chamber.",0
m_dungeon_empty:
        .BYTE   "The flooded halls are silent. Their ancient master is dead.",0
m_world:
        .BYTE   "You return to the land of Wyrmhold.",0
m_dragon_fallen:
        .BYTE   "The dragon falls! Return to King Aldren at Wyrmhold Castle.",0
m_breath_warn:
        .BYTE   "The Dragon draws breath! Leave the glowing fire lane!",0
m_breath_miss:
        .BYTE   "Dragon fire tears through the chamber, but you stand clear.",0
m_poisoned:
        .BYTE   "The Snake's venom courses through your veins!",0
m_poison_tick:
        .BYTE   "Poison burns for 1 damage.",0
m_marsh_poison:
        .BYTE   "Marsh venom seeps through the mire!",0
m_skeleton_wakes:
        .BYTE   "The Skeleton wakes and leaves its guard post!",0
m_troll_regens:
        .BYTE   "The Troll's wounds begin to knit.",0
m_guard:
        .BYTE   "You brace for the enemy's next attack.",0
m_critical:
        .BYTE   "Critical strike!",0
m_high_ground:
        .BYTE   "High ground adds 2 damage.",0
m_discover_cache:
        .BYTE   "Hidden cache! You recover 40 gold and 75 provisions.",0
m_discover_cairn:
        .BYTE   "The hilltop cairn blesses you with 5 maximum health.",0
m_discover_waystone:
        .BYTE   "Waystone: castle north; dragon cave northwest; shrine south.",0
m_shortcut_cross:
        .BYTE   "You force the reed ford, losing health and provisions.",0
m_shortcut_weak:
        .BYTE   "The reed ford is too dangerous while so wounded.",0
m_shortcut_blocked:
        .BYTE   "A monster blocks the far side of the reed ford.",0
