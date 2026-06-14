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
;   Damage = 1..(weapon_power) + small bonus.  Apply, possibly kill.
;----------------------------------------------------------------
player_attacks_monster:
        STX     monidx
; damage roll: base from weapon
        LDX     pweapon
        LDA     wpn_power,X
        JSR     rng_d           ; 1..power
        STA     tmp0            ; damage
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
        JSR     sfx_hit
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
; boss?
        LDA     tmp1
        CMP     #M_BOSS
        BNE     @lvl
        LDA     #1
        STA     bosskilled
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
        JSR     sfx_hit
        RTS

;----------------------------------------------------------------
; Weapon power (damage sides) and armor defense tables
;----------------------------------------------------------------
wpn_power:
        .BYTE   2               ; 0 fists
        .BYTE   4               ; 1 dagger
        .BYTE   7               ; 2 sword
        .BYTE   10              ; 3 axe
arm_def:
        .BYTE   0               ; 0 clothes
        .BYTE   1               ; 1 leather
        .BYTE   2               ; 2 chain
        .BYTE   4               ; 3 plate

;----------------------------------------------------------------
; Monster name table (for messages), indexed by type id
;----------------------------------------------------------------
mon_names:
        .WORD   mn_none, mn_orc, mn_snake, mn_skel, mn_thief, mn_troll, mn_boss
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
c_levelup:
        .BYTE   "Welcome to level ",0

;----------------------------------------------------------------
; Map / event messages (shared with player.asm)
;----------------------------------------------------------------
m_blocked:
        .BYTE   "Blocked!",0
m_treasure:
        .BYTE   "You found gold in a chest!",0
m_town:
        .BYTE   "Town. Step on the door '+' (south) to leave. T at 'S' to shop.",0
m_dungeon:
        .BYTE   "Dungeon! Step on the stairs '<' to climb back out.",0
m_world:
        .BYTE   "You return to the land of Wyrmhold.",0
