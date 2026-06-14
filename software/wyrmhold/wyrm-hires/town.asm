;______________________________________________________________________________
;  town.asm - shop interaction inside towns
;
;  When the player presses USE ('T') in a town while adjacent to a
;  shop counter ('S' tile), the shop menu opens.  The menu is drawn
;  over the message rows; the player picks an option, gold is spent,
;  and the menu returns to the map.
;______________________________________________________________________________

PRICE_HEAL      = 10            ; gold per visit (full heal)
PRICE_FOOD      = 20            ; gold for +100 food
PRICE_WEAPON    = 60            ; gold to upgrade weapon one tier
PRICE_ARMOR     = 50            ; gold to upgrade armor one tier

;----------------------------------------------------------------
; use_action - bound to the USE key ('T').  Only meaningful in a
; town next to a shop counter.
;----------------------------------------------------------------
use_action:
        LDA     loc
        CMP     #LOC_TOWN
        BEQ     @intown
        PRINTMSG_MSG t_nothing
        RTS
@intown:
; is a shop tile adjacent (4-neighbour) or under us?
        JSR     shop_adjacent
        BCS     @open
        PRINTMSG_MSG t_noshop
        RTS
@open:
        JMP     shop_menu

;----------------------------------------------------------------
; shop_adjacent - C=1 if a T_SHOP tile is at or next to the player.
;----------------------------------------------------------------
shop_adjacent:
; center
        LDA     px
        STA     tgtx
        LDA     py
        STA     tgty
        JSR     tileat
        LDA     tgttile
        CMP     #T_SHOP
        BEQ     @yes
; up
        LDA     px
        STA     tgtx
        LDA     py
        SEC
        SBC     #1
        STA     tgty
        JSR     tileat
        LDA     tgttile
        CMP     #T_SHOP
        BEQ     @yes
; down
        LDA     py
        CLC
        ADC     #1
        STA     tgty
        LDA     px
        STA     tgtx
        JSR     tileat
        LDA     tgttile
        CMP     #T_SHOP
        BEQ     @yes
; left
        LDA     py
        STA     tgty
        LDA     px
        SEC
        SBC     #1
        STA     tgtx
        JSR     tileat
        LDA     tgttile
        CMP     #T_SHOP
        BEQ     @yes
; right
        LDA     px
        CLC
        ADC     #1
        STA     tgtx
        JSR     tileat
        LDA     tgttile
        CMP     #T_SHOP
        BEQ     @yes
        CLC
        RTS
@yes:
        SEC
        RTS

;----------------------------------------------------------------
; shop_menu - draw the shop menu over the message area and loop
; processing purchases until the player presses X.
;----------------------------------------------------------------
shop_menu:
        JSR     sfx_door
; start with the greeting as the status line
        LDA     #<t_greet
        STA     shopstat
        LDA     #>t_greet
        STA     shopstat+1
@redraw:
        JSR     shop_draw
@key:
        JSR     getkey_block
        CMP     #'1'
        BEQ     @heal
        CMP     #'2'
        BEQ     @food
        CMP     #'3'
        BEQ     @weapon
        CMP     #'4'
        BEQ     @armor
        CMP     #'X'
        BEQ     @goleave
        CMP     #'x'
        BEQ     @goleave
        JMP     @key
@goleave:
        JMP     @leave
@heal:
        LDA     #PRICE_HEAL
        JSR     spend_gold
        BCS     :+
        JMP     @poor
:
        LDA     pmaxhp
        STA     phealth
        SETSTAT t_healed
        JMP     @redraw
@food:
        LDA     #PRICE_FOOD
        JSR     spend_gold
        BCC     @poor
        CLC
        LDA     pfood
        ADC     #100
        STA     pfood
        LDA     pfood+1
        ADC     #0
        STA     pfood+1
        SETSTAT t_fed
        JMP     @redraw
@weapon:
        LDA     pweapon
        CMP     #3
        BCS     @maxwpn
        LDA     #PRICE_WEAPON
        JSR     spend_gold
        BCC     @poor
        INC     pweapon
        SETSTAT t_boughtw
        JMP     @redraw
@maxwpn:
        SETSTAT t_maxed
        JMP     @redraw
@armor:
        LDA     parmor
        CMP     #3
        BCS     @maxarm
        LDA     #PRICE_ARMOR
        JSR     spend_gold
        BCC     @poor
        INC     parmor
        SETSTAT t_boughta
        JMP     @redraw
@maxarm:
        SETSTAT t_maxed
        JMP     @redraw
@poor:
        SETSTAT t_poor
        JMP     @redraw
@leave:
; fully restore the game screen, then report leaving
        JSR     full_redraw
        JSR     msg_redraw
        PRINTMSG_MSG t_bye
        RTS

;----------------------------------------------------------------
; spend_gold - try to spend A gold.  C=1 success (gold deducted),
;              C=0 if not enough.
;----------------------------------------------------------------
spend_gold:
        STA     tmp0
; gold >= tmp0 ?  (16-bit gold, 8-bit cost)
        LDA     pgold+1
        BNE     @ok             ; high byte nonzero -> plenty
        LDA     pgold
        CMP     tmp0
        BCC     @no
@ok:
        SEC
        LDA     pgold
        SBC     tmp0
        STA     pgold
        LDA     pgold+1
        SBC     #0
        STA     pgold+1
        SEC
        RTS
@no:
        CLC
        RTS

;----------------------------------------------------------------
; shop_draw - render the shop menu in the 4 bottom mixed-mode text
; rows (20..23).  The HIRES map stays visible above.
;   row 20: "- SHOP -   Gold nnnnn"
;   row 21: "1)Heal 10  2)Food 20"
;   row 22: "3)Weapon 60  4)Armor 50  X)Leave"
;   row 23: status line (greeting / last action)
; pad_eol pads each line with spaces (stopping before the last col).
;----------------------------------------------------------------
shop_draw:
        LDX     #0
        LDY     #STATY0
        JSR     locate
        LDA     #C_SHOPTTL
        STA     CURCOLOR
        PRINTMSG t_header
        PRINTMSG t_gold
        COPY16  pgold, numarg
        JSR     displaynum
        JSR     shop_padeol

        LDX     #0
        LDY     #STATY1
        JSR     locate
        LDA     #C_SHOPTXT
        STA     CURCOLOR
        PRINTMSG t_opt1
        PRINTMSG t_opt2
        JSR     shop_padeol

        LDX     #0
        LDY     #MSGY0
        JSR     locate
        LDA     #C_SHOPTXT
        STA     CURCOLOR
        PRINTMSG t_opt3
        PRINTMSG t_opt4
        PRINTMSG t_optx
        JSR     shop_padeol

        LDX     #0
        LDY     #MSGY0+1
        JSR     locate
        LDA     #C_SHOPST
        STA     CURCOLOR
        LDA     shopstat
        LDY     shopstat+1
        JSR     prmsg
        JSR     shop_padeol
        RTS

; pad spaces to end of the current line, stopping at the last column
; so the firmware never advances past the bottom-right cell.
shop_padeol:
@p:
        LDA     CURX
        CMP     #SCRW-1
        BCS     @d
        LDA     #space
        JSR     putc
        JMP     @p
@d:
        RTS

;----------------------------------------------------------------
; Shop strings
;----------------------------------------------------------------
t_header:
        .BYTE   "- SHOP -    ",0
t_opt1:
        .BYTE   "1)Heal 10   ",0
t_opt2:
        .BYTE   "2)Food+100 20",0
t_opt3:
        .BYTE   "3)Weapon 60   ",0
t_opt4:
        .BYTE   "4)Armor 50   ",0
t_optx:
        .BYTE   "X)Leave",0
t_gold:
        .BYTE   "  Gold ",0

t_greet:
        .BYTE   "Welcome! What dost thou need?",0
t_nothing:
        .BYTE   "Nothing happens.",0
t_noshop:
        .BYTE   "There is no shop here. Find the 'S' counter.",0
t_healed:
        .BYTE   "Thou art fully healed.        ",0
t_fed:
        .BYTE   "Provisions purchased.         ",0
t_boughtw:
        .BYTE   "A finer weapon is thine!      ",0
t_boughta:
        .BYTE   "Sturdier armor is thine!      ",0
t_maxed:
        .BYTE   "Thou hast the finest already. ",0
t_poor:
        .BYTE   "Thou canst not afford that.   ",0
t_bye:
        .BYTE   "Come again, adventurer.",0
