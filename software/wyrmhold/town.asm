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
; Shop box geometry - a clean panel drawn inside the viewport,
; well clear of the bottom-right auto-scroll cell.
;----------------------------------------------------------------
SHOP_X0         = 3             ; left column of box
SHOP_X1         = 42            ; right column of box
SHOP_Y0         = 4             ; top row of box
SHOP_Y1         = 15            ; bottom row of box
SHOP_W          = SHOP_X1-SHOP_X0+1

;----------------------------------------------------------------
; shop_draw - render the shop as a self-contained bordered panel.
; The whole box is repainted each call (background + border via
; direct VRAM, then text via firmware) so it is always clean.
;----------------------------------------------------------------
shop_draw:
; --- paint box background + border directly to VRAM ---
        JSR     vid_enter
        LDA     #SHOP_Y0
        STA     rowidx
@bgrow:
        LDA     rowidx
        JSR     rowbase
        LDY     #SHOP_X0
@bgcol:
; border on edges, blank interior
        CPY     #SHOP_X0
        BEQ     @edge
        CPY     #SHOP_X1
        BEQ     @edge
        LDA     rowidx
        CMP     #SHOP_Y0
        BEQ     @edge
        CMP     #SHOP_Y1
        BEQ     @edge
; interior cell
        LDA     #space
        STA     (vptr),Y
        LDA     #C_SHOPBG
        STA     (cptr),Y
        JMP     @bgnext
@edge:
        LDA     #' '
        STA     (vptr),Y
        LDA     #C_SHOPBRD
        STA     (cptr),Y
@bgnext:
        INY
        CPY     #SHOP_X1+1
        BNE     @bgcol
        INC     rowidx
        LDA     rowidx
        CMP     #SHOP_Y1+1
        BNE     @bgrow
        JSR     vid_exit

; --- text lines via firmware (interior color C_SHOPBG) ---
        LDA     #C_SHOPTTL
        STA     CURCOLOR
        LDX     #SHOP_X0+8
        LDY     #SHOP_Y0
        JSR     locate
        PRINTMSG t_header

        LDA     #C_SHOPTXT
        STA     CURCOLOR
        LDX     #SHOP_X0+3
        LDY     #SHOP_Y0+2
        JSR     locate
        PRINTMSG t_opt1
        LDX     #SHOP_X0+3
        LDY     #SHOP_Y0+3
        JSR     locate
        PRINTMSG t_opt2
        LDX     #SHOP_X0+3
        LDY     #SHOP_Y0+4
        JSR     locate
        PRINTMSG t_opt3
        LDX     #SHOP_X0+3
        LDY     #SHOP_Y0+5
        JSR     locate
        PRINTMSG t_opt4
        LDX     #SHOP_X0+3
        LDY     #SHOP_Y0+6
        JSR     locate
        PRINTMSG t_optx

; gold line
        LDX     #SHOP_X0+3
        LDY     #SHOP_Y0+8
        JSR     locate
        LDA     #C_SHOPTXT
        STA     CURCOLOR
        PRINTMSG t_gold
        COPY16  pgold, numarg
        JSR     displaynum

; status line (last action / greeting)
        LDX     #SHOP_X0+3
        LDY     #SHOP_Y1-1
        JSR     locate
        LDA     #C_SHOPST
        STA     CURCOLOR
        LDA     shopstat
        LDY     shopstat+1
        JSR     prmsg
        RTS

;----------------------------------------------------------------
; Shop strings
;----------------------------------------------------------------
t_header:
        .BYTE   "- SHOP -",0
t_opt1:
        .BYTE   "1) Heal to full     10 gold",0
t_opt2:
        .BYTE   "2) Provisions +100  20 gold",0
t_opt3:
        .BYTE   "3) Better weapon    60 gold",0
t_opt4:
        .BYTE   "4) Better armor     50 gold",0
t_optx:
        .BYTE   "X) Leave the shop",0
t_gold:
        .BYTE   "Thy gold: ",0

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
