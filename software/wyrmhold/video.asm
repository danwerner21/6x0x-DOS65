;______________________________________________________________________________
;  video.asm - memory-mapped video: paging, framing, viewport, UI text
;
;  Direct VRAM access (the fast path used by SpeedScript): map video
;  sub-page $F9 into CPU bank $A.  Then:
;     text  char at  $A000 + row*80 + col
;     color cell at  $A800 + row*80 + col
;  Color byte = (bg<<4)|fg.
;
;  All direct-write routines must be bracketed by vid_enter / vid_exit.
;  UI text via the firmware (prmsg / putxy) does NOT page; it uses
;  FARCALL chrout/locate and must be used while in task 0.
;______________________________________________________________________________

;----------------------------------------------------------------
; vid_enter - map video text/color page into $A000-$AFFF (task 1)
;             trashes A,X ; preserves Y
;----------------------------------------------------------------
vid_enter:
        LDA     #$01            ; configure task 01
        LDX     #$0A            ; CPU bank $A ($A000-$AFFF)
        LDY     #VIDTEXT_PAGE   ; physical video page $F9
        JSR     SETPAGE
        LDA     #$01
        STA     PC6502_ACT_TASK ; activate task 01
        RTS

;----------------------------------------------------------------
; vid_exit - restore normal memory at $A000 (task 0)
;----------------------------------------------------------------
vid_exit:
        LDA     #$00
        STA     PC6502_ACT_TASK
        RTS

;----------------------------------------------------------------
; rowbase - compute VRAM pointers for the start of a screen row.
;   IN : A = row (0..23)
;   OUT: vptr -> $A000 + row*80   (text)
;        cptr -> $A800 + row*80   (color)
;   row*80 = row*64 + row*16 = (row<<6)+(row<<4)
;----------------------------------------------------------------
rowbase:
        STA     tmp0            ; row
; row*16 -> ptr2
        LDA     #0
        STA     ptr2+1
        LDA     tmp0
        ASL     A               ; *2
        ASL     A               ; *4
        ASL     A               ; *8
        ASL     A               ; *16 (row<=23 so <=368, may set carry)
        STA     ptr2
        LDA     #0
        ROL     A
        STA     ptr2+1          ; high bits of row*16
; row*64 -> ptr
        LDA     tmp0
        STA     ptr
        LDA     #0
        STA     ptr+1
        ASL     ptr
        ROL     ptr+1           ; *2
        ASL     ptr
        ROL     ptr+1           ; *4
        ASL     ptr
        ROL     ptr+1           ; *8
        ASL     ptr
        ROL     ptr+1           ; *16
        ASL     ptr
        ROL     ptr+1           ; *32
        ASL     ptr
        ROL     ptr+1           ; *64
; ptr = ptr + ptr2 = row*80
        CLC
        LDA     ptr
        ADC     ptr2
        STA     ptr
        LDA     ptr+1
        ADC     ptr2+1
        STA     ptr+1
; vptr = $A000 + ptr
        CLC
        LDA     ptr
        STA     vptr
        LDA     ptr+1
        ADC     #>VRAM_TEXT
        STA     vptr+1
; cptr = $A800 + ptr
        CLC
        LDA     ptr
        STA     cptr
        LDA     ptr+1
        ADC     #>VRAM_COLOR
        STA     cptr+1
        RTS

;----------------------------------------------------------------
; putcell - write one char+color cell directly to VRAM.
;   MUST be called between vid_enter / vid_exit.
;   IN : tmp1 = row, tmp2 = col, tmp3 = char, colidx = color
;   trashes A,Y
;----------------------------------------------------------------
putcell:
        LDA     tmp1
        JSR     rowbase
        LDY     tmp2
        LDA     tmp3
        STA     (vptr),Y
        LDA     colidx
        STA     (cptr),Y
        RTS

;----------------------------------------------------------------
; cls_vram - fill the whole screen with spaces in C_BLANK.
;----------------------------------------------------------------
cls_vram:
        JSR     vid_enter
        LDA     #0
        STA     rowidx
@row:
        LDA     rowidx
        JSR     rowbase
        LDY     #0
@col:
        LDA     #space
        STA     (vptr),Y
        LDA     #C_BLANK
        STA     (cptr),Y
        INY
        CPY     #SCRW
        BNE     @col
        INC     rowidx
        LDA     rowidx
        CMP     #SCRH
        BNE     @row
        JMP     vid_exit

;----------------------------------------------------------------
; fillrow_vram - fill one row [colA..colB) with char/color.
;   IN: tmp1=row, tmp2=start col, tmp3=char, colidx=color, cnt0=count
;   MUST be inside vid_enter/vid_exit.
;----------------------------------------------------------------
fillrow_vram:
        LDA     tmp1
        JSR     rowbase
        LDY     tmp2
        LDX     cnt0
@fl:
        LDA     tmp3
        STA     (vptr),Y
        LDA     colidx
        STA     (cptr),Y
        INY
        DEX
        BNE     @fl
        RTS

;----------------------------------------------------------------
; draw_frame - draw the static game UI: title bar, viewport frame,
;              stat panel frame, and message area divider.
;              Done once at game start (and after returning from
;              full-screen menus).
;----------------------------------------------------------------
draw_frame:
        JSR     cls_vram
        JSR     vid_enter

; --- top title bar (row 0) ---
        LDA     #0
        STA     tmp1
        LDA     #0
        STA     tmp2
        LDA     #space
        STA     tmp3
        LDA     #C_TITLE
        STA     colidx
        LDA     #SCRW
        STA     cnt0
        JSR     fillrow_vram
; centered title text written below via vid_exit path

; --- viewport top/bottom horizontal borders ---
; top border at row 0 already part of title; we frame the
; viewport with vertical bars and a bottom rule at VPY0+VPH.
; vertical left/right bars for viewport rows
        LDX     #VPY0
@vbar:
        TXA
        STA     tmp1
        JSR     rowbase
        LDY     #0              ; left edge col 0
        LDA     #'|'
        STA     (vptr),Y
        LDA     #C_BORDER
        STA     (cptr),Y
        LDY     #(VPW+1)        ; right edge of viewport
        LDA     #'|'
        STA     (vptr),Y
        LDA     #C_BORDER
        STA     (cptr),Y
        INX
        CPX     #(VPY0+VPH)
        BNE     @vbar

; viewport bottom rule + message divider share row VPY0+VPH
; (= MSGY0-1).  Draw it full-width.
        LDA     #(VPY0+VPH)
        STA     tmp1
        LDA     #0
        STA     tmp2
        LDA     #'='
        STA     tmp3
        LDA     #C_BORDER
        STA     colidx
        LDA     #SCRW
        STA     cnt0
        JSR     fillrow_vram

; panel header row (row 1) background highlight
        LDA     #1
        STA     tmp1
        LDA     #(VPW+2)
        STA     tmp2
        LDA     #space
        STA     tmp3
        LDA     #C_PANELHDR
        STA     colidx
        LDA     #(SCRW-(VPW+2))
        STA     cnt0
        JSR     fillrow_vram

        JSR     vid_exit

; write the title bar + panel header text via firmware
        LDX     #30
        LDY     #0
        JSR     locate
        LDA     #C_TITLE
        STA     CURCOLOR
        PRINTMSG titlebar

        LDX     #(VPW+4)
        LDY     #1
        JSR     locate
        LDA     #C_PANELHDR
        STA     CURCOLOR
        PRINTMSG panelhdr
        RTS

;----------------------------------------------------------------
; locate - position the firmware text cursor.  X=col, Y=row.
;----------------------------------------------------------------
locate:
        FARCALL FC_LOCATE
        RTS

;----------------------------------------------------------------
; putc - output one char via firmware (A=char), honoring CURCOLOR.
;        (Cannot use the FARCALL macro - it would clobber the char
;         in A.  Set farfunct, then restore A before the call.)
;----------------------------------------------------------------
putc:
        PHA
        LDA     #FC_CHROUT
        STA     farfunct
        PLA
        JSR     DO_FARCALL
        RTS

;----------------------------------------------------------------
; prmsg - print a 0-terminated string via firmware chrout.
;   IN: A=lo, Y=hi of string address.  Uses CURCOLOR for color.
;----------------------------------------------------------------
prmsg:
        STA     strp
        STY     strp+1
        LDY     #0
@pl:
        LDA     (strp),Y
        BEQ     @done
        PHY
        JSR     putc
        PLY
        INY
        BNE     @pl
@done:
        RTS

;----------------------------------------------------------------
; putxy - position cursor (X=col,Y=row), set color (A=color),
;         then print string at strp.  Helper for panel fields.
;   IN: tmp2=col, tmp1=row, colidx=color, strp=string ptr
;----------------------------------------------------------------
putxy:
        LDX     tmp2
        LDY     tmp1
        JSR     locate
        LDA     colidx
        STA     CURCOLOR
        LDA     strp
        LDY     strp+1
        JSR     prmsg
        RTS

;----------------------------------------------------------------
; clear_panel_value - blank a panel field area before redraw.
;   IN: X=col, Y=row, A=width
;----------------------------------------------------------------
clear_panel_value:
        STA     cnt1            ; width
        JSR     locate
        LDA     #C_PANEL
        STA     CURCOLOR
@cpv:
        LDA     #space
        JSR     putc
        DEC     cnt1
        BNE     @cpv
        RTS

;----------------------------------------------------------------
; render_view - draw the scrolling map viewport centered on the
; player. Each world tile is a 2x2 character-cell metatile.
;
;   uses: px,py (player world coords), loc (active map)
;   The top-left WORLD cell shown is (px-VPCX, py-VPCY).
;----------------------------------------------------------------
render_view:
        JSR     vid_enter
; world-tile row = 0..VPTH-1
        LDA     #0
        STA     rowidx
@vrow:
; world y for this row = py - VPCY + rowidx
        SEC
        LDA     py
        SBC     #VPCY
        CLC
        ADC     rowidx
        STA     tgty
; world-tile column loop
        LDA     #0
        STA     colidx
@vcol:
; world x = px - VPCX + colidx
        SEC
        LDA     px
        SBC     #VPCX
        CLC
        ADC     colidx
        STA     tgtx
; fetch tile (returns tgttile)
        JSR     tileat
; metatile base glyph = MG_TERRAIN_BASE + tile_code*4
        LDA     tgttile
        ASL     A
        ASL     A
        CLC
        ADC     #MG_TERRAIN_BASE
        STA     cnt0
        LDX     tgttile
        LDA     tile_color,X
        STA     cnt1
; pick an alternate art variant for some cells so large regions of
; one terrain don't show an obvious repeating grid. A terrain has a
; variant iff tile_variant[code] != 0; we use it when the cheap
; position hash (tgtx ^ tgty) bit0 is set.
        LDA     tile_variant,X
        BEQ     @novar          ; this terrain has no variant
        LDA     tgtx
        EOR     tgty
        AND     #$01
        BEQ     @novar          ; hash says keep the base art
        LDA     tile_variant,X  ; swap in the variant metatile base
        STA     cnt0
@novar:
; screen column = VPX0 + colidx*2
        LDA     colidx
        ASL     A
        CLC
        ADC     #VPX0
        STA     tmp2
; screen row = VPY0 + rowidx*2
        LDA     rowidx
        ASL     A
        CLC
        ADC     #VPY0
        STA     tmp3
        JSR     blit_metatile_vram
; next column
        INC     colidx
        LDA     colidx
        CMP     #VPTW
        BNE     @vcol
; next row
        INC     rowidx
        LDA     rowidx
        CMP     #VPTH
        BNE     @vrow

; --- overlay monsters that fall within the viewport ---
        JSR     draw_monsters_vram

; --- overlay the dragon's warned fire lane ---
        JSR     draw_dragon_telegraph_vram

; --- overlay fixed location characters ---
        JSR     draw_castle_ruler_vram

; --- overlay the player at the fixed center metatile ---
        LDA     #MG_PLAYER
        STA     cnt0
        LDA     #C_PLAYER
        STA     cnt1
        LDA     #(VPX0+(VPCX*2))
        STA     tmp2
        LDA     #(VPY0+(VPCY*2))
        STA     tmp3
        JSR     blit_entity_vram

        JMP     vid_exit

;----------------------------------------------------------------
; draw_castle_ruler_vram - draw the ruler over the audience-room
; throne using the player silhouette in royal colors.
;----------------------------------------------------------------
draw_castle_ruler_vram:
        LDA     loc
        CMP     #LOC_CASTLE
        BNE     @done
        LDA     #CASTLE_RULER_X
        STA     tgtx
        LDA     #CASTLE_RULER_Y
        STA     tgty
        LDA     #MG_PLAYER
        STA     cnt0
        LDA     #COLOR(CO_BRYELLOW,CO_RED)
        STA     cnt1
        JMP     plot_view_cell
@done:
        RTS

;----------------------------------------------------------------
; blit_metatile_vram - draw a 2x2 terrain metatile with one color.
;   IN: cnt0=base glyph, cnt1=color, tmp2=screen col, tmp3=screen row
;   MUST be called inside vid_enter/vid_exit.
;----------------------------------------------------------------
blit_metatile_vram:
        LDA     tmp3
        JSR     rowbase
        LDY     tmp2
        LDA     cnt0
        STA     (vptr),Y
        LDA     cnt1
        STA     (cptr),Y
        INY
        LDA     cnt0
        CLC
        ADC     #1
        STA     (vptr),Y
        LDA     cnt1
        STA     (cptr),Y

        LDA     tmp3
        CLC
        ADC     #1
        JSR     rowbase
        LDY     tmp2
        LDA     cnt0
        CLC
        ADC     #2
        STA     (vptr),Y
        LDA     cnt1
        STA     (cptr),Y
        INY
        LDA     cnt0
        CLC
        ADC     #3
        STA     (vptr),Y
        LDA     cnt1
        STA     (cptr),Y
        RTS

;----------------------------------------------------------------
; blit_entity_vram - overlay a 2x2 entity metatile while preserving
; each terrain cell's background color.
;   IN: cnt0=base glyph, cnt1=entity color, tmp2=screen col,
;       tmp3=screen row
;   MUST be called inside vid_enter/vid_exit.
;----------------------------------------------------------------
blit_entity_vram:
        LDA     tmp3
        JSR     rowbase
        LDY     tmp2
        LDA     cnt0
        STA     (vptr),Y
        JSR     entity_color_at_y
        INY
        LDA     cnt0
        CLC
        ADC     #1
        STA     (vptr),Y
        JSR     entity_color_at_y

        LDA     tmp3
        CLC
        ADC     #1
        JSR     rowbase
        LDY     tmp2
        LDA     cnt0
        CLC
        ADC     #2
        STA     (vptr),Y
        JSR     entity_color_at_y
        INY
        LDA     cnt0
        CLC
        ADC     #3
        STA     (vptr),Y
        JSR     entity_color_at_y
        RTS

; preserve background nibble at the current cptr,Y; apply cnt1 foreground
entity_color_at_y:
        LDA     (cptr),Y
        AND     #$F0
        STA     tmp0
        LDA     cnt1
        AND     #$0F
        ORA     tmp0
        STA     (cptr),Y
        RTS

;----------------------------------------------------------------
; plot_view_cell - overlay an entity metatile at WORLD coords
; (tgtx,tgty) if it is visible.
;   IN: tgtx,tgty=world coords, cnt0=base glyph, cnt1=color
;   MUST be called inside vid_enter/vid_exit.
;----------------------------------------------------------------
plot_view_cell:
; viewport tile column = tgtx - px + VPCX
        SEC
        LDA     tgtx
        SBC     px
        CLC
        ADC     #VPCX
; must be 0..VPTW-1
        BMI     @no             ; (signed underflow check via N flag)
        CMP     #VPTW
        BCS     @no
        ASL     A
        CLC
        ADC     #VPX0
        STA     tmp2            ; screen column
; viewport tile row = tgty - py + VPCY
        SEC
        LDA     tgty
        SBC     py
        CLC
        ADC     #VPCY
        BMI     @no
        CMP     #VPTH
        BCS     @no
        ASL     A
        CLC
        ADC     #VPY0
        STA     tmp3            ; screen row
        JMP     blit_entity_vram
@no:
        RTS

;----------------------------------------------------------------
; Static UI strings
;----------------------------------------------------------------
titlebar:
        .BYTE   "= W Y R M H O L D =",0
panelhdr:
        .BYTE   "ADVENTURER",0
