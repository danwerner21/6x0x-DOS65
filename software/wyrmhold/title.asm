;______________________________________________________________________________
;  title.asm - graphical title screen
;
;  Composes a colorful scene with direct-VRAM fills, the custom terrain
;  tiles, a big block-letter "WYRMHOLD" logo, and a dragon, rather than
;  plain ASCII text.
;______________________________________________________________________________

; big-letter logo geometry
BL_W            = 5             ; cells per letter (incl. 1 col gap handled by caller)
BL_H            = 5             ; rows per letter
BL_ROW          = 3             ; top screen row of the logo
BL_COL          = 8             ; left screen col of the logo

;----------------------------------------------------------------
; draw_title_scene - paint the whole title screen.
;----------------------------------------------------------------
draw_title_scene:
        JSR     cls_vram
        JSR     vid_enter

        ; --- sky band (rows 0..9) : deep blue background ---
        LDA     #0
        STA     rowidx
@sky:
        LDA     rowidx
        STA     tmp1
        LDA     #0
        STA     tmp2
        LDA     #space
        STA     tmp3
        LDA     #COLOR(CO_BRWHITE, CO_BLUE)
        STA     colidx
        LDA     #SCRW
        STA     cnt0
        JSR     fillrow_vram
        INC     rowidx
        LDA     rowidx
        CMP     #10
        BNE     @sky

        ; sprinkle a few "stars" in the sky (asterisks)
        LDA     #'.'
        STA     tmp3
        LDA     #COLOR(CO_BRWHITE, CO_BLUE)
        STA     colidx
        LDX     #0
@stars:
        LDA     star_row,X
        STA     tmp1
        LDA     star_col,X
        STA     tmp2
        PHX
        JSR     putcell
        PLX
        INX
        CPX     #NSTARS
        BNE     @stars

        ; --- ground band (rows 16..23) : green ---
        LDA     #16
        STA     rowidx
@grnd:
        LDA     rowidx
        STA     tmp1
        LDA     #0
        STA     tmp2
        LDA     #G_GRASS
        STA     tmp3
        LDA     #C_GRASS
        STA     colidx
        LDA     #SCRW
        STA     cnt0
        JSR     fillrow_vram
        INC     rowidx
        LDA     rowidx
        CMP     #24
        BNE     @grnd

        ; --- a mountain range across the horizon (row 15) ---
        LDA     #15
        STA     tmp1
        LDA     #0
        STA     tmp2
        LDA     #G_MOUNT
        STA     tmp3
        LDA     #C_MOUNT
        STA     colidx
        LDA     #SCRW
        STA     cnt0
        JSR     fillrow_vram

        JSR     vid_exit

        ; --- scenery objects (single tiles) via putcell helper ---
        JSR     draw_scenery

        ; --- the big block-letter logo ---
        JSR     draw_logo

        ; --- text: subtitle, prompt, controls (colored) ---
        LDX     #26
        LDY     #10
        JSR     locate
        LDA     #COLOR(CO_BRTURQ, CO_BLACK)
        STA     CURCOLOR
        PRINTMSG ttl_by

        LDX     #23
        LDY     #18
        JSR     locate
        LDA     #COLOR(CO_BRYELLOW, CO_BLACK)
        STA     CURCOLOR
        PRINTMSG ttl_prompt

        LDX     #20
        LDY     #21
        JSR     locate
        LDA     #COLOR(CO_BRWHITE, CO_BLACK)
        STA     CURCOLOR
        PRINTMSG ttl_keys1
        LDX     #20
        LDY     #22
        JSR     locate
        PRINTMSG ttl_keys2
        RTS

;----------------------------------------------------------------
; draw_scenery - place tile objects on the landscape: forests, a
; castle, a lake, and a big dragon (boss glyph) menacing the scene.
;----------------------------------------------------------------
draw_scenery:
        JSR     vid_enter
        ; forests dotted along the ground (row 16/17)
        LDX     #0
@trees:
        LDA     tree_row,X
        STA     tmp1
        LDA     tree_col,X
        STA     tmp2
        LDA     #G_FOREST
        STA     tmp3
        LDA     #C_FOREST
        STA     colidx
        PHX
        JSR     putcell
        PLX
        INX
        CPX     #NTREES
        BNE     @trees

        ; a castle on the left hill (row 14-15 area)
        LDA     #14
        STA     tmp1
        LDA     #6
        STA     tmp2
        LDA     #G_CASTLE
        STA     tmp3
        LDA     #C_CASTLE
        STA     colidx
        JSR     putcell
        LDA     #14
        STA     tmp1
        LDA     #7
        STA     tmp2
        JSR     putcell

        ; a small lake on the right (row 17)
        LDA     #17
        STA     tmp1
        LDA     #62
        STA     tmp2
        LDA     #G_WATER
        STA     tmp3
        LDA     #C_WATER
        STA     colidx
        LDA     #10
        STA     cnt0
        JSR     fillrow_vram

        ; the DRAGON - big and red, hovering at the right of the sky
        LDA     #12
        STA     tmp1
        LDA     #60
        STA     tmp2
        LDA     #G_BOSS
        STA     tmp3
        LDA     #COLOR(CO_BRRED, CO_BLUE)
        STA     colidx
        JSR     putcell
        ; a second dragon, smaller/left, different color
        LDA     #6
        STA     tmp1
        LDA     #66
        STA     tmp2
        LDA     #G_BOSS
        STA     tmp3
        LDA     #COLOR(CO_BRYELLOW, CO_BLUE)
        STA     colidx
        JSR     putcell

        JMP     vid_exit

;----------------------------------------------------------------
; draw_logo - stamp the big block-letter "WYRMHOLD" logo using
; solid wall tiles in bright gold.  Each letter is a 5x5 bitmap
; (5 bytes, top bit = leftmost of 5 columns).  Letters are spaced
; BL_W+1 columns apart.
;----------------------------------------------------------------
draw_logo:
        JSR     vid_enter
        LDA     #0
        STA     logo_li         ; letter index
@letter:
        LDA     logo_li
        CMP     #8              ; 8 letters
        BCC     :+
        JMP     @done
:
        ; pointer to this letter's 5 bytes:  logo_font + li*5
        LDA     logo_li
        STA     tmp0
        ASL     A               ; *2
        ASL     A               ; *4
        CLC
        ADC     tmp0            ; *5
        TAY                     ; Y = li*5 offset into logo_font
        ; left screen column for this letter = BL_COL + li*(BL_W+1)
        LDA     logo_li
        STA     tmp0
        ASL     A               ; *2
        ASL     A               ; *4
        CLC
        ADC     tmp0            ; *5
        CLC
        ADC     logo_li         ; + li  -> li*6
        CLC
        ADC     #BL_COL
        STA     logo_lx         ; base column
        ; draw 5 rows
        LDA     #0
        STA     logo_ry
@row:
        LDA     logo_ry
        CMP     #BL_H
        BCS     @nextletter
        ; fetch the bitmap byte for (li,row): logo_font[li*5 + row]
        TYA
        CLC
        ADC     logo_ry
        TAX
        LDA     logo_font,X
        STA     logo_bits       ; 5 bits, bit4..bit0 = cols 0..4
        ; draw 5 columns
        LDA     #0
        STA     logo_cx
@col:
        LDA     logo_cx
        CMP     #BL_W
        BCS     @nextrow
        ; test bit (4 - cx) of logo_bits
        LDA     #%00010000      ; bit4 mask
        LDX     logo_cx
        BEQ     @havemask
@shr:
        LSR     A
        DEX
        BNE     @shr
@havemask:
        AND     logo_bits
        BEQ     @blank
        ; set pixel -> solid gold block
        LDA     logo_ry
        CLC
        ADC     #BL_ROW
        STA     tmp1            ; screen row
        LDA     logo_lx
        CLC
        ADC     logo_cx
        STA     tmp2            ; screen col
        LDA     #G_WALL
        STA     tmp3
        LDA     #COLOR(CO_BRYELLOW, CO_RED)
        STA     colidx
        JSR     putcell
@blank:
        INC     logo_cx
        JMP     @col
@nextrow:
        INC     logo_ry
        ; Y must be reloaded (clobbered above): recompute li*5
        LDA     logo_li
        STA     tmp0
        ASL     A
        ASL     A
        CLC
        ADC     tmp0
        TAY
        JMP     @row
@nextletter:
        INC     logo_li
        JMP     @letter
@done:
        JMP     vid_exit

;----------------------------------------------------------------
; Big-letter 5x5 font for  W Y R M H O L D  (bit4..bit0 = 5 cols)
;----------------------------------------------------------------
logo_font:
        ; W
        .BYTE   %10001
        .BYTE   %10001
        .BYTE   %10101
        .BYTE   %11011
        .BYTE   %10001
        ; Y
        .BYTE   %10001
        .BYTE   %01010
        .BYTE   %00100
        .BYTE   %00100
        .BYTE   %00100
        ; R
        .BYTE   %11110
        .BYTE   %10001
        .BYTE   %11110
        .BYTE   %10010
        .BYTE   %10001
        ; M
        .BYTE   %10001
        .BYTE   %11011
        .BYTE   %10101
        .BYTE   %10001
        .BYTE   %10001
        ; H
        .BYTE   %10001
        .BYTE   %10001
        .BYTE   %11111
        .BYTE   %10001
        .BYTE   %10001
        ; O
        .BYTE   %01110
        .BYTE   %10001
        .BYTE   %10001
        .BYTE   %10001
        .BYTE   %01110
        ; L
        .BYTE   %10000
        .BYTE   %10000
        .BYTE   %10000
        .BYTE   %10000
        .BYTE   %11111
        ; D
        .BYTE   %11110
        .BYTE   %10001
        .BYTE   %10001
        .BYTE   %10001
        .BYTE   %11110

;----------------------------------------------------------------
; decorative element positions
;----------------------------------------------------------------
NSTARS  = 10
star_row:
        .BYTE   1, 2, 0, 3, 1, 2, 0, 4, 1, 3
star_col:
        .BYTE   4, 12, 22, 30, 44, 52, 58, 16, 70, 74

NTREES  = 9
tree_row:
        .BYTE   16, 17, 16, 16, 17, 16, 17, 16, 16
tree_col:
        .BYTE   18, 24, 30, 38, 44, 50, 14, 56, 34
