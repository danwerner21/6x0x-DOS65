;______________________________________________________________________________
;  title.asm - HIRES graphical title screen
;
;  Lays out an 8x10 tile scene (same grid the game viewport uses) from a
;  fixed layout table, blits it with hblit_tile, then prints the title and
;  prompt in the bottom mixed-mode text rows.
;______________________________________________________________________________

;----------------------------------------------------------------
; draw_title_scene - paint the title scene + text.
;----------------------------------------------------------------
draw_title_scene:
        JSR     hires_clear
        ; blit the 8x10 layout from title_map (row-major, VPTILESW wide)
        LDA     #0
        STA     vrow
        SETW16  srcp2, title_map
@row:
        LDA     #0
        STA     vcol
@col:
        LDY     #0
        LDA     (srcp2),Y               ; tile id for this cell
        STA     tmp0
        LDA     vrow
        STA     tmp1
        LDA     vcol
        STA     tmp2
        JSR     hblit_tile
        ; advance layout pointer
        INC     srcp2
        BNE     :+
        INC     srcp2+1
:
        INC     vcol
        LDA     vcol
        CMP     #VPTILESW
        BNE     @col
        INC     vrow
        LDA     vrow
        CMP     #VPTILESH
        BNE     @row

        ; --- title + prompt in the bottom text rows ---
        LDX     #2
        LDY     #STATY0
        JSR     locate
        LDA     #COLOR(CO_BRYELLOW, CO_BLACK)
        STA     CURCOLOR
        PRINTMSG ttl_name
        LDX     #2
        LDY     #STATY1
        JSR     locate
        LDA     #COLOR(CO_BRCYAN, CO_BLACK)
        STA     CURCOLOR
        PRINTMSG ttl_by
        LDX     #2
        LDY     #MSGY0
        JSR     locate
        LDA     #COLOR(CO_BRWHITE, CO_BLACK)
        STA     CURCOLOR
        PRINTMSG ttl_prompt
        LDX     #2
        LDY     #MSGY0+1
        JSR     locate
        PRINTMSG ttl_keys1
        RTS

;----------------------------------------------------------------
; Title scene layout: VPTILESW(5) x VPTILESH(6) tile ids.
; Mountains + castle up top, the dragon centre stage, a town/dungeon
; and water below.
;----------------------------------------------------------------
title_map:
        ;     col:   0           1           2           3           4
        .BYTE   GFX_MOUNT,  GFX_FOREST, GFX_CASTLE, GFX_FOREST, GFX_MOUNT   ; row 0
        .BYTE   GFX_FOREST, GFX_GRASS,  GFX_GRASS,  GFX_GRASS,  GFX_FOREST  ; 1
        .BYTE   GFX_GRASS,  GFX_GRASS,  GFX_BOSS,   GFX_GRASS,  GFX_GRASS   ; 2
        .BYTE   GFX_GRASS,  GFX_TOWN,   GFX_GRASS,  GFX_DUNG,   GFX_GRASS   ; 3
        .BYTE   GFX_ROAD,   GFX_ROAD,   GFX_BRIDGE, GFX_ROAD,   GFX_ROAD    ; 4
        .BYTE   GFX_WATER,  GFX_WATER,  GFX_BRIDGE, GFX_WATER,  GFX_WATER   ; 5

ttl_name:
        .BYTE   "W Y R M H O L D",0
