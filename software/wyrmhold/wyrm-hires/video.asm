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

; (draw_frame removed - the old text-mode frame referenced the retired
;  text viewport geometry; HIRES mode draws the play area via render_view
;  and the UI via the bottom text rows.)

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
; (render_view / plot_view_cell now live in hires.asm - the HIRES
;  render layer replaces the old text-mode viewport.)
;----------------------------------------------------------------

;----------------------------------------------------------------
; Static UI strings
;----------------------------------------------------------------
titlebar:
        .BYTE   "= W Y R M H O L D =",0
panelhdr:
        .BYTE   "ADVENTURER",0
