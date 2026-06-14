;______________________________________________________________________________
;  hires.asm - HIRES color render layer (single hires, 140x192, 16 colors)
;
;  The video card exposes a HIRES color buffer at VRAM $2000.  Address math
;  (from V_PLOT_HIRES_COLOR in dbasic/screencmds.asm):
;     byteoff = Y*70 + (X>>1)            (Y 0..191, X 0..139)
;     2 pixels per byte: X even -> low nibble, X odd -> high nibble
;     each nibble = a 16-color value
;  VRAM is mapped into CPU bank $A in 4KB windows:
;     physaddr = $2000 + byteoff
;     page     = (physaddr>>12) + VIDEOBANK     ; $2xxx->$FA, $3xxx->$FB, ...
;     window   = ($A000) | (physaddr & $0FFF)
;  13,440 bytes span physical pages $FA..$FE.
;
;  We precompute, per scanline y (0..191): the page and the in-window base
;  address ($A000-form) of the start of that scanline, so the blitter never
;  multiplies or re-pages per pixel.
;______________________________________________________________________________

; --- video config registers, in bank-$A mapped form (page $F8 -> $A000) ---
HVideoTextMode   = $A005
HVideoLoresMode  = $A006
HVideoDoubleLores= $A007
HVideoHiresMode  = $A008
HVideoDoubleHires= $A009
HVideo80col      = $A00A
HVideoMixedMode  = $A00B
HVideoQuadHires  = $A00C
HVideoMonoHires  = $A00D

HIRES_BASE      = $2000         ; VRAM byte offset of hires buffer
HROW_BYTES      = 70            ; bytes per scanline (single hires)
HROWS           = 192           ; scanlines

;----------------------------------------------------------------
; cfg_enter / cfg_exit - map the config page ($F8) into bank $A.
;----------------------------------------------------------------
cfg_enter:
        LDA     #$01            ; task 01
        LDX     #$0A            ; bank $A
        LDY     #VIDEOBANK      ; physical page $F8 (config + chargen)
        JSR     SETPAGE
        LDA     #$01
        STA     PC6502_ACT_TASK
        RTS
cfg_exit:
        LDA     #$00
        STA     PC6502_ACT_TASK
        RTS

;----------------------------------------------------------------
; hpage - map physical page A into bank $A and activate task 1.
;   IN: A = physical page ($FA..$FE)
;   trashes X,Y (and A)
;----------------------------------------------------------------
hpage:
        TAY                     ; Y = physical page
        LDA     #$01            ; task 01
        LDX     #$0A            ; bank $A
        JSR     SETPAGE
        LDA     #$01
        STA     PC6502_ACT_TASK
        RTS

;----------------------------------------------------------------
; hires_mode_on - switch the card to single HIRES + mixed mode and
; clear the hires buffer.  (Mirrors SETUPMODE2 / SETUPMODE2_CLEAR.)
;----------------------------------------------------------------
hires_mode_on:
        JSR     cfg_enter
        LDA     #$01
        STA     HVideoHiresMode         ; HIRES on
        LDA     #$02
        STA     HVideoTextMode          ; text off
        STA     HVideoLoresMode         ; lores off
        STA     HVideoDoubleHires       ; single (double off)
        STA     HVideoQuadHires
        STA     HVideoMonoHires
        LDA     #$01
        STA     HVideoMixedMode         ; mixed: 4 text rows at bottom
        JSR     cfg_exit

        JSR     hires_clear
        JSR     hrow_build
        RTS

;----------------------------------------------------------------
; hires_mode_off - restore 80-column text mode (for exit to DOS).
;----------------------------------------------------------------
hires_mode_off:
        LDA     #FC_SETMODE
        STA     farfunct
        LDA     #1                      ; 80 col text
        JSR     DO_FARCALL
        RTS

;----------------------------------------------------------------
; hires_clear - zero the hires buffer (physical pages $FA..$FE).
;----------------------------------------------------------------
hires_clear:
        LDA     #(HIRES_BASE>>12)+VIDEOBANK     ; first page = $FA
        STA     tmp0                    ; current page
@pgloop:
        LDA     tmp0
        JSR     hpage
        ; zero $A000-$AFFF
        LDA     #$00
        STA     ptr
        LDA     #$A0
        STA     ptr+1
        LDY     #$00
        LDA     #$00
@zl:
        STA     (ptr),Y
        INC     ptr
        BNE     @zl
        INC     ptr+1
        LDX     ptr+1
        CPX     #$B0
        BNE     @zl
        INC     tmp0
        LDA     tmp0
        CMP     #$FF                    ; clear $FA..$FE (stop at $FF)
        BNE     @pgloop
        JSR     cfg_exit
        RTS

;----------------------------------------------------------------
; hrow_build - fill hrow_lo[y]/hrow_hi[y] with the ABSOLUTE VRAM
; address (HIRES_BASE + y*70) for each scanline y=0..191.  The
; blitter derives page + in-window offset from the absolute address
; per byte, so a tile row that straddles a 4KB page boundary is
; handled correctly.
;----------------------------------------------------------------
hrow_build:
        LDA     #<HIRES_BASE
        STA     ptr
        LDA     #>HIRES_BASE
        STA     ptr+1
        LDX     #0
@row:
        LDA     ptr
        STA     hrow_lo,X
        LDA     ptr+1
        STA     hrow_hi,X
        CLC
        LDA     ptr
        ADC     #HROW_BYTES
        STA     ptr
        LDA     ptr+1
        ADC     #0
        STA     ptr+1
        INX
        CPX     #HROWS
        BNE     @row
        RTS

;----------------------------------------------------------------
; hblit_tile - draw one 24x24 packed tile (TILESZ=288 bytes).
;   IN: tmp0 = tile id (art index)
;       tmp1 = viewport tile row  -> py = tilerow*24
;       tmp2 = viewport tile col  -> xbyte = tilecol*12  (px/2)
;   Each tile = 24 rows x 12 bytes pre-packed.
;
;   srcp -> tile art ; cnt0 = scanline counter ; rowidx = current y
;----------------------------------------------------------------
hblit_tile:
        ; srcp = tile_gfx + tile*288.  288 = 256 + 32.
        ;   id*288 = (id<<8) + (id<<5)
        LDA     #0
        STA     srcp
        LDA     tmp0
        STA     srcp+1                  ; srcp = id*256
        ; add id*32 (16-bit)
        LDA     tmp0
        STA     tmp3                    ; id
        LDA     #0
        STA     ptr2+1
        LDA     tmp3
        ASL     A                       ; *2
        ROL     ptr2+1
        ASL     A                       ; *4
        ROL     ptr2+1
        ASL     A                       ; *8
        ROL     ptr2+1
        ASL     A                       ; *16
        ROL     ptr2+1
        ASL     A                       ; *32
        ROL     ptr2+1
        STA     ptr2                    ; ptr2 = id*32
        CLC
        LDA     srcp
        ADC     ptr2
        STA     srcp
        LDA     srcp+1
        ADC     ptr2+1
        STA     srcp+1
        ; + base tile_gfx
        CLC
        LDA     srcp
        ADC     #<tile_gfx
        STA     srcp
        LDA     srcp+1
        ADC     #>tile_gfx
        STA     srcp+1

        ; starting screen y = tilerow*24 = tilerow*16 + tilerow*8
        LDA     tmp1
        ASL     A
        ASL     A
        ASL     A                       ; *8
        STA     tmp3                    ; tilerow*8
        ASL     A                       ; *16
        CLC
        ADC     tmp3                    ; *16 + *8 = *24
        STA     rowidx                  ; current scanline y

        ; X byte offset within a row = tilecol*12 = tilecol*8 + tilecol*4
        LDA     tmp2
        ASL     A
        ASL     A                       ; *4
        STA     tmp3                    ; tilecol*4
        ASL     A                       ; *8
        CLC
        ADC     tmp3                    ; *8 + *4 = *12
        STA     tmp3                    ; xbyte

        LDA     #TILEROWS
        STA     cnt0                    ; 24 scanlines
        LDA     #$FF
        STA     curpage                 ; force a page set on first byte
@line:
        ; absolute VRAM addr of this scanline's first tile byte:
        ;   rowabs = hrow[rowidx] + xbyte   -> ptr (kept across bytes)
        LDX     rowidx
        CLC
        LDA     hrow_lo,X
        ADC     tmp3
        STA     ptr
        LDA     hrow_hi,X
        ADC     #0
        STA     ptr+1
        ; copy TILEBYTES (12) bytes; tmp2 = byte index, Y = source index
        LDY     #0
@cp:
        STY     tmp2
        CLC
        LDA     ptr
        ADC     tmp2
        STA     ptr2
        LDA     ptr+1
        ADC     #0
        STA     ptr2+1
        JSR     hsetptr                 ; dstp = exact window addr; re-page if needed
        LDY     tmp2
        LDA     (srcp),Y                ; source byte
        LDX     #0
        STA     (dstp,X)                ; store to window addr
        INY
        CPY     #TILEBYTES
        BNE     @cp
        ; advance srcp by TILEBYTES
        CLC
        LDA     srcp
        ADC     #TILEBYTES
        STA     srcp
        LDA     srcp+1
        ADC     #0
        STA     srcp+1
        ; next scanline
        INC     rowidx
        DEC     cnt0
        BNE     @line
        JSR     cfg_exit
        RTS

;----------------------------------------------------------------
; hsetptr - given absolute VRAM address in ptr2, set:
;   curpage = (addr>>12)+VIDEOBANK  (page in, only if it changed)
;   dstp    = $A000 | (addr & $0FFF)
; trashes A,X,Y when it re-pages.
;----------------------------------------------------------------
hsetptr:
        LDA     ptr2+1
        LSR     A
        LSR     A
        LSR     A
        LSR     A                       ; (addr>>12)
        CLC
        ADC     #VIDEOBANK
        CMP     curpage
        BEQ     @same
        STA     curpage
        JSR     hpage                   ; trashes A,X,Y
@same:
        LDA     ptr2
        STA     dstp
        LDA     ptr2+1
        AND     #$0F
        ORA     #$A0
        STA     dstp+1
        RTS

;----------------------------------------------------------------
; render_view - draw the VPTILESW x VPTILESH tile viewport centered
; on the player, then overlay visible monsters and the player.
;
; For each viewport tile (vc,vr): world coord = (px-VPCX+vc, py-VPCY+vr);
; tileat -> tile code (== art id for terrain 0..14); hblit_tile.
;
; Loop counters live in vrow/vcol (BSS) because hblit_tile clobbers
; tmp0-3/rowidx/cnt0.  World coords for the lookup go in tgtx/tgty.
;----------------------------------------------------------------
render_view:
        LDA     #0
        STA     vrow
@rowloop:
        ; world y = py - VPCY + vrow
        SEC
        LDA     py
        SBC     #VPCY
        CLC
        ADC     vrow
        STA     vwy                     ; remember world-y for this row
        LDA     #0
        STA     vcol
@colloop:
        ; world x = px - VPCX + vcol
        SEC
        LDA     px
        SBC     #VPCX
        CLC
        ADC     vcol
        STA     tgtx
        LDA     vwy
        STA     tgty
        JSR     tileat                  ; -> tgttile (terrain code = art id)
        LDA     tgttile
        STA     tmp0                    ; art id
        LDA     vrow
        STA     tmp1                    ; tile row
        LDA     vcol
        STA     tmp2                    ; tile col
        JSR     hblit_tile
        INC     vcol
        LDA     vcol
        CMP     #VPTILESW
        BNE     @colloop
        INC     vrow
        LDA     vrow
        CMP     #VPTILESH
        BNE     @rowloop

        ; overlay monsters
        JSR     hdraw_monsters
        ; overlay player at fixed center tile
        LDA     #GFX_PLAYER
        STA     tmp0
        LDA     #VPCY
        STA     tmp1
        LDA     #VPCX
        STA     tmp2
        JMP     hblit_tile

;----------------------------------------------------------------
; hblit_world - blit art (tmp0) at WORLD coords (tgtx,tgty) IF that
; cell is within the viewport.  Used to overlay entities.
;   converts world->viewport tile, range-checks, then hblit_tile.
;----------------------------------------------------------------
hblit_world:
        ; vc = tgtx - px + VPCX
        SEC
        LDA     tgtx
        SBC     px
        CLC
        ADC     #VPCX
        BMI     @no
        CMP     #VPTILESW
        BCS     @no
        STA     tmp2                    ; tile col
        ; vr = tgty - py + VPCY
        SEC
        LDA     tgty
        SBC     py
        CLC
        ADC     #VPCY
        BMI     @no
        CMP     #VPTILESH
        BCS     @no
        STA     tmp1                    ; tile row
        JMP     hblit_tile              ; tmp0 already = art id
@no:
        RTS

;----------------------------------------------------------------
; hdraw_monsters - overlay every visible monster as its art tile.
; (mon type -> art id: M_ORC=1 -> GFX_ORC=16, so art = type-1+GFX_ORC.)
;----------------------------------------------------------------
hdraw_monsters:
        LDA     #0
        STA     monidx
@dm:
        LDX     monidx
        LDA     mon_type,X
        CMP     #M_NONE
        BEQ     @next
        ; art id = (type-1) + GFX_ORC
        SEC
        SBC     #1
        CLC
        ADC     #GFX_ORC
        STA     tmp0                    ; art id (preserved across hblit via tmp0? no)
        ; stash art in a var that hblit_world won't clobber before use
        STA     mart
        LDA     mon_x,X
        STA     tgtx
        LDA     mon_y,X
        STA     tgty
        LDA     mart
        STA     tmp0
        JSR     hblit_world
@next:
        INC     monidx
        LDA     monidx
        CMP     #MAXMON
        BNE     @dm
        RTS
