;______________________________________________________________________________
;  tiles.asm - custom 8x8 character bitmaps for terrain, items, monsters
;
;  The memory-mapped video card has a writable character generator.
;  To redefine character N (see V_PATTERN in dbasic/screencmds.asm):
;     1. map video sub-page $F8 (VIDEOBANK) into CPU bank $A
;     2. write N to VideoCharGenOffset ($A002)
;     3. write 8 scanline bytes to VideoCharGenData ($A003)
;        (the hardware auto-advances through the 8 rows of the glyph)
;
;  Each bitmap is 8 bytes, top row first; bit7 = leftmost pixel.
;  We redefine codes 128.. (G_* constants) so the normal ASCII font
;  used for UI text is left intact.
;______________________________________________________________________________

VideoCharGenOffset = $A002
VideoCharGenData = $A003

;----------------------------------------------------------------
; cg_enter / cg_exit - map the character-generator RAM (page $F8)
; into bank $A and select task 1 (mirrors PAGE_ENTER/PAGE_EXIT).
;----------------------------------------------------------------
cg_enter:
        LDA     #$01            ; task 01 (driver)
        LDX     #$0A            ; CPU bank $A
        LDY     #VIDEOBANK      ; physical page $F8 (char generator)
        JSR     SETPAGE
        LDA     #$01
        STA     PC6502_ACT_TASK
        RTS
cg_exit:
        LDA     #$00
        STA     PC6502_ACT_TASK
        RTS

;----------------------------------------------------------------
; chargen_init - upload every custom glyph bitmap.
;
; Driven by a table of (charcode, 8 bytes) records.  The table ends
; with a $00 charcode marker (code 0 is never one of ours).
;----------------------------------------------------------------
chargen_init:
        JSR     cg_enter
        SETW16  srcp, glyphtab
@rec:
        LDY     #0
        LDA     (srcp),Y        ; char code
        BEQ     @done           ; 0 = end of table
        STA     VideoCharGenOffset
; write the 8 scanline bytes
        LDY     #1
@row:
        LDA     (srcp),Y
        STA     VideoCharGenData
        INY
        CPY     #9
        BNE     @row
; advance srcp by 9 (1 code + 8 data)
        CLC
        LDA     srcp
        ADC     #9
        STA     srcp
        LDA     srcp+1
        ADC     #0
        STA     srcp+1
        JMP     @rec
@done:
        JMP     cg_exit

;----------------------------------------------------------------
; Glyph bitmap table: each row is  code, b0..b7
;----------------------------------------------------------------
glyphtab:

; --- terrain ---------------------------------------------------
; Terrain is kept very light so the player and monsters read clearly
; against it.  The cell's background color fills the empty pixels.

; grass: blank (color plane gives it its green)
        .BYTE   G_GRASS
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000

; forest: a single small tree, lots of empty space
        .BYTE   G_FOREST
        .BYTE   %00000000
        .BYTE   %00011000
        .BYTE   %00111100
        .BYTE   %00111100
        .BYTE   %00011000
        .BYTE   %00011000
        .BYTE   %00000000
        .BYTE   %00000000

; mountain: a simple peak
        .BYTE   G_MOUNT
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00011000
        .BYTE   %00111100
        .BYTE   %01111110
        .BYTE   %11111111
        .BYTE   %00000000
        .BYTE   %00000000

; water: a couple of gentle ripples
        .BYTE   G_WATER
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %01100110
        .BYTE   %10011001
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000

; town: one tidy house
        .BYTE   G_TOWN
        .BYTE   %00000000
        .BYTE   %00011000
        .BYTE   %00111100
        .BYTE   %01111110
        .BYTE   %01111110
        .BYTE   %01100110
        .BYTE   %01100110
        .BYTE   %00000000

; dungeon: an arched cave mouth (outline)
        .BYTE   G_DUNG
        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01111110
        .BYTE   %00000000

; castle: crenellated tower (outline)
        .BYTE   G_CASTLE
        .BYTE   %10101010
        .BYTE   %11111110
        .BYTE   %10000010
        .BYTE   %10010010
        .BYTE   %10000010
        .BYTE   %10000010
        .BYTE   %11111110
        .BYTE   %00000000

; road: a faint dashed path
        .BYTE   G_ROAD
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %01100110
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000

; bridge: two rails with planks between
        .BYTE   G_BRIDGE
        .BYTE   %11111111
        .BYTE   %00000000
        .BYTE   %00100100
        .BYTE   %00100100
        .BYTE   %00100100
        .BYTE   %00000000
        .BYTE   %11111111
        .BYTE   %00000000

; floor: blank (interior is just dark)
        .BYTE   G_FLOOR
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000

; wall: solid block (fills the whole cell)
        .BYTE   G_WALL
        .BYTE   %11111111
        .BYTE   %11111111
        .BYTE   %11111111
        .BYTE   %11111111
        .BYTE   %11111111
        .BYTE   %11111111
        .BYTE   %11111111
        .BYTE   %11111111

; door: a clear doorway outline
        .BYTE   G_DOOR
        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %00100100
        .BYTE   %00100100
        .BYTE   %00100100
        .BYTE   %00101100
        .BYTE   %00100100
        .BYTE   %00000000

; treasure: a small chest
        .BYTE   G_TREAS
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01111110
        .BYTE   %01011010
        .BYTE   %01111110
        .BYTE   %00000000
        .BYTE   %00000000

; stairs up: clear diagonal steps
        .BYTE   G_UPSTAIR
        .BYTE   %00000000
        .BYTE   %00000110
        .BYTE   %00001100
        .BYTE   %00011000
        .BYTE   %00110000
        .BYTE   %01100000
        .BYTE   %00000000
        .BYTE   %00000000

; shop: a coin
        .BYTE   G_SHOP
        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %01011010
        .BYTE   %01011010
        .BYTE   %01000010
        .BYTE   %00111100
        .BYTE   %00000000

; --- player ----------------------------------------------------
; player: a clear little figure
        .BYTE   G_PLAYER
        .BYTE   %00011000
        .BYTE   %00011000
        .BYTE   %00111100
        .BYTE   %01011010
        .BYTE   %00011000
        .BYTE   %00011000
        .BYTE   %00100100
        .BYTE   %00100100

; --- monsters --------------------------------------------------
; Distinct silhouettes with empty margins so they stand out and are
; easy to tell apart.

; orc: squat brute with two tusks at the bottom
        .BYTE   G_ORC
        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01011010
        .BYTE   %01111110
        .BYTE   %01111110
        .BYTE   %01011010
        .BYTE   %00100100
        .BYTE   %00000000

; snake: slim S-curve
        .BYTE   G_SNAKE
        .BYTE   %00000000
        .BYTE   %00111000
        .BYTE   %00100000
        .BYTE   %00111000
        .BYTE   %00001000
        .BYTE   %00011000
        .BYTE   %00000000
        .BYTE   %00000000

; skeleton: skull on a thin body
        .BYTE   G_SKELETON
        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01011010
        .BYTE   %00111100
        .BYTE   %00011000
        .BYTE   %00111100
        .BYTE   %00100100
        .BYTE   %00000000

; thief: hooded, narrow shoulders
        .BYTE   G_THIEF
        .BYTE   %00000000
        .BYTE   %00011000
        .BYTE   %00111100
        .BYTE   %00011000
        .BYTE   %00111100
        .BYTE   %00011000
        .BYTE   %00100100
        .BYTE   %00000000

; troll: big broad ogre
        .BYTE   G_TROLL
        .BYTE   %00000000
        .BYTE   %01100110
        .BYTE   %01111110
        .BYTE   %11111111
        .BYTE   %01111110
        .BYTE   %01111110
        .BYTE   %01100110
        .BYTE   %00000000

; boss: a winged dragon (clear wings + body)
        .BYTE   G_BOSS
        .BYTE   %10000001
        .BYTE   %11000011
        .BYTE   %01100110
        .BYTE   %00111100
        .BYTE   %01111110
        .BYTE   %11011011
        .BYTE   %00100100
        .BYTE   %00000000

        .BYTE   0               ; end of table
