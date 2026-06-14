;______________________________________________________________________________
;  defines.asm - system equates, constants and game state
;______________________________________________________________________________

;----------------------------------------------------------------
; DOS/65 system entry points
;----------------------------------------------------------------
DFLFCB          = $107          ; DEFAULT FCB
PEM             = $103          ; PEM ENTRY
BOOT            = $100          ; WARM BOOT (exit back to DOS/65)

;----------------------------------------------------------------
; Memory-mapped video state (firmware-shared, fixed addresses)
;----------------------------------------------------------------
CURX            = $0647         ; current cursor column
CURY            = $0648         ; current cursor row
SHOWCRSR        = $0649         ; show cursor (1=yes,0=no)
CURCOLOR        = $064A         ; current print color
CSRCOLOR        = $064B         ; current cursor color
BVIDEOMODE      = $064C         ; current video mode (0=40,1=80)

;----------------------------------------------------------------
; FARCALL dispatch
;----------------------------------------------------------------
DO_FARCALL      = $FFF0
farfunct        = $32           ; zero-page: FARCALL function number

; MMU / video paging
SETPAGE         = $FFF6         ; A=task, X=bank, Y=physical page
VIDEOBANK       = $F8           ; base physical page for video RAM
PC6502_ACT_TASK = $EFE0         ; MMU active task register (write only)
VIDTEXT_PAGE    = $01+VIDEOBANK ; physical page for text page 1 ($F9)

; FARCALL function numbers (see 6502PC_firmware/dos65drv.asm)
FC_CHROUT       = 19
FC_CHRIN        = 20            ; non-blocking, returns A=key or $FF
FC_LOCATE       = 37            ; X=col, Y=row
FC_SCNCLR       = 38
FC_COLOR        = 39            ; X=color, Y=cursor color
FC_SCROLLDN     = 56
FC_SETMODE      = 57
FC_PAINTCSR     = 58
FC_UNPAINTCSR   = 59

;----------------------------------------------------------------
; Video memory window (we map sub-page $F9 into CPU bank $A,
; matching software/speedscript/screen.asm).
;   text  chars : $A000 + row*80 + col
;   color cells : $A800 + row*80 + col   (low nibble fg, high nibble bg)
;----------------------------------------------------------------
VRAM_TEXT       = $A000
VRAM_COLOR      = $A800
SCRW            = 80
SCRH            = 24

;----------------------------------------------------------------
; Color values.  color byte = (bg<<4) | fg
; Base palette nibbles (typical 4-bit IRGB-ish ordering used by the card)
;----------------------------------------------------------------
CO_BLACK        = $0
CO_RED          = $1
CO_GREEN        = $2
CO_YELLOW       = $3
CO_BLUE         = $4
CO_MAGENTA      = $5
CO_CYAN         = $6
CO_WHITE        = $7
CO_GREY         = $8
CO_BRRED        = $9
CO_BRGREEN      = $A
CO_BRYELLOW     = $B
CO_BRBLUE       = $C
CO_BRMAGENTA    = $D
CO_BRCYAN       = $E
CO_BRWHITE      = $F

; convenience: build a color byte from fg,bg nibbles
        .DEFINE COLOR(fg,bg) (((bg)<<4)|(fg))

; named cell colors used throughout the game
C_GRASS         = COLOR(CO_BRGREEN, CO_BLACK)
C_WATER         = COLOR(CO_BRBLUE,  CO_BLUE)
C_FOREST        = COLOR(CO_GREEN,   CO_BLACK)
C_MOUNT         = COLOR(CO_GREY,    CO_BLACK)
C_TOWN          = COLOR(CO_BRYELLOW,CO_BLACK)
C_CASTLE        = COLOR(CO_BRWHITE, CO_BLACK)
C_DUNG          = COLOR(CO_BRMAGENTA,CO_BLACK)
C_ROAD          = COLOR(CO_BRYELLOW,CO_BLACK)
C_BRIDGE        = COLOR(CO_YELLOW,  CO_BLACK)
C_FLOOR         = COLOR(CO_GREY,    CO_BLACK)
C_WALL          = COLOR(CO_BRWHITE, CO_GREY)
C_TREAS         = COLOR(CO_BRYELLOW,CO_BLACK)
C_PLAYER        = COLOR(CO_BRWHITE, CO_BLACK)
C_MONST         = COLOR(CO_BRRED,   CO_BLACK)
C_BOSS          = COLOR(CO_BRRED,   CO_RED)
C_BORDER        = COLOR(CO_BRCYAN,  CO_BLACK)
C_PANEL         = COLOR(CO_BRWHITE, CO_BLACK)
C_PANELHDR      = COLOR(CO_BRYELLOW,CO_BLUE)
C_MSG           = COLOR(CO_BRWHITE, CO_BLACK)
C_TITLE         = COLOR(CO_BRYELLOW,CO_BLACK)
C_BLANK         = COLOR(CO_WHITE,   CO_BLACK)

; shop panel colors (blue panel with bright text)
C_SHOPBG        = COLOR(CO_BRWHITE, CO_BLUE)
C_SHOPBRD       = COLOR(CO_BRCYAN,  CO_BLUE)
C_SHOPTTL       = COLOR(CO_BRYELLOW,CO_BLUE)
C_SHOPTXT       = COLOR(CO_BRWHITE, CO_BLUE)
C_SHOPST        = COLOR(CO_BRGREEN, CO_BLUE)

; exit tiles (town door / dungeon stairs) - bright so they stand out
C_EXIT          = COLOR(CO_BLACK,   CO_BRGREEN)

;----------------------------------------------------------------
; Tile codes (index into terrain tables).  Stored in map data.
;----------------------------------------------------------------
T_GRASS         = 0
T_FOREST        = 1
T_MOUNT         = 2
T_WATER         = 3
T_TOWN          = 4
T_DUNG          = 5
T_CASTLE        = 6
T_ROAD          = 7
T_BRIDGE        = 8
T_FLOOR         = 9             ; dungeon/town floor
T_WALL          = 10            ; dungeon/town wall (impassable)
T_DOOR          = 11            ; exit tile (leave town/dungeon)
T_TREAS         = 12            ; treasure chest
T_UPSTAIR       = 13            ; leave dungeon
T_SHOP          = 14            ; shop counter inside a town
NUM_TILES       = 15

;----------------------------------------------------------------
; Map glyphs.  These are custom characters whose 8x8 bitmaps are
; uploaded to the character generator at startup by chargen_init
; (see tiles.asm).
;
; We use codes in the CONTROL range (1..31).  The UI only ever
; prints ASCII 32+, so these slots are free to redefine and they
; live safely in the low 128 of the font (some cards only redefine
; / display 128 glyphs and treat bit7 as an attribute, which broke
; the earlier 128+ codes - monsters showed the stock font).
;----------------------------------------------------------------
G_GRASS         = 1
G_FOREST        = 2
G_MOUNT         = 3
G_WATER         = 4
G_TOWN          = 5
G_DUNG          = 6
G_CASTLE        = 7
G_ROAD          = 8
G_BRIDGE        = 9
G_FLOOR         = 10
G_WALL          = 11
G_DOOR          = 12
G_TREAS         = 13
G_UPSTAIR       = 14
G_SHOP          = 15

G_PLAYER        = 16

; monster glyphs
G_ORC           = 17
G_SNAKE         = 18
G_SKELETON      = 19
G_THIEF         = 20
G_TROLL         = 21
G_BOSS          = 22

;----------------------------------------------------------------
; Monster type ids
;----------------------------------------------------------------
M_NONE          = 0
M_ORC           = 1
M_SNAKE         = 2
M_SKELETON      = 3
M_THIEF         = 4
M_TROLL         = 5
M_BOSS          = 6
NUM_MTYPE       = 7
MAXMON          = 12            ; max simultaneously active monsters

;----------------------------------------------------------------
; HIRES viewport geometry (24x24 tiles in the 140x192 hires area).
; Mixed mode reserves the bottom 4 TEXT rows (32 px); graphics area
; is the top 160 px = 6 tiles tall (144 px).  140 px wide = 5 tiles
; (120 px) plus a small right margin.  Player centered at tile (2,3).
; Larger tiles -> more detail per object (finer art).
;----------------------------------------------------------------
TILEPX          = 24            ; tile size in hires pixels
TILEBYTES       = 12            ; bytes per tile row (24 px / 2)
TILEROWS        = 24            ; rows per tile
TILESZ          = 288           ; bytes per tile (24*12)
VPTILESW        = 5             ; viewport width  in tiles
VPTILESH        = 6             ; viewport height in tiles
VPCX            = 2             ; player tile column within viewport (0-based)
VPCY            = 3             ; player tile row within viewport (0-based)

;----------------------------------------------------------------
; Text UI in the bottom 4 mixed-mode rows (text rows 20..23).
;   row 20    : stat line 1   (HP / Lvl / XP)
;   row 21    : stat line 2   (Gold / Food / Wpn / Armor / Where)
;   rows 22-23: scrolling message log
;----------------------------------------------------------------
STATY0          = 20
STATY1          = 21
MSGY0           = 22
MSGROWS         = 2

; map dimensions
OWW             = 64            ; overworld width
OWH             = 64            ; overworld height
TOWNW           = 32            ; town map width
TOWNH           = 20            ; town map height
DUNGW           = 32            ; dungeon map width
DUNGH           = 20            ; dungeon map height

; world/location ids (which map is active)
LOC_WORLD       = 0
LOC_TOWN        = 1
LOC_DUNG        = 2

; misc constants
space           = 32
ESC             = 27
CR              = 13

;----------------------------------------------------------------
; Zero-page allocation.  $00-$31 are largely free for the TEA;
; $32 (farfunct) is reserved by the system.  We use a contiguous
; block below it and a high block ($F0-$FE scratch pointers) the
; same way speedscript does.
;----------------------------------------------------------------
; 16-bit scratch pointers
ptr             = $10           ; general pointer (2)
ptr2            = $12           ; general pointer (2)
vptr            = $14           ; video text pointer (2)
cptr            = $16           ; video color pointer (2)
srcp            = $18           ; source pointer (2)
dstp            = $1A           ; dest pointer (2)
strp            = $1C           ; string pointer for prmsg (2)
numarg          = $1E           ; 16-bit number for displaynum (2)

; 8-bit scratch
tmp0            = $20
tmp1            = $21
tmp2            = $22
tmp3            = $23
cnt0            = $24
cnt1            = $25
colidx          = $26           ; saved column index
rowidx          = $27           ; saved row index
numspace        = $28           ; displaynum digit accumulator
keych           = $29           ; last key read
dx              = $2A           ; signed move delta x
dy              = $2B           ; signed move delta y
tgtx            = $2C           ; target tile x
tgty            = $2D           ; target tile y
tgttile         = $2E           ; tile code at target
monidx          = $2F           ; current monster index
seedlo          = $30           ; RNG state low
seedhi          = $31           ; RNG state high
; $32 = farfunct (system)
seed2lo         = $33           ; RNG state 2 low
seed2hi         = $34           ; RNG state 2 high
srcp2           = $35           ; 2nd source pointer (title layout) (2)

;----------------------------------------------------------------
; Game state (in TEA RAM, defined as labels in wyrmhold.asm BSS area)
; declared here as equates is not possible; real storage is in the
; .SEGMENT "TEA" at the end of wyrmhold.asm.  See gamevars there.
;----------------------------------------------------------------
