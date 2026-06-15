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
CO_BLUE         = $1
CO_GREEN        = $2
CO_TURQ         = $3
CO_RED          = $4
CO_VIOLET       = $5
CO_BROWN        = $6
CO_CYAN         = $7
CO_GREY         = $8
CO_BRBLUE       = $9
CO_BRGREEN      = $A
CO_BRTURQ       = $B
CO_BRRED        = $C
CO_BRPURPLE     = $D
CO_BRYELLOW     = $E
CO_BRWHITE      = $F

; convenience: build a color byte from fg,bg nibbles
        .DEFINE COLOR(fg,bg) (((bg)<<4)|(fg))

; named cell colors used throughout the game
; Terrain uses colored backgrounds so adjoining cells read as landscape,
; while the brighter foreground pixels provide texture and landmarks.
C_GRASS         = COLOR(CO_BRGREEN, CO_GREEN)
C_WATER         = COLOR(CO_BRTURQ,  CO_BLUE)
C_FOREST        = COLOR(CO_BRGREEN, CO_BLACK)
C_MOUNT         = COLOR(CO_BRWHITE, CO_GREY)
C_TOWN          = COLOR(CO_BRYELLOW,CO_RED)
C_CASTLE        = COLOR(CO_BRWHITE, CO_GREY)
C_DUNG          = COLOR(CO_BRTURQ,  CO_BLACK)
C_ROAD          = COLOR(CO_BRYELLOW,CO_RED)
C_BRIDGE        = COLOR(CO_BRYELLOW,CO_BLUE)
C_FLOOR         = COLOR(CO_GREY,    CO_BLACK)
C_WALL          = COLOR(CO_BRWHITE, CO_GREY)
C_HILLS         = COLOR(CO_BRYELLOW,CO_GREEN)
C_MARSH         = COLOR(CO_BRGREEN, CO_BLUE)
C_TREAS         = COLOR(CO_BRYELLOW,CO_BLACK)
C_PLAYER        = COLOR(CO_BRWHITE, CO_BLACK)
C_MONST         = COLOR(CO_BRRED,   CO_BLACK)
C_BOSS          = COLOR(CO_BRRED,   CO_RED)
C_BREATH        = COLOR(CO_BRYELLOW,CO_RED)
C_BORDER        = COLOR(CO_BRTURQ,  CO_BLACK)
C_PANEL         = COLOR(CO_BRWHITE, CO_BLACK)
C_PANELHDR      = COLOR(CO_BRYELLOW,CO_BLUE)
C_MSG           = COLOR(CO_BRWHITE, CO_BLACK)
C_TITLE         = COLOR(CO_BRYELLOW,CO_BLACK)
C_BLANK         = COLOR(CO_GREY,    CO_BLACK)

; shop panel colors (blue panel with bright text)
C_SHOPBG        = COLOR(CO_BRWHITE, CO_BLUE)
C_SHOPBRD       = COLOR(CO_BRTURQ,  CO_BLUE)
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
T_HILLS         = 15            ; passable rolling foothills
T_MARSH         = 16            ; passable wetland
NUM_TILES       = 17

;----------------------------------------------------------------
; Small custom glyphs used by the title scene. Gameplay uses the
; 2x2 metatile glyphs in the upper half of the character set below.
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

G_HILLS         = 23
G_MARSH         = 24

;----------------------------------------------------------------
; Gameplay metatile glyphs. Each world tile occupies four adjacent
; character cells:
;   base+0 base+1
;   base+2 base+3
;
; Terrain bases are computed as MG_TERRAIN_BASE + tile_code*4.
; $80..$DF stays clear of printable ASCII used by the UI.
; The DBASIC PATTERN implementation accepts character numbers 0..255.
;----------------------------------------------------------------
MG_TERRAIN_BASE = $80

MG_GRASS        = MG_TERRAIN_BASE+(T_GRASS*4)
MG_FOREST       = MG_TERRAIN_BASE+(T_FOREST*4)
MG_MOUNT        = MG_TERRAIN_BASE+(T_MOUNT*4)
MG_WATER        = MG_TERRAIN_BASE+(T_WATER*4)
MG_TOWN         = MG_TERRAIN_BASE+(T_TOWN*4)
MG_DUNG         = MG_TERRAIN_BASE+(T_DUNG*4)
MG_CASTLE       = MG_TERRAIN_BASE+(T_CASTLE*4)
MG_ROAD         = MG_TERRAIN_BASE+(T_ROAD*4)
MG_BRIDGE       = MG_TERRAIN_BASE+(T_BRIDGE*4)
MG_FLOOR        = MG_TERRAIN_BASE+(T_FLOOR*4)
MG_WALL         = MG_TERRAIN_BASE+(T_WALL*4)
MG_DOOR         = MG_TERRAIN_BASE+(T_DOOR*4)
MG_TREAS        = MG_TERRAIN_BASE+(T_TREAS*4)
MG_UPSTAIR      = MG_TERRAIN_BASE+(T_UPSTAIR*4)
MG_SHOP         = MG_TERRAIN_BASE+(T_SHOP*4)
MG_HILLS        = MG_TERRAIN_BASE+(T_HILLS*4)
MG_MARSH        = MG_TERRAIN_BASE+(T_MARSH*4)

MG_PLAYER       = $C4
MG_ORC          = $C8
MG_SNAKE        = $CC
MG_SKELETON     = $D0
MG_THIEF        = $D4
MG_TROLL        = $D8
MG_BOSS         = $DC

;----------------------------------------------------------------
; Terrain variant metatiles ($E0..$EF). A few high-traffic field
; terrains get a 2nd art variant so large regions don't show an
; obvious grid. render_view picks variant 0 (the MG_* base above)
; or variant 1 (these) from a position hash. Tiles without a
; variant entry here are left to fall through to their base.
;   NOTE: depends on the video card redefining glyphs >= $80; the
;   $E0+ range is verified on hardware at build time.
;----------------------------------------------------------------
MGV_GRASS       = $E0
MGV_FOREST      = $E4
MGV_WATER       = $E8
MGV_MOUNT       = $EC
; $F0..$F3 is the Wyrm Warden, $F4..$F7 is dragon breath,
; and $F8..$FF remains free.

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
M_WARDEN        = 7
NUM_MTYPE       = 8
MAXMON          = 12            ; max simultaneously active monsters

; dedicated miniboss artwork in the remaining high glyph range
MG_WARDEN       = $F0
MG_BREATH       = $F4

; dragon breath directions
BREATH_NONE     = 0
BREATH_UP       = 1
BREATH_DOWN     = 2
BREATH_LEFT     = 3
BREATH_RIGHT    = 4

;----------------------------------------------------------------
; Viewport geometry (left map window). Gameplay world tiles are
; rendered as 2x2 character-cell metatiles.
;   frame at col 0..VPW+1, rows 0..VPH+1  (top bar handled separately)
;----------------------------------------------------------------
VPW             = 44            ; viewport interior width in screen cells
VPH             = 20            ; viewport interior height (rows 1..20)
VPX0            = 1             ; first interior column (screen)
VPY0            = 1             ; first interior row (screen)
VPTW            = VPW/2         ; viewport width in world tiles
VPTH            = VPH/2         ; viewport height in world tiles
VPCX            = 11            ; player world-tile column within viewport
VPCY            = 5             ; player world-tile row within viewport

; panel geometry (right side)
PANX            = 48            ; panel text start column
PANW            = 31

; message log geometry (bottom two rows)
MSGY0           = 22
MSGROWS         = 2

; map dimensions
OWW             = 64            ; overworld width
OWH             = 64            ; overworld height
TOWNW           = 32            ; town map width
TOWNH           = 20            ; town map height
DUNGW           = 32            ; dungeon map width
DUNGH           = 20            ; dungeon map height
CASTLEW         = 32            ; castle audience chamber width
CASTLEH         = 20            ; castle audience chamber height
SHRINEW         = 32            ; Sunken Shrine width
SHRINEH         = 20            ; Sunken Shrine height

; world/location ids (which map is active)
LOC_WORLD       = 0
LOC_TOWN        = 1
LOC_DUNG        = 2
LOC_CASTLE      = 3
LOC_SHRINE      = 4

; opening quest progression
QUEST_NONE      = 0             ; seek the castle and speak with the ruler
QUEST_FIND_KEY  = 1             ; recover the lost Wyrm Key
QUEST_HAVE_KEY  = 2             ; return to the ruler with the key
QUEST_DUNG_OPEN = 3             ; dragon's ward has been broken
QUEST_DRAGON_DEAD = 4           ; return to Aldren after defeating the dragon
QUEST_COMPLETE  = 5             ; Aldren has acknowledged the victory

; fixed audience-chamber landmark used for interaction and rendering
CASTLE_RULER_X  = 15
CASTLE_RULER_Y  = 4

; overworld and interior positions for the Wyrm Key quest
KEY_SITE_X      = 20
KEY_SITE_Y      = 58
WARDEN_X        = 27
WARDEN_Y        = 17
DRAGON_X        = 27
DRAGON_Y        = 18

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

;----------------------------------------------------------------
; Game state (in TEA RAM, defined as labels in wyrmhold.asm BSS area)
; declared here as equates is not possible; real storage is in the
; .SEGMENT "TEA" at the end of wyrmhold.asm.  See gamevars there.
;----------------------------------------------------------------
