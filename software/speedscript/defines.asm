
; DOS/65 system entry points
DFLFCB          = $107          ;DEFAULT FCB
PEM             = $103          ;PEM ENTRY
BOOT            = $100          ;WARM BOOT

CURX            = $0647         ; CURRENT CURSOR POSITION (MEMORY MAPPED)
CURY            = $0648
SHOWCRSR        = $0649         ; SHOW CURSOR (1-YES, 0-NO) (MEMORY MAPPED)
CURCOLOR        = $064A         ; CURRENT PRINT COLOR (MEMORY MAPPED)
CSRCOLOR        = $064B         ; CURRENT CURSOR COLOR (MEMORY MAPPED)
BVIDEOMODE      = $064C         ; CURRENT VIDEO MODE (MEMORY MAPPED) 00=40 COL, 01=80 COL


; DO_FARCALL dispatch vector
DO_FARCALL      = $FFF0
farfunct        = $32           ;zero-page: FARCALL function number

; MMU / video paging (6502PC hardware)
SETPAGE         = $FFF6         ;configure MMU mapping (A=task, X=bank, Y=page)
VIDEOBANK       = $F8           ;base physical page for video RAM
PC6502_ACT_TASK = $EFE0        ;MMU active task register (write-only)
VIDTEXT_PAGE    = $01+VIDEOBANK ;physical page for text page 1 chars ($F9)

; FARCALL function numbers
FC_CHROUT       = 19            ;output character (A=char)
FC_CHRIN        = 20            ;read keyboard (returns A=key or $FF)
FC_LOCATE       = 37            ;set cursor position (X=col, Y=row)
FC_SCNCLR       = 38            ;clear screen
FC_SETMODE      = 57            ;set screen mode
FC_COLOR        = 39            ;set color (X=fg|bg, Y=cursor_fg|bg)
FC_SCROLLDN     = 56            ;scroll screen down
FC_PAINTCSR     = 58            ;paint cursor at current position
FC_UNPAINTCSR   = 59            ;unpaint/hide cursor

; memory map
TEXEND_INIT     = $9000
TEXBUF_INIT     = $A000
BUFEND_INIT     = $B800

INSCOLOR        = $7C
TOPCOLOR        = $7B
CRCOLOR         = $E1
SCNCOLOR        = $1E

DEFAULT_DEVICE  = 1

; constants
space           = 32
BLINK_DELAY     = $60           ;cursor blink speed (higher=slower)

; Screen state variables - relocated from C64 KERNAL addresses
; to safe zero-page locations
COLUMNS         = $50
ROWS            = $51
CURRENT_COLUMN  = $52
QUOTE_MODE      = $53
INSERT_MODE     = $54

;Locations used by high-speed memory
;move routines:

froml           = $55
fromh           = $56
destl           = $57
desth           = $58
llen            = $59
hlen            = $5A

;curr: Position of cursor within text
;memory. scr: used by the refresh
;routine.

curr            = $61

;tex: An alternate location used in tan-
;dem with curr. COLR is used by RE-
;FRESH. temp is used throughout as a
;reusable scratchpad pointer. INDIR is
;also a reusable indirect pointer.
;UNDERCURS stores the value of the
;character highlighted by the cursor.

tex             = $FB
temp            = $5E
indir           = $FD
UNDERCURS       = $FA

;RETCHAR is the screen-
;code value of the return mark (a left-
;pointing arrow).

retchar         = 31

; refresh routine state
scrrow          = $5B           ; current screen row during refresh
csrcol          = $5C           ; cursor column found during refresh
csrrow          = $5D           ; cursor row found during refresh
