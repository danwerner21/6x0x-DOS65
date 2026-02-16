

DFLFCB          = $107          ;DEFAULT FCB
PEM             = $103          ;PEM ENTRY
BOOT            = $100          ;WARM BOOT


; memory map
TEXEND_INIT     = $9000
TEXBUF_INIT     = $A000
BUFEND_INIT     = $B800

INSCOLOR        = 14
TOPFGCOLOR      = 1
DEFAULT_DEVICE  = 1

; PETSCII constants
PETSCII_CLR     = 147
PETSCII_MODE    = $8F
space           = 32



;COLUMNS = $D9
COLUMNS         = $D9
ROWS            = $DA

CURRENT_COLUMN  = $D3
QUOTE_MODE      = $D4
INSERT_MODE     = $D8

;Locations used by high-speed memory
;move routines:

froml           = $26
fromh           = $27
destl           = $28
desth           = $29
llen            = $2A
hlen            = $2B

;curr: Position of cursor within text
;memory. scr: used by the refresh
;routine.

curr            = $39

;tex: An alternate location used in tan-
;dem with curr. COLR is used by RE-
;FRESH. temp is used throughout as a
;reusable scratchpad pointer. INDIR is
;also a reusable indirect pointer.
;UNDERCURS stores the value of the
;character highlighted by the cursor.

tex             = $FB
temp            = $3B
indir           = $FD
UNDERCURS       = $FA

;WINDCOLR: Color of command line
;window supported by refresh.
;RETCHAR is the screen-
;code value of the return mark (a left-
;pointing arrow).

windcolr        = $0C
retchar         = 31



;SCREEN          = $FFED
;PLOT            = $FFF0
;chrout          = $FFD2
;stop            = $FFE1
;setlfs          = $FFBA
;setnam          = $FFBD
;clall           = $FFE7
;open            = $FFC0
;chrin           = $FFCF
;chkin           = $FFC6
;chkout          = $FFC9
;getin           = $FFE4
;clrchn          = $FFCC
;close           = $FFC3
;load            = $FFD5
;save            = $FFD8
;ioinit          = $FF84
;scnkey = $FF9F ; not supported in x16?

; VERA I/O registers
;V_H_INC1        = $10
;V_H             = $9f22
;V_M             = $9f21
;V_L             = $9f20
;V_1             = $9f23
;CTRL            = $9F25
