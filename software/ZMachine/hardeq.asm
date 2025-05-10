;        PAGE
;        SBTTL   "--- HARDWARE EQUATES: DOS/65 ---"

; ---------
; CONSTANTS
; ---------

XSIZE           = 79            ; X-SIZE OF SCREEN
YSIZE           = 24            ; Y-SIZE OF SCREEN

EOL             = $0D           ; EOL CHAR
SPACE           = $20           ; SPACE CHAR
BACKSP          = $08           ; BACKSPACE

DFLFCB          = $107          ;DEFAULT FCB
PEM             = $103          ;PEM ENTRY
BOOT            = $100          ;WARM BOOT
CCMLNG          = 2048          ;CCM LENGTH

; ---------
; ZERO-PAGE
; ---------


; -----------
; PAGES 2 & 3
; -----------

LBUFF           = $0740         ; 89-BYTE LINE BUFFER
