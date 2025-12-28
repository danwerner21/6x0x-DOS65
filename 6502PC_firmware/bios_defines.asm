;__________________________________________________________________________________________________
;
; CONFIGURATION
;__________________________________________________________________________________________________
;
PC6502BIOS = 1
PC6502_IOSPACE   = $EF00
PC6502_SHADOW_ROM = $F000
FUNCTION_DISPATCHER = $C000

; PAGER
PC6502_ACT_TASK  = PC6502_IOSPACE+$E0
PC6502_MAP_SETUP = PC6502_IOSPACE+$E1
PC6502_MMU_ENA   = PC6502_IOSPACE+$E2
PC6502_MAP_SPACE = PC6502_IOSPACE+$D0


;
;
;__________________________________________________________________________________________________
;
; DATA CONSTANTS
;__________________________________________________________________________________________________
;ZERO PAGE	ADDRESS			; FUNCTION
XTIDETIMEOUT    = $F0

;
; DRIVER WORKING STORAGE
;
FLATCH_STORE    = $02FD         ;
PPIDETIMEOUT    = $02FE         ; (word)
INBUFFER        = $0300         ;
