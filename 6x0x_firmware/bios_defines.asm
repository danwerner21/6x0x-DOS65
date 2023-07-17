;__________________________________________________________________________________________________
;
; CONFIGURATION
;__________________________________________________________________________________________________
;
M6X0X_IOSPACE   = $E000
M6X0X_SHADOW_ROM = $F000

; PAGER
M6X0X_ACT_TASK  = M6X0X_IOSPACE+$A00
M6X0X_MAP_SETUP = M6X0X_IOSPACE+$A10
M6X0X_MAP_SPACE = M6X0X_IOSPACE+$A20
M6X0X_MMU_ENA   = M6X0X_IOSPACE+$A30

;
;
;__________________________________________________________________________________________________
;
; DATA CONSTANTS
;__________________________________________________________________________________________________
;ZERO PAGE	ADDRESS			; FUNCTION

;
; DRIVER WORKING STORAGE
;
FLATCH_STORE    = $02FD         ;
PPIDETIMEOUT    = $02FE         ; (word)
INBUFFER        = $0300         ;
