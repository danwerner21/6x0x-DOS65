        .IFDEF  DUODYNE
STACK_BOTTOM    = $4000         ; stack bottom, no offset
STACK           = $5FFF         ; stack top, no offset
Ram_top         = $D000         ; end of user RAM+1 (set as needed, should be page aligned)
        .ELSE
Ram_top         = $B800         ; end of user RAM+1 (set as needed, should be page aligned)
        .ENDIF

SETPAGE         = $FFF6
DO_FARCALL      = $FFF0
farfunct        = $32           ; function to call in driver area

; offsets from a base of X or Y
PLUS_0          = $00           ; X or Y plus 0
PLUS_1          = $01           ; X or Y plus 1
PLUS_2          = $02           ; X or Y plus 2
PLUS_3          = $03           ; X or Y plus 3

LAB_STAK        = $0100         ; stack bottom, no offset

LAB_SKFE        = LAB_STAK+$FE
; flushed stack address
LAB_SKFF        = LAB_STAK+$FF
; flushed stack address

ccflag          = $0600         ; BASIC CTRL-C flag, 00 = enabled, 01 = dis
ccbyte          = ccflag+1      ; BASIC CTRL-C byte
ccnull          = ccbyte+1      ; BASIC CTRL-C byte timeout

VEC_CC          = ccnull+1      ; ctrl c check vector

; Ibuffs can now be anywhere in RAM AS LONG AS IT IS BEFORE RAM_BASE AND IS NOT PAGE ALIGNED!, ensure that the max length is < $80
Ibuffs          = (ENDOFBASIC&$FF00)+$181
Ibuffe          = Ibuffs+81     ; end of input buffer
Ram_base        = (Ibuffe&$FF00)+$100; start of user RAM (set as needed, should be page aligned)
