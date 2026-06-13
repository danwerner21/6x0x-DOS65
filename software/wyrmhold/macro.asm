;______________________________________________________________________________
;  macro.asm - 16-bit helpers and a FARCALL convenience macro
;  Style follows software/speedscript/macro.asm
;______________________________________________________________________________

; Copy a 16-bit value src -> dst
        .MACRO  COPY16  src, dst
        LDA     src
        STA     dst
        LDA     src+1
        STA     dst+1
        .ENDMACRO

; left = left + right (16-bit)
        .MACRO  ADD16   left,right
        CLC
        LDA     left
        ADC     right
        STA     left
        LDA     left+1
        ADC     right+1
        STA     left+1
        .ENDMACRO

; left = left - right (16-bit)
        .MACRO  SUB16   left,right
        SEC
        LDA     left
        SBC     right
        STA     left
        LDA     left+1
        SBC     right+1
        STA     left+1
        .ENDMACRO

; Set a 16-bit zero-page word to an immediate constant.
        .MACRO  SETW16  dst, value
        LDA     #<(value)
        STA     dst
        LDA     #>(value)
        STA     dst+1
        .ENDMACRO

; Issue a FARCALL: store function number then JSR DO_FARCALL.
; Pass any X/Y/A arguments BEFORE invoking (this macro only touches A).
        .MACRO  FARCALL func
        LDA     #func
        STA     farfunct
        JSR     DO_FARCALL
        .ENDMACRO

; Print a zero-terminated message via the firmware console (FARCALL chrout).
; message = address of a 0-terminated string.
        .MACRO  PRINTMSG message
        LDA     #<message
        LDY     #>message
        JSR     prmsg
        .ENDMACRO

; Push a message into the scrolling message log at the bottom.
        .MACRO  PRINTMSG_MSG message
        LDA     #<message
        LDY     #>message
        JSR     msg_print
        .ENDMACRO

; Set the shop status-line pointer (shopstat) to a message address.
        .MACRO  SETSTAT message
        LDA     #<message
        STA     shopstat
        LDA     #>message
        STA     shopstat+1
        .ENDMACRO
