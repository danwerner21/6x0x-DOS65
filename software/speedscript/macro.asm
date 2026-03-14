; some macros
        .macro COPY16  src, dst
        LDA     src
        STA     dst
        LDA     src+1
        STA     dst+1
        .endmacro

        .macro COPY162  src, dst, dst2
        LDA     src
        STA     dst
        STA     dst2
        LDA     src+1
        STA     dst+1
        STA     dst2+1
        .endmacro


        .macro ADD16   left,right
        CLC
        LDA     left
        ADC     right
        STA     left
        LDA     left+1
        ADC     right+1
        STA     left+1
        .endmacro

;Compares two 16-bit values, and sets the C and Z flags the
;same as a LDA and CMP;instruction would for 8-bit addresses.
        .macro COMP16  left,right
        SEC
        LDA     left
        SBC     right
        STA     temp
        LDA     left+1
        SBC     right+1
        ORA     temp
        .endmacro

; Compares two 16-bit values but only affects the C flag.
        .macro COMPC16 left,right
        SEC
        LDA     left
        SBC     right
        LDA     left+1
        SBC     right+1
        .endmacro

;  sub16 left,right       :  left - right -> left
;  sub16 left,right,dst   : left - right -> dst
        .macro SUB16   left,right
        SEC
        LDA     left
        SBC     right
        STA     left
        LDA     left+1
        SBC     right+1
        STA     left+1
        .endmacro

        .macro SUB162   left,right,dst
        SEC
        LDA     left
        SBC     right
        STA     dst
        LDA     left+1
        SBC     right+1
        STA     dst+1
        .endmacro

; Subtract an 8-bit value from a 16-bit one
        .macro  SUB8    left,right
        SEC
        LDA     left
        SBC     right
        STA     left
        LDA     left+1
        SBC     #0
        STA     left+1
        .endmacro


        .macro  SUB82    left,right,dst
        SEC
        LDA     left
        SBC     right
        STA     dst
        LDA     left+1
        SBC     #0
        STA     dst+1
        .endmacro

        .macro  SUB83    left,right,dst,dst2
        SEC
        LDA     left
        SBC     right
        STA     dst
        STA     dst2
        LDA     left+1
        SBC     #0
        STA     dst+1
        STA     dst2+1
        .endmacro

        .macro PRINTMESSAGE message
        LDA     #<message
        LDY     #>message
        JSR     PRMSG
        .endmacro

        .macro  TOPPRINTMESSAGE message
        JSR     topclr
        PRINTMESSAGE message
        .endmacro

;Display a 16-bit integer from the A and X registers
;in decimal using CHROUT.
        .macro         DISPLAY_NUMBER
        JSR     displaynum
        .endmacro

;TODO: DOS/65 has no modifier key state register.
; For now, always branch (shift/ctrl combos disabled).
        .macro   B_UNLESS_SHIFT_CTRL addr
        JMP     addr
        .endmacro

        .macro        B_UNLESS_SHIFT addr
        JMP     addr
        .endmacro

        .MACRO  B_IF_PUNCT dst
        CMP     #'.'
        BEQ     dst
        CMP     #'!'
        BEQ     dst
        CMP     #'?'
        BEQ     dst
        .ENDMACRO

        .macro        PRCODE  dst
        JSR     aschex
        STA     dst
        .endmacro
        .macro  PRCODE16 dst
        PRCODE  dst
        LDA     hex+1
        STA     dst+1
        .endmacro

        .macro  RVS_TEXT text
        .BYTE   $12
        .BYTE   text
        .BYTE   $92
        .endmacro
