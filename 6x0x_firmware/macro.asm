;__MACRO___________________________________________________________________________________________________________________
;
; 	Macros for the betterment of Mankind
;________________________________________________________________________________________________________________________________
;

.macro          PRTS      message
.LOCAL p1
.LOCAL p2
.LOCAL p3
.LOCAL p4
.LOCAL p5
  .if     .paramcount <> 1
        .error  "Too few parameters for macro PRTS"
        .endif
        PHA
        PHX
        PHY
        LDX #$00
p1:
        LDA p4,x
        INX
        CMP #'$'
        BEQ p2
        JSR IOF_OUTCH
        JMP p1
p2:
        PLY
        plx
        pla
        JMP p5
p4:
        .BYTE message
p5:
.endmacro



NEWLINE:
        PHA
        PHX
        PHY
        LDA     #$0D
        JSR     IOF_OUTCH
        LDA     #$0A
        JSR     IOF_OUTCH
        PLY
        PLX
        PLA
        RTS

PRTDEC:
        PHY
        PHX
        PHA
        LDY     #00
        LDX     #$FF
        SEC
PrDec100:
        INX
        SBC     #100
        BCS     PrDec100        ;Count how many 100s
        ADC     #100
        JSR     PrDecDigit      ;Print the 100s
        LDX     #$FF
        SEC                     ;Prepare for subtraction
PrDec10:
        INX
        SBC     #10
        BCS     PrDec10         ;Count how many 10s
        ADC     #10
        JSR     PrDecDigit      ;Print the 10s
        TAX                     ;Pass 1s into X
        LDY     #1
        JSR     PrDecDigit      ;Print the 1s
        PLA
        PLX
        PLY
        RTS
PrDecDigit:
        PHA
        CPY     #$00
        BNE     PrDecDigit1
        TXA
        TAY
        CPY     #$00
        BNE     PrDecDigit1
        JMP     PrDecDigit2
PrDecDigit1:
        TXA                     ;Save A, pass digit to A
        ORA     #'0'
        JSR     IOF_OUTCH       ;Convert to character and print it
PrDecDigit2:
        PLA
        RTS                     ;Restore A and return
