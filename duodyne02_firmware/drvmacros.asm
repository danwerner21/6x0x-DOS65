;___________________________________________________________________________________________________
;
;	USEFUL MACROS
;__________________________________________________________________________________________________
.macro          PRTDBG      message
.LOCAL p1
.LOCAL p2
.LOCAL p3
.LOCAL p4
.LOCAL p5
  .if     .paramcount <> 1
        .error  "Too few parameters for macro PRTDBG"
        .endif
        .if DEBUG=1
        PHA
        txa
        PHA
        tya
        PHA
        LDX #$00
p1:
        LDA p4,x
        INX
        CMP #'$'
        BEQ p2
        JSR MACRO_OUTCH
        JMP p1
p2:
        LDA #13
        jsr MACRO_OUTCH
        LDA #10
        jsr MACRO_OUTCH
        PLA
        tay
        PLA
        tax
        pla
        JMP p5
p4:
        .BYTE message
p5:
        .endif
.endmacro

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
        JSR MACRO_OUTCH
        JMP p1
p2:
        PLY
        PLX
        PLA
        JMP p5
p4:
        .BYTE message
p5:
.endmacro

.macro          DBGFLAG      character
  .if     .paramcount <> 1
        .error  "Too few parameters for macro DBGFLAG"
        .endif
        .if DEBUG=1
        PHA
        LDA #character
        JSR MACRO_OUTCH
        pla
        .endif
.endmacro


;__PRTHEXBYTE__________________________________________________
; PRINT OUT ACCUMULATOR AS HEX NUMBER
;______________________________________________________________
PRTHEXBYTE:
        PHA
        sta     STACKA
        txa
        PHa
        tya
        PHA
        lda     STACKA
        TAX				; SAVE A REGISTER
        LSR 				; SHIFT HIGH NIBBLE TO LOW NIBBLE
        LSR 				;
        LSR 				;
        LSR 				;
        CLC               		; CLEAR CARRY
        JSR PRINT_DIGIT			; PRINT LOW NIBBLE
        TXA				; RESTORE ACCUMULATOR
        JSR PRINT_DIGIT			; PRINT LOW NIBBLE
        pla
        TAY
        pla
        TAX
        PLA
        RTS

;__PRINT_DIGIT_________________________________________________
;
; PRINT OUT LOW NIBBLE OF ACCUMULATOR IN HEX
;
;______________________________________________________________
PRINT_DIGIT:
               AND #$0F				; STRIP OFF HIGH NIBBLE
               ORA #$30				; ADD $30 TO PRODUCE ASCII
               CMP #$3A               		; IS GREATER THAN 9
               BMI PRINT_DIGIT_OUT		; NO, SKIP ADD
               CLC				; CLEAR CARRY
               ADC #$07				; ADD ON FOR LETTER VALUES
PRINT_DIGIT_OUT:					;
               JMP MACRO_OUTCH              		; PRINT OUT CHAR

NEWLINE:
                pha
                LDA #$0D
                JSR MACRO_OUTCH
                LDA #$0A
                Jsr MACRO_OUTCH
                pla
                rts
SPACE:
                pha
                LDA #' '
                JSR MACRO_OUTCH
                pla
                rts


PRTDEC:
                PHA
                STA     STACKA
                TYA
                phA
                TXA
                PHA
                LDA     STACKA
                PHA
                ldy #00
                LDX #$FF
                SEC
PrDec100:
                INX
                SBC #100
                BCS PrDec100            ;Count how many 100s
                ADC #100
                JSR PrDecDigit          ;Print the 100s
                LDX #$FF
                SEC                     ;Prepare for subtraction
PrDec10:
                INX
                SBC #10
                BCS PrDec10             ;Count how many 10s
                ADC #10
                JSR PrDecDigit          ;Print the 10s
                TAX                     ;Pass 1s into X
                ldy #1
                JSR PrDecDigit          ;Print the 1s
                PLA
                pla
                TAX
                pla
                TAY
                RTS
PrDecDigit:
                PHA
                cpy #$00
                bne PrDecDigit1
                txa
                tay
                cpy #$00
                bne PrDecDigit1
                jmp PrDecDigit2
PrDecDigit1:
                TXA                     ;Save A, pass digit to A
                ORA #'0'
                JSR  MACRO_OUTCH        ;Convert to character and print it
PrDecDigit2:
                PLA
                RTS                     ;Restore A and return


MACRO_OUTCH:
        PHA
        LDA     CONSOLE
        STA     farfunct
        PLA
        JMP     FUNCTION_DISPATCHER
;