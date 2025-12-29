
        .INCLUDE "../dos65_os/dosdefn.asm"



PC6502_IO       = $E000
PC6502_IOSPACE  = $EF00
UART1DATA       = PC6502_IOSPACE+$84; SERIAL PORT 1 (I/O Card)
UART1STATUS     = PC6502_IOSPACE+$85; SERIAL PORT 1 (I/O Card)
UART1COMMAND    = PC6502_IOSPACE+$86; SERIAL PORT 1 (I/O Card)
UART1CONTROL    = PC6502_IOSPACE+$87; SERIAL PORT 1 (I/O Card)

PC6502_ACT_TASK = $EFE0
PC6502_MMU_ENA  = $EFE2
;
;
        .SEGMENT "TEA"
        .ORG    $1000
;
;
        LDA     #$00
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 00
        LDA     #$01
        STA     PC6502_MMU_ENA  ; ENABLE MMU --- FEEEEEL THE POOOOWERRRR


;
        jsr     MULTIOINIT

:
        JSR     KBD_GETKEY
        CMP     #27
        BEQ     :+
        CMP     #$FF
        BEQ     :-
        PHA
        JSR     PRINT_BYTE
        PLA
        JSR     DFT_CONSOLE_OUT
        JMP     :-
:
        BRK

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
        TXA
        pha
        TYA
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
        PLA
        TAY
        plA
        TAX
        pla
        JMP p5
p4:
        .BYTE message
p5:
.endmacro



NEWLINE:
        PHA
        TXA
        PHA
        TYA
        PHA
        LDA     #$0D
        JSR     MACRO_OUTCH
        LDA     #$0A
        JSR     MACRO_OUTCH
        PLA
        TAY
        plA
        TAX
        PLA
        RTS

PRTDEC:
        PHA
        TXA
        PHA
        TYA
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
        TAY
        plA
        TAX
        PLA
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
        JSR     MACRO_OUTCH       ;Convert to character and print it
PrDecDigit2:
        PLA
        RTS                     ;Restore A and return


MACRO_OUTCH:
        jmp  DFT_CONSOLE_OUT


;__WRSER1________________________________________________________________________________________________________________________
;
;	WRITE CHARACTER(A) TO UART
;________________________________________________________________________________________________________________________________
;
DFT_CONSOLE_OUT:
WRSER1:
        PHA
WRSER1a:
        LDA     UART1STATUS     ; GET STATUS
        AND     #%00010000      ; IS TX READY
        BEQ     WRSER1a         ; NO, WAIT FOR IT
        PLA
        STA     UART1DATA       ; WRITE DATA
        RTS

;------------------------------------------------------------------------
LFCR:
        LDA     #10
        JSR     DFT_CONSOLE_OUT ; PRINT CHAR IN ACC
        LDA     #13
        JSR     DFT_CONSOLE_OUT ; PRINT CHAR IN ACC
        RTS

;__WRSTR_______________________________________________________
;
; OUTPUT THE STRING POINTED TO BY OUTSTR TO THE SCREEN
;
;______________________________________________________________
WRSTR:
        LDY     #$00            ; LOAD $00 INTO Y
OUTSTRLP:
        LDA     (STRPTR),Y      ; LOAD NEXT CHAR FROM STRING INTO ACC
        CMP     #$00            ; IS NULL?
        BEQ     ENDOUTSTR       ; YES, END PRINT OUT
        JSR     DFT_CONSOLE_OUT ; PRINT CHAR IN ACC
        INC     STRPTR
        BNE     OUTSTRLP
        INC     STRPTR+1
        JMP     OUTSTRLP        ; DO NEXT CHAR
ENDOUTSTR:
        RTS                     ; RETURN

PRINT_BYTE:
        STX     SAVX            ; save X
        JSR     ASCTWO          ; get hex chars for byte in X (lower) and A (upper)
        JSR     DFT_CONSOLE_OUT ; output upper nybble
        TXA                     ; transfer lower to A
        LDX     SAVX            ; restore X
        JMP     DFT_CONSOLE_OUT ; output lower nybble
ASCTWO:
        PHA                     ; save byte
        JSR     ASCII           ; do low nybble
        TAX                     ; save in X
        PLA                     ; restore byte
        LSR     A               ; shift upper nybble down
        LSR     A
        LSR     A
        LSR     A
; convert low nybble in A to hex digit
ASCII:
        AND     #$0F            ; clear upper nibble
        CMP     #$0A            ; if less than A, skip next step
        BCC     ASC1
        ADC     #6              ; skip ascii chars between 9 and A
ASC1:
        ADC     #$30            ; add ascii char 0 to value
        RTS
SAVX:
        .BYTE   00


MESSAGE2:
        .BYTE  "   IO=0x",00

        .INCLUDE "bios_multi.asm"
