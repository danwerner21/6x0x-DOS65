; system dependant i/o vectors
; these are in RAM and are set by the monitor at start-up

        .IFNDEF MEMORYMAPPEDSCREEN
; Serial port IO
V_INPT: ; non halting scan input device
BYTEIN:
        STY     DBGY            ;
        STX     DBGX            ;
        LDX     #11             ;
        JSR     PEM             ;
        CMP     #$00            ;
        BEQ     LAB_nobyw       ; branch if no byte waiting
        LDX     #6              ;
        JSR     PEM             ;
        LDY     DBGY
        LDX     DBGX
        SEC                     ; flag byte received
        RTS
LAB_nobyw:
        LDY     DBGY
        LDX     DBGX
        CLC                     ; flag no byte received
        RTS                     ;

V_OUTP: ; send byte to output device
BYTEOUT:
        STA     DBGA
        STY     DBGY
        STX     DBGX
        LDX     #2              ;
        JSR     PEM             ;
        LDA     DBGA
        LDY     DBGY
        LDX     DBGX
        RTS

        .ELSE

; Serial port IO
V_INPT: ; non halting scan input device
BYTEIN:
        STY     DBGY            ;
        STX     DBGX            ;
        LDX     #11             ;
        JSR     PEM             ;
        CMP     #$00            ;
        BEQ     LAB_nobyw       ; branch if no byte waiting
        LDX     #6              ;
        JSR     PEM             ;
        LDY     DBGY
        LDX     DBGX
        SEC                     ; flag byte received
        RTS
LAB_nobyw:
        LDY     DBGY
        LDX     DBGX
        CLC                     ; flag no byte received
        RTS                     ;

V_OUTP: ; send byte to output device
BYTEOUT:
        STA     DBGA
        STY     DBGY
        STX     DBGX
        LDX     #2              ;
        JSR     PEM             ;
        LDA     DBGA
        LDY     DBGY
        LDX     DBGX
        RTS



        .ENDIF




UART1DATA       = $EF84         ; SERIAL PORT 1 (I/O Card)
UART1STATUS     = $EF85         ; SERIAL PORT 1 (I/O Card)
UART1COMMAND    = $EF86         ; SERIAL PORT 1 (I/O Card)
UART1CONTROL    = $EF87         ; SERIAL PORT 1 (I/O Card)

PRINT_BYTE:
        STX     TTX
        STY     TTY
        STA     TTA
        STX     SAVX            ; save X
        JSR     ASCTWO          ; get hex chars for byte in X (lower) and A (upper)
        JSR     WRSER1          ; output upper nybble
        TXA                     ; transfer lower to A
        LDX     SAVX            ; restore X
        JSR     WRSER1          ; output lower nybble
        LDX     TTX
        LDY     TTY
        LDA     TTA
        RTS
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
TTX:
        .BYTE   00
TTY:
        .BYTE   00
TTA:
        .BYTE   00
;__WRSER1________________________________________________________________________________________________________________________
;
;	WRITE CHARACTER(A) TO UART
;________________________________________________________________________________________________________________________________
;
WRSER1:
        PHA
WRSER1a:
        LDA     UART1STATUS     ; GET STATUS
        AND     #%00010000      ; IS TX READY
        BEQ     WRSER1a         ; NO, WAIT FOR IT
        PLA
        STA     UART1DATA       ; WRITE DATA
        RTS


        .INCLUDE "diskcmds.asm"
        .INCLUDE "screencmds.asm"
        .INCLUDE "sid.asm"
        .INCLUDE "rtc.asm"
