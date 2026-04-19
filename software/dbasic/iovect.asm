; system dependant i/o vectors
; these are in RAM and are set by the monitor at start-up

printflag:
        .BYTE   00

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
        LDA     printflag
        CMP     #00
        BNE     :+
        LDA     DBGA
        LDX     #2              ;
        JSR     PEM             ;
        LDA     DBGA
        LDY     DBGY
        LDX     DBGX
        RTS
:
        LDA     DBGA
        LDX     #5              ;
        JSR     PEM             ;
        LDA     DBGA
        LDY     DBGY
        LDX     DBGX
        RTS

        .ELSE

V_OUTP: ; send byte to memory mapped video device
BYTEOUT:
        STA     DBGA
        STY     DBGY
        STX     DBGX
        LDA     printflag
        CMP     #00
        BNE     :+
        LDA     #19
        STA     farfunct
        LDA     DBGA
        JSR     DO_FARCALL
        LDA     DBGA
        LDY     DBGY
        LDX     DBGX
        RTS
:
        LDA     DBGA
        LDX     #5              ;
        JSR     PEM             ;
        LDA     DBGA
        LDY     DBGY
        LDX     DBGX
        RTS

V_INPT: ; non halting keyboard scan
BYTEIN:
        STY     DBGY            ;
        STX     DBGX            ;
        LDA     #20
        STA     farfunct
        JSR     DO_FARCALL
        CMP     #$FF            ;
        BEQ     LAB_nobyw       ; branch if no byte waiting
        LDY     DBGY
        LDX     DBGX
        SEC                     ; flag byte received
        RTS
LAB_nobyw:
        LDA     #$00
        LDY     DBGY
        LDX     DBGX
        CLC                     ; flag no byte received
        RTS                     ;

;___ScreenEditor____________________________________________
;
; Basic Screen editor code
;
;__________________________________________________________
ScreenEditor:
; allow prepopulate of screen
ploop:
        LDA     #58
        STA     farfunct
        JSR     DO_FARCALL      ; PAINT cursor
ploop1:
        LDA     #20
        STA     farfunct
        JSR     DO_FARCALL      ; GET KEYSTROKE
        CMP     #$FF
        BEQ     ploop1
        PHA
        LDA     #59
        STA     farfunct
        JSR     DO_FARCALL      ; UNPAINT cursor
        PLA
        CMP     #$F0
        BEQ     insert
        CMP     #$F1
        BEQ     delete
        CMP     #$F6
        BEQ     crsrup
        CMP     #$F7
        BEQ     crsrdn
        CMP     #$F8
        BEQ     crsrlt
        CMP     #$F9
        BEQ     Lcrsrrt
        CMP     #$0A
        BEQ     ploop
        CMP     #13
        BEQ     Lpexit

        PHA
        LDA     #19
        STA     farfunct
        PLA
        JSR     DO_FARCALL
        JMP     ploop
Lpexit:
        JMP     pexit
Lcrsrrt:
        JMP     crsrrt
insert:
        JMP     DOINSERT
delete:
        JMP     DODELETE
crsrup:
        LDA     CURY
        CMP     #00
        BEQ     ploop
        DEC     CURY
        JMP     ploop
crsrdn:
        LDA     CURY
        CMP     #23
        BEQ     crsrdn_1
        INC     CURY
        JMP     ploop
crsrdn_1:
        LDA     #56
        STA     farfunct
        JSR     DO_FARCALL
        JMP     ploop
crsrlt:
        LDA     CURX
        CMP     #00
        BEQ     crsrlt_1
        DEC     CURX
        JMP     ploop
crsrlt_1:
        LDA     CURY
        CMP     #00
        BEQ     :+
        LDX     VIDEOWIDTH
        DEX
        STX     CURX
        LDY     CURY
        DEY
        STY     CURY
:
        JMP     ploop
crsrrt:
        LDX     VIDEOWIDTH
        DEX
        CPX     CURX
        BEQ     crsrrt_1
        INC     CURX
        JMP     ploop
crsrrt_1:
        LDA     #00
        STA     CURX
        JMP     crsrdn
pexit:
        JSR     LdKbBuffer
        LDX     #80
        LDA     #$00
        STA     Ibuffs,X
TERMLOOP:
        DEX
        LDA     Ibuffs,X
        CMP     #32
        BEQ     TERMLOOP_B
        CMP     #00
        BEQ     TERMLOOP_C
        JMP     TERMLOOP_A
TERMLOOP_B:
        LDA     #00
        STA     Ibuffs,X
TERMLOOP_C:
        CPX     #00
        BNE     TERMLOOP
TERMLOOP_A:
        LDA     #19
        STA     farfunct
        LDA     #13
        JSR     DO_FARCALL
        LDA     #19
        STA     farfunct
        LDA     #10
        JSR     DO_FARCALL
        RTS
DOINSERT:
        JSR     GETVIDEOADDRESS
        LDY     #$01+VIDEOBANK  ; SCREEN AREA $1000-$1FFF
        JSR     PAGE_ENTER
        LDY     VIDEOWIDTH
        DEY
:
        DEY
        LDA     (TEMPW),Y
        INY
        STA     (TEMPW),Y
        DEY
        CPY     CURX
        BNE     :-
        LDA     #32
        STA     (TEMPW),Y

        JSR     PAGE_EXIT
        JMP     ploop
DODELETE:
        JSR     GETVIDEOADDRESS
        LDY     #$01+VIDEOBANK  ; SCREEN AREA $1000-$1FFF
        JSR     PAGE_ENTER
        LDY     CURX
:
        INY
        LDA     (TEMPW),Y
        DEY
        STA     (TEMPW),Y
        INY
        CPY     VIDEOWIDTH
        BNE     :-
        LDY     VIDEOWIDTH
        DEY
        LDA     #32
        STA     (TEMPW),Y

        JSR     PAGE_EXIT
        JMP     ploop
LdKbBuffer:
; clear input buffer
        LDX     #81
:
        LDA     #00
        STA     Ibuffs-1,X
        DEX
        BNE     :-

        JSR     GETVIDEOADDRESS
        LDY     #$01+VIDEOBANK  ; SCREEN AREA $1000-$1FFF
        JSR     PAGE_ENTER
        LDY     #0
:
        LDA     (TEMPW),Y
        STA     Ibuffs,Y
        INY
        CPY     VIDEOWIDTH
        BNE     :-

        JSR     PAGE_EXIT
        RTS

GETVIDEOADDRESS:
        LDY     CURY

        STY     PTEMPW
        LDA     #0
        STA     PTEMPW+1
        CLC
        ASL     PTEMPW
        ROL     PTEMPW+1        ; *2
        ASL     PTEMPW
        ROL     PTEMPW+1        ; *4
        ASL     PTEMPW
        ROL     PTEMPW+1        ; *8
        LDA     PTEMPW
        STA     PTEMPW1
        LDA     PTEMPW+1
        STA     PTEMPW1+1       ; PTEMPW1=Y*8
        ASL     PTEMPW
        ROL     PTEMPW+1        ; *16
        ASL     PTEMPW
        ROL     PTEMPW+1        ; *32
                                ; TEMPW  = Y*8 + Y*32  (Y*40)
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PTEMPW          ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW1         ; Add the low byte of the second number (plus carry)
        STA     TEMPW           ; Store the low byte of the result
        LDA     PTEMPW+1        ; Load the high byte of the first number
        ADC     PTEMPW1+1       ; Add the high byte of the second number (plus carry from previous op)
        STA     TEMPW+1         ; Store the high byte of the result

        LDA     VIDEOWIDTH      ; If 80 col, double again
        CMP     #80
        BNE     :+
        ASL     TEMPW
        ROL     TEMPW+1         ; *2  (80)
:
        LDA     TEMPW+1
        ORA     #$A0            ; add in the bank#
        STA     TEMPW+1
        RTS


        .ENDIF

V_LPRINT:
        PHP
        LDA     #$01
        STA     printflag
        PLP
        JSR     LAB_PRINT
        LDA     #$00
        STA     printflag
        RTS
V_LLIST:
        PHP
        LDA     #$01
        STA     printflag
        PLP
        JSR     LAB_LIST
        LDA     #$00
        STA     printflag
        RTS


UART1DATA       = $EF84         ; SERIAL PORT 1 (I/O Card)
UART1STATUS     = $EF85         ; SERIAL PORT 1 (I/O Card)
UART1COMMAND    = $EF86         ; SERIAL PORT 1 (I/O Card)
UART1CONTROL    = $EF87         ; SERIAL PORT 1 (I/O Card)

PRINT_CRLF:
        STX     TTX
        STY     TTY
        STA     TTA
        LDA     #13
        JSR     WRSER1          ; output upper nybble
        LDA     #10
        JSR     WRSER1          ; output upper nybble
        LDX     TTX
        LDY     TTY
        LDA     TTA
        RTS

PRINT_SPACE:
        STX     TTX
        STY     TTY
        STA     TTA
        LDA     #32
        JSR     WRSER1          ; output upper nybble
        LDX     TTX
        LDY     TTY
        LDA     TTA
        RTS


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
        .INCLUDE "ay38910.asm"
        .INCLUDE "rtc.asm"
