
;__MONITOR_______________________________________________________
; This is the SUPERMON based rom monitor program.
;
; It assumes that the 65C02 board is set for IOPage $DF.
;
;__________________________________________________________________________________________________
;
; DATA CONSTANTS
;__________________________________________________________________________________________________
;REGISTER       IO PORT                         ; FUNCTION
; UART 16C550 SERIAL
UART0           = $0358         ; DATA IN/OUT
UART1           = $0359         ; CHECK RX
UART2           = $035A         ; INTERRUPTS
UART3           = $035B         ; LINE CONTROL
UART4           = $035C         ; MODEM CONTROL
UART5           = $035D         ; LINE STATUS
UART6           = $035E         ; MODEM STATUS
UART7           = $035F         ; SCRATCH REG.

BANK00          = $0350
BANK40          = $0351
BANK80          = $0352
BANKC0          = $0353
CONSOLE         = $060F

STRPTR          = $10


        .SEGMENT "ROM"

;__COLD_START___________________________________________________
;
; PERFORM SYSTEM COLD INIT
;
;_______________________________________________________________
COLD_START:
        SEI                     ; DISABLE INTERRUPTS
        CLD                     ;  VERIFY DECIMAL MODE IS OFF
        LDX     #$FF            ;
        TXS                     ; CLEAR STACK

;* Setup Memory Banks (RAM 0000-C000, ROM C000-FFFF)
        LDA     #$80
        STA     BANK00
        LDA     #$81
        STA     BANK40
        LDA     #$02
        STA     BANK80
        LDA     #$03
        STA     BANKC0
        LDA     #$0B            ; ACTIVATE MAPPER
        STA     UART4           ;

        JSR     INIT_SERIAL

        LDA     #<STARTUP       ; OUTPUT STARTUP STRING
        STA     STRPTR          ;
        LDA     #>STARTUP       ;
        STA     STRPTR+1        ;
        JSR     OUTSTR          ;
;
        BRK                     ; PERFORM BRK (START MONITOR)

;__________________________________________________________________________________________________________
INIT_SERIAL:
        LDA     #$80            ;
        STA     UART3           ; SET DLAB FLAG
        LDA     #12             ; SET TO 12 = 9600 BAUD
        STA     UART0           ; save baud rate
        LDA     #00             ;
        STA     UART1           ;
        LDA     #03             ;
        STA     UART3           ; SET 8 BIT DATA, 1 STOPBIT
        LDA     #$0B            ; ACTIVATE MAPPER
        STA     UART4           ;
        RTS

;__OUTCH_________________________________________________________________________________________________
;
; PERFORM CONSOLE WRITE
;________________________________________________________________________________________________________
OUTCH:
        PHA
TX_BUSYLP:
        LDA     UART5           ; READ LINE STATUS REGISTER
        AND     #$20            ; TEST IF UART IS READY TO SEND (BIT 5)
        CMP     #$00
        BEQ     TX_BUSYLP       ; IF NOT REPEAT
        PLA
        STA     UART0           ; THEN WRITE THE CHAR TO UART
        RTS

;__IOF_CONIN_____________________________________________________________________________________________
;
; PERFORM CONSOLE READ
;________________________________________________________________________________________________________
IOF_CONIN:
        LDA     UART5           ; READ LINE STATUS REGISTER
        AND     #$01            ; TEST IF DATA IN RECEIVE BUFFER
        CMP     #$00
        BEQ     :+
        LDA     UART0           ; THEN WRITE THE CHAR TO UART
        RTS
:
        LDA     #$00
        RTS

IOF_CONINW:
        LDA     UART5           ; READ LINE STATUS REGISTER
        AND     #$01            ; TEST IF DATA IN RECEIVE BUFFER
        CMP     #$00
        BEQ     IOF_CONINW
        LDA     UART0           ; THEN WRITE THE CHAR TO UART
        RTS


;__OUTSTR______________________________________________________
;
; OUTPUT THE STRING POINTED TO BY OUTSTR TO THE SCREEN
;
;______________________________________________________________
OUTSTR:
        LDY     #$00            ; LOAD $00 INTO Y
OUTSTRLP:
        LDA     (STRPTR),Y      ; LOAD NEXT CHAR FROM STRING INTO ACC
        CMP     #$00            ; IS NULL?
        BEQ     ENDOUTSTR       ; YES, END PRINT OUT
        JSR     OUTCH           ; PRINT CHAR IN ACC
        INC     STRPTR
        BNE     OUTSTRLP
        INC     STRPTR+1
        JMP     OUTSTRLP        ; DO NEXT CHAR
ENDOUTSTR:
        RTS                     ; RETURN

        .INCLUDE"SUPERMON.ASM"

; START BANNER
STARTUP:
        .BYTE   $0D,$0A

        .BYTE   "  Duodyne",$0D,$0A
        .BYTE   "   __ _____  _____ ___ ___ ",$0D,$0A
        .BYTE   "  / /| ____|/ ____/ _ \__ \ ",$0D,$0A
        .BYTE   " / /_| |__ | |   | | | | ) | ",$0D,$0A
        .BYTE   "| '_ \___ \| |   | | | |/ / ",$0D,$0A
        .BYTE   "| (_) |__) | |___| |_| / /_ ",$0D,$0A
        .BYTE   " \___/____/ \_____\___/____| ",$0D,$0A
        .BYTE   "* 65c02 SuperMON ",$0D,$0A,$00

        .SEGMENT "VECTORS"
NNTVECTOR:
        .WORD   BRKROUTINE      ;
RSTVECTOR:
        .WORD   COLD_START      ;
INTVECTOR:
        .WORD   BRKROUTINE      ; ROM VECTOR FOR IRQ

        .END
