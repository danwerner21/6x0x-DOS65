;________________________________________________________________________________________________________________________________
;
;	6502PC dos/65 banked driver code
;
;
;  DWERNER 12/20/2025 	Initial
;________________________________________________________________________________________________________________________________

PC6502_IOSPACE  = $EF00
        .INCLUDE "../dos65_os/dosdefn.asm"

        .SEGMENT "DRIVERS"
        .ORG    $C000

;__DISPATCHER________________________________________________________________________________________
;
;  Function dispatcher
;  function to call is located in "farfunct"
;____________________________________________________________________________________________________
;
FUNCTION_DISPATCHER1:
        PHA
        TXA
        PHA
        LDA     farfunct
        ASL     A               ; DOUBLE NUMBER FOR TABLE LOOKUP
        TAX
        LDA     DISPATCHTABLE,X
        STA     farpointer
        LDA     DISPATCHTABLE+1,X
        STA     farpointer+1
        PLA
        TAX
        PLA
        JSR     jsrindirect
        RTS

jsrindirect:
        JMP     (farpointer)

DISPATCHTABLE:
        .WORD   DFT_CONSOLE     ; FUNCTION 00 - WRITE CONSOLE
        .WORD   DFT_CONSOLE     ; FUNCTION 01 - READ CONSOLE
        .WORD   DFT_CONSOLE     ; FUNCTION 02 - READ CONSOLE (BLOCKING)
        .WORD   DFT_CONSOLE     ; FUNCTION 03 - GET CONSOLE STATUS

        .WORD   WRSER1          ; FUNCTION 04 - WRITE SERIAL PORT
        .WORD   RDSER1          ; FUNCTION 05 - READ SERIAL PORT
        .WORD   RDSER1W         ; FUNCTION 06 - READ SERIAL PORT (BLOCKING)
        .WORD   SERIALSTATUS    ; FUNCTION 07 - GET SERIAL STATUS
        .WORD   SERIALINIT      ; FUNCTION 08 - SERIAL PORT INIT

        .WORD   ESPVIDEOOUT     ; FUNCTION 09 - WRITE VIDEO
        .WORD   ESPPS2IN        ; FUNCTION 10 - READ KEYBOARD
        .WORD   ESPPS2INW       ; FUNCTION 11 - READ KEYBOARD (BLOCKING)
        .WORD   ESPPS2BUFL      ; FUNCTION 12 - GET KEYBOARD STATUS
        .WORD   ESPINIT         ; FUNCTION 13 - INIT INTERFACE

        .WORD   drv_noop        ; FUNCTION 14
        .WORD   drv_noop        ; FUNCTION 15
        .WORD   drv_noop        ; FUNCTION 16
        .WORD   drv_noop        ; FUNCTION 17
        .WORD   drv_noop        ; FUNCTION 18

        .WORD   drv_noop        ; FUNCTION 19
        .WORD   drv_noop        ; FUNCTION 20
        .WORD   drv_noop        ; FUNCTION 21
        .WORD   drv_noop        ; FUNCTION 22
        .WORD   drv_noop        ; FUNCTION 23

        .WORD   drv_noop        ; FUNCTION 24
        .WORD   drv_noop        ; FUNCTION 25
        .WORD   drv_noop        ; FUNCTION 26
        .WORD   drv_noop        ; FUNCTION 27
        .WORD   drv_noop        ; FUNCTION 28

        .WORD   drv_noop        ; FUNCTION 29
        .WORD   drv_noop        ; FUNCTION 30
        .WORD   drv_noop        ; FUNCTION 31
        .WORD   drv_noop        ; FUNCTION 32
        .WORD   drv_noop        ; FUNCTION 33

        .WORD   LPT_OUT         ; SEND BYTE TO LPT PORT ; FUNCTION 34
        .WORD   KBD_GETKEY      ; GET a character from the MULTI IO ps/2 keyboard ; FUNCTION 35
        .WORD   MULTIOINIT      ; INIT MULTI IO CARD   ; FUNCTION 36
        .WORD   drv_noop        ; FUNCTION 37
        .WORD   drv_noop        ; FUNCTION 38
        .WORD   drv_noop        ; FUNCTION 39

        .WORD   drv_noop        ; DSKY_INIT       ; FUNCTION 40 -
        .WORD   drv_noop        ; DSKY_SHOW       ; FUNCTION 41 -
        .WORD   drv_noop        ; DSKY_BIN2SEG    ; FUNCTION 42 -
        .WORD   drv_noop        ; DSKY_RESET      ; FUNCTION 43 -
        .WORD   drv_noop        ; DSKY_STAT       ; FUNCTION 44 -
        .WORD   drv_noop        ; DSKY_GETKEY     ; FUNCTION 45 -
        .WORD   drv_noop        ; DSKY_BEEP       ; FUNCTION 46 -
        .WORD   drv_noop        ; DSKY_DSPL       ; FUNCTION 47 -
        .WORD   drv_noop        ; DSKY_PUTLED     ; FUNCTION 48 -
        .WORD   drv_noop        ; DSKY_BLANK      ; FUNCTION 49 -
;
        .WORD   RTC_WRITE       ; FUNCTION 50 - WRITE RTC REGISTER
        .WORD   RTC_READ        ; FUNCTION 51 - READ RTC REGISTER
        .WORD   RTC_INIT        ; FUNCTION 52 - INIT RTC
        .WORD   RTC_LED         ; FUNCTION 53 - CONTROL LEDS
        .WORD   RTC_BUTTON      ; FUNCTION 54 - READ BUTTON
        .WORD   RTC_BEEP        ; FUNCTION 55 - MAKE SOME NOISE
;
        .WORD   drv_noop        ; FUNCTION 56
        .WORD   drv_noop        ; FUNCTION 57
        .WORD   drv_noop        ; FUNCTION 58
        .WORD   drv_noop        ; FUNCTION 59

        .WORD   XTIDE_INIT      ; FUNCTION 60 - called during OS init
        .WORD   IDE_READ_SECTOR ; FUNCTION 61 - read a sector from drive
        .WORD   IDE_WRITE_SECTOR; FUNCTION 62 - write a sector to drive
;
        .WORD   CH375INIT       ; FUNCTION 63 - init the mem device
        .WORD   CH_READSEC      ; FUNCTION 64 - read a sector from the memory device
        .WORD   CH_WRITESEC     ; FUNCTION 65 - write a sector to the memory device
;
        .WORD   drv_noop        ; FL_SETUP        ; FUNCTION 66 - init floppy device
        .WORD   drv_noop        ; FL_READ_SECTOR  ; FUNCTION 67 - read a sector from floppy device
        .WORD   drv_noop        ; FL_WRITE_SECTOR ; FUNCTION 68 - write a sector to floppy device

;______________________________________________________________________________



;__DRIVERS___________________________________________________________________________________________
;
        .INCLUDE "bios_serial.asm"
        .INCLUDE "bios_ide.asm"
        .INCLUDE "bios_ch375.asm"
        .INCLUDE "bios_esp.asm"
        .INCLUDE "bios_multi.asm"
        .INCLUDE "bios_rtc.asm"




;__DFT_CONSOLE___________________________________________________________________________________________________________________
;
;	      TRANSFER CONSOLE COMMAND TO DESIGNATED CONSOLE
;________________________________________________________________________________________________________________________________
DFT_CONSOLE:
        STA     drvtmp
        LDA     CONSOLE
        CLC
        ADC     farfunct
        STA     farfunct
        PLA
        PLA
        LDA     drvtmp
        JMP     FUNCTION_DISPATCHER1
drvtmp:
        .BYTE   00

drv_noop:
        RTS

DFT_CONSOLE_OUT:
        pha
        LDA    CONSOLE
        sta    farfunct
        pla
        JMP    FUNCTION_DISPATCHER1

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
