;________________________________________________________________________________________________________________________________
;
;	Nhyodyne dos/65 banked driver code
;       Intended for RAM BANK $85
;
;  DWERNER 05/3/2025 	Initial
;________________________________________________________________________________________________________________________________

DSKYOSC         = 1000000       ; Set DSKY NG Osc frequency

        .SEGMENT "DRIVERS"
        .ORG    $8800
;       Area from $85:8000 to $85:8800 reserved for work RAM for drivers
;

;__DISPATCHER________________________________________________________________________________________
;
;  Function dispatcher
;  function to call is located in "farfunct"
;____________________________________________________________________________________________________
;
FUNCTION_DISPATCHER:
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

        .WORD   WRSER1          ; FUNCTION 09 - WRITE VIDEO
        .WORD   RDSER1          ; FUNCTION 10 - READ KEYBOARD
        .WORD   RDSER1W         ; FUNCTION 11 - READ KEYBOARD (BLOCKING)
        .WORD   SERIALSTATUS    ; FUNCTION 12 - GET KEYBOARD STATUS
        .WORD   SERIALINIT      ; FUNCTION 13 - INIT INTERFACE

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
        .WORD   drv_noop        ; FUNCTION 34
        .WORD   drv_noop        ; FUNCTION 35
        .WORD   drv_noop        ; FUNCTION 36
        .WORD   drv_noop        ; FUNCTION 37
        .WORD   drv_noop        ; FUNCTION 38
        .WORD   drv_noop        ; FUNCTION 39
;
        .WORD   drv_noop        ;DSKY_INIT       ; FUNCTION 40 -
        .WORD   drv_noop        ;DSKY_SHOW       ; FUNCTION 41 -
        .WORD   drv_noop        ;DSKY_BIN2SEG    ; FUNCTION 42 -
        .WORD   drv_noop        ;DSKY_RESET      ; FUNCTION 43 -
        .WORD   drv_noop        ;DSKY_STAT       ; FUNCTION 44 -
        .WORD   drv_noop        ;DSKY_GETKEY     ; FUNCTION 45 -
        .WORD   drv_noop        ;DSKY_BEEP       ; FUNCTION 46 -
        .WORD   drv_noop        ;DSKY_DSPL       ; FUNCTION 47 -
        .WORD   drv_noop        ;DSKY_PUTLED     ; FUNCTION 48 -
        .WORD   drv_noop        ;DSKY_BLANK      ; FUNCTION 49 -
;
        .WORD   drv_noop        ; FUNCTION 50 -
        .WORD   drv_noop        ; FUNCTION 51 -
        .WORD   drv_noop        ; FUNCTION 52 -

        .WORD   drv_noop        ; FUNCTION 53
        .WORD   drv_noop        ; FUNCTION 54
        .WORD   drv_noop        ; FUNCTION 55
        .WORD   drv_noop        ; FUNCTION 56
        .WORD   drv_noop        ; FUNCTION 57
        .WORD   drv_noop        ; FUNCTION 58
        .WORD   drv_noop        ; FUNCTION 59

        .WORD   PPIDE_INIT      ; FUNCTION 60 - called during OS init
        .WORD   IDE_READ_SECTOR ; FUNCTION 61 - read a sector from drive
        .WORD   IDE_WRITE_SECTOR; FUNCTION 62 - write a sector to drive

        .WORD   drv_noop        ; FUNCTION 63
        .WORD   drv_noop        ; FUNCTION 64
        .WORD   drv_noop        ; FUNCTION 65
;
;    .WORD   MD_SHOW         ; FUNCTION 63 - init the mem device
;      .WORD   MD_READ_SECTOR  ; FUNCTION 64 - read a sector from the memory device
;     .WORD   MD_WRITE_SECTOR ; FUNCTION 65 - write a sector to the memory device
;

    .WORD   FL_SETUP        ; FUNCTION 66 - init floppy device
    .WORD   FL_READ_SECTOR  ; FUNCTION 67 - read a sector from floppy device
  .WORD   FL_WRITE_SECTOR ; FUNCTION 68 - write a sector to floppy device


;__DRIVERS___________________________________________________________________________________________
;
        .INCLUDE "drvmacros.asm"
        .INCLUDE "dosser.asm"
        .INCLUDE "doside.asm"
        .INCLUDE "dosrtc.asm"
;        .INCLUDE "dosdskyn.asm"
        .INCLUDE "dosflp.asm"



;__DFT_CONSOLE___________________________________________________________________________________________________________________
;
;	      TRANSFER CONSOLE COMMAND TO DESIGNATED CONSOLE
;________________________________________________________________________________________________________________________________
DFT_CONSOLE:
        PHA
        LDA     CONSOLE
        CLC
        ADC     farfunct
        STA     farfunct
        PLA
        JMP     FUNCTION_DISPATCHER


drv_noop:
        RTS
