; Character devices:
;----------------------------
;  Console (Redirected)
;  Serial ports (1-x)
;  ParPortProp
;  VDU
;  CVDU
;  Printer Port

; Block Devices
;----------------------------
; ParPortProp SD Card
; PPP IDE
; Floppy
; MD

; DSKY?
; RTC?
; IO PORTS?
; SOUND CARDS?
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

        .WORD   PPPOUTCH        ; FUNCTION 09 - WRITE PROP VIDEO
        .WORD   PPPCONIN        ; FUNCTION 10 - READ PROP KEYBOARD
        .WORD   PPPCONINW       ; FUNCTION 11 - READ  PROP KEYBOARD (BLOCKING)
        .WORD   PPPCONSTATUS    ; FUNCTION 12 - GET  PROP KEYBOARD STATUS
        .WORD   INIT_PPP        ; FUNCTION 13 - INIT PROP INTERFACE

        .WORD   0000            ; FUNCTION 14
        .WORD   0000            ; FUNCTION 15
        .WORD   0000            ; FUNCTION 16
        .WORD   0000            ; FUNCTION 17
        .WORD   0000            ; FUNCTION 18
        .WORD   0000            ; FUNCTION 19
        .WORD   0000            ; FUNCTION 20
        .WORD   0000            ; FUNCTION 21
        .WORD   0000            ; FUNCTION 22
        .WORD   0000            ; FUNCTION 23
        .WORD   0000            ; FUNCTION 24
        .WORD   0000            ; FUNCTION 25
        .WORD   0000            ; FUNCTION 26
        .WORD   0000            ; FUNCTION 27
        .WORD   0000            ; FUNCTION 28
        .WORD   0000            ; FUNCTION 29
        .WORD   0000            ; FUNCTION 30
        .WORD   0000            ; FUNCTION 31
        .WORD   0000            ; FUNCTION 32
        .WORD   0000            ; FUNCTION 33
        .WORD   0000            ; FUNCTION 34
        .WORD   0000            ; FUNCTION 35
        .WORD   0000            ; FUNCTION 36
        .WORD   0000            ; FUNCTION 37
        .WORD   0000            ; FUNCTION 38
        .WORD   0000            ; FUNCTION 39
;
        .WORD   DSKY_INIT       ; FUNCTION 40 -
        .WORD   DSKY_SHOW       ; FUNCTION 41 -
        .WORD   DSKY_BIN2SEG    ; FUNCTION 42 -
        .WORD   DSKY_RESET      ; FUNCTION 43 -
        .WORD   DSKY_STAT       ; FUNCTION 44 -
        .WORD   DSKY_GETKEY     ; FUNCTION 45 -
        .WORD   DSKY_BEEP       ; FUNCTION 46 -
        .WORD   DSKY_DSPL       ; FUNCTION 47 -
        .WORD   DSKY_PUTLED     ; FUNCTION 48 -
        .WORD   DSKY_BLANK      ; FUNCTION 49 -
;
        .WORD   RTC_WRITE       ; FUNCTION 50 -
        .WORD   RTC_READ        ; FUNCTION 51 -
        .WORD   RTC_RESET       ; FUNCTION 52 -

        .WORD   0000            ; FUNCTION 53
        .WORD   0000            ; FUNCTION 54
        .WORD   0000            ; FUNCTION 55
        .WORD   0000            ; FUNCTION 56
        .WORD   0000            ; FUNCTION 57
        .WORD   0000            ; FUNCTION 58
        .WORD   0000            ; FUNCTION 59

        .WORD   PPIDE_INIT      ; FUNCTION 60 - called during OS init
        .WORD   IDE_READ_SECTOR ; FUNCTION 61 - read a sector from drive
        .WORD   IDE_WRITE_SECTOR; FUNCTION 62 - write a sector to drive
;
        .WORD   PPP_INITIALIZE  ; FUNCTION 63 - init the ppp sd device
        .WORD   PPP_READ_SECTOR ; FUNCTION 64 - read a sector from the ppp sd device
        .WORD   PPP_WRITE_SECTOR; FUNCTION 65 - write a sector to the ppp sd device
;
;        .WORD   FL_SETUP        ; FUNCTION 66 - init floppy device
;        .WORD   FL_READ_SECTOR  ; FUNCTION 67 - read a sector from floppy device
;        .WORD   FL_WRITE_SECTOR ; FUNCTION 68 - write a sector to floppy device
;
;        .WORD   MD_SHOW         ; FUNCTION 69 - md show information
;        .WORD   MD_READ_SECTOR  ; FUNCTION 70 - read a sector from memory device
;        .WORD   MD_WRITE_SECTOR ; FUNCTION 71 - write a sector to memory device
