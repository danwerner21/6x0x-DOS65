;__IDE DRIVERS___________________________________________________________________________________________________________________
;
; 	DOS/65 IDE disk drivers 6502PC - XT IDE ISA CARD
;
;	Entry points:
;		XTIDE_INIT   	- CALLED DURING OS INIT
;		IDE_READ_SECTOR  - read a sector from drive
;		IDE_WRITE_SECTOR - write a sector to drive
;________________________________________________________________________________________________________________________________
;
PC6502_IO       = $E000

XTIDE_DATA_LO   = PC6502_IO+$300
XTIDE_DATA_HI   = PC6502_IO+$301
XTIDE_ERR       = PC6502_IO+$302
XTIDE_FECODE    = PC6502_IO+$302
XTIDE_SEC_CNT   = PC6502_IO+$304
XTIDE_LBALOW    = PC6502_IO+$306
XTIDE_LBAMID    = PC6502_IO+$308
XTIDE_LBAHI     = PC6502_IO+$30A
XTIDE_DEVICE    = PC6502_IO+$30C
XTIDE_COMMAND   = PC6502_IO+$30E
XTIDE_STATUS    = PC6502_IO+$30E




;IDE COMMAND CONSTANTS.  THESE SHOULD NEVER CHANGE.
XTIDE_CMD_RECAL = $10
XTIDE_CMD_READ  = $20
XTIDE_CMD_WRITE = $30
XTIDE_CMD_INIT  = $91
XTIDE_CMD_ID    = $EC
XTIDE_CMD_FEAT  = $EF
XTIDE_CMD_SPINDOWN = $E0
XTIDE_CMD_SPINUP = $E1



        .IFDEF  PC6502BIOS
XTIDE_INIT:
; RESET PORTS
        LDX     #$00
        LDA     #$FF
:
        STA     XTIDE_DATA_LO,X
        INX
        CPX     #$31
        BNE     :-

        LDX     #$00
        LDA     #$00
:
        STA     XTIDE_DATA_LO,X
        INX
        CPX     #$31
        BNE     :-
        JSR     XTIDE_PROBE
        LDA     #$E0            ; E0=MST  F0=SLV
        STA     XTIDE_DEVICE

        JSR     IDE_WAIT_NOT_BUSY;MAKE SURE DRIVE IS READY
        CMP     #$00
        BNE     :+

        LDA     #$01            ; ENABLE 8-BIT MODE (XT-CF-LITE)
        STA     XTIDE_FECODE
        LDA     #XTIDE_CMD_FEAT
        STA     XTIDE_COMMAND
:
        RTS
        .ENDIF


        .IFNDEF PC6502BIOS
XTIDETIMEOUT:
        .BYTE   $00,$00



;__XTIDE_INIT________________________________________________________________________________________
;
;  INIT AND DISPLAY IDE INFO
;____________________________________________________________________________________________________
;
XTIDE_INIT:
        JSR     LFCR            ; AND CRLF
        LDA     #<MESSAGE1      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE1      ;
        STA     STRPTR+1        ;
        JSR     WRSTR
        JSR     LFCR            ; AND CRLF
;
        LDA     #<MESSAGE2      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE2      ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT

        LDA     #>XTIDE_DATA_LO ; GET BASE PORT
        JSR     PRINT_BYTE      ; PRINT BASE PORT
        LDA     #<XTIDE_DATA_LO ; GET BASE PORT
        JSR     PRINT_BYTE      ; PRINT BASE PORT
;
        JSR     XTIDE_PROBE     ; DETECT AN ATA DEVICE, ABORT IF NOT FOUND
        CMP     #$00
        BNE     IDE_ABORT
        JMP     IDE_PRINT_INFO
IDE_ABORT:
        LDA     #<MESSAGE3      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE3      ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT
        JMP     IDE_INITA
IDE_PRINT_INFO:
        JSR     LFCR            ; AND CRLF
        LDA     #<MESSAGE4      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE4      ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT
        LDA     #$00
        JSR     IDE_READ_INFO   ; GET DRIVE INFO, ABORT IF ERROR
        LDA     #<MESSAGE5      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE5      ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT
        LDA     #$01
        JSR     IDE_READ_INFO   ; GET DRIVE INFO, ABORT IF ERROR
IDE_INITA:
        RTS                     ; DONE
        .ENDIF
;
;__XTIDE_PROBE_______________________________________________________________________________________
;
;  XTPROBE FOR IDE HARDWARE
;____________________________________________________________________________________________________
;
XTIDE_PROBE:
;
; RESET PORTS
        LDX     #$00
        LDA     #$FF
:
        STA     XTIDE_DATA_LO,X
        INX
        CPX     #$31
        BNE     :-

        LDX     #$00
        LDA     #$00
:
        STA     XTIDE_DATA_LO,X
        INX
        CPX     #$31
        BNE     :-
; CHECK SIGNATURE
        LDY     #$00
        LDX     #$00
;       SOMETIMES THE CF-XTIDE WILL ONLY READ 80, THIS CAN BE RESET BY WRITING ZEROS UNTIL VALUES ARE PROPERLY READ
:
        LDA     XTIDE_DATA_LO
        CMP     #$80
        BNE     :+
        LDA     #$00
        STA     XTIDE_DATA_LO
        NOP
        STA     XTIDE_DATA_HI
        NOP
        STA     XTIDE_LBALOW
        NOP
        STA     XTIDE_LBAMID
        NOP
        STA     XTIDE_LBAHI
        NOP
        STA     XTIDE_DEVICE
        NOP
        STA     XTIDE_COMMAND
        NOP
        STA     XTIDE_STATUS
        NOP
        INX
        CPX     #$00
        BNE     :-
        INY
        CPY     #$03
        BNE     :-
        JMP     XTIDE_PROBE_FAIL; TIMED OUT
:
        JSR     IDE_WAIT_NOT_BUSY;MAKE SURE DRIVE IS READY
        CMP     #$00
        BNE     XTIDE_PROBE_FAIL
        JMP     XTIDE_PROBE_SUCCESS
XTIDE_PROBE_FAIL:
        LDA     #$FF
        RTS
XTIDE_PROBE_SUCCESS:
        LDA     #$00
        RTS                     ; DONE, NOTE THAT A=0 AND Z IS SET

        .IFNDEF PC6502BIOS
;*__IDE_READ_INFO___________________________________________________________________________________
;*
;*  READ IDE INFORMATION
;*	CARRY SET ON ERROR
;* 	A=MST/SLV
;*____________________________________________________________________________________________________
IDE_READ_INFO:
; SET DRIVE BIT
        AND     #$01            ; ONLY WANT THE 1 BIT (MST/SLV)
        ASL     A               ; SHIFT 4
        ASL     A               ;
        ASL     A               ;
        ASL     A               ;
        ORA     #$E0            ; E0=MST  F0=SLV
        STA     XTIDE_DEVICE

        JSR     IDE_WAIT_NOT_BUSY;MAKE SURE DRIVE IS READY
        CMP     #$00
        BNE     IDE_READ_INFO_ABORT

        LDA     #$01            ; ENABLE 8-BIT MODE (XT-CF-LITE)
        STA     XTIDE_FECODE
        LDA     #XTIDE_CMD_FEAT
        STA     XTIDE_COMMAND

        NOP                     ; TINY DELAY, JUST IN CASE
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP


        LDA     #XTIDE_CMD_ID   ; ID COMMAND
        STA     XTIDE_COMMAND

        JSR     IDE_WAIT_DRQ    ;WAIT UNTIL IT'S GOT THE DATA
        CMP     #$00
        BNE     IDE_READ_INFO_ABORT

        JSR     IDE_READ_BUFFER ; GRAB THE 256 WORDS FROM THE BUFFER

        LDA     #<MESSAGE6      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE6      ;
        STA     STRPTR+1        ;
        JSR     WRSTR
        LDA     hstbuf+123
        JSR     PRINT_BYTE
        LDA     hstbuf+122
        JSR     PRINT_BYTE
        LDA     hstbuf+121
        JSR     PRINT_BYTE
        LDA     hstbuf+120
        JSR     PRINT_BYTE
        JMP     IDE_READ_INFO_OK
IDE_READ_INFO_ABORT:
        LDA     #<MESSAGE3      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE3      ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ;DO PROMPT
        JSR     LFCR            ;AND CRLF
        LDA     #$FF
        RTS                     ;
IDE_READ_INFO_OK:
        JSR     LFCR            ; AND CRLF
        LDA     #$00
        RTS
        .ENDIF


;*__IDE_READ_SECTOR___________________________________________________________________________________
;*
;*  READ IDE SECTOR (IN LBA) INTO BUFFER
;*
;*____________________________________________________________________________________________________
IDE_READ_SECTOR:
        .IFNDEF PC6502BIOS
        JSR     IDE_CONVERT_SECTOR_LBA
        LDA     debsehd         ; STORE CURRENT PARMS
        CMP     Cdebsehd        ;
        BNE     IDE_READ_SECTOR_DIRTY
        LDA     debcylm         ;
        CMP     Cdebcylm        ;
        BNE     IDE_READ_SECTOR_DIRTY
        LDA     debcyll         ;
        CMP     Cdebcyll        ;
        BNE     IDE_READ_SECTOR_DIRTY
        LDA     sekdsk          ;
        CMP     currentDrive
        BNE     IDE_READ_SECTOR_DIRTY
        LDA     #$00            ; ZERO = 1 ON RETURN = OPERATION OK
        RTS
        .ENDIF
IDE_READ_SECTOR_DIRTY:
        JSR     IDE_WAIT_NOT_BUSY;MAKE SURE DRIVE IS READY
        CMP     #$00
        BNE     IDE_READ_SECTOR_ERROR; IF TIMEOUT, REPORT NO IDE PRESENT
IDE_READ_SECTOR_1:
        JSR     IDE_SETUP_LBA   ;TELL IT WHICH SECTOR WE WANT
        LDA     #XTIDE_CMD_READ
        STA     XTIDE_COMMAND

        JSR     IDE_WAIT_DRQ    ; WAIT UNTIL IT'S GOT THE DATA
        CMP     #$00
        BNE     IDE_READ_SECTOR_ERROR; IF TIMEOUT, REPORT NO IDE PRESENT
        JSR     IDE_READ_BUFFER ; GRAB THE 256 WORDS FROM THE BUFFER
        .IFNDEF PC6502BIOS
        LDA     debsehd         ; STORE CURRENT PARMS
        STA     Cdebsehd        ;
        LDA     debcyll         ;
        STA     Cdebcyll        ;
        LDA     debcylm         ;
        STA     Cdebcylm        ;
        .ENDIF

        LDA     #$00            ; ZERO = 1 ON RETURN = OPERATION OK
        RTS
IDE_READ_SECTOR_ERROR:
        LDA     #$FF            ; ZERO = 1 ON RETURN = OPERATION OK
        RTS


;*__IDE_WAIT_NOT_BUSY_______________________________________________________________________________
;*
;*  WAIT FOR IDE CHANNEL TO BECOME READY
;*
;*____________________________________________________________________________________________________
IDE_WAIT_NOT_BUSY:
        TXA
        PHA
        TYA
        PHA
        LDA     #$00
        STA     XTIDETIMEOUT
        STA     XTIDETIMEOUT+1
IDE_WAIT_NOT_BUSY1:
        LDA     XTIDE_STATUS    ;WAIT FOR RDY BIT TO BE SET
        AND     #$80
        CMP     #$00
        BEQ     IDE_WAIT_NOT_BUSY2
        INC     XTIDETIMEOUT
        BNE     IDE_WAIT_NOT_BUSY1
        INC     XTIDETIMEOUT+1
        BNE     IDE_WAIT_NOT_BUSY1
        PLA
        TAY
        PLA
        TAX
        LDA     #$FF
        RTS
IDE_WAIT_NOT_BUSY2:
        PLA
        TAY
        PLA
        TAX
        LDA     #$00
        RTS

;*__IDE_WAIT_DRQ______________________________________________________________________________________
;*
;*	WAIT FOR THE DRIVE TO BE READY TO TRANSFER DATA.
;*
;*____________________________________________________________________________________________________
IDE_WAIT_DRQ:
        LDA     #$00
        STA     XTIDETIMEOUT
        STA     XTIDETIMEOUT+1
IDE_WAIT_DRQ1:
        LDA     XTIDE_STATUS    ;WAIT FOR DRQ BIT TO BE SET
        AND     #%10001000      ; MASK OFF BUSY(7) AND DRQ(3)
        CMP     #%00001000      ; WE WANT BUSY(7) TO BE 0 AND DRQ (3) TO BE 1
        BEQ     IDE_WAIT_DRQ2
        AND     #%00000001      ; IS ERROR?
        CMP     #%00000001      ;
        BEQ     IDE_WAIT_DRQE
        INC     XTIDETIMEOUT
        BNE     IDE_WAIT_DRQ1
        INC     XTIDETIMEOUT+1
        BNE     IDE_WAIT_DRQ1
IDE_WAIT_DRQE:
        LDA     #$FF
        RTS
IDE_WAIT_DRQ2:
        LDA     #$00
        RTS


;*__IDE_READ_BUFFER___________________________________________________________________________________
;*
;*  READ IDE BUFFER LITTLE ENDIAN
;*
;*____________________________________________________________________________________________________
IDE_READ_BUFFER:
        LDY     #$00            ; INDEX
:
        LDA     XTIDE_DATA_LO
        STA     hstbuf,Y        ;
        INY
        LDA     XTIDE_DATA_HI
        STA     hstbuf,Y        ;
        INY
        CPY     #$00            ;
        BNE     :-
:
;
        LDA     XTIDE_DATA_LO
        STA     hstbuf+$100,Y   ;
        INY
        LDA     XTIDE_DATA_HI
        STA     hstbuf+$100,Y   ;
        INY
        CPY     #$00            ;
        BNE     :-              ;
        RTS                     ;

;*__IDE_WRITE_SECTOR__________________________________________________________________________________
;*
;*  WRITE IDE SECTOR (IN LBA) FROM BUFFER
;*
;*____________________________________________________________________________________________________
IDE_WRITE_SECTOR:
        .IFNDEF PC6502BIOS
        JSR     IDE_CONVERT_SECTOR_LBA
        .ENDIF
        JSR     IDE_WAIT_NOT_BUSY;MAKE SURE DRIVE IS READY
        CMP     #$00
        BNE     IDE_WRITE_SECTOR_ERROR; IF TIMEOUT, REPORT NO IDE PRESENT
        JSR     IDE_SETUP_LBA   ;TELL IT WHICH SECTOR WE WANT
        LDA     #XTIDE_CMD_WRITE
        STA     XTIDE_COMMAND
        JSR     IDE_WAIT_DRQ    ;WAIT UNIT IT WANTS THE DATA
        CMP     #$00
        BNE     IDE_WRITE_SECTOR_ERROR; IF TIMEOUT, REPORT NO IDE PRESENT
        JSR     IDE_WRITE_BUFFER;GIVE THE DATA TO THE DRIVE
        JSR     IDE_WAIT_NOT_BUSY;WAIT UNTIL THE WRITE IS COMPLETE
        CMP     #$00
        BNE     IDE_WRITE_SECTOR_ERROR; IF TIMEOUT, REPORT NO IDE PRESENT
        .IFNDEF PC6502BIOS
        LDA     #$FF            ; INVALIDATE CACHE
        STA     Cdebsehd        ;
        STA     Cdebcyll        ;
        STA     Cdebcylm        ;
        .ENDIF
        LDA     #$00            ; ZERO = 1 ON RETURN = OPERATION OK
        RTS
IDE_WRITE_SECTOR_ERROR:
        LDA     #$FF
        RTS

;*__IDE_WRITE_BUFFER___________________________________________________________________________________
;*
;*  WRITE IDE BUFFER LITTLE ENDIAN
;*
;*____________________________________________________________________________________________________
IDE_WRITE_BUFFER:
        LDY     #$00            ; INDEX
:
        LDA     hstbuf,Y        ;
        STA     XTIDE_DATA_LO
        INY
        LDA     hstbuf,Y        ;
        STA     XTIDE_DATA_HI
        INY
        CPY     #$00            ;
        BNE     :-              ;
:
        LDA     hstbuf+$100,Y   ;
        STA     XTIDE_DATA_LO
        INY
        LDA     hstbuf+$100,Y   ;
        STA     XTIDE_DATA_HI
        INY
        CPY     #$00            ;
        BNE     :-              ;
        RTS                     ;

        .IFNDEF PC6502BIOS
MESSAGE1:
        .BYTE   "XT-IDE:"
        .BYTE   00
MESSAGE2:
        .BYTE   " IO=0x"
        .BYTE   00
MESSAGE3:
        .BYTE   " NOT PRESENT"
        .BYTE   00
MESSAGE4:
        .BYTE   " XT-IDE0: BLOCKS="
        .BYTE   00
MESSAGE5:
        .BYTE   " XT-IDE1: BLOCKS="
        .BYTE   00
MESSAGE6:
        .BYTE   "0x"
        .BYTE   00

;___IDE_CONVERT_SECTOR_LBA_______________________________________________________________________________
;
; 	TRANSLATE LBA SECTORS
;________________________________________________________________________________________________________
IDE_CONVERT_SECTOR_LBA:
        LDA     sektrk          ; LOAD TRACK # (LOW BYTE)
        AND     #$0F            ; ISOLATE HEAD IN LOW 4 BITS
        ASL     a               ; MOVE TO HIGH BYTE
        ASL     a
        ASL     a
        ASL     a
        TAX                     ; PARK IN X
        LDA     seksec          ; LOAD SECTOR # (LOW BYTE)
        LSR     A               ;
        LSR     A               ; DIVIDE BY 4 (FOR BLOCKING)
        AND     #$0F            ; CLEAR UPPER 4 BITS (JUST 'CAUSE)
        STA     debsehd         ; STORE IN SECTOR/HEAD
        TXA                     ; GET HEAD BACK
        ORA     debsehd
        STA     debsehd         ; STORE IN SECTOR/HEAD

        LDA     sektrk
        STA     debcyll         ; STORE IN TRACK (lsb)
        LDA     sektrk+1
        STA     debcylm         ; STORE IN TRACK (msb)
; REMOVE HEAD FROM TRACK VALUE (DIV/4)
        LDA     debcylm
        LSR     A
        STA     debcylm
        LDA     debcyll
        ROR     A
        STA     debcyll

        LDA     debcylm
        LSR     A
        STA     debcylm
        LDA     debcyll
        ROR     A
        STA     debcyll

        LDA     debcylm
        LSR     A
        STA     debcylm
        LDA     debcyll
        ROR     A
        STA     debcyll

        LDA     debcylm
        LSR     A
        STA     debcylm
        LDA     debcyll
        ROR     A
        STA     debcyll
;	ADD SLICE OFFSET
        LDA     sekdsk          ; GET DRIVE#
        AND     #7              ; ONLY FIRST 8 DEVICES SUPPORTED
        ASL     a               ; DOUBLE NUMBER FOR TABLE LOOKUP
        TAX                     ; MOVE TO X REGISTER
        INX                     ; WANT SECOND BYTE OF ENTRY
        LDA     dskcfg,X        ; GET SLICE#
        STA     slicetmp+1      ; SLICE OFFSET MSB
        LDA     #0              ; GET SLICE#
        STA     slicetmp        ; SLICE OFFSET LSB
        CLC                     ; VOODOO MATH TO TAKE SLICE*$4000
        ROR     slicetmp+1
        ROR     slicetmp
        ROR     slicetmp+1
        ROR     slicetmp

        LDA     dskcfg,X        ; GET SLICE#
        CLC
        ADC     slicetmp
        STA     slicetmp
        LDA     #$00            ; LOGIC ERROR FOR SLICES THAT CARRY?
        ADC     slicetmp+1      ;
        STA     slicetmp+1      ;

; ADD SLICE OFFSET TO TRACK #
        LDA     slicetmp
        ADC     debcyll
        STA     debcyll         ; store sum of LSBs
        LDA     slicetmp+1
        ADC     debcylm         ; add the MSBs using carry from
        STA     debcylm         ; the previous calculation
        RTS

        .ENDIF

;*__IDE_SETUP_LBA_____________________________________________________________________________________
;*
;*  SETUP LBA DATA
;*  A= DRIVE DEVICE
;*____________________________________________________________________________________________________
IDE_SETUP_LBA:
        LDA     currentDrive
        AND     #$01            ; only want drive cfg
        ASL     a               ; SHIFT 4
        ASL     a               ;
        ASL     a               ;
        ASL     a               ;
        ORA     #$E0            ; E0=MST  F0=SLV
        STA     XTIDE_DEVICE

        LDA     debcylm
        STA     XTIDE_LBAHI

        LDA     debcyll
        STA     XTIDE_LBAMID

        LDA     debsehd
        STA     XTIDE_LBALOW

        LDA     #$01
        STA     XTIDE_SEC_CNT
        RTS

        .IFDEF  PC6502BIOS
; Boot System [B]
BOOT:
        JSR     XTIDE_INIT      ; INIT IDE
                                ; SETUP LOCATION
        LDA     #$00
        STA     debcyll         ;
        STA     debcylm         ;
        STA     debsehd         ;
        STA     currentDrive    ;
        STA     SRC             ; SETUP SOURCE AND DEST POINTERS
        STA     DEST
        LDA     #$04
        STA     SRC+1
        LDA     #$08
        STA     DEST+1

BOOT1:
        JSR     IDE_READ_SECTOR ; READ THE SECTOR
        CMP     #$00
        BNE     BOOTX
        LDY     #$00
:
        LDA     (SRC),Y
        STA     (DEST),Y
        INY
        CPY     #$00
        BNE     :-
        INC     SRC+1
        INC     DEST+1
:
        LDA     (SRC),Y
        STA     (DEST),Y
        INY
        CPY     #$00
        BNE     :-
        INC     DEST+1
        LDA     #$04
        STA     SRC+1
        INC     debsehd
        LDA     DEST+1
        CMP     #$80
        BNE     BOOT1
        JMP     $0800
BOOTX:
        JMP     ERROR           ; back to main loop

WBOOT:
        LDA     #<MESSAGE7      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE7      ;
        STA     STRPTR+1        ;
        JSR     OUTSTR
        JSR     IOF_CONINW
        CMP     #'Y'
        BEQ     :+

        LDA     #<MESSAGE8      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE8      ;
        STA     STRPTR+1        ;
        JSR     OUTSTR
        BRK


:
        JSR     XTIDE_INIT      ; INIT IDE
                                ; SETUP LOCATION
        LDA     #$00
        STA     debcyll         ;
        STA     debcylm         ;
        STA     debsehd         ;
        STA     currentDrive    ;
        STA     SRC             ; SETUP SOURCE AND DEST POINTERS
        STA     DEST
        LDA     #$08
        STA     SRC+1
        LDA     #$04
        STA     DEST+1

BOOTW1:
        LDY     #$00
:
        LDA     (SRC),Y
        STA     (DEST),Y
        INY
        CPY     #$00
        BNE     :-
        INC     SRC+1
        INC     DEST+1
:
        LDA     (SRC),Y
        STA     (DEST),Y
        INY
        CPY     #$00
        BNE     :-

        JSR     IDE_WRITE_SECTOR; READ THE SECTOR
        CMP     #$00
        BNE     BOOTX

        INC     SRC+1
        LDA     #$04
        STA     DEST+1
        INC     debsehd
        LDA     SRC+1
        CMP     #$80
        BNE     BOOTW1

        LDA     #<MESSAGE9      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE9      ;
        STA     STRPTR+1        ;
        JSR     OUTSTR
        BRK

MESSAGE7:
        .BYTE   13,10," ARE YOU SURE YOU WANT TO REPLACE BOOT IMAGE?",13,10
        .BYTE   " PRESS 'Y' TO CONTINUE",13,10
        .BYTE   00
MESSAGE8:
        .BYTE   " BOOT IMAGE WRITE ABORTED.",13,10
        .BYTE   00
MESSAGE9:
        .BYTE   " BOOT IMAGE WRITTEN.",13,10
        .BYTE   00

        .ENDIF
