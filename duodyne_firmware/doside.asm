;__IDE DRIVERS___________________________________________________________________________________________________________________
;
; 	Duodyne IDE disk drivers for Z80PPIDE card
;
;	Entry points:
;		PPIDE_INIT   	- called during OS init
;		IDE_READ_SECTOR - read a sector from drive
;		IDE_WRITE_SECTOR- write a sector to drive
;________________________________________________________________________________________________________________________________
;
PPIDE_PPI       = IO_AREA+$88   ; PORT A
;
PPIDELO         = PPIDE_PPI+0   ; LSB
PPIDEHI         = PPIDE_PPI+1   ; MSB
PPIDECNTRL      = PPIDE_PPI+2   ; Control Signals
PPIDEPPIC       = PPIDE_PPI+3   ; CONTROL BYTE PPI 82C55

PPIDE_A0_LINE   = $01           ;DIRECT FROM 8255 TO IDE INTERFACE
PPIDE_A1_LINE   = $02           ;DIRECT FROM 8255 TO IDE INTERFACE
PPIDE_A2_LINE   = $04           ;DIRECT FROM 8255 TO IDE INTERFACE
PPIDE_CS0_LINE  = $08           ;INVERTER BETWEEN 8255 AND IDE INTERFACE
PPIDE_CS1_LINE  = $10           ;INVERTER BETWEEN 8255 AND IDE INTERFACE
PPIDE_WR_LINE   = $20           ;INVERTER BETWEEN 8255 AND IDE INTERFACE
PPIDE_RD_LINE   = $40           ;INVERTER BETWEEN 8255 AND IDE INTERFACE
PPIDE_RST_LINE  = $80           ;INVERTER BETWEEN 8255 AND IDE INTERFACE

PPIDE_DATA      = PPIDE_CS0_LINE
PPIDE_ERR       = PPIDE_CS0_LINE + PPIDE_A0_LINE
PPIDE_SEC_CNT   = PPIDE_CS0_LINE + PPIDE_A1_LINE
PPIDE_LBALOW    = PPIDE_CS0_LINE + PPIDE_A1_LINE + PPIDE_A0_LINE
PPIDE_LBAMID    = PPIDE_CS0_LINE + PPIDE_A2_LINE
PPIDE_LBAHI     = PPIDE_CS0_LINE + PPIDE_A2_LINE + PPIDE_A0_LINE
PPIDE_DEVICE    = PPIDE_CS0_LINE + PPIDE_A2_LINE + PPIDE_A1_LINE
PPIDE_COMMAND   = PPIDE_CS0_LINE + PPIDE_A2_LINE + PPIDE_A1_LINE + PPIDE_A0_LINE
PPIDE_STATUS    = PPIDE_CS0_LINE + PPIDE_A2_LINE + PPIDE_A1_LINE + PPIDE_A0_LINE
PPIDE_CONTROL   = PPIDE_CS1_LINE + PPIDE_A2_LINE + PPIDE_A1_LINE
PPIDE_ASTATUS   = PPIDE_CS1_LINE + PPIDE_A2_LINE + PPIDE_A1_LINE + PPIDE_A0_LINE


;IDE COMMAND CONSTANTS.  THESE SHOULD NEVER CHANGE.
PPIDE_CMD_RECAL = $10
PPIDE_CMD_READ  = $20
PPIDE_CMD_WRITE = $30
PPIDE_CMD_INIT  = $91
PPIDE_CMD_ID    = $EC
PPIDE_CMD_SPINDOWN = $E0
PPIDE_CMD_SPINUP = $E1


PPRD_IDE_8255   = %10010010     ;IDE_8255_CTL OUT, IDE_8255_LSB/MSB INPUT
PPWR_IDE_8255   = %10000000     ;ALL THREE PORTS OUTPUT


;__PPIDE_INIT_________________________________________________________________________________________
;
;  INIT AND DISPLAY IDE INFO
;____________________________________________________________________________________________________
;
PPIDE_INIT:
        PRTDBG  "PPIDE INIT:$"
        PRTS    "PPIDE :$"
        JSR     NEWLINE
        JSR     IDE_PPIDETECT   ; TEST FOR PPI HARDWARE
        BNE     IDE_ABORT       ; BAIL OUT IF NOT THERE
;
        PRTS    " IO=0x$"
        LDA     #>PPIDE_PPI     ; GET BASE PORT
        JSR     PRTHEXBYTE      ; PRINT BASE PORT
        LDA     #<PPIDE_PPI     ; GET BASE PORT
        JSR     PRTHEXBYTE      ; PRINT BASE PORT
;
        JSR     PPIDE_RESET     ; RESET THE BUS
        JSR     PPIDE_PROBE     ; DETECT AN ATA DEVICE, ABORT IF NOT FOUND
        BCS     IDE_ABORT
        JMP     IDE_PRINT_INFO
IDE_ABORT:
        PRTS    " NOT PRESENT$" ; NOT PRESENT
        JMP     IDE_INITA
IDE_PRINT_INFO:
        JSR     NEWLINE
        PRTS    " PPIDE0: Blocks=$"
        LDA     #$00
        JSR     IDE_READ_INFO   ; GET DRIVE INFO, ABORT IF ERROR
        PRTS    " PPIDE1: Blocks=$"
        LDA     #$01
        JSR     IDE_READ_INFO   ; GET DRIVE INFO, ABORT IF ERROR
IDE_INITA:
        JSR     NEWLINE
        RTS                     ; DONE
;
;__PPIDE_PROBE_______________________________________________________________________________________
;
;  PROBE FOR IDE HARDWARE
;____________________________________________________________________________________________________
;
PPIDE_PROBE:
;
; BELOW TESTS FOR EXISTENCE OF AN IDE CONTROLLER ON THE
; PPIDE INTERFACE.  WE WRITE A VALUE OF ZERO FIRST SO THAT
; THE PPI BUS HOLD WILL RETURN A VALUE OF ZERO IF THERE IS
; NOTHING CONNECTED TO PPI PORT A.  THEN WE READ THE STATUS
; REGISTER.  IF AN IDE CONTROLLER IS THERE, IT SHOULD ALWAYS
; RETURN SOMETHING OTHER THAN ZERO.  IF AN IDE CONTROLLER IS
; THERE, THEN THE VALUE WRITTEN TO PPI PORT A IS IGNORED
; BECAUSE THE WRITE SIGNAL IS NEVER PULSED.

        LDA     #$00
        STA     f:PPIDELO       ; PPI PORT A, DATALO

        JSR     IDE_WAIT_NOT_BUSY; WAIT FOR BUSY TO CLEAR
        BCS     PPIDE_PROBE_FAIL; IF TIMEOUT, REPORT NO IDE PRESENT
        LDA     #PPIDE_STATUS   ; GET STATUS
        JSR     IDE_READ
        TXA
        AND     #%01000000
        CMP     #$00
        BEQ     PPIDE_PROBE_FAIL; IF NOT RDY BIT (BIT 6) THEN REPORT NO IDE PRESENT

; CHECK SIGNATURE
        LDA     #PPIDE_SEC_CNT
        JSR     IDE_READ
        CPX     #$01
        BNE     PPIDE_PROBE_FAIL; IF not '01' THEN REPORT NO IDE PRESENT
        LDA     #PPIDE_LBALOW
        JSR     IDE_READ
        CPX     #$01
        BNE     PPIDE_PROBE_FAIL; IF not '01' THEN REPORT NO IDE PRESENT
        LDA     #PPIDE_LBAMID
        JSR     IDE_READ
        CPX     #$00
        BNE     PPIDE_PROBE_FAIL; IF not '00' THEN REPORT NO IDE PRESENT
        LDA     #PPIDE_LBAHI
        JSR     IDE_READ
        CPX     #$00
        BNE     PPIDE_PROBE_FAIL; IF not '00' THEN REPORT NO IDE PRESENT
        CLC
        JMP     PPIDE_PROBE_SUCCESS
PPIDE_PROBE_FAIL:
        SEC
PPIDE_PROBE_SUCCESS:
        RTS                     ; DONE, NOTE THAT A=0 AND Z IS SET


;___IDE_IDENTIFY_TYPE____________________________________________________________________________________
;
; 	READ THE DISK TYPE AND DETERMINE IF IT IS USABLE BY DOS/65
; 	A=IDE DEVICE (0=MST,1=SLV)
;________________________________________________________________________________________________________

IDE_IDENTIFY_TYPE:
        PHA
        JSR     IDE_WAIT_NOT_BUSY;MAKE SURE DRIVE IS READY
        BCS     IDE_IDENTIFY_TYPE_ERROR; IF TIMEOUT, REPORT ERROR
        LDA     #$00
        STA     debsehd
        STA     debcyll         ; STORE IN TRACK (lsb)
        STA     debcylm         ; STORE IN TRACK (msb)
        PLA                     ; GET DRIVE TYPE
        JSR     IDE_READ_SECTOR_DIRTY1
        CMP     #$FF            ; IS THERE A READ ERROR?
        BEQ     IDE_IDENTIFY_TYPE_ERROR
        LDA     f:LHSTBUF+$01FE
        CMP     #$55
        BNE     IDE_IDENTIFY_TYPE_OK
        LDA     f:LHSTBUF+$01FF
        CMP     #$AA
        BNE     IDE_IDENTIFY_TYPE_OK
        LDA     f:LHSTBUF+$01C2
        CMP     #$00
        BNE     IDE_IDENTIFY_TYPE_ERROR
        LDA     f:LHSTBUF+$01D2
        CMP     #$00
        BNE     IDE_IDENTIFY_TYPE_ERROR
        LDA     f:LHSTBUF+$01E2
        CMP     #$00
        BNE     IDE_IDENTIFY_TYPE_ERROR
        LDA     f:LHSTBUF+$01F2
        CMP     #$00
        BNE     IDE_IDENTIFY_TYPE_ERROR
IDE_IDENTIFY_TYPE_OK:
        LDA     #$00            ; EVERYTHING IS AWESOME
        RTS
IDE_IDENTIFY_TYPE_ERROR:
        LDA     #$FF            ; SIGNIFY ERROR
        RTS


;*__IDE_READ_INFO___________________________________________________________________________________
;*
;*  READ IDE INFORMATION
;*	CARRY SET ON ERROR
;* 	A=MST/SLV
;*____________________________________________________________________________________________________
IDE_READ_INFO:
        PRTDBG  "IDE Read INFORMATION$"
        PHA
        PHA
; SET DRIVE BIT
        AND     #$01            ; ONLY WANT THE 1 BIT (MST/SLV)
        ASL     a               ; SHIFT 4
        ASL     a               ;
        ASL     a               ;
        ASL     a               ;
        ORA     #$E0            ; E0=MST  F0=SLV
        TAX
        LDY     #$00
        LDA     #PPIDE_DEVICE
        JSR     IDE_WRITE


        JSR     IDE_WAIT_NOT_BUSY;MAKE SURE DRIVE IS READY
        BCS     IDE_READ_INFO_ABORT
        LDA     #PPIDE_COMMAND  ;SELECT IDE REGISTER
        LDX     #PPIDE_CMD_ID
        JSR     IDE_WRITE       ;ASK THE DRIVE TO READ IT
        JSR     IDE_WAIT_DRQ    ;WAIT UNTIL IT'S GOT THE DATA
        BCS     IDE_READ_INFO_ABORT
        JSR     IDE_READ_BUFFER ; GRAB THE 256 WORDS FROM THE BUFFER
        PRTS    "0x$"
        LDA     f:LHSTBUF+123
        JSR     PRTHEXBYTE
        LDA     f:LHSTBUF+122
        JSR     PRTHEXBYTE
        LDA     f:LHSTBUF+121
        JSR     PRTHEXBYTE
        LDA     f:LHSTBUF+120
        JSR     PRTHEXBYTE
        PLA
        JSR     IDE_IDENTIFY_TYPE
        CMP     #$00
        BNE     IDE_READ_INFO_BADFS
        JMP     IDE_READ_INFO_OK
IDE_READ_INFO_ABORT:
        PLA
        PRTS    "NOT PRESENT$"  ; NOT PRESENT
        JSR     NEWLINE
        PLA
        JSR     IDE_REMOVE_DRIVE_ASSIGNMENTS
        SEC
        RTS                     ;
IDE_READ_INFO_BADFS:
        PRTS    " BAD FILESYSTEM$"; NOT PRESENT
        JSR     NEWLINE
        PLA
        JSR     IDE_REMOVE_DRIVE_ASSIGNMENTS
        SEC
        RTS                     ;
IDE_READ_INFO_OK:
        PRTS    " FILE SYSTEM COMPATIBLE$"; NOT PRESENT
        JSR     NEWLINE
        PLA
        CLC
        RTS

IDE_REMOVE_DRIVE_ASSIGNMENTS:
        CLC
        ADC     #$30
        LDX     #$00
@1:
        CMP     dskcfg,X        ; GET device
        BEQ     @2
        INX
        INX
        CPX     #16
        BNE     @1
        JMP     @3
@2:
        PHA
        LDA     #$00
        STA     dskcfg,X        ; SET device
        INX
        STA     dskcfg,X        ; SET device
        PLA
        INX
        CPX     #16
        BNE     @1
@3:
        RTS

;__IDE_PPIDETECT____________________________________________________________________________________
;
;  PROBE FOR PPI HARDWARE
;____________________________________________________________________________________________________
;
IDE_PPIDETECT:
;
; TEST FOR PPI EXISTENCE
; WE SETUP THE PPI TO WRITE, THEN WRITE A VALUE OF ZERO
; TO PORT A (DATALO), THEN READ IT BACK.  IF THE PPI IS THERE
; THEN THE BUS HOLD CIRCUITRY WILL READ BACK THE ZERO. SINCE
; WE ARE IN WRITE MODE, AN IDE CONTROLLER WILL NOT BE ABLE TO
; INTERFERE WITH THE VALUE BEING READ.
        JSR     SET_PPI_WR
;
        LDA     #$00            ; VALUE ZERO
        STA     f:PPIDELO       ; PUSH VALUE TO PORT
        LDA     f:PPIDELO       ; GET PORT VALUE
        CMP     #$00
        RTS                     ; AND RETURN
;


;*__IDE_READ_SECTOR___________________________________________________________________________________
;*
;*  READ IDE SECTOR (IN LBA) INTO BUFFER
;*
;*____________________________________________________________________________________________________
IDE_READ_SECTOR:
        LDA     debsehd         ; STORE CURRENT PARMS
        CMP     Cdebsehd        ;
        BNE     IDE_READ_SECTOR_DIRTY
        LDA     debcylm         ;
        CMP     Cdebcylm        ;
        BNE     IDE_READ_SECTOR_DIRTY
        LDA     debcyll         ;
        CMP     Cdebcyll        ;
        BNE     IDE_READ_SECTOR_DIRTY
        LDA     #$00            ; ZERO = 1 ON RETURN = OPERATION OK
        RTS

IDE_READ_SECTOR_DIRTY:
        PRTDBG  "IDE Read Sector Buffer Dirty$"
        JSR     IDE_WAIT_NOT_BUSY;MAKE SURE DRIVE IS READY
        BCS     IDE_READ_SECTOR_DIRTY_ERROR; IF TIMEOUT, REPORT NO IDE PRESENT
IDE_READ_SECTOR_DIRTY1:
        JSR     IDE_SETUP_LBA   ;TELL IT WHICH SECTOR WE WANT
        LDA     #PPIDE_COMMAND  ;SELECT IDE REGISTER
        LDX     #PPIDE_CMD_READ
        JSR     IDE_WRITE       ;ASK THE DRIVE TO READ IT
        JSR     IDE_WAIT_DRQ    ;WAIT UNTIL IT'S GOT THE DATA
        BCS     IDE_READ_SECTOR_DIRTY_ERROR; IF TIMEOUT, REPORT NO IDE PRESENT
        JSR     IDE_READ_BUFFER ; GRAB THE 256 WORDS FROM THE BUFFER
        LDA     debsehd         ; STORE CURRENT PARMS
        STA     Cdebsehd        ;
        LDA     debcyll         ;
        STA     Cdebcyll        ;
        LDA     debcylm         ;
        STA     Cdebcylm        ;

        LDA     #$00            ; ZERO = 1 ON RETURN = OPERATION OK
        RTS
IDE_READ_SECTOR_DIRTY_ERROR:
        LDA     #$FF            ; ZERO = 1 ON RETURN = OPERATION OK
        RTS

;*__IDE_WRITE_SECTOR__________________________________________________________________________________
;*
;*  WRITE IDE SECTOR (IN LBA) FROM BUFFER
;*
;*____________________________________________________________________________________________________
IDE_WRITE_SECTOR:
        PRTDBG  "IDE Write Sector$"
        JSR     IDE_WAIT_NOT_BUSY;MAKE SURE DRIVE IS READY
        BCS     IDE_WRITE_SECTOR_ERROR; IF TIMEOUT, REPORT NO IDE PRESENT
IDE_WRITE_SECTOR_RAW:
        JSR     IDE_SETUP_LBA   ;TELL IT WHICH SECTOR WE WANT
        LDA     #PPIDE_COMMAND
        LDX     #PPIDE_CMD_WRITE
        JSR     IDE_WRITE       ;TELL DRIVE TO WRITE A SECTOR
        JSR     IDE_WAIT_DRQ    ;WAIT UNIT IT WANTS THE DATA
        BCS     IDE_WRITE_SECTOR_ERROR; IF TIMEOUT, REPORT NO IDE PRESENT
        JSR     IDE_WRITE_BUFFER;GIVE THE DATA TO THE DRIVE
        JSR     IDE_WAIT_NOT_BUSY;WAIT UNTIL THE WRITE IS COMPLETE
        BCS     IDE_WRITE_SECTOR_ERROR; IF TIMEOUT, REPORT NO IDE PRESENT
        LDA     #$FF            ; STORE CURRENT PARMS
        STA     Cdebsehd        ;
        STA     Cdebcyll        ;
        STA     Cdebcylm        ;

        LDA     #$00            ; ZERO ON RETURN = OPERATION OK
        RTS
IDE_WRITE_SECTOR_ERROR:
        LDA     #$FF            ; 1 ON RETURN = OPERATION FAIL
        RTS

;*__PPIDE_RESET____________________________________________________________________________________
;*
;*  SOFT RESET IDE CHANNEL
;*
;*____________________________________________________________________________________________________
PPIDE_RESET:
        PRTDBG  "IDE Reset$"
        LDA     #$00
        STA     debsehd
        STA     debcyll
        STA     debcylm
        LDA     #$FF            ;
        STA     Cdebsehd        ;
        STA     Cdebcyll        ;
        STA     Cdebcylm        ;

        LDA     #PPIDE_RST_LINE
        STA     f:PPIDECNTRL    ; ASSERT RST LINE ON IDE INTERFACE
        LDX     #$00
        PRTDBG  "IDE Reset Delay$"
RST_DLY:
        DEX
        CPX     #$00
        BNE     RST_DLY
        LDA     #$00
        STA     f:PPIDECNTRL    ; DEASSERT RST LINE ON IDE INTERFACE

; IF A DSKYNG IS ACTIVE AND IS ON THE SAME PPI PORT AS THE PPISD BEING
; RESET, THEN THE DSKYNG WILL ALSO BE RESET.  SO, THE DSKY IS ALSO INITIALIZED.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  JSR     DSKY_REINIT

        RTS


;*__IDE_WAIT_NOT_BUSY_______________________________________________________________________________
;*
;*  WAIT FOR IDE CHANNEL TO BECOME READY
;*
;*____________________________________________________________________________________________________
IDE_WAIT_NOT_BUSY:
        PHA
        TXA
        PHA
        TYA
        PHA
        LDA     #$00
        STA     FLRETRY
        STA     FLRETRY+1
IDE_WAIT_NOT_BUSY1:
        LDA     #PPIDE_STATUS   ;WAIT FOR RDY BIT TO BE SET
        JSR     IDE_READ
        TXA
        AND     #$80
        BEQ     IDE_WAIT_NOT_BUSY2
        INC     FLRETRY
        BNE     IDE_WAIT_NOT_BUSY1
        INC     FLRETRY+1
        BNE     IDE_WAIT_NOT_BUSY1
        SEC
        JMP     IDE_WAIT_NOT_BUSY3
IDE_WAIT_NOT_BUSY2:
        CLC
IDE_WAIT_NOT_BUSY3:
        PLA
        TAY
        PLA
        TAX
        PLA
        RTS

;*__IDE_WAIT_DRQ______________________________________________________________________________________
;*
;*	WAIT FOR THE DRIVE TO BE READY TO TRANSFER DATA.
;*
;*____________________________________________________________________________________________________
IDE_WAIT_DRQ:
        PHA
        TXA
        PHA
        TYA
        PHA
        LDA     #$00
        STA     FLRETRY
        STA     FLRETRY+1
IDE_WAIT_DRQ1:
        LDA     #PPIDE_STATUS   ;WAIT FOR DRQ BIT TO BE SET
        JSR     IDE_READ
        TXA
        AND     #%10001000      ; MASK OFF BUSY(7) AND DRQ(3)
        CMP     #%00001000      ; WE WANT BUSY(7) TO BE 0 AND DRQ (3) TO BE 1
        BEQ     IDE_WAIT_DRQ2
        AND     #%00000001      ; IS ERROR?
        CMP     #%00000001      ;
        BEQ     IDE_WAIT_DRQE
        INC     FLRETRY
        BNE     IDE_WAIT_DRQ1
        INC     FLRETRY+1
        BNE     IDE_WAIT_DRQ1
IDE_WAIT_DRQE:
        SEC
        JMP     IDE_WAIT_DRQ3
IDE_WAIT_DRQ2:
        CLC
IDE_WAIT_DRQ3:
        PLA
        TAY
        PLA
        TAX
        PLA
        RTS



;*__IDE_READ_BUFFER___________________________________________________________________________________
;*
;*  READ IDE BUFFER LITTLE ENDIAN
;*
;*____________________________________________________________________________________________________
IDE_READ_BUFFER:
        LDX     #$00            ; INDEX
IDEBUFRD:
        STX     PPIDEINDEX
        LDA     #PPIDE_DATA
        JSR     IDE_READ
        TXA
        LDX     PPIDEINDEX
        STA     f:LHSTBUF,X     ;
        INX                     ;
        TYA                     ; THEN HIGH BYTE OF WORD
        STA     f:LHSTBUF,X     ;
        INX
        CPX     #$00            ;
        BNE     IDEBUFRD        ;
IDEBUFRD1:
        STX     PPIDEINDEX
        LDA     #PPIDE_DATA
        JSR     IDE_READ
        TXA
        LDX     PPIDEINDEX
        STA     f:LHSTBUF+256,X ;
        INX                     ;
        TYA                     ; THEN HIGH BYTE OF WORD
        STA     f:LHSTBUF+256,X ;
        INX                     ;
        CPX     #$00            ;
        BNE     IDEBUFRD1       ;
        RTS                     ;

;*__IDE_WRITE_BUFFER___________________________________________________________________________________
;*
;*  WRITE IDE BUFFER LITTLE ENDIAN
;*
;*____________________________________________________________________________________________________
IDE_WRITE_BUFFER:
        LDX     #$00            ; INDEX
IDEBUFWT:
        STX     PPIDEINDEX
        LDA     f:LHSTBUF+1,X   ; SECTORS ARE BIG ENDIAN
        TAY                     ;
        LDA     f:LHSTBUF,X     ; SECTORS ARE BIG ENDIAN
        TAX
        LDA     #PPIDE_DATA
        JSR     IDE_WRITE
        LDX     PPIDEINDEX
        INX                     ;
        INX                     ;
        CPX     #$00            ;
        BNE     IDEBUFWT        ;
        LDX     #$00            ; INDEX
IDEBUFWT1:
        STX     PPIDEINDEX
        LDA     f:LHSTBUF+257,X ; SECTORS ARE BIG ENDIAN
        TAY
        LDA     f:LHSTBUF+256,X ; SECTORS ARE BIG ENDIAN
        TAX
        LDA     #PPIDE_DATA
        JSR     IDE_WRITE
        LDX     PPIDEINDEX
        INX                     ;
        INX                     ;
        CPX     #$00            ;
        BNE     IDEBUFWT1       ;
        RTS                     ;

;*__IDE_SETUP_LBA_____________________________________________________________________________________
;*
;*  SETUP LBA DATA
;*  A= DRIVE DEVICE
;*____________________________________________________________________________________________________
IDE_SETUP_LBA:
        PRTDBG  "PPIDE SETUP LBA$"
        LDA     CURRENT_IDE_DRIVE
        AND     #$01            ; only want drive cfg
        ASL     a               ; SHIFT 4
        ASL     a               ;
        ASL     a               ;
        ASL     a               ;
        ORA     #$E0            ; E0=MST  F0=SLV
        TAX
        LDY     #$00
        LDA     #PPIDE_DEVICE
        JSR     IDE_WRITE

        LDX     debcylm
        LDA     #PPIDE_LBAHI
        JSR     IDE_WRITE

        LDX     debcyll         ;
        LDA     #PPIDE_LBAMID
        JSR     IDE_WRITE

        LDX     debsehd         ;
        LDA     #PPIDE_LBALOW
        JSR     IDE_WRITE

        LDX     #$01
        LDA     #PPIDE_SEC_CNT
        JSR     IDE_WRITE

        RTS


;-------------------------------------------------------------------------------

; LOW LEVEL I/O TO THE DRIVE.  THESE ARE THE ROUTINES THAT TALK
; DIRECTLY TO THE DRIVE, VIA THE 8255 CHIP.  NORMALLY A MAIN
; PROGRAM WOULD NOT CALL TO THESE.

;DO A READ BUS CYCLE TO THE DRIVE, USING THE 8255.
;INPUT A = IDE REGSITER ADDRESS
;OUTPUT X = LOWER BYTE READ FROM IDE DRIVE
;OUTPUT Y = UPPER BYTE READ FROM IDE DRIVE

IDE_READ:
        JSR     SET_PPI_RD      ; SETUP FOR A READ CYCLE
        STA     f:PPIDECNTRL    ;DRIVE ADDRESS ONTO CONTROL LINES
        ORA     #PPIDE_RD_LINE  ; ASSERT RD PIN
        STA     f:PPIDECNTRL
        PHA
        LDA     f:PPIDELO       ; READ LOWER BYTE
        TAX
        LDA     f:PPIDEHI       ; READ UPPER BYTE
        TAY
        PLA
        EOR     #PPIDE_RD_LINE  ; DE-ASSERT RD SIGNAL
        STA     f:PPIDECNTRL
        LDA     #$00
        STA     f:PPIDECNTRL    ;DEASSERT ALL CONTROL PINS
        RTS




;DO A WRITE BUS CYCLE TO THE DRIVE, VIA THE 8255
;INPUT A = IDE REGISTER ADDRESS
;INPUT REGISTER X = LSB TO WRITE
;INPUT REGISTER Y = MSB TO WRITE
;


IDE_WRITE:
        JSR     SET_PPI_WR      ; SETUP FOR A WRITE CYCLE

        PHA
        TXA
        STA     f:PPIDELO       ; WRITE LOWER BYTE
        TYA
        STA     f:PPIDEHI       ; WRITE UPPER BYTE
        PLA

        STA     f:PPIDECNTRL    ;DRIVE ADDRESS ONTO CONTROL LINES

        ORA     #PPIDE_WR_LINE  ; ASSERT WRITE PIN
        STA     f:PPIDECNTRL

        EOR     #PPIDE_WR_LINE  ; DE ASSERT WR PIN
        STA     f:PPIDECNTRL

        LDA     #$00
        STA     f:PPIDECNTRL    ;DEASSERT ALL CONTROL PINS
        RTS


;-----------------------------------------------------------------------------------
; PPI SETUP ROUTINE TO CONFIGURE THE APPROPRIATE PPI MODE
;
;------------------------------------------------------------------------------------

SET_PPI_RD:
        PHA
        LDA     #PPRD_IDE_8255
        STA     f:PPIDEPPIC     ;CONFIG 8255 CHIP, READ MODE
        PLA
        RTS

SET_PPI_WR:
        PHA
        LDA     #PPWR_IDE_8255
        STA     f:PPIDEPPIC     ;CONFIG 8255 CHIP, WRITE MODE
        PLA
        RTS
