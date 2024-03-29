;__FLOPPY DRIVERS________________________________________________________________________________________________________________
;
; 	DOS/65 floppy drivers for direct attached disk-io V3 card
;
;	Entry points:
;		FL_SETUP        - called during OS init
;		FORMFL	        - format floppy disk
;		FL_READ_SECTOR  - read a sector from drive
;		FL_WRITE_SECTOR - write a sector to drive
;
;________________________________________________________________________________________________________________________________
;



;*
;* HARDWARE I/O ADDRESSES
;*

FMSR            = M6X0X_IOSPACE+$30; ADDRESS OF MAIN STATUS REGISTER
FDATA           = M6X0X_IOSPACE+$31; FLOPPY DATA REGISTER
FLATCH          = M6X0X_IOSPACE+$38; FLOPPY CONFIGURATION LATCH

;
; FDC CONFIGURATION LATCH OUTPUT BIT PATTERNS
MOTOR           = %00000010     ; BIT PATTERN IN LATCH FOR MOTOR CONTROL (ON)
TERMCN          = %00000001     ; BIT PATTERN IN LATCH TO WRITE A TC STROBE
RESETL          = %00000000     ; BIT PATTERN IN LATCH TO RESET ALL BITS
MINI            = %00000100     ; BIT PATTERN IN LATCH TO SET MINI MODE FDC9229 LOW DENS=1, HIGH DENS=0
PRECOMP         = %00100000     ; BIT PATTERN IN LATCH TO SET WRITE PRECOMP 125 NS:
FDDENSITY       = %01000000     ; BIT PATTERN IN LATCH TO FLOPPY LOW DENSITY (HIGH IS 1)
FDREADY         = %10000000     ; BIT PATTERN IN LATCH TO FLOPPY READY (P-34):

CFD_SENSEINT    = %00001000     ; CMD --> ST0,PCN

FLOPPY_RETRIES  = 6             ; HOW ABOUT SIX RETIRES?
FLOPPY_RETRIES1 = 2             ; TWO ITERATIONS OF RECAL?


;__SETUPDRIVE__________________________________________________________________________________________________________________________
;
;	SETUP FLOPPY DRIVE SETTINGS
;________________________________________________________________________________________________________________________________
;
;
;
FL_SETUP:
        LDA     #$00
        STA     debcyll
        STA     debcylm
        STA     debsehd
        LDA     #$FF
        STA     Cdebcyll
        STA     Cdebcylm
        STA     Cdebsehd
        PRTS    "FD: MODE=DIOV3$"
;
        PRTS    " IO=0x$"
        LDA     #>FMSR
        JSR     PRINT_BYTE      ; PRINT BASE PORT
        LDA     #<FMSR
        JSR     PRINT_BYTE      ; PRINT BASE PORT
        JSR     FD_DETECT       ; CHECK FOR FDC
        CMP     #$00
        BEQ     :+              ; CONTINUE IF FOUND
        PRTS    " NOT PRESENT$" ; NOT ZERO, H/W NOT PRESENT
        JSR     NEWLINE
        LDA     #$FF
        RTS                     ; BAIL OUT
:
        PRTS    " PRESENT$"     ; NOT ZERO, H/W NOT PRESENT
        JSR     NEWLINE

        LDA     #$00
        STA     FLATCH
        LDA     #RESETL         ; RESET SETTINGS
        ORA     #MINI           ; SELECT MINI FLOPPY (low dens=1, high dens=0)
        ORA     #PRECOMP        ; SELECT PRECOMP
        ORA     #FDDENSITY      ; SELECT HIGH DENSITY
        ORA     #FDREADY        ;
        STA     FLATCH_STORE    ; SAVE SETTINGS
        STA     FLATCH
        JSR     CHECKINT        ;
        LDA     #$03            ; SPECIFY COMMAND
        JSR     PFDATA          ; OUTPUT TO FDC
        LDA     #$7F            ; 6 MS STEP, 480 MS HEAD UNLOAD
        JSR     PFDATA          ; OUTPUT TO FDC
        LDA     #$05            ; 508 MS HEAD LOAD, NON-DMA MODE
        JSR     PFDATA          ; OUTPUT TO FDC
        JSR     CHECKINT        ;
        JSR     CHECKINT        ;
        JSR     CHECKINT        ;
        JSR     CHECKINT        ;
        JSR     CHECKINT        ;
        JSR     CHECKINT        ;
        LDA     #$00            ; SAY WHICH UNIT
        STA     DSKUNIT         ; SAY WHICH UNIT
        JSR     RECAL           ;
        LDA     #39             ;
        STA     debcyll         ;
        JSR     SETTRK1
        JSR     RECAL           ;
        LDA     #$01            ; SAY WHICH UNIT
        STA     DSKUNIT         ; SAY WHICH UNIT
        JSR     RECAL           ;
        LDA     #39             ;
        STA     debcyll         ;
        JSR     SETTRK1
        JMP     RECAL           ;

;__OUTFLATCH__________________________________________________________________________________________________________________________
;
;	SEND SETTINGS TO FLOPPY CONTROLLER
;________________________________________________________________________________________________________________________________
;
OUTFLATCH:
        LDA     FLATCH_STORE    ; SET A TO SETTINGS
        STA     FLATCH          ; OUTPUT TO CONTROLLER
        RTS


;__READFL________________________________________________________________________________________________________________________
;
; 	READ A FLOPPY SECTOR
;________________________________________________________________________________________________________________________________
;
FL_READ_SECTOR:
        LDA     FLOPPY_DETCT
        CMP     #$00
        BEQ     :+
        RTS
:
        LDA     FLATCH_STORE    ; POINT TO FLATCH
        ORA     #%00000010      ; SET MOTOR ON
        STA     FLATCH_STORE    ; POINT TO FLATCH
        JSR     OUTFLATCH       ; OUTPUT TO CONTROLLER

        LDA     debcylm         ;
        CMP     Cdebcylm        ;
        BNE     READFL_DIRTY
        LDA     debcyll         ;
        CMP     Cdebcyll        ;
        BNE     READFL_DIRTY
        LDA     debsehd         ;
        CMP     Cdebsehd        ;
        BNE     READFL_DIRTY
        LDA     sekdsk          ;
        CMP     currentDrive
        BNE     READFL_DIRTY
        LDA     #$00
        RTS


READFL_DIRTY:

        LDA     debcyll         ;  STORE CURRENT PARMS
        STA     Cdebcyll        ;
        LDA     debsehd         ;
        STA     Cdebsehd        ;
        LDA     debcylm         ;
        STA     Cdebcylm        ;


        LDA     #$00
        STA     FLRETRY
        STA     FLRETRY1
READFL1:
        LDA     #$46            ; BIT 6 SETS MFM, 06H IS READ COMMAND
        STA     FCMD
        JSR     DSKOP
        CMP     #$00
        BEQ     READFLDONE
        INC     FLRETRY
        LDA     FLRETRY
        CMP     #FLOPPY_RETRIES
        BNE     READFL1
        JSR     RECAL
        JSR     SETTRACK
        LDA     #$00
        STA     FLRETRY
        INC     FLRETRY1
        LDA     FLRETRY1
        CMP     #FLOPPY_RETRIES1
        BNE     READFL1

        LDA     #$FF
        STA     Cdebcyll        ;
        STA     Cdebsehd        ;
        STA     Cdebcylm        ;
        RTS                     ; A = $FF ON RETURN = OPERATION ERROR
READFLDONE:
        LDA     #$00            ; A = 0 ON RETURN = OPERATION OK
        RTS

;__WRITEFL________________________________________________________________________________________________________________________
;
; 	WRITE A FLOPPY SECTOR
;________________________________________________________________________________________________________________________________
;
FL_WRITE_SECTOR:
        LDA     FLOPPY_DETCT
        CMP     #$00
        BEQ     :+
        RTS
:
        LDA     FLATCH_STORE    ; POINT TO FLATCH
        ORA     #%00000010      ; SET MOTOR ON
        STA     FLATCH_STORE    ; POINT TO FLATCH
        JSR     OUTFLATCH       ; OUTPUT TO CONTROLLER

        LDA     #$FF
        STA     Cdebcyll        ;
        STA     Cdebcylm        ;
        STA     Cdebsehd        ;
        LDA     #$00
        STA     FLRETRY
        STA     FLRETRY1
WRITEFL1:
        LDA     #$45            ; BIT 6 SETS MFM, 05H IS WRITE COMMAND
        STA     FCMD
        JSR     DSKOP
        CMP     #$00
        BEQ     WRITEFLDONE
        INC     FLRETRY
        LDA     FLRETRY
        CMP     #FLOPPY_RETRIES
        BNE     WRITEFL1
        JSR     RECAL
        JSR     SETTRACK
        LDA     #$00
        STA     FLRETRY
        INC     FLRETRY1
        LDA     FLRETRY1
        CMP     #FLOPPY_RETRIES1
        BNE     WRITEFL1
        LDA     #$FF            ;
        STA     Cdebcyll        ;
        STA     Cdebcylm        ;
        STA     Cdebsehd        ;
        RTS                     ; A = $FF ON RETURN = OPERATION ERROR
WRITEFLDONE:
        LDA     #$00            ; A = 0 ON RETURN = OPERATION OK
        RTS


;__DSKOP__________________________________________________________________________________________________________________________
;
; 	PERFORM A DISK OPERATION
;________________________________________________________________________________________________________________________________
;

DSKOP:
        SEI
        JSR     CHECKINT        ; CHECK INTERRUPT STATUS, MAKE SURE IT IS CLEAR
        CMP     #$FF            ; DID IT RETURN WITH ERROR CODE?
        BEQ     DSKEXIT         ; IF YES, EXIT WITH ERROR CODE
;
;
        LDA     FLATCH_STORE    ; POINT TO FLATCH
        ORA     #%00000010      ; SET MOTOR ON
        STA     FLATCH_STORE    ; POINT TO FLATCH
        JSR     OUTFLATCH       ; OUTPUT TO CONTROLLER
;
        JSR     SETTRACK        ; PERFORM SEEK TO TRACK
;
        LDA     FCMD            ; WHAT COMMAND IS PENDING?
        CMP     #$46            ; IS IT A READ COMMAND?
        BNE     GWRR_POLL       ;
        JMP     RDD_POLL        ;
GWRR_POLL:
        JMP     WRR_POLL        ;
DSKEXIT:
        LDA     FLATCH_STORE    ; POINT TO FLATCH
        AND     #%11111101      ; SET MOTOR OFF
        STA     FLATCH_STORE    ; POINT TO FLATCH
        JSR     OUTFLATCH       ; OUTPUT TO CONTROLLER
        LDA     #$FF            ; SET -1 IF ERROR
        CLI
        RTS

SNDFDWR:
        LDY     #$00            ; BYTES/SECTOR COUNT
        CLC
        LDA     DSKUNIT         ; GET DISK UNIT NUMBER
        AND     #$01            ; MASK FOR TWO DRIVES.
        STA     slicetmp        ; PARK IT IN TEMP
        LDA     debcylm         ; GET HEAD SELECTION
        AND     #$01            ; INSURE SINGLE BIT
        ASL     A               ;
        ASL     A               ; MOVE HEAD TO BIT 2 POSITION
        ORA     slicetmp        ; OR HEAD TO UNIT BYTE IN COMMAND BLOCK
        STA     slicetmp        ; STORE IN UNIT
        LDA     FCMD            ;
        JSR     PFDATA          ; PUSH COMMAND TO I8272
        LDA     slicetmp        ;
        JSR     PFDATA          ;
        LDA     debcyll         ;
        JSR     PFDATA          ;
        LDA     debcylm         ; GET HEAD SELECTION
        AND     #$01            ; INSURE SINGLE BIT
        JSR     PFDATA          ;
        CLC                     ;
        LDA     debsehd         ;
        ADC     #$01            ;
        JSR     PFDATA          ;
        LDA     #$02            ;
        JSR     PFDATA          ; WHAT DENSITY
        LDA     #$09            ;
        JSR     PFDATA          ; ASSUME SC (SECTOR COUNT)  EOT
        LDA     #$1B            ;
        JSR     PFDATA          ; WHAT GAP IS NEEDED
        LDA     #$FF            ; DTL, IS THE LAST COMMAND BYTE TO I8272
        JSR     PFDATAS
        RTS
; PERFORM READ
; FROM READ TO READ MUST NOT EXCEED 25US WORST CASE MIN.
;
RDD_POLL:
        JSR     SNDFDWR         ;
RDS1:
        LDA     FMSR            ; GET STATUS
        BPL     RDS1
        AND     #%00100000      ; EXECUTION MODE?
        BEQ     DSKOPEND        ; NO, ERROR
        LDA     FDATA           ; GET DATA
        STA     hstbuf,Y        ; WRITE IT
        INY
        BNE     RDS1            ; KEEP GOING
RDS2:
        LDA     FMSR            ; GET STATUS
        BPL     RDS2
        AND     #%00100000      ; EXECUTION MODE?
        BEQ     DSKOPEND        ; NO, ERROR
        LDA     FDATA           ; GET DATA
        STA     hstbuf+256,Y    ; WRITE IT
        INY
        BNE     RDS2            ; KEEP GOING

DSKOPEND:
        LDA     FLATCH_STORE    ; POINT TO FLATCH
        ORA     #%00000001      ;
        STA     FLATCH_STORE    ; SET TC
        JSR     OUTFLATCH       ; OUTPUT TO CONTROLLER
        NOP                     ;
        NOP                     ; 2 MICROSECOND DELAY
        NOP                     ;
        NOP                     ;
        LDA     FLATCH_STORE    ; POINT TO FLATCH
        AND     #%11111110      ;
        STA     FLATCH_STORE    ; CLEAR TC
        JSR     OUTFLATCH       ; OUTPUT TO CONTROLLER
        PHA                     ;
        PLA                     ;
        PHA                     ;
        PLA                     ; 2 MICROSECOND DELAY
        LDA     FLATCH_STORE    ; POINT TO FLATCH
        AND     #%11111101      ; SET MOTOR OFF
        STA     FLATCH_STORE    ; POINT TO FLATCH
        JSR     OUTFLATCH       ; OUTPUT TO CONTROLLER					;
;
        JSR     GFDATA          ;GET ERROR TYPE
        STA     FLERR

;* CLEAR OUT ANY REMAINING DATA
RESUL3:
        JSR     GFDATA          ;READ BYTE FROM FDC
        CMP     #$00
        BNE     RESUL3          ;CLEAR THEM ALL
        LDA     FLERR           ;
        AND     #%11000000      ;
        RTS

WRR_POLL:
        JSR     SNDFDWR         ;
WRS1:   ;
        LDA     FMSR            ; GET STATUS
        BPL     WRS1            ; NOT READY
        AND     #%00100000      ; EXECUTION MODE?
        BEQ     WRS3            ; NO, ERROR
        LDA     hstbuf,Y        ; WRITE IT
        STA     FDATA           ; WRITE TO FDC
        INY
        BNE     WRS1            ; DO NEXT
WRS2:   ;
        LDA     FMSR            ; GET STATUS
        BPL     WRS2            ; NOT READY
        AND     #%00100000      ; EXECUTION MODE?
        BEQ     WRS3            ; NO, ERROR
        LDA     hstbuf+256,Y    ; WRITE IT
        STA     FDATA           ; WRITE TO FDC
        INY
        BNE     WRS2            ; DO NEXT
WRS3:
        JMP     DSKOPEND        ;


;__SETTRACK__________________________________________________________________________________________________________________________
;
; 	SEEK TO A TRACK ON GIVEN UNIT
; 	A: TRACK #
;________________________________________________________________________________________________________________________________
;
SETTRACK:
        LDA     FLATCH_STORE    ; POINT TO FLATCH
        ORA     #%00000010      ; SET MOTOR ON
        STA     FLATCH_STORE    ; POINT TO FLATCH
        JSR     OUTFLATCH       ; OUTPUT TO CONTROLLER

; ANY INTERUPT PENDING
; IF YES FIND OUT WHY/CLEAR
        JSR     CHECKINT        ; CHECK INTERRUPT STATUS, MAKE SURE IT IS CLEAR
        CMP     #$FF            ; DID IT RTSURN WITH ERROR CODE?
        BNE     SETTRK1
        JMP     SETTRKEXIT      ;

;
SETTRK1:
        LDA     debcyll         ; GET TRACK
        CMP     #$00            ;
        BEQ     RECAL           ; IF 0 PERFORM RECAL INSTEAD OF SEEK
        LDA     #$0F            ; SEEK COMMAND
        JSR     PFDATA          ; PUSH COMMAND
        LDA     DSKUNIT         ; SAY WHICH UNIT
        AND     #$01
        JSR     PFDATA          ; SEND THAT
        LDA     debcyll         ; TO WHAT TRACK
        JSR     PFDATA          ; SEND THAT TOO
        JMP     WAINT           ; WAIT FOR INTERRUPT SAYING DONE
RECAL:
        LDA     FLATCH_STORE    ; POINT TO FLATCH
        ORA     #%00000010      ; SET MOTOR ON
        STA     FLATCH_STORE    ; POINT TO FLATCH
        JSR     OUTFLATCH       ; OUTPUT TO CONTROLLER

        LDA     #$07            ; RECAL TO TRACK 0
        JSR     PFDATA          ; SEND IT
        LDA     DSKUNIT         ; WHICH UNIT
        AND     #$01
        JSR     PFDATA          ; SEND THAT TOO
;
WAINT:
        PHA
        TXA
        PHA
        LDX     #100
        JSR     FDVDELAY
        PLA
        TAX
        PLA
;
SETTRK2:
        JSR     CHECKINT
        LDA     FMSR            ; READ SEEK STATUS
        AND     #%00001111      ; ANY DRIVES SEEKING?
        BNE     SETTRK2         ; YES, WAIT FOR THEM
;
SETTRKEXIT:
        RTS


;__PFDATA__________________________________________________________________________________________________________________________
;
; WRITE A COMMAND OR PARAMETER SEQUENCE
;
; TRANSFERS ARE SYNCHONIZED BY MSR D7 <RQM> AND D6 <DIO>
;	RQM  DIO
;	0	0	BUSY
;	1	0	WRITE TO DATA REGISTER PERMITTED
;	1	1	BYTE FOR READ BY HOST PENDING
;	0	1	BUSY
;
;________________________________________________________________________________________________________________________________
;
PFDATA:
        PHA                     ; SAVE DATA BYTE
        LDY     #$00
WRF1:
        LDA     FMSR            ; READ FDC STATUS
        TAX
        AND     #$80            ;
        BNE     :+
        INY
        BNE     WRF1            ; FDC IS NOT READY, WAIT FOR IT
        PLA
        LDA     #$FF
        RTS
:
        TXA
        AND     #$40            ; TEST DIO BIT
        BNE     WRF2            ; FDC IS OUT OF SYNC
        PLA                     ; RESTORE DATA
        STA     FDATA           ; WRITE TO FDC
        JSR     FDDELAY
        JSR     FDDELAY
        JSR     FDDELAY
        RTS
; FDC IS OUT OF SYNC CLEAR IT OUT AND RE-TRY
WRF2:
        LDA     FDATA           ; READ DATA REGISTER
        JMP     WRF1            ; AND CONTINUE

;__PFDATAS_________________________________________________________________________________________________________________________
;
; WRITE A COMMAND OR PARAMETER SEQUENCE (NO PAUSE)
;
; TRANSFERS ARE SYNCHONIZED BY MSR D7 <RQM> AND D6 <DIO>
;	RQM  DIO
;	0	0	BUSY
;	1	0	WRITE TO DATA REGISTER PERMITTED
;	1	1	BYTE FOR READ BY HOST PENDING
;	0	1	BUSY
;
;________________________________________________________________________________________________________________________________
;
PFDATAS:
        PHA                     ; SAVE DATA BYTE
WRF1S:
        LDA     FMSR            ; READ FDC STATUS
        TAX
        AND     #$80            ;
        BEQ     WRF1S           ; FDC IS NOT READY, WAIT FOR IT
        TXA
        AND     #$40            ; TEST DIO BIT
        BNE     WRF2S           ; FDC IS OUT OF SYNC
        PLA                     ; RESTORE DATA
        STA     FDATA           ; WRITE TO FDC
        RTS
; FDC IS OUT OF SYNC CLEAR IT OUT AND RE-TRY
WRF2S:
        LDA     FDATA           ; READ DATA REGISTER
        JMP     WRF1S           ; AND CONTINUE



;__CHECKINT__________________________________________________________________________________________________________________________
;
; CHECK FOR ACTIVE FDC INTERRUPTS BEFORE GIVING I8272 COMMANDS
; POLL RQM FOR WHEN NOT BUSY AND THEN SEND FDC
; SENSE INTERRUPT COMMAND.  IF IT RTSURNS WITH NON ZERO
; ERROR CODE, PASS BACK TO JSRING ROUTINE FOR HANDLING
;________________________________________________________________________________________________________________________________
;
CHECKINT:
        LDY     #$00
:
        LDA     FMSR            ; READING OR WRITING IS KEYS TO D7 RQM
        AND     #$80
        BNE     :+              ; WAIT FOR RQM TO BE TRUE. WAIT UNTIL DONE
        JSR     FDDELAY
        INY
        BNE     :-
        JMP     ERRCLR

:
        LDA     FMSR            ; READING OR WRITING IS KEYS TO D7 RQM
        AND     #$40            ; WAITING FOR INPUT?
        BEQ     SENDINT
        RTS

ERRCLR:
        LDY     #$00
:
        LDA     FDATA           ; CLEAR THE JUNK OUT OF DATA REGISTER
        LDA     FMSR            ; CHECK WITH RQM
        AND     #$80            ; IF STILL NOT READY, READ OUT MORE JUNK
        BNE     :+              ;
        JSR     FDDELAY
        INY
        BNE     :-
:
        LDA     #$FF            ; RETURN ERROR CODE -1
;
        RTS

;__SENDINT__________________________________________________________________________________________________________________________
;
; SENSE INTERRUPT COMMAND
;________________________________________________________________________________________________________________________________
;
SENDINT:
        LDA     #CFD_SENSEINT   ; SENSE INTERRUPT COMMAND
        JSR     PFDATA          ; SEND IT
        JSR     GFDATA          ; GET RESULTS
        STA     ST0             ; STORE THAT
        AND     #$C0            ; MASK OFF INTERRUPT STATUS BITS
        CMP     #$80            ; CHECK IF INVALID COMMAND
        BEQ     ENDSENDINT      ; YES, EXIT
        JSR     GFDATA          ; GET ANOTHER (STATUS CODE 1)
        LDA     ST0             ; GET FIRST ONE
        AND     #$C0            ; MASK OFF ALL BUT INTERRUPT CODE 00 IS NORMAL
ENDSENDINT:
        RTS                     ; ANYTHING ELSE IS AN ERROR


;__GFDATA__________________________________________________________________________________________________________________________
;
; GET DATA FROM FLOPPY CONTROLLER
;
; TRANSFERS ARE SYNCHONIZED BYT MSR D7 <RQM> AND D6 <DIO>
;	RQM  DIO
;	0	0	BUSY
;	1	0	WRITE TO DATA REGISTER PERMITTED
;	1	1	BYTE FOR READ BY HOST PENDING
;	0	1	BUSY
;
;________________________________________________________________________________________________________________________________
;
GFDATA:
        LDY     #$00
:
        LDA     FMSR            ; GET STATUS
        TAX                     ;
        AND     #%10000000      ; NOT READY, WAIT
        BNE     :+              ;
        INY
        BNE     :-
        LDA     #$00
        RTS
:
        TXA
        AND     #%01000000      ; ANY DATA FOR US?
        BEQ     GFDATA1         ; NO, SKIP IT
        LDA     FDATA           ; GET FDC DATA
GFDATA1:
        RTS


;__FD_DETECT______________________________________________________________________________________________________________________
;
; 	DETECT FLOPPY HARDWARE
;________________________________________________________________________________________________________________________________
FD_DETECT:
; BLINDLY RESET FDC (WHICH MAY OR MAY NOT EXIST)
        JSR     FC_RESETFDC     ; RESET FDC

        LDA     FMSR            ; READ MSR
        CMP     #$80
        BEQ     FD_DETECT1      ; $80 IS OK
        CMP     #$D0
        BEQ     FD_DETECT1      ; $D0 IS OK
        LDA     #$FF            ; NOT OK
        STA     FLOPPY_DETCT
        RTS
;
FD_DETECT1:
        LDX     #100
        JSR     FDVDELAY        ; WAIT A BIT FOR FDC
        LDA     FMSR            ; READ MSR AGAIN
        CMP     #$80
        BEQ     :+              ; $80 IS OK
        CMP     #$D0
        LDA     #$FF            ; NOT OK
        STA     FLOPPY_DETCT
        RTS
:
        LDA     #$00            ; OK
        STA     FLOPPY_DETCT
        RTS


FC_RESETFDC:
        LDA     #$00
        STA     FLATCH
        LDX     #150
        JSR     FDVDELAY        ; WAIT A BIT FOR FDC
        LDA     #FDREADY        ;
        STA     FLATCH
        LDX     #150            ;
        JSR     FDVDELAY
        RTS


FDDELAY:
        PHA
        PLA
        PHA
        PLA
        RTS
FDVDELAY:
        PHA
        PLA
        PHA
        PLA
        DEX
        CPX     #$00
        BNE     FDVDELAY
        RTS
