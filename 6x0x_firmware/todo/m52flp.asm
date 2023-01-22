	TITLE	FLOPPY DRIVERS

;__FLOPPY DRIVERS________________________________________________________________________________________________________________
;
; 	CUBIX floppy disk drivers for direct attached disk-io card
;
;	Entry points:
;		SETUPDRIVE  - called during OS init
;		FORMFL	    - format floppy disk ('U' POINTS TO DCB)
;		READFL	    - read a sector from drive ('U' POINTS TO DCB, X TO MEMORY)
;		WRITEFL	    - write a sector to drive   ('U' POINTS TO DCB, X TO MEMORY)
;________________________________________________________________________________________________________________________________
;
	
	
	
*
* HARDWARE I/O ADDRESSES
*
FMSR		EQU	$F136		; ADDRESS OF MAIN STATUS REGISTER
FDATA		EQU	$F137		; FLOPPY DATA REGISTER
FLATCH		EQU	$F13A		; FLOPPY CONFIGURATION LATCH


;
; FDC CONFIGURATION LATCH OUTPUT BIT PATTERNS
MOTOR		EQU	%00000000	; BIT PATTERN IN LATCH FOR MOTOR CONTROL (ON)
TERMCN		EQU	%00000001	; BIT PATTERN IN LATCH TO WRITE A TC STROBE
RESETL		EQU	%00000010	; BIT PATTERN IN LATCH TO RESET ALL BITS
MINI		EQU	%00000100	; BIT PATTERN IN LATCH TO SET MINI MODE FDC9229 LOW DENS=1, HIGH DENS=0
PRECOMP		EQU	%00100000	; BIT PATTERN IN LATCH TO SET WRITE PRECOMP 125 NS:
FDDENSITY	EQU	%01000000	; BIT PATTERN IN LATCH TO FLOPPY LOW DENSITY (HIGH IS 0)
FDREADY		EQU	%10000000	; BIT PATTERN IN LATCH TO FLOPPY READY (P-34):




;__SETUPDRIVE__________________________________________________________________________________________________________________________ 
;
;	SETUP FLOPPY DRIVE SETTINGS 
;________________________________________________________________________________________________________________________________
;
;
;
SETUPDRIVE:
	LDAA	#RESETL			; RESET SETTINGS
	ORAA	#MINI			; SELECT MINI FLOPPY (low dens=1, high dens=0)
	ORAA	#PRECOMP		; SELECT PRECOMP 
	ORAA	#FDDENSITY		; SELECT DENSITY
	ORAA	#FDREADY		; SELECT READY SIGNAL
	STAA	FLATCH_STORE		; SAVE SETTINGS
	JSR	CHECKINT		;
	LDA	#$03			; SPECIFY COMMAND
	JSR	PFDATA			; OUTPUT TO FDC
	LDA	#$7F			; 6 MS STEP, 480 MS HEAD UNLOAD
	JSR	PFDATA			; OUTPUT TO FDC
	LDA	#$05			; 508 MS HEAD LOAD, NON-DMA MODE
	JSR	PFDATA			; OUTPUT TO FDC
	JSR	CHECKINT		;
	PSHS	A			;
	JMP	RECAL			;

;__OUTFLATCH__________________________________________________________________________________________________________________________ 
;
;	SEND SETTINGS TO FLOPPY CONTROLLER
;________________________________________________________________________________________________________________________________
;
OUTFLATCH:
	LDAA	FLATCH_STORE		; SET A TO SETTINGS
	STAA	FLATCH			; OUTPUT TO CONTROLLER
	RTS

	
;__FORMFL________________________________________________________________________________________________________________________ 
;
; 	FORMFL 	
;________________________________________________________________________________________________________________________________
;	
FORMFL:	
	CLR	TCYL			; START WITH CYLINDER 0
* FORMAT ONE CYLINDER
FORCYL	LDAA	FLATCH_STORE		; POINT TO FLATCH
	ANDA    #%11111101		;
	STAA	FLATCH_STORE		; POINT TO FLATCH
	BSR	OUTFLATCH		; OUTPUT TO CONTROLLER	
	JSR	CHECKINT		; CHECK INTERRUPT STATUS, MAKE SURE IT IS CLEAR
	JSR	SETTRACK		; MOVE TO THE CYLINDER
	CLR	THEAD			; START WITH BOTTOM HEAD
	
*  FORMAT ONE HEAD

FORHED	LDB	#$00			; SET SECTOR ID
	LDA	#$4D			; FORMAT COMMAND
	JSR	PFDATA			; OUTPUT TO FDC
	LDA	THEAD			; GET CURRENT HEAD
	ASLA				; SHIFT INTO...
	ASLA				; PROPER POSITION
	ORA	DRIVE,U			; INCLUDE DRIVE
	JSR	PFDATA			; OUTPUT TO FDC
	LDA	#$02			; 512 BYTES/SECTOR
	JSR	PFDATA			; OUTPUT TO FDC
	LDA	NSEC,U			; SECTORS/TRACK
	JSR	PFDATA			; OUTPUT TO FDC
	LDA	#$2A			; GAP LENGTH ($38 FOR 10 S/TRK)
	JSR	PFDATA			; OUTPUT TO FDC
	LDA	#$E5			; FILLER BYTE
	JSR	PFDATA			; OUTPUT TO FDC
	
* FORMAT ONE SECTOR

FORSEC	LDAA	>FMSR			; READ FDC STATUS
	BPL	FORSEC			; WAIT TILL READY FOR DATA
	BITA	#%00100000		; 
	BEQ	DSKFMTEND		;
	LDAA	TCYL			; GET CYLINDER ID
	STAA	>FDATA			; WRITE TRACK TO FDC
FORS2	LDAA	>FMSR			; READ FDC STATUS
	BPL	FORS2			; WAIT TILL READY FOR DATA
	BITA	#%00100000		; 
	BEQ	DSKFMTEND		;
	LDAA	THEAD			; GET HEAD ID
	STAA	>FDATA			; WRITE HEAD TO FDC
FORS3	LDAA	>FMSR			; READ FDC STATUS
	BPL	FORS3			; WAIT TILL READY FOR DATA
	BITA	#%00100000		; 
	BEQ	DSKFMTEND		;
	INCB				; ADD 1
	STAB	>FDATA			; WRITE SECTOR TO FDC
FORS5	LDAA	>FMSR			; READ FDC STATUS
	BPL	FORS5			; WAIT TILL READY FOR DATA
	BITA	#%00100000		; 
	BEQ	DSKFMTEND		;
	LDAA	#$02			; 512 BYTES/SECTOR
	STAA	>FDATA			; WRITE DATA BYTE	
	CMPB	#$09			; OVER?
	BNE	FORSEC			; NO, ITS OK
	JSR	DSKOPEND		; EVERYTHING OK?
	INC	THEAD			; MOVE TO NEXT HEAD
	LDAA	THEAD			; GET VALUE
	CMPA	NHEAD,U			; HAVE WE DONE EM ALL?
	BLO	FORHED			; NO, DO NEXT HEAD
	INC	TCYL			; MOVE TO NEXT CYLINDER
	LDAA	TCYL			; GET VALUE
	STAA	CYL,U			; 
	CMPA	NCYL,U			; HAVE WE DONE EM ALL?
	BLO	FORCYL1			; NO, DO NEXT CYLINDER	
	LDAA	FLATCH_STORE		; POINT TO FLATCH
	ORAA    #%00000010		;
	STAA	FLATCH_STORE		; POINT TO FLATCH
	JSR	OUTFLATCH		; OUTPUT TO CONTROLLER					;
	RTS
DSKFMTEND:
	JMP	DSKOPEND		; EVERYTHING OK?
FORCYL1:
	JMP	FORCYL			;

;__READFL________________________________________________________________________________________________________________________ 
;
; 	READ A FLOPPY SECTOR 	
;________________________________________________________________________________________________________________________________
;	
READFL:
	LDAA	#$46			; BIT 6 SETS MFM, 06H IS READ COMMAND
	STAA	FCMD
	JMP	DSKOP

;__WRITEFL________________________________________________________________________________________________________________________ 
;
; 	WRITE A FLOPPY SECTOR 	
;________________________________________________________________________________________________________________________________
;	
WRITEFL:
	LDAA	#$45			; BIT 6 SETS MFM, 05H IS WRITE COMMAND
	STAA	FCMD
	JMP	DSKOP
;__DSKOP__________________________________________________________________________________________________________________________ 
;
; 	PERFORM A DISK OPERATION 	
;________________________________________________________________________________________________________________________________
;		
DSKOP:
	JSR	CHECKINT		; CHECK INTERRUPT STATUS, MAKE SURE IT IS CLEAR
	CMPA	#$FF			; DID IT RTSURN WITH ERROR CODE?
	BEQ	DSKEXIT			; IF YES, EXIT WITH ERROR CODE
					;	
 					;
	LDAA	FLATCH_STORE		; POINT TO FLATCH
	ANDA	#%11111101		; SET MOTOR ON
	STAA	FLATCH_STORE		; POINT TO FLATCH	
	JSR	OUTFLATCH		; OUTPUT TO CONTROLLER
					;
	JSR	SETTRACK		; PERFORM SEEK TO TRACK
					;
	LDAA	FCMD			; WHAT COMMAND IS PENDING?
	CMPA	#$46			; IS IT A READ COMMAND?
	LBNE	WRR_POLL		;
	JMP	RDD_POLL		;
DSKEXIT:	
	LDAA	FLATCH_STORE		; POINT TO FLATCH
	ORAA	#%00000010		; SET MOTOR OFF
	STAA	FLATCH_STORE		; POINT TO FLATCH	
	JSR	OUTFLATCH		; OUTPUT TO CONTROLLER
	LDAA	#$FF			; SET -1 IF ERROR
	RTS
SNDFDWR:
	LDY	#512			; BYTES/SECTOR COUNT
	LDAA	DRIVE,U			; GET DISK UNIT NUMBER
	ANDA	#$03			; MASK FOR FOUR DRIVES.
	STAA	UNIT			; PARK IT IN TEMP
	LDAA	HEAD,U			; GET HEAD SELECTION
	ANDA	#$01			; INSURE SINGLE BIT
	ASLA				;
	ASLA				; MOVE HEAD TO BIT 2 POSITION
	ORAA	UNIT			; OR HEAD TO UNIT BYTE IN COMMAND BLOCK
	STAA	UNIT			; STORE IN UNIT

	LDAA	FCMD			;
	JSR	PFDATA			; PUSH COMMAND TO I8272
	LDAA	UNIT			;
	JSR	PFDATA			; 
	LDAA	CYL,U			;
	JSR	PFDATA			; 
	LDAA	HEAD,U			;
	JSR	PFDATA			; 
	LDAA	SEC,U			;
	INCA
	JSR	PFDATA			; 
	LDAA	#$02			;
	JSR	PFDATA			; WHAT DENSITY
	LDAA	#$09			;
	JSR	PFDATA			; ASSUME SC (SECTOR COUNT)  EOT
	LDAA	#$0D			;
	JSR	PFDATA			; WHAT GAP IS NEEDED
	LDAA	#$FF			; DTL, IS THE LAST COMMAND BYTE TO I8272
	JSR	PFDATA
	RTS
; PERFORM READ
; FROM READ TO READ MUST NOT EXCEED 25US WORST CASE MIN.
; 1 Mhz 6809 = 1 CYC PER US OR 25 CYCLES WORST CASE (50 FOR 2 Mhz)
;	
RDD_POLL:
	BSR	SNDFDWR			; 
RDS1	LDA	>FMSR			; GET STATUS
	BPL	RDS1			; NOT READY
	BITA	#%00100000		; EXECUTION MODE?
	BEQ	DSKOPEND		; NO, ERROR
	LDA	>FDATA			; GET DATA
	STA	,X+			; WRITE IT
	LEAY	-1,Y			; REDUCE COUNT
	BNE	RDS1			; KEEP GOING
DSKOPEND:
	LDAA	FLATCH_STORE		; POINT TO FLATCH
	ORAA	#%00000001		;
	STAA	FLATCH_STORE		; SET TC
	JSR	OUTFLATCH		; OUTPUT TO CONTROLLER
	NOP				;
	NOP				; 2 MICROSECOND DELAY
	NOP				;
	NOP				; 
	ANDA	#%11111110		;
	STAA	FLATCH_STORE		; CLEAR TC
	JSR	OUTFLATCH		; OUTPUT TO CONTROLLER
	NOP				;
	NOP				; 2 MICROSECOND DELAY
*	NOP				;
*	NOP				; 2 MICROSECOND DELAY
	ORAA    #%00000010		; MOTOR OFF
	STAA	FLATCH_STORE		; POINT TO FLATCH
	JSR	OUTFLATCH		; OUTPUT TO CONTROLLER					;
					;
RSTEXIT:
	CLR	,-S			;ZERO RESULT CODE
	JSR	GFDATA			;GET ERROR TYPE
	TFR	A,B			;'B' = FDC RESULTS
	JSR	GFDATA			;READ STATUS REG#2
	BEQ	RESUL4			;NO SECOND REGISTER
* ERROR CODES DETECTED, TRANSLATE
	LDY	#RESTAB			;GET TABLE
RESUL1	LSLA				;SHIFT OUT
	BCS	RESUL2			;FOUND ERROR
	BEQ	RESUL3			;NO MORE LEFT
	LEAY	1,Y			;ADVANCE 'Y'
	BRA	RESUL1			;CONTINUE
RESUL2	LDA	,Y			;GET RESULT CODE
	STA	,S			;SET RESULT CODE
* CLEAR OUT ANY REMAINING DATA
RESUL3	
	JSR	GFDATA			;READ BYTE FROM FDC
	BNE	RESUL3			;CLEAR THEM ALL
RESUL4	BITB	#%11000000		;TEST FOR ANY ERRORS	
	PULS	A,PC			;

WRR_POLL:
	JSR	SNDFDWR			;
WRS1	LDB	,X+			;GET DATA
WRS2	LDA	>FMSR			;GET STATUS
	BPL	WRS2			;NOT READY
	BITA	#%00100000		;EXECUTION MODE?
	BEQ	WRS3			;NO, ERROR
	STB	>FDATA			;WRITE TO FDC
	LEAY	-1,Y			;BACKUP COUNT
	BNE	WRS1			; DO NEXT
WRS3	
	BRA	DSKOPEND		; 

	
		
;__SETTRACK__________________________________________________________________________________________________________________________ 
;
; 	SEEK TO A TRACK ON GIVEN UNIT
; 	A: TRACK #
;________________________________________________________________________________________________________________________________
;
SETTRACK:
	PSHS	A			; STORE A
					; ANY INTERUPT PENDING
					; IF YES FIND OUT WHY/CLEAR
	JSR	CHECKINT		; CHECK INTERRUPT STATUS, MAKE SURE IT IS CLEAR
	CMPA	#$FF			; DID IT RTSURN WITH ERROR CODE?
	BNE	SETTRK1
	JMP	SETTRKEXIT		;
					;
SETTRK1:					
	LDAA	CYL,U			; GET TRACK
	CMPA	#$00			;
	BEQ	RECAL			; IF 0 PERFORM RECAL INSTEAD OF SEEK
	LDAA	#$0F			; SEEK COMMAND
	JSR	PFDATA			; PUSH COMMAND
	LDAA	DRIVE,U			; SAY WHICH UNIT
	JSR	PFDATA			; SEND THAT
	LDAA	CYL,U			; TO WHAT TRACK
	JSR	PFDATA			; SEND THAT TOO
	JMP	WAINT			; WAIT FOR INTERRUPT SAYING DONE
RECAL:
	LDAA	#$07			; RECAL TO TRACK 0
	JSR	PFDATA			; SEND IT
	LDAA	DRIVE,U			; WHICH UNIT
	JSR	PFDATA			; SEND THAT TOO
;
WAINT:
;
SETTRK2:		
	JSR	CHECKINT			
	LDAA	>FMSR			; READ SEEK STATUS
	BITA	#%00001111		; ANY DRIVES SEEKING?
	BNE	SETTRK2			; YES, WAIT FOR THEM
;
SETTRKEXIT:
	PULS	A
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
	PSHS	A			; SAVE DATA BYTE
WRF1:
	LDAA	>FMSR			; READ FDC STATUS
	BPL	WRF1			; FDC IS NOT READY, WAIT FOR IT
	BITA	#%01000000		; TEST DIO BIT
	BNE	WRF2			; FDC IS OUT OF SYNC
	PULS	A			; RESTORE DATA
	STAA	>FDATA			; WRITE TO FDC
	RTS
*; FDC IS OUT OF SYNC CLEAR IT OUT AND RE-TRY
WRF2:
	LDAA	>FDATA			; READ DATA REGISTER
	BRA	WRF1			; AND CONTINUE

	
;__CHECKINT__________________________________________________________________________________________________________________________ 
;
; CHECK FOR ACTIVE FDC INTERRUPTS BEFORE GIVING I8272 COMMANDS
; POLL RQM FOR WHEN NOT BUSY AND THEN SEND FDC
; SENSE INTERRUPT COMMAND.  IF IT RTSURNS WITH NON ZERO
; ERROR CODE, PASS BACK TO JSRING ROUTINE FOR HANDLING
;________________________________________________________________________________________________________________________________
;
CHECKINT:
	LDAA	>FMSR			; READING OR WRITING IS KEYS TO D7 RQM
	BPL	CHECKINT		; WAIT FOR RQM TO BE TRUE. WAIT UNTIL DONE
	BITA	#%01000000		; WAITING FOR INPUT?
	LBEQ	SENDINT
ERRCLR:	
	LDAA	>FDATA			; CLEAR THE JUNK OUT OF DATA REGISTER
	LDAA	>FMSR			; CHECK WITH RQM
	ANDA	#$80			; IF STILL NOT READY, READ OUT MORE JUNK
	BEQ	ERRCLR			;
	LDAA	#$FF			; RETURN ERROR CODE -1
					;
	RTS

;__SENDINT__________________________________________________________________________________________________________________________ 
;
; SENSE INTERRUPT COMMAND
;________________________________________________________________________________________________________________________________
;			
SENDINT:
	LDAA	#$08			; SENSE INTERRUPT COMMAND
	BSR	PFDATA			; SEND IT
	JSR	GFDATA			; GET RESULTS
	STAA	ST0			; STORE THAT
	ANDA	#$C0			; MASK OFF INTERRUPT STATUS BITS
	CMPA	#$80			; CHECK IF INVALID COMMAND
	BEQ	ENDSENDINT		; YES, EXIT
	JSR	GFDATA			; GET ANOTHER (STATUS CODE 1)
	LDAA	ST0			; GET FIRST ONE
	ANDA	#$C0			; MASK OFF ALL BUT INTERRUPT CODE 00 IS NORMAL
ENDSENDINT:
	RTS				; ANYTHING ELSE IS AN ERROR

	
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
	LDA	>FMSR			; GET STATUS
	BPL	GFDATA			; NOT READY, WAIT
	ANDA	#%01000000		; ANY DATA FOR US?
	BEQ	GFDATA1			; NO, SKIP IT
	LDA	>FDATA			; GET FDC DATA
	ANDCC	#%11111011		; INSURE 'Z' CLEAR
GFDATA1	
	RTS			

	IFND BROM	
UNIT:	FCB	$00			;		
TCYL:	FCB	00			
THEAD:	FCB	00				
FCMD:	FCB	0			; COMMAND READ OR WRITE,
ST0:	FCB	0			; COMMAND READ OR WRITE,
FLATCH_STORE:
	FCB	00
	ELSE
UNIT	EQU	$2101			;		
TCYL	EQU	$2102			
THEAD	EQU	$2103				
FCMD	EQU	$2104			; COMMAND READ OR WRITE,
ST0	EQU	$2105			; COMMAND READ OR WRITE,
FLATCH_STORE	EQU	$2106	
	ENDIF
