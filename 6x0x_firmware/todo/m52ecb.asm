	TITLE	ECB DRIVERS

;__ECB DRIVERS___________________________________________________________________________________________________________________
;
; 	CUBIX driver suite for 6809 host io processor mode
;	
;		This set of drivers handles the communication protocols to work with
;		the N8VEM Z80 cpu.  It supports drive and console IO
;
;	Entry points:
;		ECBINIT    - called during OS init
;		RDZ80	   - read a byte from console port ('A' POINTS TO BYTE)
;		WRZ80	   - write a byte to console port  ('A' POINTS TO BYTE)
;		Z80RDRIVE  - read a sector from drive  ('U' POINTS TO DCB, X TO MEMORY)
;		Z80WRDRIVE - write a sector to drive   ('U' POINTS TO DCB, X TO MEMORY)
;________________________________________________________________________________________________________________________________
;

*
* HARDWARE I/O ADDRESSES
*
PIA_A		equ	$F000		; PIA PORT A
CON_A		equ	$F001		; PIA CONTROLL A
PIA_B		equ	$F002		; PIA PORT B
CON_B		equ	$F003		; PIA CONTROLL B


;__ECBINIT_______________________________________________________________________________________________________________________
;
;	INITIALIZE THE ECB COMUNICATIONS INTERFACE
;________________________________________________________________________________________________________________________________
;
ECBINIT:
	LDAA	#$00			; CLEAR ACCUMULATOR A
	STAA	CON_A			; SET DATA DIRECTION REGISTER A
	STAA	CON_B			; SET DATA DIRECTION REGISTER B
	STAA	PIA_A			; ESTABLISH PA0-PA7 AS INPUTS
	LDAA	#%00101110		; SELECT ORA; SET MODE
	STAA	CON_A			; CONTROL FOR "A" SIDE
	LDAA	#$FF			; ESTABLISH PB0-PB7 AS OUTPUTS
	STAA	PIA_B			;	
	LDAA	#%00101110		; SELECT ORB; SET MODE
	STAA	CON_B			; CONTROL FOR "B" SIDE
	CLRA				; CLEAR ACCUMULATOR A
	RTS

;__WRZ80_________________________________________________________________________________________________________________________
;
;	write a byte to console port ('A' POINTS TO BYTE)
;________________________________________________________________________________________________________________________________
;
WRZ80	PSHS	A			; STORE A
	LDAA	#$02			; FUNCTION 02, OUT CHAR
	JSR	ECB_OUTCHAR		; SEND FUNCTION REQUEST
	PULS	A			; RESTORE A
	JSR	ECB_OUTCHAR		; FUNCTION REQUIRES ONE BYTE PARAMETER	
	RTS
	
;__RDZ80_________________________________________________________________________________________________________________________
;
;	read a byte from console port ('A' POINTS TO BYTE)
;________________________________________________________________________________________________________________________________
;
RDZ80	LDAA	#$01			; FUNCTION 01, GET KB IN
	JSR	ECB_OUTCHAR		; SEND FUNCTION REQUEST
	JSR	ECB_INCHAR		; FUNCTION RETURNS ONE BYTE
	BCC	IOF_CONIN_SUCCESS	;
	JMP	IOF_CONIN_NULL		; TIMED OUT
IOF_CONIN_SUCCESS:			;	
	CMPA	#$00			;	
	BEQ	IOF_CONIN_NULL		;
	CLRB
	RTS
IOF_CONIN_NULL:				;	
	LDAA	#$FF			; TIMED OUT
	RTS				;

;__Z80RDRIVE_____________________________________________________________________________________________________________________
;
;	 read a sector from drive  ('U' POINTS TO DCB, X TO MEMORY)
;________________________________________________________________________________________________________________________________
;
Z80RDRIVE	
	LDAA	#$04
	JSR	ECB_OUTCHAR		; DO READ SECTOR
	LDAA	DRIVE,U			; GET DRIVE
	JSR	ECB_ENC_OUTCHAR		; SEND TO ECB
	LDAA	HEAD,U			; GET CURRENT HEAD
	JSR	ECB_ENC_OUTCHAR		; SEND TO ECB
	LDAA	CYL,U			; GET CYLINDER ID
	JSR	ECB_ENC_OUTCHAR		; SEND TO ECB
	LDAA	SEC,U			; GET CYLINDER ID
	JSR	ECB_ENC_OUTCHAR		; SEND TO ECB
	JSR	ECB_DEC_CHARIN		; GET STATUS BYTE
	CMPA	#$FF			; IS ERROR?
	BEQ	DRDSECA			; YES, ABORT	
	LDAB	#$0000			; 
DRDSEC1	
	LDAA	#$06			; GET DISK BUFFER BYTE
	JSR	ECB_OUTCHAR		;
	LDAA	#$00			;
	JSR	ECB_ENC_OUTCHAR		;
	PSHS	B
	PULS	A			;
	JSR	ECB_ENC_OUTCHAR		;
	JSR	ECB_DEC_CHARIN		; GET NEXT BYTE
	STAA	,X+			; WRITE IT TO CUBIX RAM
	INCB				; BUMP COUNT
	BNE	DRDSEC1			; DONE? NO, LOOP
	LDAB	#$0000			; 
DRDSEC2
	LDAA	#$06			; GET DISK BUFFER BYTE
	JSR	ECB_OUTCHAR		;
	LDAA	#$01			;
	JSR	ECB_ENC_OUTCHAR		;
	PSHS	B
	PULS	A			;
	JSR	ECB_ENC_OUTCHAR		;
	JSR	ECB_DEC_CHARIN		; GET NEXT BYTE
	STA	,X+			; WRITE IT TO CUBIX RAM
	INCB				; BUMP COUNT
	BNE	DRDSEC2			; DONE? NO, LOOP
	LDAA	#00			; SET NO ERROR
	RTS
DRDSECA	LDAA	#02			; RETURN ERROR CODE	
	RTS
	
;__Z80WDRIVE_____________________________________________________________________________________________________________________
;
;	 write a sector to drive  ('U' POINTS TO DCB, X TO MEMORY)
;________________________________________________________________________________________________________________________________
;	
Z80WDRIVE
	LDAB	#$0000			; 
DWRSEC1	
	LDAA	#$07			; GET DISK BUFFER BYTE
	JSR	ECB_OUTCHAR		;
	LDAA	#$00			;
	JSR	ECB_ENC_OUTCHAR		;
	PSHS	B
	PULS	A			;
	JSR	ECB_ENC_OUTCHAR		;
	LDAA	,X+			; GET NEXT BYTE
	JSR	ECB_ENC_OUTCHAR		; SEND IT
	INCB				; BUMP COUNT
	BNE	DWRSEC1			; DONE? NO, LOOP
	LDAB	#$0000			; 
DWRSEC2	
	LDAA	#$07			; GET DISK BUFFER BYTE
	JSR	ECB_OUTCHAR		;
	LDAA	#$01			;
	JSR	ECB_ENC_OUTCHAR		;
	PSHS	B
	PULS	A			;
	JSR	ECB_ENC_OUTCHAR		;
	LDAA	,X+			; GET NEXT BYTE
	JSR	ECB_ENC_OUTCHAR		; SEND IT
	INCB				; BUMP COUNT
	BNE	DWRSEC2			; DONE? NO, LOOP


	LDAA	#$05
	JSR	ECB_OUTCHAR		; DO READ SECTOR
	LDAA	DRIVE,U			; GET DRIVE
	JSR	ECB_ENC_OUTCHAR		; SEND TO ECB
	LDAA	HEAD,U			; GET CURRENT HEAD
	JSR	ECB_ENC_OUTCHAR		; SEND TO ECB
	LDAA	CYL,U			; GET CYLINDER ID
	JSR	ECB_ENC_OUTCHAR		; SEND TO ECB
	LDAA	SEC,U			; GET CYLINDER ID
	JSR	ECB_ENC_OUTCHAR		; SEND TO ECB
	LDAB	$00			; SIZE OF SECTOR/2	

	JSR	ECB_DEC_CHARIN		; GET STATUS BYTE
	CMPA	#00			; IS ERROR?
	BNE	DRDSECA			; YES, ABORT
	LDAA	#00			; SET NO ERROR
	RTS
DWRSECA	LDAA	#02			; RETURN ERROR CODE		
	RTS

	
;__ECB_INCHAR____________________________________________________________________________________________________________________
;
;	read a byte from Z80
;________________________________________________________________________________________________________________________________
;
ECB_INCHAR:				;
	PSHS	X			;
	LDX	#$FFFF			;
ECB_INCHAR_LOOP:			;
	DEX				;
	CMPX	#$0000			;
	BEQ	ECB_INCHAR_ABORT	;
	TST	CON_A			; IS BYTE READY (CA1 TRANSITION)
	BPL	ECB_INCHAR_LOOP		; NO, TRY AGAIN
	LDAA	PIA_A			; READ FROM PORT A WITH CA2 STROBES
	PULS	X			;
	CLC				; SIGNAL SUCCESS
	RTS				;
ECB_INCHAR_ABORT:			;
	PULS	X			;
	SEC				; SIGNAL FAILURE
	RTS				;

;___ECB_OUTCHAR__________________________________________________________________________________________________________________
;
;	send a byte to the Z80
;________________________________________________________________________________________________________________________________
;
ECB_OUTCHAR:				;
	PSHS	B			; SAVE B-REG (COUNTER)
	STAA	PIA_B			; WRITE TO PORT B WITH CB2 STROBES
	LDAA	PIA_B			; RESET BIT 7 CRB
ECB_OUTCHAR_LOOP:			;
	TST	CON_B			; IS BYTE READY (CB1 TRANSITION)
	BPL	ECB_OUTCHAR_LOOP	; NO, TRY AGAIN
	LDAB	PIA_B			; RESET BIT 7 CRA WITH OUTPUT REGISTER READ
	PULS	B			; RESTORE B-REG
	RTS

;__ECB_ENC_OUTCHAR_______________________________________________________________________________________________________________
;
;	send an encoded byte to the Z80
;
;________________________________________________________________________________________________________________________________
;
ECB_ENC_OUTCHAR:			;
	CMPA	#27			; IS 27?
	BEQ	ECB_ENC_OUTCHAR_27	;
	CMPA	#00			; IS NULL?
	BEQ	ECB_ENC_OUTCHAR_00	;
	BRA	ECB_OUTCHAR		;
ECB_ENC_OUTCHAR_27:			;
	BSR	ECB_OUTCHAR		;
	LDAA	#27			;
	BRA	ECB_OUTCHAR		;
ECB_ENC_OUTCHAR_00:			;
	LDAA	#27			;
	BSR	ECB_OUTCHAR		;
	LDAA	#'0'			;
	BRA	ECB_OUTCHAR		;

	

;__ECB_DEC_CHARIN_____________________________________________________________________________________ 
;
;	GET A DECODED BYTE FROM THE M6809
;	A= RETURNED BYTE
;
;	THIS WILL RETURN ALL BYTES
;_____________________________________________________________________________________________________
;
ECB_DEC_CHARIN:
	BSR	ECB_INCHAR		; GET BYTE
	BCS	ECB_DEC_CHARIN		;
	RTS				;
