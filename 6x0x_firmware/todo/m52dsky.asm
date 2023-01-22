	TITLE	DSKY DRIVERS
	
;__DSKY DRIVERS___________________________________________________________________________________________________________________
;
; 	CUBIX DSKY drivers for DSKY attached to the 6809 io card
;
;	Entry points:
;		SEGDISPLAY   - called to display disk control block
;________________________________________________________________________________________________________________________________
;
*
* HARDWARE I/O ADDRESSES
*
PORTA	EQU 	$F01F
PORTB	EQU 	$F010
PORTC	EQU 	$F02F	

DDRA	EQU 	$F013
DDRB	EQU 	$F012
DDRC	EQU 	$F023


		
;__SEGDISPLAY________________________________________________________________________________________
;
;  DISPLAY CONTENTS OF TRACK, SECTOR, ST0, ST1 ON DSKY
;     
;____________________________________________________________________________________________________
SEGDISPLAY:
	PSHS	X
	LDAA	#$FF			;
	STAA	DDRA			;
	STAA	DDRC			;
	LDAA	#$00			;
	STAA	DDRB			;
	LDAA	CYL,U			;
	ANDA	#$0F			;
	STAA	DISPLAYBUF+2		;
	LDAA	CYL,U			;
	ANDA	#$F0			;
	ASRA				;
	ASRA				;
	ASRA				;
	ASRA				;
	STAA	DISPLAYBUF+3		;		
	LDAA	SEC,U			;
	ANDA	#$0F			;
	STAA	DISPLAYBUF		;
	LDAA	SEC,U			;
	ANDA	#$F0			;
	ASRA				;
	ASRA				;
	ASRA				;
	ASRA				;
	STAA	DISPLAYBUF+1		;
	LDAA	HEAD,U			;
	ANDA	#$0F			;
	STAA	DISPLAYBUF+4		;
	LDAA	HEAD,U			;
	ANDA	#$F0			;
	ASRA				;
	ASRA				;
	ASRA				;
	ASRA				;
	STAA	DISPLAYBUF+5		;		
	LDAA	#$01			;
	STAA	DISPLAYBUF+6		;
	LDAA	#$00			;
	STAA	DISPLAYBUF+7		;
	LDX	#DISPLAYBUF		;
	LDAB	#$07	
	ABX
	LDAB	#$08			; SET DIGIT COUNT
	LDAA	#$40			; set Control port 7218 to off
	STAA	PORTC			; output
	JSR 	PAUSE			; wait
	LDAA	#$D0			; set control to 1111 (Data Coming, Hex Decode,NO Decode, Normal)
	STAA	PORTA			; output to port
	LDAA	#$80			; Strobe write pulse with Control=1
	STAA	PORTC			; output to port
	JSR 	PAUSE			; wait
	LDAA	#$40			; set Control port 7218 to off
	STAA	PORTC			; output
SEGDISPLAY_LP:		
	LDAA	,X			; GET DISPLAY DIGIT
	DEX
	STAA	PORTA			; OUT TO PORTA
	LDA	#$00			; SET WRITE STROBE
	STAA	PORTC			; OUT TO PORTC
	JSR	PAUSE			; DELAY
	LDAA	#$40			; SET CONTROL PORT OFF
	STAA	PORTC			; OUT TO PORTC
	JSR	PAUSE			; WAIT
	DECB				; INC POINTER
	CMPB	#$00			;
	BNE	SEGDISPLAY_LP		; LOOP FOR NEXT DIGIT
	PULS	X
	RTS
PAUSE:
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
*	NOP
*	NOP
*	NOP
	RTS

	IFND	BROM		
DISPLAYBUF:	FCB 	01,02,03,04,05,06,07,08
	ELSE
DISPLAYBUF	EQU	$2131
	ENDIF
