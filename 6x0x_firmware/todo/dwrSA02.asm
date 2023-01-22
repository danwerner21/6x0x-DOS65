
;__DWRSA02_________________________________________________________________________________________
;
;	MINI ROM MONITOR FOR THE N8VEM 6502 HOST PROCESSOR (I/O BOARD, STAND ALONE)
;
;	WRITTEN BY: DAN WERNER -- 8/10/2010
;
;	THIS MONITOR REQUIRES THE Z80 PROCESSOR TO BE RUNNING DWCON09, TO HANDLE I/O
;	FOR THE 6502 HOST PROCESSOR
;__________________________________________________________________________________________________
;
; DATA CONSTANTS
;__________________________________________________________________________________________________
;
BROM 		.EQU	1		; LET DEVICE DRIVERS KNOW CODE IS IN ROM
BDSKY		.EQU	0		; USE DSKY TO DISPLAY SECTOR INF
;
;
;REGISTER		IO PORT		; FUNCTION
PIA_A		.EQU	$F000		; PIA PORT A
CON_A		.EQU	$F001		; PIA CONTROLL A
PIA_B		.EQU	$F002		; PIA PORT B
CON_B		.EQU	$F003		; PIA CONTROLL B

IRQVECTOR   	.EQU   	$30   		; VECTOR FOR USER IRQ RTN       
WORKPTR		.EQU   	$32		; WORK POINTER FOR COMMAND PROCESSOR		
JUMPPTR		.EQU	$34		; JUMP VECTOR FOR LOOKUP TABLE	
TEMPWORD	.EQU	$36		;
TEMPWORD1	.EQU   	$38		;
TEMPWORD2	.EQU   	$3A		;
TEMPBYTE	.EQU	$3B		;
ACC      	.EQU   	$3D		; ACC STORAGE
XREG     	.EQU   	$3E 		; X REG STORAGE
YREG     	.EQU   	$3F 		; Y REG STORAGE
PREG     	.EQU   	$40 		; CURRENT STACK POINTER
PCL      	.EQU   	$41 		; PROGRAM COUNTER LOW
PCH      	.EQU   	$42 		; PROGRAM COUNTER HIGH
SPTR     	.EQU   	$43 		; CPU STATUS REGISTER
CKSM		.EQU	$44		; CHECKSUM
BYTECT		.EQU	$45		; BYTE COUNT
STRPTR	 	.EQU	$48		;
COUNTER	 	.EQU	$4A		;
SRC	 	.EQU	$4C		;
DEST	 	.EQU	$4E		;
INBUFFER	.EQU	$0200		;


	.ORG	$F800

;__COLD_START___________________________________________________
;
; PERFORM SYSTEM COLD INIT
;
;_______________________________________________________________        
COLD_START:
         	CLD				;  VERIFY DECIMAL MODE IS OFF
         	JSR ECB_INIT			;
         	BRK				; PERFORM BRK (START MONITOR)
         
;__BRKROUTINE___________________________________________________
;
; HANDLE CPU BRK INTERRUPT PROCESING AND START MONITOR
;
;_______________________________________________________________        
BRKROUTINE:    	STA   ACC       		; SAVE A    MONITOR'S BREAK HANDLER 
               	STX   XREG    		        ; SAVE X 
               	STY   YREG   		        ; SAVE Y 
               	PLA           		        ; 
               	STA   PREG    		        ; SAVE P 
               	PLA  				;
               	TAY           		        ; PCL
               	PLA 				;
               	TAX           		        ; PCH
               	TYA 				;
               	SEC            		        ; 
               	SBC   #$02     		        ; 
               	STA   PCL     		        ; BACKUP TO BRK CMD 
               	BCS   BRK2     		        ; 
               	DEX             		;               	
BRK2:          	
		STX   PCH       		; SAVE PC 
               	TSX                    		; GET STACK POINTER 
               	STX   SPTR              	; SAVE STACK POINTER 
               	JSR   PRINT_REG         	; DUMP REGISTER CONTENTS 
               	LDX   #$FF              	; 
               	TXS                     	; CLEAR STACK 
               	CLI                     	; ENABLE INTERRUPTS AGAIN 
               	JMP   COMMAND_PROCESSOR 	; START THE MONITOR
 

;__PRINT_REG____________________________________________________
;
; PRINT OUT REGISTERS ON THE DISPLAY
;
;_______________________________________________________________    
PRINT_REG:  	LDA #(REGDATA & $FF)		; OUTPUT HEADER STRING       
                STA STRPTR			;
        	LDA #(REGDATA >> 8)       	;
             	STA STRPTR+1			; 
               	JSR OUTSTR			;
               	LDA PCH				; OUTPUT PROGRAM COUNTER HIGH BYTE
               	JSR PRINT_BYTE			;
               	LDA PCL				; OUTPUT PROGRAM COUNTER LOW BYTE
               	JSR PRINT_BYTE			;
               	LDA #$20			; OUTPUT SPACE
               	JSR OUTCH			;
               	LDA ACC				; OUTPUT ACCUMULATOR
               	JSR PRINT_BYTE			;
               	LDA #$20			; OUTPUT 2 SPACES
               	JSR OUTCH			;
               	LDA #$20			;
               	JSR OUTCH			;
	       	LDA XREG			; OUTPUT X REGISTER
	       	JSR PRINT_BYTE			;
               	LDA #$20			; OUTPUT 2 SPACES
               	JSR OUTCH			;
               	LDA #$20			;
               	JSR OUTCH			;
	       	LDA YREG			; OUTPUT Y REGISTER
	       	JSR PRINT_BYTE			;
               	LDA #$20			; OUTPUT 2 SPACES
               	JSR OUTCH       		;
               	LDA #$20			;
               	JSR OUTCH			;
               	LDA SPTR			; OUTPUT STACK POINTER
	       	JSR PRINT_BYTE			;
               	LDA #$20			; OUTPUT 2 SPACES
               	JSR OUTCH     			;
               	LDA #$20			;
               	JSR OUTCH			;
      	       	LDA PREG			; OUTPUT STATUS REGISTER
	       	JSR PRINT_BYTE			; OUTPUT IN HEX
               	LDA #$2D			; OUTPUT '-'
               	JSR OUTCH			;
       	       	LDA PREG			; OUTPUT STATUS REGISTER
	       	JSR PRINT_BIN_BYTE		; OUTPUT IN BINARY
               	LDA #$0D			; PRINT NEW LINE
               	JMP OUTCH                	;
               
;__INTERRUPT____________________________________________________
;
; HANDLE IRQ INTERRUPT AND DETERMINE IF IT IS A BRK OR AN IRQ
;
;_______________________________________________________________    
INTERRUPT:     	PHA				;
               	TXA                     	; 
               	PHA                     	; 
               	TSX                     	; GET STACK POINTER 
               	LDA   $0103,X           	; LOAD INT-P REG OFF STACK 
               	AND   #$10              	; MASK BRK 
               	BNE   BRKCMD            	; BRK CMD 
               	PLA                     	;
               	TAX                     	; 
               	PLA                     	; 
               	JMP   (IRQVECTOR)       	; LET USER ROUTINE HAVE IT (USER DEFINED IRQ)
BRKCMD:        	PLA                     	;
               	TAX
               	PLA                     	; 
               	CLD
               	JMP   BRKROUTINE        	; PATCH IN USER BRK ROUTINE



;__COMMAND_PROCESSOR____________________________________________
;
; PROMPT FOR, INPUT, AND PROCESS INCOMMING USER COMMANDS
;
;_______________________________________________________________        
COMMAND_PROCESSOR:
         
         	JSR DISPLAY_PROMPT		; PRINT PROMPT STRING

	        LDA #(INBUFFER & $FF)   	; SETUP INPUT COMMAND BUFFER
         	STA STRPTR			;
         	LDA #(INBUFFER >> 8)    	;
         	STA STRPTR +1 			;
	
         	JSR INSTR			; GET A STRING FROM THE CONSOLE
         	
         	LDA #$0D			;
         	JSR OUTCH			;
         	LDA #$0A			;
         	JSR OUTCH			;
		
		LDY #$00			; SET INDEX = 0

	        LDA #(COMMAND_LOOKUP_TABLE & $FF); SETUP INPUT COMMAND POINTER
         	STA WORKPTR			;
         	LDA #(COMMAND_LOOKUP_TABLE >> 8);
         	STA WORKPTR +1 			;
                LDA INBUFFER,Y			; MOVE FIRST BYTE OF COMMAND BUFFER TO ACC
                CMP #$00			; IS NULL?
                BEQ COMMAND_PROCESSOR		; YES, GET NEXT COMMAND

COMMAND_PROCESSOR_CMP:
               	LDX #$00			; X=0
		LDA INBUFFER,Y			; ACC= NEXT BYTE OF INPUT BUFFER
		CMP (WORKPTR,X)			; DOES NEXT BYTE OF INPUT BUFFER MATCH NEXT BYTE OF LOOKUP TABLE
		BNE CMD_PROCESOR_NEXT_CMD	; NO, GO TO NEXT COMMAND IN LOOKUP TABLE
		INY				; YES, Y=Y+1
		LDA INBUFFER,Y			; LOAD NEXT BYTE OF INPUT BUFFER
		CMP #$20			; IS IT A SPACE (SINGALING END OF COMMAND)
		BEQ CMD_PROCESSOR_MATCH_FOUND	; YES, POSSIBLE MATCH FOUND 
		CMP #$00			; IS IT A NULL (SINGALING END OF COMMAND)
		BEQ CMD_PROCESSOR_MATCH_FOUND	; YES, POSSIBLE MATCH FOUND
		JSR INCWORKPTR			; NO, INCREMENT POINTER TO LOOKUP TABLE
		LDX #$00			; 
	       	LDA (WORKPTR,X)			; A= NEXT BYTE OF LOOKUP TABLE
		CMP #$00			; IS IT A NULL? (SIGNALING END OF TABLE ENTRY)
		BEQ CMD_PROCESOR_NEXT_CMD1	; YES, ADVANCE TO NEXT COMMAND IN TABLE
		JMP COMMAND_PROCESSOR_CMP	; LOOP TO CHECK NEXT CHAR

CMD_PROCESOR_NEXT_CMD:
		JSR INCWORKPTR			; INCREMENT POINTER TO LOOKUP TABLE
		LDX #$00			; 
	       	LDA (WORKPTR,X)			; A = NEXT BYTE OF LOOKUP TABLE
		CMP #$00			; IS IT A NULL?
		BNE CMD_PROCESOR_NEXT_CMD	; NO, LOOP
		
CMD_PROCESOR_NEXT_CMD1:
		JSR INCWORKPTR			; INCREMENT POINTER TO LOOKUP TABLE
		JSR INCWORKPTR			; INCREMENT POINTER TO LOOKUP TABLE
		JSR INCWORKPTR			; INCREMENT POINTER TO LOOKUP TABLE
	       	LDX #$00			;
	       	LDA (WORKPTR,X)			; A = NEXT BYTE OF LOOKUP TABLE
		CMP #$01			; IS IT $01 (SINGALING END OF LOOKUP TABLE) 
		BEQ CMD_PROCESOR_NOT_FOUND	; YES, DISPLAY NOT FOUND MESSAGE
		LDY #$00			; NO RESET INPUT BUFFER COUNTER
		JMP COMMAND_PROCESSOR_CMP	; LOOP
		
CMD_PROCESSOR_MATCH_FOUND:		
		JSR INCWORKPTR			; INCREMENT POINTER TO LOOKUP TABLE
	       	LDX #$00			;
	       	LDA (WORKPTR,X)			; A = NEXT BYTE OF LOOKUP TABLE	
		CMP #$00			; IS IT A NULL?
		BNE CMD_PROCESOR_NEXT_CMD	; NO, TRY NEXT COMMAND
		JSR INCWORKPTR			; YES, INCREMENT POINTER TO LOOKUP TABLE
		LDX #$00			; 
		LDA (WORKPTR,X)			; A = NEXT BYTE OF LOOKUP TABLE
		STA JUMPPTR			; STORE A INTO LOW BYTE OF JUMP VECTOR
		JSR INCWORKPTR			; INCREMENT POINTER TO LOOKUP TABLE
		LDX #$00			;
		LDA (WORKPTR,X)			; A = NEXT BYTE OF LOOKUP TABLE
		STA JUMPPTR+1			; INCREMENT POINTER TO LOOKUP TABLE
		JSR CMD_PROCESOR_RUN		; RUN COMMAND
		JMP COMMAND_PROCESSOR		; GET NEXT COMMAND
				
CMD_PROCESOR_NOT_FOUND:			
		LDA #(ERROR & $FF)		; LOAD LOW BYTE OF ERROR STRING       
         	STA STRPTR			; STORE IN POINTER LOW BYTE
         	LDA #(ERROR >> 8)       	; LOAD HOGH BYTE OF ERROR STRING
         	STA STRPTR +1 			; STORE IN POINTER HIGH BYTE

         	JSR OUTSTR			; OUTPUT THE STRING
		JMP COMMAND_PROCESSOR		;
CMD_PROCESOR_RUN:
		JMP (JUMPPTR)		        ; JUMP TO COMMAND VECTOR	
	
EXIT_MONITOR:
         	JMP ($FFFC)			;


         	
;__LOAD_________________________________________________________

; LOAD A MOTOROLA FORMATTED HEX FILE
; 
;_______________________________________________________________                 	         	
LOAD:
	JSR	IOF_CONINW			;
	CMP	#'S'				;
	BNE	LOAD				; FIRST CHAR NOT (S)
	JSR	IOF_CONINW			; READ CHAR
	CMP	#'9'				;
	BEQ	LOAD21				;
	CMP	#'1'				;
	BNE	LOAD				; SECOND CHAR NOT (1)
	LDA	#$00				;
	STA	CKSM				; ZERO CHECKSUM
	JSR	GETBYTE				; READ BYTE
	SBC	#$01				;
	STA	BYTECT				; BYTE COUNT
	JSR	BADDR				; BUILD ADDRESS
	LDY	#$00				;
LOAD11:
	JSR	GETBYTE				;
	DEC	BYTECT				;
	BEQ	LOAD15				; ZERO BYTE COUNT
	STA	(TEMPWORD1),Y			; STORE DATA
	JSR 	INCTEMPWORD			;
	JMP	LOAD11				;

LOAD15:
	INC	CKSM				;
	BEQ	LOAD				;
LOAD19:
	LDA	#'?'				;
	JSR	OUTCH				;
LOAD21:	
	RTS	
GETBYTE:
	JSR	INHEX				; GET HEX CHAR
	ASL	A				;
	ASL	A				;
	ASL	A				;
	ASL	A				;
	STA	TEMPBYTE			;
	JSR	INHEX				;
	AND	#$0F				; MASK TO 4 BITS
	ORA	TEMPBYTE			;
	PHA					;
	CLC					;
	ADC	CKSM				;
	STA	CKSM				;
	PLA					;
	RTS					;
; INPUT HEX CHAR
INHEX:
	JSR	IOF_CONINW			;
	PHA					;
	JSR	OUTCH				;
	PLA					;
    	CMP #$3A  				; LESS THAN 9?
      	BCS INHEX_BIG  				; NO, SKIP NEXT
      	SBC #$2F  				; CONVERT 0-9
INHEX_BIG: 
	CMP #$41  				; A OR MORE?
      	BCC INHEX_SMALL 			; NO, SKIP NEXT
      	SBC #$37  				; CONVERT A-F
INHEX_SMALL: 
	RTS					;

; BUILD ADDRESS
BADDR:
	JSR	GETBYTE				; READ 2 FRAMES
	STA	TEMPWORD1+1			;
	JSR	GETBYTE				;
	STA	TEMPWORD1			;
	RTS
         	
         	
;__GO______________________________________________________
;
; GO COMMAND
; 
; GO XXXX 
;_______________________________________________________________        
GO: 		

	        LDA #(INBUFFER & $FF)   	; SETUP WORK BUFFER
         	STA WORKPTR			;
         	LDA #(INBUFFER >> 8)    	;
         	STA WORKPTR +1 			;
         	
		JSR INCWORKPTR			; JUMP OVER "G"
		JSR INCWORKPTR			; JUMP OVER "O"		
		
		JSR EATWHITESPACE		; SKIP OVER THE WHITESPACE
		JSR GETNUMBER			; GET THE STARTING ADDRESS
		BCS DUMP_ERROR			; IF NOT A NUMBER, REPORT ERROR

		JMP (TEMPWORD)			;		


         	

;__DUMPMEM______________________________________________________
;
; DUMP MEMORY COMMAND
; 
; DUMP XXXX (XXXX)
;_______________________________________________________________        
DUMP: 		

	        LDA #(INBUFFER & $FF)   	; SETUP WORK BUFFER
         	STA WORKPTR			;
         	LDA #(INBUFFER >> 8)    	;
         	STA WORKPTR +1 			;
         	
		JSR INCWORKPTR			; JUMP OVER "D"
		JSR INCWORKPTR			; JUMP OVER "U"		
		JSR INCWORKPTR			; JUMP OVER "M"
		JSR INCWORKPTR			; JUMP OVER "P"
		
		JSR EATWHITESPACE		; SKIP OVER THE WHITESPACE
		JSR GETNUMBER			; GET THE STARTING ADDRESS
		BCS DUMP_ERROR			; IF NOT A NUMBER, REPORT ERROR
		
		LDA TEMPWORD			; STORE STARTING ADDRESS IN WORD POINTER (TEMPWORD1)
		STA TEMPWORD1			;
		LDA TEMPWORD+1			;
		STA TEMPWORD1+1			;

		JSR EATWHITESPACE		; SKIP OVER ANY WHITESPACE
		JSR GETNUMBER			; GET THE ENDING ADDRESS
		BCS DUMP_ERROR			; IF NOT A NUMBER, REPORT ERROR

		LDA TEMPWORD1			; STORE ENDING ADDRESS IN WORD POINTER (WORKPTR)
		STA WORKPTR			;
		LDA TEMPWORD1+1			;
		STA WORKPTR+1			;
DUMP_LOOP:    	     	
	       	JSR PRINT_MEM_LINE		;	  
	       	LDA #$0D			;
	       	JSR OUTCH			;
	       	LDA #$0A			;
	       	JSR OUTCH			;     		       	
           	LDA WORKPTR+1  			; COMPARE HIGH BYTES
           	CMP TEMPWORD+1   
           	BCC DUMP_LOOP 			; IF NUM1H < NUM2H THEN NUM1 < NUM2
           	BNE DUMP_DONE			; IF NUM1H <> NUM2H THEN NUM1 > NUM2 (SO NUM1 >= NUM2)
           	LDA WORKPTR  			; COMPARE LOW BYTES
           	CMP TEMPWORD
           	BCC DUMP_LOOP			; IF NUM1L < NUM2L THEN NUM1 < NUM2
DUMP_DONE:
		RTS
DUMP_ERROR:
		JMP INVALID_NUMBER_ERROR



;__ENTERMEM_____________________________________________________
;
; ENTER MEMORY COMMAND
; 
; ENTER XXXX (XX XX XX XX XX XX XX XX XX XX XX XX XX XX )
;_______________________________________________________________        
ENTERMEM: 		

	        LDA #(INBUFFER & $FF)   	; SETUP WORK BUFFER
         	STA WORKPTR			;
         	LDA #(INBUFFER >> 8)    	;
         	STA WORKPTR +1 			;
         	
		JSR INCWORKPTR			; JUMP OVER "E"
		JSR INCWORKPTR			; JUMP OVER "N"		
		JSR INCWORKPTR			; JUMP OVER "T"
		JSR INCWORKPTR			; JUMP OVER "E"
		JSR INCWORKPTR			; JUMP OVER "R"
		
		JSR EATWHITESPACE		; SKIP OVER ANY WHITESPACE
		JSR GETNUMBER			; GET NUMBER
		BCS ENTER_ERROR			; IF NOT A NUMBER REPORT ERROR
		
		LDA TEMPWORD			; STORE ADDRESS INTO WORD POINTER (TEMPWORD1)
		STA TEMPWORD1			;
		LDA TEMPWORD+1			;
		STA TEMPWORD1+1			;
		LDY #$00			; RESET COUNTER TO 0
ENTERLOOP:
		LDX #$00			; 
		LDA (WORKPTR,X)			; GET NEXT BYTE FROM BUFFER
		CMP #$00			; IS NULL?
		BEQ ENTER_DONE			; YES, WE'RE DONE
		JSR EATWHITESPACE		; SKIP OVER ANY WHITESPACE
		JSR GETNUMBER			; GET NEXT NUMBER
		BCS ENTER_ERROR			; IF NOT A NUMBER REPORT ERROR
		LDA TEMPWORD			; STORE BYTE IN ADDRESS (INDEXED BY Y)
		STA (TEMPWORD1),Y		;		
		INY				; GO TO NEXT BYTE
		JMP ENTERLOOP			; LOOP
ENTER_DONE:
		CPY #$00			; WAS LINE BLANK?
		BNE ENTER_CONTINUE		; NO, PREPARE FOR NEXT LINE
		RTS				; YES, END DATA ENTRY
ENTER_CONTINUE:
		LDA #$0D			;
		JSR OUTCH			;
		LDA #$0A			;
		JSR OUTCH
		LDA #$3A			; OUTPUT ":" TO SCREEN
		JSR OUTCH			;
		CLC				; CLEAR CARRY
		TYA				; A=Y (LAST COUNTER)
		ADC TEMPWORD1			; ADD LAST COUNT TO BEGINNING POINTER
		STA TEMPWORD1			; STORE RESULT IN BEGINNING POINTER
		BNE ENTER_INCREMENT		; NOT ZERO?, DONE
		INC TEMPWORD1+1			; ZERO, INC HIGH BYTE
ENTER_INCREMENT:	
		LDA TEMPWORD1+1			; PRINTOUT STARTING ADDRESS TO SCREEN
		JSR PRINT_BYTE			; (HIGH)
		LDA TEMPWORD1			;
		JSR PRINT_BYTE			; (LOW)
		LDA #$20			; OUTPUT SPACE TO SCREEN
		JSR OUTCH			;
	        LDA #(INBUFFER & $FF)   	; SETUP INPUT COMMAND BUFFER
         	STA STRPTR			;
         	LDA #(INBUFFER >> 8)    	;
         	STA STRPTR +1 			;
         	JSR INSTR			; GET A STRING FROM THE CONSOLE
	        LDA #(INBUFFER & $FF)   	; SETUP WORK BUFFER
         	STA WORKPTR			;
         	LDA #(INBUFFER >> 8)    	;
         	STA WORKPTR +1 			;
         	LDY #$00 			;
         	JMP ENTERLOOP			; LOOP

		
ENTER_ERROR:
		JMP INVALID_NUMBER_ERROR

;__PRINT_MEM_LINE_______________________________________________
;
; PRINT MEMORY DUMP LINE
;  
; PRINT 16 HEX LOCATIONS STARTING WITH ADDRESS WORKPTR
;_______________________________________________________________        
PRINT_MEM_LINE:
		LDA #$3A			; LOAD ':' INTO ACC
		JSR OUTCH			; PRINT ':'
    	     	LDA WORKPTR+1			; PRINT ADDRESS
	       	JSR PRINT_BYTE			; 
      	       	LDA WORKPTR			;
	       	JSR PRINT_BYTE			;
 	       	LDA #$2D			; LOAD '-'
	       	JSR OUTCH			; PRINT '-'
	       	LDY #$00			;
PRINT_MEM_LINE_LOOP:
		LDA (WORKPTR),Y			; LOAD NEXT BYTE
		JSR PRINT_BYTE	       		; PRINT BYTE
 	       	LDA #$20			; LOAD ' '
	       	JSR OUTCH			; PRINT ' '
	       	INY				; INCREMENT COUNTER
	       	CPY #$10			; HAVE WE PRINTED 16 ADDRESSES
	       	BNE PRINT_MEM_LINE_LOOP		; NO, LOOP
		LDA #$3A			; LOAD ':' INTO ACC
		JSR OUTCH			; PRINT ':'
                LDX #$00			;
		LDY #$00			;
PRINT_MEM_LINE_LOOP_ASCII:		
		LDA (WORKPTR,X)			; GET NEXT BYTE
		JSR OUTASCII			; PRINT ASCII VALUE OF BYTE
		INY				; INCREMENT COUNTER
		JSR INCWORKPTR			; INCREMENT BUFFER POINTER
	       	CPY #$10			; HAVE WE PRINTED 16 ADDRESSES
	       	BNE PRINT_MEM_LINE_LOOP_ASCII	; NO, LOOP
	       	LDA #$0D			; YES, PRINT CR
	       	JSR OUTCH			;
	       	RTS				; RETURN
				

               
;__DISPLAY_PROMPT______________________________________________
;
; DISPLAY THE INPUT PROMPT ON THE SCREEN              
;
;______________________________________________________________                              
DISPLAY_PROMPT:
		LDA #(PROMPT & $FF)		; LOAD LOW BYTE OF PROMPT STRING       
         	STA STRPTR			; STORE IN POINTER LOW BYTE
         	LDA #(PROMPT >> 8)       	; LOAD HOGH BYTE OF PROMPR STRING
         	STA STRPTR +1 			; STORE IN POINTER HIGH BYTE

         	JMP OUTSTR			; OUTPUT THE STRING


;__INCWORKPTR___________________________________________________
; INCREMENT THE 16BIT WORK POINTER
;
;
;
;_______________________________________________________________        
INCWORKPTR: 		
		INC WORKPTR			; INCREMENT LOWBYTE
		BNE INCWORKPTR_OUT		; NOT ZERO?, DONE
		INC WORKPTR+1			; ZERO, INC HIGH BYTE
INCWORKPTR_OUT:
		RTS				; RETURN
		
		
;__INCTEMPWORD__________________________________________________
;
; INCREMENT THE 16BIT WORK POINTER
;
;_______________________________________________________________        
INCTEMPWORD: 		
		INC TEMPWORD1			; INCREMENT LOWBYTE
		BNE INCTEMPWORD_OUT		; NOT ZERO?, DONE
		INC TEMPWORD1+1			; ZERO, INC HIGH BYTE
INCTEMPWORD_OUT:
		RTS				; RETURN

;__INCTEMPWORD2__________________________________________________
;
; INCREMENT THE 16BIT WORK POINTER
;
;
;_______________________________________________________________        
INCTEMPWORD2: 		
		INC TEMPWORD2			; INCREMENT LOWBYTE
		BNE INCTEMPWORD2_OUT		; NOT ZERO?, DONE
		INC TEMPWORD2+1			; ZERO, INC HIGH BYTE
INCTEMPWORD2_OUT:
		RTS				; RETURN



;__OUTASCII_____________________________________________________
;
; PRINT CHAR IF VALID, ELS PRINT '.'
;  
;_______________________________________________________________        
OUTASCII:
		CMP #$20			; IS < 20
		BMI OUTASCII_DOT		; YES, SKIP
		JMP OUTCH			; NO, PRINT CHAR AND RETURN
OUTASCII_DOT:	
		LDA #$2E			; A= '.'
		JMP OUTCH			; PRINT '.' AND RETURN
         	


;__INVALID_NUMBER_ERROR__________________________________________
;
; PRINT "INVALID HEX NUMBER MESSAGE"
; 
;_______________________________________________________________        
INVALID_NUMBER_ERROR:
		LDA #(INERROR & $FF)		; LOAD LOW BYTE OF ERROR STRING       
         	STA STRPTR			; STORE IN POINTER LOW BYTE
         	LDA #(INERROR >> 8)       	; LOAD HOGH BYTE OF ERROR STRING
         	STA STRPTR +1 			; STORE IN POINTER HIGH BYTE
         	JMP OUTSTR			; OUTPUT THE STRING
         	
				
;__GETNUMBER______________________________________________________
;
; GET ASCII NUMBER FROM BUFFER AND PARSE INTO TEMPWORD
; 
;_______________________________________________________________        
GETNUMBER:
		LDA #$00			;
		STA TEMPWORD			; CLEAR OUT TEMPWORD (OUTPUT OF GETNUMBER)
		STA TEMPWORD+1			;
		LDX #$00			;
GETNUMBER_LOOP:
		LDA (WORKPTR,X)			; GET NEXT BYTE FROM BUFFER
		CMP #$20			; IS SPACE?
		BEQ GETNUMBER_DONE		; YES, WE'RE DONE
		CMP #$00			; IS NULL?
		BEQ GETNUMBER_DONE		; YES, WE'RE DONE
		CMP #$2C			; IS ","?
		BEQ GETNUMBER_DONE		; YES, WE'RE DONE
		CMP #$29			; IS ")"?
		BEQ GETNUMBER_DONE		; YES, WE'RE DONE
		JSR HEXIN			; GET HEX DIGIT
		BCS GETNUMBER_ERROR		; IS INVALID DIGIT?, YES PRINT ERROR AND ABORT
		CLC				; CLEAR CARRY
		ROL TEMPWORD			; MOVE WORD OVER 4 BITS TO LEFT
		ROL TEMPWORD+1			;
		CLC				;
		ROL TEMPWORD			;
		ROL TEMPWORD+1			;
		CLC				;
		ROL TEMPWORD			;
		ROL TEMPWORD+1			;
		CLC				;
		ROL TEMPWORD			;
		ROL TEMPWORD+1			;
		ORA TEMPWORD			; ADD IN NEW DIGIT
		STA TEMPWORD			; STORE BACK TO TEMPWORD
		JSR INCWORKPTR			; INCREMENT BUFFER POINTER
		JMP GETNUMBER_LOOP		; LOOP
GETNUMBER_ERROR: 		
		SEC				; SET ERROR FLAG (CARRY)
		RTS				; RETURN
GETNUMBER_DONE:
		CLC				; CLEAR ERROR FLAG (CARRY)
		RTS				; RETURN
		
;__HEXIN________________________________________________________
;
; GET NEXT CHAR FROM INPUT BUFFER AND CHANGE TO HEX DIGIT
; 
; IF INVALID, SET CARRY FLAG
;_______________________________________________________________        
HEXIN:
		LDX #$00			;
		LDA (WORKPTR,X)			; GET NEXT CHAR FROM BUFFER
      		CMP #$3A  			; LESS THAN 9?
      		BCS HEXIN_BIG  			; NO, SKIP NEXT
      		SBC #$2F  			; CONVERT 0-9
HEXIN_BIG: 
		CMP #$41  			; A OR MORE?
      		BCC HEXIN_SMALL 		; NO, SKIP NEXT
      		SBC #$37  			; CONVERT A-F
HEXIN_SMALL: 
		CMP #$10  			; RESULT TOO BIG?
      		RTS


;__EATWHITESPACE___________________________________________________
;
; FORWARD THE BUFFER POINTER PAST ANY WHITE SPACE IN THE INPUT BUFFER 
; 
;_______________________________________________________________        
EATWHITESPACE:
		LDX #$00			;
		LDA (WORKPTR,X)			; GET NEXT CHAR FROM BUFFER
		CMP #$20			; IS SPACE
		BNE EATWHITESPACE_OUT		; NO, DONE
		JSR INCWORKPTR			; YES, INCREMENT BUFFER POINTER
		JMP EATWHITESPACE		; LOOP
EATWHITESPACE_OUT:
		RTS				; RETURN 
		         	
               
;__PRINT_BYTE__________________________________________________
;
; PRINT OUT ACCUMULATOR AS HEX NUMBER 
;
;______________________________________________________________
PRINT_BYTE:     
               TAX				; SAVE A REGISTER
               LSR A				; SHIFT HIGH NIBBLE TO LOW NIBBLE
               LSR A				;
               LSR A				;
               LSR A				;
               CLC               		; CLEAR CARRY
               JSR PRINT_DIGIT			; PRINT LOW NIBBLE
               TXA				; RESTORE ACCUMULATOR
               JMP PRINT_DIGIT			; PRINT LOW NIBBLE

;__PRINT_DIGIT_________________________________________________
;
; PRINT OUT LOW NIBBLE OF ACCUMULATOR IN HEX  
;
;______________________________________________________________
PRINT_DIGIT:
               AND #$0F				; STRIP OFF HIGH NIBBLE				
               ORA #$30				; ADD $30 TO PRODUCE ASCII
               CMP #$3A               		; IS GREATER THAN 9
               BMI PRINT_DIGIT_OUT		; NO, SKIP ADD
               CLC				; CLEAR CARRY
               ADC #$07				; ADD ON FOR LETTER VALUES
PRINT_DIGIT_OUT:					;	
               JMP OUTCH               		; PRINT OUT CHAR
               

;__PRINT_BIN_BYTE______________________________________________
;
; PRINT OUT BYTE IN BINARY  
;
;______________________________________________________________
PRINT_BIN_BYTE:
               ASL A				; ROTATE BIT 7 INTO CARRY FLAG
               BCC PRINT_BIN8_0			;  IS ZERO?
               JSR PRINT_1			;  NO, PRINT OUT A '1'
               JMP PRINT_BIN_BIT_7		;  JUMP TO NEXT BIT
PRINT_BIN8_0:					; 
               JSR PRINT_0			;  YES, PRINT A '0'
PRINT_BIN_BIT_7:					; 
               ASL A				; ROTATE BIT 6 INTO CARRY FLAG
               BCC PRINT_BIN7_0			;  IS ZERO?
               JSR PRINT_1			;  NO, PRINT OUT A '1' 
               JMP PRINT_BIN_BIT_6		;  JUMP TO NEXT BIT
PRINT_BIN7_0:					; 
               JSR PRINT_0			;  YES, PRINT A '0'
PRINT_BIN_BIT_6:					; 
               ASL A				; ROTATE BIT 5 INTO CARRY FLAG
               BCC PRINT_BIN6_0			;  IS ZERO?
               JSR PRINT_1			;  NO, PRINT OUT A '1'
               JMP PRINT_BIN_BIT_5		;  JUMP TO NEXT BIT
PRINT_BIN6_0:					; 
               JSR PRINT_0			;  YES, PRINT A '0'
PRINT_BIN_BIT_5:					; 
               ASL A				; ROTATE BIT 4 INTO CARRY FLAG
               BCC PRINT_BIN5_0			;  IS ZERO?
               JSR PRINT_1			;  NO, PRINT OUT A '1'
               JMP PRINT_BIN_BIT_4		;  JUMP TO NEXT BIT
PRINT_BIN5_0:					; 
               JSR PRINT_0			;  YES, PRINT A '0'
PRINT_BIN_BIT_4:					; 
               ASL A				; ROTATE BIT 3 INTO CARRY FLAG	
               BCC PRINT_BIN3_0			;  IS ZERO?	
               JSR PRINT_1			;  NO, PRINT OUT A '1'	
               JMP PRINT_BIN_BIT_2		;  JUMP TO NEXT BIT	
PRINT_BIN3_0:					; 
               JSR PRINT_0			;  YES, PRINT A '0'
PRINT_BIN_BIT_2:					; 	
               ASL A				; ROTATE BIT 2 INTO CARRY FLAG	
               BCC PRINT_BIN2_0			;  IS ZERO?	
               JSR PRINT_1			;  NO, PRINT OUT A '1'		
               JMP PRINT_BIN_BIT_1		;  JUMP TO NEXT BIT		
PRINT_BIN2_0:					; 	
               JSR PRINT_0			;  YES, PRINT A '0'		
PRINT_BIN_BIT_1:					; 	
               ASL A				; ROTATE BIT 1 INTO CARRY FLAG	
               BCC PRINT_BIN1_0			;  IS ZERO?	
               JSR PRINT_1			;  NO, PRINT OUT A '1'
               JMP PRINT_BIN_BIT_0		;  JUMP TO NEXT BIT
PRINT_BIN1_0:					; 
               JSR PRINT_0			;  YES, PRINT A '0'
PRINT_BIN_BIT_0:					; 
               ASL A				; ROTATE BIT 0 INTO CARRY FLAG
               BCC PRINT_BIN0_0			;  IS ZERO?	
               JMP PRINT_1			;  NO, PRINT OUT A '1'
PRINT_BIN0_0:					;  	
               JMP PRINT_0			;   YES, PRINT A '0'
                
               
               
;__PRINT_1_____________________________________________________
;
; PRINT OUT A '1'             
;
;______________________________________________________________
PRINT_1:					
               PHA				; PUSH ACC TO STACK				
               LDA #$31				; LOAD '1'
               JSR OUTCH			; OUTPUT CHAR TO SCREEN
               PLA				; PULL ACC FROM STACK
               RTS				; RETURN
               
               
;__PRINT_0_____________________________________________________
;
; PRINT OUT A '0'              
;
;______________________________________________________________
PRINT_0:        
               PHA				; PUSH ACC TO STACK
               LDA #$30				; LOAD '0'
               JSR OUTCH			; OUTPUT CHAR TO SCREEN
               PLA				; PULL ACC FROM STACK
               RTS				; RETURN


;__OUTSTR______________________________________________________
;
; OUTPUT THE STRING POINTED TO BU OUTSTR TO THE SCREEN 
;
;______________________________________________________________
OUTSTR:
	        LDY   #$00			; LOAD $00 INTO Y
OUTSTRLP:
        	LDA (STRPTR),Y     		; LOAD NEXT CHAR FROM STRING INTO ACC
        	CMP #$00			; IS NULL?
        	BEQ ENDOUTSTR			; YES, END PRINT OUT
        	JSR OUTCH  			; PRINT CHAR IN ACC
        	INY      			; Y=Y+1 (BUMP INDEX)
        	JMP OUTSTRLP			; DO NEXT CHAR
ENDOUTSTR:
        	RTS				; RETURN

;__INSTR_______________________________________________________
;
; INPUT STRING FROM KEYBOARD INTO KEYBOARD BUFFER 
;
;______________________________________________________________
INSTR:
	        LDY   #$00			; LOAD $00 INTO Y
INSTRLP:
		JSR IOF_CONINW		
        	CMP #$0D			; IS CR?
        	BEQ ENDINSTR			; YES, DONE WITH INPUT 
        	CMP #$08			; IS BACKSPACE?
        	BNE INSTR_NOTBS			; NO, SKUP BACKSPACE RTN
        	CPY #$00			; IS INDEX =0 ?
        	BEQ INSTR_EMPTY_BS		; YES, SKIP BACKSPACE
        	JSR OUTCH     			; OUTPUT CHAR TO SCREEN        	
        	DEY				; Y=Y-1
        	LDA #$00			; 
        	STA (STRPTR),Y			; NULL TERMINATE INPUT BUFFER
        	DEY				; Y=Y-1
        	JMP INSTR_SKIP_STORE       	; SKIP STORE OF CHAR TO INPUT BUFFER
INSTR_NOTBS        
        	STA (STRPTR),Y			; STORE CHAR IN KEYBAORD BUFFER
        	JSR OUTCH     			; OUTPUT CHAR TO SCREEN
INSTR_SKIP_STORE                   			
	        INY				; Y=Y+1
        	CPY #$FF			; DOES Y=$FF
        	BNE INSTRLP                	; NO, LOOP FOR NEXT CHAR
ENDINSTR
        	LDA #$00			; A=0
        	STA (STRPTR),Y			; NULL TERMINATE INPUT BUFFER
        	RTS
INSTR_EMPTY_BS		
        	LDA #$00			; BLANK OUT KEYBOARD CHAR, TO SIGNAL READY FOR NEXT CHAR
        	JMP INSTRLP			; JUMP TO INPUT LOOP
	
	
;__________________________________________________________________________________________________________	
	
IOF_CONINW:					;
		JSR	IOF_CONIN		;
		CMP	#$00			;
		BEQ	IOF_CONINW		;
		RTS
	
IOF_CONIN:					;
		LDA	#$01			; FUNCTION 01, GET KB IN
		JSR	ECB_OUTCHAR		; SEND FUNCTION REQUEST
		JSR	ECB_INCHAR		; FUNCTION RETURNS ONE BYTE
		BCC	IOF_CONIN_SUCCESS	;
		LDA	#$00			; TIMED OUT
IOF_CONIN_SUCCESS:				;	
		RTS				;
	
OUTCH:						;
		PHA				; STORE A
		LDA	#$02			; FUNCTION 01, GET KB IN
		JSR	ECB_OUTCHAR		; SEND FUNCTION REQUEST
		PLA				; RESTORE A
		JSR	ECB_OUTCHAR		; FUNCTION REQUIRES ONE BYTE PARAMETER
	
		RTS

	
;__ECB_INIT___________________________________________________________________________________________ 
;
;	INIT ECB INTERFACE
;
;_____________________________________________________________________________________________________
;

ECB_INIT:
		LDA	#$00			; CLEAR ACCUMULATOR A
		STA	CON_A			; SET DATA DIRECTION REGISTER A
		STA	CON_B			; SET DATA DIRECTION REGISTER B
		STA	PIA_A			; ESTABLISH PA0-PA7 AS INPUTS
		LDA	#%00101110		; SELECT ORA; SET MODE
		STA	CON_A			; CONTROL FOR "A" SIDE
		LDA	#$FF			; ESTABLISH PB0-PB7 AS OUTPUTS
		STA	PIA_B			;	
		LDA	#%00101110		; SELECT ORB; SET MODE
		STA	CON_B			; CONTROL FOR "B" SIDE
		LDA	#$00			; CLEAR ACCUMULATOR A
		RTS					


;__ECB_DEC_CHARIN_____________________________________________________________________________________ 
;
;	GET A DECODED BYTE FROM THE M6809
;	A= RETURNED BYTE
;
;	THIS WILL RETURN ALL BYTES
;_____________________________________________________________________________________________________
;
ECB_DEC_CHARIN:
		JSR	ECB_INCHAR		; GET BYTE
		BCS	ECB_DEC_CHARIN		;
		RTS				;
	
			
ECB_ENC_OUTCHAR:				;
		CMP	#27			; IS 27?
		BEQ	ECB_ENC_OUTCHAR_27	;
		CMP	#00			; IS NULL?
		BEQ	ECB_ENC_OUTCHAR_00	;
		JMP	ECB_OUTCHAR		;
ECB_ENC_OUTCHAR_27:				;
		JSR	ECB_OUTCHAR		;
		LDA	#27			;
		JMP	ECB_OUTCHAR		;
ECB_ENC_OUTCHAR_00:				;
		LDA	#27			;
		JSR	ECB_OUTCHAR		;
		LDA	#'0'			;
		JMP	ECB_OUTCHAR		;
	
	
ECB_INCHAR:					;
		LDA	#$00			;
		STA	COUNTER			;
		STA	COUNTER+1		;
ECB_INCHAR_LOOP:				;
		DEC	COUNTER			;
		BEQ	ECB_INCHAR_ABORT	;
		LDA	CON_A			; IS BYTE READY (CA1 TRANSITION)
		BPL	ECB_INCHAR_LOOP		; NO, TRY AGAIN
		LDA	PIA_A			; READ FROM PORT A WITH CA2 STROBES
		CLC				; SIGNAL SUCCESS
		RTS				;
ECB_INCHAR_ABORT:
		DEC	COUNTER+1		;
		BNE	ECB_INCHAR_LOOP		;	
		SEC				; SIGNAL FAILURE
		RTS				;

ECB_OUTCHAR:					;
		STA	PIA_B			; WRITE TO PORT B WITH CB2 STROBES
		LDA	PIA_B			; RESET BIT 7 CRB
ECB_OUTCHAR_LOOP:				;
		LDA	CON_B			; IS BYTE READY (CB1 TRANSITION)
		BPL	ECB_OUTCHAR_LOOP	; NO, TRY AGAIN
		LDA	PIA_B			; RESET BIT 7 CRA WITH OUTPUT REGISTER READ
		RTS

IOF_BOOT:
	LDA	#$F1			; FUNCTION F1, LOAD DOS/65 IMAGE
	JSR	ECB_OUTCHAR		; SEND FUNCTION REQUEST		
	JSR	ECB_DEC_CHARIN		; SEND FUNCTION REQUEST		
	CMP	#$FF			;
	BEQ	IOF_BOOTA		;
	LDA	#$00			; SRC=0000
	STA	SRC			; DEST=D000
	STA	SRC+1			;
	STA	DEST			;
	LDA	#$D0			;
	STA	DEST+1			;
	LDY	#$00			;
IOF_BOOT1:
	LDA	#$06			; GET DISK BUFFER BYTE
	JSR	ECB_OUTCHAR		;
	LDA	SRC+1			;
	JSR	ECB_ENC_OUTCHAR		;
	LDA	SRC			;
	JSR	ECB_ENC_OUTCHAR		;
	JSR	ECB_DEC_CHARIN		; GET NEXT BYTE
	STA	(DEST),Y		; WRITE IT TO CUBIX RAM	
	INC	DEST			; BUMP COUNT
	BNE     IOF_BOOT2		;
	INC	DEST+1			;	
IOF_BOOT2:	
	INC	SRC			;
	BNE	IOF_BOOT3
	INC	SRC+1
IOF_BOOT3:
	LDA	DEST+1
	CMP	#$F0			;
	BNE	IOF_BOOT1		; DONE? NO, LOOP
	JMP	$E400
IOF_BOOTA:	
	RTS

	
IOF_WBOOT:
	LDA	#$F2			; FUNCTION F1, LOAD DOS/65 IMAGE
	JSR	ECB_OUTCHAR		; SEND FUNCTION REQUEST		
	JSR	ECB_DEC_CHARIN		; SEND FUNCTION REQUEST		
	CMP	#$FF			;
	BEQ	IOF_WBOOTA		;
	LDA	#$00			; SRC=0000
	STA	SRC			; DEST=D000
	STA	SRC+1			;
	STA	DEST			;
	LDA	#$D0			;
	STA	DEST+1			;
	LDY	#$00			;
IOF_WBOOT1:
	LDA	#$06			; GET DISK BUFFER BYTE
	JSR	ECB_OUTCHAR		;
	LDA	SRC+1			;
	JSR	ECB_ENC_OUTCHAR		;
	LDA	SRC			;
	JSR	ECB_ENC_OUTCHAR		;
	JSR	ECB_DEC_CHARIN		; GET NEXT BYTE
	STA	(DEST),Y		; WRITE IT TO CUBIX RAM	
	INC	DEST			; BUMP COUNT
	BNE     IOF_WBOOT2		;
	INC	DEST+1			;	
IOF_WBOOT2:	
	INC	SRC			;
	BNE	IOF_WBOOT3
	INC	SRC+1
IOF_WBOOT3:
	LDA	DEST+1
	CMP	#$E4			;
	BNE	IOF_WBOOT1		; DONE? NO, LOOP
IOF_WBOOTA:	
	RTS
	
	.BYTE 00,00,00		
	
	
;	.include 'M52IDE.asm'	; IDE I/O DRIVERS
	.include 'M52SER.asm'	; SERIAL I/O DRIVERS
;	.include 'M52FLP.asm'	; FLOPPY I/O DRIVERS

	#IF	BDSKY
	.include 'M52DSKY.asm'	; DSKY I/O DRIVERS
	#ENDIF
	
			
; COMMAND PROCESSOR JUMP TABLE
COMMAND_LOOKUP_TABLE
 		.BYTE "REGISTER",0,(PRINT_REG & $FF),(PRINT_REG >> 8)
 		.BYTE "DUMP",0,(DUMP & $FF),(DUMP >> 8)
 		.BYTE "ENTER",0,(ENTERMEM & $FF),(ENTERMEM >> 8)
 		.BYTE "GO",0,(GO & $FF),(GO >> 8)
 		.BYTE "LOAD",0,(LOAD & $FF),(LOAD >> 8)
 		.BYTE "BOOT",0,(IOF_BOOT & $FF),(IOF_BOOT >> 8)
		.BYTE 01,0
; COMMAND PROMPT STRING		
PROMPT  	.BYTE   $0D,$0A,".",0 
; ERROR STRING
ERROR	 	.BYTE   $0D,$0A,"? COMMAND NOT FOUND",$0D,0
INERROR		.BYTE   $0D,$0A,"? INVALID HEX NUMBER",$0D,0
; STRINGS FOR REGISTER DISPLY
REGDATA 	.BYTE   $0D,$0A,
		.TEXT   "   PC  AC  XR  YR  SP  SR(NVRBDIZC)"
		.BYTE   $0D,$0A,"! ",0
		
		.ORG 	$FFF0
PRINTVEC	.DW	OUTCH
INPVEC		.DW	IOF_CONIN
INPWVEC		.DW	IOF_CONINW
	

		 .ORG     $FFFA   		
NMIVECTOR       .DW   $0000			;
RSTVECTOR       .DW   COLD_START		; 
INTVECTOR 	.DW   INTERRUPT		; ROM VECTOR FOR IRQ

	.END
