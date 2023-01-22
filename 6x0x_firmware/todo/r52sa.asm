
;__R52SA___________________________________________________________________________________________
;
;	MINI ROM MONITOR FOR THE N8VEM 6502 HOST PROCESSOR (I/O BOARD, STAND ALONE)
;
;	WRITTEN BY: DAN WERNER -- 1/30/2011
;
;__________________________________________________________________________________________________
;
; DATA CONSTANTS
;__________________________________________________________________________________________________



DSKYCONSOLE	.EQU	0		; USE DSKY BOARD AS SYSTEM CONSOLE
SERIALCONSOLE	.EQU	1		; USE SERIAL PORT AS SYSTEM CONSOLE
COLOSSUS6X0X	.EQU	1		; USE COLOSSUS 6X0X HARDWARE
ORIGINAL6X0X	.EQU	0		; USE ORIGINAL 6X0X HARDWARE


M6X0X_IOSPACE		.EQU	$E000
M6X0X_SHADOW_ROM 	.EQU	$F000


;REGISTER		IO PORT		; FUNCTION
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
FLERR	 	.EQU	$4F		;
UNIT	 	.EQU	$50		;
TRACK	 	.EQU	$51		;
HEAD	 	.EQU	$52		;
FCMD	 	.EQU	$53		;
ST0	 	.EQU	$54		;
FLATCH_STORE	.EQU	$55		;
FLRETRY	 	.EQU	$56		;
SECTOR	 	.EQU	$57		;
KB_TEMP		.EQU	$58		;
DISPLAYBUF:	.EQU	$61		;
TEMPW		.EQU	$5E		;
ADDR		.EQU	$69		;

	  .IF ORIGINAL6X0X=1
DDRA		.EQU 	$F013
DDRB		.EQU 	$F012
DDRC		.EQU 	$F023
PORTA		.EQU 	$F01F
PORTB		.EQU 	$F010
PORTC		.EQU 	$F02F
	.ENDIF

	
	  .IF COLOSSUS6X0X=1
DDRA		.EQU 	M6X0X_IOSPACE+$CF3
DDRB		.EQU 	M6X0X_IOSPACE+$CF2
DDRC		.EQU 	M6X0X_IOSPACE+$DF3
DDRD		.EQU 	M6X0X_IOSPACE+$DF2
PORTA		.EQU 	M6X0X_IOSPACE+$CFF
PORTB		.EQU 	M6X0X_IOSPACE+$CF0
PORTC		.EQU 	M6X0X_IOSPACE+$DFF
PORTD		.EQU 	M6X0X_IOSPACE+$DF0
	.ENDIF
	

INBUFFER	.EQU	$0200		;

;*
;* HARDWARE I/O ADDRESSES
;*
IDELO		.EQU       $F120		; DATA PORT (LOW BYTE)
IDEERR		.EQU       $F121		; READ: ERROR REGISTER; WRITE: PRECOMP
IDESECTC	.EQU       $F122		; SECTOR COUNT
IDESECTN	.EQU       $F123		; SECTOR NUMBER
IDECYLLO	.EQU       $F124		; CYLINDER LOW
IDECYLHI	.EQU       $F125		; CYLINDER HIGH
IDEHEAD		.EQU       $F126		; DRIVE/HEAD
IDESTTS		.EQU       $F127		; READ: STATUS; WRITE: COMMAND
IDEHI		.EQU       $F128		; DATA PORT (HIGH BYTE)
IDECTRL		.EQU       $F12E		; READ: ALTERNATIVE STATUS; WRITE; DEVICE CONTROL
IDEADDR		.EQU       $F12F		; DRIVE ADDRESS (READ ONLY)

FMSR		.EQU	$F136		; ADDRESS OF MAIN STATUS REGISTER
FDATA		.EQU	$F137		; FLOPPY DATA REGISTER
FLATCH		.EQU	$F13A		; FLOPPY CONFIGURATION LATCH


  .IF ORIGINAL6X0X=1
UART1DATA:	.EQU	$F004		; SERIAL PORT 1 (I/O Card)
UART1STATUS:	.EQU	$F005		; SERIAL PORT 1 (I/O Card)
UART1COMMAND:	.EQU	$F006		; SERIAL PORT 1 (I/O Card)
UART1CONTROL:	.EQU	$F007		; SERIAL PORT 1 (I/O Card)
   .ENDIF
   
   
  .IF COLOSSUS6X0X=1
UART1DATA:	.EQU	M6X0X_IOSPACE+$FF4		; SERIAL PORT 1 (I/O Card)
UART1STATUS:	.EQU	M6X0X_IOSPACE+$FF5		; SERIAL PORT 1 (I/O Card)
UART1COMMAND:	.EQU	M6X0X_IOSPACE+$FF6		; SERIAL PORT 1 (I/O Card)
UART1CONTROL:	.EQU	M6X0X_IOSPACE+$FF7		; SERIAL PORT 1 (I/O Card)
   .ENDIF
  
   
;
; FDC CONFIGURATION LATCH OUTPUT BIT PATTERNS
MOTOR		.EQU	%00000000	; BIT PATTERN IN LATCH FOR MOTOR CONTROL (ON)
TERMCN		.EQU	%00000001	; BIT PATTERN IN LATCH TO WRITE A TC STROBE
RESETL		.EQU	%00000010	; BIT PATTERN IN LATCH TO RESET ALL BITS
MINI		.EQU	%00000100	; BIT PATTERN IN LATCH TO SET MINI MODE FDC9229 LOW DENS=1, HIGH DENS=0
PRECOMP		.EQU	%00100000	; BIT PATTERN IN LATCH TO SET WRITE PRECOMP 125 NS:
FDDENSITY	.EQU	%01000000	; BIT PATTERN IN LATCH TO FLOPPY LOW DENSITY (HIGH IS 0)
FDREADY		.EQU	%10000000	; BIT PATTERN IN LATCH TO FLOPPY READY (P-34):


 	.IF COLOSSUS6X0X=1
	.ORG    M6X0X_SHADOW_ROM 
	.ENDIF
		
	
 	.IF ORIGINAL6X0X=1		
	.ORG	$F800
	.ENDIF

;__COLD_START___________________________________________________
;
; PERFORM SYSTEM COLD INIT
;
;_______________________________________________________________        
COLD_START:
         	CLD				;  VERIFY DECIMAL MODE IS OFF

         	LDA	#(BRKROUTINE & $FF)
         	STA	IRQVECTOR
         	LDA	#(BRKROUTINE & $FF00)/256
         	STA	IRQVECTOR+1
         	
         	

		
		
       	  .IF SERIALCONSOLE=1			;
		JSR	SERIALINIT		; INIT SERIAL PORT
	  .ENDIF

       	  .IF DSKYCONSOLE=1			;
		JSR	DSKYINIT		; INIT DSKY PORT
	  .ENDIF
	  
	  	  
        ; 	BRK				; PERFORM BRK (START MONITOR)
         
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

               	
    
          .IF SERIALCONSOLE=1			;           	
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
               	JSR WRSER1			;
               	LDA ACC				; OUTPUT ACCUMULATOR
               	JSR PRINT_BYTE			;
               	LDA #$20			; OUTPUT 2 SPACES
               	JSR WRSER1			;
               	LDA #$20			;
               	JSR WRSER1			;
	       	LDA XREG			; OUTPUT X REGISTER
	       	JSR PRINT_BYTE			;
               	LDA #$20			; OUTPUT 2 SPACES
               	JSR WRSER1			;
               	LDA #$20			;
               	JSR WRSER1			;
	       	LDA YREG			; OUTPUT Y REGISTER
	       	JSR PRINT_BYTE			;
               	LDA #$20			; OUTPUT 2 SPACES
               	JSR WRSER1       		;
               	LDA #$20			;
               	JSR WRSER1			;
               	LDA SPTR			; OUTPUT STACK POINTER
	       	JSR PRINT_BYTE			;
               	LDA #$20			; OUTPUT 2 SPACES
               	JSR WRSER1     			;
               	LDA #$20			;
               	JSR WRSER1			;
      	       	LDA PREG			; OUTPUT STATUS REGISTER
	       	JSR PRINT_BYTE			; OUTPUT IN HEX
               	LDA #$0D			; PRINT NEW LINE
               	JMP WRSER1                	;
               



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
         	JSR WRSER1			;
         	LDA #$0A			;
         	JSR WRSER1			;
		
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


;__BOOT_________________________________________________________
;
; BOOT OS
; 
; B X
;_______________________________________________________________        
IOF_BOOT: 		

	        LDA #(INBUFFER & $FF)   	; SETUP WORK BUFFER
         	STA WORKPTR			;
         	LDA #(INBUFFER >> 8)    	;
         	STA WORKPTR +1 			;
         	
		JSR INCWORKPTR			; JUMP OVER "B"
		JSR INCWORKPTR			; JUMP OVER "O"
		JSR INCWORKPTR			; JUMP OVER "O"
		JSR INCWORKPTR			; JUMP OVER "T"
		
		JSR EATWHITESPACE		; SKIP OVER THE WHITESPACE
         	JSR HEXIN			;
         	STA UNIT			;
         	LDA #$00
         	STA SECTOR			;
         	STA TRACK			;
         	STA HEAD			;
         					;
         	LDA UNIT			;
         	CMP #$04			;
         	BEQ BOOTHDD
         	JSR SETUPDRIVE			;
         	LDA #$00
         	STA SECTOR			;
         	STA TRACK			;
         	STA HEAD			;
         	JSR READFL			;
		JMP $0200			; 	
		
BOOTHDD:		
		JSR IDE_SOFT_RESET		;
		JSR IDE_SETUP_LBA		;
		JSR IDE_READ_SECTOR		;
		
		JMP $0200			; 	
         	

         	
         	
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
	       	JSR WRSER1			;
	       	LDA #$0A			;
	       	JSR WRSER1			;     		       	
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
		JSR WRSER1			;
		LDA #$0A			;
		JSR WRSER1
		LDA #$3A			; OUTPUT ":" TO SCREEN
		JSR WRSER1			;
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
		JSR WRSER1			;
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
		JSR WRSER1			; PRINT ':'
    	     	LDA WORKPTR+1			; PRINT ADDRESS
	       	JSR PRINT_BYTE			; 
      	       	LDA WORKPTR			;
	       	JSR PRINT_BYTE			;
 	       	LDA #$2D			; LOAD '-'
	       	JSR WRSER1			; PRINT '-'
	       	LDY #$00			;
PRINT_MEM_LINE_LOOP:
		LDA (WORKPTR),Y			; LOAD NEXT BYTE
		JSR PRINT_BYTE	       		; PRINT BYTE
 	       	LDA #$20			; LOAD ' '
	       	JSR WRSER1			; PRINT ' '
	       	INY				; INCREMENT COUNTER
	       	CPY #$10			; HAVE WE PRINTED 16 ADDRESSES
	       	BNE PRINT_MEM_LINE_LOOP		; NO, LOOP
		LDA #$3A			; LOAD ':' INTO ACC
		JSR WRSER1			; PRINT ':'
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
	       	JSR WRSER1			;
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
		JMP WRSER1			; NO, PRINT CHAR AND RETURN
OUTASCII_DOT:	
		LDA #$2E			; A= '.'
		JMP WRSER1			; PRINT '.' AND RETURN
         	


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
PRINT_DIGIT_OUT:				;	
               JMP WRSER1              		; PRINT OUT CHAR
               


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
        	JSR WRSER1  			; PRINT CHAR IN ACC
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
		JSR CONINW			
        	CMP #$0D			; IS CR?
        	BEQ ENDINSTR			; YES, DONE WITH INPUT 
        	CMP #$08			; IS BACKSPACE?
        	BNE INSTR_NOTBS			; NO, SKUP BACKSPACE RTN
        	CPY #$00			; IS INDEX =0 ?
        	BEQ INSTR_EMPTY_BS		; YES, SKIP BACKSPACE
        	JSR WRSER1     			; OUTPUT CHAR TO SCREEN        	
        	DEY				; Y=Y-1
        	LDA #$00			; 
        	STA (STRPTR),Y			; NULL TERMINATE INPUT BUFFER
        	DEY				; Y=Y-1
        	JMP INSTR_SKIP_STORE       	; SKIP STORE OF CHAR TO INPUT BUFFER
INSTR_NOTBS        
        	STA (STRPTR),Y			; STORE CHAR IN KEYBAORD BUFFER
        	JSR WRSER1     			; OUTPUT CHAR TO SCREEN
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
	
       .ENDIF
	
       .IF DSKYCONSOLE=1	

;__PRINT_REG____________________________________________________
;
; PRINT OUT REGISTERS ON THE DISPLAY
;
;_______________________________________________________________    
PRINT_REG: 
 	LDA 	#CPUUP & $FF		;
	STA	TEMPWORD		;
	LDA	#(CPUUP & $FF00) / 256	;
	STA	TEMPWORD+1		;
	JSR	SEGDISPLAY		; DISPLAY 
	RTS       
       
;__COMMAND_PROCESSOR____________________________________________
;
; PROMPT FOR, INPUT, AND PROCESS INCOMMING USER COMMANDS
;
;_______________________________________________________________        
COMMAND_PROCESSOR:
	JSR	PRINT_REG
FRONTPANELLOOP1:		
	JSR	KB_Get			; GET KEY FROM KB
	CMP	#$14			; IS DEPOSIT?
	BNE	NOTDEPOSIT
	JMP	DODEPOSIT		; YES, JUMP
NOTDEPOSIT:	
	CMP	#$15			; IS EXAMINE?
	BNE	NOTEXAMINE
	JMP	DOEXAMINE		; YES, JUMP
NOTEXAMINE:	
	CMP	#$16			; IS GO?
	BNE 	NOTGO
	JMP	DOGO			; YES, JUMP
NOTGO:	
	CMP	#$17			; IS BO?
	BNE	NOTBOOTMENU
	JMP	DOBOOTMENU		; YES, JUMP
NOTBOOTMENU:	
	JMP	FRONTPANELLOOP1		; LOOP



;__DOBOOTMENU____________________________________________________________________________________________________________________ 
;
;	PERFORM BOOT FRONT PANEL ACTION
;________________________________________________________________________________________________________________________________
;
DOBOOTMENU:
	LDA 	#BOOT & $FF		;
	STA	TEMPWORD		;
	LDA	#(BOOT & $FF00) / 256	;
	STA	TEMPWORD+1		;
	JSR	SEGDISPLAY		; DISPLAY 
	LDA #$00
       	STA SECTOR			;
       	STA TRACK			;
       	STA HEAD			;
	JSR	KB_Get			; GET KEY FROM KB

	CMP	#$04			; IS IDE?
	BEQ	BOOTHDD			;
	BPL	BOOTEXIT		;
	JMP	BOOTF			;
BOOTHDD:				;
	JMP	BOOTH			;		
BOOTEXIT:
	JMP	COMMAND_PROCESSOR	; YES, JUMP
BOOTF:

         	STA UNIT			;
         					;
         	LDA UNIT			;
         	JSR SETUPDRIVE			;
         	LDA #$00
         	STA SECTOR			;
         	STA TRACK			;
         	STA HEAD			;
         	JSR READFL			;
		JMP $0200			; 			
BOOTH:		
		JSR IDE_SOFT_RESET		;
		JSR IDE_SETUP_LBA		;
		JSR IDE_READ_SECTOR		;
		
		JMP $0200			; 		
	


;__DOGO__________________________________________________________________________________________________________________________ 
;
;	PERFORM GO FRONT PANEL ACTION
;________________________________________________________________________________________________________________________________
;
DOGO:
	JSR 	GETADDR			; GET ADDRESS INTO TEMPWORD
	JMP	(TEMPWORD)		;



;__DODEPOSIT________________________________________________________________________________________________________________________ 
;
;	PERFORM DEPOSIT FRONT PANEL ACTION
;________________________________________________________________________________________________________________________________
;
DODEPOSIT:
	JSR 	GETADDR			; GET ADDRESS INTO TEMPWORD	
DEPOSITLOOP:
	LDA	TEMPWORD+1		;
	AND	#$F0			; CLEAR LOW NIBBLE
	LSR	A			; SHIFT HIGH TO LOW NIBBLE
	LSR	A			;
	LSR	A			;
	LSR	A			;
	STA	DISPLAYBUF+7		; DISPLAY
	LDA	TEMPWORD+1		;
	AND	#$0F			; CLEAR HIGH NIBBLE
	STA	DISPLAYBUF+6		; DISPLAY
	AND	#$F0			; CLEAR LOW NIBBLE
	LDA	TEMPWORD		;
	LSR	A			; SHIFT HIGH TO LOW NIBBLE
	LSR	A			;
	LSR	A			;
	LSR	A			;
	STA	DISPLAYBUF+5		; DISPLAY
	LDA	TEMPWORD		; RESTORE VALUE
	AND	#$0F			; CLEAR HIGH NIBBLE
	STA	DISPLAYBUF+4		; DISPLAY
	LDA	#$10			; CLEAR DIGITS ON DISPLAY
	STA	DISPLAYBUF+3		;
	STA	DISPLAYBUF+2		;
	JSR	GETVALUE		; GET VALUE TO STORE
	LDY	#$00
	STA	(TEMPWORD),Y		; STORE VALUE			

DEPOSITGETKEY:
	JSR	KB_Get			; GET KEY FROM KB
	CMP	#$12			; [CL] PRESSED, EXIT
	BEQ	DEPOSITEXIT		;
	CMP	#$13			; [EN] PRESSED, INC ADDRESS AND LOOP
	BEQ	DEPOSITFW		; 
	CMP	#$14			; [DE] PRESSED, PROMPT FOR NEW ADDRESS
	BEQ	DODEPOSIT		;
	JMP	DEPOSITGETKEY		; NO VALID KEY, LOOP
DEPOSITFW:
	JSR	INCTEMPWORD		; INC ADDRESS AND GET NEXT VALUE
	JMP 	DEPOSITLOOP		;	
DEPOSITEXIT:
	JMP	COMMAND_PROCESSOR	; GO BACK TO MAIN LOOP



;__DOEXAMINE________________________________________________________________________________________________________________________ 
;
;	PERFORM EXAMINE FRONT PANEL ACTION
;________________________________________________________________________________________________________________________________
;
DOEXAMINE:
	JSR 	GETADDR			; GET ADDRESS INTO X
DOEXAMINE1:	
EXAMINELOOP:
	LDA	TEMPWORD+1
	AND	#$F0			; CLEAR LOW NIBBLE
	LSR	A			; SHOW HIGH NIBBLE IN DISP 7
	LSR	A			;
	LSR	A			;
	LSR	A			;
	STA	DISPLAYBUF+7		;
	LDA	TEMPWORD+1		;
	AND	#$0F			; CLEAR HIGH NIBBLE
	STA	DISPLAYBUF+6		; DISPLAY LOW NIBBLE IN DISP 6
	LDA	TEMPWORD		;
	AND	#$F0			;
	LSR	A			; SHOW HIGH NIBBLE IN DISP 5
	LSR	A			;
	LSR	A			;
	LSR	A			;
	STA	DISPLAYBUF+5		;
	LDA	TEMPWORD		;
	AND	#$0F			; CLEAR HIGH NIBBLE
	STA	DISPLAYBUF+4		; DISPLAY LOW NIBBLE IN DISP 6
	LDA	#$10			; CLEAR OUT DISP 2 & 3
	STA	DISPLAYBUF+3		;
	STA	DISPLAYBUF+2		;
	LDY	#$00			;
	LDA	(TEMPWORD),Y		; GET VALUE FROM ADDRESS IN X
	AND	#$F0			;
	LSR	A			; DISPLAY HIGH NIB IN DISPLAY 1
	LSR	A			;
	LSR	A			;
	LSR	A			;
	STA	DISPLAYBUF+1		;
	LDA	(TEMPWORD),Y		; GET VALUE FROM ADDRESS IN X                                   
	AND	#$0F			; CLEAR OUT HIGH NIBBLE
	STA	DISPLAYBUF		;
	JSR	HEXDISPLAY		; DISPLAY BUFFER ON DISPLAYS
EXAMINEGETKEY:
	JSR	KB_Get			; GET KEY FROM KB
	CMP	#$12			; [CL] PRESSED, EXIT
	BEQ	EXAMINEEXIT		;
	CMP	#$13			; [EN] PRESSED, INC ADDRESS AND LOOP
	BEQ	EXAMINEFW		; 
	CMP	#$15			; [DE] PRESSED, PROMPT FOR NEW ADDRESS
	BEQ	DOEXAMINE		;
	JMP	EXAMINEGETKEY		; NO VALID KEY, LOOP
EXAMINEFW:
	JSR	INCTEMPWORD		; X++
	JMP 	DOEXAMINE1		;	
EXAMINEEXIT:
	JMP	COMMAND_PROCESSOR	;


;__GETADDR_______________________________________________________________________________________________________________________ 
;
;	GET ADDRESS FROM FRONT PANEL
;________________________________________________________________________________________________________________________________
;
GETADDR:
	JMP	GETADDRCLEAR		; 
GETADDR1:
	LDA 	#ADDR & $FF		;
	STA	TEMPWORD		;
	LDA	#(ADDR & $FF00) / 256	;
	STA	TEMPWORD+1		;
	JSR	SEGDISPLAY		; 
GETADDRLOOP:
	JSR	KB_Get			; GET KEY
	CMP	#$10			;
	BMI	GETADDRNUM		; NUMBER PRESSED, STORE IT
	CMP	#$13			; EN PRESSED, DONE
	BEQ	GETADDRDONE		;
	CMP	#$12			; CLEAR PRESSED, CLEAR
	BEQ	GETADDRCLEAR		; 
	JMP	GETADDRLOOP		; INVALID KEY, LOOP
GETADDRDONE:
	LDA	DISPLAYBUF+1		; GET DIGIT IN DISPLAY 1
	ASL	A			; ROTATE IT TO HIGH NIBBLE
	ASL	A			;
	ASL	A			;
	ASL	A			;
	STA	TEMPW			; STORE IT IN "A"	
	LDA	DISPLAYBUF		; GET DIGIT IN DISPLAY 0
	AND	#$0F			; CLEAR HIGH NIBBLE
	CLC
	ADC	TEMPW
	STA	TEMPWORD
	LDA	DISPLAYBUF+3		; GET DIGIT IN DISPLAY 3
	ASL	A			; ROTATE IT TO HIGH NIBBLE
	ASL	A			;
	ASL	A			;
	ASL	A			;
	STA	TEMPW			;
	LDA	DISPLAYBUF+2		; GET DIGIT IN DISPLAY 2	
	AND	#$0F			; CLEAR HIGH NIBBLE
	CLC
	ADC	TEMPW			;
	STA	TEMPWORD+1		;	
	LDA	#$10			; CLEAR OUT DISPLAYS 0,1,2 & 3
	STA	DISPLAYBUF		;
	STA	DISPLAYBUF+1		;
	STA	DISPLAYBUF+2		;
	STA	DISPLAYBUF+3		;
	RTS	
GETADDRNUM:
	PHA				; STORE A
	LDA	DISPLAYBUF+2		; SHIFT BYTES IN DISPLAY BUF TO THE LEFT
	STA  	DISPLAYBUF+3		;
	LDA	DISPLAYBUF+1		;	
	STA	DISPLAYBUF+2		;
	LDA	DISPLAYBUF		;	
	STA	DISPLAYBUF+1		;
	PLA				; DISPLAY KEYSTROKE IN RIGHT MOST DISPLAY (0)
	STA	DISPLAYBUF		;
	JMP	GETADDRDISP		;
GETADDRCLEAR:
	LDA	#$12			; CLEAR OUT DISPLAYS 0,1,2 & 3
	STA	DISPLAYBUF		;
	STA	DISPLAYBUF+1		;
	STA	DISPLAYBUF+2		;
	STA	DISPLAYBUF+3		;	
GETADDRDISP:
	LDA	DISPLAYBUF		; ENCODE DIGITS IN DISPLAY BUFFER TO DISPLAY
	JSR 	DECODEDISPLAY		;
	STA	ADDR			;
	LDA	DISPLAYBUF+1		;
	JSR 	DECODEDISPLAY		;
	STA	ADDR+1			;
	LDA	DISPLAYBUF+2		;
	JSR 	DECODEDISPLAY		;
	STA	ADDR+2			;
	LDA	DISPLAYBUF+3		;
	JSR 	DECODEDISPLAY		;
	STA	ADDR+3			;
	JMP	GETADDR1		;

	.ENDIF
	
	
;__SETUPDRIVE__________________________________________________________________________________________________________________________ 
;
;	SETUP FLOPPY DRIVE SETTINGS 
;________________________________________________________________________________________________________________________________
;
;
;
SETUPDRIVE:
	LDA	#RESETL			; RESET SETTINGS
	ORA	#MINI			; SELECT MINI FLOPPY (low dens=1, high dens=0)
	ORA	#PRECOMP		; SELECT PRECOMP 
	ORA	#FDDENSITY		; SELECT DENSITY
	ORA	#FDREADY		; SELECT READY SIGNAL
	STA	FLATCH_STORE		; SAVE SETTINGS
	JSR	CHECKINT		;
	LDA	#$03			; SPECIFY COMMAND
	JSR	PFDATA			; OUTPUT TO FDC
	LDA	#$7F			; 6 MS STEP, 480 MS HEAD UNLOAD
	JSR	PFDATA			; OUTPUT TO FDC
	LDA	#$05			; 508 MS HEAD LOAD, NON-DMA MODE
	JSR	PFDATA			; OUTPUT TO FDC
	JSR	CHECKINT		;
	JSR	CHECKINT		;
	JSR	CHECKINT		;
	JSR	CHECKINT		;
	JSR	CHECKINT		;
	JSR	CHECKINT		;
	JSR	RECAL			;
	LDA	#39			;
	STA	TRACK			;
	JSR	SETTRACK		;
	JSR	RECAL			;
	JMP	RECAL			;

;__OUTFLATCH__________________________________________________________________________________________________________________________ 
;
;	SEND SETTINGS TO FLOPPY CONTROLLER
;________________________________________________________________________________________________________________________________
;
OUTFLATCH:
	LDA	FLATCH_STORE		; SET A TO SETTINGS
	STA	FLATCH			; OUTPUT TO CONTROLLER
	RTS

	
;__READFL________________________________________________________________________________________________________________________ 
;
; 	READ A FLOPPY SECTOR 	
;________________________________________________________________________________________________________________________________
;	
READFL:
	LDA	#$00
	STA	FLRETRY
	LDA	#$46			; BIT 6 SETS MFM, 06H IS READ COMMAND
	STA	FCMD
	JSR	DSKOP
	CMP	#$00
	BEQ	READFLDONE
	INC	FLRETRY
	LDA	FLRETRY
	CMP	#$06
	BNE	READFL
	LDA	#$FF	
READFLDONE:
	RTS	
        	
;__DSKOP__________________________________________________________________________________________________________________________ 
;
; 	PERFORM A DISK OPERATION 	
;________________________________________________________________________________________________________________________________
;		

DSKOP:
	SEI
	JSR	CHECKINT		; CHECK INTERRUPT STATUS, MAKE SURE IT IS CLEAR
	CMP	#$FF			; DID IT RTSURN WITH ERROR CODE?
	BEQ	DSKEXIT			; IF YES, EXIT WITH ERROR CODE
					;
 					;
	LDA	FLATCH_STORE		; POINT TO FLATCH
	AND	#%11111101		; SET MOTOR ON
	STA	FLATCH_STORE		; POINT TO FLATCH	
	JSR	OUTFLATCH		; OUTPUT TO CONTROLLER
					;	
	JSR	SETTRACK		; PERFORM SEEK TO TRACK
					;
	JMP	RDD_POLL		;
DSKEXIT:	
	LDA	FLATCH_STORE		; POINT TO FLATCH
	ORA	#%00000010		; SET MOTOR OFF
	STA	FLATCH_STORE		; POINT TO FLATCH	
	JSR	OUTFLATCH		; OUTPUT TO CONTROLLER
	LDA	#$FF			; SET -1 IF ERROR
	CLI
	RTS

SNDFDWR:


	LDY	#$00			; BYTES/SECTOR COUNT
	LDA	HEAD			; GET HEAD SELECTION
	AND	#$01			; INSURE SINGLE BIT
	ASL	A			;
	ASL	A			; MOVE HEAD TO BIT 2 POSITION
	ORA	UNIT			; OR HEAD TO UNIT BYTE IN COMMAND BLOCK
	STA	UNIT			; STORE IN UNIT
	LDA	FCMD			;
	JSR	PFDATA			; PUSH COMMAND TO I8272
	LDA	UNIT			;
	JSR	PFDATA			; 
	LDA	TRACK			;
	JSR	PFDATA			; 
	LDA	HEAD			;
	JSR	PFDATA			; 
	CLC				;
	LDA	SECTOR			;
	ADC	#$01			;
	JSR	PFDATA			; 
	LDA	#$02			;
	JSR	PFDATA			; WHAT DENSITY
	LDA	#$09			;
	JSR	PFDATA			; ASSUME SC (SECTOR COUNT)  EOT
	LDA	#$0D			;
	JSR	PFDATA			; WHAT GAP IS NEEDED
	LDA	#$FF			; DTL, IS THE LAST COMMAND BYTE TO I8272
	JSR	PFDATAS
	RTS
; PERFORM READ
; FROM READ TO READ MUST NOT EXCEED 25US WORST CASE MIN.
;	
RDD_POLL:
	JSR	SNDFDWR			; 
RDS1:	LDA	FMSR			; GET STATUS  
	BPL	RDS1
	AND	#%00100000		; EXECUTION MODE? 
	BEQ	DSKOPEND		; NO, ERROR 
	LDA	FDATA			; GET DATA 
	STA	INBUFFER,Y		; WRITE IT 
	INY                                        
	BNE	RDS1			; KEEP GOING 
RDS2	LDA	FMSR			; GET STATUS
	BPL	RDS2
	AND	#%00100000		; EXECUTION MODE?
	BEQ	DSKOPEND		; NO, ERROR
	LDA	FDATA			; GET DATA
	STA	INBUFFER+256,Y		; WRITE IT
	INY
	BNE	RDS2			; KEEP GOING	

DSKOPEND:
	LDA	FLATCH_STORE		; POINT TO FLATCH
	ORA	#%00000001		;
	STA	FLATCH_STORE		; SET TC
	JSR	OUTFLATCH		; OUTPUT TO CONTROLLER
	NOP				;
	NOP				; 2 MICROSECOND DELAY
	NOP				;
	NOP				; 
	LDA	FLATCH_STORE		; POINT TO FLATCH
	AND	#%11111110		;
	STA	FLATCH_STORE		; CLEAR TC
	JSR	OUTFLATCH		; OUTPUT TO CONTROLLER
	PHA				;
	PLA				;
	PHA				;
	PLA				; 2 MICROSECOND DELAY
	LDA	FLATCH_STORE		; POINT TO FLATCH
	ORA    	#%00000010		; MOTOR OFF
	STA	FLATCH_STORE		; POINT TO FLATCH
	JSR	OUTFLATCH		; OUTPUT TO CONTROLLER					;
					;
	JSR	GFDATA			;GET ERROR TYPE
	STA	FLERR

;* CLEAR OUT ANY REMAINING DATA
RESUL3:	
	JSR	GFDATA			;READ BYTE FROM FDC
	BNE	RESUL3			;CLEAR THEM ALL
	LDA	FLERR			;
	AND	#%11000000		;

	RTS

;__SETTRACK__________________________________________________________________________________________________________________________ 
;
; 	SEEK TO A TRACK ON GIVEN UNIT
; 	A: TRACK #
;________________________________________________________________________________________________________________________________
;
SETTRACK:
					; ANY INTERUPT PENDING
					; IF YES FIND OUT WHY/CLEAR
	JSR	CHECKINT		; CHECK INTERRUPT STATUS, MAKE SURE IT IS CLEAR
	CMP	#$FF			; DID IT RTSURN WITH ERROR CODE?
	BNE	SETTRK1
	JMP	SETTRKEXIT		;

					;
SETTRK1:		
	LDA	TRACK			; GET TRACK
	CMP	#$00			;
	BEQ	RECAL			; IF 0 PERFORM RECAL INSTEAD OF SEEK
	LDA	#$0F			; SEEK COMMAND
	JSR	PFDATA			; PUSH COMMAND
	LDA	UNIT			; SAY WHICH UNIT
	JSR	PFDATA			; SEND THAT
	LDA	TRACK			; TO WHAT TRACK
	JSR	PFDATA			; SEND THAT TOO
	JMP	WAINT			; WAIT FOR INTERRUPT SAYING DONE
RECAL:
	LDA	#$07			; RECAL TO TRACK 0
	JSR	PFDATA			; SEND IT
	LDA	UNIT			; WHICH UNIT
	JSR	PFDATA			; SEND THAT TOO
;
WAINT:
;
SETTRK2:	
	JSR	CHECKINT			
	LDA	FMSR			; READ SEEK STATUS
	AND	#%00001111		; ANY DRIVES SEEKING?
	BNE	SETTRK2			; YES, WAIT FOR THEM
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
	PHA				; SAVE DATA BYTE
WRF1:
	LDA	FMSR			; READ FDC STATUS	
	TAX
	AND	#$80			;	
	BEQ	WRF1			; FDC IS NOT READY, WAIT FOR IT
	TXA
	AND	#$40			; TEST DIO BIT
	BNE	WRF2			; FDC IS OUT OF SYNC
	PLA				; RESTORE DATA
	STA	FDATA			; WRITE TO FDC	

	PHA
	PLA
	PHA
	PLA
	PHA
	PLA
	PHA
	PLA
	PHA
	PLA
	PHA
	PLA
	PHA
	PLA
	PHA
	PLA
	
	RTS
; FDC IS OUT OF SYNC CLEAR IT OUT AND RE-TRY
WRF2:
	LDA	FDATA			; READ DATA REGISTER	
	JMP	WRF1			; AND CONTINUE

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
	PHA				; SAVE DATA BYTE
WRF1S:
	LDA	FMSR			; READ FDC STATUS
	TAX	
	AND	#$80			;	
	BEQ	WRF1S			; FDC IS NOT READY, WAIT FOR IT
	TXA
	AND	#$40			; TEST DIO BIT
	BNE	WRF2S			; FDC IS OUT OF SYNC
	PLA				; RESTORE DATA
	STA	FDATA			; WRITE TO FDC	
	RTS
; FDC IS OUT OF SYNC CLEAR IT OUT AND RE-TRY
WRF2S:
	LDA	FDATA			; READ DATA REGISTER
	JMP	WRF1S			; AND CONTINUE
	
	
	
;__CHECKINT__________________________________________________________________________________________________________________________ 
;
; CHECK FOR ACTIVE FDC INTERRUPTS BEFORE GIVING I8272 COMMANDS
; POLL RQM FOR WHEN NOT BUSY AND THEN SEND FDC
; SENSE INTERRUPT COMMAND.  IF IT RTSURNS WITH NON ZERO
; ERROR CODE, PASS BACK TO JSRING ROUTINE FOR HANDLING
;________________________________________________________________________________________________________________________________
;
CHECKINT:
	LDA	FMSR			; READING OR WRITING IS KEYS TO D7 RQM
	AND	#$80
	BEQ	CHECKINT		; WAIT FOR RQM TO BE TRUE. WAIT UNTIL DONE
	LDA	FMSR			; READING OR WRITING IS KEYS TO D7 RQM
	AND	#$40			; WAITING FOR INPUT?
	BEQ	SENDINT
	RTS
	
ERRCLR:	
	LDA	FDATA			; CLEAR THE JUNK OUT OF DATA REGISTER
	LDA	FMSR			; CHECK WITH RQM
	AND	#$80			; IF STILL NOT READY, READ OUT MORE JUNK
	BEQ	ERRCLR			;
	LDA	#$FF			; RETURN ERROR CODE -1
					;
	RTS

;__SENDINT__________________________________________________________________________________________________________________________ 
;
; SENSE INTERRUPT COMMAND
;________________________________________________________________________________________________________________________________
;			
SENDINT:
	LDA	#$08			; SENSE INTERRUPT COMMAND
	JSR	PFDATA			; SEND IT
	JSR	GFDATA			; GET RESULTS
	STA	ST0			; STORE THAT
	AND	#$C0			; MASK OFF INTERRUPT STATUS BITS
	CMP	#$80			; CHECK IF INVALID COMMAND
	BEQ	ENDSENDINT		; YES, EXIT
	JSR	GFDATA			; GET ANOTHER (STATUS CODE 1)
	LDA	ST0			; GET FIRST ONE
	AND	#$C0			; MASK OFF ALL BUT INTERRUPT CODE 00 IS NORMAL
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
	LDA	FMSR			; GET STATUS
	TAX				;
	AND	#%10000000		; NOT READY, WAIT
	BEQ	GFDATA			;
	TXA
	AND	#%01000000		; ANY DATA FOR US?
	BEQ	GFDATA1			; NO, SKIP IT
	LDA	FDATA			; GET FDC DATA
GFDATA1:
	RTS			

;*__IDE_READ_SECTOR___________________________________________________________________________________
;*
;*  READ IDE SECTOR (IN LBA) INTO BUFFER
;*     
;*____________________________________________________________________________________________________				
IDE_READ_SECTOR:	
		JSR	IDE_WAIT_BUSY_READY	; MAKE SURE DRIVE IS READY TO PROCEED
		CMP	#$00			;
		BNE	IDE_READ_SECTOR_ERR	; ERROR, ABORT
		JSR	IDE_SETUP_LBA		; TELL DRIVE WHAT SECTOR IS REQUIRED
		LDA    	#$20			;  		
		STA    	IDESTTS			; $20 = IDE 'READ SECTOR' COMMAND 		
		JSR	IDE_WAIT_BUSY_READY	; MAKE SURE DRIVE IS READY TO PROCEED
		CMP	#$00			;
		BNE	IDE_READ_SECTOR_ERR	; ERROR, ABORT
		JSR	IDE_TEST_ERROR		; ENSURE NO ERROR WAS REPORTED
		CMP	#$00			;
		BNE	IDE_READ_SECTOR_ERR	; ERROR, ABORT
		JSR	IDE_WAIT_BUFFER		; WAIT FOR FULL BUFFER SIGNAL FROM DRIVE
		CMP	#$00			;
		BNE	IDE_READ_SECTOR_ERR	; ERROR, ABORT
		JSR	IDE_READ_BUFFER		; GRAB THE 256 WORDS FROM THE BUFFER
		LDA	#$00			; ZERO = 1 ON RETURN = OPERATION OK
		RTS
IDE_READ_SECTOR_ERR:
		LDA    #$02
		RTS
		
		
;*__IDE_SOFT_RESET____________________________________________________________________________________
;*
;*  SOFT RESET IDE CHANNEL
;*     
;*____________________________________________________________________________________________________				
IDE_SOFT_RESET
		LDA    	#%00000110		; NO INTERRUPTS, RESET DRIVE = 1
		STA    	IDECTRL			;
		LDA    	#%00000010		; NO INTERRUPTS, RESET DRIVE = 0
		STA    	IDECTRL			;
		JSR	IDE_WAIT_BUSY_READY	;
		RTS				;

;*__IDE_WAIT_BUSY_READY_______________________________________________________________________________
;*
;*  WAIT FOR IDE CHANNEL TO BECOME READY
;*     
;*____________________________________________________________________________________________________				
IDE_WAIT_BUSY_READY:
		LDX	#$00			;
		LDY	#$00			;
		
IDE_WBSY:
		INX				;
		CPX	#$00			; TIMED OUT?
		BEQ	IDE_TO			; YUP, EXIT
		LDA     IDESTTS			; READ ERROR REG
		AND    	#%11000000		; MASK OFF BUSY AND RDY BITS
		CMP   	#%01000000		; WE WANT BUSY(7) TO BE 0 AND RDY(6) TO BE 1
		BNE	IDE_WBSY		;
		LDA	#$00			;
		RTS
IDE_TO:
		INY
		CPY	#$1E			;
		BNE	IDE_WBSY
		LDA     #$FF			; ZERO 0 = TIMED OUT
		RTS
				
		
;*__IDE_TEST_ERROR____________________________________________________________________________________
;*
;*  TEST FOR IDE ERROR
;*     
;*____________________________________________________________________________________________________				
IDE_TEST_ERROR:		
		LDA    	IDESTTS			;
		AND    	#%00000001		; MASK OFF BUSY AND RDY BITS
		BNE	IDE_ERR			;		
		LDA	#$00			;
		RTS
IDE_ERR:
		LDA    IDEERR			; READ ERROR FLAGS
		LDA    #$FF			; ZERO 0 = ERROR
		RTS				;
						;
;*__IDE_WAIT_BUFFER____________________________________________________________________________________
;*
;*  WAIT FOR IDE BUFFER TO FILL
;*     
;*____________________________________________________________________________________________________				
IDE_WAIT_BUFFER:
		LDX	#$00			;
		LDY	#$00			;
IDE_WDRQ:
		INX				;
		CPX	#$FF			;
		BEQ	IDE_TO2			;
		LDA     IDESTTS			; WAIT FOR DRIVE'S 512 BYTE READ BUFFER 
		AND     #%00001000		;
		BEQ	IDE_WDRQ		;
		LDA	#$00			; ZERO 1 = OK
		RTS
IDE_TO2:
		INY				;
		CPY	#$FF			;
		BNE	IDE_WDRQ		;
		LDA     #$FF			; CARRY 0 = TIMED OUT
		RTS
		
;*__IDE_READ_BUFFER___________________________________________________________________________________
;*
;*  READ IDE BUFFER LITTLE ENDIAN
;*     
;*____________________________________________________________________________________________________				
IDE_READ_BUFFER:
		LDX    	#$00			; INDEX
IDEBUFRD:
		LDA     IDELO			; LOW BYTE OF WORD FIRST
		STA	INBUFFER,X		;
		INX				;
		LDA    	IDEHI			; THEN HIGH BYTE OF WORD
		STA	INBUFFER,X		;
		INX				;
		CPX    	#$00			;
		BNE	IDEBUFRD		;
IDEBUFRD1:
		LDA     IDELO			; LOW BYTE OF WORD FIRST
		STA	INBUFFER+256,X		;
		INX				;
		LDA    	IDEHI			; THEN HIGH BYTE OF WORD
		STA	INBUFFER+256,X		;
		INX				;
		CPX    	#$00			;
		BNE	IDEBUFRD1		;		
		RTS				;

		
;*__IDE_SETUP_LBA_____________________________________________________________________________________
;*
;*  SETUP LBA DATA
;*     
;*____________________________________________________________________________________________________				
IDE_SETUP_LBA:	   

		LDA    	#$01			;  
		STA    	IDESECTC		; SET SECTOR COUNT = 1
		LDA    	SECTOR			;
		STA    	IDESECTN		; SET LBA 0:7
		LDA    	TRACK			;
		STA    	IDECYLLO		; SET LBA 8:15
		LDA    	HEAD			;
		STA    	IDECYLHI		; SET LBA 16:23
		LDA    	#$E1			;
		STA    	IDEHEAD			; SET LBA 24:27 + BITS 5:7=111
		RTS
		
        	        	
;__________________________________________________________________________________________________________	
	
;__INCTEMPWORD__________________________________________________
;
; INCREMENT THE 16BIT WORK POINTER
;
;_______________________________________________________________        
INCTEMPWORD: 		
		INC TEMPWORD			; INCREMENT LOWBYTE
		BNE INCTEMPWORD_OUT		; NOT ZERO?, DONE
		INC TEMPWORD+1			; ZERO, INC HIGH BYTE
INCTEMPWORD_OUT:
		RTS				; RETURN
		
;__INCTEMPWORD1_________________________________________________
;
; INCREMENT THE 16BIT WORK POINTER
;
;_______________________________________________________________        
INCTEMPWORD1: 		
		INC TEMPWORD1			; INCREMENT LOWBYTE
		BNE INCTEMPWORD1_OUT		; NOT ZERO?, DONE
		INC TEMPWORD1+1			; ZERO, INC HIGH BYTE
INCTEMPWORD1_OUT:
		RTS				; RETURN


;__LOAD_________________________________________________________

; LOAD A MOTOROLA FORMATTED HEX FILE
; 
;_______________________________________________________________                 	         	
LOAD:
	JSR	CONINW				;
	CMP	#'S'				;
	BNE	LOAD				; FIRST CHAR NOT (S)
	JSR	CONINW				; READ CHAR
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
	JSR 	INCTEMPWORD1			;
	JMP	LOAD11				;

LOAD15:
	INC	CKSM				;
	BEQ	LOAD				;
LOAD19:
	LDA	#'?'				;
	JSR	WRSER1				;
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
	JSR	CONINW				;
	PHA					;
	JSR	WRSER1				;
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
		
  	
;__RDSER1________________________________________________________________________________________________________________________
;
;	READ CHARACTER FROM UART TO (A)
;________________________________________________________________________________________________________________________________
;
RDSER1:
	LDA	UART1STATUS		; GET STATUS REGISTER
	AND	#%00001000		; IS RX READY
	BEQ	RDSER1N			; NO, INDICATE NO CHAR
	LDA	UART1DATA		; GET DATA CHAR
	RTS
RDSER1N:
	LDA	#$FF			;
	RTS		


CONINW:
	JSR	RDSER1
	CMP	#$FF
	BEQ	CONINW	
	RTS
	
;__WRSER1________________________________________________________________________________________________________________________
;
;	WRITE CHARACTER(A) TO UART
;________________________________________________________________________________________________________________________________
;
WRSER1:	
	PHA
WRSER1a:
	LDA	UART1STATUS		; GET STATUS
	AND	#%00010000		; IS TX READY
	BEQ	WRSER1a			; NO, WAIT FOR IT
	PLA
	STA	UART1DATA		; WRITE DATA
	RTS	


;__SERIALINIT____________________________________________________________________________________________________________________
;
;	INITIALIZE SERIAL PORTS
;________________________________________________________________________________________________________________________________
;
SERIALINIT:
	LDA 	#$00			; RESET UART
	STA	UART1STATUS		;
	LDA	#$0B			; 
	STA	UART1COMMAND		;
	LDA	#$1E			; 9600, 8 BITS, NO PARITY, 1 STOP BIT
	STA	UART1CONTROL		;
	RTS		

	.IF SERIALCONSOLE=1			
					
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
ERROR	 	.BYTE   $0D,$0A,"? INVALID COMMAND",$0D,0
INERROR		.BYTE   $0D,$0A,"? INVALID HEX NUMBER",$0D,0
; STRINGS FOR REGISTER DISPLY
REGDATA 	.BYTE   $0D,$0A,
		.TEXT   "   PC  AC  XR  YR  SP  SR"
		.BYTE   $0D,$0A,"! ",0
		
		
		.ENDIF
		

       	  .IF DSKYCONSOLE=1
		
;__GETVALUE______________________________________________________________________________________________________________________ 
;
;	GET VALUE FROM FRONT PANEL
;________________________________________________________________________________________________________________________________
;
GETVALUE:
	LDA	#$12			;
	STA	DISPLAYBUF		;
	STA	DISPLAYBUF+1		;
GETVALUE1:
	JSR	HEXDISPLAY		; 	
GETVALUELOOP:
	JSR	KB_Get			;	
	CMP	#$10			;
	BMI	GETVALUENUM		; NUMBER PRESSED, STORE IT
	CMP	#$13			; EN PRESSED, DONE
	BEQ	GETVALUEDONE		;
	CMP	#$12			; CLEAR PRESSED, CLEAR
	BEQ	GETVALUECLEAR		; 
	JMP	GETVALUELOOP		; INVALID KEY, LOOP
GETVALUEDONE:
	LDA	DISPLAYBUF+1		;
	ASL	A			;
	ASL	A			;
	ASL	A			;
	ASL	A			;
	STA	TEMPW			;
	LDA	DISPLAYBUF		;	
	AND	#$0F			;
	CLC
	ADC	TEMPW			;
	PHA
	LDA	#$10			;
	STA	DISPLAYBUF		;
	STA	DISPLAYBUF+1		;
	PLA				;
	RTS				; RETURN		
GETVALUENUM:
	STA	TEMPW			;
	LDA	DISPLAYBUF		;	
	STA	DISPLAYBUF+1		;
	LDA	TEMPW			;
	STA	DISPLAYBUF+0		;
	JMP	GETVALUE1		;
GETVALUECLEAR:

	
;__DSKYINIT__________________________________________________________________________________________
;
;  SETUP DSKY
;     
;____________________________________________________________________________________________________
DSKYINIT:
	LDA	#$FF			;
	STA	DDRA			;
	STA	DDRC			;
	LDA	#$00			;
	STA	DDRB			;
	LDA	ADDRROM+7		;
	STA	ADDR+7			;
	LDA	ADDRROM+6		;
	STA	ADDR+6			;
	LDA	ADDRROM+5		;
	STA	ADDR+5			;
	LDA	ADDRROM+4		;
	STA	ADDR+4			;
	JSR	SERIALINIT		;
	RTS
	
;__KB_Get____________________________________________________________________________________________
;
;  Get a Single Key and Decode
;     
;____________________________________________________________________________________________________
KB_Get:
KB_Get_Loop:				; WAIT FOR KEY
	JSR	KB_Scan			;  Scan KB Once
	CMP	#$00			;  Null?
	BEQ 	KB_Get_Loop		;  Loop while not zero
	STA	TEMPBYTE
	LDA	#$4F			;  Scan All Col Lines
	STA 	PORTC			;  Send to Column Lines
        JSR    	KB_Scan_Delay		;  Delay to allow lines to stabilize
KB_Clear_Loop:				; WAIT FOR KEY TO CLEAR
	LDA	PORTB			;  Get Rows
	CMP	#$00 			;  Anything Pressed?
	BNE	KB_Clear_Loop		;  Yes, Exit.
	LDY	#$00			;
KB_Get_LLoop:
	LDA	KB_DECODE,Y		;  Point to beginning of Table	
	CMP	TEMPBYTE		;
	BEQ	KB_Get_Done		;  Found, Done
	INY				;  B + 1	
	BNE	KB_Get_LLoop		;  Not Found, Loop until EOT				
KB_Get_Done:
	TYA				;
	RTS				;



;__KB_Scan____________________________________________________________________________________________
;
;  SCan Keyboard Matrix for an input
;     
;____________________________________________________________________________________________________
KB_Scan:

	LDA    	#$00			;
	STA	KB_TEMP			;  
	LDA	#$41			;  Scan Col One
	STA 	PORTC			;  Send to Column Lines
        JSR     KB_Scan_Delay		;  Delay to allow lines to stabilize
	LDA	PORTB			;  Get Rows
	CMP	#$00 			;  Anything Pressed?
	BNE	KB_Scan_Found		;  Yes, Exit.

	LDA    	#$40			;
	STA	KB_TEMP			;  
	LDA	#$42			;  Scan Col Two
	STA 	PORTC			;  Send to Column Lines
        JSR 	KB_Scan_Delay		;  Delay to allow lines to stabilize
	LDA	PORTB			;  Get Rows
	CMP	#$00 			;  Anything Pressed?
	BNE	KB_Scan_Found		;  Yes, Exit.

	LDA    	#$80			;
	STA	KB_TEMP			;  
	LDA	#$44			;  Scan Col Three
	STA 	PORTC			;  Send to Column Lines
        JSR  	KB_Scan_Delay		;  Delay to allow lines to stabilize
	LDA	PORTB			;  Get Rows
	CMP	#$00 			;  Anything Pressed?
	BNE	KB_Scan_Found		;  Yes, Exit.

	LDA    	#$C0
	STA	KB_TEMP			;  
	LDA	#$48			;  Scan Col Four
	STA 	PORTC			;  Send to Column Lines
        JSR	KB_Scan_Delay		;  Delay to allow lines to stabilize
	LDA	PORTB			;  Get Rows
	CMP	#$00 			;  Anything Pressed?
	BNE	KB_Scan_Found		;  Yes, Exit.

	LDA	#$40			;  Turn off All Columns
	STA	PORTC			;  Send to Column Lines
	LDA	#$00			;  RETURN NULL
	RTS				;  Exit

KB_Scan_Found:
	AND	#$3F			;  Clear Top two Bits
	ORA	KB_TEMP			;  Add in Row Bits 
	PHA				;  Store Value
	LDY	#$F0			;
	JSR	MS_DELAY		;
KB_Scan_DEBOUNCE:
	LDA	PORTB			;  Get VALUE
	CMP	#$00 			;  Anything Pressed?
	BNE	KB_Scan_DEBOUNCE	;  Yes, Loop.
	LDA	#$40			;  Turn off All Columns
	STA	PORTC			;  Send to Column Lines
	PLA				;
	RTS				;  RETURN

PAUSE:
KB_Scan_Delay:
	PHA
	PLA
	PHA
	PLA
	RTS

;__MS_DELAY___________________________________________________________________________________________
;
;  DELAY FOR (y) MILLISECONDS (A/2 @ 2MHZ)
;     
;____________________________________________________________________________________________________	
MS_DELAY:
	TXA
DLY1:					;
	LDX	#$00			;
DLY:					;
	DEX				;
	BNE	DLY			;
	DEY				;
	BNE	DLY1			;
	RTS				;
	
	


;__HEXDISPLAY________________________________________________________________________________________
;
;  Display contents of DISPLAYBUF in decoded Hex bits 0-3 are displayed dig, bit 7 is DP
;     
;____________________________________________________________________________________________________
HEXDISPLAY:
	LDX	#$08			; SET DIGIT COUNT
	LDA	#$40			; set Control port 7218 to off
	STA	PORTC			; output
	JSR 	PAUSE			; wait
	LDA	#$F0			; set control to 1111 (Data Coming, Hex Decode,NO Decode, Normal)
	STA	PORTA			; output to port
	LDA	#$80			; Strobe write pulse with Control=1
	STA	PORTC			; output to port
	JSR 	PAUSE			; wait
	LDA	#$40			; set Control port 7218 to off
	STA	PORTC			; output
HEXDISPLAY_LP:		
	LDA	DISPLAYBUF-1,X		; GET DISPLAY DIGIT
	JSR	DECODEDISPLAY		; DECODE DISPLAY
	STA	PORTA			; OUT TO PORTA
	LDA	#$00			; SET WRITE STROBE
	STA	PORTC			; OUT TO PORTC
	JSR	PAUSE			; DELAY
	LDA	#$40			; SET CONTROL PORT OFF
	STA	PORTC			; OUT TO PORTC
	JSR	PAUSE			; WAIT
	DEX				; INC POINTER
	BNE	HEXDISPLAY_LP		; LOOP FOR NEXT DIGIT
	RTS				;

;__DECODEDISPLAY_____________________________________________________________________________________
;
;  Display contents of DISPLAYBUF in decoded Hex bits 0-3 are displayed dig, bit 7 is DP
;     
;____________________________________________________________________________________________________
DECODEDISPLAY:
	PHA
	TXA
	STA	XREG			;
	PLA
	TAX				;
	LDA	SEGDECODE,X		; GET VALUE
	STA	ACC			;
	LDA	XREG			;
	TAX				;
	LDA	ACC			;
	RTS				;


;__SEGDISPLAY________________________________________________________________________________________
;
;  Display contents of DISPLAYBUF in RAW dig, bit 7 is DP
;     
;____________________________________________________________________________________________________
SEGDISPLAY:
	LDY	#$07	
	LDX	#$08			; SET DIGIT COUNT
	LDA	#$40			; set Control port 7218 to off
	STA	PORTC			; output
	JSR 	PAUSE			; wait
	LDA	#$F0			; set control to 1111 (Data Coming, Hex Decode,NO Decode, Normal)
	STA	PORTA			; output to port
	LDA	#$80			; Strobe write pulse with Control=1
	STA	PORTC			; output to port
	JSR 	PAUSE			; wait
	LDA	#$40			; set Control port 7218 to off
	STA	PORTC			; output
SEGDISPLAY_LP:		
	LDA	(TEMPWORD),Y		; GET DISPLAY DIGIT
	STA	PORTA			; OUT TO PORTA
	LDA	#$00			; SET WRITE STROBE
	STA	PORTC			; OUT TO PORTC
	JSR	PAUSE			; DELAY
	LDA	#$40			; SET CONTROL PORT OFF
	STA	PORTC			; OUT TO PORTC
	JSR	PAUSE			; WAIT
	DEY				; INC POINTER
	DEX
	BNE	SEGDISPLAY_LP		; LOOP FOR NEXT DIGIT
	RTS				; RESTORE 
		
;
;__TEXT_STRINGS_________________________________________________________________________________________________________________ 
;
;	SYSTEM TEXT STRINGS
;_____________________________________________________________________________________________________________________________
;
CPUUP:
	.BYTE 	$84,$EE,$BB,$80,$BB,$EE,$CB,$84
ADDRROM:
	.BYTE 	$00,$00,$00,$00,$8C,$BD,$BD,$FE
BOOT:
	.BYTE 	$00,$00,$80,$80,$94,$9D,$9D,$9F

;_KB DECODE TABLE__________________________________________________________________________________________________________
; 
;
KB_DECODE:
;                0  1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
	.BYTE	$41,$02,$42,$82,$04,$44,$84,$08,$48,$88,$10,$50,$90,$20,$60,$A0
;               FW  BK  CL  EN  DP  EX  GO  BO
	.BYTE	$01,$81,$C1,$C2,$C4,$C8,$D0,$E0
;
; F-KEYS,
; FW = FORWARD
; BK = BACKWARD
; CL = CLEAR
; EN = ENTER
; DP = DEPOSIT (INTO MEM)
; EX = EXAMINE (MEM)
; GO = GO
; BO = BOOT
;_________________________________________________________________________________________________________________________
;_HEX 7_SEG_DECODE_TABLE__________________________________________________________________________________________________
; 
; 0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F, ,-
; AND WITH 7FH TO TURN ON DP 
;_________________________________________________________________________________________________________________________
SEGDECODE:
	.BYTE	$FB,$B0,$ED,$F5,$B6,$D7,$DF,$F0,$FF,$F7,$FE,$9F,$CB,$BD,$CF,$CE,$80,$84,$00,$EE,$9D
	

	
; DATA
RESTAB	.BYTE	3,0,2,0,0,3,4,1

	
	.ENDIF
		
		
		.ORG 	$FFDD
LOADVEC:    	JMP 	LOAD			
IDERESETVEC:	JMP 	IDE_SOFT_RESET		
IDESETUPVEC:	JMP 	IDE_SETUP_LBA		
IDEREADVEC:	JMP 	IDE_READ_SECTOR	
FLPSETUPVEC    	JMP 	SETUPDRIVE			
FLPREADVEC:    	JMP 	READFL			

		.ORG 	$FFF0	
PRINTVEC	.DW	WRSER1
INPVEC		.DW	RDSER1
INPWVEC		.DW	CONINW	
	

		 .ORG     $FFFA   		
NMIVECTOR       .DW   IRQVECTOR			;
RSTVECTOR       .DW   COLD_START		; 
INTVECTOR 	.DW   INTERRUPT		; ROM VECTOR FOR IRQ

	.END
