	TITLE	IDE DRIVERS

;__IDE DRIVERS___________________________________________________________________________________________________________________
;
; 	CUBIX IDE disk drivers for direct attached disk-io card
;
;	Entry points:
;		IDE_SOFT_RESET   - called during OS init
;		IDE_READ_SECTOR  - read a sector from drive  ('U' POINTS TO DCB, X TO MEMORY)
;		IDE_WRITE_SECTOR - write a sector to drive   ('U' POINTS TO DCB, X TO MEMORY)
;________________________________________________________________________________________________________________________________
;
	
	
	
*
* HARDWARE I/O ADDRESSES
*
IDELO		EQU       $F120			; DATA PORT (LOW BYTE)
IDEERR		EQU       $F121			; READ: ERROR REGISTER; WRITE: PRECOMP
IDESECTC	EQU       $F122			; SECTOR COUNT
IDESECTN	EQU       $F123			; SECTOR NUMBER
IDECYLLO	EQU       $F124			; CYLINDER LOW
IDECYLHI	EQU       $F125			; CYLINDER HIGH
IDEHEAD		EQU       $F126			; DRIVE/HEAD
IDESTTS		EQU       $F127			; READ: STATUS; WRITE: COMMAND
IDEHI		EQU       $F128			; DATA PORT (HIGH BYTE)
IDECTRL		EQU       $F12E			; READ: ALTERNATIVE STATUS; WRITE; DEVICE CONTROL
IDEADDR		EQU       $F12F			; DRIVE ADDRESS (READ ONLY)

		
*__IDE_READ_SECTOR___________________________________________________________________________________
*
*  READ IDE SECTOR (IN LBA) INTO BUFFER
*     
*____________________________________________________________________________________________________				
IDE_READ_SECTOR:		
		PSHS	X			; STORE BUFFER LOCATION
		JSR	IDE_WAIT_BUSY_READY	; MAKE SURE DRIVE IS READY TO PROCEED
		BNE	IDE_READ_SECTOR_ERR	; ERROR, ABORT
		JSR	IDE_SETUP_LBA		; TELL DRIVE WHAT SECTOR IS REQUIRED
		LDAA    #$20			;  		
		STAA    IDESTTS			; $20 = IDE 'READ SECTOR' COMMAND 		
		JSR	IDE_WAIT_BUSY_READY	; MAKE SURE DRIVE IS READY TO PROCEED
		BNE	IDE_READ_SECTOR_ERR	; ERROR, ABORT
		JSR	IDE_TEST_ERROR		; ENSURE NO ERROR WAS REPORTED
		BNE	IDE_READ_SECTOR_ERR	; ERROR, ABORT
		JSR	IDE_WAIT_BUFFER		; WAIT FOR FULL BUFFER SIGNAL FROM DRIVE
		BNE	IDE_READ_SECTOR_ERR	; ERROR, ABORT
		PULS	X			; GET BUFFER LOCATION
		JSR	IDE_READ_BUFFER		; GRAB THE 256 WORDS FROM THE BUFFER
		CLRA				; ZERO = 1 ON RETURN = OPERATION OK
		RTS
IDE_READ_SECTOR_ERR:
		LDAA    #$02
		RTS
		
		
*__IDE_WRITE_SECTOR__________________________________________________________________________________
*
*  WRITE IDE SECTOR (IN LBA) FROM BUFFER
*     
*____________________________________________________________________________________________________				
IDE_WRITE_SECTOR:
		PSHS 	X			; STORE BUFFER LOCATION
		JSR	IDE_WAIT_BUSY_READY	; MAKE SURE DRIVE IS READY TO PROCEED
		BNE	IDE_WRITE_SECTOR_ERR	; ERROR, ABORT
		JSR	IDE_SETUP_LBA		; TELL DRIVE WHAT SECTOR IS REQUIRED
		LDAA    #$30			;  
		STAA    IDESTTS			; $30 = IDE 'WRITE SECTOR' COMMAND 
		JSR	IDE_WAIT_BUSY_READY	;
		BNE	IDE_WRITE_SECTOR_ERR	; ERROR, ABORT
		JSR	IDE_TEST_ERROR		; ENSURE NO ERROR WAS REPORTED
		BNE	IDE_WRITE_SECTOR_ERR	; ERROR, ABORT
		JSR	IDE_WAIT_BUFFER		; WAIT FOR BUFFER READY SIGNAL FROM DRIVE
		BNE	IDE_WRITE_SECTOR_ERR	; ERROR, ABORT
		PULS	X			; GET BUFFER LOCATION
		JSR	IDE_WRITE_BUFFER       	; SEND 256 WORDS TO DRIVE'S BUFFER
		JSR	IDE_WAIT_BUSY_READY	; MAKE SURE DRIVE IS READY TO PROCEED
		BNE	IDE_WRITE_SECTOR_ERR	; ERROR, ABORT
		JSR	IDE_TEST_ERROR		; ENSURE NO ERROR WAS REPORTED
		BNE	IDE_WRITE_SECTOR_ERR	; ERROR, ABORT
		CLRA				; ZERO = 1 ON RETURN = OPERATION OK		
		RTS
IDE_WRITE_SECTOR_ERR:
		LDA	#$02
		RTS
*__IDE_SOFT_RESET____________________________________________________________________________________
*
*  SOFT RESET IDE CHANNEL
*     
*____________________________________________________________________________________________________				
IDE_SOFT_RESET

		LDAA    #%00000110		; NO INTERRUPTS, RESET DRIVE = 1
		STAA    IDECTRL			;
		LDAA    #%00000010		; NO INTERRUPTS, RESET DRIVE = 0
		STAA    IDECTRL			;
		JSR	IDE_WAIT_BUSY_READY	;
		RTS				;


*__IDE_WAIT_BUSY_READY_______________________________________________________________________________
*
*  WAIT FOR IDE CHANNEL TO BECOME READY
*     
*____________________________________________________________________________________________________				
IDE_WAIT_BUSY_READY:
		
		LDX	#$0000			;
IDE_WBSY:
		INX				;
		CPX	#$1E00			; TIMED OUT?
		BEQ	IDE_TO			; YUP, EXIT
		LDAA    IDESTTS			; READ ERROR REG
		ANDA    #%11000000		; MASK OFF BUSY AND RDY BITS
		CMPA   	#%01000000		; WE WANT BUSY(7) TO BE 0 AND RDY(6) TO BE 1
		BNE	IDE_WBSY		;
		CLRA				; ZERO 1 = OK
		RTS
IDE_TO:
		LDAA    #$FF			; ZERO 0 = TIMED OUT
		CLRA
		RTS
		
*__IDE_TEST_ERROR____________________________________________________________________________________
*
*  TEST FOR IDE ERROR
*     
*____________________________________________________________________________________________________				
IDE_TEST_ERROR:
		
		LDAA    IDESTTS			;
		ANDA    #%00000001		;
		BNE	IDE_ERR			;		
		CLRA				;
		RTS
IDE_ERR:
		LDAA    IDEERR			; READ ERROR FLAGS
		LDAA    #$FF			; ZERO 0 = ERROR
		RTS				;
						;
*__IDE_WAIT_BUFFER____________________________________________________________________________________
*
*  WAIT FOR IDE BUFFER TO FILL
*     
*____________________________________________________________________________________________________				
IDE_WAIT_BUFFER:
		LDX	#$0000			;
IDE_WDRQ:
		INX				;
		CPX	#$FFFF			;
		BEQ	IDE_TO2			;
		LDAA    IDESTTS			; WAIT FOR DRIVE'S 512 BYTE READ BUFFER 
		ANDA    #%00001000		;
		BNE	IDE_WDRQ		;
		CLRA				; ZERO 1 = OK
		RTS
IDE_TO2:
		LDAA    #$FF			; CARRY 0 = TIMED OUT
		CLRA
		RTS
*__IDE_READ_BUFFER___________________________________________________________________________________
*
*  READ IDE BUFFER LITTLE ENDIAN
*     
*____________________________________________________________________________________________________				
IDE_READ_BUFFER:
		LDY    	#$0100			; 256 WORDS (512 BYTES PER SECTOR)
IDEBUFRD:
		LDAA    IDELO			; LOW BYTE OF WORD FIRST
		LDAB    IDEHI			; THEN HIGH BYTE OF WORD
		STAA    ,X+			; 'ID DRIVE' IDE RESPONSE IS LITTLE ENDIAN FORMAT		
		STAB    ,X+			; 'ID DRIVE' IDE RESPONSE IS LITTLE ENDIAN FORMAT
		DEY     			;
		CMPY    #$0000			;
		BNE	IDEBUFRD		;
		RTS				;
*__IDE_WRITE_BUFFER___________________________________________________________________________________
*
*  WRITE IDE BUFFER LITTLE ENDIAN
*     
*____________________________________________________________________________________________________				
IDE_WRITE_BUFFER:
		LDAB    #$00			; 256 WORDS (512 BYTES PER SECTOR)
IDEBUFWT:
		LDAA    1,X			; SECTORS ARE BIG ENDIAN
		STAA    IDEHI			; SET UP HIGH LATCHED BYTE BEFORE
		LDAA    ,X++			; SECTORS ARE BIG ENDIAN
		STAA    IDELO			; WRITING WORD WITH WRITE TO LOW BYTE
		DECB     			;
		CMPB    #$00			;
		BNE	IDEBUFWT		;
		RTS				;
		
*__IDE_SETUP_LBA_____________________________________________________________________________________
*
*  SETUP LBA DATA
*     
*____________________________________________________________________________________________________				
IDE_SETUP_LBA:	   
		LDAA    #$01			;  
		STAA    IDESECTC		; SET SECTOR COUNT = 1
		LDAA    SEC,U			;
		STAA    IDESECTN		; SET LBA 0:7
		LDAA    CYL,U			;
		STAA    IDECYLLO		; SET LBA 8:15
		LDAA    HEAD,U			;
		STAA    IDECYLHI		; SET LBA 16:23
		LDAA    #$01			;
		ANDA    #%00001111		; LOWEST 4 BITS USED ONLY
		ORA     #%11100000		; TO ENABLE LBA MODE
		STAA    IDEHEAD			; SET LBA 24:27 + BITS 5:7=111
		RTS
