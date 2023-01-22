	TITLE	SERIAL DRIVERS
;__FLOPPY DRIVERS________________________________________________________________________________________________________________
;
; 	CUBIX serial drivers for 6809 IO card
;
;	Entry points:
;		SERIALINIT  - called during OS init
;		RDSER1	    - read a byte from serial port ('A' POINTS TO BYTE)
;		WRSER1	    - write a byte from serial port  ('A' POINTS TO BYTE)
;________________________________________________________________________________________________________________________________
;
	
	
	
*
* HARDWARE I/O ADDRESSES
*
UART1		EQU	$F004		; SERIAL PORT 1 (I/O Card)


;__SERIALINIT____________________________________________________________________________________________________________________
;
;	INITIALIZE SERIAL PORTS
;________________________________________________________________________________________________________________________________
;
SERIALINIT:
	LDD	#$0B1E			; 9600, 8 BITS, NO PARITY, 1 STOP BIT
	LDX	#UART1			; POINT TO FIRST UART
	CLR	1,X			; RESET UART
	STD	2,X			; INITIALIZE CONTROL REGISTERS
	RTS



;__WRSER1________________________________________________________________________________________________________________________
;
;	WRITE CHARACTER(A) TO UART
;________________________________________________________________________________________________________________________________
;
WRSER1	PSHS	B
WRSER1a	LDB	UART1+1			; GET STATUS
	BITB	#%00010000		; IS TX READY
	BEQ	WRSER1a			; NO, WAIT FOR IT
	STAA	UART1			; WRITE DATA
	PULS	B,PC

;__RDSER1________________________________________________________________________________________________________________________
;
;	READ CHARACTER FROM UART TO (A)
;________________________________________________________________________________________________________________________________
;
RDSER1	LDAA	UART1+1			; GET STATUS REGISTER
	BITA	#%00001000		; IS RX READY
	BEQ	RDSER1N			; NO, INDICATE NO CHAR
	LDAA	UART1			; GET DATA CHAR
	ORCC	#%00000100		; SET 'Z'
	RTS
RDSER1N
	LDAA	#$FF			;
	RTS				;
