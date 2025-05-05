;__SERIAL DRIVERS________________________________________________________________________________________________________________
;
; 	Nhyodyne serial drivers for single serial port card
;
;	Entry points:
;		SERIALINIT  - called during OS init
;		RDSER1	    - read a byte from serial port ('A' POINTS TO BYTE)
;		WRSER1	    - write a byte from serial port  ('A' POINTS TO BYTE)
;		RDSER1W	    - read a byte from serial port ('A' POINTS TO BYTE, WAIT FOR INPUT)
;		SERIALSTATUS- GET UART STATUS
;________________________________________________________________________________________________________________________________
;


;*
;* HARDWARE I/O ADDRESSES
;*


;__________________________________________________________________________________________________
; $8000-$8007 UART 16C550
;__________________________________________________________________________________________________
UART0           = IO+$58        ;   DATA IN/OUT
UART1           = IO+$59        ;   CHECK RX
UART2           = IO+$5A        ;   INTERRUPTS
UART3           = IO+$5B        ;   LINE CONTROL
UART4           = IO+$5C        ;   MODEM CONTROL
UART5           = IO+$5D        ;   LINE STATUS
UART6           = IO+$5E        ;   MODEM STATUS
UART7           = IO+$5F        ;   SCRATCH REG.


;__SERIALINIT____________________________________________________________________________________________________________________
;
;	INITIALIZE SERIAL PORTS
;________________________________________________________________________________________________________________________________
;
SERIALINIT:
;	LDA	#$80		;
;	STA	f:UART3		; SET DLAB FLAG
;	LDA	#12		; SET TO 12 = 9600 BAUD
;	STA	f:UART0		; save baud rate
;	LDA	#00		;
;	STA	f:UART1		;
;	LDA	#03		;
;	STA	f:UART3		; SET 8 BIT DATA, 1 STOPBIT
        LDA     #$81            ; Enable FIFOs
        STA     F:UART2         ;
        LDA     #$2B            ; Enable Auto Flow Control $03 to disable AFC
        STA     F:UART4
        RTS



;__WRSER1________________________________________________________________________________________________________________________
;
;	WRITE CHARACTER(A) TO UART
;________________________________________________________________________________________________________________________________
;
WRSER1:
        PHA
WRSER1a:
        LDA     UART5           ; Is the destination ready?
        AND     #$20
        CMP     #$00
        BEQ     WRSER1a         ; NO, WAIT FOR IT
        PLA
        STA     UART0           ; THEN WRITE THE CHAR TO UART
        RTS

;__RDSER1________________________________________________________________________________________________________________________
;
;	READ CHARACTER FROM UART TO (A)
;________________________________________________________________________________________________________________________________
;
RDSER1:
        LDA     UART5           ; READ LINE STATUS REGISTER
        AND     #$01            ; TEST IF DATA IN RECEIVE BUFFER
        CMP     #$00
        BEQ     RDSER1N         ; LOOP UNTIL DATA IS READY
        LDA     UART0           ; THEN READ THE CHAR FROM THE UART
        RTS
RDSER1N:
        LDA     #$00            ;
        RTS                     ;

;__RDSER1W_______________________________________________________________________________________________________________________
;
;	READ CHARACTER FROM UART TO (A) - WAIT FOR CHAR
;________________________________________________________________________________________________________________________________
;

RDSER1W:
        LDA     UART5           ; READ LINE STATUS REGISTER
        AND     #$01            ; TEST IF DATA IN RECEIVE BUFFER
        CMP     #$00
        BEQ     RDSER1W         ; LOOP UNTIL DATA IS READY
        LDA     UART0           ; THEN READ THE CHAR FROM THE UART
        AND     #$7F
        RTS

;__SERIALSTATUS__________________________________________________________________________________________________________________
;
;	READ UARD STATUS
;________________________________________________________________________________________________________________________________
;
SERIALSTATUS:
        LDA     UART5           ; READ LINE STATUS REGISTER
        AND     #$01            ; TEST IF DATA IN RECEIVE BUFFER
        CMP     #$00
        BEQ     RDSTAT1         ; NO, INDICATE NO CHAR
        LDA     #$FF            ; GET DATA CHAR
        RTS
RDSTAT1:
        LDA     #$00            ; GET DATA CHAR
        RTS

; end
