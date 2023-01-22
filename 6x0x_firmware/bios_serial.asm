;__SERIAL DRIVERS________________________________________________________________________________________________________________
;
; 	serial drivers
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

UART1DATA       = M6X0X_IOSPACE+$FF4; SERIAL PORT 1 (I/O Card)
UART1STATUS     = M6X0X_IOSPACE+$FF5; SERIAL PORT 1 (I/O Card)
UART1COMMAND    = M6X0X_IOSPACE+$FF6; SERIAL PORT 1 (I/O Card)
UART1CONTROL    = M6X0X_IOSPACE+$FF7; SERIAL PORT 1 (I/O Card)


;Command Register
;No bit is affected by a software reset, however, all bits are set to zero on a hardware reset.
;Bit 7 6 5  configuration
;    x x 0  no parity bit
;    0 0 1  send and receive with odd parity
;    0 1 1  send and receive with even parity
;    1 0 1  send: parity=1; receive: parity not evaluated
;    1 1 1  send: parity=0; receive: parity not evaluated
;
;Bit 4  0: no echo
;       1: echo (received characters are being sent again,
;                bits 2 and 3 must be 0 for that)
;
;Bit 3 2  sender interr.   RTS level   sender
;    0 0  no               high        off
;    0 1  yes              low         on
;    1 0  no               low         on
;    1 1  no               low         send BRK
;
;Bit 1  0: interrupt gets triggered by bit 3 in status register
;       1: no interrupt
;
;Bit 0  0: disable transceiver and interrupts, /DTR high
;       1: enable transceiver and interrupts, /DTR low
;
;Control Register
;Bits 0 to 3 are set to zero on a software reset, and all bits are set to zero on a hardware reset.
;Bit 7  0: 1 stop bit
;       1: a) with 8 data bits and 1 parity bit: 1 stop bit
;          b) with 5 data bits and no parity bit: 1.5 stop bits
;          c) otherwise 2 stop bits
;
;Bit 6 5  data bits
;    0 0  8
;    0 1  7
;    1 0  6
;    1 1  5
;
;Bit 4  0: external receive clock
;       1: builtin clock as receive clock
;
;Bit 3 2 1 0  baud rate
;    0 0 0 0  1/16 times external clock
;    0 0 0 1  50 bps
;    0 0 1 0  75 bps
;    0 0 1 1  109.92 bps
;    0 1 0 0  134.58 bps
;    0 1 0 1  150 bps
;    0 1 1 0  300 bps
;    0 1 1 1  600 bps
;    1 0 0 0  1200 bps
;    1 0 0 1  1800 bps
;    1 0 1 0  2400 bps
;    1 0 1 1  3600 bps
;    1 1 0 0  4800 bps
;    1 1 0 1  7200 bps
;    1 1 1 0  9600 bps
;    1 1 1 1  19200 bps



;__SERIALINIT____________________________________________________________________________________________________________________
;
;	INITIALIZE SERIAL PORTS
;________________________________________________________________________________________________________________________________
;
SERIALINIT:
        LDA     #$00            ; RESET UART
        STA     UART1STATUS     ;
        LDA     #$0B            ;
        STA     UART1COMMAND    ;
        LDA     #$1E            ; 9600, 8 BITS, NO PARITY, 1 STOP BIT
        STA     UART1CONTROL    ;
        RTS



;__WRSER1________________________________________________________________________________________________________________________
;
;	WRITE CHARACTER(A) TO UART
;________________________________________________________________________________________________________________________________
;
WRSER1:
        PHA
WRSER1a:
        LDA     UART1STATUS     ; GET STATUS
        AND     #%00010000      ; IS TX READY
        BEQ     WRSER1a         ; NO, WAIT FOR IT
        PLA
        STA     UART1DATA       ; WRITE DATA
        RTS

;__RDSER1________________________________________________________________________________________________________________________
;
;	READ CHARACTER FROM UART TO (A)
;________________________________________________________________________________________________________________________________
;
RDSER1:
        LDA     UART1STATUS     ; GET STATUS REGISTER
        AND     #%00001000      ; IS RX READY
        BEQ     RDSER1N         ; NO, INDICATE NO CHAR
        LDA     UART1DATA       ; GET DATA CHAR
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
        JSR     RDSER1
        CMP     #$00
        BEQ     RDSER1W
        AND     #$7F
        RTS

;__SERIALSTATUS__________________________________________________________________________________________________________________
;
;	READ UARD STATUS
;________________________________________________________________________________________________________________________________
;
SERIALSTATUS:
        LDA     UART1STATUS     ; GET STATUS REGISTER
        AND     #%00001000      ; IS RX READY
        BNE     RDSTAT1         ; NO, INDICATE NO CHAR
        LDA     #$00            ; GET DATA CHAR
        RTS
RDSTAT1:
        LDA     #$FF            ; GET DATA CHAR
        RTS
