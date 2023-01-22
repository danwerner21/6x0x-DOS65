;__Parallel Port Prop Common Code_________________________________________________________________________________________________
;
; 	This is the code that is common to all PPP drivers
;
;	Entry points:
;		INIT_PPP - INIT THE PPP HARDWARE
;		SENDCMD - SEND A COMMAND TO THE PPP HARDWARE
;		PUTBYTE -  SEND A BYTE TO THE PPP HARDWARE
;		GETBYTE -  GET BYTE FROM THE PPP HARDWARE
;________________________________________________________________________________________________________________________________
;



;*
;* HARDWARE I/O ADDRESSES
;*
PPPPIOA         = M6X0X_IOSPACE+$EFC
PPPCNTA         = M6X0X_IOSPACE+$EFD
PPPPIOB         = M6X0X_IOSPACE+$EFE
PPPCNTB         = M6X0X_IOSPACE+$EFF

;*__INIT_PPP___________________________________________________________________________________
;*
;*  INIT THE PPP HARDWARE
;*
;*____________________________________________________________________________________________________
INIT_PPP:

        LDA     #$00            ; SELECT DDR
        STA     PPPCNTA
        LDA     #$00            ; SET ALL PINS INPUT
        STA     PPPPIOA         ;
        LDA     #$00            ; SELECT DDR
        STA     PPPCNTB

        LDA     #%01101111      ; SET CONTROL PINS
; PB0=>CMD/DATA	[O]
; PB1=> X  		[O]
; PB2=> RESET 	[O]
; PB3=> X   	[O]
; PB4=> STB 	[I]
; PB5=> IBF 	[0]
; PB6=> ACK 	[O]
; PB7=> OBF 	[I]
;
        STA     PPPPIOB         ;

        LDA     #$04            ; SELECT PIO
        STA     PPPCNTA
        STA     PPPCNTB

        LDA     #%00000100      ; ASSERT RESET
        STA     PPPPIOB
        LDA     #%00000001      ; DEASSERT RESET,  DATA
        STA     PPPPIOB

INIT_PPP0:

        LDA     PPPPIOB         ; BRING ACK HIGH
        ORA     #%01000000
        STA     PPPPIOB

INIT_PPP1:
; WAIT FOR STB LOW (OUTPUT BUFFER READY)
        LDA     PPPPIOB         ;
        AND     #%00010000      ;
        BNE     INIT_PPP1       ;
        LDA     PPPPIOA         ;IS $AA?
        CMP     #$AA
        BNE     INIT_PPP1       ; NO, STILL INIT IN PROCESS

        LDA     PPPPIOB         ; YES, BRING ACK LOW
        AND     #%10111111
        STA     PPPPIOB
INIT_PPP3:                      ; WAIT FOR STB HIGH
        LDA     PPPPIOB
        AND     #%00010000
        BEQ     INIT_PPP3
        LDA     PPPPIOB         ; BRING ACK HIGH
        ORA     #%01000000
        STA     PPPPIOB
        RTS



;*__SENDCMD___________________________________________________________________________________________
;*
;*  SEND A COMMAND TO THE PPP HARDWARE
;*
;*____________________________________________________________________________________________________
SENDCMD:
        PHA
SENDCMD1:
        LDA     PPPPIOB         ;
        AND     #%11111110      ; SET CMD FLAG
        STA     PPPPIOB         ; SEND IT
        PLA
        JSR     PUTBYTE         ; SEND THE COMMAND BYTE
; TURN OFF CMD
        LDA     PPPPIOB         ;
        ORA     #%00000001      ; CLEAR CMD FLAG
        STA     PPPPIOB         ; SEND IT
        RTS

;*__PUTBYTE___________________________________________________________________________________________
;*
;*  SEND A BYTE TO THE PPP HARDWARE
;*
;*____________________________________________________________________________________________________

PUTBYTE:
        PHA
        LDA     #$00
        STA     PPPCNTA
        LDA     #$FF            ; SET ALL PINS OUTPUT
        STA     PPPPIOA         ;
        LDA     #$04
        STA     PPPCNTA
        LDA     PPPPIOB         ; BRING ACK HIGH
        ORA     #%01000000
        STA     PPPPIOB
PUTWAIT1:
; WAIT FOR OBF HIGH
        LDA     PPPPIOB         ;
        AND     #%10000000      ;
        BEQ     PUTWAIT1        ;
        PLA
        STA     PPPPIOA
        LDA     PPPPIOB         ; BRING ACK LOW
        AND     #%10111111
        STA     PPPPIOB
PUTWAIT2:
; WAIT FOR OBF LOW
        LDA     PPPPIOB         ;
        AND     #%10000000      ;
        BNE     PUTWAIT2        ;
        LDA     PPPPIOB         ; BRING ACK HIGH
        ORA     #%01000000
        STA     PPPPIOB
        RTS

;*__GETBYTE___________________________________________________________________________________________
;*
;*  GET BYTE FROM THE PPP HARDWARE
;*
;*____________________________________________________________________________________________________

GETBYTE:
        LDA     #$00
        STA     PPPCNTA
        LDA     #$00            ; SET ALL PINS INPUT
        STA     PPPPIOA         ;
        LDA     #$04
        STA     PPPCNTA
        LDA     PPPPIOB         ; BRING ACK HIGH
        ORA     #%01000000
        STA     PPPPIOB
GETWAIT1:
; WAIT FOR STB LOW
        LDA     PPPPIOB         ;
        AND     #%00010000      ;
        BNE     GETWAIT1        ;
        LDA     PPPPIOA
        PHA
        LDA     PPPPIOB         ; BRING ACK LOW
        AND     #%10111111
        STA     PPPPIOB
GETWAIT2:
; WAIT FOR STB HIGH
        LDA     PPPPIOB         ;
        AND     #%00010000      ;
        BEQ     GETWAIT2        ;
        LDA     PPPPIOB         ; BRING ACK HIGH
        ORA     #%01000000
        STA     PPPPIOB
        PLA
        RTS
