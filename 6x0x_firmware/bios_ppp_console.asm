;__PARALLEL PORT PROP CONSOLE DRIVERS____________________________________________________________________________________________
;
; 	DOS CONSOLE DRIVERS FOR THE PARALLEL PORT PROP
;
;	Entry points:
;		PPPCONIN   	- read a byte from CONSOLE ('A' POINTS TO BYTE)
;		PPPCONINW  	- read a byte from CONSOLE ('A' POINTS TO BYTE, WAIT FOR BYTE)
;		PPPOUTCH   	- write a byte from CONSOLE  ('A' POINTS TO BYTE)
;		PPPCONSTATUS	-RETURN CONSOLE STATUS
;________________________________________________________________________________________________________________________________
;





;__PPPOUTCH______________________________________________________________________________________________________________________
;
;	WRITE CHARACTER(A) TO PPP
;________________________________________________________________________________________________________________________________
;
PPPOUTCH:
        PHA
        LDA     #$20
        JSR     SENDCMD
        PLA

        JSR     PUTBYTE
        RTS

;__PPPCONIN______________________________________________________________________________________________________________________
;
;	READ CHARACTER FROM PPP
;________________________________________________________________________________________________________________________________
;
PPPCONIN:
        LDA     #$31
        JSR     SENDCMD
        JSR     GETBYTE
        RTS                     ;

;__CONINW________________________________________________________________________________________________________________________
;
;	READ CHARACTER FROM PPP (WAIT FOR CHAR)
;________________________________________________________________________________________________________________________________
;
PPPCONINW:
        LDA     #$31
        JSR     SENDCMD
        JSR     GETBYTE
        CMP     #$00
        BEQ     PPPCONINW
        AND     #$7F
        RTS

;__IOF_CONSTATUS_________________________________________________________________________________________________________________
;
;	READ STATUS FROM PPP
;________________________________________________________________________________________________________________________________
;
PPPCONSTATUS:
        LDA     #$30
        JSR     SENDCMD
        JSR     GETBYTE
        RTS
