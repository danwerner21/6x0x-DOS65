;__PARALLEL PORT PROP MASS STORAGE DRIVERS_______________________________________________________________________________________
;
; 	DOS/65 PARALLEL PORT PROP MASS STORAGE DRIVERS
;
;	Entry points:
;		PPP_SOFT_RESET   - called during OS init
;		PPP_READ_SECTOR  - read a sector from drive
;		PPP_WRITE_SECTOR - write a sector to drive
;               PPP_INITIALIZE   - INITIALIZE HARDWARE
;________________________________________________________________________________________________________________________________
;


;*__PPP_READ_SECTOR___________________________________________________________________________________
;*
;*  READ IDE SECTOR (IN LBA) INTO BUFFER
;*
;*____________________________________________________________________________________________________
PPP_READ_SECTOR:


        LDA     debsehd         ;
        CMP     Cdebsehd        ;
        BNE     PPP_READ_SECTOR_DIRTY
        LDA     debcyll         ;
        CMP     Cdebcyll        ;
        BNE     PPP_READ_SECTOR_DIRTY
        LDA     debcylm         ;
        CMP     Cdebcylm        ;
        BNE     PPP_READ_SECTOR_DIRTY

        LDA     #$00
        RTS

PPP_READ_SECTOR_DIRTY:
        LDA     debcyll         ; STORE CURRENT PARMS
        STA     Cdebcyll        ;
        LDA     debcylm         ;
        STA     Cdebcylm        ;
        LDA     debsehd         ;
        STA     Cdebsehd        ;


        LDA     #$14
        JSR     SENDCMD

        LDA     debsehd
        JSR     PUTBYTE
        LDA     debcyll
        JSR     PUTBYTE
        LDA     debcylm
        JSR     PUTBYTE
        LDA     #$00
        JSR     PUTBYTE


        JSR     GETBYTE         ; GET RESULT
        CMP     #$00
        BNE     PPP_READ_SECTOR_ERR

        LDA     #$13
        JSR     SENDCMD
        JSR     GETBUF
        LDA     #$00
        RTS
PPP_READ_SECTOR_ERR:
        LDA     #$FF            ; STORE CURRENT PARMS
        STA     Cdebsehd        ;
        STA     Cdebcyll        ;
        STA     Cdebcylm        ;

        LDA     #$02
        RTS
GETBUF:
        LDX     #$00
GETBUF1:
        JSR     GETBYTE
        STA     hstbuf,X
        INX
        CPX     #$00
        BNE     GETBUF1
GETBUF2:
        JSR     GETBYTE
        STA     hstbuf+256,X
        INX
        CPX     #$00
        BNE     GETBUF2
        RTS


;*__PPP_WRITE_SECTOR__________________________________________________________________________________
;*
;*  WRITE IDE SECTOR (IN LBA) FROM BUFFER
;*
;*____________________________________________________________________________________________________
PPP_WRITE_SECTOR:
; WRITE A SECTOR
        LDA     #$12
        JSR     SENDCMD
        JSR     PUTBUF

        LDA     #$15
        JSR     SENDCMD

        LDA     debsehd
        JSR     PUTBYTE
        LDA     debcyll
        JSR     PUTBYTE
        LDA     debcylm
        JSR     PUTBYTE
        LDA     #$00
        JSR     PUTBYTE

        JSR     GETBYTE         ; GET RESULT
        CMP     #$00
        BNE     PPP_WRITE_SECTOR_ERROR

        LDA     #$FF            ; STORE CURRENT PARMS
        STA     Cdebcyll        ;
        STA     Cdebcylm        ;
        STA     Cdebsehd        ;
        LDA     #$00
        RTS
PPP_WRITE_SECTOR_ERROR:
        LDA     #$FF            ; STORE CURRENT PARMS
        STA     Cdebsehd        ;
        STA     Cdebcyll        ;
        STA     Cdebcylm        ;
        LDA     #$02
        RTS
PUTBUF:
        LDX     #$00
PUTBUF1:
        LDA     hstbuf,X
        JSR     PUTBYTE
        INX
        CPX     #$00
        BNE     PUTBUF1
PUTBUF2:
        LDA     hstbuf+256,X
        JSR     PUTBYTE
        INX
        CPX     #$00
        BNE     PUTBUF2
        RTS


;*__PPP_SOFT_RESET____________________________________________________________________________________
;*
;*  SOFT RESET PPP CHANNEL
;*
;*____________________________________________________________________________________________________
PPP_SOFT_RESET:
        LDA     #$10
        JSR     SENDCMD
        RTS                     ;

;*__PPP_INITIALIZE____________________________________________________________________________________
;*
;*  INITIALIZE THE PPP SD DRIVERS
;*
;*____________________________________________________________________________________________________
PPP_INITIALIZE:
        PRTS    "SD CARD: INITIALIZED$"; NOT PRESENT
        JSR     NEWLINE
        JMP     PPP_SOFT_RESET
