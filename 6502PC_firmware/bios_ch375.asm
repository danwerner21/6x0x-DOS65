;__USB DRIVERS___________________________________________________________________________________________________________________
;
; 	DOS/65 USB disk drivers 6502PC - CH375 USB STORAGE
;
;	Entry points:
;		CH375INIT      	 - CALLED DURING OS INIT
;		CH_READSEC       - read a sector from drive
;		CH_WRITESEC      - write a sector to drive
;________________________________________________________________________________________________________________________________
;
; CH375 HARDWARE ADDRESS
CH0BASE         = $E260
CH0DATA         = CH0BASE
CH0COMMAND      = CH0BASE+1
;
; CH375/376 COMMANDS
;
CH_CMD_VER      = $01           ; GET IC VER
CH_CMD_RESET    = $05           ; FULL CH37X RESET
CH_CMD_EXIST    = $06           ; CHECK EXISTS
CH_CMD_MAXLUN   = $0A           ; GET MAX LUN NUMBER
CH_CMD_PKTSEC   = $0B           ; SET PACKETS PER SECTOR
CH_CMD_SETRETRY = $0B           ; SET RETRIES
CH_CMD_MODE     = $15           ; SET USB MODE
CH_CMD_TSTCON   = $16           ; TEST CONNECT
CH_CMD_ABRTNAK  = $17           ; ABORT DEVICE NAK RETRIES
CH_CMD_STAT     = $22           ; GET STATUS
CH_CMD_RD5      = $28           ; READ USB DATA (375)
CH_CMD_WR5      = $2B           ; WRITE USB DATA (375)
CH_CMD_DSKMNT   = $31           ; DISK MOUNT
CH_CMD_BYTE_LOC = $39           ; BYTE LOCATE
CH_CMD_BYTERD   = $3A           ; BYTE READ
CH_CMD_BYTERDGO = $3B           ; BYTE READ GO
CH_CMD_BYTEWR   = $3C           ; BYTE WRITE
CH_CMD_BYTEWRGO = $3D           ; BYTE WRITE GO
CH_CMD_DSKCAP   = $3E           ; DISK CAPACITY
CH_CMD_AUTOSET  = $4D           ; USB AUTO SETUP
CH_CMD_DSKINIT  = $51           ; DISK INIT
CH_CMD_DSKRES   = $52           ; DISK RESET
CH_CMD_DSKSIZ   = $53           ; DISK SIZE
CH_CMD_DSKRD    = $54           ; DISK READ
CH_CMD_DSKRDGO  = $55           ; CONTINUE DISK READ
CH_CMD_DSKWR    = $56           ; DISK WRITE
CH_CMD_DSKWRGO  = $57           ; CONTINUE DISK WRITE
CH_CMD_DSKINQ   = $58           ; DISK INQUIRY
CH_CMD_DSKRDY   = $59           ; DISK READY


CH375INIT:
        JSR     LFCR
        JSR     CH_DETECT
        BNE     NOTDETECTED

        LDA     #<CHMESSAGE2    ;
        STA     STRPTR          ;
        LDA     #>CHMESSAGE2    ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT
        JSR     LFCR            ; AND CRLF

        JSR     CH_DISKINIT
        BCS     :+

        LDA     #<CHMESSAGE3    ;
        STA     STRPTR          ;
        LDA     #>CHMESSAGE3    ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT

        LDA     hstbuf
        JSR     PRINT_BYTE
        LDA     hstbuf+1
        JSR     PRINT_BYTE
        LDA     hstbuf+2
        JSR     PRINT_BYTE
        LDA     hstbuf+3
        JSR     PRINT_BYTE


        JSR     LFCR            ; AND CRLF
        LDA     #$00
        RTS
:
        LDA     #<CHMESSAGE5    ;
        STA     STRPTR          ;
        LDA     #>CHMESSAGE5    ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT
        JSR     LFCR            ; AND CRLF
        LDA     #$FF
        RTS
NOTDETECTED:
        LDA     #<CHMESSAGE6    ;
        STA     STRPTR          ;
        LDA     #>CHMESSAGE6    ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT
        JSR     LFCR            ; AND CRLF
        LDA     #$FF
        RTS

CH_DETECT:
        LDA     #<CHMESSAGE1    ;
        STA     STRPTR          ;
        LDA     #>CHMESSAGE1    ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT
        JSR     LFCR            ; AND CRLF

        LDA     #<MESSAGE2      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE2      ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT
        LDA     #>CH0BASE       ; GET BASE PORT
        JSR     PRINT_BYTE
        LDA     #<CH0BASE       ; GET BASE PORT
        JSR     PRINT_BYTE

        JSR     CH_RESET

CH_DETECT1:
        LDA     #CH_CMD_EXIST   ; LOAD COMMAND
        JSR     CH_CMD          ; SEND COMMAND
        LDA     #$AA            ; LOAD CHECK PATTERN
        JSR     CH_WR           ; SEND IT
        JSR     CH_NAP          ; SMALL DELAY
        JSR     CH_RD           ; GET ECHO
        CMP     #$55            ; SHOULD BE INVERTED
        RTS                     ; RETURN

CH_CMD:
        STA     CH0COMMAND      ; SEND COMMAND
        JSR     CH_NAP          ;
        RTS
;
; GET STATUS
;
CH_STAT:
        LDA     CH0COMMAND      ; READ STATUS
        RTS
;
; READ A BYTE FROM DATA PORT
;
CH_RD:
        LDA     CH0DATA         ; READ BYTE
        RTS
;
; WRITE A BYTE TO DATA PORT
;
CH_WR:
        STA     CH0DATA         ; WRITE BYTE
        RTS

CH_NAP:
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


CH_RESET:
        LDA     #CH_CMD_RESET
        JSR     CH_CMD          ; SEND COMMAND
        LDY     #$FF
CH_RES1:
        LDX     #$FF
:
        DEX
        BNE     :-
        DEY
        BNE     CH_RES1
        RTS



;
; POLL WAITING FOR INTERRUPT
;
CH_POLL:
        TXA
        PHA
        TYA
        PHA
        LDY     #$FF
CH_POLL0:
        LDX     #$FF            ; PRIMARY LOOP COUNTER
CH_POLL1:
        JSR     CH_STAT         ; GET INT STATUS
        AND     #%10000000
        BEQ     CH_POLL2        ; CHECK BIT
        DEX
        BNE     CH_POLL1        ; INNER LOOP AS NEEDED
        DEY
        BNE     CH_POLL0        ; OUTER LOOP AS NEEDED
        TAY
        PLA
        TAX
        PLA
        RTS                     ; AND RETURN
CH_POLL2:
        LDA     #CH_CMD_STAT    ; GET STATUS
        JSR     CH_CMD          ; SEND IT
        JSR     CH_NAP          ; SMALL DELAY
        JSR     CH_RD           ; GET RESULT
        STA     CHRESULT
        PLA
        TAY
        PLA
        TAX
        LDA     CHRESULT
        RTS                     ; AND RETURN

CHRESULT:
        .BYTE   00

CH_DISKINIT:
        PHA
        TXA
        PHA
        TYA
        PHA
; RESET THE BUS
        LDA     #CH_CMD_MODE    ; SET MODE COMMAND
        JSR     CH_CMD          ; SEND IT
        LDA     #7              ; RESET BUS
        JSR     CH_WR           ; SEND IT
        JSR     CH_NAP          ; SMALL WAIT
        JSR     CH_RD           ; GET RESULT
        JSR     CH_NAP          ; SMALL WAIT
;
; ACTIVATE USB MODE
        LDA     #CH_CMD_MODE    ; SET MODE COMMAND
        JSR     CH_CMD          ; SEND IT
        LDA     #6              ; USB ENABLED, SEND SOF
        JSR     CH_WR           ; SEND IT
        JSR     CH_NAP          ; SMALL WAIT
        JSR     CH_RD           ; GET RESULT
        JSR     CH_NAP          ; SMALL WAIT

;
        LDY     #$FF
CH_DISKINIT1:
        LDA     #CH_CMD_DSKINIT
        JSR     CH_CMD          ; SEND COMMAND
        LDA     #$FF
        LDX     #$FF
:
        DEX
        BNE     :-
        TAX
        DEX
        TXA
        LDX     #$FF
        CMP     #$00
        BNE     :-

        JSR     CH_POLL

        CMP     #$14            ; SUCCESS?
        BEQ     CHUSB_RESET1A   ; IF SO, CHECK READY
        CMP     #$16            ; NO MEDIA
        BEQ     CHUSB_NOMEDIA   ; HANDLE IT
        JSR     CH_NAP          ; SMALL DELAY
        DEY
        BNE     CH_DISKINIT1    ; LOOP AS NEEDED
        JMP     CH_DISKINIT_TO  ; HANDLE TIMEOUT

CHUSB_RESET1A:
        JSR     CH_DSKSIZ       ; GET AND RECORD DISK SIZE
        BCC     CHUSB_RESET1B
        PHA
        LDA     #<CHMESSAGE8    ;
        STA     STRPTR          ;
        LDA     #>CHMESSAGE8    ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT
        PLA
        JSR     PRINT_BYTE
        JSR     LFCR
        JMP     CH_DISKINIT_TO

CHUSB_RESET1B:
        PLA
        TAY
        PLA
        TAX
        PLA
        CLC
        RTS

CH_DISKINIT_TO:
        PLA
        TAY
        PLA
        TAX
        PLA
        SEC
        RTS


CHUSB_NOMEDIA:
        LDA     #<CHMESSAGE7    ;
        STA     STRPTR          ;
        LDA     #>CHMESSAGE7    ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT
        JSR     LFCR            ; AND CRLF
        PLA
        TAY
        PLA
        TAX
        PLA
        SEC
        RTS

CH_DSKSIZ:
        LDA     #CH_CMD_DSKSIZ  ; DISK SIZE COMMAND
        JSR     CH_CMD          ; SEND IT
        JSR     CH_POLL         ; WAIT FOR RESULT

        CMP     #$14            ; SUCCESS?
        BNE     CHUSB_CMDERR    ; HANDLE CMD ERROR
        JSR     CH_CMD_RD       ; SEND READ USB DATA CMD
        JSR     CH_RD           ; GET RD DATA LEN

        CMP     #$08            ; MAKE SURE IT IS 8
        BNE     CHUSB_CMDERR    ; HANDLE CMD ERROR

        JSR     CH_RD
        STA     hstbuf
        JSR     CH_RD
        STA     hstbuf+1
        JSR     CH_RD
        STA     hstbuf+2
        JSR     CH_RD
        STA     hstbuf+3
        JSR     CH_RD
        JSR     CH_RD
        JSR     CH_RD
        JSR     CH_RD
        CLC
        RTS                     ; AND DONE
CHUSB_CMDERR:
        SEC
        RTS                     ; AND DONE

CHUSB_IOERR:
        LDA     #$FF            ; SET ERROR CONDITION
        SEC
        RTS                     ; AND DONE


; SEND READ USB DATA COMMAND
; USING BEST OPCODE FOR DEVICE
;
CH_CMD_RD:
        LDA     #CH_CMD_RD5
        JMP     CH_CMD
;
; SEND WRITE USB DATA COMMAND
; USING BEST OPCODE FOR DEVICE
;
CH_CMD_WR:
        LDA     #CH_CMD_WR5
        JMP     CH_CMD

DSKBUFTMP:
        .WORD   hstbuf
        .WORD   hstbuf+64
        .WORD   hstbuf+128
        .WORD   hstbuf+192
        .WORD   hstbuf+256
        .WORD   hstbuf+320
        .WORD   hstbuf+384
        .WORD   hstbuf+448
DSKBUFCNT:
        .BYTE   00


CH_READSEC:
        LDA     #CH_CMD_DSKRD   ; DISK READ COMMAND
        JSR     CHUSB_RWSTART   ; SEND CMD AND LBA
;
; READ THE SECTOR IN 64 BYTE CHUNKS
        LDA     #00
        STA     DSKBUFCNT
CHUSB_READ1:
        LDX     DSKBUFCNT
        LDA     DSKBUFTMP,X
        STA     TEMPWORD2
        INX
        LDA     DSKBUFTMP,X
        STA     TEMPWORD2+1
        INX
        STX     DSKBUFCNT
        JSR     CH_POLL         ; WAIT FOR DATA READY
        CMP     #$1D            ; DATA READY TO READ?
        BNE     CHUSB_IOERR     ; HANDLE IO ERROR
        JSR     CH_CMD_RD       ; SEND READ USB DATA CMD
        JSR     CH_RD           ; READ DATA BLOCK LENGTH
        CMP     #64             ; AS EXPECTED?
        BNE     CHUSB_IOERR     ; IF NOT, HANDLE ERROR
        LDY     #0              ; 8 CHUNKS OF 64 FOR 512 BYTE SECTOR
; BYTE READ LOOP

CHUSB_READ2:
        JSR     CH_RD           ; GET NEXT BYTE
        STA     (TEMPWORD2),Y   ; SAVE IT
        INY
        CPY     #64
        BNE     CHUSB_READ2     ; LOOP AS NEEDED
;
; PREPARE FOR NEXT CHUNK
        LDA     #CH_CMD_DSKRDGO ; CONTINUE DISK READ
        JSR     CH_CMD          ; SEND IT
        LDX     DSKBUFCNT
        CPX     #16
        BNE     CHUSB_READ1     ; LOOP TILL DONE
;
; FINAL CHECK FOR COMPLETION & SUCCESS
        JSR     CH_POLL         ; WAIT FOR COMPLETION
        CMP     #$14            ; SUCCESS?
        BNE     CHUSB_IOERR     ; IF NOT, HANDLE ERROR
;
        LDA     #$00
        RTS
;
;
;
CH_WRITESEC:
        LDA     #CH_CMD_DSKWR   ; DISK WRITE COMMAND
        JSR     CHUSB_RWSTART   ; SEND CMD AND LBA
;
; WRITE THE SECTOR IN 64 BYTE CHUNKS
        LDA     #00
        STA     DSKBUFCNT
CHUSB_WRITE1:
        LDX     DSKBUFCNT
        LDA     DSKBUFTMP,X
        STA     TEMPWORD2
        INX
        LDA     DSKBUFTMP,X
        STA     TEMPWORD2+1
        INX
        STX     DSKBUFCNT
        JSR     CH_POLL         ; WAIT FOR DATA READY
        CMP     #$1E            ; DATA READY TO WRITE
        BNE     CHUSB_IOERR1    ; HANDLE IO ERROR
        JSR     CH_CMD_WR       ; SEND WRITE USB DATA CMD
        LDA     #64             ; 64 BYTE CHUNK
        JSR     CH_WR           ; SEND DATA BLOCK LENGTH
        LDY     #0              ; 8 CHUNKS OF 64 FOR 512 BYTE SECTOR
;
; BYTE WRITE LOOP
CHUSB_WRITE2:
        LDA     (TEMPWORD2),Y   ; GET NEXT BYTE
        JSR     CH_WR           ; WRITE NEXT BYTE
        INY
        CPY     #64
        BNE     CHUSB_WRITE2    ; LOOP AS NEEDED
;
; PREPARE FOR NEXT CHUNK
        LDA     #CH_CMD_DSKWRGO ; CONTINUE DISK READ
        JSR     CH_CMD          ; SEND IT
        LDX     DSKBUFCNT
        CPX     #16
        BNE     CHUSB_WRITE1    ; LOOP TILL DONE
;
; FINAL CHECK FOR COMPLETION & SUCCESS
        JSR     CH_POLL         ; WAIT FOR COMPLETION
        CMP     #$14            ; SUCCESS?
        BNE     CHUSB_IOERR1    ; IF NOT, HANDLE ERROR
;
        LDA     #$00
        CLC                     ; SIGNAL SUCCESS
        RTS
CHUSB_IOERR1:
        JMP     CHUSB_IOERR
;
; INITIATE A DISK SECTOR READ/WRITE OPERATION
; A: READ OR WRITE OPCODE
;
CHUSB_RWSTART:
        PHA
        JSR     IDE_CONVERT_SECTOR_LBA
        PLA
        JSR     CH_CMD          ; SEND R/W COMMAND
;
; SEND LBA, 4 BYTES, LITTLE ENDIAN
        LDA     debsehd
        JSR     CH_WR           ; SEND BYTE
        LDA     debcyll
        JSR     CH_WR           ; SEND BYTE
        LDA     debcylm         ;
        JSR     CH_WR           ; SEND BYTE
        LDA     #0              ;
        JSR     CH_WR           ; SEND BYTE
; REQUEST 1 SECTOR
        LDA     #1              ;
        JSR     CH_WR           ; SEND BYTE
        RTS
;
CHMESSAGE1:
        .BYTE   "CH375 USB:",00
CHMESSAGE2:
        .BYTE   "  CH375 DETECTED.",00
CHMESSAGE3:
        .BYTE   "  CH375: BLOCKS=0x",00
CHMESSAGE5:
        .BYTE   "  CH375 MEDIA ERROR.",00
CHMESSAGE6:
        .BYTE   "  CH375 NOT DETECTED.",00
CHMESSAGE7:
        .BYTE   "  CH375 NO MEDIA.",00
CHMESSAGE8:
        .BYTE   "  CH375 INIT ERROR=0x",00
