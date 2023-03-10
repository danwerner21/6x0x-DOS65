;ALLOC
;VERSION 2.03-A
;RELEASED:	10 OCTOBER 1982
;LAST REVISION:
;	28 DECEMBER 1985
;		ADDED PARAM FOR WIDTH
;	30 MARCH 2008
;		REFORMATTED FOR TASM & ASM210
;		ELIMINATED PAGE ZERO 0 & 1
;		ADDED TOTAL BLOCK COUNT TO END MSG
;	3 APRIL 2008
;		CORRECTED ERRORS
;SYNTAX IS ALLOC X WHERE X IS THE DRIVE (A TO H)
;IF X IS BLANK THEN THE DEFAULT DRIVE IS USED.
;USERS MAY WANT TO CHANGE THE VALUE OF WIDTH
;TO A SMALLER VALUE IF THEIR CONSOLE IS LESS THAN 80
;CHARACTERS WIDE.
;EXTERNAL REFERENCES
PEM             = $103          ;PEM ENTRY
DFLFCB          = $107          ;DEFAULT FCB
TEA             = $800          ;TEA START
;FIXED PARAMETERS
CR              = $D            ;RETURN
LF              = $A            ;LINEFEED
;WIDTH IS A SOMEWHAT OBSCURE WAY OF CONTROLLING DIPLAY WIDTH
;IT CAN BE SET TO 1, 3, 7, ETC. - AND WHEN SO SET THE NUMBER OF
;8 BLOCK WIDE BIT MAPS BECOMES 2, 4, 8, ETC.
;WIDTH CAN ONLY BE SET TO (2 TO THE N POWER) - 1
WIDTH           = 3             ;(WIDTH+1)*8-->CHARS PER LINE
;PAGE ZERO VARIABLES

DCBPNT          = $02           ;DCB POINTER
BITCNT          = $04           ;COUNT OF TOTAL BLOCKS
VECPNT          = $06           ;VECTOR POINTER
DFLDRV          = $08           ;DEFAULT DRIVE
TMPDRV          = $09           ;TEMPORARY DRIVE
FREE            = $0A           ;FREE BLOCKS
BITNUM          = $0C           ;BIT (0 TO 7)
BYTNUM          = $0D           ;BYTE (0 TO 255)
LEADZ           = $0E           ;LEADING ZERO FLAG
BLKSCD          = $0F           ;BLOCK SIZE CODE
TOTAL           = $10           ;TOTAL NUMBER OF BLOCK

;MAIN PROGRAM
        .SEGMENT "TEA"
        .ORG    $0800
        LDX     #25             ;GET AND
        JSR     PEM             ;SAVE
        STA     DFLDRV          ;DEFAULT DRIVE
        LDA     DFLFCB+1        ;GET OBJECT
        STA     TMPDRV          ;AND SAVE
        LDX     #13             ;INITIALIZE
        JSR     PEM             ;SYSTEM
        LDA     TMPDRV          ;GET TEMP
        CMP     #' '            ;IF NOT A SPACE
        BNE     HAVDRV          ;USE DEFAULT
        LDA     DFLDRV          ;GET DEFAULT
        BPL     SETDRV          ;AND GO
HAVDRV:
        SEC                     ;DROP TEMP
        SBC     #1              ;BY ONE
        AND     #%111           ;LOOK AT THREE LSBS
SETDRV:
        LDX     #14             ;THEN
        STA     TMPDRV          ;SAVE DRIVE FOR DCB VECTOR
        JSR     PEM             ;SELECT
        LDX     #34             ;FIND DCB ADDRESS
        JSR     PEM
        STA     DCBPNT          ;SAVE POINTER
        STY     DCBPNT+1
        LDY     #0              ;GET MAX BLOCK NUMBER
        LDA     (DCBPNT),Y
        CLC                     ;BUMP TO GET NUMBER
        ADC     #1
        STA     FREE            ;SET FREE COUNT
        STA     TOTAL           ;AND TOTAL
        STA     BITCNT          ;AND BIT COUNTER
        INY                     ;NOW GET HIGH
        LDA     (DCBPNT),Y      ;AND ADD WITH CARRY
        ADC     #0
        STA     FREE+1
        STA     TOTAL+1
        STA     BITCNT+1
        LDY     #6              ;GET SIZE CODE
        LDA     (DCBPNT),Y
        STA     BLKSCD          ;AND SAVE
        LDX     #27             ;GET VECTOR
        JSR     PEM             ;TO MAP
        STA     VECPNT          ;SAVE
        STY     VECPNT+1        ;IN MEMORY
        LDA     #0              ;THEN CLEAR
        STA     BITNUM          ;BIT
        STA     BYTNUM          ;BYTE
        STA     LEADZ           ;AND LEADING ZERO FLAG
        JSR     CRLF            ;LEAVE BLANK LINE
OUTLPE:
        LDY     BYTNUM          ;GET BYTE NUMBER
        LDX     BITNUM          ;AND BIT NUMBER
        LDA     (VECPNT),Y      ;GET MAP BYTE
        AND     BITMSK,X        ;OVERLAY MASK
        BNE     ISFULL          ;BRANCH IF ALLOCATED
        LDA     #'0'            ;ELSE SEND
        BNE     NEXT            ;A ZERO
ISFULL:
        LDA     FREE            ;GET LOW OF FREE COUNT
        BNE     FRENZ           ;JUMP IF NOT ZERO
        DEC     FREE+1          ;ELSE DROP HIGH THEN DO LOW
FRENZ:
        DEC     FREE
        LDA     #'1'            ;SEND A
NEXT:
        JSR     CHROUT          ;ONE
        LDA     BITCNT          ;GET LOW OF COUNTER
        BNE     BITNZ           ;JUMP IF NOT ZERO
        DEC     BITCNT+1        ;ELSE DROP HIGH THEN DO LOW
BITNZ:
        DEC     BITCNT
        LDA     BITCNT          ;TEST FOR ZERO
        ORA     BITCNT+1
        BEQ     DONE            ;DONE IF ZERO
        INC     BITNUM          ;BUMP BIT NUMBER
        LDA     BITNUM          ;GET IT
        CMP     #8              ;SEE IF DONE WITH BYTE
        BNE     OUTLPE          ;THEN LOOP
        LDA     #0
        STA     BITNUM          ;ELSE CLEAR BIT NUMBER
        INC     BYTNUM          ;BUMP BYTE NUMBER
        BNE     *+4             ;SKIP IF NOT ZERO
        INC     VECPNT+1        ;ELSE BUMP HIGH POINTER
        LDA     BYTNUM          ;GET IT
        AND     #WIDTH          ;IF NOT MOD-(WIDTH+1)
        BNE     OUTLPE          ;THEN LOOP
        JSR     CRLF            ;ELSE DO CR AND LF
        JMP     OUTLPE          ;AND LOOP
DONE:
        JSR     CRLF            ;SEND TWO
        JSR     CRLF            ;CR AND LF PAIRS
;NOW SEND FREE COUNT TO SCREEN
        JSR     SNDFRE
        LDA     #' '            ;SEND A SPACE
        JSR     CHROUT
;NOW DISPLAY BLOCK SIZE
        LDA     BLKSCD          ;GET CODE
        ASL     A               ;MULT BY TWO
        TAX                     ;MAKE INDEX
        LDA     BLKTBL,X        ;GET ADDRESS
        LDY     BLKTBL+1,X
        LDX     #9              ;SEND MESSAGE
        JSR     PEM
        LDA     #(CLSMSG & $ff) ;SEND
        LDY     #((CLSMSG / $100)& $FF);SIZE
        LDX     #9              ;MESSAGE
        JSR     PEM
;NOW SHOW TOTAL BLOCK COUNT
        LDA     #0              ;FIRST CLEAR LEADING ZERO FLAG
        STA     LEADZ
        LDA     TOTAL
        LDY     TOTAL+1
        STA     FREE
        STY     FREE+1
        JSR     SNDFRE
        LDA     #(TTLMSG & $ff)
        LDY     #((TTLMSG / $100)& $FF)
        LDX     #9
        JSR     PEM
;NOW EXIT
        LDA     DFLDRV          ;SELECT
        LDX     #14             ;DEFAULT
        JMP     PEM             ;AND RETURN
;SEND 16 BIT VALUE IN FREE TO SCREEN
SNDFRE:
        LDX     #0              ;CLEAR 10000'S
        SEC
F10000:
        LDA     FREE            ;DROP BY 10000
        SBC     #(10000 & $ff)
        STA     FREE
        LDA     FREE+1
        SBC     #((10000 / $100)& $FF)
        STA     FREE+1
        INX                     ;BUMP DIGIT
        BCS     F10000          ;LOOP IF NO BORROW
        LDA     FREE            ;ELSE ADD 10000 BACK IN
        ADC     #(10000 & $ff)
        STA     FREE
        LDA     FREE+1
        ADC     #((10000 / $100)& $FF)
        STA     FREE+1
        DEX                     ;DROP DIGIT
        BEQ     N10000          ;SKIP IF ZERO
        TXA                     ;ELSE SET FLAG
        STA     LEADZ
        JSR     SNDDIG          ;SEND
N10000:
        LDX     #0              ;CLEAR 1000'S
        SEC
F1000:
        LDA     FREE            ;DROP BY 1000
        SBC     #(1000 & $ff)
        STA     FREE
        LDA     FREE+1
        SBC     #((1000 / $100)& $FF)
        STA     FREE+1
        INX                     ;BUMP DIGIT
        BCS     F1000           ;LOOP IF NO BORROW
        LDA     FREE            ;ELSE ADD BACK IN
        ADC     #(1000 & $ff)
        STA     FREE
        LDA     FREE+1
        ADC     #((1000 / $100)& $FF)
        STA     FREE+1
        JSR     DIGOUT          ;SEND DIGIT
        LDX     #0              ;CLEAR 100'S COUNT
        SEC                     ;NOW
F100:
        LDA     FREE            ;NOW DO 100'S
        SBC     #100
        STA     FREE
        LDA     FREE+1
        SBC     #0
        STA     FREE+1
        INX                     ;BUMP COUNT
        BCS     F100            ;UNTIL A BORROW
        LDA     FREE
        ADC     #100            ;THEN ADD
        STA     FREE            ;100 BACK AND SAVE
        JSR     DIGOUT          ;SEND DIGIT
        LDX     #0              ;CLEAR 10'S
        SEC                     ;NOW
        LDA     FREE            ;DROP
F10:
        SBC     #10             ;FREE BY 10
        INX                     ;AND BUMP COUNT
        BCS     F10             ;UNTIL A BORROW
        ADC     #10             ;ADD 10 BACK IN
        STA     FREE            ;AND SAVE
        JSR     DIGOUT          ;SEND DIGIT
NO10:
        LDA     FREE            ;ALWAYS
        JMP     SNDDIG          ;DO 1'S
;OUTPUT DIGIT
DIGOUT:
        DEX                     ;DROP COUNT
        BEQ     CHKLDZ          ;IF ZERO CHECK FLAG
        STX     LEADZ           ;ELSE SET
        BNE     MKEDIG          ;THEN MAKE AND SEND
CHKLDZ:
        LDA     LEADZ           ;GET FLAG
        BEQ     EXTDIG          ;DONE IF ZERO
MKEDIG:
        TXA                     ;MOVE TO A
SNDDIG:
        ORA     #'0'
        JSR     CHROUT          ;SEND
EXTDIG:
        RTS                     ;QUIT
;OUTPUT A CR AND LF
CRLF:
        LDA     #CR             ;SEND
        JSR     CHROUT          ;A CR
        LDA     #LF             ;THEN A LF
;CHARACTER OUTPUT
conwrt:
CHROUT:
        LDX     #2              ;SEND CHAR
        JMP     PEM             ;TO CONSOLE
;BIT MASK TABLE
BITMSK:
        .BYTE   128,64,32,16,8,4,2,1
;BLOCK SIZE MESSAGES
BLKMS0:
        .BYTE   "1$"
BLKMS1:
        .BYTE   "2$"
BLKMS2:
        .BYTE   "4$"
BLKMS3:
        .BYTE   "8$"
BLKMS4:
        .BYTE   "16$"
;BLOCK SIZE MESSAGE POINTERS
BLKTBL:
        .WORD   BLKMS0,BLKMS1,BLKMS2,BLKMS3,BLKMS4
;CLOSING MESSAGE
CLSMSG:
        .BYTE   "K BLOCKS FREE OF $"
TTLMSG:
        .BYTE   " TOTAL$"

        .END
