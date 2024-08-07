;EDIT
;DOS/65 CONTEXT EDITOR
;VERSION 2.03-A
;RELEASED:	25 SEPTEMBER 1982
;LAST REVISION:
;	27 NOVEMBER 1983
;		ADDED CODE SO RETURNS TO DEFAULT DRIVE
;	20 APRIL 1986
;		ADDED VERSION REPORTING AT START
;		ADDED ? COMMAND FOR HELP
;		ELIMINATED LOC 0 AND 1
;	31 MARCH 2008
;		REFORMATTED FOR TASM & ASM210
;ASSEMBLY TIME OPTIONS
STRMAX          = 100           ;MAXIMUM STRING LENGTH
SRCLNG          = 1024          ;SOURCE BUFFER LENGTH
DSTLNG          = 1024          ;DESTINATION BUFFER LENGTH
;DOS/65 REFERENCES
BOOT            = $100          ;WARM BOOT
PEM             = $103          ;PEM JUMP
DFLFCB          = $107          ;DEFAULT FCB
DFLBUF          = $128          ;DEFAULT BUFFER
TEA             = $800          ;ORIGIN
CONDEF          = 55            ;CONDEF BLOCK OFFSET IN SIM
;FIXED PARAMETERS
PLUS            = $FF           ;PLUS SIGN
MINUS           = 0             ;MINUS SIGN
CR              = $D            ;CARRIAGE RETURN
LF              = $A            ;LINEFEED
EOF             = $1A           ;END OF FILE
NULL            = 0
BLANK           = $20           ;ASCII SPACE
TAB             = 9             ;CTL-I
CTLR            = $12           ;REPEAT LINE
CTLX            = $18           ;CANCEL LINE
CTLL            = $C            ;CR-LF SUBSTITUTE
DELETE          = $7F           ;BACKSPACE
;PAGE ZERO VARIABLES
PRMSGN          = $02           ;PARAMETER SIGN (0=-,FF=+)
NUMBER          = $03           ;PARAMETER VALUE
COLUMN          = $05           ;CONSOLE COLUMN
CURDRV          = $06           ;CURRENT DRIVE (DEFAULT)
DSTDRV          = $07           ;DESTINATION DRIVE
NXTCHR          = $08           ;NEXT CHARACTER FROM INPUT
CNSIND          = $09           ;INDEX INTO CNSTXT
MACFLG          = $0A           ;MACRO FLAG AND MAXIMUM INDEX
MACIND          = $0B           ;MACRO BUFFER INDEX
INSFLG          = $0C           ;INSERT MODE IF >127
NOMORE          = $0D           ;NO MORE CONSOLE INPUT IF >127
LIBIND          = $0E           ;.LIB FILE INDEX
XLBIND          = $0F           ;X$$$$$$$.LIB FILE INDEX
RDSCCN          = $10           ;READ SECTOR COUNT
STRIND          = $11           ;INDEX INTO STRBUF
NXTTXT          = $12           ;POINTER TO NEXT CHAR POSITION
SRCIND          = $14           ;SOURCE FILE POINTER
DSTIND          = $16           ;DESTINATION FILE POINTER
UPRTXT          = $18           ;UPPER TEXT POINTER VALUE
LWRLMT          = $1A           ;LOWER MOVE LIMIT
UPRLMT          = $1C           ;MAXIMUM TEXT POINTER
MAXTXT          = $1E           ;TXTLMT-1
TXTLMT          = $20           ;LIMIT OF TEXT BUFFER
POINT           = $22           ;GENERAL USE BUFFER
MACCNT          = $24           ;MACRO COUNT
TYPPNT          = $26           ;POINTER FOR TYPE
CMPPNT          = $28           ;POINTER FOR COMPRE
PRSSTR          = $2A           ;PARSE START
ENDSTR          = $2B           ;END OF SEARCH STRING
XLBPNT          = $2C           ;X$$$$$$$.LIB POINTER
NORMAL          = $2E           ;NORMAL VIDEO
INVERT          = $2F           ;INVERT VIDEO
FORWAR          = $30           ;FORWARD SPACE
CLREOL          = $31           ;CLEAR TO EOL
BACKSP          = $32           ;BACKSPACE
ENDCOL          = $33           ;LAST COLUMN
OUTFLG          = $34           ;OUTPUT OK IF < 128
DFLDRV          = $35           ;DEFAULT DRIVE
LASTZP          = $36           ;
;-------------------------------
;MAIN PROGRAM
;-------------------------------
        .FEATURE labels_without_colons
        .SEGMENT "TEA"
        .ORG    $0800

        JMP     EDIT            ;GO EXECUTE
        .BYTE   "COPYRIGHT (C) 2008 - "
        .BYTE   "RICHARD A. LEARY"
;CLEAR PAGE ZERO
EDIT
        LDX     #2              ;SET STARTING LOC
        LDA     #0              ;CLEAR ACCUM
CLRZRP
        STA     $00,X           ;CLEAR BYTE
        INX                     ;COUNT UP
        CPX     #LASTZP         ;SEE IF END
        BNE     CLRZRP          ;LOOP IF MORE
;SEND OPENING MESSAGE
        LDA     #<OPNMSG        ;POINT TO IT
        LDY     #>OPNMSG
        JSR     PRTBUF          ;SEND IT
;GET CONSOLE DEFINITION PARAMETERS FROM SIM
        LDA     BOOT+2          ;GET HIGH
        STA     GETSYS+2        ;AND SAVE
        LDX     #CONDEF+3       ;GET NORMAL
        JSR     GETSYS          ;CHAR
        STA     NORMAL          ;AND SAVE
        LDX     #CONDEF+4       ;SAME
        JSR     GETSYS          ;FOR
        STA     INVERT          ;INVERT
        LDX     #CONDEF+2       ;AND
        JSR     GETSYS          ;FOR
        STA     FORWAR          ;FORWARD
        LDX     #CONDEF+1       ;THEN
        JSR     GETSYS          ;FOR
        STA     CLREOL          ;CLEAR TO EOL
        LDX     #CONDEF+0       ;FINALLY
        JSR     GETSYS          ;GET
        STA     BACKSP          ;BACKSPACE
;FIND DEFAULT DRIVE
        JSR     RDECUR          ;GET FROM PEM
        STA     DFLDRV          ;AND SAVE FOR LATER
;CALCULATE POINTERS FOR TEXT BUFFER
        SEC                     ;SET
        LDA     PEM+1           ;UPPER
        LDY     PEM+2           ;TEXT
        SBC     #1              ;LIMITS
        STA     TXTLMT          ;TO
        STA     MAXTXT          ;PEM
        BCS     *+3             ;LOCATION
        DEY                     ;MINUS
        STY     TXTLMT+1        ;ONE
        STY     MAXTXT+1        ;THEN
        LDY     #0              ;CLEAR INDEX
        TYA                     ;AND ACCUM
        STA     (TXTLMT),Y      ;AND INSERT A ZERO
        LDA     MAXTXT          ;SUBTRACT
        BNE     *+4             ;ONE FROM LIMIT
        DEC     MAXTXT+1        ;TO MAKE
        DEC     MAXTXT          ;MAXIMUM
;TEST FOR GOOD NAME FORMAT
        LDA     DFLFCB+1        ;GET FIRST CHAR
        CMP     #BLANK          ;IF NOT A BLANK
        BNE     *+5             ;IS OK
        JMP     DOSERR          ;ELSE ERROR
        LDA     DFLFCB+17       ;THEN IF SECOND
        CMP     #BLANK          ;IS BLANK
        BEQ     *+5             ;IS OK
        JMP     DOSERR          ;ELSE ERROR
;SET UP CURRENT AND DESTINATION DRIVES
        LDA     DFLFCB          ;GET AUTOMATIC DRIVE
        PHA                     ;SAVE IT
        LDA     #0              ;CLEAR
        STA     DFLFCB          ;AUTOMATIC
        PLA                     ;GET DRIVE
        STA     CURDRV          ;AND SAVE
        BNE     NOATSR          ;IF NOT ZERO USE
        LDA     DFLDRV          ;ELSE GET CURRENT
        STA     CURDRV          ;SAVE IT
        INC     CURDRV          ;BUMP IT
NOATSR
        DEC     CURDRV          ;DROP IT TO 0 TO 7
        LDA     DFLFCB+16       ;GET AUTOMATIC
        STA     DSTDRV          ;AND SAVE
        BNE     NOATDS          ;IF NON ZERO USE
        LDX     CURDRV          ;GET CURRENT
        INX                     ;BUMP IT
        STX     DSTDRV          ;AND USE IT
NOATDS
        DEC     DSTDRV          ;DROP IN EITHER CASE
;INITIALIZE FILES
        JSR     INTXLB          ;INITIALIZE X$$$$$$$.LIB
;RENTRY FOR RE-EDIT OF FILE
RENTRY
        LDA     #<(SOURCE+SRCLNG);SET
        LDY     #>(SOURCE+SRCLNG);SOURCE
        STA     SRCIND          ;INDEX TO
        STY     SRCIND+1        ;ILLEGAL
        LDA     #<DEST          ;SET
        LDY     #>DEST          ;DESTINATION
        STA     DSTIND          ;POINTER
        STY     DSTIND+1        ;TO FIRST
        LDA     #0              ;CLEAR
        STA     DFLFCB+12       ;EXTENT
        STA     DFLFCB+13
        STA     DFLFCB+32       ;AND NEXT RECORD
        LDX     #32             ;MOVE
STDSFC
        LDA     DFLFCB,X        ;DEFAULT FCB
        STA     DSTFCB,X        ;TO DESTINATION
        DEX                     ;FCB
        BPL     STDSFC          ;IN HIGH MEM
        LDA     DSTDRV          ;GET DESTINATION DRIVE
        CMP     CURDRV          ;COMPARE TO CURRENT
        BEQ     DSTSME          ;BRANCH IF SAME
        JSR     SETDRV          ;ELSE SET DEST
        LDA     #<DFLFCB        ;POINT
        LDY     #>DFLFCB        ;TO DEFAULT
        LDX     #17             ;SEARCH
        JSR     PEM             ;FOR IT
        BMI     DSTSME          ;OK IF NOT
        LDA     #<FLXMSG        ;ELSE SEND
        LDY     #>FLXMSG        ;FILE EXISTS
        JSR     PRCRBF          ;MESSAGE
        JMP     DLXLEX          ;AND EXIT
DSTSME
        LDA     CURDRV          ;GET CURRENT
        JSR     SETDRV          ;AND SET
        LDA     #<DFLFCB        ;THEN
        LDY     #>DFLFCB        ;OPEN
        JSR     OPNFLE          ;SOURCE
        BNE     SRISOK          ;OK IF THERE
        LDA     #<DFLFCB        ;ELSE
        LDY     #>DFLFCB        ;CREATE
        JSR     CRTFLE          ;IT
        BNE     *+5             ;JUMP IF OK
        JMP     DOSERR          ;ELSE ERROR
        LDA     #<NWFMSG        ;POINT TO
        LDY     #>NWFMSG        ;NEW FILE MESSAGE
        JSR     PRCRBF          ;AND SEND IT
        JSR     CRLF            ;AND ANOTHER CR AND LF
SRISOK
        LDA     #<BAKSTR        ;CHANGE
        LDY     #>BAKSTR        ;DESTINATION
        JSR     CHGTYP          ;TO .BAK
        LDA     #<DSTFCB        ;DELETE
        LDY     #>DSTFCB        ;IT IF
        JSR     DLTFLE          ;THERE
        LDA     DSTDRV          ;IF DEST
        CMP     CURDRV          ;SAME AS CURRENT
        BEQ     DNTDDD          ;JUMP
        JSR     SETDRV          ;ELSE
        LDA     #<DSTFCB        ;DELETE
        LDY     #>DSTFCB        ;IT ON
        JSR     DLTFLE          ;DESTINATION
DNTDDD
        LDA     #<DLRSTR        ;CHANGE
        LDY     #>DLRSTR        ;TYPE
        JSR     CHGTYP          ;TO .$$$
        LDA     #<DSTFCB        ;DELETE
        LDY     #>DSTFCB        ;IT IF
        JSR     DLTFLE          ;THERE
        LDA     #<DSTFCB        ;THEN
        LDY     #>DSTFCB        ;CREATE
        JSR     CRTFLE          ;IT
        BNE     *+5             ;JUMP IF OK
        JMP     DLXLEX          ;ELSE EXIT
        LDA     #LF             ;INSERT A LF
        STA     TXTBUF          ;AT START OF BUFFER
        LDA     #<(TXTBUF+1)    ;INITIALIZE
        LDY     #>(TXTBUF+1)    ;NEXT
        STA     NXTTXT          ;POINTER TO
        STY     NXTTXT+1        ;START + 1
        LDA     MAXTXT          ;SET UPPER
        LDY     MAXTXT+1        ;TO MAX
        STA     UPRTXT          ;POINT
        STY     UPRTXT+1        ;IN BUFFER
        LDA     #0              ;CLEAR COLUMN
        STA     COLUMN          ;INDEX
        JMP     BEGIN           ;AND BEGIN
;ERROR/BREAK LOOP ENTRIES
;CAN NOT DO COMMAND SPECIFIED TIMES
CNTDMR
        LDA     #0
        BEQ     BRKLPE
;UNRECOGNIZED COMMAND
UNRCCM
        LDA     #1
        BNE     BRKLPE
;MEMORY BUFFER FULL
MMBFFL
        LDA     #2
BRKLPE
        PHA                     ;SAVE ERROR
        LDA     #<BRKMSG        ;SEND
        LDY     #>BRKMSG        ;BREAK
        JSR     PRCRBF          ;MESSAGE
        PLA                     ;THEN
        ASL     A               ;MAKE INDEX
        TAX
        LDA     ERRTBL,X        ;GET MESSAGE ADDRESS
        LDY     ERRTBL+1,X
        JSR     PRTBUF          ;PRINT MESSAGE
        LDA     #<ATMSG         ;SEND
        LDY     #>ATMSG         ;AT
        JSR     PRTBUF          ;MESSAGE
        LDA     NXTCHR          ;THEN LAST
        JSR     CHROUT          ;CHAR
        JSR     CRLF            ;AND A CR&LF
BEGIN
        LDX     #$FF            ;RESET STACK
;    TXS                     ;IN CASE ALTERED
        STX     NOMORE          ;BUFFER EMPTY
        INX                     ;CLEAR
        STX     MACFLG          ;MACRO FLAG
;MAIN PARSING LOOP ENTRY
PRSMRE
        LDA     #0              ;CLEAR INSERT
        STA     INSFLG          ;MODE FLAG
        JSR     GTNXCH          ;GET AN INPUT
        LDA     CNSIND          ;GET INDEX
        STA     PRSSTR          ;AND SAVE
;? FOR HELP
        LDA     #'?'            ;MATCH ?
        JSR     TSTFON
        BNE     NOTHLP          ;NO SO TRY NEXT
        LDA     #<HELP0         ;DO FIRST PART
        LDY     #>HELP0
        JSR     PRTBUF
        LDA     #<HELP1         ;THEN SECOND PART
        LDY     #>HELP1
        JSR     PRTBUF
        LDA     #<HELP2         ;THEN THIRD PART
        LDY     #>HELP2
        JSR     PRTBUF
        LDA     #<HELP3         ;AND FINAL PART
        LDY     #>HELP3
        JSR     PRTBUF
        JMP     PRSMRE          ;AND DO MAIN LOOP
;E FOR END
NOTHLP
        LDA     #'E'            ;SEE IF E
        JSR     TSTFON          ;AND ONLY E
        BNE     NOTEND          ;TRY NEXT IF NOT
        JSR     FLUSFL          ;FLUSH FILE
        JMP     DLXLEX          ;DELETE X$$$$$$$.LIB AND BOOT
;H FOR HEAD
NOTEND
        LDA     #'H'            ;SEE IF H
        JSR     TSTFON          ;AND ONLY H
        BNE     NOTHEA          ;TRY NEXT IF NOT
        JSR     FLUSFL          ;FLUSH FILE
        LDA     DSTDRV          ;SWITCH
        LDX     CURDRV          ;CURRENT AND
        STX     DSTDRV          ;DESTINATION
        STA     CURDRV          ;DRIVES
        JMP     RENTRY          ;AND RESTART
;I FOR INSERT
NOTHEA
        LDA     NXTCHR          ;GET NEXT CHAR
        CMP     #'I'            ;IF I
        BEQ     *+5             ;THEN INSERT
        JMP     NOTINS          ;ELSE TRY NEXT
        LDX     #0              ;SET X TO ZERO
        LDA     CNSLNG          ;COMPARE LENGTH
        CMP     CNSIND          ;TO INDEX
        BNE     STINMD          ;IF NOT SAME CLEAR INSERT MODE
        LDA     MACFLG          ;GET MACRO FLAG
        BNE     STINMD          ;CLEAR INSERT IF NOT ZERO
        DEX                     ;ELSE SET X TO FF
STINMD
        STX     INSFLG          ;SAVE INSERT FLAG
INSMRE
        JSR     GTTSEN          ;GET INPUT AND TEST FOR EOF
        BNE     *+5             ;CONTINUE IF NOT
        JMP     ENDINS          ;ELSE END INSERT
        LDA     NXTCHR          ;GET CHAR
        BEQ     INSMRE          ;LOOP IF A NULL
        CMP     #CTLX           ;IF NOT CTL-X
        BNE     NTCTLX          ;TRY NEXT
        JSR     CLEAR           ;CLEAR LINE
        JSR     STNM0           ;SET NUMBER TO ZERO
        LDA     #MINUS          ;AND SIGN
        STA     PRMSGN          ;TO MINUS
        JSR     LMTCLC          ;SEE HOW MUCH TO MOVE
        JSR     MOVDLT          ;DO IT
        JMP     INSMRE          ;AND LOOP
NTCTLX
        CMP     #CTLR           ;IF NOT CTL-R
        BNE     NTRTDL          ;TRY NEXT
DORPT
        JSR     CLEAR           ;CLEAR LINE
        JSR     STNM0           ;SET NUMBER TO ZERO
        LDA     #MINUS          ;AND SIGN
        STA     PRMSGN          ;TO NEGATIVE
        JSR     TYPE            ;ELSE TYPE LINE
        JMP     INSMRE          ;AND GET MORE
NTRTDL
        CMP     #DELETE         ;IF NOT DELETE
        BNE     NOTDLT          ;TRY NEXT
        JSR     DOBS            ;WIPE OUT THE DELETE
DLT
        LDA     #<(TXTBUF+1)    ;IF
        CMP     NXTTXT          ;NOT
        BNE     DLTOK           ;AT
        LDA     #>(TXTBUF+1)    ;BEGINNING
        CMP     NXTTXT+1        ;IS
        BNE     DLTOK           ;OK
        JSR     CLEAR           ;CLEAR LINE
        JMP     INSMRE          ;ELSE TRY AGAIN
DLTOK
        JSR     GETCOL          ;SET CURRENT COLUMN
        LDA     COLUMN          ;AND SAVE
        STA     ENDCOL
        JSR     DRNXTX          ;DROP NEXT TEXT
        LDY     #0              ;CLEAR INDEX
        LDA     (NXTTXT),Y      ;GET CHAR
        CMP     #LF             ;IF A LF
        BEQ     DLT             ;THEN KILL CR TOO
        CMP     #CR             ;IF A CR
        BEQ     DORPT           ;TYPE NEW LINE
        JSR     GETCOL          ;CALC NEW COLUMN
        SEC
        LDA     ENDCOL          ;CALCULATE NUMBER OF BS'S
        SBC     COLUMN
        STA     ENDCOL          ;AND SAVE
BSLPE
        JSR     DOBS            ;DO ONE
        DEC     ENDCOL
        BNE     BSLPE           ;LOOP IF MORE
        JMP     INSMRE          ;THEN GET NEXT CHAR
NOTDLT
        CMP     #CTLL           ;IF NOT A CLT-L
        BNE     NOTCTL          ;TRY NEXT
        JSR     INCRLF          ;ELSE INSERT CR AND LF
        JMP     INSMRE          ;AND LOOP
NOTCTL
        JSR     INSTCH          ;INSERT CHAR
        LDA     NXTCHR          ;GET IT AGAIN
        CMP     #LF             ;IF NOT A LF
        BNE     NOTALF          ;TRY NEXT
        JMP     INSMRE          ;ELSE LOOP
NOTALF
        CMP     #CR             ;IF A CR
        BEQ     ISACR           ;CONTINUE
        CMP     #TAB            ;SEE IF TAB
        BNE     *+5             ;CONTINUE IF NOT
        JMP     INSMRE          ;ELSE LOOP
        CMP     #' '            ;IF LESS THAN BLANK
        BCC     *+5             ;CONTINUE
        JMP     INSMRE          ;ELSE LOOP
        JSR     CHROUT          ;ECHO CONTROL
        JMP     INSMRE          ;AND LOOP
ISACR
        LDA     #LF             ;GET A LF
        STA     NXTCHR          ;STORE IT
        LDX     MACFLG          ;GET MACRO FLAG
        BNE     *+5             ;JUMP IF MACRO
        JSR     CHROUT          ;ELSE ECHO
        JSR     INSTCH          ;INSERT IT
        JMP     INSMRE          ;AND LOOP
ENDINS
        LDA     NXTCHR          ;GET CHAR
        CMP     #EOF            ;IF AN EOF
        BEQ     *+5             ;THEN SKIP
        JSR     INCRLF          ;CR AND LF INSERTION
        JMP     PRSMRE          ;PARSE MORE
;O FOR ORIGINAL FILE
NOTINS
        LDA     #'O'            ;GET AN O
        JSR     TSTVRF          ;SEE IF ONE AND ONLY
        BNE     *+5             ;TRY NEXT IF NOT
        JMP     RENTRY          ;ELSE RESTART
;R FOR READ LIBRARY FILE
        LDA     NXTCHR          ;GET CHAR
        CMP     #'R'            ;IF AN R
        BEQ     *+5             ;IS A READ
        JMP     NOTRLB          ;ELSE TRY NEXT
        LDA     #1              ;SET INDEX
        STA     LIBIND          ;TO ONE
        JSR     CURDFL          ;DEFAULT FILE
GTRLNM
        JSR     GTTSEN          ;GET CHAR
        BEQ     RNMEND          ;JUMP IF END
        LDX     LIBIND          ;GET INDEX
        CPX     #9              ;COMPARE TO MAX+1
        BCC     *+5             ;OK IF LESS
        JMP     CNTDMR          ;ELSE ERROR
        JSR     CHTOLF          ;INSERT INTO FCB
        JMP     GTRLNM          ;LOOP FOR MORE
RNMEND
        LDA     #BLANK          ;SET FOR
        STA     NXTCHR          ;BLANK FILL
        LDX     LIBIND          ;GET INDEX
        DEX                     ;DROP
        BNE     NOTXLB          ;.LIB IF NOT ZERO
        LDX     #8              ;ELSE
XLFLBF
        LDA     XLBFCB,X        ;IS
        STA     LIBFCB,X        ;X$$$$$$$.LIB
        DEX                     ;SO FILL
        BNE     XLFLBF          ;FCB
        BEQ     LBFLOK          ;AND GO USE
NOTXLB
        LDX     LIBIND          ;GET INDEX
        CPX     #9              ;COMPARE TO 9
        BCS     LBFLOK          ;DONE IF THAT
        JSR     CHTOLF          ;ELSE INSERT
        JMP     NOTXLB          ;AND LOOP
LBFLOK
        LDA     #0              ;CLEAR
        STA     LIBFCB+12       ;EXTENT
        STA     LIBFCB+13
        STA     LIBFCB+32       ;AND RECORD
        LDA     #<LIBFCB        ;THEN
        LDY     #>LIBFCB        ;OPEN
        JSR     OPNFLE          ;FILE
        BNE     *+7             ;CONTINUE IF OK
        LDA     #3              ;ELSE SEND
        JMP     BRKLPE          ;BREAK
        LDA     #128            ;SET INDEX
        STA     LIBIND          ;TO ILLEGAL
RDLBCH
        JSR     GETLIB          ;GET CHAR
        STA     NXTCHR          ;STORE
        CMP     #EOF            ;IF EOF
        BEQ     *+8             ;THEN DONE
        JSR     INSTCH          ;ELSE INSERT
        JMP     RDLBCH          ;AND LOOP
        JMP     PRSMRE          ;MAIN LOOP
;Q FOR QUIT
NOTRLB
        LDA     #'Q'            ;SEE IF Q
        JSR     TSTVRF          ;AND VERIFY
        BNE     NOTQUT          ;BRANCH IF NOT
        LDA     #<DSTFCB        ;ELSE
        LDY     #>DSTFCB        ;DELETE
        JSR     DLTFLE          ;DESTINATION
        JMP     DLXLEX          ;AND EXIT
;NUMBER
NOTQUT
        JSR     DFLPRM          ;SET DEFAULT PARMS
        LDA     NXTCHR          ;GET CHAR
        CMP     #'-'            ;IF NOT A -
        BNE     SGNPOS          ;THEN BRANCH
        LDA     #MINUS          ;ELSE GET
        STA     PRMSGN          ;MINUS AND SET
        JSR     GTNXCH          ;AND GET COMMAND
        LDA     NXTCHR          ;GET CHAR
SGNPOS
        CMP     #'#'            ;IF NOT #
        BNE     NTMXNU          ;SKIP FORWARD
        JSR     STNMMX          ;ELSE SET TO MAX
        JSR     GTNXCH          ;GET ANOTHER
        JMP     GOTNUM          ;AND CONTINUE
NTMXNU
        JSR     TSTDEC          ;TEST FOR DECIMAL
        BCC     GOTNUM          ;IF NOT JUMP
        JSR     BLDNUM          ;ELSE BUILD PARM
GOTNUM
        JSR     TSTNM0          ;TEST FOR ZERO
        BNE     *+6             ;BRANCH IF NOT
        LDA     #MINUS          ;IF ZERO SET
        STA     PRMSGN          ;SIGN TO MINUS
;B FOR BEGINNING OR END (+B OR -B)
        LDA     NXTCHR          ;GET CHAR
        CMP     #'B'            ;IF NOT B
        BNE     NOTBGN          ;TRY NEXT
        LDA     PRMSGN          ;GET SIGN
        EOR     #$FF            ;COMPLEMENT IT
        STA     PRMSGN          ;AND SAVE
        LDA     #<(TXTBUF+1)    ;SET
        LDY     #>(TXTBUF+1)    ;LOWER
        STA     LWRLMT          ;LIMIT
        STY     LWRLMT+1        ;TO START
        LDA     MAXTXT          ;AND
        LDY     MAXTXT+1        ;UPPER
        STA     UPRLMT          ;LIMIT
        STY     UPRLMT+1        ;TO MAX
        JSR     MOVONL          ;MOVE
        JMP     PRSMRE          ;AND LOOP
;C FOR MOVE N CHAR
NOTBGN
        CMP     #'C'            ;IF NOT C
        BNE     NOTCMV          ;TRY NEXT
        JSR     CHRLMT          ;CALC LIMITS
        JSR     MOVONL          ;AND MOVE
        JMP     PRSMRE          ;AND LOOP
;D FOR DELETE N CHARACTERS
NOTCMV
        CMP     #'D'            ;IF NOT D
        BNE     NOTCDL          ;TRY NEXT
        JSR     CHRLMT          ;CALC LIMITS
        JSR     MOVDLT          ;MOVE AND DELEUE
        JMP     PRSMRE          ;AND LOOP
;K FOR KILL N LINES
NOTCDL
        CMP     #'K'            ;IF NOT K
        BNE     NOTKLN          ;TRY NEXT
        JSR     LMTCLC          ;CALC LIMITS
        JSR     MOVDLT          ;MOVE WITH DELETE
        JMP     PRSMRE          ;LOOP FOR MORE
;L FOR MOVE N LONES
NOTKLN
        CMP     #'L'            ;IF NOT L
        BNE     NOTMLN          ;TRY NEXT
        JSR     MOVNLN          ;DO THE MOVE
        JMP     PRSMRE          ;AND LOOP
;T FOR TYPE N LINES
NOTMLN
        CMP     #'T'            ;IF NOT T
        BNE     NOTTYP          ;TRY NEXT
        JSR     TYPE            ;GO DO IT
        JMP     PRSMRE          ;LOOP FOR MORE
;CR FOR MOVE N LINES AND TYPE
NOTTYP
        CMP     #CR             ;IF NOT A CR
        BNE     NTCR            ;TRY NEXT
        LDX     MACFLG          ;BUT IF A MACRO
        BNE     ENDCR           ;DO NOTHING
        LDX     PRSSTR          ;GET START
        CPX     #1              ;IF NOT ONE
        BNE     ENDCR           ;DO NOTHING
        JSR     MOVNLN          ;MOVE
        JSR     DFLPRM          ;THEN TYPE
        JSR     TYPE            ;ONE LINE
ENDCR
        JMP     PRSMRE          ;LOOP
;FOR ALL FOLLOWING COMMANDS SIGN MUST BE PLUS
NTCR
        BIT     PRMSGN          ;TEST SIGN
        BMI     TRYA            ;OK IF PLUS
        JSR     TSTNM0          ;IF NUMBER ZERO
        BEQ     *+5             ;IS OK
        JMP     UNRCCM          ;ELSE UNKNOWN
        LDA     NXTCHR          ;GET CHAR
;A FOR APPEND LINES
TRYA
        CMP     #'A'            ;IF OT A
        BNE     NOTAPP          ;TRY NEXT
        LDA     NXTTXT          ;SET
        LDY     NXTTXT+1        ;LOWER
        STA     LWRLMT          ;MIMIT
        STY     LWRLMT+1        ;TO NEXT
        LDA     MAXTXT          ;AND UPPER
        LDY     MAXTXT+1        ;LIMIT
        STA     UPRLMT          ;TO
        STY     UPRLMT+1        ;MAXIMUM
        JSR     MOVONL          ;THEN MOVE
        JSR     TSTNM0          ;IF NUMBER NOT ZERO
        BNE     *+4             ;THEN USE IT
        INC     NUMBER          ;ELSE SET TO ONE
APPNLP
        JSR     TSTADJ          ;TEST FOR ZERO
        BEQ     APPEND          ;EXIT IF DONE
        JSR     ADDLNE          ;ELSE ADD A LINE
        JMP     APPNLP          ;AND LOOP FOR NEXT
APPEND
        LDA     #MINUS          ;SET SIGN TO
        STA     PRMSGN          ;MINUS AND
        JSR     MOVONL          ;MOVE BACK
        JMP     PRSMRE          ;LOOP FOR MORE
;F FOR FIND STRING
NOTAPP
        CMP     #'F'            ;IF NOT F
        BNE     NOTFND          ;TRY NEXT
        JSR     CLSTFL          ;CLEAR INDEX AND FILL
FNDSTR
        JSR     TSTADJ          ;TEST NUMBER
        BNE     *+5             ;CONTINUE IF NOT ZERO
        JMP     PRSMRE          ;ELSE DONE
        JSR     MATCH           ;TRY TO MATCH
        JMP     FNDSTR          ;JUMP IF MATCH
;M FOR MACRO
NOTFND
        CMP     #'M'            ;IF NOT M
        BNE     NOTMAC          ;TRY NEXT
        LDX     MACFLG          ;JUMP IF FLAG
        BNE     NOTMAC          ;NOT ZERO
        LDA     #$FF            ;ELSE SET INDEX
        STA     MACIND          ;TO -1
        LDA     NUMBER+1        ;IF HIGH NUMBER
        BNE     MACLPE          ;IS NOT ZERO THEN USE
        LDX     NUMBER          ;IF LOW NUMBER
        DEX                     ;IS NOT ZERO
        BNE     MACLPE          ;THEN USE
        JSR     STNM0           ;ELSE SET TO ZESO
MACLPE
        INC     MACIND          ;BUMP INDEX
        JSR     GETCHR          ;GET CHAR
        LDX     MACIND          ;AND NEW INDEX
        STA     MACBUF,X        ;SAVE CHAR
        CMP     #CR             ;IF NOT A CR
        BNE     MACLPE          ;LOOP FOR MORE
        LDA     MACIND          ;GET INDEX
        STA     MACFLG          ;AND SET FLAG
        LDA     #0              ;THEN CLEAR
        STA     MACIND          ;INDEX
        LDA     NUMBER          ;SET
        LDY     NUMBER+1        ;MACRO
        STA     MACCNT          ;COUNT
        STY     MACCNT+1        ;TO NUMBER
        JMP     PRSMRE          ;AND LOOP
;S FOR SUBSTITUTE STRINGS
NOTMAC
        CMP     #'S'            ;IF NOT S
        BNE     NOTSUB          ;TRY NEXT
        JSR     CLSTFL          ;GET SEARCH STRING
        JSR     FLSTBF          ;THEN REPLACEMENT
SUBLPE
        JSR     TSTADJ          ;TEST NUMBER
        BNE     *+5             ;JUMP IF MORE
        JMP     PRSMRE          ;ELSE DONE
        JSR     MATCH           ;TRY TO MATCH
        SEC                     ;DROP
        LDA     NXTTXT          ;NEXT
        SBC     ENDSTR          ;POINTER
        STA     NXTTXT          ;BY
        BCS     *+4             ;SEARCH
        DEC     NXTTXT+1        ;LENGTH
        LDX     ENDSTR          ;GET START INDEX
SBCHLP
        CPX     STRIND          ;COMPARE TO END
        BCS     SUBLPE          ;BRANCH IF END OR MORE
        LDA     STRBUF,X        ;ELSE GET CHAR
        STA     NXTCHR          ;SAVE IT
        JSR     INSTCH          ;INSERT IN TEXT
        INX                     ;BUMP INDEX
        BNE     SBCHLP          ;AND LOOP
;W FOR WRITE
NOTSUB
        CMP     #'W'            ;IF NOT W
        BNE     NOTWRT          ;TRY NEXT
        JSR     WRITE           ;ELSE WRITE
        JMP     PRSMRE          ;AND LOOP
;X FOR TRANSFER TO X$$$$$$$.LIB
NOTWRT
        CMP     #'X'            ;IF NOT X
        BNE     NOTXFR          ;TRY NEXT
        JSR     STUPXL          ;SET UP FILES
        JSR     TSTNM0          ;TEST FOR ZERO
        BNE     NTDLXL          ;BRANCH IF NOT
        JSR     INTXLB          ;ELSE
        LDA     #<XLBFCB        ;DELETE
        LDY     #>XLBFCB        ;THE
        JSR     DLTFLE          ;FILE
        JMP     PRSMRE          ;AND LOOP
NTDLXL
        JSR     INTXLB          ;ELSE SET UP
        LDA     #<XLBFCB        ;THEN
        LDY     #>XLBFCB        ;DELETE
        JSR     DLTFLE          ;FILE
        LDA     #<XLBFCB        ;THEN
        LDY     #>XLBFCB        ;INITIALIZE
        JSR     CRTFLE          ;IT AGAIN
        BNE     *+5             ;BRANCH IF OK
        JMP     DOSERR          ;ELSE ERROR
        JSR     LMTCLC          ;CALCULATE LIMITS
        LDA     LWRLMT          ;THEN SET
        LDY     LWRLMT+1        ;POINTER
        STA     XLBPNT          ;TO LOWER
        STY     XLBPNT+1        ;LIMIT
XFRLPE
        LDA     UPRLMT          ;IF XLBPNT
        CMP     XLBPNT          ;IS GREATER
        LDA     UPRLMT+1        ;THAN UPPER
        SBC     XLBPNT+1        ;LIMIT
        BCC     ENDXFR          ;END TRANSFER
        LDY     #0              ;ELSE CLEAR INDEX
        LDA     (XLBPNT),Y      ;GET BYTE
        JSR     PUTXLB          ;AND WRITE
        INC     XLBPNT          ;BUMP
        BNE     XFRLPE          ;POINTER
        INC     XLBPNT+1        ;AND
        BNE     XFRLPE          ;LOOP
ENDXFR
        BIT     XLBIND          ;TEST INDEX
        BMI     CLSXFR          ;DONE IF > 127
        LDA     #EOF            ;ELSE INSERT
        JSR     PUTXLB          ;AN EOF
        JMP     ENDXFR          ;AND LOOP
CLSXFR
        LDA     #<XLBFCB        ;WRITE
        LDY     #>XLBFCB        ;FINAL
        JSR     WRTRCR          ;RECORD
        BEQ     *+5             ;CONTINUE IF OK
        JMP     DOSERR          ;ELSE ERROR
        LDA     #<XLBFCB        ;CLOSE
        LDY     #>XLBFCB        ;THE
        JSR     CLSFLE          ;FILE
        JMP     PRSMRE          ;LOOP FOR MORE
;NULL COMMAND
NOTXFR
        CMP     #NULL           ;IF NOT NULL
        BNE     *+5             ;BRANCH
        JMP     PRSMRE          ;ELSE LOOP
        JMP     UNRCCM          ;BAD COMMAND
;-------------------------------
;SUBROUTINES
;-------------------------------
;CALCULATE CURRENT COLUMN
GETCOL
        LDA     #$FF            ;TURN OFF OUTPUT
        STA     OUTFLG
        JSR     CLEAR           ;DO A CR
        JSR     STNM0           ;TYPE LINE TO GET COLUMN
        LDA     #MINUS
        STA     PRMSGN
        JSR     TYPE
        LSR     OUTFLG          ;CLEAR FLAG
        RTS
;DO BACKSPACE SEQUENCE
DOBS
        LDA     BACKSP          ;DO ONE
        JSR     CNSOUT
        LDA     #' '            ;THEN A SPACE
        JSR     CNSOUT
        LDA     BACKSP          ;AND ONE MORE
        JMP     CNSOUT
;CLEAR CURRENT LINE
CLEAR
        LDA     #CR             ;GET A RETURN
        JSR     CHROUT          ;SEND IT
        LDA     CLREOL          ;THEN A CLEAR
        JMP     CNSOUT          ;TO EOL
;READ FROM CONDEF BLOCK
GETSYS
        LDA     $FF00,X         ;DUMMY INDEXED
        RTS
;FLUSH TEXT BUFFER AND SOURCE FILE TO DESTINATION FILE
FLUSFL
        JSR     EMTXBF          ;EMPTY TEXT BUFFER
FLUSLP
        JSR     GETSRC          ;GET SOURCE CHAR
        CMP     #EOF            ;IF EOF
        BEQ     FLUSEX          ;THEN CLOSE
        JSR     PUTDST          ;ELSE PUT IN DEST
        JMP     FLUSLP          ;AND LOOP
FLUSEX
        JMP     CLSDST          ;CLOSE OUT DEST
;EMPTY TEXT BUFFER TO DESTINATION FILE
EMTXBF
        JSR     STNMMX          ;SET NUMBER TO MAX
        JMP     WRITE           ;AND WRITE
;CARRIAGE RETURN AND LINEFEED
CRLF
        LDA     #CR
        JSR     CHROUT
        LDA     #LF
;OUTPUT CHAR TO CONSOLE
CHROUT
        CMP     #BLANK          ;IF BLANK OR MORE
        BCS     SNDINC          ;SEND WITH COL BUMP
        CMP     #CR             ;IF A RETURN
        BEQ     CLRCOL          ;CLEAR COLUMN
        CMP     #LF             ;IF A LINEFEED
        BEQ     CNSOUT          ;JUST SEND
        CMP     #TAB            ;IF A TAB
        BEQ     TABBLK          ;GO EXPAND
        CMP     #NULL           ;IF NOT A
        BNE     *+3             ;NULL CONTINUE
        RTS                     ;ELSE DONE
        PHA                     ;SAVE CHAR
        LDA     INVERT          ;GET INVERT
        CMP     #' '            ;SEE IF PRINTING
        BCC     NOINC           ;BRANCH IF NOT
        JSR     SNDINC          ;ELSE SEND WITH BUMP
        JMP     COUT            ;AND CONTINUE
NOINC
        JSR     CNSOUT          ;SEND IT
COUT
        PLA                     ;GET CHAR
        ORA     #'A'-1          ;CONVERT TO ASCII
        JSR     SNDINC          ;AND SEND
        LDA     NORMAL          ;GET NORMAL
        JMP     CNSOUT          ;AND SEND IT
TABBLK
        LDA     #BLANK          ;SEND A
        JSR     SNDINC          ;SPACE
        LDA     COLUMN          ;GET COLUMN
        AND     #7              ;IF NOT MOD 8
        BNE     TABBLK          ;THE LOOP
        RTS                     ;ELSE DONE
CLRCOL
        LDA     #255            ;SET COLUMN
        STA     COLUMN          ;TO -1
        LDA     #CR             ;GET CR BACK
SNDINC
        INC     COLUMN          ;BUMP COLUMN
CNSOUT
        BIT     OUTFLG          ;TEST FLAG
        BPL     *+3             ;PRINT IF CLEAR
        RTS
        LDX     #2              ;AND SEND
        JMP     PEM             ;THROUGH PEM
;READ CHARACTER FROM CONSOLE
CNSIN
        LDX     #1
        JMP     PEM
;PRINT STRING
PRTBUF
        LDX     #9
        JMP     PEM
;PRINT CR AND LF AND THEN STRING
PRCRBF
        PHA                     ;SAVE
        TYA                     ;STRING
        PHA                     ;POINTER
        JSR     CRLF            ;DO CR AND LF
        PLA                     ;RESTORE
        TAY                     ;STRING
        PLA                     ;POINTER
        JMP     PRTBUF          ;AND PRINT
;OPEN FILE (Z=1 IF ERROR)
OPNFLE
        LDX     #15
        JSR     PEM             ;EXECUTE
        CMP     #255            ;SEE IF BAD
        RTS
;CLOSE FILE (Z=1 IF ERROR)
CLSFLE
        LDX     #16
        JSR     PEM             ;EXECUTE
        CMP     #255            ;SEE IF BAD
        RTS
;DELETE FILE
DLTFLE
        LDX     #19
        JMP     PEM
;READ RECORD
RDERCR
        LDX     #20
        JMP     PEM
;WRITE RECORD
WRTRCR
        LDX     #21
        JMP     PEM
;CREATE FILE (Z=1 IF ERROR)
CRTFLE
        LDX     #22
        JSR     PEM             ;EXECUTE
        CMP     #255            ;SEE IF BAD
        RTS
;RENAME FILE
RNMFLE
        LDX     #23
        JMP     PEM
;CHECK CONSOLE STATUS (Z=1 IF NONE)
CONSTS
        LDX     #11
        JSR     PEM             ;CHECK FOR KEY
        BNE     *+3             ;BRANCH IF READY
        RTS                     ;ELSE DONE
        JSR     CNSIN           ;CLEAR INPUT
        LDA     #255            ;AND SET Z=0
        RTS
;READ CURRENT DRIVE
RDECUR
        LDX     #25
        JMP     PEM
;SET DRIVE
SETDRV
        LDX     #14
        JMP     PEM
;SET BUFFER ADDRESS
SETBUF
        LDX     #26
        JMP     PEM
;DELETE X$$$$$$$.LIB AND BOOT
DLXLEX
        LDA     CURDRV          ;SET DRIVE
        JSR     SETDRV          ;TO CURRENT
        LDA     #<XLBFCB        ;POINT TO
        LDY     #>XLBFCB        ;FCB
        JSR     DLTFLE          ;DELETE IT
        LDA     DFLDRV          ;SET DRIVE TO DEFAULT
        JSR     SETDRV
        JMP     BOOT            ;AND BOOT
;DOS/65 ERROR EXIT
DOSERR
        JSR     CRLF            ;SEND A CR AND LF
        LDA     #<PEMERR        ;THEN SEND
        LDY     #>PEMERR        ;ANOTHER CR AND LF
        JSR     PRCRBF          ;AND MESSAGE
        LDA     #<DSTFCB        ;CLOSE
        LDY     #>DSTFCB        ;OUTPUT
        JSR     CLSFLE          ;FILE
        JSR     CRLF            ;ANOTHER CR AND LF
        JMP     DLXLEX          ;AND DELETE X$$$$$$$.LIB AND BOOT
;INITIALIZE X$$$$$$$.LIB
INTXLB
        LDA     #0              ;CLEAR
        STA     XLBFCB+12       ;EXTENT
        STA     XLBFCB+13
        STA     XLBFCB+32       ;RECORD
        STA     XLBIND          ;AND INDEX
        RTS
;SET UP FOR X$$$$$$$.LIB TRANSFER
STUPXL
        LDA     CURDRV          ;SET DRIVE
        JSR     SETDRV          ;TO CURRENT
        LDA     #<XLBBUF        ;THEN POINT
        LDY     #>XLBBUF        ;BUFFER
        JMP     SETBUF          ;TO CORRECT
;CLEAR SOURCE INDEX TO START
CLSIND
        LDA     #<SOURCE        ;GET
        LDY     #>SOURCE        ;START
        STA     SRCIND          ;THEN SET
        STY     SRCIND+1        ;INDEX
        RTS
;SET CURRENT DRIVE AND DEFAULT BUFFER
CURDFL
        LDA     CURDRV          ;SET
        JSR     SETDRV          ;DRIVE
        LDA     #<DFLBUF        ;THEN
        LDY     #>DFLBUF        ;DO
        JMP     SETBUF          ;BUFFER
;GET CHAR FROM .LIB FILE
GETLIB
        LDX     LIBIND          ;GET INDEX
        BPL     LBINOK          ;USE IF <128
        JSR     CURDFL          ;ELSE SETUP
        LDA     #<LIBFCB        ;THEN
        LDY     #>LIBFCB        ;READ
        JSR     RDERCR          ;A RECORD
        BEQ     *+5             ;USE IF OK
        LDA     #EOF            ;ELSE GET EOF
        RTS                     ;AND RETURN
        TAX                     ;SET INDEX
        STX     LIBIND          ;AND SAVE
LBINOK
        INC     LIBIND          ;BUMP FO NEXT
        LDA     DFLBUF,X        ;GET CHAR
        RTS
;TEST FOR NUMBER = ZERO (Z=1 IF ZERO)
TSTNM0
        LDA     NUMBER          ;SEE IF
        ORA     NUMBER+1        ;BOTH ZERO
        RTS
;SET NUMBER TO ZERO
STNM0
        LDA     #0              ;CLEAR
        STA     NUMBER          ;BOTH
        STA     NUMBER+1
        RTS
;SET NUMBER TO MAX
STNMMX
        LDA     #$FF            ;SET
        STA     NUMBER          ;BOTH
        STA     NUMBER+1        ;TO $FF
        RTS
;TEST FOR ZERO AND IF NOT DROP NUMBER (Z=1 IF ZERO)
TSTADJ
        JSR     TSTNM0          ;TEST FOR ZERO
        BNE     *+3             ;JUMP IF NOT
        RTS
        JSR     DRNUMB          ;DROP NUMBER
        LDA     #$FF            ;SET Z
        RTS                     ;TO 0
;DROP NUMBER BY ONE
DRNUMB
        LDA     NUMBER          ;GET LOW
        BNE     *+4             ;IF NOT ZERO
        DEC     NUMBER+1        ;SKIP HIGH DROP
        DEC     NUMBER          ;ALWAYS DROP LOW
        RTS
;BUMP NEXT POINTER BY ONE
BPNXTX
        INC     NXTTXT          ;BUMP LOW
        BNE     *+4             ;DONE IF NOT ZERO
        INC     NXTTXT+1        ;BUMP HIGH
        RTS
;DROP NEXT TEXT POINTER BY ONE
DRNXTX
        LDA     NXTTXT          ;GET LOW
        BNE     *+4             ;IF NOT ZERO SKIP
        DEC     NXTTXT+1        ;DROP OF HIGH
        DEC     NXTTXT          ;ALWAYS DROP LOW
        RTS
;BUMP UPPER TEXT POINTER BY ONE
BPUPTX
        INC     UPRTXT          ;BUMP LOW
        BNE     *+4             ;DONE IF NOT ZERO
        INC     UPRTXT+1        ;BUMP HIGH
        RTS
;DROP UPPER TEXT POINTER BY ONE
DRUPTX
        LDA     UPRTXT          ;GET LOW
        BNE     *+4             ;IF NOT ZERO SKIP
        DEC     UPRTXT+1        ;DROP OF HIGH
        DEC     UPRTXT          ;ALWAYS DROP LOW
        RTS
;SET DEFAULT PARM VALUES
DFLPRM
        LDA     #PLUS           ;SET SIGN
        STA     PRMSGN          ;TO PLUS
        LDY     #1              ;AND
        STY     NUMBER          ;NUMBER
        DEY                     ;TO
        STY     NUMBER+1        ;1
        RTS
;TEST UPRTXT AGAINST UPRLMT (C=0 IF UPRTXT < UPRLMT)
TSUPUP
        LDA     UPRTXT          ;COMPARE UPPER
        CMP     UPRLMT          ;TO UPPER LIMIT
        LDA     UPRTXT+1        ;AND SET
        SBC     UPRLMT+1        ;CARRY
        RTS                     ;ACCORDINGLY
;TEST NXTTXT AGAINST LWRLMT (C=0 IF NXTTXT > LWRLMT)
TSNXLW
        LDA     LWRLMT          ;COMPARE NEXT
        CMP     NXTTXT          ;TO LOWER LIMIT
        LDA     LWRLMT+1        ;AND SET
        SBC     NXTTXT+1        ;CARRY
        RTS                     ;ACCORDINGLY
;GET NEXT BYTE FROM SOURCE
GETSRC
        LDA     SRCIND          ;COMPARE
        CMP     #<(SOURCE+SRCLNG);POINTER
        LDA     SRCIND+1        ;TO
        SBC     #>(SOURCE+SRCLNG);MAXIMUM
        BCC     *+5             ;USE IF LESS
        JSR     RDESRC          ;ELSE READ MORE
        LDY     #0              ;CLEAR INDEX
        LDA     (SRCIND),Y      ;GET CHAR
        CMP     #EOF            ;IF NOT EOF
        BNE     *+3             ;GO BUMP POINTER
        RTS                     ;ELSE DONE
        INC     SRCIND          ;BUMP LOW
        BNE     *+4             ;DONE IF NOT ZERO
        INC     SRCIND+1        ;BUMP HIGH
        RTS
;INSERT CHAR INTO .LIB FCB
CHTOLF
        LDA     NXTCHR          ;GET CHAR
        JSR     CNVLWR          ;CONVERT TO UPPER
        LDX     LIBIND          ;GET INDEX
        INC     LIBIND          ;BUMP FOR NEXT
        STA     LIBFCB,X        ;STORE CHAR
        RTS
;MOVE TO LIMITS IN DIRECTION OF SIGN
MOVONL
        LDY     #0              ;CLEAR INDEX
        BIT     PRMSGN          ;TEST SIGN
        BPL     MOVOMI          ;IF MINUS GO DO IT
MOVOPL
        JSR     TSUPUP          ;COMPARE UPPER TO UPPER LIMIT
        BCC     *+3             ;CONTINUE IF LESS
        RTS                     ;ELSE DONE
        JSR     BPUPTX          ;BUMP UPPER POINTER
        LDA     (UPRTXT),Y      ;GET BYTE
        STA     (NXTTXT),Y      ;STORE CHAR
        JSR     BPNXTX          ;THEN BUMP NEXT
        JMP     MOVOPL          ;AND LOOP
MOVOMI
        JSR     TSNXLW          ;COMPARE LOWER LIMIT TO NEXT
        BCC     *+3             ;CONTINUE IF LESS
        RTS                     ;ELSE DONE
        JSR     DRNXTX          ;DROP NEXT POINTER
        LDA     (NXTTXT),Y      ;GET CHAR
        STA     (UPRTXT),Y      ;STORE
        JSR     DRUPTX          ;DROP UPPER POINTER
        JMP     MOVOMI          ;AND LOOP
;MOVE TO LIMITS IN DIRECTION OF SIGN AND DELETE
MOVDLT
        LDY     #0              ;CLEAR INDEX
        BIT     PRMSGN          ;TEST SIGN
        BPL     MOVDMI          ;IF MINUS GO DO IT
MOVDPL
        JSR     TSUPUP          ;TEST UPPER AGAINST UPPER LIMIT
        BCC     *+3             ;CONTINUE IF LESS
        RTS                     ;ELSE DONE
        JSR     BPUPTX          ;BUMP UPPER POINTER
        JMP     MOVDPL          ;AND LOOP
MOVDMI
        JSR     TSNXLW          ;COMPARE LOWER TO NEXT
        BCC     *+3             ;CONTINUE IF LESS
        RTS                     ;ELSE DONE
        JSR     DRNXTX          ;DROP NEXT POINTER
        JMP     MOVDMI          ;AND LOOP
;READ SOURCE TO FILL BUFFER
RDESRC
        JSR     CLSIND          ;SET INDEX TO START
        LDA     CURDRV          ;SET
        JSR     SETDRV          ;DRIVE
        LDA     #SRCLNG/128     ;AND SET SECTOR
        STA     RDSCCN          ;COUNT
RDESLP
        LDA     SRCIND          ;GET CURRENT
        LDY     SRCIND+1        ;POINTER
        JSR     SETBUF          ;AND SET AS BUFFER
        LDA     #<DFLFCB        ;POINT
        LDY     #>DFLFCB        ;TO FCB
        JSR     RDERCR          ;READ RECORD
        BEQ     RDESOK          ;BRANCH IF OK
        BPL     *+5             ;EOF IF POSITIVE
        JMP     DOSERR          ;ELSE ERROR
        LDY     #0              ;CLEAR INDEX
        LDA     #EOF            ;GET AN EOF
        STA     (SRCIND),Y      ;PUT INTO BUFFER
        JMP     CLSIND          ;EXIT WITH INDEX SET
RDESOK
        CLC                     ;ADD
        LDA     SRCIND          ;128 TO
        ADC     #128            ;LOW PART
        STA     SRCIND          ;OF POINTER
        BCC     *+4             ;IF NO CARRY SKIP
        INC     SRCIND+1        ;BUMP OF HIGH
        DEC     RDSCCN          ;DROP COUNT
        BNE     RDESLP          ;LOOP FOR MORE
        JMP     CLSIND          ;THEN SET INDEX TO START
;PUT CHAR INTO X$$$$$$$.LIB BUFFER
PUTXLB
        LDX     XLBIND          ;GET INDEX
        BPL     GDXLBI          ;IF <128 USE IT
        PHA                     ;ELSE SAVE CHAR
        JSR     STUPXL          ;SET UP FOR WRITE
        LDA     #<XLBFCB        ;POINT
        LDY     #>XLBFCB        ;TO FCB
        JSR     WRTRCR          ;WRITE A RECORD
        BEQ     *+5             ;CONTINUE IF OK
        JMP     DOSERR          ;ELSE ERROR
        TAX                     ;CLEAR INDEX
        STX     XLBIND          ;AND SAVE
        PLA                     ;GET CHAR
GDXLBI
        INC     XLBIND          ;BUMP FOR NEXT
        STA     XLBBUF,X        ;INSERT CHAR
        RTS
;SET DESTINATION INDEX TO START
CLDIND
        LDA     #<DEST          ;SET
        LDY     #>DEST          ;INDEX
        STA     DSTIND          ;TO START
        STY     DSTIND+1        ;OF BUFFER
        RTS
;MOVE PRIMARY NAME TO SECOND HALF OF FCB
MOVNME
        LDX     #15             ;MOVE ALL
        LDA     DSTFCB,X        ;OF FIRST HALF
        STA     DSTFCB+16,X     ;TO SECOND
        DEX                     ;HALF
        BPL     MOVNME+2        ;THEN
        RTS                     ;EXIT
;TEST CHAR FOR FIRST AND ONLY COMMAND AND NOT MACRO
; Z=1 IF TRUE
TSTFON
        CMP     NXTCHR          ;IF CHAR NOT
        BNE     TSTFEX          ;SAME IS FALSE
        LDA     CNSLNG          ;IF CONSOLE LENGTH
        CMP     #1              ;NOT ONE
        BNE     TSTFEX          ;IS FALSE
        LDA     MACFLG          ;IS TRUE IF NOT MACRO
TSTFEX
        RTS
;TEST FOR FIRST AND ONLY AND VERIFY
TSTVRF
        PHA                     ;SAVE CHAR
        JSR     TSTFON          ;TEST FOR FIRST
        BEQ     *+4             ;CONTINUE IF IT IS
        PLA                     ;CLEAR STACK
        RTS                     ;DONE
        JSR     CRLF            ;SEND A CR AND LF
        PLA                     ;GET CHAR
        JSR     CNSOUT          ;AND SEND IT
        LDA     #<QUSMSG        ;SEND
        LDY     #>QUSMSG        ;-(Y/N)?
        JSR     PRTBUF          ;MESSAGE
        JSR     CNSIN           ;GET ANSWER
        JSR     CNVLWR          ;CONVERT TO UPPER CASE
        PHA                     ;SAVE ANSWER
        JSR     CRLF            ;ECHO A CR AND LF
        PLA                     ;GET CHAR
        CMP     #'Y'            ;SEE IF Y
        RTS
;TEST FOR DECIMAL (IF DECIMAL THEN C=1 ELSE C=0)
TSTDEC
        LDA     NXTCHR          ;GET CHAR
        CMP     #'0'            ;IF "0" OR MORE
        BCS     *+3             ;MAY BE DECIMAL
        RTS                     ;ELSE IS NOT
        CMP     #'9'+1          ;IF > "9"
        BCS     *+6             ;IS NOT DECIMAL
        AND     #$F             ;MAKE A NIBBLE
        SEC                     ;AND SET FLAG
        RTS
        CLC                     ;IS NOT DECIMAL
        RTS
;BUILD DECIMAL NUMBER FROM INPUT
BLDNUM
        JSR     STNM0           ;CLEAR NUMBER
        JSR     TSTDEC          ;GET NEXT DIGIT
        BCS     *+3             ;IF 0-9 USE
        RTS                     ;ELSE DONE
        PHA                     ;SAVE DIGIT
        LDA     NUMBER          ;GET LOW NUMBER
        ASL     A               ;MULT BY TWO
        STA     POINT           ;AND SAVE
        LDA     NUMBER+1        ;GET HIGH NUMBER
        ROL     A               ;MULT IT BY TWO
        STA     POINT+1         ;AND SAVE
        LDX     #3              ;THEN
MULN2
        ASL     NUMBER          ;MULTIPLY
        ROL     NUMBER+1        ;NUMBER
        DEX                     ;BY
        BNE     MULN2           ;EIGHT
        CLC                     ;ADD
        LDA     NUMBER          ;8X
        ADC     POINT           ;TO
        STA     NUMBER          ;2X
        LDA     NUMBER+1        ;TO
        ADC     POINT+1         ;GET
        STA     NUMBER+1        ;10X
        CLC                     ;THEN
        PLA                     ;GET DIGIT
        ADC     NUMBER          ;ADD IT
        STA     NUMBER          ;AND SAVE
        BCC     *+4             ;THEN PROPOGATE
        INC     NUMBER+1        ;CARRY
        JSR     GTNXCH          ;GET NEXT CHAR
        JMP     BLDNUM+3        ;AND LOOP
;FILL STRING BUFFER UNTIL END
FLSTBF
        JSR     GTTSEN          ;GET CHAR AND TEST
        BNE     *+3             ;CONTINUE IF NOT END
        RTS                     ;ELSE DONE
        CMP     #CTLL           ;IF NOT A CTL-L
        BNE     FLSTNC          ;THEN CHECK FOR NULL
        LDA     #CR             ;ELSE
        STA     NXTCHR          ;INSERT
        JSR     STBFIN          ;A CR
        LDA     #LF             ;THEN A
        STA     NXTCHR          ;LF
FLSTNC
        CMP     #NULL           ;IF NOT A NULL
        BNE     *+5             ;USE IT
        JMP     UNRCCM          ;ELSE IS ERROR
        JSR     STBFIN          ;INSERT
        JMP     FLSTBF          ;AND LOOP
;INSERT A CHAR INTO STRING BUFFER
STBFIN
        LDX     STRIND          ;GET INDEX
        LDA     NXTCHR          ;AND CHAR
        STA     STRBUF,X        ;STORE IT
        INX                     ;BUMP INDEX
        STX     STRIND          ;AND SAVE
        CPX     #STRMAX         ;IF LESS THAN MAX
        BCC     *+5             ;IS OK
        JMP     MMBFFL          ;ELSE IS TOO LONG
        RTS
;INSERT CHAR INTO TEXT BUFFER
INSTCH
        LDA     NXTTXT          ;IF NEXT
        CMP     UPRTXT          ;LESS THAN
        LDA     NXTTXT+1        ;UPPER
        SBC     UPRTXT+1        ;THEN
        BCC     *+5             ;USE IT
        JMP     MMBFFL          ;ELSE BUFFER FULL
        LDA     NXTCHR          ;GET CHAR
        LDY     #0              ;CLEAR INDEX
        STA     (NXTTXT),Y      ;STORE CHAR
        JMP     BPNXTX          ;BUMP POINTER
;GET CHAR AND TEST FOR END OF STRING
; IF END THEN Z=1 ELSE Z=0
GTTSEN
        JSR     GETCHR          ;GET CHAR
        STA     NXTCHR          ;SAVE IT
        CMP     #EOF            ;IF NOT AN EOF
        BNE     *+3             ;THEN TRY CR
        RTS                     ;ELSE DONE
        CMP     #CR             ;IF A CR
        BEQ     *+3             ;MAY BE OK
        RTS                     ;ELSE IS NOT
        LDX     INSFLG          ;IF NOT INSERT
        RTS                     ;THEN IS END
;ADD LINE TO TEXT BUFFER
ADDLNE
        LDA     NXTTXT          ;IF NEXT
        CMP     UPRTXT          ;LESS
        LDA     NXTTXT+1        ;THAN
        SBC     UPRTXT+1        ;UPPER
        BCC     *+5             ;USE
        JMP     MMBFFL          ;ELSE SEND FULL MESSAGE
        JSR     GETSRC          ;GET BYTE
        CMP     #EOF            ;IF NOT EOF
        BNE     *+5             ;CONTINUE
        JMP     STNM0           ;ELSE EXIT WITH N=0
        LDY     #0              ;CLEAR INDEX
        STA     (NXTTXT),Y      ;STORE BYTE
        JSR     BPNXTX          ;BUMP NEXT POINTER
        CMP     #LF             ;IF NOT A LF
        BNE     ADDLNE          ;LOOP FOR MORE
        RTS
;WRITE DESTINATION BUFFER
WRTDST
        LDA     DSTDRV          ;SET DRIVE
        JSR     SETDRV          ;TO DESTINATION
        SEC                     ;THEN
        LDA     DSTIND          ;CALCULATE
        SBC     #<DEST          ;LENGTH
        STA     POINT           ;OF
        LDA     DSTIND+1        ;BUFFER
        SBC     #>DEST          ;IN
        STA     POINT+1         ;BYTES
        LDX     #7              ;DIVIDE
WRTDV
        LSR     POINT+1         ;BY 128
        ROR     POINT           ;TO GET
        DEX                     ;NUMBER
        BNE     WRTDV           ;RECORDS
        CPX     POINT           ;IF NUMBER
        BNE     *+3             ;NON-ZERO OK
        RTS                     ;ELSE EMPTY FILE
        JSR     CLDIND          ;SET INDEX TO START
WRTDLP
        LDA     DSTIND          ;THEN
        LDY     DSTIND+1        ;SET BUFFER
        JSR     SETBUF          ;ADDRESS
        LDA     #<DSTFCB        ;POINT TO
        LDY     #>DSTFCB        ;FCB AND
        JSR     WRTRCR          ;WRITE
        BEQ     *+5             ;OK IF ZERO
        JMP     DOSERR          ;ELSE ERROR
        CLC                     ;ADD
        LDA     DSTIND          ;128
        ADC     #128            ;TO
        STA     DSTIND          ;BUFFER
        BCC     *+4             ;ADDRESS
        INC     DSTIND+1        ;FOR NEXT WRITE
        DEC     POINT           ;DROP SECTOR COUNT
        BNE     WRTDLP          ;LOOP IF MORE
        JMP     CLDIND          ;ELSE SET INDEX
;PUT BYTE IN DESTINATION BUFFER
PUTDST
        LDX     DSTIND          ;IF LOW
        CPX     #<(DEST+DSTLNG) ;NOT AT MAX
        BNE     NODSWR          ;THEN OK
        LDX     DSTIND+1        ;OR IF HIGH
        CPX     #>(DEST+DSTLNG) ;NOT AT MAX
        BNE     NODSWR          ;IS ALSO OK
        PHA                     ;SAVE CHAR
        JSR     WRTDST          ;WRITE BUFFER
        PLA                     ;GET CHAR
NODSWR
        LDY     #0              ;THEN INSERT
        STA     (DSTIND),Y      ;CHAR
        INC     DSTIND          ;AND
        BNE     *+4             ;BUMP
        INC     DSTIND+1        ;INDEX
        RTS
;CLOSE DESTINATION FILE
CLSDST
        SEC                     ;GET LOW
        LDA     DSTIND          ;BYTE
        SBC     #<DEST          ;OF OFFSET
        AND     #127            ;SEE IF MOD 128
        BEQ     WHLREC          ;IF SO OK
        LDA     #EOF            ;ELSE INSERT
        JSR     PUTDST          ;AN EOF
        JMP     CLSDST          ;AND LOOP
WHLREC
        JSR     WRTDST          ;WRITE IT ALL
        LDA     #<DSTFCB        ;THEN
        LDY     #>DSTFCB        ;CLOSE
        JSR     CLSFLE          ;.$$$
        BNE     *+5             ;CONTINUE IF OK
        JMP     DOSERR          ;ELSE ERROR
        LDA     #<BAKSTR        ;CHANGE
        LDY     #>BAKSTR        ;.$$$
        JSR     CHGTYP          ;TO .BAK
        JSR     MOVNME          ;THEN MOVE
        LDA     CURDRV          ;SET CURRENT
        JSR     SETDRV          ;DRIVE
        LDX     #15             ;THEN
DFDSMV
        LDA     DFLFCB,X        ;MOVE SOURCE
        STA     DSTFCB,X        ;NAME TO DEST
        DEX                     ;FCB
        BPL     DFDSMV
        LDA     #<DSTFCB        ;POINT
        LDY     #>DSTFCB        ;TO IT
        JSR     RNMFLE          ;AND NAME IT .BAK
        JSR     MOVNME          ;PUT IT IN SECOND
        LDA     #<DLRSTR        ;CHANGE
        LDY     #>DLRSTR        ;TYPE
        JSR     CHGTYP          ;TO .$$$
        LDA     DSTDRV          ;GO TO
        JSR     SETDRV          ;DESTINATION
        LDA     #<DSTFCB        ;AND END
        LDY     #>DSTFCB        ;WITH
        JMP     RNMFLE          ;IT RENAMED
;TEST FOR LOWER CASE (IF LOWER CASE THEN C=1 ELSE C=0)
TSTLWR
        CMP     #'A'            ;IF LESS THAN "A"
        BCC     NTLWR           ;IS NOT LOWER CASE
        CMP     #'Z'+1          ;IF Z+1 OR MORE
        BCS     NTLWR           ;IS NOT LOWER
        SEC                     ;IS LOWER
        RTS                     ;CASE
NTLWR
        CLC                     ;NOT LOWER
        RTS                     ;CASE
;CONVERT CHARACTER TO UPPER CASE
CNVLWR
        JSR     TSTLWR          ;TEST FOR LOWER
        BCC     *+4             ;EXIT IF NOT
        AND     #%01011111      ;ELSE CONVERT
        RTS
;GET INPUT CHARACTER
GETCHR
        LDA     MACFLG          ;GET FLAG
        BEQ     NOTMCI          ;BRANCH IF NOT MACRO
        JSR     CONSTS          ;TEST FOR BREAK
        BEQ     *+5             ;CONTINUE IF NONE
        JMP     CNTDMR          ;ELSE DO BREAK
        LDX     MACIND          ;GET INDEX
        CPX     MACFLG          ;COMPARE TO MAX
        BCC     USMCIN          ;USE IF LESS
        LDA     MACCNT          ;IF COUNT
        ORA     MACCNT+1        ;IS ZERO
        BEQ     CLMCIN          ;CLEAR INDEX
        LDA     MACCNT          ;ELSE
        BNE     *+4             ;DROP
        DEC     MACCNT+1        ;COUNT
        DEC     MACCNT          ;BY ONE
        LDA     MACCNT          ;IF RESULT
        ORA     MACCNT+1        ;IS NOT ZERO
        BNE     *+5             ;GO AHEAD
        JMP     CNTDMR          ;ELSE DONE
CLMCIN
        LDX     #0              ;CLEAR
        STX     MACIND          ;INDEX
USMCIN
        INC     MACIND          ;BUMP FOR NEXT
        LDA     MACBUF,X        ;GET CHAR
        RTS
;NOT MACRO
NOTMCI
        BIT     INSFLG          ;TEST INSERT MODE
        BPL     NTINMD          ;JUMP IF NOT
        JMP     CNSIN           ;GET CHAR
;COMMAND MODE
NTINMD
        BIT     NOMORE          ;TEST FOR NO INPUT
        BPL     ISINPT          ;BRANCH IF INPUT
        LDA     #0              ;ELSE CHANGE
        STA     NOMORE          ;STATUS
        LDA     #'*'            ;SEND PROMPT
        JSR     CHROUT          ;TO CONSOLE
        LDA     #<CNSBUF        ;GET
        LDY     #>CNSBUF        ;INPUT
        LDX     #10             ;LINE
        JSR     PEM             ;FROM DOS
        LDA     #LF             ;ECHO A
        JSR     CHROUT          ;LINEFEED
        LDA     #0              ;CLEAR
        STA     COLUMN          ;COLUMN AND
        STA     CNSIND          ;INDEX
ISINPT
        LDA     #0              ;CLEAR ACCUM
        LDX     CNSIND          ;GET INDEX
        CPX     CNSLNG          ;COMPARE TO LENGTH
        PHP                     ;SAVE RESULT
        BNE     *+4             ;JUMP IF NOT SAME
        LDA     #$FF            ;ELSE SET
        STA     NOMORE          ;NO MORE INPUT FLAG
        PLP                     ;GET RESULT
        BNE     NOTEQL          ;JUMP IF NOT EQUAL
        LDA     #CR             ;THEN INSERT
        STA     CNSTXT,X        ;A CR
NOTEQL
        INC     CNSIND          ;BUMP NEXT INDEX
        LDA     CNSTXT,X        ;GET CHAR
        RTS
;MOVE N LINES
MOVNLN
        JSR     LMTCLC          ;CALCULATE LIMITS
        JMP     MOVONL          ;AND MOVE
;CHANGE TYPE OF OUTPUT TO STRING POINTED TO BY YA
CHGTYP
        STA     POINT           ;SET
        STY     POINT+1         ;POINTER
        LDY     #2              ;SET INDEX
CHGTLP
        LDA     (POINT),Y       ;GET NEW VALUE
        STA     DSTFCB+9,Y      ;STORE IN FCB
        DEY                     ;COUNT DOWN
        BPL     CHGTLP          ;LOOP FOR MORE
        RTS
;GET NEXT COMMAND CHARACTER
GTNXCH
        JSR     GETCHR          ;GET CHAR
        JSR     CNVLWR          ;CONVERT
        STA     NXTCHR          ;SAVE
        RTS
;INSERT A CR AND LF
INCRLF
        LDA     #CR             ;INSERT
        STA     NXTCHR          ;THE
        JSR     INSTCH          ;CR
        LDA     #LF             ;THEN
        STA     NXTCHR          ;THE
        JMP     INSTCH          ;LF
;CALCULATE LIMITS FROM NUMBER CHARACTERS
CHRLMT
        BIT     PRMSGN          ;TEST SIGN
        BMI     CHRPOS          ;BRANCH IF POSITIVE
;NEGATIVE
        LDA     UPRTXT          ;SET UPPER
        LDY     UPRTXT+1        ;LIMIT
        STA     UPRLMT          ;TO CURRENT
        STY     UPRLMT+1        ;MAXIMUM
        SEC                     ;THEN
        LDA     NXTTXT          ;SUBTRACT
        SBC     NUMBER          ;NUMBER
        STA     LWRLMT          ;FROM
        LDA     NXTTXT+1        ;NEXT
        SBC     NUMBER+1        ;POINTER
        STA     LWRLMT+1        ;AND SAVE
        BCC     CHTOSM          ;JUMP IF BORROW
        LDA     LWRLMT          ;IF RESULT
        CMP     #<(TXTBUF+1)    ;IS
        LDA     LWRLMT+1        ;TXTBUF+1
        SBC     #>(TXTBUF+1)    ;OR MORE
        BCS     USCLPT          ;USE IT
CHTOSM
        LDA     #<(TXTBUF+1)    ;ELSE
        LDY     #>(TXTBUF+1)    ;USE
        STA     LWRLMT          ;TXTBUF+1
        STY     LWRLMT+1        ;AS LOWER LIMIT
USCLPT
        RTS
;POSITIVE
CHRPOS
        LDA     NXTTXT          ;SET LOWER
        LDY     NXTTXT+1        ;LIMIT
        STA     LWRLMT          ;TO NEXT
        STY     LWRLMT+1        ;POSITION
        CLC                     ;CALCULATE
        LDA     UPRTXT          ;UPPER
        ADC     NUMBER          ;LIMIT
        STA     UPRLMT          ;AS
        LDA     UPRTXT+1        ;UPPER
        ADC     NUMBER+1        ;PLUS
        STA     UPRLMT+1        ;NUMBER
        BCS     CHTOBG          ;IF CARRY TOO BIG
        LDA     UPRLMT          ;COMPARE
        CMP     TXTLMT          ;RESULT
        LDA     UPRLMT+1        ;TO TEXT
        SBC     TXTLMT+1        ;LIMIT
        BCC     NTTOBG          ;USE IF LESS
CHTOBG
        LDA     MAXTXT          ;ELSE
        LDY     MAXTXT+1        ;SET
        STA     UPRLMT          ;UPPER TO
        STY     UPRLMT+1        ;MAXIMUM
NTTOBG
        RTS
;CALCULATE MOVE/DELETE LIMITS AS FUNCTION OF LINES
LMTCLC
        BIT     PRMSGN          ;TEST SIGN
        BPL     LMTCMI          ;BRANCH IF MINUS
;POSITIVE
        LDX     UPRTXT          ;GET UPPER LOW
        LDY     UPRTXT+1        ;AND HIGH
        STX     UPRLMT          ;SET
        STY     UPRLMT+1        ;POINTER
        INX                     ;BUMP LOWER
        BNE     *+3             ;IF NOT ZERO SKIP
        INY                     ;BUMP OF HIGH
        STX     LWRLMT          ;SAVE
        STY     LWRLMT+1        ;ALWAYS
        LDY     #1              ;SET INDEX
LMTPLP
        LDA     UPRLMT          ;COMPARE UPPER
        CMP     MAXTXT          ;LIMIT TO MAXIMUM
        BNE     LMTPNE          ;BRANCH IF NOT EQUAL
        LDA     UPRLMT+1        ;DO SAME FOR
        CMP     MAXTXT+1        ;HIGH BYTES
        BNE     LMTPNE          ;BRANCH IF NOT SAME
        LDA     NUMBER          ;DROP
        BNE     *+4             ;NUMBER
        DEC     NUMBER+1        ;BY
        DEC     NUMBER          ;ONE
        RTS                     ;AND EXIT
LMTPNE
        LDA     (UPRLMT),Y      ;GET CHAR
        CMP     #LF             ;IF A LF
        BEQ     LMTPLE          ;BRANCH
LMTPAD
        INC     UPRLMT          ;ELSE
        BNE     LMTPLP          ;BUMP
        INC     UPRLMT+1        ;UPPER LIMIT
        JMP     LMTPLP          ;AND LOOP
LMTPLE
        LDA     NUMBER          ;DROP NUMBER
        BNE     *+4             ;FOR
        DEC     NUMBER+1        ;LINEFEED
        DEC     NUMBER          ;BY ONE
        BNE     LMTPAD          ;LOOP IF NOT ZERO
        LDA     NUMBER+1        ;TEST HIGH
        BNE     LMTPAD          ;LOOP IF IT NOT ZERO
        INC     UPRLMT          ;BUMP
        BNE     *+4             ;LIMIT
        INC     UPRLMT+1        ;AND
        RTS                     ;EXIT
;NEGATIVE
LMTCMI
        LDX     NXTTXT          ;GET NEXT
        LDY     NXTTXT+1        ;POSITION
        STX     LWRLMT          ;SET LOWER
        STY     LWRLMT+1        ;LIMIT
        CPX     #0              ;IF LOW NOT ZERO
        BNE     *+3             ;DO NOT
        DEY                     ;DROP HIGH
        DEX                     ;DROP LOW ALWAYS
        STX     UPRLMT          ;SET UPPER
        STY     UPRLMT+1        ;LIMIT
        INC     NUMBER          ;BUMP
        BNE     *+4             ;NUMBER
        INC     NUMBER+1        ;BY ONE
        LDY     #0              ;CLEAR INDEX
LMTMLP
        LDA     LWRLMT          ;COMPARE LOWER
        CMP     #<TXTBUF        ;TO START
        BNE     LMTMNE          ;BRANCH IF DIFFERENT
        LDA     LWRLMT+1        ;DO SAME
        CMP     #>TXTBUF        ;FOR HIGH
        BNE     LMTMNE          ;BRANCH IF NOT SAME
        LDA     NUMBER          ;DROP
        BNE     *+4             ;NUMBER
        DEC     NUMBER+1        ;BY
        DEC     NUMBER          ;ONE
        INC     LWRLMT          ;BUMP
        BNE     *+4             ;LOWER
        INC     LWRLMT+1        ;BACK UP
        RTS                     ;AND EXIT
LMTMNE
        LDA     LWRLMT          ;DROP
        BNE     *+4             ;LIMIT
        DEC     LWRLMT+1        ;BY
        DEC     LWRLMT          ;ONE
        LDA     (LWRLMT),Y      ;GET CHAR
        CMP     #LF             ;IF NOT A LF
        BNE     LMTMLP          ;THEN BRANCH
        LDA     NUMBER          ;ELSE
        BNE     *+4             ;DROP
        DEC     NUMBER+1        ;NUMBER
        DEC     NUMBER          ;BY ONE
        BNE     LMTMLP          ;LOOP IF NOT ZERO
        LDA     NUMBER+1        ;IF HIGH NOT ZERO
        BNE     LMTMLP          ;THEN LOOP
        INC     LWRLMT          ;ELSE BUMP
        BNE     *+4             ;BACK TO
        INC     LWRLMT+1        ;CHAR AFTER
        RTS                     ;THE LF
;WRITE N LINES TO DESTINATION
; IF N=0 THEN N <-- 1
WRITE
        LDA     #MINUS          ;SET SIGN
        STA     PRMSGN          ;TO MINUS
        LDA     #<(TXTBUF+1)    ;THEN SET
        LDY     #>(TXTBUF+1)    ;POINTER
        STA     LWRLMT          ;FOR
        STY     LWRLMT+1        ;MOVE
        LDA     UPRTXT          ;TO BEGINNING
        LDY     UPRTXT+1        ;OF THE
        STA     UPRLMT          ;TEXT
        STY     UPRLMT+1        ;BUFFER
        JSR     MOVONL          ;DO THE MOVE
        JSR     TSTNM0          ;TEST FOR ZERO
        BNE     *+4             ;JUMP IF NOT
        INC     NUMBER          ;ELSE MAKE IT ONE
WRLNLP
        JSR     TSTADJ          ;TEST FOR END
        BEQ     WRLNEN          ;JUMP IF DONE
WRCHLP
        LDA     UPRTXT          ;COMPARE
        CMP     MAXTXT          ;UPPER
        LDA     UPRTXT+1        ;POINTER
        SBC     MAXTXT+1        ;TO LIMIT
        BCC     *+7             ;BRANCH IF LESS
        JSR     STNM0           ;ELSE CLEAR NUMBER
        BEQ     WRLNEN          ;AND EXIT
        JSR     BPUPTX          ;BUMP UPPER POINTER
        LDY     #0              ;CLEAR INDEX
        LDA     (UPRTXT),Y      ;GET CHAR
        PHA                     ;SAVE IT
        JSR     PUTDST          ;INSERT IN DEST
        PLA                     ;RESTORE CHAR
        CMP     #LF             ;IF NOT A LF
        BNE     WRCHLP          ;LOOP FOR MORE CHAR
        BEQ     WRLNLP          ;ELSE LOOP FOR LINE
WRLNEN
        LDA     UPRTXT          ;IF UPPER
        CMP     UPRLMT          ;POINTER
        LDA     UPRTXT+1        ;IS NOT
        SBC     UPRLMT+1        ;AT LIMIT
        BCC     *+3             ;THEN MOVE
        RTS                     ;ELSE DONE
        LDA     #PLUS           ;SET SIGN
        STA     PRMSGN          ;TO PLUS
        JMP     MOVONL          ;AND MOVE
;MATCH STRINGS
MATCH
        JSR     COMPRE          ;DO COMPARISON
        BNE     *+3             ;JUMP IF NONE
        RTS                     ;ELSE OK
        JMP     CNTDMR          ;BREAK FOR NONE
;TYPE N LINES
TYPE
        JSR     LMTCLC          ;CALCULATE LIMITS
        LDA     #$FF            ;SET INSERT
        STA     INSFLG          ;MODE
        BIT     PRMSGN          ;TEST SIGN
        BPL     TYPEMI          ;JUMP IF NEGATIVE
        LDA     NXTTXT          ;ELSE SET
        LDY     NXTTXT+1        ;TYPE POINTER
        STA     TYPPNT          ;TO NEXT
        STY     TYPPNT+1        ;CHAR POINTER
        JMP     TYPEIT          ;AND CONTINUE
TYPEMI
        LDA     LWRLMT          ;FOR NEGATIVE
        LDY     LWRLMT+1        ;SET POINTER
        STA     TYPPNT          ;TO LOWER
        STY     TYPPNT+1        ;LIMIT OF TEXT
TYPEIT
        LDA     TYPPNT          ;BACKUP
        BNE     *+4             ;POINTER
        DEC     TYPPNT+1        ;BY
        DEC     TYPPNT          ;ONE
        LDY     #0              ;CLEAR INDEX
        LDA     (TYPPNT),Y      ;GET CHAR
        CMP     #LF             ;IF NOT A LF
        BNE     NTLNBG          ;THEN SKIP AHEAD
        LDA     COLUMN          ;GET COLUMN
        BEQ     NTLNBG          ;JUMP IF ZERO
        JSR     CRLF            ;ELSE SEND CR AND LF
NTLNBG
        LDA     LWRLMT          ;SET TYPE
        LDY     LWRLMT+1        ;POINTER
        STA     TYPPNT          ;TO LOWER
        STY     TYPPNT+1        ;LIMIT
TYPELP
        LDA     UPRLMT          ;IF LIMIT
        CMP     TYPPNT          ;GREATER THAN
        LDA     UPRLMT+1        ;OR EQUAL
        SBC     TYPPNT+1        ;POINTER THEN
        BCS     TYPEMR          ;CONTINUE
        RTS
TYPEMR
        LDY     #0              ;CLEAR INDEX
        LDA     (TYPPNT),Y      ;GET CHAR
        PHA                     ;SAVE IT
        JSR     CHROUT          ;SEND IT
        PLA                     ;THEN GET IT BACK
        INC     TYPPNT          ;BUMP
        BNE     *+4             ;POINPER
        INC     TYPPNT+1        ;BY ONE
        CMP     #LF             ;IF NOT A LF
        BNE     TYPELP          ;THEN LOOP FOR MORE
        JSR     CONSTS          ;ELSE SEE IF BREAK
        BEQ     TYPELP          ;LOOP IF NOT
        JMP     CNTDMR          ;ELSE DO HALT
;COMPARE STRING TO BUFFER CONTENTS
; Z=1 IF COMPARE ELSE Z=0
COMPRE
        LDA     UPRTXT          ;SET COMPARE
        LDY     UPRTXT+1        ;POINTER TO
        STA     CMPPNT          ;START OF
        STY     CMPPNT+1        ;UPPER
COMPLP
        LDA     CMPPNT          ;IF COMPARE
        CMP     MAXTXT          ;POINTER
        LDA     CMPPNT+1        ;IS LESS
        SBC     MAXTXT+1        ;THAN MAX
        BCC     *+5             ;THEN CONTINUE
        JMP     NOCOMP          ;ELSE NO COMPARE
        INC     CMPPNT          ;BUMP
        BNE     *+4             ;POINTER
        INC     CMPPNT+1        ;BY ONE
        LDA     CMPPNT          ;AND
        LDY     CMPPNT+1        ;ALSO
        STA     UPRLMT          ;SET
        STY     UPRLMT+1        ;LIMIT
        LDY     #0              ;CLEAR INDEX
COMPNX
        LDA     STRBUF,Y        ;GET STRING
        CMP     (CMPPNT),Y      ;COMPARE TO TEXT
        BNE     COMPLP          ;RESTART IF NO MATCH
        INC     UPRLMT          ;BUMP UPPER
        BNE     *+4             ;LIMIT
        INC     UPRLMT+1        ;AND
        INY                     ;BUMP INDEX
        CPY     ENDSTR          ;COMPARE TO END + 1
        BNE     COMPNX          ;LOOP IF MORE
        LDA     UPRLMT          ;DROP
        BNE     *+4             ;UPPER
        DEC     UPRLMT+1        ;LIMIT
        DEC     UPRLMT          ;BY ONE
        JSR     MOVONL          ;AND MOVE
        LDA     #0              ;RETURN WITH
        RTS                     ;Z=1
NOCOMP
        LDA     #$FF            ;RETURN WITH
        RTS                     ;Z=0
;CLEAR STRING BUFFER INDEX AND FILL
CLSTFL
        LDA     #0              ;CLEAR
        STA     STRIND          ;INDEX
        JSR     FLSTBF          ;FILL BUFFER
        LDA     STRIND          ;THEN SET
        STA     ENDSTR          ;END
        RTS
;STRINGS AND MESSAGES
BAKSTR
        .BYTE   "BAK"
DLRSTR
        .BYTE   "$$$"
OPNMSG
        .BYTE   "EDIT - VERSION 2.03-A"
        .BYTE   CR,LF,"$"
HELP0
        .BYTE   "COMMAND SUMMARY - S IS SIGN"
        .BYTE   " - N IS NUMBER "
        .BYTE   "[TYPE # FOR MAX]"
        .BYTE   CR,LF,"  SN TO MOVE RIGHT [S=+]"
        .BYTE   " OR LEFT [S=-] N LINES"
        .BYTE   " AND TYPE LINE"
        .BYTE   CR,LF,"  NA TO APPEND N LINES"
        .BYTE   CR,LF,"  SB TO MOVE TO BEGINNING"
        .BYTE   " [S=+] OR END [S=-]"
        .BYTE   CR,LF,"  SNC TO MOVE N CHARACTERS"
        .BYTE   " RIGHT OR LEFT"
        .BYTE   "$"
HELP1
        .BYTE   CR,LF,"  SND TO DELETE N "
        .BYTE   "CHARACTERS RIGHT OR LEFT"
        .BYTE   CR,LF,"  E TO EXIT"
        .BYTE   CR,LF,"  NFSTRING TO FIND NTH "
        .BYTE   "OCCURENCE OF STRING"
        .BYTE   CR,LF,"  H TO RETURN TO HEAD OF FILE"
        .BYTE   CR,LF,"  I TO INSERT TEXT"
        .BYTE   CR,LF,"  SNK TO KILL N LINES RIGHT"
        .BYTE   " OR LEFT"
        .BYTE   CR,LF,"  SNL TO MOVE N LINES RIGHT"
        .BYTE   " OR LEFT"
        .BYTE   "$"
HELP2
        .BYTE   CR,LF,"  NM TO DO MACRO N TIMES"
        .BYTE   CR,LF,"  O TO RESTART WITH "
        .BYTE   "ORIGINAL FILE"
        .BYTE   CR,LF,"  Q TO QUIT WITHOUT "
        .BYTE   "ALTERING FILE"
        .BYTE   CR,LF,"  RNAME TO READ LIBRARY FILE"
        .BYTE   CR,LF,"  NSSTRING1[CTL-Z]STRING2"
        .BYTE   " TO SUBSTITUTE"
        .BYTE   " STRING2 FOR STRING1"
        .BYTE   CR,LF,"  SNT TO TYPE N LINES RIGHT"
        .BYTE   " OR LEFT"
        .BYTE   "$"
HELP3
        .BYTE   CR,LF,"  NW TO WRITE N LINES"
        .BYTE   CR,LF,"  NX TO STORE N LINES IN"
        .BYTE   " TEMP BUFFER"
        .BYTE   CR,LF,"$"
NWFMSG
        .BYTE   "NEW FILE$"
QUSMSG
        .BYTE   "-(Y/N)?$"
PEMERR
        .BYTE   "PEM FILE ERROR (FULL?)$"
ATMSG
        .BYTE   " AT $"
BRKMSG
        .BYTE   "BREAK - $"
FLXMSG
        .BYTE   "DESTINATION FILE EXISTS$"
MFLMSG
        .BYTE   "MEMORY BUFFER FULL$"
LFEMSG
        .BYTE   "LIBRARY FILE ERROR$"
CNCMSG
        .BYTE   "CAN NOT DO COMMAND "
        .BYTE   "SPECIFIED TIMES$"
URCMSG
        .BYTE   "UNRECOGNIZED COMMAND$"
;ERROR TABLE
ERRTBL
        .WORD   CNCMSG
        .WORD   URCMSG
        .WORD   MFLMSG
        .WORD   LFEMSG
;BUFFERS AND FCBS
;CONSOLE BUFFER
CNSBUF
        .BYTE   128
CNSLNG
        .BYTE   0
CNSTXT
        .RES    128
;.LIB FILE FCB
LIBFCB
        .RES    9
        .BYTE   "LIB"
        .RES    21
;X$$$$$$$.LIB FILE FCB
XLBFCB
        .RES    1
        .BYTE   "X$$$$$$$LIB"
        .RES    21
;DESTINATION FILE FCB
DSTFCB
        .RES    33
;STRING BUFFER
STRBUF
        .RES    STRMAX
;MACRO BUFFER
MACBUF
        .RES    128
;X$$$$$$$.LIB BUFFER
XLBBUF
        .RES    128
;SOURCE BUFFER
SOURCE
        .RES    SRCLNG
;DESTINATION BUFFER
DEST
        .RES    DSTLNG
;TEXT BUFFER
TXTBUF
        .END
