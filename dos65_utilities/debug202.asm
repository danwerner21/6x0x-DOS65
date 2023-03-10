;DEBUG - DOS/65 DEBUGGER
;VERSION 2.02-A
;RELEASED:	30 SEPTEMBER 1982
;LAST REVISION:
;	29 MARCH 2008
;		REFORMATTED FOR ASM210 & TASM
;		DELETED REDUNDANT COPYRIGHT NOTICE
;		ELIMINATED PAGE ZERO 0 & 1
;	30 MARCH 2008
;		SET MINIMUM LOAD ADDRESS TO TEA
;		ALIGNED START OF RUN TIME ON PAGE BOUNDARY
;		CORRECTED ERROR NEAR JMP (EXECUT)
;BASE ADDRESSES
BTEJMP          = $100          ;SIM+3 ENTRY
PEMJMP          = $103          ;PEM ENTRY
DFLFCB          = $107          ;DEFAULT FCB
DFLBUF          = $128          ;DEFAULT BUFFER
MINMEM          = $800          ;MINIMUM LOAD ADDRESS
TEA             = $800          ;TEA START

;ZERO PAGE FOR SETUP
POINT           = $02           ;RELOCATE POINTER
ADJUST          = $04           ;RELOCATE DISTANCE
LOWER           = $06           ;LOWER LIMIT
PAGLIM          = $08           ;UPPER PAGE LIMIT
LENGTH          = $09           ;PROGRAM LENGTH
NEWSTR          = $0B           ;START AFTER MOVE
EXECUT          = $0D           ;EXECUTE VECTOR
FROM            = $0F           ;START FOR MOVE
MOVCNT          = $11           ;COUNTER FOR MOVE
;FIXED PARAMETERS
LF              = $A            ;LINEFEEED
CR              = $D            ;RETURN
EOF             = $1A           ;END OF FILE
SEMICO          = 59            ;SEMICOLON
;SET-UP POINTERS
        .FEATURE labels_without_colons
        .SEGMENT "TEA"
        .ORG    $0800

        JMP     START           ;GO START
        .BYTE   "COPYRIGHT (C) 2008 -"
        .BYTE   " RICHARD A. LEARY"
;FIRST USE PEM JMP IN PAGE 1 TO GET VECTOR TO PEM SO PEM
;LINK CAN BE MAINTAINED.
START
        LDA     PEMJMP+1        ;SET VECTOR
        LDY     PEMJMP+2        ;TO PEM
        STA     VECTOR          ;FROM PAGE
        STY     VECTOR+1        ;ONE JUMP
        STA     GOTOPM+1        ;SET MESSAGE PEM CALL ADDRESS
        STY     GOTOPM+2
        LDX     #17-1           ;INITIALIZE 17 BYTES
SETZPG
        LDA     INITTB,X        ;PAGE
        STA     2,X             ;ZERO
        DEX                     ;VARIABLES
        BPL     SETZPG          ;AND POINTERS
;ZERO PAGE HAS BEEN INITIALIZED
;NOW CALCULATE DESTINATION START ADDRESS FOR RELOCATION AND MOVE
        SEC                     ;SUBTRACT LOW OF LENGTH
        LDA     VECTOR          ;FROM LOW
        SBC     LENGTH          ;PEM LOCATION
        STA     NEWSTR          ;AND SAVE
        STA     PEMJMP+1        ;ALSO IN VECTOR AROUND DEBUG
        STA     EXECUT          ;ALSO IN FINAL LINK
        LDA     VECTOR+1        ;GET HIGH
        SBC     LENGTH+1        ;AND SUBTRACT HIGH
        STA     NEWSTR+1        ;AND SAVE
        STA     PEMJMP+2
        STA     EXECUT+1
;NOW CALCULATE DISTANCE TO MOVE SINCE POINT STARTS OUT SET TO
;PEMVEC LOCATION
        SEC                     ;CALCULATE
        LDA     NEWSTR          ;DISTANCE
        SBC     POINT           ;TO
        STA     ADJUST          ;RELOCATE
        LDA     NEWSTR+1        ;FROM NEW
        SBC     POINT+1         ;AND OLD
        STA     ADJUST+1        ;START
;BEGIN RELOCATION
STRTRL
        LDY     #0              ;GET OPCODE
        LDA     (POINT),Y       ;FROM MEMORY
        TAY                     ;SAVE IN Y
        LDX     #7              ;SEARCH SEVEN
RLLOOP
        TYA                     ;GET OPCODE
        AND     TABLE1-1,X      ;REMOVE BITS
        EOR     TABLE2-1,X      ;TEST REST
        BEQ     GOTOPC          ;IF ZERO A MATCH
        DEX                     ;ELSE TRY
        BNE     RLLOOP          ;NEXT
GOTOPC
        LDY     TABLE3,X        ;GET LENGTH/FLAG
        BPL     SKIPBY          ;NO CHANGE IF POSITIVE
        INY                     ;ADD ONE
        BMI     STSMPG          ;EXIT IF TERMINATOR
        INY                     ;ELSE ADD ONE MORE
        LDA     (POINT),Y       ;GET LOW
        TAX                     ;SAVE IN X
        INY                     ;THEN GET
        LDA     (POINT),Y       ;HIGH
        CMP     PAGLIM          ;IF AT HIGH LIMIT
        BCS     NORELC          ;DO NOTHING
        CMP     LOWER+1         ;IF ABOVE
        BNE     TRYLOW          ;LOWER LIMIT
        CPX     LOWER           ;THEN
TRYLOW
        BCC     NORELC          ;DO NOTHING
        PHA                     ;ELSE SAVE HIGH
        TXA                     ;ADD OFFSET
        CLC                     ;TO LOW
        ADC     ADJUST          ;AND THEN
        TAX                     ;SAVE
        PLA                     ;GET HIGH
        ADC     ADJUST+1        ;ADD AGAIN
NORELC
        STA     (POINT),Y       ;PUT BACK
        DEY                     ;BACKUP
        TXA                     ;GET LOW
        STA     (POINT),Y       ;SAVE
        LDY     #3              ;GO UP THREE
SKIPBY
        INC     POINT           ;ADD ONE
        BNE     NOINCY          ;WITH
        INC     POINT+1         ;CARRY
NOINCY
        DEY                     ;LOOP IF
        BNE     SKIPBY          ;MORE
        BEQ     STRTRL          ;THEN START OVER
;SET SIM PAGE ADDRESS
STSMPG
        LDA     BTEJMP+2        ;GET SIM PAGE
        STA     GTCHLN+2        ;SET SIZE PARAM ADDRESSES
        STA     GTLNSC+2
;DISABLE INTERRUPTS BEFORE TESTING
        PHP                     ;SAVE CURRENT STATUS
        SEI                     ;THEN DISABLE
;SEE IF INTERRUPTS ALLOWED AND SET POINTERS
        LDA     #$FE            ;SET POINTER TO VECTOR
        LDY     #$FF
        STA     POINT
        STY     POINT+1
;LOOP BACK POINT FOR ROM TEST
TSPTRM
        JSR     TSTROM          ;SEE IF ROM
        BEQ     WASROM          ;JUMP IF WAS
GOTRAM
        LDA     (POINT),Y       ;ELSE GET LOW
        STA     IRQVEC          ;AND SAVE
        INY                     ;NOW GO FOR HIGH
        LDA     (POINT),Y
        STA     IRQVEC+1        ;AND SAVE
        LDA     IBKVEC+1        ;GET HIGH OF VECTOR
        STA     (POINT),Y       ;AND PUT AT POINT
        DEY                     ;DO LOW
        LDA     IBKVEC
        STA     (POINT),Y       ;IN SAME WAY
;SEND MESSAGE SAYING IRQ/BRK VECTOR HAS BEEN SET
        LDA     #<SETMSG        ;POINT TO MESSAGE
        LDY     #>SETMSG
SETEXT
        LDX     #9              ;SEND MESSAGE
GOTOPM
        JSR     $FFFF           ;TO CONSOLE
        JMP     EXRELC          ;AND CONTINUE
;AT THIS POINT IT HAS BEEN DETERMINED THAT IRQ/BRK VECTOR IS IN ROM
;BUT NOW TEST TO SEE IF IT POINTS TO A JMP, EITHER ABSOLUTE OR INDIRECT.
;IF THAT IS THE CASE SEE IF THE JMP OPERAND CAN BE MODIFIED.
WASROM
        JSR     LDEPNT          ;DO POINT LOAD
        LDA     (POINT),Y       ;GET BYTE
        CMP     #$4C            ;SEE IF JMP OPCODE
        BEQ     ISJMAB          ;JUMP IF IS
        CMP     #$6C            ;SEE IF JMP INDIRECT
        BNE     CNNTST          ;IF NOT CAN NOT SET
        JSR     BMPPNT          ;BUMP POINTER
        JSR     LDEPNT          ;THEN (POINT) --> POINT
        JMP     TSPTRM          ;AND LOOP BACK TO START
;THIS IS TARGET WHEN IT HAS BEEN DETERMINED THAT IRQ/BRK CA NOT BE SET
CNNTST
        LDY     #255            ;SET Y TO $FF
        STY     BRKOK           ;TO DISABLE BRK
;SEND MESSAGE SAYING IRQ/BRK CAN NOT BE SET
        LDA     #<NTSMSG        ;POINT TO MESSAGE
        LDY     #>NTSMSG
        JMP     SETEXT          ;THEN CONTINUE
;IT HAS BEEN DETERMINED THAT IRQ/BRK POINTS TO A JMP - SEE IF IT
;CAN BE CHANGED
ISJMAB
        JSR     BMPPNT          ;BUMP POINTER PAST JMP
        JSR     TSTROM          ;SEE IF ROM
        BEQ     WASROM          ;JUMP IF WAS
        BNE     GOTRAM          ;ELSE GO RAM
;DO ACTUAL MOVE OF DEBUG EXECUTE MODULE TO CORRECT LOCATION
EXRELC
        LDY     #0              ;CLEAR INDEX
        LDA     (FROM),Y        ;GET BYTE
        STA     (NEWSTR),Y      ;MOVE
        INC     FROM            ;THEN
        BNE     FROMOK          ;BUMP
        INC     FROM+1          ;POINTERS
FROMOK
        INC     NEWSTR          ;BOTH
        BNE     NEWOK           ;FROM AND
        INC     NEWSTR+1        ;TO
NEWOK
        SEC                     ;NOW
        LDA     MOVCNT          ;DECREMENT
        SBC     #1              ;COUNT
        STA     MOVCNT          ;BY ONE
        BCS     SKPDEC          ;JUMP IF NO BORROW
        DEC     MOVCNT+1        ;DECREMENT HIGH
SKPDEC
        ORA     MOVCNT+1        ;IF NOT ZERO
        BNE     EXRELC          ;MOVE MORE
;PREPARE TO JUMP TO NEW START + 3 TO BYPASS JMP TO TRUE PEM
        CLC                     ;ELSE
        LDA     EXECUT          ;GET START
        ADC     #3              ;ADD THREE
        STA     EXECUT          ;SAVE
        BCC     NOXQCY          ;JUMP IF NO CARRY
        INC     EXECUT+1        ;ELSE BUMP HIGH
NOXQCY
        JMP     (EXECUT)        ;THEN EXECUTE
;RELOCATION TABLES
TABLE1
        .BYTE   $C,$1F,$D,$87,$1F,$FF,$3
TABLE2
        .BYTE   $C,$19,$8,$0,$10,$20,$3
TABLE3
        .BYTE   2,$FF,$FF,1,1,2,$FF,$FE
;INITIALIZATION TABLES
INITTB
        .WORD   PEMVEC          ;POINT
        .WORD   0               ;ADJUST
        .WORD   TEA-1           ;LOWER
        .BYTE   $FF             ;PAGLIM
        .WORD   LAST-PEMVEC+1   ;LENGTH
        .WORD   0               ;NEWSTR
        .WORD   0               ;EXECUT
        .WORD   PEMVEC          ;FROM
        .WORD   LAST-PEMVEC+1   ;MOVCNT
;MESSAGES
SETMSG
        .BYTE   "IRQ/BRK VECTOR SET$"
NTSMSG
        .BYTE   "CAN NOT SET IRQ/BRK VECTOR$"
;LOAD POINT FROM (POINT)
;EXITS WITH Y=0
LDEPNT
        LDY     #0              ;CLEAR INDEX
        LDA     (POINT),Y       ;GET LOW
        TAX                     ;SAVE IN X
        INY                     ;Y TO ONE
        LDA     (POINT),Y       ;GET HIGH
        STA     POINT+1
        STX     POINT
        DEY                     ;SET Y TO ZERO
        RTS
;SUBROUTINE TO CHECK (POINT) FOR ROM
;RETURNS Z=1 IF ROM OR Z=0 IF RAM
;RETURNS WITH Y=0
TSTROM
        LDY     #0              ;CLEAR INDEX
        LDA     (POINT),Y       ;GET VALUE
        PHA                     ;SAVE ON STACK
        TYA                     ;CLEAR A
        STA     (POINT),Y       ;STORE A ZERO
        LDA     (POINT),Y       ;GET IT BACK
        BNE     PTISRM          ;IS ROM IF NOT ZERO
        LDA     #$FF            ;SET A TO FF
        STA     (POINT),Y       ;AND SET
        CMP     (POINT),Y       ;THEN COMPARE
        BNE     PTISRM          ;ROM IF DIFFERENT
        PLA                     ;IS RAM SO RESTORE
        STA     (POINT),Y
        LDA     #$FF            ;SET Z=0 FOR RAM
        RTS
PTISRM
        PLA                     ;CLEAR STACK
        STA     (POINT),Y       ;DO A RESTORE TO BE SAFE
        LDA     #0              ;SET Z=1 FOR ROM
        RTS
;BUMP PAGE ZERO POINT
BMPPNT
        INC     POINT           ;DO LOW
        BNE     *+4
        INC     POINT+1
        RTS
;----------------------------------------
;DEBUG
;FROM HERE ON IS THE CODE AND DATA THAT GETS MOVED BELOW PEM AND ACTS
;AS THE COMMAND SHELL TO LOAD FILES, MANIPULATE MEMORY, ETC
;----------------------------------------
;FIRST ALIGN START ON A PAGE BOUNDARY
        .ALIGN  256
;VECTOR TO PEM
PEMVEC
        JMP     (VECTOR)        ;VECTOR FROM PAGE ONE
;MAIN PROGRAM
DEBUG
        PLP                     ;RESTORE INTERRUPT STATUS
        JSR     SETBUF          ;POINT TO DEFAULT BUFFER
        LDA     #' '            ;IF NAME
        CMP     DFLFCB+1        ;NOT BLANK
        BNE     TRYAFN          ;CHECK FOR AFN
        CMP     DFLFCB+9        ;IF TYPE BLANK
        BEQ     GETCOM          ;DO NOTHING
TRYAFN
        LDX     #11             ;CHECK
        LDA     #'?'            ;ALL OF NAME
TSTAFN
        CMP     DFLFCB,X        ;FOR AMBIGUOUS
        BEQ     BDFILE          ;FILE NAME
        DEX                     ;COUNT DOWN
        BNE     TSTAFN          ;AND LOOP UNTIL DONE
        BEQ     RDEINP          ;THEN EXECUTE
BDFILE
        JMP     ERROR           ;ERROR IN FILE
RDEINP
        JSR     XQREAD          ;READ FILE
GETCOM
        LDX     USRS            ;RESET
        TXS                     ;STACK
        JSR     CRLF            ;DO A CR AND LF
        LDA     #'-'            ;PROMPT WITH
        JSR     CHROUT          ;A DASH
        JSR     RDEBUF          ;GET COMMAND LINE
        LDY     #0              ;SET INDEX TO ZERO
        LDX     CNSLNG          ;GET LENGTH
        BEQ     GETCOM          ;IF ZERO TRY AGAIN
LWRUPR
        LDA     CNSTXT,Y        ;GET CHARACTER
        CMP     #'A'            ;IF LESS THAN
        BCC     NXTCHG          ;A OK
        CMP     #'{'            ;OR IF OVER
        BCS     NXTCHG          ;A { OK
        AND     #%01011111      ;ELSE CONVERT
        STA     CNSTXT,Y        ;AND PUT BACK
NXTCHG
        INY                     ;BUMP INDEX
        DEX                     ;DECREMENT COUNT
        BNE     LWRUPR          ;LOOP IF MORE
        TXA                     ;SET A TO ZERO
        STA     CNSTXT,Y        ;INSERT STOPPER
        TAY                     ;SET INDEX TO ZERO
        JSR     SKPSPC          ;GET FIRST NON-BLANK
        STA     COMMND          ;SAVE COMMAND
        BEQ     GETCOM          ;TRY AGAIN IF NONE
        CMP     #'I'            ;IF NOT I
        BNE     NOTINP          ;THEN CONTINUE
        JSR     XQINPT          ;ELSE INPUT FILE NAME
        JMP     GETCOM          ;AND LOOP
NOTINP
        CMP     #'X'
        BEQ     *+5             ;CONTINUE IF X
        JMP     NOTSTE
;X=MACHINE STATE
        INY                     ;GET COMMAND
        JSR     SKPSPC          ;OBJECT REGISTER
        BEQ     NOSTCH          ;OK IF NONE
        STA     XRGCMD          ;ELSE SAVE
        INY                     ;THEN GET
        JSR     SKPSPC          ;EQUALS SIGN
        BNE     *+5             ;OK IF NOT NULL
        JMP     ERROR           ;ELSE ERROR
        CMP     #'='            ;IF EQUALS
        BEQ     *+5             ;THEN OK
        JMP     ERROR           ;ELSE ERROR
        JSR     GETPRM          ;GET PARAMETER
        LDA     NUMPRM          ;GET NUMBER PARMS
        CMP     #1              ;IF ONE
        BEQ     *+5             ;IS OK
        JMP     ERROR           ;ELSE ERROR
        LDA     XRGCMD          ;GET COMMAND
        CMP     #'*'            ;IF NOT PC
        BNE     *+8             ;TRY BYTE/BIT
        JSR     PRMUPC          ;ELSE SET PC
        JMP     NOSTCH          ;AND DISPLAY
;BYTE CHANGES
        LDA     PARM3+1         ;IF HIGH ZERO
        BEQ     *+5             ;IS OK
        JMP     ERROR           ;ELSE ERROR
        LDX     #0              ;CLEAR INDEX
        LDY     #0              ;FOR SEARCH
        LDA     XRGCMD          ;GET COMMAND
TSTREG
        CMP     REGTBL,Y        ;IF SAME AS TABLE
        BEQ     GOTREG          ;IS OK
        CPY     #8              ;IF PAST S
        BCS     *+3             ;DO NOT BUMP
        INX                     ;BYTE INDEX
        INY                     ;ELSE BUMP
        INY                     ;INDEX BY TWO
        CPY     #22             ;IF NOT TOO BIG
        BNE     TSTREG          ;TRY AGAIN
        JMP     ERROR           ;ELSE ERROR
;FOUND REGISTER OR BIT
GOTREG
        CPX     #4              ;IF A BIT CHANGE
        BEQ     ISABIT          ;GO DO IT
        LDA     PARM3           ;ELSE GET BYTE
        STA     USRA,X          ;SET REGISTER
        JMP     NOSTCH          ;AND DISPLAY
;IS A FLAG BIT
ISABIT
        INY                     ;POINT TO MASK
        LDA     PARM3           ;IF PARM ZERO
        BEQ     ZROBIT          ;THEN CLEAR
        CMP     #1              ;IF ONE OK
        BEQ     *+5             ;TO USE
        JMP     ERROR           ;ELSE ERROR
        LDA     REGTBL,Y        ;GET MASK
        STA     PARM3           ;SET
ZROBIT
        LDA     REGTBL,Y        ;GET MASK
        EOR     #$FF            ;COMPLEMENT
        AND     USRP            ;GET FLAGS
        ORA     PARM3           ;OR WITH NEW
        STA     USRP            ;AND SAVE
NOSTCH
        LDX     #0              ;CLEAR INDEX
STMSLP
        LDA     STEMSG,X        ;GET MESSAGE
        BEQ     EXTSMS          ;DONE IF NULL
        JSR     CHROUT          ;ELSE SEND
        INX                     ;NEXT CHAR
        BNE     STMSLP          ;LOOP
EXTSMS
        LDA     USRPC+1         ;GET HIGH PC
        JSR     OUTBYT          ;SEND BYTE
        LDA     USRPC           ;GET LOW PC
        JSR     BYTSPC          ;SEND BYTE AND SPACE
        LDA     USRA            ;GET A
        JSR     BYTSPC          ;SEND IT
        LDA     USRX            ;GET X
        JSR     BYTSPC          ;SEND IT
        LDA     USRY            ;GET Y
        JSR     BYTSPC          ;SEND IT
        LDA     USRS            ;GET S
        JSR     BYTSPC          ;SEND IT
        LDX     #8              ;EIGHT BITS
        LDA     USRP            ;GET FLAGS
STSLPE
        ASL     A               ;SHIFT LEFT
        PHA                     ;SAVE WHAT IS LEFT
        LDA     #0              ;GET A ZERO
        BCC     *+4             ;SKIP IF MSB IS ZERO
        LDA     #1              ;GET A ONE
        JSR     OUTNIB          ;TO CONSOLE
        PLA                     ;GET FLAGS BACK
        DEX                     ;COUNT DOWN
        BNE     STSLPE          ;LOOP IF MORE
        JMP     GETCOM          ;GET NEW COMMAND
;DISPATCH COMMAND
NOTSTE
        JSR     GETPRM          ;GET PARMS
        LDX     NUMPRM          ;GET NUMBER PARMS
        LDA     COMMND          ;AND COMMAND
        CMP     #'R'            ;IF NOT READ
        BNE     NTREAD          ;TRY NEXT
;R=READ FILE
        CPX     #2              ;IF ZERO OR ONE
        BCC     DOREAD          ;DO READ
        JMP     ERROR           ;ELSE ERROR
DOREAD
        JSR     XQREAD          ;READ FILE
        JMP     GETCOM          ;AND LOOP
NTREAD
        CMP     #'D'            ;IF NOT DISPLAY
        BNE     NTDISP          ;TRY NEXT
;D=DISPLAY MEMORY
        TXA                     ;GET NUMBER PARMS
        BNE     TDOPRM          ;IF NOT ZERO TRY ONE
CLDEND
        JSR     CLTLBY          ;ADD NUMBER BYTES MINUS ONE
        CLC
        ADC     POINTR          ;TO POINTER
        STA     PARM2           ;AND SAVE
        LDA     POINTR+1        ;IN PARM
        ADC     #0              ;NUMBER
        STA     PARM2+1         ;TWO AS END
        JMP     DODISP          ;THEN EXECUTE
TDOPRM
        CMP     #1              ;IF NOT ONE
        BNE     TDTPRM          ;TRY TWO
        JSR     PR3PTR          ;ELSE MOVE PARM3 TO POINTER
        JMP     CLDEND          ;AND SET END
TDTPRM
        CMP     #2              ;IF NOT TWO PARMS
        BNE     SUBERR          ;THEN ERROR
        JSR     CLBYLN          ;ELSE FIND BYTES PER LINE
        JSR     PR2PTR          ;MOVE PARM2 TO POINTER
        JSR     PR3PR2          ;MOVE PARM3 TO PARM2
DODISP
        JSR     XQDISP          ;DO DISPLAY
        JMP     GETCOM          ;THEN LOOP
NTDISP
        CMP     #'S'            ;IF NOT SUBSTITUTE
        BNE     NTSUBS          ;TRY NEXT
;S=SUBSTITUTE MEMORY
        TXA                     ;GET NUMBER PARMS
        BEQ     DOSUBS          ;IF ZERO DO IT
        CMP     #1              ;IF NOT ONE
        BNE     TRTWSU          ;TRY TWO
        JSR     PR3PTR          ;ELSE SET POINTER
DOSUBS
        JSR     XQSUBS          ;EXECUTE
        JMP     GETCOM          ;AND LOOP
TRTWSU
        CMP     #2              ;IF TWO PARMS
        BEQ     DOUBLE          ;DO DOUBLE STORE
SUBERR
        JMP     ERROR           ;ELSE IS ERROR
DOUBLE
        JSR     PR2PTR          ;SET POINTER
        JSR     STRABS          ;STORE ABSOLUTE
        LDA     PARM3           ;GET LOW
        JSR     MOVPNT          ;SET
        JSR     INCPNT          ;GO TO NEXT
        LDA     PARM3+1         ;GET HIGH
        JSR     MOVPNT          ;SET
        JMP     GETCOM          ;THEN LOOP
NTSUBS
        CMP     #'F'            ;IF NOT FILL
        BNE     NTFILL          ;TRY NEXT
;F=FILL MEMORY
        CPX     #3              ;IF THREE PARMS
        BEQ     DOFILL          ;DO IT
FILLER
        JMP     ERROR           ;ELSE IS ERROR
DOFILL
        LDA     PARM3+1         ;IF HIGH NOT
        BNE     FILLER          ;ZERO THEN ERROR
        JSR     PR1PTR          ;SET POINTER
        JSR     STRABS          ;STORE ABSOLUTE
FILMRE
        LDA     PARM3           ;GET BYTE
        JSR     MOVPNT          ;STORE
        JSR     INCPNT          ;POINT TO NEXT
        JSR     TSTPR0          ;TEST FOR ZERO POINTER
        BEQ     EXFILL          ;DONE IF IT IS
        JSR     LSTCHK          ;IF NOT LAST
        BCS     FILMRE          ;DO MORE
EXFILL
        JMP     GETCOM          ;ELSE LOOP
NTFILL
        CMP     #'L'            ;IF NOT LIST
        BNE     NTLIST          ;TRY NEXT
;L=LIST (DISASSEMBLE)
        TXA                     ;GET NUMBER PARMS
        BEQ     DOONLS          ;DO ONE IF ZERO
        CMP     #1              ;IF NOT ONE
        BNE     TRTWLS          ;TRY TWO
        JSR     PR3PTR          ;SET POINTER
DOONLS
        JSR     GTLNSC          ;DO NUM LINES MINUS ONE
        STA     COUNT           ;INSTRUCTIONS
DSONMR
        JSR     DSMBL           ;DO DISASSEMBLY
        DEC     COUNT           ;COUNT DOWN
        BNE     DSONMR          ;LOOP IF MORE
        JMP     GETCOM          ;ELSE GET NEXT
TRTWLS
        CMP     #2              ;IF TWO THEN
        BEQ     DOTWDS          ;DO IT
        JMP     ERROR           ;ELSE ERROR
DOTWDS
        JSR     PR2PTR          ;SET POINTER
        JSR     PR3PR2          ;MOVE PARM3 TO PARM2
DSMORE
        JSR     DSMBL           ;DO ONE INTRUCTION
        JSR     LSTCHK          ;TEST FOR END
        BCS     DSMORE          ;LOOP IF MORE
        JMP     GETCOM          ;ELSE GET NEXT COM
NTLIST
        CMP     #'G'            ;IF G
        BEQ     *+5             ;DO
        JMP     NTGO            ;ELSE TRY NEXT
;G=GO (EXECUTE)
        TXA                     ;GET NUMBER PARMS
        BNE     *+5             ;IF NOT ZERO TRY MORE
        JMP     DOGO            ;ELSE GO
        CMP     #1              ;IF NOT ONE
        BNE     TGTPRM          ;SEE IF TWO
        JSR     PRMUPC          ;SET USER PC
        JMP     DOGO            ;AND GO
TGTPRM
        CMP     #2              ;IF NOT TWO
        BNE     TG3PRM          ;SEE IF THREE
        LDA     PARM2           ;IF PC
        ORA     PARM2+1         ;IS ZERO
        BEQ     NOPC2           ;USE OLD
        LDA     PARM2           ;ELSE GET
        LDY     PARM2+1         ;NEW
        STA     USRPC           ;AND
        STY     USRPC+1         ;SET
NOPC2
        BIT     BRKOK           ;IF BRK NOT
        BMI     DOGO2           ;ALLOWED GO DO
        JSR     PR3PTR          ;ELSE MOVE
        JSR     LDEABS          ;SET LOAD MODE
        JSR     MOVPNT          ;GET OPCODE
        BEQ     DOGO2           ;IF BRK EXECUTE
        STA     OPCDE1          ;ELSE SAVE
        JSR     SETBPT          ;GET BREAK
        LDX     #0              ;SET
        JSR     PNTBPT          ;POINTER
DOGO2
        JMP     DOGO            ;AND GO
TG3PRM
        CMP     #3              ;IF THREE
        BEQ     *+5             ;THE OK
        JMP     ERROR           ;ELSE ERROR
        LDA     PARM1           ;IF PC 0
        ORA     PARM1+1         ;THEN USE
        BEQ     NOPC3           ;OLD
        LDA     PARM1           ;ELSE GET
        LDY     PARM1+1         ;NEW
        STA     USRPC           ;AND
        STY     USRPC+1         ;SET
NOPC3
        BIT     BRKOK           ;IF BRK NOT OK
        BMI     DOGO            ;JUST EXECUTE
        JSR     PR2PTR          ;ELSE SET POINTER
        JSR     LDEABS          ;LOAD MODE
        JSR     MOVPNT          ;GET OPCODE
        BEQ     TRYBK2          ;IF ZERO SEE IF SECOND
        STA     OPCDE1          ;ELSE SAVE
        JSR     SETBPT          ;GET BREAK
        LDX     #0              ;SET
        JSR     PNTBPT          ;POINTER
TRYBK2
        JSR     LDEABS          ;LOAD MODE
        JSR     PR3PTR          ;SET POINTER
        JSR     MOVPNT          ;GET OPCODE
        BEQ     DOGO            ;IF BREAK GO
        STA     OPCDE2          ;ELSE SAVE
        JSR     SETBPT          ;SET BREAK
        LDX     #2              ;THEN SET
        JSR     PNTBPT          ;LOCATION
DOGO
        LDY     USRY            ;SET Y
        LDX     USRS            ;GET STACK
        TXS                     ;AND SET
        LDA     USRPC+1         ;GET HIGH PC
        PHA                     ;PUSH
        LDA     USRPC           ;THEN GET LOW
        PHA                     ;PUSH
        LDA     USRP            ;GET FLAGS
        PHA                     ;PUSH
        LDX     USRX            ;GET X
        LDA     USRA            ;AND A
        RTI                     ;AND GO
NTGO
        JMP     GETCOM          ;LOOP
;----------------------------------------
;INPUT FILE DESIGNATOR
;----------------------------------------
XQINPT
        LDX     #11             ;FILL
        LDA     #' '            ;NAME
MRESPC
        STA     DFLFCB,X        ;AND
        DEX                     ;TYPE
        BNE     MRESPC          ;WITH SPACES
        INX                     ;SET INDEX TO ONE
        LDY     #0              ;SKIP I
        JSR     SKPSPC          ;IN COMMAND
        INY                     ;GET NEXT
        JSR     SKPSPC          ;NON BLANK CHAR
        BEQ     INPERR          ;ERROR IF NONE
        AND     #%00000111      ;CONVERT TO NUMBER
        STA     DFLFCB          ;SET AUTOMATIC
        INY                     ;POINT TO NEXT
        LDA     CNSTXT,Y        ;GET CHAR
        CMP     #':'            ;IF : THEN
        BEQ     GETNME          ;THEN CONTINUE
        DEY                     ;ELSE BACKUP
        DEY                     ;TWO PLACES
        LDA     #0              ;THEN CLEAR
        STA     DFLFCB          ;AUTOMATIC
GETNME
        INY                     ;GO TO NEXT
GTMRNM
        LDA     CNSTXT,Y        ;GET CHAR
        BEQ     ENDALL          ;OVER IF NONE
        CMP     #'.'            ;IF SEPARATOR
        BEQ     ENDNME          ;THEN HAVE NAME
        JSR     TSTLGL          ;MAKE SURE ITS LEGAL
        BEQ     INPERR          ;IF NOT ERROR
        STA     DFLFCB,X        ;ELSE PUT IN FCB
        INY                     ;POINT TO
        INX                     ;NEXT POSITIONS
        CPX     #9              ;IF NOT TO BIG
        BNE     GTMRNM          ;GET MORE
ENDNME
        LDX     #9              ;START OF TYPE
        LDA     CNSTXT,Y        ;GET INPUT
        BEQ     ENDALL          ;IF ZERO DONE
        CMP     #'.'            ;IF NOT DELIMITER
        BNE     INPERR          ;THEN ERROR
        INY                     ;POINT TO NEXT
NXTTYP
        LDA     CNSTXT,Y        ;GET CHAR
        BEQ     ENDALL          ;DONE IF NONE
        JSR     TSTLGL          ;BUT IF ILLEGAL
        BEQ     INPERR          ;IS AN ERROR
        STA     DFLFCB,X        ;SAVE CHAR
        INY                     ;POINT TO
        INX                     ;NEXT POSITIONS
        CPX     #12             ;IF NOT AT END
        BNE     NXTTYP          ;TRY ANOTHER
        JSR     SKPSPC          ;IF MORE CHAR
        BNE     INPERR          ;IS WRONG
ENDALL
        DEX                     ;AND IF NO
        BEQ     INPERR          ;INPUT IS ERROR
        RTS                     ;ELSE RETURN
INPERR
        JMP     ERROR           ;ERROR EXIT
;----------------------------------------
;READ FILE -- COM OR KIM
;----------------------------------------
XQREAD
        LDA     #0              ;CLEAR
        LDX     #32             ;FCB
ZERFCB
        STA     DFLFCB,X        ;EXCEPT
        DEX                     ;FOR NAME
        CPX     #11             ;AND TYPE
        BNE     ZERFCB          ;AND DRIVE
        JSR     OPNDFL          ;TRY TO OPEN
        CMP     #255            ;IF OK CONTINUE
        BNE     *+5             ;SINCE GOT IT
        JMP     ERROR           ;ELSE ERROR
        LDX     #2              ;IF TYPE
TSTTYP
        LDA     COMTYP,X        ;IS NOT COM
        CMP     DFLFCB+9,X      ;THEN
        BNE     KIMTYP          ;DO KIM READ
        DEX                     ;ELSE COUNT DOWN
        BPL     TSTTYP          ;AND LOOP UNTIL DONE
;COM READ
        LDA     #<TEA           ;GET TEA
        LDY     #>TEA           ;START
        CLC                     ;ADD OFFSET
        ADC     PARM3           ;LOW
        STA     POINTR          ;AND SET
        TYA                     ;POINTER
        ADC     PARM3+1         ;TO NEW
        STA     POINTR+1        ;VALUE
        TAY                     ;GET HIGH BACK
        LDA     POINTR          ;AND LOW
        PHA                     ;SAVE LOW
        CMP     #<MINMEM        ;TEST AGAINST LOW
        TYA                     ;GET HIGH
        SBC     #>MINMEM        ;SUBTRACT LOW LIMIT
        PLA                     ;GET LOW BACK
        BCS     CMRDLP          ;IF OK CONTINUE
        JMP     ERROR           ;ELSE ERROR
CMRDLP
        JSR     SETDMA          ;SET BUFFER ADDRESS
        LDA     POINTR          ;GET LOW
        LDY     POINTR+1        ;AND HIGH POINTER
        CLC                     ;ADD
        ADC     #128            ;128
        STA     POINTR          ;AND SAVE
        BCC     NOADCY          ;SKIP INC W/O CARRY
        INY                     ;ELSE BUMP HIGH
        STY     POINTR+1        ;AND SAVE
NOADCY
        CMP     STRVEC          ;TEST AGAINST DEBUG
        TYA                     ;LOWER
        SBC     STRVEC+1        ;LIMIT
        BCC     LASTOK          ;CONTINUE IF OK
        JMP     ERROR           ;ELSE ERROR
LASTOK
        JSR     RDEDFL          ;READ SECTOR
        CMP     #0              ;IF NOT OK
        BNE     ENDCOM          ;THEN DONE
        LDA     POINTR          ;ELSE GET POINTER
        LDY     POINTR+1        ;LOW AND HIGH
        JMP     CMRDLP          ;AND LOOP
ENDCOM
        CMP     #1              ;IF ONE
        BEQ     COMEOF          ;THEN EOF
        JMP     ERROR           ;ELSE ERROR
COMEOF
        JSR     NXTPCM          ;SEND MESSAGE
        LDA     POINTR          ;GET
        LDY     POINTR+1        ;POINTER
        SEC                     ;SUBTRACT
        SBC     #128            ;128 FROM
        STA     POINTR          ;POINTER
        BCS     NOADBR          ;AND SAVE
        DEY                     ;HIGH
        STY     POINTR+1        ;AND LOW
NOADBR
        PHA                     ;SAVE LOW
        TYA                     ;GET HIGH
        JSR     OUTBYT          ;SEND IT
        PLA                     ;GET LOW
        JSR     OUTBYT          ;SEND IT
        JSR     SETBUF          ;SET DEFAULT
        RTS
;KIM READ
KIMTYP
        LDA     #128            ;SET POINTER
        STA     KIMPNT          ;TO ILLEGAL
        ASL     A               ;CLEAR A
        STA     SAVPNT          ;AND SAVE POINTER
        JSR     STRABS          ;STORE ABSOLUTE
KIMLOP
        JSR     GETCHR          ;GET CHAR
        CMP     #'$'            ;IF $
        BEQ     ENDKIM          ;THEN DONE
        CMP     #EOF            ;IF EOF
        BEQ     ENDKIM          ;THEN DONE
        CMP     #SEMICO         ;IF NOT FILE MARK
        BNE     KIMLOP          ;TRY AGAIN
        JSR     GETTWO          ;GET COUNT
        BEQ     ENDKIM          ;DONE IF ZERO
        STA     COUNT           ;SET COUNT
        JSR     GETTWO          ;GET HIGH ADDR
        STA     POINTR+1        ;AND SAVE
        JSR     GETTWO          ;GET LOW ADDR
        CLC                     ;ADD
        ADC     PARM3           ;OFFSET
        STA     POINTR          ;AND SAVE
        PHA                     ;LOCALLY
        LDA     POINTR+1        ;GET HIGH
        ADC     PARM3+1         ;ADD OFFSET
        STA     POINTR+1        ;AND SAVE
        TAY                     ;SET HIGH
        PLA                     ;AND LOW
        PHA                     ;SAVE LOW AGAIN
        CMP     #<MINMEM        ;SEE IF
        TYA                     ;TOO SMALL
        SBC     #>MINMEM        ;LESS THAN TEA
        PLA                     ;GET LOW BACK
        BCS     CHKHIG          ;IF OK CONTINUE
        JMP     ERROR           ;ELSE ERROR
CHKHIG
        CLC                     ;ADD
        ADC     COUNT           ;COUNT
        BCC     NOYINC          ;TO START
        INY                     ;AND SAVE
NOYINC
        CMP     STRVEC          ;SEE IF
        TYA                     ;TOO
        SBC     STRVEC+1        ;BIG
        BCC     KMHIOK          ;IF OK CONTINUE
        JMP     ERROR           ;ELSE ERROR
KMHIOK
        JSR     GETTWO          ;GET BYTE
        JSR     MOVPNT          ;STORE
        JSR     INCPNT          ;BUMP POINTER
        DEC     COUNT           ;COUNT DOWN
        BNE     KMHIOK          ;LOOP IF MORE
        JSR     GETTWO          ;IGNORE
        JSR     GETTWO          ;CHECKSUM
        JMP     KIMLOP          ;AND LOOP
ENDKIM
        JSR     NXTPCM          ;SEND MESSAGE
        LDA     POINTR+1        ;SEND HIGH
        JSR     OUTBYT          ;POINTER
        LDA     POINTR          ;THEN LOW
        JMP     OUTBYT          ;POINTER
;----------------------------------------
;DISPLAY MEMORY
;----------------------------------------
XQDISP
        JSR     LSTCHK          ;CHECK START>=END
        BCC     ENDDSP          ;EXIT IF NOT
        JSR     LDEABS          ;LOAD ABSOLUTE
LNLOOP
        LDA     POINTR          ;GET
        LDY     POINTR+1        ;POINTER
        STA     PARM3           ;SAVE IN
        STY     PARM3+1         ;PARM3
        JSR     CRLF            ;SEND A CR AND LF
        JSR     OUTPNT          ;SEND POINTER
        JSR     OUTSPC          ;THAN A SPACE
HXLOOP
        JSR     MOVPNT          ;GET BYTE
        JSR     BYTSPC          ;SEND IT AND SPACE
        JSR     INCPNT          ;BUMP POINTER
        JSR     TSTPR0          ;TEST FOR ZERO POINTER
        BEQ     HEXEND          ;EXIT IF IT IS
        JSR     LSTCHK          ;TEST FOR END
        BCC     HEXEND          ;DONE IF IS
        LDA     POINTR          ;ELSE GET POINTER
        AND     LINMSK          ;IF NOT START
        BNE     HXLOOP          ;THEN LOOP
HEXEND
        JSR     PR3PTR          ;RESET POINTER
ASLOOP
        JSR     MOVPNT          ;GET CHAR
        AND     #%01111111      ;MASK PARITY
        CMP     #' '+1          ;IF OVER SPACE
        BCS     *+4             ;USE
        LDA     #'.'            ;ELSE USE PERIOD
        JSR     CHROUT          ;SEND AS ASCII
        JSR     INCPNT          ;BUMP POINTER
        JSR     TSTPR0          ;TEST FOR ZERO POINTER
        BEQ     ENDDSP          ;EXIT IF IT IS
        JSR     LSTCHK          ;IF MORE
        BCC     ENDDSP          ;THEN LAST THEN DONE
        LDA     POINTR          ;GET POINTER
        AND     LINMSK          ;IF MORE IN LINE
        BNE     ASLOOP          ;DO IT
        BEQ     LNLOOP          ;ELSE NEW LINE
ENDDSP
        RTS                     ;DONE
;----------------------------------------
;SUBSTITUTE
;----------------------------------------
XQSUBS
        JSR     CRLF            ;SEND A CR AND LF
        JSR     OUTPNT          ;SEND POINTER
        JSR     OUTSPC          ;THEN A SPACE
        LDA     #0              ;CLEAR
        STA     NUMBER          ;CONVERT
        STA     NUMBER+1        ;BUFFER
        LDA     #2              ;SET FOR
        STA     COUNT           ;TWO CHARACTERS
        JSR     LDEABS          ;LOAD ABSOLUTE
        JSR     MOVPNT          ;GET BYTE
        JSR     BYTSPC          ;SEND IT AND SPACE
NXSBIN
        JSR     CHRINP          ;GET CHAR
        CMP     #CR             ;IF A CR
        BEQ     EXTSUB          ;THEN DONE
        CMP     #'.'            ;IF A PERIOD
        BEQ     NXTSUB          ;GO TO NEXT
        CMP     #'A'            ;IF LESS THAN "A"
        BCC     NTLWCS          ;SKIP CONVERSION
        CMP     #'Z'+1          ;IF OVER "Z"
        BCS     NTLWCS          ;SKIP CONVERSION
        AND     #%01011111      ;ELSE CONVERT
NTLWCS
        JSR     PACK            ;PACK
        BCC     *+5             ;IF OK CONTINUE
        JMP     ERROR           ;ELSE ERROR
        DEC     COUNT           ;COUNT DOWN
        BNE     NXSBIN          ;IF MORE LOOP
        JSR     STRABS          ;STORE ABSOLUTE
        LDA     NUMBER          ;GET BYTE
        JSR     MOVPNT          ;AND SET
NXTSUB
        JSR     INCPNT          ;GO TO NEXT
        JMP     XQSUBS          ;LOOP
EXTSUB
        JMP     INCPNT          ;END WITH BUMP
;NEXT ADDRESS MESSAGE
NXTPCM
        LDX     #0              ;CLEAR INDEX
PCMLPE
        LDA     NXTMSG,X        ;GET CHAR
        BEQ     EXTPMS          ;IF NULL DONE
        JSR     CHROUT          ;ELSE SEND
        INX                     ;BUMP INDEX
        BNE     PCMLPE          ;AND LOOP
EXTPMS
        RTS
;DISASSEMBLE AN INSTRUCTION
DSMBL
        JSR     INSDSP
        JSR     PNTAJ
        STA     POINTR
        STY     POINTR+1
        RTS
INSDSP
        JSR     PRPC
        JSR     LDEABS          ;LOAD ABSOLUTE
        JSR     MOVPNT          ;GET BYTE
        TAY
        LSR     A
        BCC     IEVEN
        LSR     A
        BCS     ERR
        CMP     #$22
        BEQ     ERR
        AND     #7
        ORA     #$80
IEVEN
        LSR     A
        TAX
        LDA     MODE,X
        BCS     RTMODE
        LSR     A
        LSR     A
        LSR     A
        LSR     A
RTMODE
        AND     #$F
        BNE     GETFMT
ERR
        LDY     #$80
        LDA     #0
GETFMT
        TAX
        LDA     MODE2,X
        STA     FORMAT
        AND     #3
        STA     LENGT
        TYA
        AND     #$8F
        TAX
        TYA
        LDY     #3
        CPX     #$8A
        BEQ     MNNDX3
MNNDX1
        LSR     A
        BCC     MNNDX3
        LSR     A
MNNDX2
        LSR     A
        ORA     #$20
        DEY
        BNE     MNNDX2
        INY
MNNDX3
        DEY
        BNE     MNNDX1
        PHA
PROP
        LDA     #$B9            ;LOAD ABS,Y
        STA     MOVPNT          ;MODE
        JSR     MOVPNT          ;GET BYTE
        JSR     OUTBYT
        LDX     #1
PROPBL
        JSR     PRBL2
        CPY     LENGT
        INY
        BCC     PROP
        LDX     #3
        CPY     #4
        BCC     PROPBL
        PLA
        TAY
        LDA     MNEML,Y
        STA     LMNEM
        LDA     MNEMR,Y
        STA     RMNEM
PRMN1
        LDA     #0
        LDY     #5
PRMN2
        ASL     RMNEM
        ROL     LMNEM
        ROL     A
        DEY
        BNE     PRMN2
        ADC     #$BF
        JSR     CHROUT          ;SEND TO CONSOLE
        DEX
        BNE     PRMN1
        JSR     PRBL
        LDX     #6
PRADR1
        CPX     #3
        BNE     PRADR3
        LDY     LENGT
        BEQ     PRADR3
PRADR2
        LDA     FORMAT
        CMP     #$E8
        LDA     #$B9            ;LOAD ABS,Y
        STA     MOVPNT          ;MODE
        JSR     MOVPNT          ;GET BYTE
        BCS     RELADR
        JSR     OUTBYT
        DEY
        BNE     PRADR2
PRADR3
        ASL     FORMAT
        BCC     PRADR4
        LDA     CHAR1-1,X
        JSR     CHROUT
        LDA     CHAR2-1,X
        BEQ     PRADR4
        JSR     CHROUT
PRADR4
        DEX
        BNE     PRADR1
        RTS
RELADR
        JSR     PNTAJ3
        TAX
        INX
        BNE     OUTYX
        INY
OUTYX
        TYA
OUTAX
        JSR     OUTBYT
OUTX
        TXA
        JMP     OUTBYT
PRPC
        JSR     CRLF
        JSR     OUTPNT
        LDA     #'-'
        JSR     CHROUT
PRBL
        LDX     #3
PRBL2
        JSR     OUTSPC
        DEX
        BNE     PRBL2
        RTS
PNTAJ
        LDA     LENGT
PNTAJ2
        SEC
PNTAJ3
        LDY     POINTR+1
        TAX
        BPL     PNTAJ4
        DEY
PNTAJ4
        ADC     POINTR
        BCC     RTS1
        INY
RTS1
        RTS
;GATHER PARAMETERS
GETPRM
        LDA     #0              ;CLEAR
        STA     NUMPRM          ;COUNT
PRMLPE
        JSR     PSHOVE          ;PUSH PARMS
        INY                     ;FIND NEXT
        JSR     SKPSPC          ;NON-BLANK
        BEQ     ENDPRM          ;DONE IF NONE
        STA     LSTCHR          ;SAVE LAST CHAR
        INC     NUMPRM          ;ELSE BUMP COUNT
        CMP     #','            ;IF SEPARATOR
        BEQ     PRMLPE          ;TRY FOR ANOTHER
PACKIT
        JSR     PACK            ;CONVERT AND	PACK
        BCS     PRMERR          ;IF ERROR EXIT
        INY                     ;POINT TO NEXT
        LDA     CNSTXT,Y        ;GET CHAR
        BEQ     ENDPRM          ;EXIT IF NONE
        STA     LSTCHR          ;SAVE LAST CHAR
        CMP     #','            ;IF SEPARATOR
        BEQ     PRMLPE          ;LOOP FOR MORE
        CMP     #' '            ;IF NOT SPACE
        BNE     PACKIT          ;TRY TO CONVERT
        JSR     SKPSPC          ;ELSE GET NEXT
        BEQ     ENDPRM          ;DONE IF NONE
        STA     LSTCHR          ;SAVE LAST CHAR
        CMP     #','            ;IF SEPSRATOR
        BEQ     PRMLPE          ;LOOP FOR MORE
PRMERR
        JMP     ERROR           ;ELSE ERROR
ENDPRM
        LDA     LSTCHR          ;GET LAST
        CMP     #','            ;IF NOT COMMA
        BNE     NOMRPR          ;THEN DONE
        INC     NUMPRM          ;ELSE BUMP COUNT
NOMRPR
        JMP     PSHOVE          ;PUSH LAST
;SKIP BLANKS
SKPSPC
        LDA     CNSTXT,Y        ;GET CHAR
        BEQ     EXTSKP          ;DONE IF NONE
        CMP     #' '            ;IF NOT SPACE
        BNE     EXTSKP          ;THEN DONE
        INY                     ;ELSE LOOP
        BNE     SKPSPC          ;AND TRY AGAIN
EXTSKP
        RTS
;TEST FOR ILLEGAL
TSTLGL
        LDA     CNSTXT,Y        ;GET CHAR
        BEQ     EXTLGL          ;DONE IF NULL
        CMP     #' '            ;IF LESS THAN SPACE
        BCC     BADINP          ;IS AN ERROR
        BEQ     EXTLGL
        CMP     #'?'
        BEQ     EXTLGL
        CMP     #'*'
        BEQ     EXTLGL
        CMP     #'='
        BEQ     EXTLGL
        CMP     #'_'
        BEQ     EXTLGL
        CMP     #'.'
        BEQ     EXTLGL
        CMP     #':'
        BEQ     EXTLGL
        CMP     #SEMICO
        BEQ     EXTLGL
        CMP     #'<'
        BEQ     EXTLGL
        CMP     #'>'
EXTLGL
        RTS
BADINP
        JMP     ERROR
;PUSH PARMS DOWN
PSHOVE
        LDX     #16             ;SHIFT 16 BITS
SHVLPE
        ASL     PARM3           ;FROM
        ROL     PARM3+1         ;PARM3
        ROL     PARM2           ;TO PARM2
        ROL     PARM2+1         ;AND FROM
        ROL     PARM1           ;PARM2 TO
        ROL     PARM1+1         ;PARM1
        DEX                     ;LOOP UNTIL
        BNE     SHVLPE          ;ALL DONE
        LDA     NUMBER          ;GET NUMBER
        STA     PARM3           ;AND SET PARM3
        LDA     NUMBER+1        ;LOW AND
        STA     PARM3+1         ;HIGH
        STX     NUMBER          ;CLEAR
        STX     NUMBER+1        ;NUMBER
        RTS
;GET CHARACTER
GETCHR
        LDX     KIMPNT          ;GET POINTER
        BPL     NOREAD          ;IF OK USE
        JSR     RDEDFL          ;ELSE READ
        BEQ     GETOK           ;IF OK CONTINUE
        CMP     #1              ;IF EOF
        BEQ     *+5             ;IS OK
        JMP     ERROR           ;ELSE ERROR
        LDA     #EOF            ;GET AN EOF
        STA     DFLBUF          ;PUT IN BUFFER
        LDA     #0              ;CLEAR A TO
GETOK
        TAX                     ;CLEAR INDEX
        STX     KIMPNT          ;AND POINTER
NOREAD
        INC     KIMPNT          ;BUMP POINTER
        LDA     DFLBUF,X        ;GET CHAR
        AND     #%01111111      ;MASK OUT PARITY
        RTS
;GET BYTE
GETTWO
        LDA     #0              ;CLEAR
        STA     NUMBER          ;BUFFER
        STA     NUMBER+1        ;SO NO ERROR
        JSR     GETONE          ;GET NIBBLE
GETONE
        JSR     GETCHR          ;GET CHAR
        JSR     PACK            ;PACK AND CONVERT
        BCS     GETERR          ;ERROR IF CARRY
        LDA     NUMBER          ;ELSE GET RESULT
        RTS
GETERR
        JMP     ERROR           ;ERROR EXIT
;PACK ASCII
PACK
        CMP     #'0'            ;IF TOO SMALL
        BMI     PACKER          ;THEN ERROR
        CMP     #'9'+1          ;IF "0" TO "9"
        BMI     DECNUM          ;IS DECIMAL
        CMP     #'A'            ;IF LESS THAN "A"
        BMI     PACKER          ;THEN ERROR
        CMP     #'F'+1          ;IF MORE THAN "F"
        BPL     PACKER          ;THEN ERROR
        CLC                     ;ELSE ADD
        ADC     #9              ;NINE
DECNUM
        ROL     A               ;MOVE
        ROL     A               ;TO
        ROL     A               ;HIGH
        ROL     A               ;NIBBLE
        LDX     #4              ;SHIFT
PACKLP
        ROL     A               ;INPUT
        ROL     NUMBER          ;AND
        ROL     NUMBER+1        ;BUFFER
        BCS     PACKER          ;ERROR IF CARRY
        DEX                     ;COUNT DOWN
        BNE     PACKLP          ;AND LOOP
        RTS
PACKER
        SEC                     ;ERROR
        RTS                     ;RETURN
;OUTPUT NIBBLE
OUTNIB
        AND     #%00001111      ;LOW NIBBLE
        ORA     #'0'            ;MAKE ASCII
        CMP     #'9'+1          ;IF "0" TO "9"
        BMI     NOTALP          ;SEND IT
        CLC                     ;ELSE
        ADC     #7              ;ADD SEVEN
NOTALP
        JMP     CHROUT          ;WRITE IT
;OUTPUT BYTE
OUTBYT
        PHA                     ;SAVE
        LSR     A               ;MOVE
        LSR     A               ;HIGH
        LSR     A               ;TO
        LSR     A               ;LOW
        JSR     OUTNIB          ;SEND
        PLA                     ;THEN SEND
        JMP     OUTNIB          ;HIGH
;OUTPUT POINTER
OUTPNT
        LDA     POINTR+1        ;GET HIGH
        JSR     OUTBYT          ;SEND
        LDA     POINTR          ;THEN LOW
        JMP     OUTBYT          ;SEND IT
;PARAMETER 1 TO POINTER
PR1PTR
        LDA     PARM1           ;GET LOW
        LDY     PARM1+1         ;AND HIGH
        STA     POINTR          ;SET
        STY     POINTR+1        ;POINTER
        RTS
;PARAMETER 2 TO POINTER
PR2PTR
        LDA     PARM2           ;GET LOW
        LDY     PARM2+1         ;AND HIGH
        STA     POINTR          ;SET
        STY     POINTR+1        ;POINTER
        RTS
;PARAMETER 3 TO POINTER
PR3PTR
        LDA     PARM3           ;GET LOW
        LDY     PARM3+1         ;AND HIGH
        STA     POINTR          ;SET
        STY     POINTR+1        ;POINTER
        RTS
;PARAMETER 3 TO PARAMETER 2
PR3PR2
        LDA     PARM3           ;GET LOW
        LDY     PARM3+1         ;AND HIGH
        STA     PARM2           ;SET
        STY     PARM2+1         ;PARM 2
        RTS
;LAST ADDRESS CHECK
LSTCHK
        LDA     PARM2           ;SUBTRACT
        CMP     POINTR          ;POINTER
        LDA     PARM2+1         ;FROM
        SBC     POINTR+1        ;PARM2
        RTS
;INCREMENT POINTER
INCPNT
        INC     POINTR          ;BUMP LOW
        BNE     *+5             ;DONE IF NOT ZERO
        INC     POINTR+1        ;ELSE BUMP HIGH
        RTS
;DECREMENT POINTER
DECPNT
        LDA     POINTR          ;GET LOW
        BNE     *+5             ;JUMP IF NOT ZERO
        DEC     POINTR+1        ;ELSE DROP HIGH
        DEC     POINTR          ;ALWAYS DROP LOW
        RTS
;INCREMENT PARAMETER 2
INCPR2
        INC     PARM2           ;BUMP LOW
        BNE     *+5             ;DONE IF NOT ZERO
        INC     PARM2+1         ;ELSE BUMP HIGH
        RTS
;ERROR HANDLER
ERROR
        JSR     CRLF            ;SEND CR LF
        LDA     #'?'            ;THEN A
        JSR     CHROUT          ;QUESTION
        JMP     GETCOM          ;RESTART
;PARM3 TO USER PC
PRMUPC
        LDA     PARM3           ;GET LOW
        LDY     PARM3+1         ;AND HIGH
        STA     USRPC           ;SET PC
        STY     USRPC+1         ;IN RAM
        RTS
;TEST FOR POINTER=0
TSTPR0
        LDA     POINTR          ;GET LOW
        ORA     POINTR+1        ;OR WITH HIGH
        RTS
;CR AND LF
CRLF
        LDA     #CR             ;DO
        JSR     CHROUT          ;A CR
        LDA     #LF             ;AND
        BNE     CHROUT          ;A LF
;OUTPUT SPACE
OUTSPC
        LDA     #' '            ;GET SPACE
;OUTPUT WITH SAVE
CHROUT
        PHA                     ;SAVE CHAR
        STX     SAVEX           ;SAVE X
        STY     SAVEY           ;AND Y
        JSR     OUTCHR          ;SEND
        LDY     SAVEY           ;RESTORE
        LDX     SAVEX           ;X AND Y
        PLA                     ;THEN CHAR
        RTS
;DEFAULT FILE REFERENCES
SETBUF
        LDA     #<DFLBUF        ;POINT TO
        LDY     #>DFLBUF        ;DEFAULT BUFFER
SETDMA
        LDX     #26             ;SET DMA
        BNE     PEMGO
OPNDFL
        LDX     #15             ;OPEN FILE
        BNE     SETDFL
RDEDFL
        LDX     #20             ;READ RECORD
SETDFL
        LDA     #<DFLFCB        ;POINT TO
        LDY     #>DFLFCB        ;DEFAULT FCB
        JMP     PEMGO           ;GO
;GENERAL PEM REFERENCES
CHRINP
        LDX     #1              ;CHARACTER INPUT
        BNE     PEMGO
OUTCHR
        AND     #$7F            ;CLEAR MSB
        LDX     #2              ;DO CHAR OUTPUT
        BNE     PEMGO
RDEBUF
        LDA     BUFVEC          ;READ BUFFER
        LDY     BUFVEC+1
        LDX     #10
PEMGO
        JMP     (VECTOR)
;SET BRK OPCODE
SETBPT
        JSR     STRABS          ;STORE ABSOLUTE
        LDA     #0              ;BRK CODE
        JMP     MOVPNT          ;SET
;MOVE POINTER TO BREAKPOINT POINTER (X=0 OR 2)
PNTBPT
        LDA     POINTR          ;GET LOW
        STA     BRKPT1,X        ;SET LOW
        LDA     POINTR+1        ;GET HIGH
        STA     BRKPT1+1,X      ;THEN SET
        DEX                     ;DROP X
        BPL     *+3             ;IF 1 OK
        INX                     ;ELSE MAKE 0
        LDA     #$FF            ;GET FLAG
        STA     BRFLG1,X        ;AND SET
        RTS
;SEND BYTE THEN ASCII SPACE
BYTSPC
        JSR     OUTBYT          ;SEND BYTE
        JMP     OUTSPC          ;THEN SPACE
;LOAD ABSOLUTE MODE
LDEABS
        LDA     #$AD            ;LOAD ABSOLUTE CODE
        STA     MOVPNT          ;SET
        RTS
;STORE ABSOLUTE MODE
STRABS
        LDA     #$8D            ;STORE ABSOLUTE CODE
        STA     MOVPNT          ;SET
        RTS
;CLEAR BREAKPOINTS
CLRBPS
        JSR     STRABS          ;SET STORE ABSOLUTE
        LDX     #0              ;FIRST POINTER
        LDY     #0              ;FIRST FLAG AND OPCODE
        JSR     CLR1            ;CLEAR FIRST
        INY                     ;SECOND FLAG AND OPCODE
        LDX     #2              ;SECOND POINTER
;CLEAR ONE BREAKPOINT
CLR1
        LDA     BRFLG1,Y        ;GET FLAG
        BEQ     CLR1EX          ;DO NOTHING IN CLEAR
        LDA     BRKPT1,X        ;ELSE GET
        STA     POINTR          ;ADDRESS
        LDA     BRKPT1+1,X      ;AND SET
        STA     POINTR+1        ;POINTER
        LDA     OPCDE1,Y        ;THEN GET OPCODE
        JSR     MOVPNT          ;AND RESTORE
        LDA     #0              ;CLEAR
        STA     BRFLG1,Y        ;FLAG
CLR1EX
        RTS
;IRQ AND BRK HANDLER
IRQBRK
        PHP                     ;SAVE P
        PHA                     ;A
        TXA                     ;AND
        PHA                     ;X
        TSX                     ;GET STATUS
        LDA     $104,X          ;FROM STACK
        AND     #%00010000      ;LOOK AT BRK BIT
        BNE     ISABRK          ;BRK IF NOT ZERO
        PLA                     ;ELSE RESTORE
        TAX                     ;X
        PLA                     ;A
USRBRK
        PLP                     ;AND P
        JMP     (IRQVEC)        ;AND DO IRQ
ISABRK
        PLA                     ;RESTORE
        TAX                     ;X
        PLA                     ;A
        PLP                     ;AND P
        STA     USRA            ;NOW SAVE
        STX     USRX            ;EVERYTHING
        STY     USRY            ;IN
        PLA                     ;USER
        STA     USRP            ;SAVE
        PLA                     ;LOCATIONS
        STA     USRPC           ;INCLUDING
        PLA                     ;PC
        STA     USRPC+1         ;AND
        TSX                     ;STACK
        STX     USRS            ;BEFORE BRK
        CLD                     ;BINARY MODE
        SEC                     ;ADJUST
        LDA     USRPC           ;PC
        SBC     #2              ;TO
        STA     USRPC           ;CORRECTLY
        BCS     *+5             ;POINT TO
        DEC     USRPC+1         ;BRK
        LDA     BRFLG1          ;IF FLAG ZERO
        BEQ     TRYBP2          ;IS NOT SET
        LDA     USRPC           ;ELSE GET LOW
        LDY     USRPC+1         ;AND HIGH PC
        CMP     BRKPT1          ;IF LOW NOT
        BNE     TRYBP2          ;EQUAL TRY 2
        CPY     BRKPT1+1        ;IF HIGH
        BEQ     RSTOPC          ;EQUAL IS OK
TRYBP2
        LDA     BRFLG2          ;IF NOT SET
        BEQ     NOTSET          ;MUST BE USER
        LDA     USRPC           ;ELSE GET
        LDY     USRPC+1         ;USER PC
        CMP     BRKPT2          ;IF LOW NOT SAME
        BNE     NOTSET          ;IS USER
        CPY     BRKPT2+1        ;IF HIGH NOT SAME
        BNE     NOTSET          ;IS USER
RSTOPC
        JSR     CLRBPS          ;CLEAR BREAKPOINTS
        LDA     USRPC           ;SET
        LDY     USRPC+1         ;POINTER
        STA     POINTR          ;TO PC
        STY     POINTR+1        ;FOR OPCODE RESTORE
        JSR     INSDSP          ;SHOW INSTRUCTION
        JMP     NOSTCH          ;SHOW STATUS
;USER BRK
NOTSET
        CLC                     ;ADJUST
        LDA     USRPC           ;USER
        ADC     #2              ;PC
        STA     USRPC           ;TO ORIGINAL
        BCC     *+5             ;FOR
        INC     USRPC+1         ;BRK
        LDA     USRPC+1         ;GET HIGH
        PHA                     ;PUSH
        LDA     USRPC           ;GET LOW
        PHA                     ;PUSH
        LDA     USRP            ;GET FLAGS
        PHA                     ;PUSH
        LDX     USRX            ;GET X
        LDY     USRY            ;AND Y
        LDA     USRA            ;AND A
        JMP     USRBRK
;GET CHAR PER LINE
GTCHLN
        LDA     $FF00+61        ;DUMMY WITH OFFSET
        RTS
;GET LINES PER SCREEN - 1
GTLNSC
        LDA     $FF00+60        ;DUMMY WITH OFFSET
        SEC
        SBC     #1
        RTS
;CALCULATE BYTES PER LINE
CLBYLN
        JSR     GTCHLN          ;GET CHAR PER LINE
        CMP     #33
        BCS     GT32            ;IF >32 THEN
        LDA     #2
        LDX     #%11
        BNE     GOTMOD
GT32
        CMP     #65
        BCS     GT64            ;IF >64 THEN
        LDA     #3
        LDX     #%111
        BNE     GOTMOD
GT64
        LDA     #4
        LDX     #%1111
GOTMOD
        STA     BYTPLN          ;SAVE BOTH PARAMS
        STX     LINMSK
        RTS
;CALCULATE TOTAL BYTES-1
CLTLBY
        JSR     CLBYLN          ;BYTES PER LINE
        TAX                     ;SAVE LOG OF BYTES PER LINE
        JSR     GTLNSC
        SEC
        SBC     #1              ;DROP ANOTHER FOR SECURITY
CLTLLP
        ASL     A
        BCC     *+4             ;OK IF NO CARRY
        LDA     #0              ;ELSE IS "256"
        DEX
        BNE     CLTLLP          ;LOOP IF MORE
        SEC
        SBC     #1              ;DROP BY ONE
        RTS
;RELOCATABLE VECTORS
        .BYTE   $20
STRVEC
        .WORD   PEMVEC          ;START OF DEBUG
        .BYTE   $20
BUFVEC
        .WORD   CNSBUF          ;CONSOLE BUFFER
        .BYTE   $20
IBKVEC
        .WORD   IRQBRK          ;IRQ/BRK VECTOR
        .BYTE   $FF             ;STOPPER FOR RELOCATE
;REGISTER TABLE
REGTBL
        .BYTE   "A",0
        .BYTE   "X",0
        .BYTE   "Y",0
        .BYTE   "S",0
        .BYTE   "N",128
        .BYTE   "V",64
        .BYTE   "B",16
        .BYTE   "D",8
        .BYTE   "I",4
        .BYTE   "Z",2
        .BYTE   "C",1
;DISASSEMBLER TABLES
MODE
        .BYTE   $40,$2,$45,$3,$D0,$8,$40,$9
        .BYTE   $30,$22,$45,$33,$D0,$8,$40,$9
        .BYTE   $40,$2,$45,$33,$D0,$8,$40,$9
        .BYTE   $40,$2,$45,$B3,$D0,$8,$40,$9
        .BYTE   $0,$22,$44,$33,$D0,$8C,$44,$0
        .BYTE   $11,$22,$44,$33,$D0,$8C,$44,$9A
        .BYTE   $10,$22,$44,$33,$D0,$8,$40,$9
        .BYTE   $10,$22,$44,$33,$D0,$8,$40,$9
        .BYTE   $62,$13,$78,$A9
MODE2
        .BYTE   $0,$21,$81,$82,$0,$0,$59,$4D
        .BYTE   $91,$92,$86,$4A,$85,$9D
CHAR1
        .BYTE   $AC,$A9,$AC,$A3,$A8,$A4
CHAR2
        .BYTE   $D9,$0,$D8,$A4,$A4,$0
MNEML
        .BYTE   $1C,$8A,$1C,$23,$5D,$8B,$1B,$A1
        .BYTE   $9D,$8A,$1D,$23,$9D,$8B,$1D,$A1
        .BYTE   $0,$29,$19,$AE,$69,$A8,$19,$23
        .BYTE   $24,$53,$1B,$23,$24,$53,$19,$A1
        .BYTE   $0,$1A,$5B,$5B,$A5,$69,$24,$24
        .BYTE   $AE,$AE,$A8,$AD,$29,$0,$7C,$0
        .BYTE   $15,$9C,$6D,$9C,$A5,$69,$29,$53
        .BYTE   $84,$13,$34,$11,$A5,$69,$23,$A0
MNEMR
        .BYTE   $D8,$62,$5A,$48,$26,$62,$94,$88
        .BYTE   $54,$44,$C8,$54,$68,$44,$E8,$94
        .BYTE   $0,$B4,$8,$84,$74,$B4,$28,$6E
        .BYTE   $74,$F4,$CC,$4A,$72,$F2,$A4,$8A
        .BYTE   $0,$AA,$A2,$A2,$74,$74,$74,$72
        .BYTE   $44,$68,$B2,$32,$B2,$0,$22,$0
        .BYTE   $1A,$1A,$26,$26,$72,$72,$88,$C8
        .BYTE   $C4,$CA,$26,$48,$44,$44,$A2,$C8
;MESSAGES
COMTYP
        .BYTE   "COM"
NXTMSG
        .BYTE   CR,LF,"NEXT ADDRESS=",0
STEMSG
        .BYTE   CR,LF,"*    A  X  Y  S  NV?BDIZC",CR,LF,0
;VARIABLE STORAGE
        .ALIGN  2               ;ALIGN ON WORD BOUNDARY
IRQVEC
        .WORD   0               ;IRQ VECTOR
VECTOR
        .WORD   0               ;PEM VECTOR
CNSBUF
CNSMAX
        .BYTE   32              ;INPUT
CNSLNG
        .BYTE   0               ;BUFFER
CNSTXT
        .RES    32
PARM1
        .WORD   0               ;PARAMETERS
PARM2
        .WORD   0               ;FOR
PARM3
        .WORD   0               ;COMMANDS
COMMND
        .BYTE   0               ;COMMAND LETTER
MOVPNT
        .BYTE   $8D             ;OPCODE
POINTR
        .WORD   TEA             ;POINTER
        .BYTE   $60             ;RETURN
BUFADD
        .WORD   TEA             ;COM READ ADDRESS
KIMPNT
        .BYTE   0               ;KIM CHAR POINTER
SAVPNT
        .BYTE   0               ;SAVE POINTER
COUNT
        .BYTE   0               ;COUNTER
NUMBER
        .WORD   0               ;PACK BUFFER
USRPC
        .WORD   TEA             ;USER REGISTERS
USRA
        .BYTE   0
USRX
        .BYTE   0
USRY
        .BYTE   0
USRS
        .BYTE   $FF
USRP
        .BYTE   %00000100
BRKPT1
        .WORD   0               ;BREAKPOINT 1
BRKPT2
        .WORD   0               ;BREAKPOINT 2
OPCDE1
        .BYTE   0               ;OPCODE 1
OPCDE2
        .BYTE   0               ;OPCODE 2
BRFLG1
        .BYTE   0               ;BREAK SET
BRFLG2
        .BYTE   0               ;FLAGS
NUMPRM
        .BYTE   0               ;NUMBER PARMS
SAVEX
        .BYTE   0               ;SAVE
SAVEY
        .BYTE   0               ;REGISTERS
LSTCHR
        .BYTE   0               ;LAST PARM CHAR
BRKOK
        .BYTE   0               ;0 IF BRK ALLOWED
XRGCMD
        .BYTE   0               ;STATE COMMAND
LENGT
        .BYTE   0               ;INSTRUCTION LENGTH
FORMAT
        .BYTE   0               ;FORMAT CODE
LMNEM
        .BYTE   0               ;LEFT NAME
RMNEM
        .BYTE   0               ;RIGHT NAME
LINMSK
        .BYTE   0               ;LINE END MASK FOR DISPLAY
BYTPLN
        .BYTE   0               ;BYTES PER LINE FOR DISPLAY
;ALIGN END ON PAGE BOUNDARY-1
LAST            = */256*256+256-1
        .END
