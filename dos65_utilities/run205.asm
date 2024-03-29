;RUN
;BASIC-E/65 RUN TIME INTERPRETER
;VERSION 2.05-A
;COPYRIGHT - RICHARD A. LEARY - 1982
;RELEASED:	30 OCTOBER 1982
;LAST REVISION:
;	3 FEBRUARY 1983
;		CORRECTED SLE ERROR
;	5 MARCH 1983
;		CORRECTED CONRDE ERROR
;	17 NOVEMBER 1983
;		CORRECTED ERROR NUMBER IN EX92
;		CORRECTED LINE COUNT IN OUTMSG
;	28 JULY 1985
;		CORRECTED OR ERROR
;		MADE PAGE ZERO 6510 COMPATIBLE
;	2 APRIL 2008
;		REFORMATTED FOR TASM AND ASM211
;		MOVED MULTIPLE VARIABLES OUT OF PAGE ZERO
;EXTERNAL REFERENCES
BOOT            = $100          ;BOOT ENTRY
PEM             = $103          ;PEM ENTRY
DFLFCB          = $107          ;DEFAULT FCB
BUF             = $128          ;DEFAULT BUFFER
TEA             = $800          ;LOAD ADDRESS
;FIXED PARAMETERS AND CONSTANTS
;CAUTION: WHEN A REGISTER IS SET TO TRUE, Z IS SET TO 0 (I.E. NE).  IF A
;REGISTER IS SET TO FALSE THEN Z IS SET TO 1 (I.E. EQ).
TRUE            = $FF
FALSE           = 0
;ASCII CHARACTERS
TAB             = 9             ;TAB
LF              = 10            ;LINEFEED
CR              = 13            ;RETURN AND EOL
EOF             = 26            ;EOF CHAR
SEMICO          = 59            ;SEMICOLON
;OTHER PARAMETERS
NUMFIL          = 20            ;MAX NUMBER USER FILES
PRBFLN          = 132           ;PRINT BUFFER LENGTH
NRSTCK          = 24*4          ;STACK SIZE * 4
MAXPSN          = 8             ;MAX NUMBER TAB POSITIONS (132 COL PAPER)
;BASIC-E/65 OPCODES
DAT             = 51
ILS             = 28
DEF             = 94
BRS             = 54
BRC             = 55
PRO             = 30
CON             = 46
;PAGE ZERO VARIABLES
;ALL NON-ZERO INITIAL VALUES ARE SHOWN AS I=XXXX WHERE XXXX IS THE INITIAL
;VALUE. PL/M NAME IS SHOWN IF SIGNIFICANTLY DIFFERENT.
FCB             = $61           ;ADDRESS OF FCB
TIME4T          = $63           ;TEMP IN TIMES4
LCLSED          = $64           ;LOCAL SEED FOR RANDOM NUMBER
BASE            = $66           ;BASE ADDRESS IN BUILD
AP              = $68           ;ACCUM INDEX
TBASE           = $69           ;TEMP IN BUILD
LZ              = $6B           ;
BUFFER          = $6D           ;ADDRESS OF DISKBUFFER
EXH             = $82           ;HOLD IN EX81 AND EX82
WTCHLD          = $84           ;HOLD IN WRTTCN [WRITE$TO$CONSOLE]
RCRPNT          = $86           ;RECORD$POINTER
DOBF            = $88           ;BUFFER IN DISK$OPEN
WTFPNT          = $8A           ;POINT IN WRTOFL (WRITE$TO$FILE)
DCBPNT          = $8C           ;DCB POINTER
CONBPT          = $8E           ;CONBUFFPTR
GTSTMP          = $90           ;TEMP IN GET$STRING$FIELD
NWSTAD          = $92           ;NEWSTRING ADDRESS
PTRADR          = $94           ;PTRADDR IN STORE
SPTR            = $96           ;PTR IN STORE
FRSTST          = $98           ;FIRSTSTRING
SCNDST          = $9A           ;SECONDSTRING
SSTMP           = $9C           ;TEMPA IN STRSEG [STRING$SEGMENT]
ARYADR          = $9E           ;ARRAY ADDRESS
SPACE           = $A0           ;
DSPNT           = $A2           ;
DSTMP           = $A4           ;
DSTMP1          = $A6           ;
DSTMP2          = $A8           ;
DSTMP3          = $AA           ;TEMP IN UNLINK
MFROM           = $AC           ;FROM IN MOVE
MTO             = $AE           ;TO IN MOVE
SX              = $B0           ;SECONDARY
PX              = $B2           ;PRIMARY
FN              = $B6           ;N FOR FILL
TRIGS           = $B8           ;TRIG FUNCTION SIGN
LSB             = $B9           ;SAVE LSB IN INT
SRPNT           = $BA           ;SERIES EVALUATION POINTER
SRCNT           = $BC           ;SERIES EVALUATION COUNTER
TY              = $BD           ;TEMP IN FLTOUT
DVZERO          = $BE           ;DIVIDE BY ZERO FLAG
OVERFL          = $BF           ;OVERFLOW FLAG
ME              = $C0           ;MEMORY POINTER
SL              = $C2           ;STORE MEMORY POINTER
CNVIND          = $C4           ;NUMBER CONVERT BUFFER INDEX
SAVPR           = $C5           ;SAVE FOR PR IN ADPRSC
NE              = $C6           ;EXPONENT
DPFLG           = $C7           ;DECIMAL POINT FLAG
DPOFF           = $C8           ;DP OFFSET
NESFLG          = $C9           ;EXPONENT SIGN FLAG
NS              = $CA           ;SIGN
PE              = $CB           ;EXPONENT
PM              = $CC           ;MANTISSA
PS              = $CF           ;SIGN
PC              = $D0           ;CARRY
PR              = $D1           ;ROUNDING
SE              = $D2           ;EXPONENT
SM              = $D3           ;MANTISSA
SS              = $D6           ;SIGN
QE              = $D7           ;EXPONENT
QM              = $D8           ;MANTISSA
SGNCMP          = $DB           ;PRIMARY . SECONDARY SIGN COMPARISON
RA              = $DC           ;ADDRESS OF REGISTER A
RB              = $DE           ;ADDRESS OF REGISTER B
RC              = $E0           ;ADDRESS OF REGISTER C
SB              = $E2           ;STACK BOTTOM
ST              = $E4           ;STACK TOP
MPR             = $E6           ;BASE ADDRESS OF PRT
MDA             = $E8           ;BASE ADDRESS OF DATA AREA
MCD             = $EA           ;BASE OF CODE AREA
CURLIN          = $EC           ;CURRENT SOURCE LINE
DTARPT          = $EE           ;DATAAREAPTR, CURRENT LOCATION IN DATA AREA
MBASE           = $F0           ;BEGINNING OF FREE STORAGE AREA
IBASE           = $F2           ;BASEIN INITMM [INITMEM]
PRBUFF          = $F4           ;PRINTBUFFER (INDEX TO BUFFER)
FILADR          = $F5           ;FILEADDR, CURRENT FCB POINTER BASE
EOFADR          = $F7           ;EOFADDR
BUFEND          = $F9           ;BUFFER$END
INFSAT          = $FB           ;TEMP IN INFSA
SEDPTR          = $FC           ;POINTER (PTR) IN SEDAAD
SEDAAT          = $FE           ;TEMP IN SET$DATA$ADDR (SEDAAD)

;ENTRY POINT
        .FEATURE labels_without_colons
        .SEGMENT "TEA"
        .ORG    $0800
        JMP     BUILD
;SPECIAL I/O ROUTINES
        JMP     SETLST
        JMP     CLRLST
        JMP     DOPEM
        JMP     DOSIM
;SPECIAL I/O VARIABLES
AREG
        .BYTE   0               ;A
YREG
        .BYTE   0               ;Y
XREG
        .BYTE   0               ;X
LSTFLG
        .BYTE   FALSE           ;LIST FLAG
;COPYRIGHT NOTICE
        .BYTE   "COPYRIGHT - RICHARD"
        .BYTE   " A. LEARY - 2008"
;GENERAL STORAGE AREA - VARIABLES THAT WERE IN PAGE ZERO - SECTION
;NAME IS SAME AS THAT USED FOR REMAINING PAGE ZERO VARIABLES
;FILE PROCESSING VARIABLES
EOFRA
        .WORD   0               ;EOF LOCATION FOR RA
EOFRB
        .WORD   0               ;SAME FOR RB
BLKSZE
        .WORD   0               ;BLOCKSIZE
BYTSWR
        .WORD   0               ;BYTES WRITTEN
FRSFLD
        .BYTE   0               ;FIRSTFIELD
EXTSZE
        .BYTE   0               ;EXTENT SIZE FLAG (0=16K, 128=8K)
DFLDRV
        .BYTE   0               ;DEFAULT DRIVE
;GENERALIZED INPUT VARIABLES
RERDAD
        .WORD   0               ;REREADADDR
INPTYP
        .BYTE   0               ;INPUTTYPE
INPIND
        .BYTE   0               ;INPUTINDEX
INPPTR
        .WORD   0               ;INPUTPTR
;STRING PROCESSING VARIABLES
FRSTLN
        .BYTE   0               ;FIRSTSTRING LENGTH
SCSTLN
        .BYTE   0               ;SECONDSTRING LENGTH
NWSTLN
        .BYTE   0               ;NEWSTRING LENGTH
;ARRAY ADDRESSING VARIABLES
ASIZE
        .WORD   0               ;ARRAY SIZE IN CLCROW
SAVERA
        .WORD   0               ;SAVE FOR RA
SAVERB
        .WORD   0               ;SAVE FOR RB
CI
        .BYTE   0               ;I IN CLCROW
CLCT
        .BYTE   0               ;TEMP IN CLCROW
LOCATN
        .WORD   0               ;LOCATION IN CLCSUB
NUMDIM
        .BYTE   0               ;NUMBER DIMENSIONS
;DYNAMIC STORAGE ALLOCATION VARIABLES
NBYTES
        .WORD   0
HOLD
        .WORD   0
TOTAL
        .WORD   0
AVLLT2
        .WORD   0               ;NBYTES+5 IN AVLL
AVLLT
        .WORD   0               ;TEMP IN AVLL
;FOR MOVE
MCOUNT
        .WORD   0               ;COUNT IN MOVE
;DATA FOR BUILD
CURCHR
        .BYTE   0               ;CURRENT CHAR FROM .INT FILE
BUFF
        .BYTE   0               ;INDEX FOR .INT FILE
OFFSET
        .WORD   0               ;OFFSET IN BUILD
;CONVERT LOWERCASE TO UPPER
LWRUPR
        CMP     #'A'            ;SEE IF UNDER A
        BCC     NOTLWR          ;SKIP IF IS
        CMP     #'Z'+1          ;SEE IF OVER Z
        BCS     NOTLWR          ;SKIP IF IS
        AND     #$5F            ;ELSE CONVERT
NOTLWR
        RTS
;SET LIST FLAG
SETLST
        LDA     #TRUE
        STA     LSTFLG
        RTS
;CLEAR LIST FLAG
CLRLST
        LSR     LSTFLG
        RTS
;DO DIRECT PEM CALL
DOPEM
        LDA     AREG
        LDY     YREG
        LDX     XREG
        JSR     PEM
        STA     AREG
        STY     YREG
        STX     XREG
        RTS
;DO DIRECT SIM CALL
DOSIM
        LDA     BOOT+2
        STA     SIMJSR+2
        LDA     XREG
        STA     SIMJSR+1
        LDA     AREG
        LDY     YREG
SIMJSR
        JSR     $FFFF
        STA     AREG
        STY     YREG
        STX     XREG
        RTS
;INTERPRETER INITIALIZATION ROUTINES
;GET$PARAMETERS
GETPAR
        LDA     MCD
        LDY     MCD+1
        STA     RC
        STY     RC+1            ;RC:=MCD
        LDY     MDA+1
        LDX     MDA
        BNE     *+3
        DEY
        DEX
        STX     DTARPT
        STY     DTARPT+1        ;DATAAREAPTR:=MDA-1
        CLC
        LDA     SB
        ADC     #NRSTCK
        STA     ST
        STA     MBASE
        LDY     SB+1
        BCC     *+3
        INY
        STY     ST+1
        STY     MBASE+1         ;MBASE,ST:=SB+NRSTACK
        LDA     SB
        LDY     SB+1
        STA     RB
        STY     RB+1            ;RB:=SB
        CLC
        ADC     #4
        STA     RA
        BCC     *+3
        INY
        STY     RA+1            ;RA:=SB+4
        RTS
;INITMEM
INITMM
        LDA     #<MEMORY
        LDY     #>MEMORY
        STA     MTO
        STY     MTO+1           ;TO:=.MEMORY
        CLC
        ADC     OFFSET
        STA     MFROM
        TYA
        ADC     OFFSET+1
        STA     MFROM+1         ;FROM:=.MEMORY+OFFSET
        SEC
        LDA     MPR
        SBC     #<MEMORY
        TAX
        LDA     MPR+1
        SBC     #>MEMORY
        TAY
        TXA
        JSR     MOVE            ;MOVE(BEGIN+OFFSET,BEGIN,MPR-BEGIN)
        SEC
        LDA     MBASE
        SBC     MPR
        STA     FN
        LDA     MBASE+1
        SBC     MPR+1
        STA     FN+1
        LDX     #0
        LDA     MPR
        LDY     MPR+1
        JSR     FILL            ;FILL(MPR,0,MBASE-MPR)
        LDA     ST
        LDY     ST+1
        STA     IBASE
        STY     IBASE+1
        LDY     #0
        SEC
        LDA     PEM+1
        SBC     #4
        STA     (IBASE),Y
        INY
        LDA     PEM+2
        SBC     #0
        STA     (IBASE),Y       ;A(0):=TOP-4
        LDA     #0
        LDY     #1*2            ;WORD ADDRESSING
L051
        STA     (IBASE),Y
        INY
        CPY     #3*2
        BNE     L051            ;A(1),A(2):=0
        LDY     #0*2
        LDA     (IBASE),Y
        PHA
        INY
        LDA     (IBASE),Y
        TAY
        PLA
        STA     IBASE
        STY     IBASE+1         ;BASE:=A(0)
        LDY     #0
        TYA
        STA     (IBASE),Y
        INY
        STA     (IBASE),Y       ;A(0):=0
        INY
        LDA     ST
        STA     (IBASE),Y
        INY
        LDA     ST+1
        STA     (IBASE),Y       ;A(1):=ST
        RTS
;INITIALIZE$EXECUTE
INTLEX
        JSR     GETPAR          ;GET$PARAMETERS
        JSR     INITMM          ;INITMEM
        LDX     #NUMFIL*4
        LDA     #0
L052
        STA     FILES-1,X
        DEX
        BNE     L052            ;FILL(.FILES,0,TIMES4(NUMFILES))
        JMP     CLRPBF          ;CLEAR$PRINT$BUFF
;GENERALIZED INPUT ROUTINES
;CONSOLE$READ
CONRDE
        LDA     #'?'
        JSR     PRNCHR          ;PRINTCHAR(?)
        LDA     #' '
        JSR     PRNCHR          ;PRINTCHAR(BLANK)
        LDA     #<INPBUF
        LDY     #>INPBUF
        JSR     READ            ;READ(.INPUTBUFFER)
        JSR     DMPPBF          ;OUTPUT CR/LF AND CLEAR POINTER
        LDA     INPSPC+1
        CMP     #EOF            ;IF SPACE(1) <> EOF
        BNE     *+5             ;THEN
        JSR     EXTINT          ;ELSE EXIT$INTERP
        LDA     #<INPSPC
        LDY     #>INPSPC
        STA     CONBPT
        STY     CONBPT+1        ;CONBUFFPTR:=.SPACE
        LDX     INPSPC
        INX
        LDA     #CR
        STA     INPSPC,X        ;SPACE(SPACE(0)+1):=EOLCHAR
        RTS
;MORE$CON$INPUT
MRECIN
        SEC
        LDA     CONBPT
        SBC     #<INPSPC
        CMP     INPSPC
        BCC     *+5             ;IF CONBUFFPTR<.SPACE(SPACE(0))
        LDA     #FALSE
        RTS
        LDA     #TRUE
        RTS
;CONSOLE$INPUT$ERROR
CONIER
        JSR     POPSTK          ;POP$STACK
        LDA     RERDAD
        LDY     RERDAD+1
        STA     RC
        STY     RC+1            ;RC:=REREADADDR
        LDA     #7              ;ILLEGAL INPUT WARNING
        JSR     WARNNG          ;WARNING(II)
        JMP     ERREXT          ;ERROR$EXIT
;GET$DATA$CHAR
GTDTCH
        INC     DTARPT
        BNE     *+4
        INC     DTARPT+1        ;DATAAREAPTR:=DATAAREAPTR+1
        LDA     DTARPT
        CMP     SB
        LDA     DTARPT+1
        SBC     SB+1
        BCC     *+7             ;IF DATAAREAPTR<SB THEN OK
        LDA     #14             ;ELSE OUT OF DATA ERROR
        JMP     ERROR
        LDY     #0
        LDA     (DTARPT),Y      ;A:=CHAR
        RTS
;GET$CON$CHAR
GTCNCH
        INC     CONBPT
        BNE     *+4
        INC     CONBPT+1        ;CONBUFFPTR:=CONBUFFPTR+1
        LDY     #0
        LDA     (CONBPT),Y      ;A:=CHAR
        RTS
;NEXT$INPUT$CHAR
NXINCH
        LDA     INPTYP
        BNE     L008
L009
        LDA     #80
        CMP     INPIND
        BCS     *+7             ;IF INPUTINDEX<=CONBUFFSIZE THEN
        LDA     #25             ;ELSE DATA FIELD TOO LONG
        JMP     ERROR
        JSR     GTDSCH          ;GETDISKCHAR
        LDX     INPIND
        STA     INPSPC,X        ;SPACE(INPUTINDEX):=
        CMP     #LF
        BNE     L010            ;IF NOT LF THEN
        JSR     VARBSZ
        BEQ     L009            ;IF NOT VAR$BLOCKSIZE THEN OK
        LDA     #17             ;ELSE ATTEMPT TO READ PAST END ERROR
        JMP     ERROR
L010
        LDY     #0
        LDA     (RCRPNT),Y      ;RETURN NEXTDISKCHAR
        RTS
L008
        CMP     #1              ;IF INPUTTYPE
        BNE     *+5             ;<> 1 THEN
        JMP     GTCNCH          ;ELSE RETURN GETCONCHAR
        CMP     #2              ;IF INPUTTYPE
        BNE     *+5             ;<> 2 THEN
        JMP     GTDTCH          ;ELSE RETURN GETDATACHAR
        RTS
;COUNT$INPUT
;DETERMINES EXTENT OF NEXT FIELD AND COLLECT THE FIELD IN THE APPROPRIATE BUFF
IHOLD
        .RES    1
DELIM
        .RES    1
COUNTI
        LDA     #0
        STA     INPIND          ;INPUT$INDEX:=0
L011
        JSR     NXINCH          ;DO WHILE(HOLD:=NEXT$INPUT$CHAR)=' '
        STA     IHOLD
        CMP     #' '
        BEQ     L011            ;ELIMNATES BLANKS
        LDA     INPTYP          ;IF INPUTTYPE
        BNE     L012            ;<> 0 THEN
        LDA     #<INPSPC        ;ELSE
        LDY     #>INPSPC
        STA     INPPTR
        STY     INPPTR+1        ;INPUTPTR:=.SPACE
        JMP     L014
L012
        CMP     #1              ;IF INPUTTYPE
        BNE     L013            ;<> 1 THEN
        LDA     CONBPT
        LDY     CONBPT+1
        STA     INPPTR
        STY     INPPTR+1        ;INPUTPTR:=CONBUFFPTR
        JMP     L014
L013
        CMP     #2
        BNE     L014            ;IF INPUTTYPE<>2 THEN
        LDA     DTARPT
        LDY     DTARPT+1
        STA     INPPTR
        STY     INPPTR+1
L014
        LDA     IHOLD
        CMP     #'"'
        BEQ     L015            ;IF HOLD=" THEN
        LDA     #','
        STA     DELIM           ;ELSE DELIM:=,
        JMP     L017
L015
        LDA     #'"'
        STA     DELIM           ;DELIM:="
        LDA     INPTYP
        BEQ     L016            ;IF INPUTTYPE=0 THEN
        INC     INPPTR
        BNE     L016
        INC     INPPTR+1        ;ELSE INPUTPTR:=INPUTPTR+1
L016
        JSR     NXINCH
        STA     IHOLD           ;HOLD:=NEXT$INPUT$CHAR
L017
        LDA     IHOLD           ;DO WHILE HOLD
        CMP     DELIM           ;<> DELIM
        BEQ     L018            ;AND
        CMP     #CR             ;<> EOLCHAR
        BEQ     L018
        INC     INPIND          ;INPUTINDEX:=INPUTINDEX+1
        JSR     NXINCH
        STA     IHOLD           ;HOLD:=NEXT$INPUT$CHAR
        JMP     L017
L018
        LDA     DELIM
        CMP     #'"'            ;IF DELIM<>"
        BNE     L020            ;THEN
L019
        JSR     NXINCH
        STA     IHOLD           ;HOLD:=NEXT$INPUT$CHAR
        CMP     #','            ;DO WHILE HOLD
        BEQ     L020            ;<> ,
        CMP     #CR
        BNE     L019            ;<> EOLCHAR
L020
        JMP     PSHSTK          ;PUSH$STACK
;GET$STRING$FIELD
GTSTFL
        JSR     COUNTI          ;COUNT$INPUT
        CLC
        LDA     INPIND
        LDY     #0
        ADC     #1
        JSR     GETS
        STA     GTSTMP
        STY     GTSTMP+1        ;TEMP:=GETSPACE( )
        CLC
        ADC     #1
        BNE     *+3
        INY
        STA     MTO
        STY     MTO+1           ;TO:=TEMP+1
        LDA     INPPTR
        LDY     INPPTR+1
        STA     MFROM
        STY     MFROM+1         ;FROM:=INPUTPTR
        LDA     INPIND
        LDY     #0
        JSR     MOVE            ;( , ,INPUTINDEX)
        LDY     #0
        LDA     GTSTMP
        STA     (RA),Y
        INY
        LDA     GTSTMP+1
        STA     (RA),Y          ;ARA:=TEMP
        LDA     #0
        JSR     FLSTRA          ;FLAG$STRING$ADDR(0)
        LDY     #0
        LDA     INPIND
        STA     (GTSTMP),Y      ;LNG:=INPUTINDEX
        RTS
;GET$NUMERIC$FIELD
GTNMFL
        JSR     COUNTI          ;COUNT$INPUT
        LDX     INPIND
        BEQ     L021            ;IF INPUTINDEX=0 THEN
        LDA     INPPTR
        LDY     INPPTR+1
        JSR     FLTINP          ;ELSE FP$INPUT(INPUTINDEX,INPUTPTR)
        LDA     RA
        LDY     RA+1
        JSR     STP             ;STORE ACCUM @ RA
        JMP     CHKOVR          ;CHECK$OVERFLOW
L021
        LDA     INPTYP
        CMP     #1
        BNE     *+5             ;IF INPUTTYPE<>1 THEN
        JMP     CONIER          ;ELSE CONSOLE$INPUT$ERROR
        LDY     #0
        TYA
        STA     (RA),Y          ;BRAZ:=0
        RTS
;FILE PROCESSING ROUTINES
;INITIALIZE$DISK$BUFFER
INTDBF
        LDY     #127
        LDA     #EOF
L030
        STA     (BUFFER),Y      ;INSERT EOF
        DEY
        BPL     L030
        RTS
;BUFFER$STATUS$BYTE
BUFSTB
        LDY     #33
        LDA     (FILADR),Y
        RTS                     ;RETURN FCB(33)
;SET$BUFFER$STATUS$BYTE(STATUS)
STBFSB
        LDY     #33
        STA     (FILADR),Y      ;FCB(33)=STATUS
        RTS
;WRITE$MARK
WRTMRK
        JSR     BUFSTB
        AND     #1              ;SET FLAGS
        RTS
;SET$WRITE$MARK
SWRMRK
        JSR     BUFSTB
        ORA     #1
        JMP     STBFSB
;CLEAR$WRITE$MARK
CWRMRK
        JSR     BUFSTB
        AND     #%11111110
        JMP     STBFSB
;ACTIVE$BUFFER
ACTBUF
        JSR     BUFSTB
        LSR     A
        AND     #1
        RTS
;SET$BUFFER$INACTIVE
SETBFI
        JSR     BUFSTB
        AND     #%11111001
        JMP     STBFSB
;SET$BUFFER$ACTIVE
SETBFA
        JSR     BUFSTB
        ORA     #%00000010
        JMP     STBFSB
;SET$RANDOM$MODE
SETRAN
        JSR     BUFSTB
        ORA     #%10000000
        JMP     STBFSB
;RANDOM$MODE
RANMDE
        JSR     BUFSTB
        ROL     A
        ROL     A
        AND     #1
        RTS
;STORE$REC$PTR
STRRPT
        LDY     #18*2           ;WORD ADDRESSING
        LDA     RCRPNT
        STA     (FILADR),Y
        INY
        LDA     RCRPNT+1
        STA     (FILADR),Y      ;FCBADD(18):=RECORDPOINTER
        RTS
;DISK$EOF
DSKEOF
        LDA     EOFADR
        ORA     EOFADR+1        ;IF EOFADDR <> 0
        BNE     *+7             ;THEN
        LDA     #5              ;ELSE EOF ERROR
        JMP     ERROR
        LDX     EOFADR
        LDY     EOFADR+1
        INX
        STX     RC
        BNE     *+3
        INY
        STY     RC+1            ;RC:=EOFADDR+1
        LDA     EOFRA
        LDY     EOFRA+1
        STA     RA
        STY     RA+1            ;RA:=EOFRA
        LDA     EOFRB
        LDY     EOFRB+1
        STA     RB
        STY     RB+1            ;RB:=EOFRB
        LDA     RCRPNT
        CMP     BUFFER
        BNE     RPBFNE          ;IF RECORD$POINTER <> BUFFER THEN
        LDA     RCRPNT+1
        CMP     BUFFER+1
        BEQ     L031
RPBFNE
        JSR     BUFSTB
        ORA     #%00000100      ;BUFFER$STATUS$BYTE OR 4
        JSR     STBFSB
L031
        LDA     RCRPNT
        BNE     *+4
        DEC     RCRPNT+1
        DEC     RCRPNT          ;RECORD$POINTER:=RECORD$POINTER-1
        JSR     STRRPT          ;STORE$REC$PTR
        JMP     EOFEXT          ;DROP TO OUTER LOOP
;FILL$FILE$BUFFER
FILFBF
        JSR     DSKRDE          ;IF DISKREAD <> 0
        BNE     *+5             ;THEN
        JMP     SETBFA          ;ELSE SET$BUFFER$ACTIVE
        JSR     RANMDE          ;IF RANDOM$MODE
        BNE     *+5             ;THEN
        JMP     DSKEOF          ;ELSE DISK$EOF
        JSR     INTDBF          ;INITIALIZE$DISK$BUFFER
        JSR     SETBFA          ;SET$BUFFER$ACTIVE
        LDY     #32
        LDA     (FILADR),Y
        CLC
        ADC     #1
        STA     (FILADR),Y      ;FCB(32):=FCB(32)+1
        RTS
;WRITE$DISK$IF$REQ
WDIFRQ
        JSR     WRTMRK          ;IF NOT WRITE$MARK
        BEQ     L032            ;THEN JUST SET RECORDPOINTERS
        JSR     BUFSTB          ;ELSE
        AND     #4
        BEQ     L033            ;IF NOT SHR(BUFFER$STATUS$BYTE,2) THEN
        LDY     #32
        LDA     (FILADR),Y
        BEQ     L034            ;IF ZERO DON'T SUBTRACT
        SEC
        SBC     #1
        STA     (FILADR),Y      ;ELSE FCB(32):=FCB(32)-1
L034
        JSR     BUFSTB
        AND     #%11111011
        JSR     STBFSB
L033
        JSR     DSKWRT
        BEQ     *+7             ;IF OK THEN
        LDA     #3              ;ELSE DISK WRITE ERROR
        JMP     ERROR
        JSR     CWRMRK          ;CLEAR$WRITE$MARK
        JSR     RANMDE
        BEQ     *+8             ;IF NOT$RANDOM$MODE THEN
        JSR     SETBFI          ;SET$BUFFER$INACTIVE
        JMP     L032
        JSR     INTDBF          ;INITIALIZE$DISK$BUFFER
L032
        LDA     BUFFER
        LDY     BUFFER+1
        STA     RCRPNT
        STY     RCRPNT+1        ;RECORD$POINTER:=BUFFER
        RTS
;AT$END$DISK$BUFFER
ATENDB
        INC     RCRPNT
        BNE     *+4
        INC     RCRPNT+1        ;RECORD$POINTER:=RECORD$POINTER+1
        LDA     RCRPNT
        CMP     BUFEND
        LDA     RCRPNT+1
        SBC     BUFEND+1
        BCS     *+5             ;IF RECORD$POINTER >= BUFFER$END THEN
        LDA     #FALSE          ;ELSE
        RTS
        LDA     #TRUE
        RTS
;VAR$BLOCK$SIZE
VARBSZ
        LDA     BLKSZE
        ORA     BLKSZE+1
        BNE     *+5             ;IF BLOCKSIZE <> 0 THEN
        LDA     #FALSE          ;ELSE
        RTS
        LDA     #TRUE
        RTS
;WRITE$A$BYTE(CHAR)
WRABYT
        PHA                     ;SAVE CHAR
        JSR     VARBSZ          ;IF NOT VAR$BLOCK$SIZE
        BEQ     L035            ;THEN OK
        INC     BYTSWR
        BNE     *+5
        INC     BYTSWR+1        ;BYTESWRITTEN:=BYTESWRITTEN+1
        LDA     BLKSZE
        CMP     BYTSWR
        LDA     BLKSZE+1
        SBC     BYTSWR+1        ;IF BLOCKSIZE >= BYTESWRITTEN
        BCS     L035            ;THEN OK
        LDA     #6              ;ELSE RECORD SIZE ERROR
        JMP     ERROR
L035
        JSR     ATENDB          ;IF NOT AT$END$DISK$BUFFER
        BEQ     *+5             ;THEN
        JSR     WDIFRQ          ;ELSE WRITE$DISK$IF$REQ
        JSR     ACTBUF          ;IF ACTIVE$BUFFER
        BNE     L036            ;THEN
        JSR     RANMDE          ;OR IF NOT RANDOM$MODE
        BEQ     L036            ;THEN
        JSR     FILFBF          ;ELSE FILL$FILE$BUFFER
        LDY     #32
        LDA     (FILADR),Y
        SEC
        SBC     #1
        STA     (FILADR),Y      ;FCB(32):=FCB(32)-1
L036
        PLA
        LDY     #0
        STA     (RCRPNT),Y      ;NEXTDISKCHAR:=CHAR
        JMP     SWRMRK          ;SET$WRITE$MARK
;GET$FILE$NUMBER
GTFLNU
        LDY     #0
        LDA     (RA),Y
        CMP     #NUMFIL
        BEQ     E21             ;IF BRAZ=NUMFILES THEN OK
        BCC     E21             ;OR IF BRAZ<NUMFILES THEN OK
        LDA     #11             ;ELSE ILLEGAL FILE ERROR
        JMP     ERROR
E21
        RTS
;SET$FILE$ADDR
STFLAD
        JSR     GTFLNU
        ASL     A
        TAX
        LDA     FILES,X
        STA     FILADR
        LDA     FILES+1,X
        STA     FILADR+1        ;FILEADDR:=FILES(GET$FILE$NUMBER)
        ORA     FILADR
        BNE     *+7             ;IF <> 0 THEN OK
        LDA     #9
        JMP     ERROR           ;ELSE UNOPENED FILE ERROR
        LDY     #0
        LDA     (RA),Y
        ASL     A
        TAX
        LDA     EOFBRN,X
        STA     EOFADR
        LDA     EOFBRN+1,X
        STA     EOFADR+1        ;EOFADDR:=EOFBRANCH(BRAZ)
        RTS
;SET$FILE$POINTERS
STFLPT
        CLC
        LDA     FILADR
        LDY     FILADR+1
        ADC     #38
        STA     BUFFER
        BCC     *+3
        INY
        STY     BUFFER+1        ;BUFFER:=FILEADDR+38
        CLC
        ADC     #128
        STA     BUFEND
        BCC     *+3
        INY
        STY     BUFEND+1        ;BUFFER$END=BUFFER+DISKRECSIZE
        LDY     #17*2           ;WORD ADDRESSING
        LDA     (FILADR),Y
        STA     BLKSZE
        INY
        LDA     (FILADR),Y
        STA     BLKSZE+1        ;BLOCKSIZE:=FCBADD(17)
        INY
        LDA     (FILADR),Y
        STA     RCRPNT
        INY
        LDA     (FILADR),Y
        STA     RCRPNT+1        ;RECORDPOINTER:=FCBADD(18)
        JSR     SETDMA          ;SETDMA
        LDY     #0              ;CLEAR EXTENT SIZE FLAG
        STY     EXTSZE
        LDA     (FILADR),Y      ;GET DRIVE
        BNE     CLCDRV          ;USE IF NOT ZERO
        LDA     DFLDRV          ;ELSE GET DEFAULT
        JMP     SETDRV
CLCDRV
        SEC                     ;DROP BY ONE
        SBC     #1
SETDRV
        LDX     #14
        JSR     PEM             ;SELECT DRIVE
        LDX     #34
        JSR     PEM             ;READ DCB ADDRESS
        STA     DCBPNT
        STY     DCBPNT+1
        LDY     #6
        LDA     (DCBPNT),Y      ;GET BLOCK SIZE
        BNE     EXTSTF          ;DONE IF NOT 1K
        LDY     #1
        LDA     (DCBPNT),Y      ;GET HIGH OF MAXBLK
        BEQ     EXTSTF          ;DONE IF MAXBLK < 256
        SEC
        ROR     EXTSZE          ;ELSE SET FLAG
EXTSTF
        RTS
;SETUP$FILE$EXTENT
SUFLEX
        JSR     OPEN            ;IF OPEN OK
        BPL     E22             ;THEN DONE
        JSR     CREATE          ;THEN IF CREATE OK
        BPL     SUFLEX          ;THEN OPEN IT
        LDA     #10             ;ELSE ERROR WHILE MAKING FILE
        JMP     ERROR
E22
        RTS
;TEST$LEGAL
;TEST FOR ILLEGAL CHARACTER IN FILE NAME
;IF ILLEGAL SEND ERROR MESSAGE AND EXIT
TSTLGL
        CMP     #' '            ;SEE IF SPACE OR LESS
        BCC     ILGCHR          ;ERROR IF LESS
        BEQ     ILGCHR          ;OR IF SPACE
        CMP     #'='
        BEQ     ILGCHR          ;ERROR IF =
        CMP     #'_'
        BEQ     ILGCHR          ;OR IF _
        CMP     #'.'
        BEQ     ILGCHR          ;OR IF .
        CMP     #':'
        BEQ     ILGCHR          ;OR IF :
        CMP     #SEMICO
        BEQ     ILGCHR          ;OR IF ;
        CMP     #'<'
        BEQ     ILGCHR          ;OR IF <
        CMP     #'>'
        BEQ     ILGCHR          ;OR IF >
        CMP     #$7F
        BEQ     ILGCHR          ;OR IF DELETE
        CMP     #'*'
        BEQ     ILGCHR          ;OR IF *
        CMP     #'?'
        BEQ     ILGCHR          ;OR IF ?
        RTS                     ;ELSE OK
ILGCHR
        LDA     #28             ;FILE NAME ERROR
        JMP     ERROR
;DISK$OPEN
;OPENS THE FILE - RA CONTAINS THE ADDRESS OF THE FILE NAME AND RB CONTAINS THE
;BLOCK SIZE.  THE ARRAY FILES WILL HOLD THE ADDRESS OF THE FILE CONTROL BLOCK
;IN THE FSA.  THE FCB IS FOLLOWED BY 3 FLAGS - BLOCKSIZE(ADDR),
;RECORDPOINTER(ADDR), WRITEFLAG(BYTE).  THIS IS FOLLOWED BY THE 128 BYTE BUFFER
;TO DO FILE I/O
NXTFLE  ;NEXTFILE
        .RES    1
DOI     ;I
        .RES    1
DOIT    ;TYPE INDEX
        .RES    1
DOJ     ;J
        .RES    1
DSKOPN
        LDA     #8
        STA     DOIT            ;SET TYPE INDEX
        LDX     #0              ;NEXTFILE:=0
L022
        INX
        INX
        LDA     FILES,X
        ORA     FILES+1,X
        BNE     L022            ;DO WHILE FILES( ) <> 0
        STX     NXTFLE
        LDA     #166
        LDY     #0
        JSR     GETS            ;GETSPACE(166)
        STA     FILADR
        STY     FILADR+1        ;FILEADDR:=
        LDX     NXTFLE
        STA     FILES,X
        TYA
        STA     FILES+1,X       ;FILES(NEXTFILE):=
        CLC
        LDA     FILADR
        LDY     FILADR+1
        ADC     #38
        STA     BUFFER
        BCC     *+3
        INY
        STY     BUFFER+1        ;BUFFER:=FILEADDR+38
        JSR     SETDMA
        LDY     #11
        LDA     #' '
L023
        STA     (FILADR),Y
        DEY
        BNE     L023            ;FILL((FILENAME:=FILEADDR+1),' ',11)
        LDA     (RA),Y
        STA     DOBF
        INY
        LDA     (RA),Y
        STA     DOBF+1          ;BUFF:=ARA
        INY                     ;Y:=2
        LDA     (DOBF),Y
        CMP     #':'            ;IF CHAR(2) <> :
        BNE     L024            ;THEN
        DEY                     ;Y:=1
        LDA     (DOBF),Y
        JSR     LWRUPR          ;CONVERT TO UPPER CASE
        CMP     #'A'            ;MAKE SURE IS A OR MORE
        BCC     ILGCHR          ;ERROR IF NOT
        CMP     #'H'+1
        BCS     ILGCHR          ;ERROR IF OVER H
        AND     #$F
        DEY                     ;Y:=0
        STA     (FILADR),Y      ;FCB(0):=CHAR(1) AND $F
        LDA     (DOBF),Y
        SEC
        SBC     #2
        STA     DOI             ;I:=CHAR(0)-2
        CLC
        LDA     DOBF
        ADC     #2
        STA     DOBF
        BCC     *+4
        INC     DOBF+1          ;BUFF:=BUFF+2
        JMP     L026
L024
        LDY     #0
        LDA     (DOBF),Y
        STA     DOI             ;I:=CHAR(0)
L026
        LDA     DOI
        CMP     #12+1
        BCC     L027
        LDA     #12
        STA     DOI             ;I:=12
L027
        INC     DOBF
        BNE     *+4
        INC     DOBF+1          ;BUFF:=BUFF+1
        LDA     #255
        STA     DOJ             ;J:=255
L028
        INC     DOJ
        LDY     DOJ
        LDA     (DOBF),Y
        JSR     LWRUPR          ;CONVERT TO UPPERCASE
        CMP     #'.'            ;IF CHAR(INCJ$)=.
        BEQ     L029            ;THEN DONE
        CPY     DOI             ;OR IF J >= I
        BCS     L029            ;THEN DONE
        INY
        JSR     TSTLGL          ;MAKE SURE IS LEGAL
        STA     (FILADR),Y      ;ELSE PUT IN FCB
        JMP     L028            ;AND LOOP
L029
        INC     DOJ
        INC     DOIT            ;BUMP TYPE INDEX
        LDY     DOJ
        CPY     DOI             ;IF J >= I
        BCS     L037            ;THEN DONE
        LDA     (DOBF),Y
        JSR     LWRUPR          ;CONVERT TO UPPERCASE
        LDY     DOIT            ;GET TYPE INDEX
        JSR     TSTLGL          ;MAKE SURE IS LEGAL
        STA     (FILADR),Y
        JMP     L029
L037
        JSR     SUFLEX          ;SETUP$FILE$EXTENT
        JSR     INTDBF          ;INITIALIZE$DISK$BUFFER
        LDA     FILADR
        LDX     FILADR+1
        INX                     ;AX:=FILEADDR+256
        LDY     #18*2           ;WORD ADDRESS
        STA     (FILADR),Y
        INY
        TXA
        STA     (FILADR),Y      ;FCBADD(18):=FILEADDR+256
        JSR     POPSTK          ;POP$STACK
        LDY     #0
        LDA     (RA),Y
        TAX
        INY
        LDA     (RA),Y
        LDY     #17*2+1         ;HIGH OF WORD ADDRESS
        STA     (FILADR),Y
        DEY
        TXA
        STA     (FILADR),Y      ;FCBADD(17):=ARA
        JMP     POPSTK          ;POP$STACK
;SET$EOF$STACK
STEFST
        LDA     RA
        LDY     RA+1
        STA     EOFRA
        STY     EOFRA+1         ;EOFRA:=RA
        LDA     RB
        LDY     RB+1
        STA     EOFRB
        STY     EOFRB+1         ;EOFRB:=RB
        RTS
;SETUP$DISK$IO
SUDSIO
        JSR     STFLAD          ;SET$FILE$ADDR
        JSR     STFLPT          ;SET$FILE$POINTER
        LDA     #0
        STA     BYTSWR
        STA     BYTSWR+1        ;BYTES$WRITTEN:=0
        LDA     #TRUE
        STA     FRSFLD          ;FIRSTFIELD:=TRUE
        JMP     POPSTK          ;POP$STACK
;RANDOM$SETUP
RURC    ;RECORD
        .RES    1
RANSTU
        JSR     VARBSZ          ;IF VAR$BLOCK$SIZE
        BNE     *+7             ;THEN OK
        LDA     #18             ;ELSE UNBLOCKED FILE ERROR
        JMP     ERROR
        JSR     RAZERA          ;IF RA$ZERO$ADDRESS
        BNE     L038            ;THEN ERROR
        JSR     RANEG           ;IF POSITIVE
        BEQ     *+7             ;THEN OK
L038
        LDA     #8              ;ELSE INVALID RECORD #
        JMP     ERROR
        LDY     #0
        SEC
        LDA     (RA),Y
        SBC     #1
        STA     (RA),Y
        INY
        LDA     (RA),Y
        SBC     #0
        STA     (RA),Y
        JSR     SETRAN          ;SET$RANDOM$MODE
        JSR     SETBFI          ;SET$BUFFER$INACTIVE
        JSR     WDIFRQ          ;WRITE$DISK$IF$REQ
        LDA     BLKSZE
        LDY     BLKSZE+1
        STA     PX
        STY     PX+1            ;PX:=BLOCKSIZE
        JSR     ARA             ;AY:=RECORD NUMBER - 1
        JSR     MXAP            ;AY:=AY*PX
        AND     #$7F            ;A:=BYTECOUNT AND $7F
        CLC
        ADC     BUFFER
        LDY     BUFFER+1
        BCC     *+3
        INY
        TAX
        BNE     *+3
        DEY
        DEX
        STX     RCRPNT
        STY     RCRPNT+1        ;RECORDPOINTER:=( )+BUFFER-1
        JSR     STRRPT          ;STORE$REC$PTR
        JSR     SHFPXL          ;SHIFT ACCUM LEFT
        LDA     PX+1            ;GET RECORD BYTE
        AND     #$7F            ;CLEAR MSB
        BIT     EXTSZE          ;TEST FLAG
        BPL     NOT8KE          ;SKIP IF 16K
        AND     #$3F            ;ELSE TRIM SOME MORE
        JSR     SHFPXL          ;AND SHIFT EXTRA TIME
NOT8KE
        STA     RURC            ;PUT IN RECORD
        JSR     SHFPXL          ;SHIFT LEFT AGAIN
        LDY     #12
        LDA     (FILADR),Y      ;A:=FCB(12)
        CMP     PX+2            ;COMPARE TO LOW
        BNE     CHGEXT          ;CHANGE IF DIFF
        INY
        LDA     (FILADR),Y
        CMP     PX+3            ;SAME FOR HIGH
        BEQ     L040            ;IF EXTENT=( ) THEN
CHGEXT
        JSR     CLOSE           ;ELSE CLOSE
        BPL     *+7             ;IF OK
        LDA     #1              ;ELSE FILE CLOSING ERROR
        JMP     ERROR
        LDY     #12
        LDA     PX+2
        STA     (FILADR),Y      ;FCB(12):=EXTENT
        INY                     ;SAME FOR HIGH
        LDA     PX+3
        STA     (FILADR),Y
        JSR     SUFLEX          ;SETUP$FILE$EXTENT
L040
        LDA     RURC
        LDY     #32
        STA     (FILADR),Y      ;FCB(32):=LOW(RECORD) AND $7F
        JMP     POPSTK          ;POP$STACK
;SHIFT FIXED POINT ACCUM LEFT ONE BIT
SHFPXL
        ASL     PX
        ROL     PX+1
        ROL     PX+2
        ROL     PX+3
        RTS
;GET$DISK$CHAR
GTDSCH
        JSR     ATENDB          ;IF NOT AT$END$DISK$BUFFER
        BEQ     L041            ;THEN
        JSR     WDIFRQ          ;WRITE$DISK$IF$REQ
        JSR     FILFBF          ;FILL$FILE$BUFFER
L041
        JSR     ACTBUF          ;IF ACTIVE$BUFFER
        BNE     *+5             ;THEN
        JSR     FILFBF          ;ELSE FILL$FILE$BUFFER
        LDY     #0
        LDA     (RCRPNT),Y      ;IF NEXTDISKCHAR
        CMP     #EOF            ;<> EOFFILLER
        BNE     *+5             ;THEN
        JSR     DSKEOF          ;ELSE DISK$EOF
        LDY     #0
        LDA     (RCRPNT),Y
        RTS                     ;RETURN NEXTDISKCHAR
;INC$POINT
INCWFP
        INC     WTFPNT
        BNE     *+4
        INC     WTFPNT+1
        RTS                     ;POINT:=POINT+1
;WRITE$TO$FILE(TYPE)
;IF TYPE=0 THEN NUMBER, =1 THEN STRING
WFTYP   ;TYPE
        .RES    1
WFI     ;I
        .RES    1
WFCNT   ;COUNT
        .RES    1
WRTOFL
        STA     WFTYP           ;SAVE TYPE
        TAX
        BNE     *+5             ;IF STRING THEN
        JSR     NUMOUT          ;ELSE NUMERICOUT
        BIT     FRSFLD
        BMI     L000            ;IF FIRSTFIELD THEN
        LDA     #','
        JSR     WRABYT          ;ELSE WRITE$A$BYTE(,)
        JMP     L001
L000
        LDA     #FALSE
        STA     FRSFLD          ;FIRSTFIELD:=FALSE
L001
        LDY     #0
        LDA     (RA),Y
        STA     WTFPNT
        INY
        LDA     (RA),Y
        STA     WTFPNT+1        ;POINT:=ARA
        DEY
        LDA     (WTFPNT),Y
        STA     WFCNT           ;COUNT:=CHAR
        LDA     WFTYP
        BNE     L002            ;IF TYPE NOT NUMERIC THEN
        DEC     WFCNT           ;ELSE COUNT:=COUNT-1
        JMP     L003
L002
        LDA     #'"'
        JSR     WRABYT          ;WRITE$A$BYTE(")
L003
        JSR     INCWFP          ;INC$POINT
        LDA     #1
        STA     WFI             ;I:=1
L004
        LDA     WFCNT
        CMP     WFI             ;IF I > COUNT
        BCC     L005            ;THEN
        LDY     #0
        LDA     (WTFPNT),Y      ;A:=CHAR
        CMP     #'"'
        BNE     *+7             ;IF NOT QUOTE THEN
        LDA     #24             ;ELSE WRITE QUOTE ERROR
        JMP     ERROR
        JSR     WRABYT          ;WRITE$A$BYTE(CHAR)
        JSR     INCWFP          ;INC$POINT
        INC     WFI             ;BUMP I
        JMP     L004            ;AND LOOP
L005
        LDA     WFTYP
        BEQ     L006            ;IF TYPE NOT STRING THEN
        LDA     #'"'            ;ELSE
        JSR     WRABYT          ;WRITE$A$BYTE(")
        JSR     STRFRE          ;STRINGFREE
L006
        JMP     POPSTK          ;POP$STACK
;DISK$CLOSE
DSKCLS
        JSR     STFLPT          ;SET$FILE$POINTERS
        JSR     WDIFRQ          ;WRITE$DISK$IF$REQ
        JSR     CLOSE
        BPL     *+7             ;IF CLOSE OK THEN
        LDA     #1              ;ELSE CLOSE ERROR
        JMP     ERROR
        LDA     FILADR
        LDY     FILADR+1
        JMP     RELS            ;RELEASE(FILEADDR)
;CLOSEFILES
CLSFLS
        LDX     #0              ;I:=0
L007
        INX                     ;I:=I+1
        CPX     #NUMFIL
        BCC     *+5             ;IF I < NUMFILES THEN
        BEQ     *+3             ;IF I = NUMFILES THEN
        RTS
        TXA                     ;A:=I
        PHA                     ;SAVE ON STACK
        ASL     A               ;A:=A*2
        TAX
        LDA     FILES,X
        STA     FILADR
        LDA     FILES+1,X
        STA     FILADR+1        ;FILEADDR:=FILES(I)
        ORA     FILADR
        BEQ     *+5             ;IF FILEADDR=0 THEN
        JSR     DSKCLS          ;ELSE DISK$CLOSE
        PLA                     ;GET I BACK
        TAX
        JMP     L007            ;AND LOOP
;ROUTINE TO EXIT INTERPRETER
;EXIT$INTERP
EXTINT
        JSR     CLSFLS          ;CLOSEFILES
        JSR     DMPPBF          ;DUMP$PRINT$BUFF
        JSR     CRLF
        JMP     BOOT
;STORE(TYPE)
;STORE PLACES RA IN THE PRT LOCATION REFERENCED BY RB. RA MAY CONTAIN A
;FLOATING POINT NUMBER OR A REFERENCE TO A STRING.  IN THE CASE OF A STRING
;THE FOLLOWING IS ALSO PERFORMED
;	(1)	IF THE PRT CELL ALREADY CONTAINS A REFERENCE TO A STRING IN THE
;		FSA THAT STRING'S COUNTER IS DECREMENTED AND IF EQUAL TO 1 THEN
;		THE SPACE IS FREED.
;	(2)	THE NEW STRING COUNTER IS INCREMENTED.  IF IT IS ALREADY 255
;		THEN A COPY IS MADE AND THE NEW COUNTER SET TO 2.
STORE
        PHA                     ;SAVE TYPE
        LDA     RB
        LDY     RB+1
        JSR     SEDAAD          ;SET$DATA$ADDR(RB)
        PLA                     ;GET TYPE
        AND     #1
        BNE     *+5             ;IF STRING CONTINUE
        JMP     JSTSTR          ;IF NUMBER JUST STORE
        LDA     #0
        JSR     FLSTRA          ;FLAG$STRING$ADDR(0)
        JSR     ARB
        STA     PTRADR
        STY     PTRADR+1        ;PTRADDR:=ARB
        LDY     #0
        LDA     (PTRADR),Y
        PHA
        INY
        LDA     (PTRADR),Y
        TAY
        PLA                     ;AY:=STRINGADDR
        JSR     INFSA           ;IN$FSA(STRINGADDR)
        BEQ     NTTHR           ;BRANCH IF NOT
        LDY     #0
        LDA     (PTRADR),Y
        TAX
        INY
        LDA     (PTRADR),Y
        TAY
        TXA
        BNE     *+3
        DEY
        DEX                     ;STRINGADDR-1
        STX     SPTR
        STY     SPTR+1          ;PTR:=
        SEC
        LDY     #0
        LDA     (SPTR),Y
        SBC     #1
        STA     (SPTR),Y        ;COUNTER:=COUNTER-1
        CMP     #1
        BNE     NTTHR           ;IF <> 1 THEN
        LDA     (PTRADR),Y
        PHA
        INY
        LDA     (PTRADR),Y
        TAY
        PLA
        JSR     RELS            ;ELSE RELEASE(STRINGADDR)
NTTHR
        JSR     ARA
        SEC
        SBC     #1
        BCS     *+3
        DEY
        STA     SPTR
        STY     SPTR+1          ;PTR:=ARA-1
        JSR     INFSA           ;INFSA(PTR)
        BEQ     JSTSTR          ;IF NOT THEN
        LDY     #0
        LDA     (SPTR),Y        ;A:=COUNTER
        CMP     #255            ;IF NOT 255
        BNE     BMPCNT          ;JUST BUMP COUNT
        INC     SPTR
        BNE     *+4
        INC     SPTR+1
        LDA     (SPTR),Y        ;COUNTER
        CLC
        ADC     #1              ;A:=COUNTER+1
        JSR     GETS            ;GETSPACE(COUNTER+1)
        PHA                     ;SAVE LOW
        TYA                     ;HIGH TO A
        LDY     #1
        STA     (RA),Y
        STA     MTO+1
        DEY
        PLA
        STA     (RA),Y          ;ARA:=GETSPACE(COUNTER+1)
        STA     MTO             ;TO:=ARA
        LDA     SPTR
        LDY     SPTR+1
        STA     MFROM
        STY     MFROM+1         ;FROM:=PTR
        LDY     #0
        LDA     (SPTR),Y
        CLC
        ADC     #1
        JSR     MOVE            ;MOVE( , , )
        LDY     #0
        LDA     (RA),Y
        TAX
        INY
        LDA     (RA),Y
        TAY
        TXA
        BNE     *+3
        DEY
        DEX
        STX     SPTR
        STY     SPTR+1          ;PTR:=ARA-1
BMPCNT
        LDY     #0
        LDA     (SPTR),Y
        CLC
        ADC     #1
        STA     (SPTR),Y        ;COUNTER:=COUNTER+1
JSTSTR
        LDA     RA
        LDY     RA+1
        STA     MFROM
        STY     MFROM+1         ;FROM:=RA
        JSR     ARB
        STA     MTO
        STY     MTO+1           ;TO:=ARB
        JMP     MOVE4           ;MOVE4(FROM,TO)
;ARRAY ADDRESSING ROUTINES
;CALC$ROW
;SETS UP AN ARRAY IN THE FSA IN ROW MAJOR ORDER. THE BYTE OF CODE FOLLOWING THE
;OPERATOR IS THE NUMBER OF DIMENSIONS.  THE STACK CONTAINS THE UPPER BOUND OF
;EACH DIMENSION - RA HOLDS DIMENSION N, RB N-1, ETC.  THE LOWER BOUND IS ALWAYS
;ZERO.
CLCROW
        LDA     #1
        LDY     #0
        STA     ASIZE
        STY     ASIZE+1         ;ASIZE:=1
        JSR     STINCN          ;STEP$INS$CNT
        LDA     RA
        LDY     RA+1
        STA     SAVERA
        STY     SAVERA+1        ;SAVERA:=RA
        LDA     RB
        LDY     RB+1
        STA     SAVERB
        STY     SAVERB+1        ;SAVERB:=RB
        LDA     #1
        STA     CI              ;I:=1
CLRP
        LDY     #0
        LDA     (RC),Y          ;GET NUMDIM
        CMP     CI              ;IF I>NUMDIM
        BCC     GTALLD          ;THEN
        JSR     ARA
        CLC
        ADC     #1
        BCC     *+3
        INY
        STA     PX
        STY     PX+1            ;PX:=ARA+1
        LDA     ASIZE
        LDY     ASIZE+1
        JSR     MXAP            ;AY:=ASIZE*(ARA+1)
        STA     ASIZE
        STY     ASIZE+1         ;ASIZE:=
        LDY     #0
        STA     (RA),Y
        INY
        LDA     ASIZE+1
        STA     (RA),Y          ;ARA:=ASIZE
        JSR     POPSTK          ;POP$STACK
        INC     CI              ;I:=I+1
        JMP     CLRP            ;AND LOOP
GTALLD
        LDA     SAVERA
        LDY     SAVERA+1
        STA     RA
        STY     RA+1            ;RA:=SAVERA
        LDA     SAVERB
        LDY     SAVERB+1
        STA     RB
        STY     RB+1            ;RB:=SAVERB
        LDY     #0
        LDA     (RC),Y          ;A:=NUMDIM
        CLC
        ADC     #1
        ASL     A
        STA     CLCT
        LDA     ASIZE
        LDY     ASIZE+1
        JSR     TIMES4          ;TIMES4(ASIZE)
        CLC
        ADC     CLCT
        BCC     *+3
        INY
        JSR     GETS            ;GETSPACE()
        STA     SAVERA
        STY     SAVERA+1        ;SAVERA:=
        STA     ARYADR
        STY     ARYADR+1        ;ARRAYADDR:=
        LDY     #0
        LDA     (RC),Y          ;A:=NUMDIM
        STA     (ARYADR),Y
        TYA
        INY
        STA     (ARYADR),Y      ;:=0
        LDA     #1
        STA     CI              ;I:=1
CALP
        LDY     #0
        LDA     (RC),Y
        CMP     CI              ;IF I>NUMDIM
        BCC     GTALAA          ;THEN
        CLC
        LDA     ARYADR
        ADC     #2
        STA     ARYADR
        BCC     *+4
        INC     ARYADR+1        ;ARRAYADDR:=ARRAYADDR+2
        LDY     #0
        LDA     (RA),Y
        STA     (ARYADR),Y
        INY
        LDA     (RA),Y
        STA     (ARYADR),Y      ;ARRAYPOS:=ARA
        JSR     POPSTK          ;POP$STACK
        INC     CI              ;I:=I+1
        JMP     CALP
GTALAA
        JSR     PSHSTK          ;PUSH$STACK
        LDY     #0
        LDA     SAVERA
        STA     (RA),Y
        INY
        LDA     SAVERA+1
        STA     (RA),Y          ;ARA:=SAVERA
        RTS
;INC$ARRAYADDR
INCAAD
        CLC
        LDA     ARYADR
        ADC     #2
        STA     ARYADR
        BCC     *+4
        INC     ARYADR+1        ;ARRAYADDR:=ARRAYADDR+2
        RTS
;CALC$SUB
;PERFORMS A SUBSCRIPT CALCULATION FOR THE ARRAY REFERENCED BY RA.  THE VALUE OF
;EACH DIMENSION IS ON THE STACK BELOW THE ARRAY ADDRESS STARTING WITH THE NTH
;DIMENSION.  A CHECK IS MADE TO SEE IF THE SELECTED ELEMENT IS OUTSIDE THE AREA
;ASSIGNED TO THE ARRAY
CLCSUB
        JSR     ARA
        STA     ARYADR
        STY     ARYADR+1        ;ARRAYADDR:=ARA
        JSR     POPSTK          ;POP$STACK
        JSR     ARA
        STA     LOCATN
        STY     LOCATN+1        ;LOCATION:=ARA
        LDY     #0
        LDA     (ARYADR),Y
        STA     NUMDIM          ;NUMDIM:=ARRAYPOS
        LDA     #2
        STA     CI              ;I:=2
CLSLP
        LDA     NUMDIM
        CMP     CI              ;IF I>NUMDIM
        BCC     CLALSB          ;THEN
        JSR     POPSTK          ;ELSE POP$STACK
        JSR     INCAAD          ;INC$ARRAYADDR
        LDY     #0
        LDA     (ARYADR),Y
        STA     PX
        INY
        LDA     (ARYADR),Y
        STA     PX+1
        JSR     ARA
        JSR     MXAP            ;AY:=ARA*ARRAYPOS
        CLC
        ADC     LOCATN
        STA     LOCATN
        TYA
        ADC     LOCATN+1
        STA     LOCATN+1        ;LOCATION:=ARA*ARRAYPOS+LOCATION
        INC     CI              ;I:=I+1
        JMP     CLSLP           ;AND LOOP
CLALSB
        JSR     INCAAD          ;INC$ARRAYADDR
        LDY     #0
        LDA     LOCATN
        CMP     (ARYADR),Y
        INY
        LDA     LOCATN+1
        SBC     (ARYADR),Y
        BCC     *+7             ;IF ARRAYPOS>LOCATION THEN OK
        LDA     #19             ;ELSE SUBSCRIPTING ERROR
        JMP     ERROR
        LDA     LOCATN
        LDY     LOCATN+1
        JSR     TIMES4          ;TIMES4(LOCATION)
        CLC
        ADC     ARYADR
        TAX
        TYA
        ADC     ARYADR+1
        TAY                     ;+ARRAYADDR
        CLC
        TXA
        ADC     #2
        TAX
        BCC     *+3
        INY                     ;+2
        TYA
        LDY     #1              ;HIGH FIRST
        STA     (RA),Y
        DEY
        TXA
        STA     (RA),Y          ;ARA:=
        RTS
;BRANCHING ROUTINES
;UNCOND$BRANCH
UNCBRA
        JSR     ARA
        CLC
        ADC     RC
        TAX
        TYA
        ADC     RC+1
        TAY                     ;AY:=ARA+RC
        TXA
        BNE     *+3
        DEY
        DEX
        STX     RC
        STY     RC+1            ;RC:=RC+ARA-1
        JMP     POPSTK          ;POP$STACK
;COND$BRANCH
CNDBRA
        JSR     RBZER
        BEQ     *+8             ;IF NOT RBZERO THEN
        JSR     UNCBRA          ;ELSE DO BRANCH
        JMP     POPSTK
        JSR     POPSTK          ;POP$STACK TWICE
        JMP     POPSTK
;ABSOLUTE$BRANCH
ABSBRA
        JSR     STINCN          ;STEP$INS$CNT
        JSR     ARC
        STA     RC
        STY     RC+1            ;RC:=TWOBYTEOPERAND
        RTS
;GLOBAL STRING HANDLING ROUTINES
CHKSAD
        LDY     #2
        LDA     (RA),Y
        RTS
;STRING$FREE
STRFRE
        JSR     CHKSAD
        AND     #1
        BNE     *+3             ;IF CHECK$STRING$ADDR THEN
        RTS
        JSR     ARA
        JMP     RELS            ;RELEASE(ARA)
;GET$STRING$LEN
GETSLN
        STA     GETSL+1
        STY     GETSL+2
        ORA     GETSL+2
        BNE     *+3
        RTS
GETSL
        LDA     $FFFF
        RTS
;COMP$FIX
COMFIX
        AND     #1              ;IF NOT FLAG
        BEQ     NTCFLG          ;THEN
        LDY     #3
COMFXL
        LDA     MONE,Y          ;MOVE4(.MINUSONE,RA)
        STA     (RA),Y
        DEY
        BPL     COMFXL
        RTS
NTCFLG
        TAY                     ;BRAZ:=0
        STA     (RA),Y
        RTS
;CHKCARRY
CHKSCY
        BCC     *+7             ;IF C=0 THEN OK
        LDA     #20             ;ELSE STRING LENGTH ERROR
        JMP     ERROR
        RTS
;CONCATENATE
;THE STRING POINTED TO BY RA IS CONCATENATED TO THE STRING POINTED TO BY RB AND
;THE POINTER TO THE RESULT IS PLACED IN RB.  THE STACK IS POPPED AND THE RESULT
;IS FLAGGED AS A TEMPORARY STRING.
CONCAT
        JSR     RAZERA          ;IF RA$ZERO$ADDRESS
        BNE     E14             ;THEN
        JSR     RBZERA          ;ELSE IF NOT RB$ZERO$ADDRESS
        BEQ     DOCAT           ;THEN
        JSR     MVRARB          ;ELSE MOVE$RA$RB
E14
        JMP     POPSTK          ;POP$STACK
DOCAT
        JSR     ARB
        JSR     GETSLN          ;GETSTRINGLEN(ARB)
        CLC
        ADC     #1              ;+1
        STA     FRSTLN          ;FIRSTSTRINGLENGTH:=
        JSR     CHKSCY          ;CHKCARRY
        JSR     ARA
        JSR     GETSLN          ;GETSTRINGLEN(ARA)
        STA     SCSTLN          ;SECONDSTRINGLENGTH:=
        CLC
        ADC     FRSTLN          ;+FIRSTSTRINGLENGTH
        STA     NWSTLN          ;NEWSTRINGLENGTH:=
        JSR     CHKSCY          ;CHKCARRY
        LDA     NWSTLN
        LDY     #0
        JSR     GETS            ;GETSPACE(NEWSTRINGLENGTH)
        STA     NWSTAD
        STY     NWSTAD+1        ;NEWSTRINGADDRESS:=
        STA     MTO
        STY     MTO+1           ;TO:=
        LDY     #0
        LDA     (RB),Y
        STA     MFROM
        INY
        LDA     (RB),Y
        STA     MFROM+1         ;FROM:=ARB
        LDA     FRSTLN
        DEY                     ;LENGTH:=FIRSTSTRINGLENGTH
        JSR     MOVE            ;( , , )
        LDY     #0
        LDA     (RA),Y
        STA     MFROM
        INY
        LDA     (RA),Y
        STA     MFROM+1
        INC     MFROM
        BNE     *+4
        INC     MFROM+1         ;FROM:=ARA+1
        CLC
        LDA     NWSTAD
        ADC     FRSTLN
        STA     MTO
        LDY     NWSTAD+1
        BCC     *+3
        INY
        STY     MTO+1           ;TO:=NEWSTRINGADDRESS+FIRSTSTRINGLENGTH
        LDA     SCSTLN
        LDY     #0              ;LENGTH:=SECONDSTRINGLENGTH
        JSR     MOVE            ;MOVE( , , )
        JSR     STRFRE          ;STRINGFREE
        JSR     POPSTK          ;POP$STACK
        JSR     STRFRE          ;STRINGFREE
        LDY     #0
        LDA     NWSTAD
        STA     (RA),Y
        INY
        LDA     NWSTAD+1
        STA     (RA),Y          ;ARA:=NEWSTRINGADDRESS
        LDX     NWSTLN
        DEX
        TXA
        DEY                     ;Y:=0
        STA     (NWSTAD),Y      ;LENGTH:=NEWSTRINGLENGTH-1
        LDA     #TRUE
        JMP     FLSTRA          ;FLAG$STRING$ADDR(TRUE)
;FIXSTACK
FIXSTK
        JSR     STRFRE          ;STRING$FREE
        JSR     POPSTK          ;POP$STACK
        JMP     STRFRE          ;STRING$FREE
;INC$BRA
INCBRA
        LDY     #0
        LDA     (RA),Y
        CLC
        ADC     #1              ;RETURN BRAZ+1
        RTS
;COMPARE$STRING
;THE STRING POINTED TO BY RB IS COMPARED TO THE STRING POINTED TO BY RA
;IF RB < RA THEN RETURN 1
;   RB > RA 		2
;   RB = RA		3
;TWO STRINGS ARE EQUAL IF AND ONLY IF THE TWO STRINGS HAVE THE SAME LENGTH AND
;CONTAIN IDENTICAL CHARACTERS.  THE ASCII COLLATING SEQUENCE IS USED TO
;DETERMINE THE RELATIONSHIP BETWEEN EQUAL LENGTH STRINGS.  IF TWO STRINGS ARE
;NOT OF EQUAL LENGTH THE SHORTER IS ALWAYS LESS THAN THE LONGER ONE.  ALL NULL
;STRINGS ARE EQUAL AND LESS THAN ANY OTHER STRING.
CSI     ;I
        .RES    1
CSTL    ;TEMPLENGTH
        .RES    1
CMPSTR
        JSR     RAZERA          ;IF NOT RA$ZERO$ADDRESS
        BEQ     L061            ;THEN
        LDA     RA
        LDY     RA+1
        STA     SCNDST
        STY     SCNDST+1        ;SECONDSTRING:=RA
        JMP     L062
L061
        LDY     #0
        LDA     (RA),Y
        STA     SCNDST
        INY
        LDA     (RA),Y
        STA     SCNDST+1        ;SECONDSTRING:=ARA
L062
        JSR     RBZERA          ;IF NOT RB$ZERO$ADDRESS
        BEQ     L063            ;THEN
        LDA     RB
        LDY     RB+1
        STA     FRSTST
        STY     FRSTST+1        ;FIRSTSTRING:=RB
        JMP     L064
L063
        LDY     #0
        LDA     (RB),Y
        STA     FRSTST
        INY
        LDA     (RB),Y
        STA     FRSTST+1        ;FIRSTSTRING:=ARB
L064
        LDY     #0
        LDA     (FRSTST),Y
        STA     CSTL            ;TEMPLENGTH:=CHARSTRING1
        STY     CSI             ;I:=0
L065
        LDA     CSTL
        CMP     CSI             ;IF I>TEMPLENGTH
        BCC     L068            ;THEN
        LDY     #0
        LDA     (FRSTST),Y
        CMP     (SCNDST),Y
        BCS     L066            ;IF CHARSTRING1 >= CHARSTRING2 THEN
        JSR     FIXSTK          ;FIXSTACK
        LDA     #1
        RTS
L066
        BEQ     L067            ;IF CHARSTRING1 = CHARSTRING2 THEN
        JSR     FIXSTK          ;ELSE
        LDA     #2
        RTS
L067
        INC     FRSTST
        BNE     *+4
        INC     FRSTST+1
        INC     SCNDST
        BNE     *+4
        INC     SCNDST+1        ;BUMP STRING POINTERS
        INC     CSI             ;AND INDEX
        JMP     L065            ;THEN LOOP
L068
        JSR     FIXSTK          ;FIXSTACK
        LDA     #3
        RTS
;STRING$SEGMENT(TYPE)
SSTYPE  ;TYPE
        .RES    1
SSTMP2  ;TEMPA2
        .RES    2
SSTMPB  ;TEMPB1
        .RES    1
SSLNG2  ;LNG2
        .RES    1
STRSEG
        STA     SSTYPE          ;SAVE TYPE
        LDX     #0
        STX     SSTMPB          ;TEMPB1:=0
        CMP     #2
        BNE     L070            ;IF NOT MID THEN
        JSR     FLIP
        JSR     RANEG           ;IF RA$NEGATIVE
        BNE     L069            ;THEN ERROR
        JSR     RAZER           ;IF NOT
        BEQ     *+7             ;RA$ZERO THEN
L069
        LDA     #21             ;ELSE SECOND PARM OF MID
        JMP     ERROR           ;ZERO OR NEGATIVE
        JSR     CNVBAD          ;CONV$TO$BIN$ADDR
        LDY     #0
        LDA     (RA),Y
        STA     SSTMPB          ;TEMPB1:=BRAZ
        JSR     POPSTK          ;POP$STACK
L070
        JSR     RANEG
        BNE     L071            ;IF RA$NEGATIVE THEN
        JSR     ARB
        JSR     GETSLN          ;GET$STRING$LEN(ARB)
        CMP     SSTMPB          ;IF < TEMPB1
        BCC     L071            ;THEN
        JSR     RAZER           ;IF NOT
        BEQ     L072            ;RA$ZERO THEN
L071
        JSR     POPSTK          ;POP$STACK
        JSR     STRFRE          ;STRINGFREE
        LDY     #0
        TYA
        STA     (RA),Y
        INY
        STA     (RA),Y          ;ARA:=0
        RTS
L072
        JSR     CNVBAD          ;CONV$TO$BIN$ADDR
        JSR     ARB
        JSR     GETSLN          ;GET$STRING$LEN(ARB)
        SEC
        SBC     SSTMPB          ;-TEMPB1
        STA     SSLNG2          ;LNG2:=
        LDY     #0
        CMP     (RA),Y
        BCS     L074            ;IF >= BRAZ THEN
        LDA     SSTYPE
        CMP     #2
        BNE     L073            ;IF NOT MID THEN
        CLC
        LDA     SSLNG2
        ADC     #1
        STA     (RA),Y          ;BRAZ:=LNG2+1
        JMP     L074
L073
        LDA     SSLNG2
        STA     (RA),Y          ;BRAZ:=LNG2
L074
        LDA     SSTYPE
        BNE     L075            ;IF TYPE <> LEFT THEN
        JSR     ARB
        STA     SSTMP2
        STY     SSTMP2+1        ;TEMPA2:=ARB
        JMP     L077
L075
        CMP     #1
        BNE     L076            ;IF NOT RIGHT THEN
        JSR     ARB
        CLC
        ADC     SSLNG2
        BCC     *+3
        INY
        STY     SSTMP2+1
        SEC
        LDY     #0
        SBC     (RA),Y
        STA     SSTMP2
        BCS     *+5
        DEC     SSTMP2+1        ;TEMPA2:=ARB+LNG2-BRAZ
        JMP     L077
L076
        JSR     ARB
        CLC
        ADC     SSTMPB
        BCC     *+3
        INY
        SEC
        SBC     #1
        BCS     *+3
        DEY
        STA     SSTMP2
        STY     SSTMP2+1        ;TEMPA2:=ARB+TEMPB1-1
L077
        LDA     SSTMP2
        LDY     SSTMP2+1
        STA     MFROM
        STY     MFROM+1         ;FROM:=TEMPA2
        JSR     INCBRA
        LDY     #0
        JSR     GETS            ;GETSPACE(INC$BRA)
        STA     SSTMP
        STY     SSTMP+1         ;TEMPA:=
        STA     MTO
        STY     MTO+1           ;TO:=
        JSR     INCBRA
        LDY     #0
        JSR     MOVE            ;MOVE(FROM,TO,AY)
        LDY     #0
        LDA     (RA),Y
        STA     (SSTMP),Y       ;LNG:=BRAZ
        JSR     POPSTK          ;POP$STACK
        JSR     STRFRE          ;STRINGFREE
        LDY     #0
        LDA     SSTMP
        STA     (RA),Y
        INY
        LDA     SSTMP+1
        STA     (RA),Y          ;ARA:=TEMPA
        LDA     #TRUE
        JMP     FLSTRA          ;FLAG$STRING$ADDR(TRUE)
;DOS/65 INTERFACE ROUTINES
;CRLF
CRLF
        LDA     #CR
        JSR     PRNCHR
LFOUT
        LDA     #LF
;PRINTCHAR(CHAR)
PRNCHR
        BIT     LSTFLG          ;TEST FLAG
        BMI     *+7             ;IF SET DO LIST
PRNCON
        LDX     #2
        JMP     PEM
        LDX     #5
        JMP     PEM
;READ(A)
;WAIT FOR FIRST CHARACTER AND SET LOCALSEED TO BE USED TO SEED RANDOM NUMBER
;GENERATOR
READ
        PHA                     ;SAVE ADDRESS ON STACK
        TYA
        PHA
READ1
        LDX     #11
        JSR     PEM             ;SEE IF READY
        BNE     READ2           ;BRANCH IF IS
        INC     LCLSED
        BNE     *+4
        INC     LCLSED+1        ;LOCALSEED:=LOCALSEED+1
        JMP     READ1           ;AND LOOP
READ2
        PLA
        TAY
        PLA                     ;GET ADDRESS
        LDX     #10
        JMP     PEM             ;READ BUFFER
;OPEN A FILE
OPEN
        LDA     FILADR
        LDY     FILADR+1
        LDX     #15
        JMP     PEM
;CLOSE A FILE
CLOSE
        LDA     FILADR
        LDY     FILADR+1
        LDX     #16
        JMP     PEM
;DISKREAD
DSKRDE
        LDA     FILADR
        LDY     FILADR+1
        LDX     #20
        JMP     PEM
;DISKWRITE
DSKWRT
        LDA     FILADR
        LDY     FILADR+1
        LDX     #21
        JMP     PEM
;CREATE
CREATE
        LDA     FILADR
        LDY     FILADR+1
        LDX     #22
        JMP     PEM
;MAKE
MAKE
        LDA     FILADR
        LDY     FILADR+1
        LDX     #19
        JSR     PEM             ;DELETE
        JMP     CREATE
;SETDMA
;SET ADDRESS FOR DISK I/O
SETDMA
        LDA     BUFFER
        LDY     BUFFER+1
        LDX     #26
        JMP     PEM
;PRINT(LOCATION)
;PRINT STRING POINTED TO BY AY UNTIL $ IS ENCOUNTERED
PRINT
        LDX     #9
        JMP     PEM
;DYNAMIC STORAGE ALLOCATION ROUTINES
;AVAILABLE
AVLL
        STA     NBYTES
        STY     NBYTES+1        ;SAVE INPUT PARM
        LDA     MBASE
        LDY     MBASE+1
        STA     DSPNT
        STY     DSPNT+1         ;POINT:=MBASE
        LDA     #0
        STA     TOTAL
        STA     TOTAL+1         ;TOTAL:=0
AVLLP
        LDA     DSPNT
        ORA     DSPNT+1         ;DO WHILE POINT<>0
        BEQ     DSPNTZ
        LDY     #4
        LDA     (DSPNT),Y       ;A:=SWITCH(4)
        BNE     SW4NZ           ;IF <>0 THEN
        LDY     #0
        LDA     (DSPNT),Y
        TAX
        INY
        LDA     (DSPNT),Y
        TAY                     ;XY:=HERE
        SEC
        TXA
        SBC     DSPNT
        TAX
        TYA
        SBC     DSPNT+1
        TAY                     ;XY:=HERE-POINT
        SEC
        TXA
        SBC     #5
        BCS     *+3
        DEY
        STA     AVLLT           ;TEMP:=HERE-POINT-5
        STY     AVLLT+1
        CLC
        ADC     TOTAL
        STA     TOTAL
        TYA
        ADC     TOTAL+1
        STA     TOTAL+1         ;TOTAL:=TOTAL+TEMP
        LDA     NBYTES
        ORA     NBYTES+1        ;IF NBYTES<>0 THEN
        BEQ     SW4NZ           ;ELSE
        CLC
        LDA     NBYTES
        LDY     NBYTES+1
        ADC     #5
        BCC     *+3
        INY
        STA     AVLLT2
        STY     AVLLT2+1        ;SAVE NBYTES+5
        LDA     AVLLT
        CMP     AVLLT2
        LDA     AVLLT+1
        SBC     AVLLT2+1
        BCC     SW4NZ           ;IF NBYTES+5>TEMP THEN
        LDA     DSPNT
        LDY     DSPNT+1
        RTS                     ;ELSE RETURN POINT
SW4NZ
        LDY     #0
        LDA     (DSPNT),Y
        TAX
        INY
        LDA     (DSPNT),Y
        STX     DSPNT
        STA     DSPNT+1         ;POINT:=HERE
        JMP     AVLLP           ;LOOP TO DO WHILE
DSPNTZ
        LDA     NBYTES
        ORA     NBYTES+1
        BEQ     *+7             ;OK IF NBYTES=0
        LDA     #23             ;ELSE NO MEMORY ERROR
        JMP     ERROR
        LDA     TOTAL
        LDY     TOTAL+1
        RTS                     ;RETURN TOTAL
;GETSPACE
GETS
        STA     NBYTES
        STY     NBYTES+1
        ORA     NBYTES+1
        BNE     *+3
        RTS                     ;RETURN 0
        LDA     NBYTES          ;GET A BACK (Y IS OK)
        JSR     AVLL            ;AVAILABLE(NBYTES)
        STA     DSPNT
        STY     DSPNT+1         ;POINT:=
        LDY     #4              ;LINK UP THE SPACE
        LDA     #1
        STA     (DSPNT),Y       ;SET SWITCH(4):=1
        CLC
        LDA     NBYTES
        LDY     NBYTES+1
        ADC     #5
        BCC     *+3
        INY
        CLC
        ADC     DSPNT
        STA     DSTMP1
        TYA
        ADC     DSPNT+1
        STA     DSTMP1+1        ;TEMP1:=POINT+NBYTES+5
        LDY     #0
        LDA     (DSPNT),Y
        STA     (DSTMP1),Y
        INY
        LDA     (DSPNT),Y
        STA     (DSTMP1),Y      ;ADR1:=HERE
        LDY     #0
        LDA     (DSPNT),Y
        PHA
        INY
        LDA     (DSPNT),Y
        TAY
        PLA                     ;AY:=HERE
        CLC
        ADC     #2
        BCC     *+3
        INY
        STA     DSTMP2
        STY     DSTMP2+1        ;TEMP2:=HERE+2
        LDY     #0
        LDA     DSTMP1
        STA     (DSPNT),Y
        STA     (DSTMP2),Y
        INY
        LDA     DSTMP1+1
        STA     (DSPNT),Y
        STA     (DSTMP2),Y      ;HERE,ADR2:=TEMP1
        LDY     #4
        LDA     #0
        STA     (DSTMP1),Y      ;SWITCH2(4):=0
        CLC
        LDA     DSTMP1
        ADC     #2
        STA     DSTMP1
        BCC     *+4
        INC     DSTMP1+1        ;TEMP1:=TEMP1+2
        LDY     #0
        LDA     DSPNT
        STA     (DSTMP1),Y
        INY
        LDA     DSPNT+1
        STA     (DSTMP1),Y      ;ADR1:=POINT
        LDA     NBYTES
        LDY     NBYTES+1
        STA     FN
        STY     FN+1            ;FILL( , ,NBYTES)
        CLC
        LDA     DSPNT
        ADC     #5
        STA     DSPNT
        BCC     *+4
        INC     DSPNT+1         ;POINT:=POINT+5
        LDY     DSPNT+1         ;AY:=POINT
        LDX     #0
        JSR     FILL            ;FILLE(POINT,0, )
        LDA     DSPNT
        LDY     DSPNT+1
        RTS                     ;RETURN POINT
;RELEASE(SPACE)
RELS
        SEC
        SBC     #5
        STA     SPACE
        STA     HOLD
        BCS     *+3
        DEY
        STY     SPACE+1
        STY     HOLD+1          ;HOLD,SPACE:=SPACE-5
        LDY     #4
        LDA     #0
        STA     (SPACE),Y       ;SWITCH(4):=0
        JSR     UNLINK          ;COMBINE SPACE IF POSSIBLE
        CLC
        LDA     SPACE
        ADC     #2
        STA     SPACE
        BCC     *+4
        INC     SPACE+1         ;SPACE:=SPACE+2
        LDY     #0
        LDA     (SPACE),Y
        PHA
        INY
        LDA     (SPACE),Y
        TAY
        PLA
        STA     SPACE
        STY     SPACE+1         ;SPACE:=HERE
        ORA     SPACE+1
        BNE     *+3             ;IF ( ) <> 0 THEN
        RTS
        LDY     #4
        LDA     (SPACE),Y       ;IF SWITCH(4)=0
        BEQ     *+3             ;THEN
        RTS
        JSR     UNLINK          ;UNLINK
        LDA     SPACE
        LDY     SPACE+1
        STA     HOLD
        STY     HOLD+1          ;HOLD:=SPACE
        RTS
;UNLINK
UNLINK
        LDY     #0
        LDA     (SPACE),Y
        STA     DSTMP3
        INY
        LDA     (SPACE),Y
        STA     DSTMP3+1        ;TEMP:=HERE
        LDA     (DSTMP3),Y
        DEY
        ORA     (DSTMP3),Y
        BNE     *+3             ;IF ADRS<>0 THEN
        RTS
        LDY     #4
        LDA     (DSTMP3),Y
        BEQ     *+3             ;IF LOOK(4)=0 THEN
        RTS
        LDY     #0
        LDA     (DSTMP3),Y
        STA     (SPACE),Y
        PHA
        INY
        LDA     (DSTMP3),Y
        STA     (SPACE),Y       ;HERE:=ADRS
        TAY
        PLA
        CLC
        ADC     #2
        STA     DSTMP3
        BCC     *+3
        INY
        STY     DSTMP3+1        ;TEMP:=HERE+2
        LDY     #0
        LDA     SPACE
        STA     (DSTMP3),Y
        INY
        LDA     SPACE+1
        STA     (DSTMP3),Y      ;ADRS:=SPACE
        RTS
;GENERAL PURPOSE INTERPRETER ROUTINES
;MOVE4(FROM,TO)
MOVE4
        LDY     #3
L080
        LDA     (MFROM),Y
        STA     (MTO),Y
        DEY
        BPL     L080
        RTS
;MOVE
;ASSUMES FROM AND TO POINTERS ARE PRESTORED BY CALLING ROUTINE AND THAT COUNT
;IS IN AY AT ENTRY
MOVE
        STA     MCOUNT
        STY     MCOUNT+1
        LDY     #0
MOVELP
        LDA     MCOUNT
        ORA     MCOUNT+1
        BNE     *+3             ;IF COUNT<>0 THEN
        RTS                     ;ELSE DONE
        LDA     MCOUNT
        BNE     *+5
        DEC     MCOUNT+1
        DEC     MCOUNT          ;COUNT:=COUNT-1
        LDA     (MFROM),Y
        STA     (MTO),Y         ;(TO):=(FROM)
        INC     MFROM
        BNE     *+4
        INC     MFROM+1         ;FROM:=FROM+1
        INC     MTO
        BNE     MOVELP
        INC     MTO+1
        JMP     MOVELP          ;TO:=TO+1
;TIMES4(N)
TIMES4
        STY     TIME4T          ;STORE HIGH
        ASL     A
        ROL     TIME4T
        ASL     A
        ROL     TIME4T
        LDY     TIME4T
        RTS
;FILL
;ASSUMES NUMBER BYTES IS PRESTORED AT FN AND AY CONTAINS FIRST ADDRESS AND X
;CONTAINS FILL CHAR
FILL
        STA     FILLIT+1
        STY     FILLIT+2        ;SET ADDRESS
FILLLP
        LDA     FN
        BNE     *+4
        DEC     FN+1
        DEC     FN              ;N:=N-1
        LDA     FN
        AND     FN+1
        CMP     #$FF            ;IF N <> $FFFF
        BNE     *+3             ;THEN MORE
        RTS                     ;ELSE DONE
FILLIT
        STX     $FFFF           ;D:=CHAR
        INC     FILLIT+1
        BNE     FILLLP
        INC     FILLIT+2        ;DEST:=DEST+1
        JMP     FILLLP          ;AND LOOP
;OUTPUT$MSG
OUTMSG
        ASL     A               ;MULT # BU TWO
        TAX                     ;MAKE INDEX
        LDA     ERMTBL,X
        LDY     ERMTBL+1,X      ;GET ADDRESS
        JSR     PRINT           ;PRINT MESSAGE
        LDA     CURLIN
        ORA     CURLIN+1
        BEQ     CCRLF           ;IF CURRENTLINE=0 THEN
        LDA     #<INLMSG
        LDY     #>INLMSG
        JSR     PRINT           ;PRINT(' IN LINE ')
        LDA     CURLIN
        LDY     CURLIN+1
        CLC                     ;ADD ONE TO NUMBER
        ADC     #1
        BCC     *+3
        INY
        JSR     PRNDEC
CCRLF
        LDA     #CR
        JSR     PRNCON
        LDA     #LF
        JMP     PRNCON
;PRINT AY AS DECIMAL NUMBER
DECOUT  ;ACCUM
        .RES    2
LZFLAG  ;LEADING ZERO FLAG
        .RES    1
PRNDEC
        STA     DECOUT
        STY     DECOUT+1        ;SAVE NUMBER
        LDA     #FALSE
        STA     LZFLAG          ;CLEAR LEADING ZERO FLAG
        LDX     #'0'-1          ;SET DIGIT TO '0' MINUS ONE
        SEC
P10000
        LDA     DECOUT
        SBC     #(10000*256/256) & $FF
        STA     DECOUT
        LDA     DECOUT+1
        SBC     #10000/256
        STA     DECOUT+1        ;SUBTRACT 10000
        INX                     ;BUMP DIGIT
        BCS     P10000          ;LOOP IF NO BORROW
        LDA     DECOUT
        ADC     #(10000*256/256) & $FF
        STA     DECOUT
        LDA     DECOUT+1
        ADC     #10000/256
        STA     DECOUT+1        ;ADD 10000 BACK IN
        JSR     DIGOUT          ;SEND DIGIT
P1000
        LDA     DECOUT
        SBC     #(1000*256/256) & $FF
        STA     DECOUT
        LDA     DECOUT+1
        SBC     #1000/256
        STA     DECOUT+1        ;SUBTRACT 1000
        INX
        BCS     P1000           ;LOOP IF NO BORROW
        LDA     DECOUT
        ADC     #(1000*256/256) & $FF
        STA     DECOUT
        LDA     DECOUT+1
        ADC     #1000/256
        STA     DECOUT+1        ;ADD 1000 BACK IN
        JSR     DIGOUT          ;SEND DIGIT
P100
        LDA     DECOUT
        SBC     #100
        STA     DECOUT
        LDA     DECOUT+1
        SBC     #0
        STA     DECOUT+1        ;SUBTRACT 100
        INX                     ;BUMP DIGIT
        BCS     P100            ;LOOP IF NO BORROW
        LDA     DECOUT
        ADC     #100
        STA     DECOUT          ;ADD 100 BACK IN
        JSR     DIGOUT          ;SEND DIGIT
P10
        LDA     DECOUT
        SBC     #10
        STA     DECOUT          ;SUBTRACT 10
        INX                     ;BUMP DIGIT
        BCS     P10             ;LOOP IF NO BORROW
        ADC     #10
        STA     DECOUT          ;ADD 10 BACK IN
        JSR     DIGOUT          ;OUTPUT DIGIT
        LDA     #'0'
        ORA     DECOUT          ;MAKE ONE'S
        JMP     PRNCON          ;PRINT ONE'S ALWAYS
;SEND A SINGLE DIGIT IN X AND HANDLE LEADING ZEROS
DIGOUT
        LDA     #' '            ;PRESET FOR LEADING ZERO
        BIT     LZFLAG          ;TEST FLAG
        BMI     D2              ;BRANCH IF SET
        CPX     #'0'            ;SEE IF DIGIT = 0
        BEQ     D3              ;BRANCH IF IS
        LDA     #TRUE
        STA     LZFLAG          ;SET FLAG
D2
        TXA                     ;GET CHAR
D3
        JSR     PRNCON          ;PRINT CHAR
        LDX     #'0'-1          ;PRESET DIGIT
        SEC
        RTS
;ERROR(AY)
ERROR
        PHA
        JSR     CCRLF
        LDA     #<ERRMSG
        LDY     #>ERRMSG
        JSR     PRINT
        PLA
        JSR     OUTMSG
        JMP     BOOT
;WARNING(AY)
WARNNG
        PHA
        JSR     CCRLF
        LDA     #<WRNMSG
        LDY     #>WRNMSG
        JSR     PRINT           ;PRINT('WARNING ')
        PLA
        JMP     OUTMSG
;STACK MANIPULATION ROUTINES
;RA$NEGATIVE
RANEG
        LDY     #1
        LDA     (RA),Y          ;GET BRA(1)
        BMI     *+5
        LDA     #FALSE
        RTS
        LDA     #TRUE
        RTS
;RB$NEGATIVE
RBNEG
        LDY     #1
        LDA     (RB),Y          ;GET BRB(1)
        BMI     *+5
        LDA     #FALSE
        RTS
        LDA     #TRUE
        RTS
;FLAG$STRING$ADDR(A)
FLSTRA
        LDY     #2
        AND     #1              ;ONLY DO LSB
        STA     (RA),Y          ;BRA(2)=A
        RTS
;MOVE$RB$RA
MVRBRA
        LDY     #3
MVBAL
        LDA     (RB),Y
        STA     (RA),Y
        DEY
        BPL     MVBAL
        RTS
;MOVE$RA$RB
MVRARB
        LDY     #3
MVABL
        LDA     (RA),Y
        STA     (RB),Y
        DEY
        BPL     MVABL
        RTS
;RA$ZERO
RAZER
        LDY     #0
        LDA     (RA),Y          ;GET BRAZ
        BEQ     *+5
        LDA     #FALSE
        RTS
        LDA     #TRUE
        RTS
;RB$ZERO
RBZER
        LDY     #0
        LDA     (RB),Y          ;GET BRBZ
        BEQ     *+5
        LDA     #FALSE
        RTS
        LDA     #TRUE
        RTS
;RA$ZERO$ADDRESS
RAZERA
        LDY     #0
        LDA     (RA),Y          ;GET LOW(ARA)
        INY
        ORA     (RA),Y          ;OR WITH HIGH
        BEQ     *+5
        LDA     #FALSE
        RTS
        LDA     #TRUE
        RTS
;RB$ZERO$ADDRESS
RBZERA
        LDY     #0
        LDA     (RB),Y          ;GET LOW(ARB)
        INY
        ORA     (RB),Y          ;OR WITH HIGH
        BEQ     *+5
        LDA     #FALSE
        RTS
        LDA     #TRUE
        RTS
;FLIP
FLIP
        LDY     #3
FLIPLP
        LDA     (RA),Y
        TAX
        LDA     (RB),Y
        STA     (RA),Y
        TXA
        STA     (RB),Y
        DEY
        BPL     FLIPLP
        RTS
;POP$STACK
POPSTK
        LDA     RB
        LDY     RB+1
        STA     RA
        STY     RA+1            ;RA:=RB
        SEC
        SBC     #4
        STA     RB
        BCS     *+4
        DEC     RB+1            ;RB:=RB-4
        CMP     SB
        LDA     RB+1
        SBC     SB+1
        BCC     *+3             ;IF RB<SB THEN
        RTS
        SEC                     ;RB:=ST-4
        LDA     ST
        SBC     #4
        STA     RB
        LDY     ST+1
        BCS     *+3
        DEY
        STY     RB+1
        RTS
;STEP$INS$CNT
STINCN
        INC     RC              ;RC:=RC+1
        BNE     *+4
        INC     RC+1
        RTS
;PUSH$STACK
PSHSTK
        LDA     RA
        LDY     RA+1
        STA     RB
        STY     RB+1            ;RB:=RA
        CLC
        ADC     #4
        STA     RA
        BCC     *+5
        INY
        STY     RA+1            ;RA:=RA+4
        CMP     ST
        TYA
        SBC     ST+1            ;IF (RA:=RA+4) >= ST
        BCS     *+3             ;THEN
        RTS
        LDA     SB
        LDY     SB+1
        STA     RA
        STY     RA+1            ;RA:=SB
        RTS
;IN$FSA(LOCATION)
;RETURNS TRUE IF LOCATION IS IN FSA
INFSA
        STA     INFSAT
        LDA     ST
        CMP     INFSAT
        STY     INFSAT          ;SAVE Y
        LDA     ST+1
        SBC     INFSAT          ;IF AY>ST
        LDA     #TRUE
        BCC     *+4             ;THEN RETURN:=TRUE
        LDA     #FALSE          ;ELSE RETURN:=FALSE
        RTS
;SET$DATA$ADDR(PTR)
SEDAAD
        STA     SEDPTR          ;SAVE PTR
        STY     SEDPTR+1
        LDY     #0              ;SET A
        LDA     (SEDPTR),Y
        TAX                     ;SAVE LOW IN X
        INY
        LDA     (SEDPTR),Y
        TAY
        TXA
        JSR     INFSA           ;IF IN$FSA
        BNE     E12             ;THEN DONE
        LDY     #0
        LDA     (SEDPTR),Y      ;GET LOW
        STA     SEDAAT          ;AND SAVE
        INY
        LDA     (SEDPTR),Y
        DEY
        ASL     SEDAAT
        ROL     A
        ASL     SEDAAT
        ROL     A               ;TIMES4(A)
        TAX                     ;SAVE HIGH IN X
        CLC
        LDA     MPR
        ADC     SEDAAT
        STA     (SEDPTR),Y
        INY
        TXA
        ADC     MPR+1
        STA     (SEDPTR),Y      ;A:=MPR+TIMES4(A)
E12
        RTS
;LOAD$RA
;(ARA) TO (RA)
LOADRA
        LDA     RA
        LDY     RA+1
        JSR     SEDAAD          ;SET$DATA$ADDR(RA)
        JSR     ARA
        STA     MFROM
        STY     MFROM+1
        LDA     RA
        LDY     RA+1
        STA     MTO
        STY     MTO+1
        JMP     MOVE4           ;MOVE4(FROM,TO)
;CALCULATE ARB
ARB
        LDY     #0
        LDA     (RB),Y
        PHA
        INY
        LDA     (RB),Y
        TAY
        PLA
        RTS
;CALCULATE ARA
ARA
        LDY     #0
        LDA     (RA),Y
        PHA
        INY
        LDA     (RA),Y
        TAY
        PLA
        RTS
;CALCULATE TWOBYTEOPRND (ARC)
ARC
        LDY     #0
        LDA     (RC),Y
        PHA
        INY
        LDA     (RC),Y
        TAY
        PLA
        RTS
;CONSOLE OUTPUT ROUTINES
;DUMP$PRINT$BUFF
DMPPBF
        JSR     CRLF            ;DO CR AND LF
;CLEAR$PRINT$BUFF
CLRPBF
        LDA     #0
        STA     PRBUFF          ;PRINTBUFFR:=0
        RTS
;NUMERIC$OUT
NUMOUT
        JSR     FLODRA          ;PRIM:=(RA)
        JSR     FLTOUT          ;CONVERT TO ASCII
        LDA     PRNWRK          ;GET FIRST
        CMP     #' '            ;SEE IF SPACE
        BNE     L091            ;SKIP AHEAD IF NOT
        LDX     #255
L090
        INX
        LDA     PRNWRK+1,X      ;MOVE EVERYTHING
        STA     PRNWRK,X        ;AHEAD ONE CHAR
        BNE     L090            ;UNTIL HIT ZERO
L091
        LDX     #0
L042
        INX
        LDA     NUOWRK,X        ;CALC STRING LENGTH
        BNE     L042
        STX     NUOWRK          ;MAKE LENGTH FIRST BYTE
        LDA     #' '
        STA     NUOWRK,X        ;INSERT A BLANK IN PLACE OF $00
        LDY     #0
        LDA     #<NUOWRK
        STA     (RA),Y
        INY
        LDA     #>NUOWRK
        STA     (RA),Y          ;ARA:=.NUMBEROUTWORKAREA
        RTS
;WRITE$TO$CONSOLE
WTCI    ;INDEX
        .RES    1
WRTTCN
        LDY     #0
        LDA     (RA),Y
        STA     WTCHLD
        INY
        LDA     (RA),Y
        STA     WTCHLD+1        ;HOLD:=ARA
        ORA     WTCHLD
        BEQ     E13             ;IF HOLD=0 THEN
        STY     WTCI            ;INDEX:=1
L048
        LDY     #0
        LDA     (WTCHLD),Y      ;A:=H(0)
        CMP     WTCI
        BCC     E13             ;IF INDEX > H(0) THEN
        LDY     WTCI
        LDA     (WTCHLD),Y
        JSR     PRNCHR          ;OUTPUT CHARACTER
        INC     PRBUFF          ;PRINTBUFFER:=PRINTBUFFER+1
        LDA     #131
        CMP     PRBUFF
        BCS     *+5             ;IF PRINTBUFFER <= PRINTBUFFEND THEN
        JSR     DMPPBF          ;ELSE DUMP$PRINT$BUFF
        INC     WTCI
        JMP     L048            ;LOOP
E13
        RTS
;FIXED POINT ROUTINES
;MULTIPLY PRIMARY FIXED POINT ACCUM BY (AY) AND PUT RESULT IN AY
MXAP
        STA     SX
        STY     SX+1            ;SX:=AY
        LDA     #0              ;CLEAR HIGH
        STA     PX+2
        STA     PX+3
        LDY     #16             ;DO 16 BITS
MXAPL
        LDA     PX
        LSR     A               ;TEST LSB
        BCC     MXAPNA          ;DON'T ADD IF CLEAR
        CLC
        LDA     PX+2
        ADC     SX
        STA     PX+2
        LDA     PX+3
        ADC     SX+1
        STA     PX+3
MXAPNA
        ROR     PX+3            ;SHIFT ALL RIGHT
        ROR     PX+2
        ROR     PX+1
        ROR     PX
        DEY
        BNE     MXAPL           ;LOOP IF MORE
        LDA     PX
        LDY     PX+1
        RTS
;FLOATING POINT INTERFACE ROUTINES
;ROUND$CONV$BIN
RNCBRA
        JSR     FLODRA          ;GET @ RA
        LDA     #<HALF
        LDY     #>HALF
        JSR     APA             ;ADD .5
        JSR     FSTRRA          ;STORE @ RA
        JSR     CHKOVR          ;CHECK FOR OVERFLOW AND FAAL THROUGH
;CONV$TO$BIN$ADDR
CNVBAD
        JSR     CNVBRA          ;CONV$TO$BINARY(RA)
        LDY     #3
        LDA     (RA),Y
        LDY     #0
        STA     (RA),Y          ;BRA(0):=BRA(3)
        LDY     #2
        LDA     (RA),Y
        DEY
        STA     (RA),Y          ;BRA(1):=BRA(2)
        RTS
;CONV$TO$BINARY(RB)
CNVBRB
        LDA     RB
        LDY     RB+1
        JMP     CNVBIN
;CONV$TO$BINARY(RA)
CNVBRA
        LDA     RA
        LDY     RA+1
;CONV$TO$BUNARY(AY)
;CONVERTS FLOATING POINT NUMBER @ AY TO 24 BIT TWO'S COMPLEMENT NUMBER AND
;RETURNS VALUE TO @ AY
CNVBIN
        JSR     LPA             ;PRIM:=(AY)
        JSR     FXU             ;FIX IT
        LDA     #0
        STA     PR
        STA     PS              ;CLEAR ROUNDING AND SIGN
        LDX     #0              ;PRESET FOR POSITIVE
        BIT     PM              ;NOW SIGN EXTEND INTO PE
        BPL     *+3
        DEX
        STX     PE
        LDA     ME              ;AY SAVED IN ME AND ME+1
        LDY     ME+1
        JSR     STP             ;STORE IT
        LDY     #1              ;REDO MANTISSA
        LDA     PM
        STA     (SL),Y          ;SO SIGN IS OK
        RTS
;CONV$TO$FP(RB)
CNVFRB
        LDA     RB
        LDY     RB+1
        JMP     CNVFLT
;CONV$TO$FP(RA)
CNVFRA
        LDA     RA
        LDY     RA+1
;CONV$TO$FP(AY)
;CONVERTS 24 BIT INTEGER @ AY TO FLOATING POINT NUMBER AND STORE @ AY
CNVFLT
        JSR     LPA             ;PRIM:=(AY)
        LDY     #1
        LDA     (ME),Y          ;REDO FIRST MANTISSA
        STA     PM              ;SO SIGN BIT IS OK
        LDX     #24+128
        STX     PE              ;SET EXPONENT
        LDA     PM
        EOR     #$FF
        ROL     A               ;SET CARRY TO -SIGN
        LDA     #0
        STA     PR
        STA     PS              ;CLEAR SIGN AND ROUNDING
        JSR     CYENT           ;NORMALIZE
        LDA     ME
        LDY     ME+1            ;AY SAVED IN ME AND ME+1
        JMP     STP             ;STORE IT
;FLOAT$ADDR(AY)
FLTADR
        JSR     FLT16           ;FLOAT AY
        JMP     FSTRRA          ;AND STORE
;COMPARE$FP
;RETURNS 1 IF (RB)<(RA); 2 IF (RB)>(RA); 3 IF (RB)=(RA)
COMPFP
        JSR     FLODRA          ;LOAD (RA)
        LDA     RB
        LDY     RB+1
        JSR     SPA             ;PRIM:=(RB)-(RA)
        JSR     FSTRRA          ;STORE @ (RA)
        JSR     RAZER
        BEQ     L049            ;IF NOT RA$ZERO THEN
        JSR     POPSTK          ;ELSE
        LDA     #3
        RTS
L049
        JSR     RANEG
        BEQ     L050            ;IF NOT RA$NEGATIVE THEN
        JSR     POPSTK
        LDA     #1
        RTS
L050
        JSR     POPSTK
        LDA     #2
        RTS
;LOAD (RA) INTO PRIMARY FLOATING POINT ACCUM
FLODRA
        LDA     RA
        LDY     RA+1
        JMP     LPA             ;PRIM:=(RA)
;STORE PRIMARY FLOATING POINT ACCUM AT (RA)
FSTRRA
        LDA     RA
        LDY     RA+1
        JMP     STP
;LOAD (RB) INTO PRIMARY FLOATING POINT ACCUM
FLODRB
        LDA     RB
        LDY     RB+1
        JMP     LPA
;STORE PRIMARY FLOATING POINT ACCUM AT (RB)
FSTRRB
        LDA     RB
        LDY     RB+1
        JMP     STP
;CHECK$OVERFLOW
CHKOVR
        BIT     OVERFL
        BPL     E24             ;IF NOT OVERFLOW THEN
        LDA     #26             ;ELSE SEND OVERFLOW WARNING
        JSR     WARNNG
        LDY     #3
L025
        LDA     MAXNUM,Y
        STA     (RA),Y
        DEY
        BPL     L025            ;(RA):=MAXNUM
        INY
        STY     OVERFL          ;OVERFLOW:=FALSE
        STY     PR
        STY     PC
        STY     PS              ;CLEAR SIGN, CARRY, AND ROUNDING
E24
        RTS
;FLOATING POINT ROUTINES
;CONSTANTS
MAXNUM
        .BYTE   $FF,$7F,$FF,$FF
HALF
        .BYTE   $80,0,0,0       ;(0.5)
FIVE9S
        .BYTE   $91,$43,$4F,$F8 ;(99999.9375)
SIX9S
        .BYTE   $94,$74,$23,$F7 ;(999999.4375)
MILLON
        .BYTE   $94,$74,$24,0   ;(1000000)
TEN
        .BYTE   $84,$20,0,0     ;(10)
ONE
        .BYTE   $81,0,0,0       ;(1)
PI2
        .BYTE   $81,$49,$0F,$DB ;(PI/2)
SQR2
        .BYTE   $81,$35,$04,$F3 ;(SQR(2))
MHALF
        .BYTE   $80,$80,0,0     ;(-0.5)
LN2
        .BYTE   $80,$31,$72,$18 ;(LN(2))
LN2INV
        .BYTE   $81,$38,$AA,$3B ;(1/LN(2))
SQR2D2
        .BYTE   $80,$35,$04,$F3 ;(SQR(2)/2)
TWOPI
        .BYTE   $83,$49,$0F,$DB ;(2*PI)
QUARTR
        .BYTE   $7F,0,0,0       ;(0.25)
MONE
        .BYTE   $81,$80,0,0     ;(-1)
RNDWRK
        .BYTE   $80,$3F,$C7,$52
RNDMUL
        .BYTE   $98,$35,$44,$7A
RNDADD
        .BYTE   $68,$28,$B1,$46
;CONVERSION DATA TABLES
CNVTBL
        .BYTE   $FE,$79,$60     ;-100,000
        .BYTE   0,$27,$10       ;10,000
        .BYTE   $FF,$FC,$18     ;-1000
        .BYTE   0,0,100         ;100
        .BYTE   $FF,$FF,$F6     ;-10
        .BYTE   0,0,1           ;1
;SERIES EVALUATION CHAINS
LOGCHN
        .BYTE   2
        .BYTE   $80,$19,$56,$AA
        .BYTE   $80,$76,$22,$F0
        .BYTE   $82,$38,$AA,$40
EXPCHN
        .BYTE   7
        .BYTE   $74,$94,$2E,$40
        .BYTE   $77,$2E,$4F,$70
        .BYTE   $7A,$88,$02,$6E
        .BYTE   $7C,$2A,$A0,$E6
        .BYTE   $7E,$AA,$AA,$50
        .BYTE   $7F,$7F,$FF,$FF
        .BYTE   $81,$80,0,0
        .BYTE   $81,0,0,0
SINCHN
        .BYTE   4
        .BYTE   $86,$1E,$D7,$BA
        .BYTE   $87,$99,$26,$64
        .BYTE   $87,$23,$34,$58
        .BYTE   $86,$A5,$5D,$E0
        .BYTE   $83,$49,$0F,$DA
ATNCHN
        .BYTE   8
        .BYTE   $78,$3B,$D7,$4A
        .BYTE   $7B,$84,$6E,$02
        .BYTE   $7C,$2F,$C1,$FE
        .BYTE   $7D,$9A,$31,$74
        .BYTE   $7D,$5A,$3D,$84
        .BYTE   $7E,$91,$7F,$C8
        .BYTE   $7E,$4C,$BB,$E4
        .BYTE   $7F,$AA,$AA,$6C
        .BYTE   $81,0,0,0
;FLOAT 8 BIT VALUE IN A AS POSITIVE NUMBER
FLT8
        LDY     #0              ;CLEAR MS BYTE
;FLOAT 16 BIT VALUE IN AY AS POSITIVE NUMBER
FLT16
        STA     PM+1
        STY     PM              ;SET MANTISSA
        LDX     #16+128         ;EXPONENT:=16
        SEC
        STX     PE
        LDA     #0
        STA     PM+2
        STA     PR
        STA     PS              ;CLEAR SIGN, MANTISSA (HIGH), AND ROUNDING
        JMP     NORMLZ          ;GO NORMALIZE
;TWO'S COMPLEMENT OF PRIMARY ACCUMULATOR
TCP
        LDA     PS              ;COMPLEMENT SIGN
        EOR     #$FF
        STA     PS
TCPNS
        LDA     PM              ;THEN MANTISSA
        EOR     #$FF
        STA     PM
        LDA     PM+1
        EOR     #$FF
        STA     PM+1
        LDA     PM+2
        EOR     #$FF
        STA     PM+2
        LDA     PR              ;THEN ROUNDING BYTE
        EOR     #$FF
        STA     PR
        INC     PR              ;NOW ADD ONE TO ALL
        BNE     E1
;INCREMENT PRIMARY MANTISSA
INP
        INC     PM+2            ;BUMP MANTISSA
        BNE     E1
        INC     PM+1
        BNE     E1
        INC     PM
E1
        RTS
;EXTRACT SIGN OF PRIMARY ACCUMULATOR AND TEST PRIMARY ACCUMULATOR FOR ZERO.
;IF PRIMARY ZERO THEN Z=1 AND A=0 ELSE IF POSITIVE THEN Z=0 AND A=1 ELSE IF
;NEGATIVE THEN Z=0 AND A= $FF
ESP
        LDA     PE              ;GET EXPONENT
        BEQ     E2              ;EXIT IF ZERO
ESPS
        LDA     PS              ;GET SIGN
ESPSRL
        ROL     A               ;MOVE TO CARRY
        LDA     #$FF            ;PRESET FOR MINUS
        BCS     E2              ;BRANCH IF IS
        LDA     #1              ;ELSE POSITIVE
E2
        RTS
;STORE PRIMARY ACCUMULATOR AT MEMORY AT (AY)
STP
        STA     SL
        STY     SL+1            ;SL:=AY
;STORE PRIMARY ACCUMULATOR AT (SL) AFTER ROUNDING
SPM
        JSR     RNP             ;ROUND IT
        LDY     #3
        LDA     PM+2
        STA     (SL),Y
        DEY
        LDA     PM+1
        STA     (SL),Y
        DEY
        LDA     PS              ;NOW OR SIGN WITH PM
        ORA     #$7F
        AND     PM
        STA     (SL),Y
        DEY
        LDA     PE
        STA     (SL),Y
        STY     PR              ;CLEAR ROUNDING
        RTS
;LOAD PRIMARY ACCUMULATOR FROM MEMORY AT (AY)
LPA
        STA     ME
        STY     ME+1            ;ME:=AY
;LOAD PRIMARY ACCUMULATOR FROM MEMORY AT (ME)
LPM
        LDY     #3              ;FOUR-ONE BYTES
        LDA     (ME),Y
        STA     PM+2
        DEY
        LDA     (ME),Y
        STA     PM+1
        DEY
        LDA     (ME),Y
        STA     PS
        ORA     #$80
        STA     PM              ;SET IMPLIED 1
        DEY
        LDA     (ME),Y
        STA     PE
        STY     PR              ;ROUNDING:=0
        RTS
;LOAD SECONDARY ACCUMULATOR FROM (AY)
LSA
        STA     ME
        STY     ME+1            ;ME:=AY
;LOAD SECONDARY ACCUMULATOR FROM (ME)
LSM
        LDY     #3
        LDA     (ME),Y
        STA     SM+2
        DEY
        LDA     (ME),Y
        STA     SM+1
        DEY
        LDA     (ME),Y
        STA     SS
        EOR     PS
        STA     SGNCMP
        LDA     SS
        ORA     #$80
        STA     SM
        DEY
        LDA     (ME),Y
        STA     SE
        RTS
;TRANSFER SECONDARY ACCUMULATOR TO PRIMARY AND CLEAR ROUNDING
TSP
        LDA     SS
        STA     PS
        LDA     SM+2
        STA     PM+2
        LDA     SM+1
        STA     PM+1
        LDA     SM
        STA     PM
        LDA     SE
        STA     PE
        LDA     #0
        STA     PR
        RTS
;TRANSFER PRIMARY ACCUMULATOR TO SECONDARY AFTER ROUNDING AND CLEAR ROUNDING
TPS
        JSR     RNP             ;ROUND PRIMARY
        LDA     PS
        STA     SS
        LDA     PM+2
        STA     SM+2
        LDA     PM+1
        STA     SM+1
        LDA     PM
        STA     SM
        LDA     PE
        STA     SE
        LDA     #0
        STA     PR
        RTS
;SUBTRACT PRIMARY ACCUMULATOR FROM MEMORY (AY)
;P:=(AY)-P
SPA
        JSR     LSA             ;MOVE MEMORY TO SECONDARY
;SUBTRACT PRIMARY ACCUMULATOR FROM SECONDARY
;P:=S-P
SPS
        LDA     PS              ;COMPLEMENT PRIMARY SIGN
        EOR     #$FF
        STA     PS
        EOR     SS
        STA     SGNCMP          ;SET COMPARE STATUS
        JMP     APS             ;AND ADD
;ALTERNATE ENTRY TO SHIFT ROUTINE
SXRYAL
        PHA                     ;SAVE A
        JMP     SXRYE
;SHIFT PRODUCT STAGING AREA RIGHT ONE BYTE
SQR8
        LDX     #QE
;SHIFT NUMBER POINTED TO BY X RIGHT A TIMES. MUST BE AT LEAST 8 TIMES TO START
;AS ENTIRE BYTES ARE SHIFTED
SXR8
        LDY     3,X             ;MOVE WHOLE BYTES
        STY     PR
        LDY     2,X
        STY     3,X
        LDY     1,X
        STY     2,X
        LDY     PC
        STY     1,X
;SHIFT NUMBER POINTED TO BY X RIGHT A TIMES
SXRA
        ADC     #8              ;ADD 8
        BMI     SXR8            ;LOOP IF AT LEAST A BYTE
        BEQ     SXR8
        SBC     #8              ;DROP BACK TO BITS
        TAY                     ;MOVE TO Y
        LDA     PR              ;GET ROUNDING
        BCS     E4              ;DONE IF CARRY
SXRY
        PHA
        LDA     1,X
        AND     #$80            ;CLEAR ALL BUT MSB
        LSR     1,X             ;SHIFT RIGHT AND CLEAR CY
        ORA     1,X
        STA     1,X
SXRYE
        ROR     2,X
        ROR     3,X
        PLA
        ROR     A
        INY
        BNE     SXRY            ;LOOP IF MORE
E4
        CLC                     ;ALWAYS CLEAR CARRY
        RTS
;COMPARE NUMBER AT (AY) TO PRIMARY ACCUMULATOR
CPA
        STA     SL
        STY     SL+1            ;SL:=AY
;COMPARE PRIMARY ACCUMULATOR TO NUMBER AT (SL)
CPM
        LDY     #0
        LDA     (SL),Y          ;GET EXPONENT
        INY
        TAX                     ;TEST
        BNE     *+5             ;CONTINUE IF N <> 0
        JMP     ESP             ;ELSE GO EXTRACT PRIMARY SIGN
        LDA     (SL),Y          ;GET MANTISSA
        EOR     PS              ;TEST SIGN PART
        BPL     *+5             ;BRANCH IF SAME
        JMP     ESPS            ;TEST PRIMARY
        CPX     PE              ;COMPARE EXPONENTS
        BNE     CPMSGN          ;TRY SIGNS IF DIFFERENT
        LDA     (SL),Y          ;GET M AGAIN
        ORA     #$80            ;SET HIDDEN BIT
        CMP     PM              ;COMPARE TO PM
        BNE     CPMSGN
        INY
        LDA     (SL),Y          ;GET NEXT
        CMP     PM+1            ;COMPARE
        BNE     CPMSGN
        INY
        LDA     #127
        CMP     PR              ;SUBTRACT
        LDA     (SL),Y
        SBC     PM+2
        BNE     CPMSGN
        RTS
CPMSGN
        LDA     PS              ;GET SIGN
        BCC     *+4             ;OK IF BORROW
        EOR     #$FF            ;ELSE COMPLEMENT
        JMP     ESPSRL          ;NOW TEST
;ADD SINGLE BYTE IN A INTO PRIMARY ACCUMULATOR
ADP
        PHA                     ;SAVE DIGIT
        LDA     #<T0
        LDY     #>T0
        JSR     STP             ;T:=P
        PLA                     ;GET DIGIT
        STA     PM
        EOR     #$FF
        ROL     A
        LDX     #8+128
        STX     PE              ;EXP:=8
        LDA     #0
        STA     PM+1
        STA     PM+2
        STA     PR
        STA     PS
        JSR     CYENT           ;NORMALIZE
        LDA     #<T0
        LDY     #>T0
        JMP     APA             ;P:=P+A
;ADD PRIMARY ACCUMULATOR TO MEMORY AT (AY)
;P:=(AY)+P
APA
        JSR     LSA             ;LOAD SECONDARY WITH MEMORY
;ADD PRIMARY ACCUMULATOR TO SECONDARY AND PUT RESULT IN PRIMARY
;P:=P+S
APS
        LDA     PE              ;GET PRIM EXP
        BNE     *+5             ;USE IF P <> 0
        JMP     TSP             ;ELSE JUST MOVE S TO P
        LDX     PR              ;GET ROUNDING
        STX     SAVPR           ;SAVE IT
        LDX     #SE             ;POINT TO SECONDARY
        LDA     SE              ;GET SEC EXP
SPE
        TAY
        BNE     E5S             ;DONE IF ZERO
        JMP     E5
E5S:
        SEC
        SBC     PE              ;A:=SE-PE
        BEQ     ENDALG          ;BRANCH IF ALIGNED
        BCC     PEGTSE          ;BRANCH IF PE>SE
        STY     PE              ;MOVE SE TO PE
        LDY     SS              ;GET SEC SIGN
        STY     PS              ;MOVE TO PRIM SIGN
        EOR     #$FF            ;COMPLEMENT DIFFERENCE
        ADC     #0
        LDY     #0              ;CLEAR PRIM ROUNDING
        STY     SAVPR           ;SAVE LOCATION
        LDX     #PE             ;DO PRIMARY RATHER THAN SECONDARY
        JMP     DOALGN          ;GO ALIGN
PEGTSE
        LDY     #0              ;CLEAR REAL ROUNDING
        STY     PR
DOALGN
        CMP     #$F9            ;CHECK EXP DIFFERENCE
        BPL     ALGNP           ;BRANCH IF LESS
        JSR     SXRA            ;ELSE SHIFT RIGHT
        JMP     ENDALG          ;THEN GO AHEAD
ALGNP
        TAY                     ;GET DIFFERENCE
        LDA     PR
        LSR     1,X             ;CLEAR MSB OF FIRST MANTISSA
        JSR     SXRYAL          ;GO SHIFT
ENDALG
        BIT     SGNCMP          ;TEST SIGN COMPARE
        BPL     DOADD           ;GO DO IF SIGNS SAME
;SUBTRACT TWO VALUES
        LDY     #PE
        CPX     #SE
        BEQ     *+4             ;JUMP IF SAME
        LDY     #SE             ;ELSE POINT TO SE
        SEC
        EOR     #$FF
        ADC     SAVPR
        STA     PR
        LDA     3,Y             ;START SUBTRACTION
        SBC     3,X
        STA     PM+2
        LDA     2,Y
        SBC     2,X
        STA     PM+1
        LDA     1,Y
        SBC     1,X
        STA     PM
CYENT
        BCS     NORMLZ          ;IF NO BORROW OK
        JSR     TCP             ;ELSE COMPLEMENT
NORMLZ
        LDY     #0              ;CLEAR A Y AND CY
        TYA
        CLC
SPLL
        LDX     PM              ;GET FIRST PART OF MANTISSA
        BNE     SHFBTL          ;GO SHIFT BITS IF NOT ZERO
        LDX     PM+1            ;ELSE SHIFT WHOLE BYTES
        STX     PM
        LDX     PM+2
        STX     PM+1
        LDX     PR
        STX     PM+2
        STY     PR              ;CLEAR ROUNDING
        ADC     #8              ;BUMP EXPONENT
        CMP     #32             ;SEE IF TOO SMALL
        BNE     SPLL            ;BRANCH IF MORE
ZRORSL
        LDA     #0              ;UNDERFLOW THEREFORE = 0
        STA     PE
        STA     PS
E5
        RTS
;ADD TWO VALUES
DOADD
        ADC     SAVPR
        STA     PR
        LDA     PM+2
        ADC     SM+2
        STA     PM+2
        LDA     PM+1
        ADC     SM+1
        STA     PM+1
        LDA     PM
        ADC     SM
        STA     PM
        JMP     CHKCRY          ;GO SEE IF CARRY AND THUS NORMALIZATION
BITSL
        ADC     #1              ;BUMP EXPONENT
        ASL     PR
        ROL     PM+2
        ROL     PM+1
        ROL     PM
SHFBTL
        BPL     BITSL           ;LOOP IF MSB NOT 1
        SEC
        SBC     PE
        BCS     ZRORSL          ;ZERO IF UNDERFLOW
        EOR     #$FF            ;TWO'S COMPLEMENT
        ADC     #1
        STA     PE
CHKCRY
        BCC     E6              ;DONE IF NO CARRY
ADJPE
        INC     PE              ;BUMP EXPONENT
        BEQ     APSOVF          ;OVERFLOW IF ZERO
        ROR     PM              ;SHIFT ALL RIGHT
        ROR     PM+1
        ROR     PM+2
        ROR     PR
E6
        RTS
;ADD OVERFLOW TRAP
APSOVF
        LDA     #TRUE
        STA     OVERFL          ;OVERFLOW:=TRUE
        RTS
;NEGATE PRIMARY ACCUMULATOR
NEG
        LDA     PE
        BEQ     E10             ;DONE IF ZERO
        LDA     PS
        EOR     #$FF
        STA     PS              ;COMPLEMENT SIGN
E10
        RTS
;ROUND PRIMARY ACCUMULATOR
RNP
        LDA     PE              ;GET EXP
        BEQ     E3              ;DONE IF ZERO
        ASL     PR              ;TEST MSB OF ROUNDING
        BCC     E3              ;DONE IF ZERO
        JSR     INP             ;ELSE BUMP PRIM
        BNE     E3              ;DONE IF NOT ZERO
        JSR     ADJPE           ;ADJUST EXPONENT
E3
        RTS
;SETUP MULTIPLICATION AND DIVISION
SUPMD
        LDA     SE              ;IF S=0
SUPMDX
        BEQ     MDZRO           ;THEN P:=0
        CLC
        ADC     PE              ;A:=PE+SE
        BCS     *+6             ;BRANCH IF CARRY
        BPL     MDZRO           ;BRANCH IF 0 TO 7F
        BMI     NZRO            ;BRANCH IF 80 TO FF
        BMI     MDZRO           ;CARRY SET AND 80 TO FF
;RESULT NOT NECESSARILY ZERO
NZRO
        CLC
        ADC     #$80
        STA     PE              ;PE:=PE+SE+$80
        BEQ     *+4
        LDA     SGNCMP          ;IF DIFFERENT SIGNS
        STA     PS              ;THEN P < 0
        RTS
;RESULT ZERO
MDZRO
        PLA
        PLA                     ;CLEAR STACK
        LDA     #0
        STA     PE              ;PE:=0
        STA     PS              ;P POSITIVE
        RTS
;MULTIPLY BY ONE BYTE
MULNZB
        BNE     *+5             ;IF BYTE <> 0 THEN DO SLOW
        JMP     SQR8            ;ELSE JUST SHIFT WHOLE BYTES
MULBYT
        LSR     A               ;LSB TO CY
        ORA     #%10000000      ;SET MSB FOR COUNTER
MULLPE
        TAY                     ;SAVE COUNTER IN Y
        BCC     SHONLY          ;SHIFT ONLY IF ZERO
        CLC                     ;ELSE Q:=Q+S
        LDA     QM+2
        ADC     SM+2
        STA     QM+2
        LDA     QM+1
        ADC     SM+1
        STA     QM+1
        LDA     QM
        ADC     SM
        STA     QM
SHONLY
        ROR     QM              ;SHIFT ALL RIGHT
        ROR     QM+1
        ROR     QM+2
        ROR     PR
        TYA                     ;GET COUNTER
        LSR     A               ;SHIFT IT
        BNE     MULLPE          ;LOOP IF MORE
        RTS
;MULTIPLY PRIMARY ACCUMULATOR BY 10 BY SHIFTING (FOR NUMBER CONVERSION)
MPT
        JSR     TPS             ;S:=P
        LDA     PE              ;IF P=0
        BEQ     E9              ;THEN DONE
        CLC
        ADC     #2              ;PE:=PE+2
        BCS     MPTOVF          ;ERROR
        LDX     #0
        STX     SGNCMP          ;CLEAR SIGN COMPARE
        JSR     SPE             ;SHIFT PRIMARY
        INC     PE              ;PE:=PE+1
        BEQ     MPTOVF          ;ERROR
E9
        RTS
MPTOVF
        LDA     #TRUE
        STA     OVERFL          ;OVERFLOW:=TRUE
        RTS
;MULTIPLY PRIMARY ACCUMULATOR BY MEMORY AT (AY)
;P:=P*(AY)
MPA
        JSR     LSA             ;LOAD MEMORY INTO SECONDARY
;MULTIPLY PRIMARY ACCUMULATOR BY SECONDARY
;P:=P*S
MPS
        LDA     PE              ;TEST PRIMARY
        BEQ     E8              ;DONE IF ZERO
        JSR     SUPMD           ;SET UP
        LDA     #0              ;Q:=0
        STA     QM
        STA     QM+1
        STA     QM+2
        LDA     PR              ;MULTIPLY BYTE AT A TIME
        JSR     MULNZB
        LDA     PM+2
        JSR     MULNZB
        LDA     PM+1
        JSR     MULNZB
        LDA     PM
        JSR     MULBYT
;TRANSFER QUOTIENT TO PRIMARY ACCUM AND NORMALIZE
TQP
        LDA     QM              ;P:=Q
        STA     PM
        LDA     QM+1
        STA     PM+1
        LDA     QM+2
        STA     PM+2
        JMP     NORMLZ          ;GO NORMALIZE
E8
        RTS
;DIVIDE PRIMARY ACCUMULATOR BY 10
DPT
        JSR     TPS             ;S:=P
        LDA     #0
        STA     SGNCMP          ;CLEAR SIGN COMPARISON
        LDA     #<TEN
        LDY     #>TEN
        JSR     LPA             ;P:=10
        JMP     DSP             ;P:=P/10
;DIVIDE MEMORY AT (AY) BY PRIMRY ACCUMULATOR
;P:=(AY)/P
DAP
        JSR     LSA             ;S:=(AY)
;DIVIDE SECONDARY ACCUMULATOR BY PRIMARY
DSP
        LDA     PE              ;IF P=0
        BEQ     DIVZRO          ;THEN ERROR
        LDA     #0
        SEC
        SBC     PE
        STA     PE              ;PE:=-PE
        JSR     SUPMD           ;SETUP EVERYTHING
        INC     PE
        BEQ     DIVOVF          ;OVERFLOW ERROR
        LDX     #$FD            ;X:=-3
        LDA     #1              ;SET BIT COUNTER TO 1
CMPSP
        LDY     SM              ;IF S<>P
        CPY     PM
        BNE     SNEP            ;THEN
        LDY     SM+1
        CPY     PM+1
        BNE     SNEP
        LDY     SM+2
        CPY     PM+2
SNEP
        PHP                     ;SAVE CARRY
        ROL     A               ;BUMP COUNTER
        BCC     MORE            ;BRANCH IF MORE
        INX                     ;ELSE BUMP INDEX
        STA     QM+2,X          ;SET NEXT BYTE
        BNE     *+6             ;BRANCH IF X<>0
        LDA     #64             ;DO LAST BYTE
        BNE     MORE
        BPL     DDONE           ;EXIT IF LAST
        LDA     #1              ;ELSE RESET BIT COUNTER
MORE
        PLP                     ;GET CARRY
        BCS     NOBRW           ;BRANCH IF NO BORROW
SHFS
        ASL     SM+2
        ROL     SM+1
        ROL     SM
        BCS     SNEP
        BMI     CMPSP
        BPL     SNEP
NOBRW
        TAY                     ;SAVE COUNTER IN Y
        LDA     #0
        SBC     PR              ;S:=S-P
        LDA     SM+2
        SBC     PM+2
        STA     SM+2
        LDA     SM+1
        SBC     PM+1
        STA     SM+1
        LDA     SM
        SBC     PM
        STA     SM
        TYA                     ;GET COUNT
        JMP     SHFS            ;AND LOOP
;EXIT FROM DIVISION
DDONE
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        STA     PR
        PLP
;NOW DO P:=Q AND NORMALIZE
        JMP     TQP
;DIVISION ERROR ROUTINES
DIVOVF
        LDA     #TRUE
        STA     OVERFL
        RTS
DIVZRO
        LDA     #TRUE
        STA     DVZERO
        RTS
;CONVERT FLOATING TO FIXED UNSIGNED
FXU
        LDA     PE
        BNE     ISNTZ
        STA     PM              ;IF P=0 CLEAR ALL
        STA     PM+1
        STA     PM+2
        TAY                     ;AND Y
        RTS
ISNTZ
        SEC
        SBC     #24+128
        BIT     PS              ;TEST SIGN
        BPL     ISPOST          ;IF POSITIVE
        TAX
        LDA     #255
        STA     PC              ;SET CARRY
        JSR     TCPNS           ;COMPLEMENT
        TXA
ISPOST
        LDX     #PE             ;POINT TO PRIM
        CMP     #$F9
        BPL     NOSHFR
        JSR     SXRA            ;SHIFT RIGHT
        STY     PC              ;CLEAR CARRY
        RTS
NOSHFR
        TAY                     ;SET COUNTER
        LDA     PS              ;SET MSB OF PM TO PS
        AND     #$80
        LSR     PM              ;SET CARRY
        ORA     PM
        STA     PM
        JSR     SXRYAL          ;GO SHIFT
        STY     PC              ;CLEAR CARRY
        RTS
;CONVERT FLOATING TO SIGNED 24 BIT INTEGER
INT
        LDA     PE
        CMP     #24+128
        BCS     E7              ;DONE IF EXACT OR TOO BIG
        JSR     FXU             ;FIX IT
        STY     PR              ;CLEAR ROUNDING
        LDA     PS              ;GET SIGN
        STY     PS              ;THEN CLEAR
        EOR     #$80
        ROL     A               ;SET CARRY BY SIGN
        LDA     #24+128         ;SET EXPONENT
        STA     PE
        LDA     PM+2
        STA     LSB             ;SAVE LSB
        JMP     CYENT           ;GO NORMALIZE
E7
        RTS
;SERIES EVALUATION FOR SQUARED ARGUMENTS WITH CHAIN AT (AY)
SRSAS
        STA     SRPNT
        STY     SRPNT+1
;SERIES EVALUATION FOR SQUARED ARGUMENTS WITH CHAIN AT (SRPNT)
SRSS
        LDA     #<T0
        LDY     #>T0
        JSR     STP             ;SAVE ACCUM IN TEMP 0
        LDA     #<T0
        LDY     #>T0
        JSR     MPA             ;P:=X*X
        JSR     SRS             ;EVALUATE SERIES
        LDA     #<T0
        LDY     #>T0
        JMP     MPA             ;P:=P*X
;SERIES EVAUATION FOR ARGUMENTS WITH CHAIN AT (AY)
SRSA
        STA     SRPNT
        STY     SRPNT+1
;SERIES EVAUATION FOR ARGUMENTS WITH CHAIN AT (SRPNT)
SRS
        LDA     #<T1
        LDY     #>T1
        JSR     STP             ;T1:=X
        LDY     #0
        LDA     (SRPNT),Y       ;GET COUNT
        STA     SRCNT           ;AND SAVE
        INC     SRPNT
        BNE     *+4
        INC     SRPNT+1         ;SRPNT:=SRPNT+1
        LDA     SRPNT
        LDY     SRPNT+1         ;GET DESTINATION
SRSLPE
        JSR     MPA             ;P:=P*(SRPNT)
        CLC
        LDA     SRPNT
        ADC     #4
        STA     SRPNT
        BCC     *+4
        INC     SRPNT           ;SRPNT:=SRPNT+4
        LDY     SRPNT+1
        JSR     APA             ;P:=P+(SRPNT)
        LDA     #<T1
        LDY     #>T1
        DEC     SRCNT
        BNE     SRSLPE          ;LOOP IF MORE
        RTS
;GET NEXT CHAR FROM ACCUM, BUMP INDEX FOR NEXT ACCESS, AND TEST FOR NUMBER
GETTST
        INC     CNVIND          ;BUMP INDEX
        LDX     CNVIND          ;GET INDEX
        LDA     ACCUM,X         ;GET CHAR
;TEST FOR A NUMBER
;IF A NUMBER THEN C=0 ELSE C=1
TSTNUM
        CMP     #'9'+1          ;IF <= '9'
        BCC     *+3             ;MAY BE A NUMBER
        RTS                     ;ELSE ISN'T
        CMP     #'0'            ;IF <0
        BCC     *+4             ;ISN'T
        CLC
        RTS                     ;ELSE IS
        SEC
        RTS
;ADD CHAR AT ACCUM(CNVIND) TO INPUT CONVERSION EXPONENT
ACE
        LDA     NE
        ASL     A
        ASL     A               ;A:=4*EXP
        CLC
        ADC     NE              ;A:=5*EXP
        ASL     A               ;A:=10*EXP
        CLC
        LDY     CNVIND
        ADC     ACCUM,Y         ;A:=ACCUM(CNVIND)
        SEC
        SBC     #'0'
        STA     NE              ;NE:=10*NE+ACCUM(CNVIND)
        RTS
;MOVE ASCII STRING AT (AY) TO ACCUM AND CONVERT X CHARACTERS TO FLOATING
FLTINP
        STA     GFLTIC+1
        STY     GFLTIC+2        ;SET ADDRESS
        CPX     #0              ;IF 1 OR MORE
        BNE     *+5             ;GO DO
        JMP     ZRORSL          ;ELSE JUST CLEAR ACCUM
        LDY     #0              ;CLEAR INDEX
GFLTIC
        LDA     $FFFF,Y         ;GET CHAR
        STA     ACCUM,Y         ;PUT IN ACCUM
        INY
        DEX
        BNE     GFLTIC          ;LOOP IF MORE
        TXA
        STA     ACCUM,Y         ;INSERT TERMINAL CHAR
        LDX     #9
CLRN
        STA     NE,X
        DEX
        BPL     CLRN            ;LOOP FOR MORE
        STX     CNVIND          ;SET INDEX TO -1
        JSR     GETTST          ;GET AND TEST
        BCC     NOSIGN          ;BRANCH IF IS
        CMP     #'+'            ;IF A +
        BEQ     GNNC            ;GET NEXT CHAR
        CMP     #'-'            ;IF NOT A -
        BNE     NOTDIG          ;TRY E AND .
        LDX     #$FF
        STX     NS              ;SET SIGN TO NEG
GNNC
        JSR     GETTST          ;GET AND TEST
        BCS     NOTDIG          ;BRANCH IF NOT NUMBER
NOSIGN
        PHA                     ;SAVE CHAR
        BIT     DPFLG           ;TEST FOR PRIOR DEC POINT
        BPL     *+4             ;BRANCH IF NONE
        INC     DPOFF           ;BUMP OFFSET
        JSR     MPT             ;P:=P*10
        PLA
        AND     #$0F
        JSR     ADP             ;P:=P+NUM
        JMP     GNNC            ;LOOP FOR MORE
NOTDIG
        CMP     #'.'            ;IF A .
        BEQ     DPHNDL          ;GO HANDLE IT
        CMP     #'E'            ;IF NOT E
        BNE     NESSET          ;GO AHEAD
        JSR     GETTST          ;GET NEXT CHAR
        BCC     DOEN            ;NUMBER SO IS EXPONENT
        CMP     #'-'            ;IF -
        BEQ     EXPMIN          ;EXP IS MINUS
        CMP     #'+'
        BEQ     GNEC            ;IF PLUS GET EXP
        BNE     ENDEXP          ;ELSE END
EXPMIN
        ROR     NESFLG          ;SHIFT CY INTO FLAG
GNEC
        JSR     GETTST          ;GET AND TEST
        BCS     ENDEXP          ;DONE IF NOT NUMBER
DOEN
        JSR     ACE             ;ADD TO EXPONENT
        JMP     GNEC            ;AND LOOP
ENDEXP
        BIT     NESFLG          ;TEST NEG EXP FLAG
        BPL     NESSET          ;BRANCH IF POS
        LDA     #0
        SEC
        SBC     NE              ;-NE
        JMP     NESSUB
DPHNDL
        ROR     DPFLG           ;SET DEC PT FLAG
        BIT     DPFLG           ;TEST FOR TWO
        BVC     GNNC            ;BRANCH IF NONE
NESSET
        LDA     NE
NESSUB
        SEC
        SBC     DPOFF           ;NE-DPOFFSET
        STA     NE
        BEQ     SETSGN          ;DONE IF ZERO
        BPL     MUL10           ;MUKLT IF +
DIV10
        JSR     DPT             ;ELSE DIVIDE BY TEN
        INC     NE
        BNE     DIV10           ;UNTIL DONE
        JMP     SETSGN
MUL10
        JSR     MPT             ;MULT BY 10
        DEC     NE
        BNE     MUL10
SETSGN
        LDA     NS              ;GET SIGN
        BPL     E11             ;DONE IF POS
        LDA     PE
        BEQ     E11             ;OR IF P=0
        LDA     PS
        EOR     #$FF
        STA     PS              ;ELSE COMPLEMENT SIGN
E11
        RTS
;CONVERT CONTENTS OF PRIMARY ACCUMULATOR TO 12 DIGIT NUMBER AT PRINTWORKAREA
;CAUTION: PRIMARY ACCUMULATOR CONTENTS DESTROYED
FLTOUT
        LDY     #1
        LDA     #' '            ;PRESET FOR POSITIVE
        BIT     PS              ;TEST SIGN
        BPL     *+4             ;BRANCH IF POS
        LDA     #'-'            ;ELSE GET -
        STA     PRNWRK-1,Y      ;PUT IN BUFFER
        STA     PS              ;MAKE SIGN POSITIVE
        STY     CNVIND          ;SAVE INDEX
        INY
        LDA     #'0'
        LDX     PE              ;GET EXPONENT
        BNE     *+5             ;OK IF NOT ZERO
        JMP     ONEZRO          ;ELSE IS ZERO
        LDA     #0
        CPX     #128            ;SEE IF >= 0
        BEQ     *+4             ;BRANCH IF =
        BCS     SVEDOF          ;BRANCH IF > 0
        LDA     #<MILLON        ;MULT BY MILLION
        LDY     #>MILLON
        JSR     MPA
        LDA     #$FA            ;OFFSET = -6
SVEDOF
        STA     DPOFF           ;SAVE OFFSET
TRY69S
        LDA     #<SIX9S         ;POINT TO 999999.4375
        LDY     #>SIX9S
        JSR     CPA             ;AND COMPARE
        BEQ     TRY59S          ;BRANCH
        BPL     MUSTDV          ;MUST DIVIDE
TRY59S
        LDA     #<FIVE9S        ;POINT TO 99999.9375
        LDY     #>FIVE9S
        JSR     CPA             ;AND COMPARE
        BEQ     *+4             ;MULT IF =
        BPL     EXTMDP          ;EXIT
        JSR     MPT             ;MULTIPLY BY 10
        DEC     DPOFF           ;DROP OFFSET
        BNE     TRY59S          ;LOOP IF MORE
MUSTDV
        JSR     DPT             ;DIVIDE PRI BY 10
        INC     DPOFF           ;BUMP OFFSET
        BNE     TRY69S          ;LOOP IF MORE
EXTMDP
        LDA     #<HALF          ;ROUND
        LDY     #>HALF
        JSR     APA
        JSR     FXU             ;AND MAKE INTEGER
        LDX     #1
        LDA     DPOFF           ;GET OFFSET
        CLC
        ADC     #7              ;ADD SEVEN
        BMI     JUSTS           ;SUBTRACT IF MINUS
        CMP     #8
        BCS     JUSTS           ;OR IF >= 8
        ADC     #255            ;ELSE ADD -1
        TAX                     ;SAVE
        LDA     #2              ;A:=2
JUSTS
        SEC
        SBC     #2
        STA     NE              ;SAVE EXPONENT
        STX     DPOFF           ;SAVE OFFSET
        TXA
        BEQ     *+4
        BPL     NODP            ;BRANCH IF NO DP
        LDY     CNVIND          ;GET INDEX
        LDA     #'.'            ;GET PERIOD
        INY
        STA     PRNWRK-1,Y      ;PUT IN
        TXA
        BEQ     NZYET
        LDA     #'0'            ;PUT IN A ZERO
        INY
        STA     PRNWRK-1,Y
NZYET
        STY     CNVIND          ;SAVE INDEX
NODP
        LDY     #0              ;CLEAR ADD/SUB INDEX
        LDX     #128
PRIOLP
        CLC
        LDA     PM+2
        ADC     CNVTBL+2,Y
        STA     PM+2
        LDA     PM+1
        ADC     CNVTBL+1,Y
        STA     PM+1
        LDA     PM
        ADC     CNVTBL,Y
        STA     PM
        INX
        BCS     *+6
        BPL     PRIOLP          ;LOOP IF NO OVERFLOW
        BMI     *+4             ;ELSE GOT DIGIT
        BMI     PRIOLP          ;ALSO LOOP
        TXA                     ;MOVE COUNT TO A
        BCC     *+6
        EOR     #$FF
        ADC     #10
        ADC     #'0'-1          ;MAKE ASCII
        INY
        INY
        INY                     ;ADJUST INDEX
        STY     TY              ;SAVE IT
        LDY     CNVIND          ;GET INDEX
        INY                     ;BUMP
        TAX
        AND     #$7F            ;CLEAR MSB
        STA     PRNWRK-1,Y      ;INSERT CHAR
        DEC     DPOFF           ;DROP OFFSET-
        BNE     STLNDP          ;BRANCH IF STILL NO DP
        LDA     #'.'
        INY
        STA     PRNWRK-1,Y      ;INSERT .
STLNDP
        STY     CNVIND          ;SAVE INDEX
        LDY     TY              ;GET Y BACK
        TXA
        EOR     #$FF
        AND     #$80
        TAX
        CPY     #18             ;SEE IF Y AT MAX
        BNE     PRIOLP          ;LOOP IF NOT
        LDY     CNVIND
SKPZRO
        LDA     PRNWRK-1,Y      ;GET CHAR
        DEY                     ;AND BACKUP PAST LEADING ZEROS
        CMP     #'0'
        BEQ     SKPZRO
        CMP     #'.'            ;IF DP
        BEQ     *+3             ;OK
        INY                     ;ELSE FORWARD
        LDA     #'+'            ;PRESET FOR EXP SIGN
        LDX     NE              ;GET EXPONENT
        BEQ     PUTSTP          ;DONE IF NONE
        BPL     EXPPOS          ;IF POS GO
        SEC
        LDA     #0
        SBC     NE
        TAX                     ;ADJUST EXPONENT
        LDA     #'-'
EXPPOS
        STA     PRNWRK+1,Y      ;PUT IN SIGN
        LDA     #'E'            ;THEN E
        STA     PRNWRK,Y
        TXA
        LDX     #'0'-1          ;PRESET 10S
        SEC
CLC10S
        INX
        SBC     #10
        BCS     CLC10S
        ADC     #'0'+10         ;CALC ONES
        STA     PRNWRK+3,Y      ;INSERT IT
        TXA
        STA     PRNWRK+2,Y      ;THEN 10S
        LDA     #0
        STA     PRNWRK+4,Y      ;INSERT STOPPER
        RTS
ONEZRO
        STA     PRNWRK-1,Y      ;INSERT CHAR
PUTSTP
        LDA     #0
        STA     PRNWRK,Y
        RTS
;TRANSCENDENTAL ROUTINES
;GENERATE RANDOM NUMBER AND LEAVE IN PRIM ACCUMULATOR
RND
        LDA     #<RNDWRK
        LDY     #>RNDWRK
        JSR     LPA             ;MOVE OLD TO ACCUM
        LDA     #<RNDMUL
        LDY     #>RNDMUL
        JSR     MPA             ;MULTIPLY MY SCALE
        LDA     #<RNDADD
        LDY     #>RNDADD
        JSR     APA             ;ADD OFFSET
NEWRND
        LDX     PM+2            ;SHUFFLE THINGS AROUND
        LDA     PM
        STA     PM+2
        STX     PM
        LDA     #0
        STA     PS              ;MUST BE POSITIVE
        LDA     PE
        STA     PR
        LDA     #0+128
        STA     PE              ;< 1
        JSR     NORMLZ          ;NORMALIZE IT
        LDA     #<RNDWRK
        LDY     #>RNDWRK
        JMP     STP             ;SAVE NEW RANDOM NUMBER
;NATURAL LOG OF PRIMARY ACCUMULATOR
LOG
        JSR     ESP             ;TEST SIGN OF PRIMARY
        BEQ     LOGOVF          ;ERROR IF ZERO
        BMI     LOGOVF          ;ERROR IF MINUS
        LDA     PR
        ORA     PM+2
        ORA     PM+1
        BNE     DOLOG           ;CAN'T BE 1.000000
        LDA     PM
        AND     #$7F            ;SEE IF ONLY 1
        BNE     DOLOG           ;ISN'T
        LDA     PE
        CMP     #1+128          ;MUST BE 1*2
        BNE     DOLOG           ;ISN'T
        LDA     #0              ;RESULT
        STA     PE              ;EXACTLY ZERO
        STA     PM
        RTS
DOLOG
        SEC
        LDA     PE
        SBC     #128            ;GET FOR EXP*LOG(2)
        PHA                     ;SAVE FOR LATER
        LDA     #0+128          ;SET TO <1
        STA     PE
        LDA     #<SQR2D2
        LDY     #>SQR2D2
        JSR     APA             ;ADD SQR(2)/2
        LDA     #<SQR2
        LDY     #>SQR2
        JSR     DAP             ;SQR(2)/P
        LDA     #<ONE
        LDY     #>ONE
        JSR     SPA             ;1-P
        LDA     #<LOGCHN
        LDY     #>LOGCHN
        JSR     SRSAS           ;DO SERIES FOR X*X
        LDA     #<MHALF
        LDY     #>MHALF
        JSR     APA             ;-.5+P
        PLA                     ;GET EXP
        JSR     ADP             ;ADD DIGIT
        LDA     #<LN2
        LDY     #>LN2
        JMP     MPA             ;MULTIPLY BY LN 2
;ILLEGAL LOG (I.E. NEG OR ZERO ARGUMENT)
LOGOVF
        LDA     #TRUE
        STA     OVERFL
        RTS
;E TO THE PRIMARY ACCUMULATOR
EXP
        LDA     #<T0
        LDY     #>T0
        JSR     STP             ;T0:=P
        LDA     #<LN2INV
        LDY     #>LN2INV
        JSR     MPA             ;P:=P/(LN(2))
        LDA     PE
        CMP     #8+128          ;IF PE < 8
        BCC     EXPOK           ;THEN OK
        BIT     PS              ;ELSE TEST SIGN
        BPL     EXPOVF          ;OVERFLOW IF POSITIVE
        LDA     #0
        STA     PE
        STA     PS              ;ELSE SET P TO 0
        RTS
EXPOK
        JSR     INT             ;FIND INTEGER PART
        CLC
        LDA     LSB
        ADC     #1+128          ;ADD TO CHECK FOR OVERFLOW
        BEQ     EXPOVF          ;ERROR IF RESULT 127
        PHA                     ;ELSE SAVE
        LDA     #<ONE
        LDY     #>ONE
        JSR     APA             ;P:=P+1
        LDA     #<LN2
        LDY     #>LN2
        JSR     MPA             ;P:=P*LN(2)
        LDA     #<T0
        LDY     #>T0
        JSR     SPA             ;P:=X-P
        JSR     NEG             ;P:=-P
        LDA     #<EXPCHN
        LDY     #>EXPCHN
        JSR     SRSA            ;DO SERIES
        LDA     #0
        STA     SGNCMP          ;CLEAR COMPARISON
        PLA                     ;GET INTEGER PART
        JSR     SUPMDX          ;HANDLE IT
        RTS
;ILLEGAL EXPONENT ARGUMENT
EXPOVF
        LDA     #TRUE
        STA     OVERFL          ;SET OVERFLOW TO TRUE
        RTS
;COS OF PRIMARY ACCUMULATOR
COS
        LDA     #<PI2
        LDY     #>PI2
        JSR     APA             ;ADD PI/2
;SIN OF PRIMARY ACCUMULATOR
SIN
        JSR     TPS             ;MOVE TO SECONDARY
        LDA     SS
        STA     SGNCMP          ;SET COMPARISON TO SECONDARY SIGN
        LDA     #<TWOPI
        LDY     #>TWOPI
        JSR     LPA             ;P:=2*PI
        JSR     DSP             ;DIVIDE TO GET REVOLUTIONS
        JSR     TPS             ;S:=P
        JSR     INT             ;GET WHOLE REVS
        LDA     #0
        STA     SGNCMP          ;CLEAR SIGN COMPARISON
        JSR     SPS             ;P:=REV-INT(REV)
        LDA     #<QUARTR
        LDY     #>QUARTR
        JSR     SPA             ;P:=P-.25
        LDA     PS
        PHA                     ;SAVE PS
        BPL     SINPPS          ;BRANCH IF POSITIVE
        LDA     #<HALF
        LDY     #>HALF
        JSR     APA             ;P:=P+.5
        LDA     PS
        BMI     SINSSM          ;BRANCH IF STILL NEG
        LDA     TRIGS
        EOR     #$FF
        STA     TRIGS           ;COMPLEMENT TRIG SIGN
SINPPS
        JSR     NEG             ;P:=-P
SINSSM
        LDA     #<QUARTR
        LDY     #>QUARTR
        JSR     APA             ;P:=P+.25
        PLA                     ;GET PS BACK
        BPL     *+5             ;SKIP IF POS
        JSR     NEG             ;ELSE NEGATE
        LDA     #<SINCHN
        LDY     #>SINCHN
        JMP     SRSAS           ;DO SQUARED SERIES
;ARCTANGENT OF PRIMARY ACCUMULATOR
ATN
        LDA     PS
        PHA                     ;SAVE PS
        BPL     *+5             ;OK IF POS
        JSR     NEG             ;ELSE P:=-P
        LDA     PE
        PHA                     ;SAVE PE
        CMP     #1+128          ;IF < 1
        BCC     ATNEL1          ;THEN SKIP AHEAD
        LDA     #<ONE
        LDY     #>ONE
        JSR     DAP             ;P:=1/P
ATNEL1
        LDA     #<ATNCHN
        LDY     #>ATNCHN
        JSR     SRSAS           ;DO SERIES
        PLA                     ;GET EXP
        CMP     #1+128          ;IF LESS THAN 1
        BCC     NOMPI2          ;SKIP AHEAD
        LDA     #<PI2
        LDY     #>PI2
        JSR     SPA             ;ELSE P:=PI/2-P
NOMPI2
        PLA                     ;GET SIGN
        BPL     *+5             ;DONE IF POS
        JMP     NEG             ;ELSE P:=-P
        RTS
;PSEUDO MACHINE OPCODE EXECUTION ROUTINE
EXECUT
        LDY     #0
        LDA     (RC),Y
        BPL     L060            ;IF MSB=0 THEN NOT LIT OR LI-LOD
        JSR     PSHSTK          ;PUSH$STACK
        LDY     #1
        LDA     (RC),Y
        DEY
        STA     (RA),Y          ;BRA(0):=CV(1)
        LDA     (RC),Y
        AND     #$3F
        INY
        STA     (RA),Y          ;BRA(1):=C AND $3F
        DEY
        LDA     (RC),Y
        ASL     A
        BPL     *+5             ;IF NOT ROL(C,2) THEN
        JSR     LOADRA          ;LOAD$RA
        JSR     STINCN          ;STEP$INS$CNT
        JSR     STINCN
        JMP     EXECUT          ;DO FOREVER
L060
        ASL     A
        TAX                     ;MAKE AN INDEX
        LDA     EXTBL,X
        STA     DOOP+1
        LDA     EXTBL+1,X
        STA     DOOP+2          ;SET ADDRESS
        JSR     DOOP            ;DO OP
        JSR     STINCN          ;STEP$INS$CNT
        JMP     EXECUT          ;DO FOREVER
DOOP
        JMP     $FFFF           ;DUMMY
;EXECUTE VECTOR TABLE
EXTBL
        .WORD   EX0,EX1,EX2,EX3,EX4,EX5,EX6,EX7
        .WORD   EX8,EX9,EX10,EX11,EX12,EX13,LOADRA,EX15
        .WORD   EX16,POPSTK,EX18,FLIP,EX20,EX21,EX22,EX23
        .WORD   EX24,EX25,EX26,EX27,EX28,CONCAT,EX30,EX31
        .WORD   CLCROW,CLCSUB,EX34,EX35,EX36,EX37,EX38,EX39
        .WORD   EX40,EX41,EX42,EX43,EX44,DSKOPN,EX46,EX47
        .WORD   EX48,EX49,EXNOP,EXNOP,DMPPBF,EX53,ABSBRA,EX55
        .WORD   CNDBRA,UNCBRA,CNVBRA,EX59,EX60,EX61,EX62,EX63
        .WORD   EX64,EX65,EX66,EX67,EX68,EX69,EX70,EX71
        .WORD   EX72,EX73,EX74,EX75,EX76,EX77,EX78,EX79
        .WORD   EX80,EX81,EX82,EX83,EX84,EX85,EX86,EX87
        .WORD   EX88,EX89,EX90,RNCBRA,EX92,EX93,EX94,EX95
        .WORD   EX96
; OPCODE EXECUTION ROUTINES
;DO NOTHING
EXNOP
        RTS
;0	FAD:	RB:=RA+RB
EX0
        JSR     FLODRA
        LDA     RB
        LDY     RB+1
        JSR     APA
        JSR     FSTRRB
        JSR     POPSTK
        JMP     CHKOVR
;1	FMI:	RB:=RB-RA
EX1
        JSR     FLODRA
        LDA     RB
        LDY     RB+1
        JSR     SPA
        JSR     FSTRRB
        JSR     POPSTK
        JMP     CHKOVR
;2	FMU:	RB:=RB*RA
EX2
        JSR     FLODRA
        LDA     RB
        LDY     RB+1
        JSR     MPA
        JSR     FSTRRB
        JSR     POPSTK
        JMP     CHKOVR
;3	FDI:	RB:=RB/RA
EX3
        JSR     RAZER           ;IF NOT RA$ZERO
        BEQ     *+7             ;THEN
        LDA     #4              ;ELSE DIV BY ZERO WARNING
        JSR     WARNNG
EX3A
        JSR     FLODRA
        LDA     RB
        LDY     RB+1
        JSR     DAP
        JSR     FSTRRB
        JSR     POPSTK
        JMP     CHKOVR
;4	EXP:	RB:=RB**RA
EX4
        JSR     RBZER           ;IF NOT RB$ZERO THEN
        BEQ     EX4B
        JSR     RAZER           ;IF NOT RA$ZERO THEN
        BEQ     EX4C
        LDY     #3
EX4A
        LDA     ONE,Y
        STA     (RB),Y
        DEY
        BPL     EX4A            ;ELSE MOVE4(.PLUSONE,RB)
        BMI     EX4C            ;DONE
EX4B
        JSR     RBNEG           ;IF NOT RB$NEGATIVE
        BEQ     *+7             ;THEN OK
        LDA     #12             ;ELSE ATTEMPT TO RAISE NEG NUMBER TO POWER
        JMP     ERROR
        JSR     FLODRB
        JSR     LOG
        LDA     RA
        LDY     RA+1
        JSR     MPA
        JSR     EXP
        JSR     FSTRRB
EX4C
        JSR     POPSTK
        JMP     CHKOVR
;5  LSS
EX5
        JSR     COMPFP
        CMP     #1
        BEQ     EX5T
EX5F
        LDA     #FALSE
        JMP     COMFIX
EX5T
        LDA     #TRUE
        JMP     COMFIX
;6  GTR
EX6
        JSR     COMPFP
        CMP     #2
        BEQ     EX5T
        BNE     EX5F
;7  EQU
EX7
        JSR     COMPFP
        CMP     #3
        BEQ     EX5T
        BNE     EX5F
;8  NEQ
EX8
        JSR     COMPFP
        CMP     #3
        BNE     EX5T
        BEQ     EX5F
;9  GEQ
EX9
        JSR     COMPFP
        CMP     #1
        BNE     EX5T
        BEQ     EX5F
;10 LEQ
EX10
        JSR     COMPFP
        CMP     #2
        BNE     EX5T
        BEQ     EX5F
;11 NOT
EX11
        JSR     CNVBRA          ;CONV$TO$BINARY(RA)
        LDY     #3
EX11A
        LDA     (RA),Y
        EOR     #$FF
        STA     (RA),Y
        DEY
        BNE     EX11A           ;(RA):=NOT (RA)
        JMP     CNVFRA          ;CONV$TO$FLOATING(RA)
;12 AND
EX12
        JSR     CNVBRA          ;CONV$TO$BINARY(RA)
        JSR     CNVBRB          ;RB
        LDY     #3
EX12A
        LDA     (RA),Y
        AND     (RB),Y
        STA     (RB),Y
        DEY
        BNE     EX12A           ;(RB):=(RB) AND (RA)
        JSR     POPSTK          ;POP$STACK
        JMP     CNVFRA          ;CONV$TO$FLOATING(RA)
;13 OR
EX13
        JSR     CNVBRA
        JSR     CNVBRB
        LDY     #3              ;SET INDEX
EX13A
        LDA     (RA),Y
        ORA     (RB),Y
        STA     (RB),Y
        DEY
        BNE     EX13A           ;(RB):=(RB) OR (RA)
        JSR     POPSTK
        JMP     CNVFRA
;15 STO
EX15
        LDA     #0
        JSR     STORE           ;STORE(0)
        JSR     MVRARB          ;MOVE$RA$RB
        JMP     POPSTK          ;POP$STACK
;16 XIT
EX16
        PLA
        PLA                     ;CLEAR STACK
        RTS                     ;AND RETURN TO OUTER LOOP
;18 DUP
EX18
        JSR     PSHSTK
        JMP     MVRBRA
;20 STD
EX20
        LDA     #0
        JSR     STORE
        JSR     POPSTK
        JMP     POPSTK
;21 SLT
EX21
        JSR     CMPSTR
        CMP     #1
        BEQ     EX21T
EX21F
        LDA     #FALSE
        JMP     COMFIX
EX21T
        LDA     #TRUE
        JMP     COMFIX
;22 SGT
EX22
        JSR     CMPSTR
        CMP     #2
        BEQ     EX21T
        BNE     EX21F
;23 SEQ
EX23
        JSR     CMPSTR
        CMP     #3
        BEQ     EX21T
        BNE     EX21F
;24 SNE
EX24
        JSR     CMPSTR
        CMP     #3
        BNE     EX21T
        BEQ     EX21F
;25 SGE
EX25
        JSR     CMPSTR
        CMP     #1
        BNE     EX21T
        BEQ     EX21F
;26 SLE
EX26
        JSR     CMPSTR
        CMP     #2
        BNE     EX21T
        BEQ     EX21F
;27 STS
EX27
        LDA     #1
        JSR     STORE
        JSR     POPSTK
        JMP     POPSTK
;28 ILS
EX28
        JSR     PSHSTK
        JSR     STINCN
        LDY     #0
        LDA     RC
        STA     (RA),Y
        INY
        LDA     RC+1
        STA     (RA),Y          ;ARA:=RC
        DEY
        LDA     (RC),Y          ;A:=C
        CLC
        ADC     RC
        STA     RC
        BCC     *+4
        INC     RC+1            ;RC:=RC+C
        LDA     #FALSE
        JMP     FLSTRA          ;FLAG$STRING$ADDRESS(FALSE)
;30 PRO
EX30
        JSR     STINCN
        JSR     PSHSTK
        CLC
        LDA     RC
        LDY     RC+1
        ADC     #2
        PHA
        BCC     *+3
        INY
        TYA
        LDY     #1
        STA     (RA),Y
        PLA
        DEY
        STA     (RA),Y          ;ARA:=RC+1+1
        LDA     (RC),Y
        TAX
        INY
        LDA     (RC),Y
        STA     RC+1
        STX     RC              ;RC:=TWOBYTEOPRAND
        RTS
;31 RTN
EX31
        JSR     ARA
        SEC
        SBC     #1
        BCS     *+3
        DEY
        STA     RC
        STY     RC+1
        JMP     POPSTK
;34 RDV	READS A NUMBER FROM CONSOLE
EX34
        JSR     MRECIN
        BNE     *+5
        JSR     CONIER
        JMP     GTNMFL          ;GET$NUMERIC$FIELD
;35 WRV	PRINTS THE NUMBER ON THE TOP OF THE STACK
EX35
        JSR     NUMOUT          ;NUMERIC$OUT
        JSR     WRTTCN          ;WRITE$TO$CONSOLE
        JMP     POPSTK
;36 WST PRINTS THE STRING WHOSE ADDRESS IS ON TOP OF STACK
EX36
        JSR     WRTTCN          ;WRITE$TO$CONSOLE
        JSR     STRFRE          ;STRING$FREE
        JMP     POPSTK
;37 RDF READY A RANDOM BLOCK
EX37
        JSR     SUDSIO          ;SETUP$DISK$IO
        JSR     RANSTU          ;RANDOM$SETUP
        JMP     STEFST          ;SET$EOF$STACK
;38 RDB READY NEXT SEQUENTIAL BLOCK
EX38
        JSR     SUDSIO
        JMP     STEFST
;39 ECR
EX39
        JSR     MRECIN          ;IF NOT MORE$CON$INPUT
        BEQ     EX39A           ;THEN
        JSR     PSHSTK
        JMP     CONIER          ;CONSOLE$INPUT$ERROR
EX39A
        RTS
;40 OUT	ACTUALLY POKE
EX40
        JSR     ARA
        STA     POKE+1
        STY     POKE+2          ;LOCATION IS ARA
        LDY     #0
        LDA     (RB),Y          ;A:=BRAZ
POKE
        STA     $FFFF
        JSR     POPSTK
        JMP     POPSTK
;41 RDN	READ A NUMBER FROM DISK
EX41
        LDA     #0
        STA     INPTYP
        JMP     GTNMFL          ;GET$NUMERIC$FIELD
;42 RDS READ A STRING FROM DISK
EX42
        LDA     #0
        STA     INPTYP
        JMP     GTSTFL          ;GET$STRING$FIELD
;43 WRN WRITE A NUMBER TO DISK
EX43
        LDA     #0
        JMP     WRTOFL          ;WRITE$TO$FILE(0)
;44 WRS WRITE A STRING TO DISK
EX44
        LDA     #1
        JMP     WRTOFL          ;WRITE$TO$FILE(1)
;46 CON
EX46
        JSR     PSHSTK
        JSR     STINCN          ;STEP$INS$CNT
        LDY     #0
        LDA     (RC),Y
        STA     MFROM
        INY
        LDA     (RC),Y
        STA     MFROM+1
        LDA     RA
        LDY     RA+1
        STA     MTO
        STY     MTO+1
        JSR     MOVE4
        JMP     STINCN
;47 RST RESET POINTER TO BEGINNING OF DATA AREA
EX47
        LDY     MDA+1
        LDX     MDA
        BNE     *+3
        DEY
        DEX
        STX     DTARPT
        STY     DTARPT+1        ;DATA$AREA$PTR:=MDA-1
        RTS
;48 NEG
EX48
        JSR     FLODRA          ;PRIM:=(RA)
        JSR     NEG             ;PRIM:=-PRIM
        JSR     FSTRRA          ;(RA):=PRIM
        JMP     CHKOVR          ;CHECK$OVERFLOW
;49 RES READ STRING
EX49
        JSR     MRECIN          ;IF MORE$CON$INPUT
        BNE     *+5             ;THEN
        JSR     CONIER          ;ELSE CONSOLE$INPUT$ERROR
        JMP     GTSTFL          ;GET$STRING$FIELD
;53 NSP
EX53
        LDA     PRBUFF          ;GET CURRENT POSITION
        CMP     #112            ;IF UNDER 112 THEN OK
        BCC     *+5
        JMP     DMPPBF          ;ELSE DO A CR AND LF
EX53A
        LDA     #' '            ;SEND A SPACE
        JSR     PRNCHR
        INC     PRBUFF          ;AND BUMP INDEX
        SEC
        LDA     PRBUFF          ;GET INDEX
EX53B
        SBC     #14             ;SUBTRACT 14
        BCC     EX53A           ;IF BORROW THEN NOT THERE
        BNE     EX53B           ;IF NOT ZERO MAY BE MORE
        RTS                     ;ELSE DONE
;55 BRC
EX55
        JSR     RAZER           ;IF RA$ZERO
        BNE     EX55A           ;THEN
        CLC
        LDA     RC
        ADC     #2
        STA     RC
        BCC     *+4
        INC     RC+1            ;ELSE RC:=RC+2
        JMP     POPSTK
EX55A
        JSR     ABSBRA          ;ABSOLUTE$BRANCH
        JMP     POPSTK
;59 RCH
EX59
        LDA     #1
        STA     INPTYP          ;INPUTTYPE:=1
        LDA     RC
        LDY     RC+1
        STA     RERDAD
        STY     RERDAD+1        ;REREADADDR:=RC
        JMP     CONRDE          ;CONSOLE$READ
;60 DRS READ STRING FROM DATA AREA
EX60
        LDA     #2
        STA     INPTYP          ;INPUTTYPE:=2
        JMP     GTSTFL          ;GET$STRING$FIELD
;61 DRF READ FLOATING POINT NUMBER FROM DATA AREA
EX61
        LDA     #2
        STA     INPTYP
        JMP     GTNMFL          ;GET$NUMERIC$FIELD
;62 EDR END OF RECORD FOR READ
EX62
        JSR     VARBSZ
        BEQ     EX62B           ;IF NOT VAR$BLOCK$SIZE THEN
EX62A
        JSR     GTDSCH          ;ELSE
        CMP     #LF
        BNE     EX62A           ;DO WHILE GET$DISK$CHAR <> LF
EX62B
        JMP     STRRPT          ;STORE$REC$PTR
;63 EDW END OF RECORD FOR WRITE
EX63BS  ;BLOCKSIZE-2
        .RES    2
EX63
        JSR     VARBSZ
        BEQ     EX63B           ;IF NOT VAR$BLOCK$SIZE THEN
        SEC
        LDA     BLKSZE
        LDY     BLKSZE+1
        SBC     #2
        STA     EX63BS
        BCS     *+3
        DEY
        STY     EX63BS+1        ;CALCULATE BLOCKSIZE-2
EX63A
        LDA     BYTSWR
        CMP     EX63BS
        LDA     BYTSWR+1
        SBC     EX63BS+1
        BCS     EX63B           ;IF BYTES$WRITTEN >= BLOCKSIZE-2 THEN
        LDA     #' '            ;ELSE
        JSR     WRABYT          ;WRITE$A$BYTE(BLANK)
        JMP     EX63A
EX63B
        LDA     #CR
        JSR     WRABYT
        LDA     #LF
        JSR     WRABYT
        JMP     STRRPT          ;STORE$REC$PTR
;64 CLS CLOSE A FILE
EX64
        JSR     STFLAD          ;SET$FILE$ADDR
        JSR     DSKCLS          ;DISK$CLOSE
        LDY     #0
        LDA     (RA),Y
        ASL     A
        TAX                     ;X:=2*BRAZ
        TYA
        STA     FILES,X
        STA     FILES+1,X
        STA     EOFBRN,X
        STA     EOFBRN+1,X      ;FILES(BRAZ),EOFBRANCH(BRAZ):=0
        JMP     POPSTK
;65 ABSOLUTE
EX65
        LDY     #1
        LDA     (RA),Y
        AND     #$7F
        STA     (RA),Y          ;BRA(1):=BRA(1) AND $7F
        RTS
;66 INTEGER
EX66
        JSR     FLODRA          ;PRIM:=(RA)
        JSR     INT
        JSR     FSTRRA          ;(RA):=INT(PRIM)
        JMP     CHKOVR          ;CHECK$OVERFLOW
;67 RANDOM NUMBER GENERATOR
EX67
        JSR     RND             ;CALCULATE NUMBER
        JSR     PSHSTK          ;MAKE ROOM FOR NUMBER
        JMP     FSTRRA          ;PUT ON STACK
;68 SGN
EX68
        JSR     RANEG
        PHA
        JSR     RAZER
        EOR     #$FF
        JSR     COMFIX          ;COMP$FIX(NOT RA$ZERO)
        PLA
        BNE     *+5             ;IF RA$NEGATIVE THEN
        JMP     EX48            ;ELSE CHANGE SIGN
        RTS
;69 SIN
EX69
        JSR     FLODRA
        JSR     SIN
        JSR     FSTRRA
        JMP     CHKOVR
;70 COS
EX70
        JSR     FLODRA
        JSR     COS
        JSR     FSTRRA
        JMP     CHKOVR
;71 ATN
EX71
        JSR     FLODRA
        JSR     ATN
        JSR     FSTRRA
        JMP     CHKOVR
;72 TAN
EX72
        JSR     PSHSTK          ;EXTEND STACK
        JSR     MVRBRA          ;DUPLICATE PARAM
        JSR     EX69            ;CALCULATE SIN
        JSR     POPSTK
        JSR     EX70            ;NOW DO COS
        JSR     PSHSTK          ;SIN STILL THERE
        JSR     RBZER
        BEQ     *+7             ;IF COS NOT ZERO THEN
        LDA     #22             ;ELSE TAN(PI/2) ERROR
        JMP     ERROR
        JSR     FLIP
        JMP     EX3A            ;DIVIDE
;73 SGR
EX73
        JSR     FLODRA
        BIT     PS
        BPL     *+7             ;OK IF POSITIVE
        LDA     #12             ;ELSE NEGATIVE NUMBER TO POWER ERROR
        JMP     ERROR
        JSR     LOG
        LDA     #<HALF
        LDY     #>HALF
        JSR     MPA             ;1/2 * LOG()
        JSR     EXP             ;EXP()
        JSR     FSTRRA
        JMP     CHKOVR
;74 TAB
EX74
        JSR     CNVBAD          ;CONV$TO$BIN$ADDR
        LDY     #0
        LDA     (RA),Y          ;GET LOW
        CMP     #133            ;SEE IF TOO BIG
        BCS     EX74E           ;ERROR IF IS
        INY
        LDA     (RA),Y          ;IF ANYTHING IN HIGH
        BNE     EX74E           ;IS ERROR
        DEY
        SEC
        LDA     (RA),Y
        BEQ     EX74Z           ;SPECIAL CASE IF ZERO
        SBC     #1
        STA     (RA),Y          ;ARA:=ARA-1  (LOW ONLY)
        LDA     PRBUFF
        CMP     (RA),Y
        BCC     EX74D           ;IF ARA > PRINTBUFFER THEN
        BEQ     EX74X           ;BUT IF SAME DONE
        JSR     DMPPBF          ;DUMP$PRINT$BUFFER
        LDY     #0              ;GET DEST AGAIN
        LDA     (RA),Y
        BEQ     EX74X           ;DONE IF ZERO
EX74D
        LDA     #' '            ;SEND A BLANK
        JSR     PRNCHR          ;TO CONSOLE
        INC     PRBUFF          ;AND BUMP INDEX
        LDY     #0
        LDA     (RA),Y          ;GET END POSITION BACK
        CMP     PRBUFF          ;SEE IF THERE
        BNE     EX74D           ;LOOP IF NOT
EX74X
        JMP     POPSTK
EX74Z
        STA     PRBUFF          ;CLEAR POINTER
        LDA     #CR             ;THEN SEND CARRIAGE RETURN
        JSR     PRNCHR
        JMP     POPSTK          ;AND POP STACK POINTER
EX74E
        LDA     #27             ;ILLEGAL ARGUMENT
        JMP     ERROR
;75 EXP
EX75
        JSR     FLODRA
        JSR     EXP
        JSR     FSTRRA
        JMP     CHKOVR
;76 FREE AREA IN FSA
EX76
        JSR     PSHSTK
        LDA     #0
        TAY
        JSR     AVLL            ;AVAILABLE(0)
        JMP     FLTADR          ;FLOAT$ADDR()
;77 IRN RANDOMIZE
EX77
        LDA     LCLSED
        LDY     LCLSED
        STA     RNDWRK+1
        STY     RNDWRK+2
        JMP     RND
;78 LOG
EX78
        JSR     FLODRA
        JSR     LOG
        JSR     FSTRRA
        JMP     CHKOVR
;79 POSITION OF PRINT BUFFER PTR
EX79
        JSR     PSHSTK
        LDA     PRBUFF
        CLC
        ADC     #1
        LDY     #0
        JMP     FLTADR          ;FLOAT$ADDR(PRINTBUFFER-PRINTBUFFERLOC+1)
;80 INP [ACTUALLY PEEK]
EX80
        JSR     RNCBRA
        JSR     ARA
        STA     PEEK+1
        STY     PEEK+2
PEEK
        LDA     $FFFF
        LDY     #0
        JMP     FLTADR
;81 ASCII CONVERSION
EX81
        JSR     ARA
        STA     EXH
        STY     EXH+1           ;HOLD:=ARA
        ORA     EXH+1           ;IF = 0
        BEQ     EX81A           ;THEN ERROR
        LDY     #0
        LDA     (EXH),Y
        BNE     *+7             ;IF H(0) <> 0 THEN OK
EX81A
        LDA     #0              ;ELSE NULL STRING ERROR
        JMP     ERROR
        LDY     #1
        LDA     (EXH),Y         ;A:=H(1)
        PHA
        JSR     STRFRE          ;STRING$FREE
        PLA
        LDY     #0
        JMP     FLTADR          ;FLOAT$ADDR(TEMP)
;82 CHR CONVERTS TO ASCII
EX82
        JSR     CNVBAD          ;CONV$TO$BIN$ADDR
        LDA     #2
        LDY     #0
        JSR     GETS            ;GETSPACE(2)
        STA     EXH
        STY     EXH+1           ;HOLD:=
        LDY     #0
        LDA     #1
        STA     (EXH),Y         ;LOC(0):=1
        LDA     (RA),Y
        INY
        STA     (EXH),Y         ;LOC(1):=BRA(0)
        LDA     EXH+1
        STA     (RA),Y
        DEY
        LDA     EXH
        STA     (RA),Y          ;ARA:=HOLD
        LDA     #TRUE
        JMP     FLSTRA          ;FLAG$STRING$ADDR(TRUE)
;83 LEFT END OF STRING
EX83
        LDA     #0
        JMP     STRSEG          ;STRING$SEGMENT(0)
;84 LENGTH OF STRING
EX84
        JSR     ARA
        JSR     GETSLN          ;GET$STRING$LEN(ARA)
        PHA
        JSR     STRFRE
        PLA
        LDY     #0
        JMP     FLTADR          ;FLOAT$ADDR(LENGTH)
;85 MIDDLE OF STRING
EX85
        LDA     #2
        JMP     STRSEG          ;STRING$SEGMENT(2)
;86 RIGHT END OF STRING
EX86
        LDA     #1
        JMP     STRSEG          ;STRING$SEGMENT(1)
;87 CONVERSION TO STRING
EX87
        JSR     NUMOUT
        CLC
        LDA     NUOWRK
        ADC     #1
        PHA
        LDY     #0
        JSR     GETS            ;GET$SPACE()
        STA     MTO
        STY     MTO+1
        LDY     #0
        STA     (RA),Y
        INY
        LDA     MTO+1
        STA     (RA),Y          ;ARA:=
        LDA     #<NUOWRK
        LDY     #>NUOWRK
        STA     MFROM
        STY     MFROM+1
        PLA
        LDY     #0
        JSR     MOVE            ;MOVE(FROM,TO,AY)
        LDA     #TRUE
        JMP     FLSTRA          ;FLAG$STRING$ADDR(TRUE)
;88 VALUE
EX88
        JSR     ARA
        JSR     GETSLN          ;GET$STRING$LEN(ARA)
        TAX
        JSR     ARA
        CLC
        ADC     #1
        BCC     *+3
        INY
        JSR     FLTINP
        JSR     STRFRE          ;STRING$FREE
        JMP     FSTRRA          ;(RA):=PRIM
;89 CALL
EX89
        JSR     CNVBAD          ;CONVERT TO ADDRESS
        JSR     ARA
        STA     CALL+1
        STY     CALL+2
CALL
        JSR     $FFFF           ;DO CALL
        JMP     FLTADR          ;FLOAT AND RETURN
;90 SINH
EX90
        JSR     FLODRA
        LDA     PE              ;TEST FOR ZERO
        BNE     *+3             ;CONTINUE IF NOT
        RTS                     ;ELSE DONE
        LDA     #<T2
        LDY     #>T2
        JSR     STP             ;SAVE IN T2
        JSR     EXP
        LDA     #<T3
        LDY     #>T3
        JSR     STP             ;SAVE EXP(X) IN T3
        LDA     #<T2
        LDY     #>T2
        JSR     LPA             ;GET X BACK
        JSR     NEG             ;-X
        JSR     EXP             ;LOG(-X)
        JSR     NEG             ;-LOG(-X)
        LDA     #<T3
        LDY     #>T3
        JSR     APA             ;LOG(X)-LOG(-X)
        LDA     #<HALF
        LDY     #>HALF
        JSR     MPA             ;SINH(X)
        JSR     FSTRRA          ;STORE AT RA
        JMP     CHKOVR          ;CHECKOVERFLOW
;92 CKO
;RA CONTAINS MAX NUMBER LABELS IN THE ON STATEMENT
;RB CONTAINS SELECTED LABEL
;CHECKS TO INSURE THAT SLECTED LABEL EXISTS. IF NOT AN ERROR IS NOTED
EX92T
        .RES    1
EX92
        LDY     #0
        LDA     (RB),Y
        SEC
        SBC     #1
        STA     (RB),Y          ;BRBZ:=BRBZ-1
        STA     EX92T           ;SAVE FOR COMPARISON
        SEC
        LDA     (RA),Y
        SBC     #1              ;BRAZ-1
        CMP     EX92T
        BCS     *+7             ;IF BRAZ-1 >= BRBZ THEN OK
        LDA     #16             ;ELSE INDEX IN ON STATEMENT OUT OF BOUNDS
        JMP     ERROR
        JSR     POPSTK          ;POP$STACK
        LDY     #0
        LDA     (RA),Y
        CLC
        ADC     #1
        STA     EX92T           ;SAVE
        LDA     (RA),Y
        ASL     A
        CLC
        ADC     EX92T
        STA     (RA),Y          ;BRAZ:=
        RTS
;93 EXR
EX93
        JSR     CNVBRA
        JSR     CNVBRB
        LDY     #3
EX93A
        LDA     (RA),Y
        EOR     (RB),Y
        STA     (RB),Y
        DEY
        BPL     EX93A
        JSR     POPSTK          ;POP$STACK
        JMP     CNVFRA
;94 DEF
EX94
        JSR     STINCN          ;STEP$INS$CNT
        JSR     GTFLNU          ;GET$FILE$NUMBER
        ASL     A
        TAX
        JSR     ARC             ;TWOBYTEOPRAND
        STA     EOFBRN,X
        TYA
        STA     EOFBRN+1,X      ;EOFBRANCH():=
        JSR     STINCN          ;STEP$INS$CNT
        JMP     POPSTK          ;POP$STACK
;95 BOL
EX95
        JSR     ARA
        STA     CURLIN
        STY     CURLIN+1        ;CURRENTLINE:=ARA
        JMP     POPSTK          ;POP$STACK
;96 ADJ
EX96
        JSR     ARA
        CLC
        ADC     MCD
        PHA
        TYA
        ADC     MCD+1           ;ARA+MCD
        LDY     #1
        STA     (RA),Y
        DEY
        PLA
        STA     (RA),Y          ;ARA:=
        RTS
;MAIN PROGRAM
MAIN
        JSR     CRLF
        JSR     INTLEX          ;INITIALIZE$EXECUTE
EOFEXT
ERREXT
        LDX     #$FF
        TXS                     ;RESET STACK
        JSR     EXECUT          ;EXECUTE
        JMP     EXTINT          ;EXIT INTERPRETER
;MESSAGES
ERRMSG
        .BYTE   "ERROR - $"
WRNMSG
        .BYTE   "WARNING - $"
INLMSG
        .BYTE   " IN LINE $"
;ERROR MESSAGES
ERM0
        .BYTE   "NULL STRING PASSED AS "
        .BYTE   "PARAMETER TO ASC FUNCTION$"
ERM1
        .BYTE   "ERROR WHILE CLOSING A FILE$"
ERM2
        .BYTE   "DISK READ ERROR - UNWRITTEN"
        .BYTE   " DATA IN RANDOM ACCESS$"
ERM3
        .BYTE   "DISK WRITE ERROR$"
ERM4
        .BYTE   "DIVISION BY ZERO$"
ERM5
        .BYTE   "EOF FOR DISK FILE AND"
        .BYTE   " NO ACTION SPECIFIED$"
ERM6
        .BYTE   "RECORD SIZE EXCEEDED "
        .BYTE   "FOR BLOCKED FILE$"
ERM7
        .BYTE   "INVALID INPUT FROM CONSOLE$"
ERM8
        .BYTE   "INVALID RECORD IN RANDOM "
        .BYTE   "ACCESS$"
ERM9
        .BYTE   "ACCESSING AN UNOPENED FILE$"
ERM10
        .BYTE   "ERROR WHILE CREATING FILE$"
ERM11
        .BYTE   "FILE IDENTIFIER TOO LARGE "
        .BYTE   "OR ZERO$"
ERM12
        .BYTE   "ATTEMPT TO RAISE A NEGATIVE "
        .BYTE   "NUMBER TO A POWER$"
ERM13
        .BYTE   "NO INT FILE FOUND IN "
        .BYTE   "DIRECTORY$"
ERM14
        .BYTE   "ATTEMPT TO READ PAST END "
        .BYTE   "OF DATA AREA$"
ERM15
        .BYTE   "ERROR WHILE OPENING A FILE$"
ERM16
        .BYTE   "INDEX IN ON STATEMENT OUT "
        .BYTE   "OF BOUNDS$"
ERM17
        .BYTE   "ATTEMPT TO READ PAST END OF"
        .BYTE   " RECORD ON BLOCKED FILE$"
ERM18
        .BYTE   "UNBLOCKED FILE USED WITH "
        .BYTE   "RANDOM ACCESS$"
ERM19
        .BYTE   "ARRAY SUBSCRIPT OUT OF "
        .BYTE   "BOUNDS$"
ERM20
        .BYTE   "STRING LENGTH EXCEEDS 255$"
ERM21
        .BYTE   "SECOND PARAMETER OF MID IS "
        .BYTE   "NEGATIVE$"
ERM22
        .BYTE   "ATTEMPT TO EVALUATE TANGENT "
        .BYTE   "OF PI OVER TWO$"
ERM23
        .BYTE   "OUT OF MEMORY$"
ERM24
        .BYTE   "ATTEMPT TO WRITE A QUOTE "
        .BYTE   "TO DISK$"
ERM25
        .BYTE   "DISK DATA FIELD TOO LONG "
        .BYTE   "DURING READ$"
ERM26
        .BYTE   "OVERFLOW IN ARITHMETIC "
        .BYTE   "OPERATION$"
ERM27
        .BYTE   "ILLEGAL TAB ARGUMENT$"
ERM28
        .BYTE   "ILLEGAL CHARACTER IN "
        .BYTE   "FILE NAME$"
;ERROR TABLE
ERMTBL
        .WORD   ERM0,ERM1,ERM2,ERM3,ERM4,ERM5,ERM6,ERM7
        .WORD   ERM8,ERM9,ERM10,ERM11,ERM12,ERM13,ERM14,ERM15
        .WORD   ERM16,ERM17,ERM18,ERM19,ERM20,ERM21,ERM22,ERM23
        .WORD   ERM24,ERM25,ERM26,ERM27,ERM28
;STRING ACCUMULATOR
ACCUM
        .RES    32
;INPUT BUFFER
INPBUF
        .BYTE   80
INPSPC
        .RES    81
;NUMERIC OUT WORK AREA
NUOWRK
        .BYTE   0
;PRINT WORK AREA
PRNWRK
        .RES    14
;TEMP NUMBER STORAGE LOCATIONS
T0
        .RES    4
T1
        .RES    4
T2
        .RES    4
T3
        .RES    4
;FCB POINTER ARRAYS
FILES
        .RES    NUMFIL*2
EOFBRN  ;EOFBRANCH(NUMFILES)
        .RES    NUMFIL*2
MEMORY
;BUILD
;THE BUILD ROUTINE GAINS CONTROL AT INITIAL EXECUTION OF RUN.COM.  IT
;INITIALIZES PAGE ZERO STORAGE AND THEN OPENS THE .INT FILE AND BUILDS
;THE BASIC-E/65 MACHINE IN THE FOLLOWING ORDER.
;	(1)	NUMERIC CONSTANTS ARE READ FROM THE .INT FILE, CONVERTED TO
;		INTERNAL REPRESENTATION (I.E. FOUR BYTE FLOATING POINT) AND
;		STORED IN THE FSA.
;	(2)	THE SIZE OF THE CODE AREA, DATA AREA, AND NUMBER OF PRT ENTRIES
;		ARE READ FROM THE .INT FILE.  BUILD THEN DETERMINES THE
;		ABSOLUTE ADDRESS OF EACH SECTION OF THE BASIC-E/65 MACHINE.
;		THESE ADDRESSES ARE PASSED TO THE MAIN INTERPRETER VIA FIXED
;		PAGE ZERO LOCATIONS.
;	(3)	FINALLY INSTRUCTIONS ARE READ FROM THE FILE AND PLACED IN
;		EITHER THE DATA AREA OR THE CODE AREA.  IN THE CASE OF BRS,
;		BRC, PRO, CON, AND DEF OPERATORS THE ADDRESS FOLLOWING THE
;		INSTRUCTION IS RELOCATED TO REFLECT ACTUAL MACHINE ADDRESSES
;		(MINUS 1 BECAUSE PROGRAM COUNTER GETS INCREMENTED PRIOR TO USE
;		(EXCEPT FOR CON) !!) AFTER (REAPEAT AFTER) THE MACHINE HAS BEEN
;		REPOSITIONED BY THE MAIN INTERPRETER.  THE END OF THE .INT FILE
;		IS INDICATED BY A MACHINE INSTRUCTION OF $7F.
;OPEN$INT$FILE
OPNINT
        LDA     #'I'            ;SET EXTENSION TO INT
        STA     DFLFCB+9
        LDA     #'N'
        STA     DFLFCB+10
        LDA     #'T'
        STA     DFLFCB+11
        LDA     #<DFLFCB
        LDY     #>DFLFCB
        LDX     #15
        JSR     PEM             ;TRY TO OPEN
        BMI     *+3             ;IF ERROR
        RTS
        LDA     #13             ;NO INT FILE ERROR
        JMP     ERROR
;READ$INT$FILE
;NEXT RECORD IS READ FROM INT FILE
;RETURNS 0 IF OK OR A 1 IF EOF
RDEINT
        LDA     #<DFLFCB
        LDY     #>DFLFCB
        LDX     #20
        JMP     PEM
;
INCBUF
        INC     BUFF            ;BUMP INDEX
        BPL     BE1             ;DONE OF < 128
        LDA     #0
        STA     BUFF            ;ELSE CLEAR INDEX
        JSR     RDEINT          ;READ RECORD
        BEQ     BE1             ;DONE IF OK
        LDA     #$7F
        STA     BUF             ;ELSE CHAR:= $7F
BE1
        RTS
;STO$CHAR$INC
;GET NEXT CHAR FROM INT FILE AND PLACE IN CODE AREA THEN INCREMENT PTR INTO
;CODE AREA
STOINC
        LDY     BUFF
        LDA     BUF,Y
        LDY     #0
        STA     (BASE),Y        ;B:=CHAR
        INC     BASE
        BNE     *+4
        INC     BASE+1          ;BASE:=BASE+1
        RTS
;NEXT$CHAR
NXTCHR
        JSR     INCBUF
        LDY     BUFF
        LDA     BUF,Y
        STA     CURCHR          ;CURCHR:=CHAR
        RTS
;GET$TWO$BYTES
;GET NEXT TWO BYTES FROM THE INT FILE AND PLACE IN THE CODE AREA IN REVERSE
;ORDER
GETTWO
        JSR     NXTCHR
        LDY     #1
        STA     (BASE),Y        ;BV(1)=NEXT$CHAR
        JSR     NXTCHR
        LDY     #0
        STA     (BASE),Y        ;BV(0)=NEXT$CHAR
        RTS
;INC$BASE$TWO
INCB2
        CLC
        LDA     BASE
        ADC     #2
        STA     BASE
        BCC     *+4
        INC     BASE+1          ;BASE:=BASE+1+1
        RTS
;GETPARM
;READ A 16 BIT PARAMETER FROM INT FILE AND CONVERT IT TO A 6502 ADDRESS
;QUANTITY
GETPRM
        JSR     NXTCHR          ;HIGH
        PHA                     ;SAVE
        JSR     NXTCHR          ;LOW
        TAX                     ;SAVE IN X AS TEMP
        PLA
        TAY                     ;GET HIGH
        TXA                     ;AND LOW
        RTS
;MAIN BUILD ROUTINE
BUILD
;CLEAR ALL OF PAGE ZERO
        LDX     #2
        LDA     #0
CLRZP
        STA     0,X
        INX                     ;BUMP INDEX
        CPX     #LZ             ;SEE IF AT END
        BNE     CLRZP
        LDA     #127
        STA     BUFF            ;SET BUFF TO ILLEGAL
        LDX     #25
        JSR     PEM             ;READ DEFAULT DRIVE
        STA     DFLDRV          ;AND SAVE
        LDA     #<OPNMSG
        LDY     #>OPNMSG
        JSR     PRINT           ;PRINT OPENING
        JSR     CRLF
        JSR     OPNINT          ;OPEN INT FILE
        LDX     #>BMEM
        INX
        STX     BASE+1          ;BASE:=(.MEMORY+$100) AND $FF00
        SEC
        LDA     BASE
        SBC     #<MEMORY
        STA     OFFSET
        LDA     BASE+1
        SBC     #>MEMORY
        STA     OFFSET+1        ;OFFSET:=BASE-.MEMORY
;INITIALIZE FLOATING POINT PACKAGE - PAGE ZERO CLEAR DID IT
;PROCESS CONSTANTS - EACH CONSTANT IS TERMINATED BY AN  $ AND THE LAST IS
;FOLLOWED BY A *
BLDCON
        JSR     NXTCHR
        STA     ACCUM           ;ACCUM(0):=NEXT$CHAR
        CMP     #'*'
        BEQ     ENDCON          ;DONE IF *
        LDA     #0
        STA     AP              ;AP:=0
;DO ONE CONSTANT
BLD1C
        INC     AP              ;AP:=AP+1
        JSR     NXTCHR
        LDY     AP
        STA     ACCUM,Y         ;ACCUM(AP:=AP+1):=NEXT$CHAR
        CMP     #'$'
        BNE     BLD1C           ;DO WHILE <> $
        LDA     #<ACCUM         ;(ACTUALLY MOVES DATA FROM ACCUM TO ACCUM)
        LDY     #>ACCUM
        LDX     AP
        JSR     FLTINP          ;CONVERT TO FLOATING
        LDA     BASE
        LDY     BASE+1
        JSR     STP             ;STORE AT (BASE)
        CLC
        LDA     BASE
        ADC     #4
        STA     BASE
        BCC     BLDCON
        INC     BASE+1          ;BASE:=BASE+4
        JMP     BLDCON
;SETUP MACHINE ADDRESS
;BASE WILL NOW BE NEXT POSITION IN CODE AREA
;MBASE WILL BE NEXT POSITION IN DATA AREA
ENDCON
        JSR     GETPRM
        CLC
        ADC     BASE
        STA     MBASE
        TYA
        ADC     BASE+1
        STA     MBASE+1         ;MBASE:=GETPARM+BASE
;ACTUAL DATA AREA ADDR
        SEC
        LDA     MBASE
        SBC     OFFSET
        STA     MDA
        LDA     MBASE+1
        SBC     OFFSET+1
        STA     MDA+1           ;MDA:=MBASE-OFFSET
;ACTUAL CODE AREA ADDR
        SEC
        LDA     BASE
        SBC     OFFSET
        STA     MCD
        LDA     BASE+1
        SBC     OFFSET+1
        STA     MCD+1           ;MCD:=BASE-OFFSET
;ACTUAL BEGINNING OF PRT
        JSR     GETPRM
        CLC
        ADC     MDA
        STA     MPR
        TYA
        ADC     MDA+1
        STA     MPR+1           ;MPR:=GETPARM+MDA
;ENSURE THERE IS ENOUGH MEMORY
        LDA     MPR
        CMP     PEM+1
        LDA     MPR+1
        SBC     PEM+2
        BCC     *+7             ;OK IF MPR<MAX
        LDA     #23
        JMP     ERROR           ;OUT OF MEMORY ERROR
        JSR     GETPRM
        STA     SB
        STY     SB+1
        ASL     SB
        ROL     SB+1
        ASL     SB
        ROL     SB+1            ;SHL(GETPARM,2)
        CLC
        LDA     SB
        ADC     MPR
        STA     SB
        LDA     SB+1
        ADC     MPR+1
        STA     SB+1            ;SB:=SHL(GETPARM,2)+MPR
;BUILD MACHINE AT LAST
;AS OPCODES ARE READ THEY MAY BE:
;	(1)	DAT-WHICH MEANS ALL CHARACTERS FOLLOWING DAT GO INTO DATA AREA
;		UNTIL A BINARY ZERO IS ENCOUNTERED
;	(2)	GREATER THAN 127 - WHICH IS A LIT OR LID.  TREAT THIS AS A 16
;		OPCODE AN PUT IN CODE AREA IN ORDER THEY ARE IN INT FILE
;	(3)	IRS - WHICH MEANS ALL CHARACTERS FOLLOWING GO INTO CODE AREA
;		UNTIL A BINARY ZERO IS ENCOUNTERED - BUT FIRST PUT AN ILS IN
;		CODE AREA AND THE NEXT BYTE IS SET TO ZERO AND INCREMENTED
;		FOR EACH CHARCETR IN THE STRING.  THUS A STRING CONSTANT IS AN
;		ILS OPCODE, A LENGTH, AND A STRING
;	(4)	A NORMAL OPCODE - PUT IN CODE AREA - BUT IF IT IS A BRS, BRC,
;		DEF, OR PRO THEN THE NEXT TWO BYTES ARE AN ADDRESS WHICH MUST
;		BE RELOCATED TO THE ACTUAL CODE AREA MINUS 1 - IT COULD ALSO
;		BE A CON WHICH IS RELOCATED TO THE FDA.
BLDMCH
        JSR     NXTCHR
        CMP     #$7F
        BNE     *+5             ;DO WHILE NEXT$CHAR <> $7F
        JMP     MAIN            ;ELSE GO TO MAIN
;SEE IF DAT
        CMP     #DAT            ;IF CURCHAR <> DAT
        BNE     NOTDAT          ;THEN
BLDDAT
        JSR     NXTCHR
        LDY     #0
        STA     (MBASE),Y       ;MF:=NEXT$CHAR
        CMP     #0              ;DO WHILE () <> 0
        BEQ     BLDMCH          ;START OVER IF IS 0
        INC     MBASE
        BNE     BLDDAT
        INC     MBASE+1         ;MBASE:=MBASE+1
        JMP     BLDDAT
;SEE IF LIT OR LID
NOTDAT
        CMP     #128            ;IF CURCHAR < 128
        BCC     NOTLIT          ;THEN
        JSR     STOINC          ;ELSE STO$CHAR$INC
        JSR     INCBUF
        JSR     STOINC
        JMP     BLDMCH          ;AND LOOP
;SEE IF ILS
NOTLIT
        CMP     #ILS
        BNE     NOTILS
        JSR     STOINC          ;STO$CHAR$INC
        LDA     BASE
        LDY     BASE+1
        STA     TBASE
        STY     TBASE+1         ;TEMP:=BASE
        LDY     BUFF
        LDA     #0
        STA     BUF,Y           ;CHAR:=0
        JSR     STOINC          ;STO$CHAR$INC
BLDILS
        JSR     NXTCHR          ;DO WHILE NEXT$CHAR <> 0
        BEQ     BLDMCH
        JSR     STOINC          ;STO$CHAR$INC
        LDY     #0
        LDA     (TBASE),Y       ;T:=T+1
        CLC
        ADC     #1
        STA     (TBASE),Y
        JMP     BLDILS          ;LOOP FOR MORE
;IS AN OPCODE
NOTILS
        JSR     STOINC          ;STO$CHAR$INC
        LDA     CURCHR
        CMP     #BRS
        BEQ     DOADDR
        CMP     #BRC
        BEQ     DOADDR
        CMP     #DEF
        BEQ     DOADDR
        CMP     #PRO
        BNE     TRYCON          ;IF BRS OR BRC OR DEF OR PRO THEN ELSE
DOADDR
        JSR     GETTWO
        LDY     #0
        LDA     (BASE),Y        ;LOW
        TAX
        INY
        LDA     (BASE),Y
        TAY
        TXA
        CLC                     ;ADD MCD
        ADC     MCD
        TAX
        TYA
        ADC     MCD+1
        TAY
        TXA                     ;TEST LOW
        BNE     *+3
        DEY
        DEX                     ;DROP BY ONE
        TYA                     ;HIGH TO A
        LDY     #1
        STA     (BASE),Y
        DEY
        TXA
        STA     (BASE),Y        ;SET
        JSR     INCB2           ;INC$BASE$TWO
        JMP     BLDMCH
;MAY BE CON
TRYCON
        CMP     #CON
        BEQ     *+5             ;CONTINUE IF IS
        JMP     BLDMCH          ;ELSE GET NEXT
        JSR     GETTWO          ;GET$TWO$BYTES
        LDY     #0
        LDA     (BASE),Y
        ASL     A               ;LOW * 2
        TAX
        INY
        LDA     (BASE),Y
        ROL     A
        TAY
        TXA
        ASL     A
        TAX
        TYA
        ROL     A
        TAY
        CLC
        TXA
        ADC     #<MEMORY        ;ADD OFFSET
        TAX
        TYA
        ADC     #>MEMORY
        LDY     #1
        STA     (BASE),Y
        DEY
        TXA
        STA     (BASE),Y
        JSR     INCB2           ;INC$BASE$TWO
        JMP     BLDMCH          ;AND LOOP
;BUILD MESSAGES
OPNMSG
        .BYTE   "BASIC-E/65 INTERPRETER"
        .BYTE   " - VERSION 2.05-A$"
;BUILD MEMORY
BMEM
        .END
