;FILE COMPARISON ROUTINE
;THE COMMAND SYNTAX FOR THIS ROUTINE IS
;	COMPARE FILE1 FILE2
;WHERE FILE1 AND FILE2 ARE BOTH UFNS.
;VERSION 2.02-
;RELEASED:	18 JULY 1983
;LAST REVISION:
;	28 DECEMBER 1985
;		INCREASED BYTE COUNT TO 3
;	31 MARCH 2008
;		CONVERTED TO TASM ASM21X FORMAT
;		ELIMINATED PAGE ZERO 0 & 1
;DEFINITIONS
CR              = $D            ;CARRIAGE RETURN
LF              = $A            ;LINEFEED
EOF             = $1A           ;EOF CHARACTER
BOOT            = $100          ;WARM BOOT ENTRY
PEM             = $103          ;PEM ENTRY
DFLFCB          = $107          ;DEFAULT FCB
DFLBUF          = $128          ;DEFAULT BUFFER
TEA             = $800          ;TEA START
BUFLNG          = 1024          ;FILE BUFFER LENGTH
;PAGE ZERO DATA DEFINITIONS
F1PNT           = $02           ;POINTER INTO FILE 1
F1EOF           = $04           ;ONE PAST LAST VALID BYTE
F2PNT           = $06           ;POINTER INTO FILE 2
F2EOF           = $08           ;ONE PAST LAST VALID BYTE
EOFFLG          = $0A           ;EOF FLAG
BYTCNT          = $0B           ;BYTE COUNTER
LEADZF          = $0E           ;LEADING ZERO FLAG
MODULS          = $0F           ;DIGIT MODULUS
BYTE1           = $12           ;BYTE FROM FILE ONE
BYTE2           = $13           ;BYTE FROM FILE TWO
;START OF MAIN PROGRAM
        .FEATURE labels_without_colons
        .SEGMENT "TEA"
        .ORG    $0800

        LDA     #0              ;CLEAR EOF FLAG
        STA     EOFFLG
        STA     BYTCNT          ;AND BYTE COUNT
        STA     BYTCNT+1
        STA     BYTCNT+2
        LDA     #(F1MAX & $FF)  ;SET POINTERS TO ILLEGAL
        LDY     #((F1MAX / $100)& $FF)
        STA     F1PNT
        STY     F1PNT+1
        STA     F1EOF           ;ALSO PRESET EOF POINTER
        STY     F1EOF+1
        LDA     #(F2MAX & $FF)
        LDY     #((F2MAX / $100)& $FF)
        STA     F2PNT
        STY     F2PNT+1
        STA     F2EOF
        STY     F2EOF+1
        LDA     #(OPNMSG & $FF) ;POINT TO OPENING MESSAGE
        LDY     #((OPNMSG / $100)& $FF)
        JSR     MSGOUT          ;SEND IT
        LDX     #0              ;MOVE DEFAULT FCB DATA
MVFCBS
        LDA     DFLFCB,X        ;GET FILE 1
        STA     F1FCB,X
        LDA     DFLFCB+16,X     ;THEN GET FILE 2
        STA     F2FCB,X
        INX                     ;BUMP INDX
        CPX     #12             ;SEE IF AT LIMIT
        BNE     MVFCBS          ;LOOP IF NOT
        LDA     #0              ;CLEAR REST OF FCB
CLFCBS
        STA     F1FCB,X         ;BOTH 1
        STA     F2FCB,X         ;AND 2
        INX
        CPX     #16             ;UNTIL END
        BNE     CLFCBS
        LDA     F2FCB+1         ;NOW SEE IF FCB2 IS BLANK
        CMP     #' '
        BNE     NTBLNK          ;BRANCH IF NOT
        LDA     F2FCB+9         ;CHECK TYPE ALSO
        CMP     #' '            ;BRANCH IF NOT BLANK
        BNE     NTBLNK
        LDX     #1              ;NOW MOVE FCB ONE TO TWO
CPFCB
        LDA     F1FCB,X         ;GET FROM ONE
        STA     F2FCB,X         ;PUT IN X
        INX                     ;BUMP INDEX
        CPX     #12             ;SEE IF AT LIMIT
        BNE     CPFCB           ;LOOP IF NOT
NTBLNK
        JSR     OPEN1           ;OPEN FIRST
        JSR     OPEN2           ;THEN SECOND
COMPAR
        JSR     READ1           ;GET BYTE FROM ONE
        JSR     READ2           ;GET BYTE FROM TWO
        CMP     BYTE1           ;COMPARE THE BYTES
        BEQ     COMPAR          ;IF SAME KEEP LOOPING
UNEQUL
        LDA     #(UNEMSG & $FF) ;SEND UNEQUAL MESSAGE
        LDY     #((UNEMSG / $100)& $FF)
        JSR     MSGOUT
        JSR     OUTDEC          ;SEND COUNT
        LDA     #(DECMSG & $FF) ;THEN SEND DECIMAL MESSAGE
        LDY     #((DECMSG / $100)& $FF)
        JSR     MSGOUT
        JMP     BOOT            ;EXIT WITH BOOT
;SUBROUTINES
;BYTE READ ROUTINES
READ1
        JSR     F1GET           ;GET BYTE
        BCS     EOF1            ;BRANCH IF EOF
        INC     BYTCNT          ;ELSE BUMP COUNT
        BNE     READ1X          ;EXIT IF NOT ZERO
        INC     BYTCNT+1
        BNE     READ1X          ;EXIT IF NOT ZERO
        INC     BYTCNT+2
READ1X
        RTS
;EOF OF FILE 1
EOF1
        LDA     #128            ;SET FLAG
        STA     EOFFLG
        JSR     READ2           ;NOW READ FROM TWO
        LDA     #(EF1MSG & $FF) ;SEND EOF 1 NOT EOF 2
        LDY     #((EF1MSG / $100)& $FF)
        JSR     MSGOUT
        JMP     UNEQUL          ;THEN SAY UNEQUAL
;FILE TWO
READ2
        JSR     F2GET           ;GET BYTE
        BCC     R2OK            ;EXIT IF OK
;GOT EOF ON FILE 2 - DID WE GET IT ON FILE ONE
        BIT     EOFFLG          ;TEST FLAG
        BMI     AOK             ;IF SET IS OK
        LDA     #(EF2MSG & $FF) ;ELSE SEND EOF 2 NOT 1
        LDY     #((EF2MSG / $100)& $FF)
        JSR     MSGOUT
        JMP     UNEQUL          ;THEN SAY UNEQUAL
;FILES MATCH
AOK
        LDA     #(MCHMSG & $FF) ;SO SEND MESSAGE
        LDY     #((MCHMSG / $100)& $FF)
        JSR     MSGOUT
        JSR     OUTDEC          ;PRINT LENGTH
        JMP     BOOT
R2OK
        RTS
;CONVERT THREE BINARY BYTES TO DECIMAL
OUTDEC
        LDA     #0              ;CLEAR LEADING ZERO FLAG
        STA     LEADZF
;SINCE ASSEMBLER CAN ONLY DO 16 BIT ARITHMETIC, THE
;FOLLOWING NUMBERS ARE FOR 100000.
        LDA     #$A0            ;100000 AND FF
        LDY     #$86            ;(100000/256) AND FF
        LDX     #$01            ;100000/65536
        JSR     DODIGT
        LDA     #(10000 & $FF)  ;DO 10000S
        LDY     #((10000 / $100)& $FF)
        LDX     #0
        JSR     DODIGT
        LDA     #(1000 & $FF)   ;THEN 1000S
        LDY     #((1000 / $100)& $FF)
        LDX     #0
        JSR     DODIGT
        LDA     #100            ;THEN 100S
        LDY     #0
        LDX     #0
        JSR     DODIGT
        LDA     #10             ;THEN 10S
        LDY     #0
        LDX     #0
        JSR     DODIGT
        LDA     BYTCNT          ;THEN ALWAYS DO 1S
        ORA     #'0'
        JMP     CONOUT
;DO A SINGLE DIGIT
DODIGT
        STA     MODULS          ;SAVE MODULUS
        STY     MODULS+1
        STX     MODULS+2
        LDX     #$FF            ;CLEAR DIGIT
        SEC
DIGLPE
        LDA     BYTCNT          ;SUBTRACT UNTIL BORROW
        SBC     MODULS
        STA     BYTCNT
        LDA     BYTCNT+1
        SBC     MODULS+1
        STA     BYTCNT+1
        LDA     BYTCNT+2
        SBC     MODULS+2
        STA     BYTCNT+2
        INX                     ;BUMP DIGIT
        BCS     DIGLPE          ;LOOP IF NO BORROW
        LDA     BYTCNT          ;NOW ADD MODULUS BACK
        ADC     MODULS
        STA     BYTCNT
        LDA     BYTCNT+1
        ADC     MODULS+1
        STA     BYTCNT+1
        LDA     BYTCNT+2
        ADC     MODULS+2
        STA     BYTCNT+2
        TXA                     ;TEST DIGIT
        BEQ     DIGZRO          ;SKIP IF ZERO
        SEC                     ;ELSE SET FLAG
        ROR     LEADZF
DIGZRO
        BIT     LEADZF          ;TEST FLAG
        BPL     DONTPR          ;SKIP IF CLEAR
        ORA     #'0'            ;ELSE MAKE ASCII
        JSR     CONOUT          ;AND SEND
DONTPR
        RTS
;PEM ENTRIES
;READ FILE ONE
F1RDE
        LDA     #(F1FCB & $FF)  ;POINT TO FCB
        LDY     #((F1FCB / $100)& $FF)
        JMP     RDEFLE
;READ FILE TWO
F2RDE
        LDA     #(F2FCB & $FF)  ;POINT TO FCB
        LDY     #((F2FCB / $100)& $FF)
RDEFLE
        LDX     #20
        JMP     PEM
;OUTPUT CHAR TO CONSOLE
CONOUT
        LDX     #2
        JMP     PEM
;SEND MESSAGE TO CONSOLE
MSGOUT
        LDX     #9              ;GET OPERATION NUMBER
        JMP     PEM             ;GO DO IT
;SET DMA ADDRESS TO VALUE IN AY
SETDMA
        LDX     #26
        JMP     PEM
;OPEN FILE 1
OPEN1
        LDA     #(F1FCB & $FF)  ;POINT TO FCB
        LDY     #((F1FCB / $100)& $FF)
        JMP     OPNFLE          ;THEN GO OPEN
;OPEN FILE 2
OPEN2
        LDA     #(F2FCB & $FF)  ;POINT TO FCB
        LDY     #((F2FCB / $100)& $FF)
;OPEN FILE
OPNFLE
        LDX     #15             ;OPEN CODE
        JSR     PEM
        BPL     OPNIOK          ;OK IF POSITIVE
        LDA     #(NFLMSG & $FF) ;SAY NO FILE
        LDY     #((NFLMSG / $100)& $FF)
        JSR     MSGOUT
        JMP     BOOT            ;AND EXIT
OPNIOK
        RTS
;GET BYTE FROM FILE ONE AND SET CARRY IF EOF
F1GET
        JSR     TF1PNT          ;TEST POINTER
        BCC     F1GNOW          ;IF SO GO GET BYTE
        JSR     CF1PNT          ;ELSE SET POINTER TO START
F1LPE
        LDA     F1PNT           ;SET ADDRESS
        LDY     F1PNT+1
        JSR     SETDMA
        JSR     F1RDE           ;THEN READ RECORD
        BNE     F1END           ;IF NOT ZERO IS EOF
        JSR     BF1PNT          ;BUMP POINTER
        JSR     TF1PNT          ;AND TEST POINTER
        BCC     F1LPE           ;LOOP IF MORE TO READ
F1CONT
        JSR     CF1PNT          ;ELSE CLEAR POINTER
F1GNOW
        LDY     #0              ;GET BYTE
        LDA     (F1PNT),Y
        STA     BYTE1           ;SAVE IN REGISTER
        LDA     F1PNT           ;NOW SEE IF AT EOF
        CMP     F1EOF
        LDA     F1PNT+1
        SBC     F1EOF+1
        INC     F1PNT           ;NOW BUMP POINTER
        BNE     *+4
        INC     F1PNT+1
        LDA     BYTE1           ;GET BYTE BACK
        RTS
;END OF FILE FOR FILE ONE
F1END
        LDA     F1PNT           ;SET EOF POINTER
        LDY     F1PNT+1
        STA     F1EOF
        STY     F1EOF+1
        JMP     F1CONT
;GET BYTE FROM FILE TWO AND SET CARRY IF EOF
F2GET
        JSR     TF2PNT          ;TEST POINTER
        BCC     F2GNOW          ;IF SO GO GET BYTE
        JSR     CF2PNT          ;ELSE SET POINTER TO START
F2LPE
        LDA     F2PNT           ;SET ADDRESS
        LDY     F2PNT+1
        JSR     SETDMA
        JSR     F2RDE           ;THEN READ RECORD
        BNE     F2END           ;IF NOT ZERO IS EOF
        JSR     BF2PNT          ;BUMP POINTER
        JSR     TF2PNT          ;AND TEST POINTER
        BCC     F2LPE           ;LOOP IF MORE TO READ
F2CONT
        JSR     CF2PNT          ;ELSE CLEAR POINTER
F2GNOW
        LDY     #0              ;GET BYTE
        LDA     (F2PNT),Y
        STA     BYTE2           ;SAVE IN REGISTER
        LDA     F2PNT           ;NOW SEE IF EOF
        CMP     F2EOF
        LDA     F2PNT+1
        SBC     F2EOF+1
        INC     F2PNT           ;NOW BUMP POINTER
        BNE     *+4
        INC     F2PNT+1
        LDA     BYTE2           ;GET BYTE BACK
        RTS
;END OF FILE FOR FILE TWO
F2END
        LDA     F2PNT           ;SET EOF POINTER
        LDY     F2PNT+1
        STA     F2EOF
        STY     F2EOF+1
        JMP     F2CONT
;TEST FILE ONE POINTER AND SET CARRY IF TOO BIG
TF1PNT
        LDA     F1PNT           ;DO BY SUBTRACTING
        CMP     #(F1MAX & $FF)
        LDA     F1PNT+1
        SBC     #((F1MAX / $100)& $FF)
        RTS
;TEST FILE TWO POINTER AND SET CARRY IF TOO BIG
TF2PNT
        LDA     F2PNT           ;DO BY SUBTRACTING
        CMP     #(F2MAX & $FF)
        LDA     F2PNT+1
        SBC     #((F2MAX / $100)& $FF)
        RTS
;SET FILE ONE POINTER TO START
CF1PNT
        LDA     #(F1BUF & $FF)  ;GET START
        LDY     #((F1BUF / $100)& $FF)
        STA     F1PNT           ;THEN SET
        STY     F1PNT+1
        RTS
;SET FILE TWO POINTER TO START
CF2PNT
        LDA     #(F2BUF & $FF)  ;GET START
        LDY     #((F2BUF / $100)& $FF)
        STA     F2PNT           ;AND SET
        STY     F2PNT+1
        RTS
;BUMP FILE ONE POINTER BY 128
BF1PNT
        CLC                     ;CLEAR CARRY
        LDA     F1PNT
        ADC     #128
        STA     F1PNT           ;SAVE LOW
        BCC     *+4
        INC     F1PNT+1         ;DO HIGH IF NEEDED
        RTS
;BUMP FILE TWO POINTER BY 128
BF2PNT
        CLC                     ;CLEAR CARRY
        LDA     F2PNT
        ADC     #128
        STA     F2PNT           ;SAVE LOW
        BCC     *+4
        INC     F2PNT+1         ;DO HIGH IF NEEDED
        RTS
;MESSAGES
OPNMSG
        .BYTE   "FILE COMPARISON ROUTINE"
        .BYTE   CR,LF,"VERSION 2.02-A$"
NFLMSG
        .BYTE   CR,LF,"ONE OF THE FILES "
        .BYTE   "DOES NOT EXIST!$"
CRLMSG
        .BYTE   CR,LF,"$"
MCHMSG
        .BYTE   CR,LF,"FILES MATCH - LENGTH IS $"
DECMSG
        .BYTE   " (DECIMAL)$"
UNEMSG
        .BYTE   CR,LF,"FILES ARE UNEQUAL AT BYTE $"
EF1MSG
        .BYTE   CR,LF,"EOF ON FILE 1 BUT"
        .BYTE   " NOT FILE 2$"
EF2MSG
        .BYTE   CR,LF,"EOF ON FILE 2 BUT"
        .BYTE   " NOT FILE 1$"
;FCBS
F1FCB   ;FILE 1
        .RES    32
        .BYTE   0               ;RECORD
F2FCB   ;FILE 2
        .RES    32
        .BYTE   0
;BUFFERS
F1BUF   ;FILE 1
        .RES    BUFLNG
F1MAX
F2BUF   ;FILE 2
        .RES    BUFLNG
F2MAX
        .END
