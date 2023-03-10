;COPY
;VERSION 2.01-A
;RELEASED:	18 MAY 1996
;LAST REVISION:
;	27 MARCH 2008
;		REFORMATTED FOR TASM & ASM 2.10
;		ELIMINATED PAGE ZERO 0 & 1
;		CORRECTED REFERENCES TO SIMLNG TO CCMLNG
;PROGRAM TO COPY FILES FROM ONE LOCATION TO ANOTHER.
;SYNTAX IS
;	COPY FROMUFN TOUFN
;WHERE THE TOUFN MAY SIMPLY BE A DRIVE
;SPECIFICATION IN WHICH CASE THE DESTINATION
;WILL HAVE THE SAME NAME AS THE SOURCE.
;KEY DIFFERENCES BETWEEN THIS AND PRIOR VERSIONS IS
;USE OF THE FROM TO SYNTAX AND INCORPORATION OF CODE
;TO ENSURE CCM IS PRESERVED SO A RTS CAN BE DONE
;AT THE END RATHER THAN A WARM BOOT.
;FIXED PARAMETERS
DFLFCB          = $107          ;DEFAULT FCB
PEM             = $103          ;PEM ENTRY
BOOT            = $100          ;WARM BOOT
TEA             = $800          ;EXECUTION ORG
CCMLNG          = 2048          ;CCM LENGTH
;PAGE ZERO VARIABLES
NAMED           = $02           ;"TO" FILE NAMED IF <128
TOIND           = $03           ;"TO" FILE INDEX
FRMIND          = $05           ;"FROM" FILE INDEX
EOFFLG          = $07           ;EOF FLAG IF >127
BUFLNG          = $08           ;BUFFER LENGTH
;MAIN PROGRAM
        .FEATURE labels_without_colons
        .SEGMENT "TEA"
        .ORG    $0800
        SEC                     ;CALCULATE
        LDA     PEM+1           ;AMOUNT
        SBC     #(BUFFER & $ff) ;OF FREE MEMORY
        AND     #$80            ;IN MOD 128
        STA     BUFLNG          ;AND
        LDA     PEM+2           ;SAVE
        SBC     #((BUFFER / $100)& $FF);FOR
        STA     BUFLNG+1        ;READ/WRITE
        SEC                     ;NOW BACK UP BEFORE SIM
        LDA     BUFLNG
        SBC     #(CCMLNG*256/256) & $FF
        STA     BUFLNG
        LDA     BUFLNG+1
        SBC     #CCMLNG/256
        STA     BUFLNG+1
        CLC                     ;NOW
        LDA     BUFLNG          ;ADD
        ADC     #(BUFFER & $ff) ;START
        STA     BUFLNG          ;BACK
        LDA     BUFLNG+1        ;TO
        ADC     #((BUFFER / $100)& $FF);GET
        STA     BUFLNG+1        ;MAX INDEX
;CHECK FOR AMBIGUOUS NAMES
        LDA     #'?'            ;GET A ?
        LDX     #11             ;SET INDEX
TSTAMB
        CMP     DFLFCB,X        ;TEST TO
        BNE     *+5             ;OK IF NOT
        JMP     BADFIL          ;ELSE ERROR
        CMP     DFLFCB+16,X     ;NOW TRY FFROM
        BNE     *+5             ;ALSO OK IF NOT
        JMP     BADFIL          ;ELSE ERROR
        DEX                     ;DROP INDEX
        BNE     TSTAMB          ;LOOP IF MORE
;NOW MAKE SURE SOURCE IS AMED
        LDA     #' '            ;GET A SPACE
        CMP     DFLFCB+1        ;TRY NAME
        BNE     NOTZIP          ;OK IF DIFFERENT
        CMP     DFLFCB+9        ;NOW TRY TYPE
        BNE     NOTZIP          ;ALSO OK IF DIFF
        JMP     BADFIL          ;ELSE ERROR
;NOW SEE IF DESTINATION NOT NAMED
NOTZIP
        STX     NAMED           ;SAY NAMED FOR NOW
        CMP     DFLFCB+17       ;TRY NAME
        BNE     ISNAMD          ;OK IF DIFF
        CMP     DFLFCB+25       ;NOW TRY TYPE
        BNE     ISNAMD          ;ALSO OK IF DIFF
        DEC     NAMED           ;ELSE MAKE NEG
;SET UP FCBS
ISNAMD
        LDX     #11             ;SET INDEX
MOVNME
        LDA     DFLFCB,X        ;GET FROM
        STA     FRMFCB,X        ;AND SET
        BIT     NAMED           ;TEST FLAG
        BMI     NMESME          ;BRANCH IF NO NAME
        LDA     DFLFCB+16,X     ;ELSE GET TO
NMESME
        STA     TOFCB,X         ;SAVE "TO" NAME
        STA     ENDFCB,X        ;AND IN END
        DEX                     ;DROP COUNT
        BNE     MOVNME          ;LOOP IF MORE
;MAKE SURE DRIVES ARE RIGHT
        LDA     DFLFCB+16       ;GET TO
        STA     TOFCB           ;AND SET
        STA     ENDFCB          ;BOTH TO AND END
        LDA     DFLFCB          ;AND SAME
        STA     FRMFCB          ;FOR FROM
;CLEAR REST OF FCBS
        TXA                     ;CLEAR A
        LDX     #32             ;SET INDEX
CLRFCB
        STA     TOFCB,X         ;CLEAR
        STA     ENDFCB,X        ;ALL
        STA     FRMFCB,X        ;FCBS
        DEX                     ;DROP COUNT
        CPX     #11             ;SEE IF AT NAME
        BNE     CLRFCB          ;LOOP IF MORE
;MAKE "TO" A TEMP
        LDX     #3              ;SET INDEX
        LDA     #'$'            ;GET BYTE
TMPLPE
        STA     TOFCB+8,X       ;INSERT
        DEX                     ;DROP COUNT
        BNE     TMPLPE          ;LOOP IF MORE
;TRY TO OPEN SOURCE
        JSR     SFMFCB          ;POINT TO FCB
        JSR     OPNFIL          ;OPEN IT
        BPL     *+5             ;OK
        JMP     BADFIL          ;ELSE ERROR
;SETUP DESTINATION
        JSR     STOFCB          ;POINT TO "TO"
        JSR     DLTFIL          ;DELETE IT
        JSR     STOFCB          ;POINT AGAIN
        JSR     CRTFIL          ;MAKE IT
        BPL     *+5             ;OK
        JMP     BADFIL          ;ELSE ERROR
        JSR     STOFCB          ;ONE MORE TIME
        JSR     OPNFIL          ;AND OPEN
        BPL     *+5             ;OK
        JMP     BADFIL          ;ELSE ERROR
;DO THE MOVE
        LDA     #0              ;CLEAR EOF
        STA     EOFFLG          ;FLAG
DOMOVE
        JSR     RDEBUF          ;READ BUFFER
        JSR     WRTBUF          ;NOW WRITE IT
        BIT     EOFFLG          ;TEST FLAG
        BPL     DOMOVE          ;LOOP IF NOT
        JSR     STOFCB          ;POINT TO "TO"
        JSR     CLSFIL          ;CLOSE IT
        BPL     *+5             ;OK
        JMP     BADFIL          ;ELSE ERROR
;DELETE OLD "END" AND RENAME "TO"
        JSR     SENFCB          ;POINT TO IT
        JSR     DLTFIL          ;DELETE IT
        LDX     #11             ;NOW MOVE
RNMLPE
        LDA     ENDFCB,X        ;"END"
        STA     TOFCB+16,X      ;TO
        DEX                     ;"TO"
        BNE     RNMLPE          ;FCB
        JSR     STOFCB          ;POINT TO IT
        JSR     RNMFIL          ;AND RENAME
        RTS                     ;DONE
;SUBROUTINES
;OPEN FILE
OPNFIL
        LDX     #15
        JMP     PEM
;CLOSE FILE
CLSFIL
        LDX     #16
        JMP     PEM
;DELETE FILE
DLTFIL
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
;CREATE FILE
CRTFIL
        LDX     #22
        JMP     PEM
;RENAME FILE
RNMFIL
        LDX     #23
        JMP     PEM
;SET BUFFER
SETBUF
        LDX     #26
        JMP     PEM
;SET DEST FCB
STOFCB
        LDA     #(TOFCB & $ff)
        LDY     #((TOFCB / $100)& $FF)
        RTS
;SET SOURCE FCB
SFMFCB
        LDA     #(FRMFCB & $ff)
        LDY     #((FRMFCB / $100)& $FF)
        RTS
;SET END FCB
SENFCB
        LDA     #(ENDFCB & $ff)
        LDY     #((ENDFCB / $100)& $FF)
        RTS
;DISK ERROR EXIT
BADFIL
        LDA     #(ERRMSG & $ff)
        LDY     #((ERRMSG / $100)& $FF)
        LDX     #9
        JSR     PEM
        JMP     BOOT
;READ BUFFER
;IF EOF THEN SET FLAG
RDEBUF
        LDA     #(BUFFER & $ff) ;SET
        LDY     #((BUFFER / $100)& $FF);INDEX
        STA     FRMIND          ;TO BUFFER
        STY     FRMIND+1        ;START
RDELPE
        LDA     FRMIND          ;GET INDEX
        LDY     FRMIND+1        ;AND
        JSR     SETBUF          ;SET BUFFER
        JSR     SFMFCB          ;POINT TO FCB
        JSR     RDERCR          ;READ A RECORD
        BEQ     RDEOK           ;OK IF ZERO
        BPL     *+5             ;EOF IF POSITIVE
        JMP     BADFIL          ;ELSE ERROR
        DEC     EOFFLG          ;SET FLAG
        RTS                     ;AND QUIT
RDEOK
        CLC                     ;NOW
        LDA     FRMIND          ;ADD
        ADC     #128            ;128
        STA     FRMIND          ;TO
        BCC     *+4             ;INDEX
        INC     FRMIND+1        ;FOR NEXT
        CMP     BUFLNG          ;COMPARE TO LOW
        BNE     RDELPE          ;LOOP IF OK
        LDA     FRMIND+1        ;GET HIGH
        CMP     BUFLNG+1        ;COMPARE IT
        BNE     RDELPE          ;ALSO LOOP IF MORE
        RTS
;WRITE BUFFER
WRTBUF
        LDA     #(BUFFER & $ff) ;SET
        LDY     #((BUFFER / $100)& $FF);INDEX
        STA     TOIND           ;TO
        STY     TOIND+1         ;START
WRTLPE
        LDA     TOIND           ;GET CURRENT
        LDY     TOIND+1         ;INDEX
        JSR     SETBUF          ;AND SET
        LDA     TOIND           ;COMPARE INDEX
        CMP     FRMIND          ;TO READ
        BNE     DOWRT           ;OK IF DIFF
        LDA     TOIND+1         ;DO SAME
        CMP     FRMIND+1        ;FOR HIGH
        BNE     DOWRT           ;INDEX
        RTS                     ;ELSE DONE
DOWRT
        JSR     STOFCB          ;POINT TO FCB
        JSR     WRTRCR          ;WRITE RECORD
        BEQ     *+5             ;OK
        JMP     BADFIL          ;ELSE ERROR
        CLC                     ;NOW
        LDA     TOIND           ;ADD
        ADC     #128            ;128
        STA     TOIND           ;TO INDEX
        BCC     WRTLPE          ;WITH
        INC     TOIND+1         ;CARRY
        BNE     WRTLPE          ;AND LOOP
;MESSAGES
ERRMSG
        .BYTE   "DOS ERROR - ABORTING$"
;SOURCE FCB
FRMFCB
        .RES    33
;DESTINATION FCB
TOFCB
        .RES    33
;END FCB
ENDFCB
        .RES    33
;BUFFER
BUFFER
        .END
