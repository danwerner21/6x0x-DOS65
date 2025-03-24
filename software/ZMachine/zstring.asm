;        PAGE
;        SBTTL   "--- Z-STRING HANDLERS ---"

; -----------------------
; POINT TO ZSTRING IN [I]
; -----------------------

SETSTR:
        LDA     I+LO            ; WORD-ALIGN THE ADDRESS
        ASL     A
        STA     MPCL
        LDA     I+HI
        ROL     A
        STA     MPCM
        LDA     #0
        STA     MPCFLG          ; [MPC] IS CHANGING!
        ROL     A
        STA     MPCH

ZSTEX:
        RTS

; -----------------------
; PRINT Z-STRING AT [MPC]
; -----------------------

PZSTR:
        LDX     #0
        STX     PSET            ; ASSUME PERMANENT CHARSET
        STX     ZFLAG           ; CLEAR BYTE FLAG
        DEX                     ; = $FF
        STX     TSET            ; NO TEMPSET ACTIVE

PZTOP:
        JSR     GETZCH          ; GET A Z-CHAR
        BCS     ZSTEX           ; END OF STRING IF CARRY IS SET

        STA     ZCHAR           ; ELSE SAVE CHAR HERE
        TAX                     ; SET FLAGS
        BEQ     BLANK           ; PRINT SPACE IF CHAR = 0

        CMP     #4              ; IS THIS AN F-WORD?
        BCC     DOFREQ          ; APPARENTLY SO

        CMP     #6              ; PERHAPS A SHIFT CODE?
        BCC     NEWSET          ; YES, CHANGE CHARSETS

        JSR     GETSET          ; ELSE GET CHARSET
        TAX                     ; SET FLAGS
        BNE     SET1            ; SKIP IF NOT CHARSET #0

; PRINT A LOWER-CASE CHAR (CHARSET #0)

        LDA     #$61-6          ; ASCII "a" MINUS Z-OFFSET

TOASC:
        CLC
        ADC     ZCHAR           ; ADD Z-CHAR INDEX

SHOVE:
        JSR     COUT            ; SHOW THE CHAR
        JMP     PZTOP           ; AND GRAB NEXT CHAR

; PRINT AN UPPER-CASE CHAR (CHARSET #1)

SET1:
        CMP     #1              ; MAKE SURE IT'S SET #1
        BNE     SET2            ; ELSE MUST BE SET #2

        LDA     #$41-6          ; ASCII "A" MINUS Z-OFFSET
        BNE     TOASC           ; SAME AS SET #0

; PRINT FROM CHARSET #2

SET2:
        LDA     ZCHAR           ; RETRIEVE THE Z-CHAR
        SEC
        SBC     #6              ; ZERO-ALIGN IT
        BEQ     DIRECT          ; IF ZERO, IT'S A "DIRECT" ASCII

        TAX                     ; OTHERWISE USE CODE AS AN INDEX
        LDA     CHRTBL,X        ; INTO THE CHARSET TABLE
        JMP     SHOVE           ; AND PRINT THE CHAR

; DECODE A "DIRECT" ASCII CHAR

DIRECT:
        JSR     GETZCH          ; FETCH NEXT Z-CHAR
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        ASL     A               ; SHIFT INTO POSITION
        STA     ZCHAR           ; AND SAVE HERE
        JSR     GETZCH          ; GRAB YET ANOTHER Z-CHAR
        ORA     ZCHAR           ; SUPERIMPOSE THE 2ND BYTE
        JMP     SHOVE           ; AND PRINT THE RESULT

; PRINT A SPACE

BLANK:
        LDA     #SPACE          ; ASCII SPACE CHAR
        BNE     SHOVE

; CHANGE CHARSET

NEWSET:
        SEC                     ; CONVERT THE SHIFT CODE
        SBC     #3              ; TO 1 OR 2
        TAY
        JSR     GETSET          ; IS MODE TEMPORARY?
        BNE     TOPERM          ; YES, DO A PERMSHIFT
        STY     TSET            ; ELSE JUST A TEMPSHIFT
        JMP     PZTOP           ; AND CONTINUE

TOPERM:
        STY     PSET            ; SET PERM CHARSET
        CMP     PSET            ; SAME AS BEFORE?
        BEQ     PZTOP           ; YES, CONTINUE
        LDA     #0
        STA     PSET            ; ELSE RESET CHARSET
        BEQ     PZTOP           ; BEFORE LOOPING BACK

; PRINT AN F-WORD

DOFREQ:
        SEC
        SBC     #1              ; ZERO-ALIGN THE CODE
        ASL     A               ; AND MULTIPLY TIMES 64
        ASL     A               ; TO OBTAIN THE SEGMENT OFFSET
        ASL     A               ; INTO THE F-WORDS TABLE
        ASL     A
        ASL     A
        ASL     A
        STA     OFFSET          ; SAVE OFFSET FOR LATER

        JSR     GETZCH          ; NOW GET THE F-WORD POINTER
        ASL     A               ; WORD-ALIGN IT
        CLC                     ; AND
        ADC     OFFSET          ; ADD THE SEGMENT OFFSET
        TAY                     ; TO GET THE OFFSET OF THE F-WORD
        LDA     (FWORDS),Y      ; FROM THE START OF THE F-WORDS TABLE
        STA     I+HI            ; SAVE MSB OF F-WORD ADDRESS
        INY
        LDA     (FWORDS),Y      ; ALSO SAVE LSB
        STA     I+LO            ; Z-ADDRESS OF F-WORD IS IN [I]

; SAVE THE STATE OF CURRENT Z-STRING

        LDA     MPCH
        PHA
        LDA     MPCM
        PHA
        LDA     MPCL
        PHA
        LDA     PSET
        PHA
        LDA     ZFLAG
        PHA
        LDA     ZWORD+HI
        PHA
        LDA     ZWORD+LO
        PHA

        JSR     SETSTR          ; PRINT THE Z-STRING
        JSR     PZSTR           ; IN [I]

; RESTORE OLD Z-STRING

        PLA
        STA     ZWORD+LO
        PLA
        STA     ZWORD+HI
        PLA
        STA     ZFLAG
        PLA
        STA     PSET
        PLA
        STA     MPCL
        PLA
        STA     MPCM
        PLA
        STA     MPCH

        LDX     #$FF
        STX     TSET            ; DISABLE TEMP CHARSET
        INX                     ; = 0
        STX     MPCFLG          ; [MPC] HAS CHANGED
        JMP     PZTOP           ; CONTINUE INNOCENTLY

; ----------------------
; RETURN CURRENT CHARSET
; ----------------------

GETSET:
        LDA     TSET
        BPL     GS
        LDA     PSET
        RTS

GS:
        LDY     #$FF
        STY     TSET
        RTS

; -----------------
; FETCH NEXT Z-CHAR
; -----------------

GETZCH:
        LDA     ZFLAG           ; WHICH BYTE IS THIS?
        BPL     GTZ0            ; $FF = LAST
        SEC                     ; SET CARRY TO INDICATE
        RTS                     ; NO MORE CHARS

GTZ0:
        BNE     GETZ1           ; NOT FIRST CHAR, EITHER

; GET A Z-WORD INTO [ZWORD], RETURN 1ST CHAR IN TRIPLET

        INC     ZFLAG           ; UPDATE CHAR COUNT

        JSR     GETBYT          ; GET TRIPLET AT [MPC]
        STA     ZWORD+HI        ; INTO [ZWORD]
        JSR     GETBYT
        STA     ZWORD+LO

        LDA     ZWORD+HI
        LSR     A
        LSR     A               ; SHIFT 1ST CHAR INTO PLACE
        JMP     GTEXIT          ; AND RETURN IT

GETZ1:
        SEC
        SBC     #1
        BNE     GETZ2           ; LAST CHAR IN TRIPLET IF ZERO
        LDA     #2              ; ELSE
        STA     ZFLAG           ; RESET CHAR INDEX

        LDA     ZWORD+LO        ; GET BOTTOM HALF OF TRIPLET
        STA     I+LO            ; MOVE HERE FOR SHIFTING
        LDA     ZWORD+HI        ; GET TOP HALF

        ASL     I+LO            ; SHIFT THE TOP 3 BITS OF LOWER HALF
        ROL     A               ; INTO THE BOTTOM OF THE TOP HALF
        ASL     I+LO
        ROL     A
        ASL     I+LO
        ROL     A
        JMP     GTEXIT

GETZ2:
        LDA     #0              ; SET FLAG TO INDICATE
        STA     ZFLAG           ; END OF TRIPLET

        LDA     ZWORD+HI        ; TEST TOP HALF OF TRIPLET
        BPL     GETZ3           ; CONTINUE IF NOT END OF STRING
        LDA     #$FF            ; ELSE
        STA     ZFLAG           ; INDICATE LAST TRIPLET IN STRING

GETZ3:
        LDA     ZWORD+LO        ; GET BOTTOM HALF OF TRIPLET

GTEXIT:
        AND     #%00011111      ; MASK OUT GARBAGE BITS
        CLC
        RTS

; ---------------------------------
; CONVERT [IN] TO Z-STRING IN [OUT]
; ---------------------------------

CONZST:
        LDA     #$05            ; FILL OUTPUT BUFFER
        TAX                     ; WITH PAD CHARS ($05)
CZSL:
        STA     OUT,X
        DEX
        BPL     CZSL

        LDA     #6              ; INIT
        STA     CONCNT          ; CHAR COUNT

        LDA     #0              ; CLEAR
        STA     CONIN           ; SOURCE AND
        STA     CONOUT          ; OUTPUT INDEXES

CONTOP:
        LDX     CONIN           ; FETCH SOURCE INDEX
        INC     CONIN           ; AND UPDATE
        LDA     IN,X            ; GRAB AN ASCII CHAR
        STA     ZCHAR           ; SAVE IT HERE
        BNE     NEXTZ           ; CONTINUE IF CHAR WAS NZ

        LDA     #5              ; ELSE SHIP OUT
        BNE     CSHIP           ; A PAD CHAR

NEXTZ:
        LDA     ZCHAR
        JSR     SAYSET          ; WHICH CHARSET TO USE?
        BEQ     CSET0           ; LOWER-CASE IF ZERO

        CLC                     ; ELSE DO A TEMP-SHIFT
        ADC     #3              ; 4 = CHARSET 1, 5 = CHARSET 2
        LDX     CONOUT          ; FETCH OUTPUT INDEX
        STA     OUT,X           ; SEND THE SHIFT CHAR

        INC     CONOUT          ; UPDATE INDEX
        DEC     CONCNT          ; AND CHAR COUNT
        BNE     CTEST           ; IF OUT OF CHARS
        JMP     ZCRUSH          ; CRUSH 'EM!

CTEST:
        LDA     ZCHAR           ; TEST CHAR AGAIN
        JSR     SAYSET
        CMP     #2
        BEQ     CSET2           ; CHARSET #2

; HANDLE CHARSET #1 (UPPER CASE ALPHA)

        LDA     ZCHAR
        SEC
        SBC     #$41-6          ; CONVERT TO Z-CHAR
        BPL     CSHIP           ; AND SEND TO OUTPUT

; HANDLE CHARSET #0 (LOWER CASE ALPHA)

CSET0:
        LDA     ZCHAR
        SEC
        SBC     #$61-6          ; CONVERT TO Z-CHAR

; SHIP Z-CHAR TO OUTPUT BUFFER

CSHIP:
        LDX     CONOUT          ; FETCH OUTPUT INDEX
        STA     OUT,X

        INC     CONOUT          ; UPDATE INDEX
        DEC     CONCNT          ; DONE 6 CHARS YET?
        BNE     CONTOP          ; NO, LOOP BACK
        JMP     ZCRUSH          ; ELSE CRUSH

; HANDLE CHARSET #2 (MISCELLANEOUS)

CSET2:
        LDA     ZCHAR           ; GRAB CHAR
        JSR     CTABLE          ; IS IT IN CHARSET #3 TABLE?
        BNE     CSHIP           ; YES, SEND IT TO OUTPUT

; SEND A "DIRECT" ASCII CHAR

        LDA     #6              ; ASCII ALERT!
        LDX     CONOUT
        STA     OUT,X

        INC     CONOUT          ; UPDATE INDEX
        DEC     CONCNT          ; AND CHAR COUNT
        BEQ     ZCRUSH          ; BUFFER FULL!

; SEND 1ST HALF OF "DIRECT"

        LDA     ZCHAR
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #%00000011      ; MASK GARBAGE
        LDX     CONOUT
        STA     OUT,X

        INC     CONOUT
        DEC     CONCNT
        BEQ     ZCRUSH          ; BUFFER FULL!

; SEND 2ND HALF OF "DIRECT"

        LDA     ZCHAR           ; GET CHAR YET AGAIN
        AND     #%00011111      ; MASK JUNK
        JMP     CSHIP           ; AND SHIP IT OUT

; ---------------------
; IS [A] IN CHARSET #3?
; ---------------------

; EXIT: [A] = CHAR CODE IF FOUND, Z-FLAG CLEARED
;       Z-FLAG SET IF NOT FOUND

CTABLE:
        LDX     #25
CNL:
        CMP     CHRTBL,X
        BEQ     CNOK
        DEX
        BNE     CNL
        RTS                     ; Z-FLAG SET IF NO MATCH

CNOK:
        TXA                     ; CHAR CODE IS INDEX
        CLC
        ADC     #6              ; PLUS 6
        RTS

; -----------------------------
; RETURN CHARSET OF CHAR IN [A]
; -----------------------------

SAYSET:
        CMP     #'a'
        BCC     SAY1
        CMP     #'z'+1
        BCS     SAY1
        LDA     #0              ; IT'S CHARSET #0
        RTS

SAY1:
        CMP     #'A'
        BCC     SAY2
        CMP     #'Z'+1
        BCS     SAY2
        LDA     #1              ; IT'S CHARSET #1
        RTS

SAY2:
        LDA     #2              ; IT'S CHARSET #2
        RTS

; ----------------------
; CRUSH Z-CHARS IN [OUT]
; ----------------------

ZCRUSH:
        LDA     OUT+1           ; GET 2ND Z-CHAR
        ASL     A               ; SHIFT BITS INTO POSITION
        ASL     A
        ASL     A
        ASL     A
        ROL     OUT             ; ALONG WITH 1ST Z-CHAR
        ASL     A
        ROL     OUT
        ORA     OUT+2           ; SUPERIMPOSE 3RD Z-CHAR
        STA     OUT+1

        LDA     OUT+4           ; GET 5TH Z-CHAR
        ASL     A               ; SHIFT BITS
        ASL     A
        ASL     A
        ASL     A
        ROL     OUT+3           ; ALONG WITH 4TH Z-CHAR
        ASL     A
        ROL     OUT+3
        ORA     OUT+5           ; SUPERIMPOSE 6TH Z-CHAR
        TAX                     ; SAVE HERE
        LDA     OUT+3           ; GRAB 4TH Z-CHAR
        ORA     #%10000000      ; SET HIGH BIT
        STA     OUT+2           ; MOVE CRUSHED Z-WORD
        STX     OUT+3           ; INTO PLACE
        RTS

; -----------------------
; CHARSET #2 DECODE TABLE
; -----------------------

CHRTBL:
        .BYTE   0               ; DUMMY BYTE FOR "DIRECT"
        .BYTE   $0D             ; EOL
        .BYTE   "0123456789.,!?_#"
        .BYTE   $27             ; SINGLE QUOTE
        .BYTE   $22             ; DOUBLE QUOTE
        .BYTE   "/\-:()"
