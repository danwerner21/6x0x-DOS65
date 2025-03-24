;        PAGE
;        SBTTL   "--- X-OPS ---"

; ------
; EQUAL?
; ------

; IS [ARG1] = [ARG2] (OR [ARG3] OR [ARG4])?

ZEQUAL:
        DEC     NARGS           ; DOUBLE-CHECK # ARGS
        BNE     DOEQ            ; MUST BE AT LEAST TWO, OR ...

; *** ERROR #9: NOT ENOUGH "EQUAL?" ARGS ***

        LDA     #9
        JMP     ZERROR

DOEQ:
        LDA     ARG1+LO         ; FETCH LSB
        LDX     ARG1+HI         ; AND MSB OF [ARG1]

        CMP     ARG2+LO         ; TEST LSB OF [ARG2]
        BNE     TRY2            ; NO GOOD, LOOK FOR ANOTHER ARG
        CPX     ARG2+HI         ; ELSE TRY MSB OF [ARG2]
        BEQ     EQOK            ; MATCHED!

TRY2:
        DEC     NARGS           ; OUT OF ARGS YET?
        BEQ     EQBAD           ; YES, WE FAILED

        CMP     ARG3+LO         ; TRY LSB OF [ARG3]
        BNE     TRY3            ; NO GOOD, LOOK FOR ANOTHER ARG
        CPX     ARG3+HI         ; HOW ABOUT MSB OF [ARG3]?
        BEQ     EQOK            ; YAY!

TRY3:
        DEC     NARGS           ; OUT OF ARGS YET?
        BEQ     EQBAD           ; IF NOT ...

        CMP     ARG4+LO         ; TRY [ARG4]
        BNE     EQBAD           ; SORRY, CHUM
        CPX     ARG4+HI         ; MSB MATCHED?
        BNE     EQBAD           ; TOO BAD

EQOK:
        JMP     PREDS           ; FINALLY MATCHED!

EQBAD:
        JMP     PREDF           ; FAILURE (SNIFF!)

; ----
; CALL
; ----

; BRANCH TO FUNCTION AT ([ARG1]*2), PASSING
; OPTIONAL PARAMETERS IN [ARG2]-[ARG4]

ZCALL:
        LDA     ARG1+LO
        ORA     ARG1+HI         ; IS CALL ADDRESS ZERO?
        BNE     DOCALL          ; NO, CONTINUE

        JMP     PUTBYT          ; ELSE RETURN THE ZERO IN [A]

DOCALL:
        LDX     OLDZSP          ; SAVE OLD STACK POINTER
        LDA     ZPCL            ; AND LSB OF [ZPC]
        JSR     PUSHXA          ; ON THE Z-STACK

        LDX     ZPCM            ; SAVE MIDDLE 8 BITS
        LDA     ZPCH            ; AND TOP BIT OF [ZPC]
        JSR     PUSHXA          ; AS WELL

; FORM 16-BIT ADDRESS FROM [ARG1]

        LDA     #0              ; CLEAR HIGH BIT FOR ROTATE
        STA     ZPCFLG          ; AND INVALIDATE [ZPC]

        ASL     ARG1+LO         ; MULTIPLY [ARG1]
        ROL     ARG1+HI         ; BY TWO
        ROL     A               ; HIGH BIT INTO [A]
        STA     ZPCH            ; NEW HIGH BIT OF [ZPC]

        LDA     ARG1+HI         ; GET NEW LOW BYTES
        STA     ZPCM
        LDA     ARG1+LO
        STA     ZPCL

        JSR     NEXTPC          ; FETCH # LOCALS TO PASS
        STA     J+LO            ; SAVE HERE FOR COUNTING
        STA     J+HI            ; AND HERE FOR LATER REFERENCE
        BEQ     ZCALL2          ; SKIP IF NO LOCALS

        LDA     #0
        STA     I+LO            ; ELSE INIT STORAGE INDEX

ZCALL1:
        LDY     I+LO
        LDX     LOCALS+LO,Y     ; GET LSB OF LOCAL INTO [X]
        LDA     LOCALS+HI,Y     ; AND MSB INTO [A]
        STY     I+LO            ; SAVE THE INDEX
        JSR     PUSHXA          ; PUSH LOCAL IN [X/A] ONTO Z-STACK

        JSR     NEXTPC          ; GET MSB OF NEW LOCAL
        STA     I+HI            ; SAVE IT HERE
        JSR     NEXTPC          ; NOW GET LSB

        LDY     I+LO            ; RESTORE INDEX
        STA     LOCALS+LO,Y     ; STORE LSB INTO [LOCALS]
        LDA     I+HI            ; RETRIEVE MSB
        STA     LOCALS+HI,Y     ; STORE IT INTO [LOCALS]

        INY
        INY                     ; UPDATE
        STY     I+LO            ; THE STORAGE INDEX

        DEC     J+LO            ; ANY MORE LOCALS?
        BNE     ZCALL1          ; YES, KEEP LOOPING

; MOVE UP TO 3 ARGUMENTS TO [LOCALS]

ZCALL2:
        DEC     NARGS           ; EXTRA ARGS IN THIS CALL?
        BEQ     ZCALL3          ; NO, CONTINUE

        LDA     ARG2+LO         ; MOVE [ARG2] TO LOCAL #1
        STA     LOCALS+LO
        LDA     ARG2+HI
        STA     LOCALS+HI

        DEC     NARGS           ; ANY LEFT?
        BEQ     ZCALL3          ; NO, SCRAM

        LDA     ARG3+LO         ; MOVE [ARG3] TO LOCAL #2
        STA     LOCALS+LO+2
        LDA     ARG3+HI
        STA     LOCALS+HI+2

        DEC     NARGS           ; ANY LEFT?
        BEQ     ZCALL3          ; NO, EXUENT

        LDA     ARG4+LO         ; MOVE [ARG4] TO LOCAL #3
        STA     LOCALS+LO+4
        LDA     ARG4+HI
        STA     LOCALS+HI+4

ZCALL3:
        LDX     J+HI            ; RETRIEVE # LOCALS
        TXA                     ; DUPE INTO [A]
        EOR     #$FF            ; COMPLEMENT FOR ERROR CHECK (BM 11/24/84)
        JSR     PUSHXA          ; PUSH # LOCALS ONTO Z-STACK

        LDA     ZSP             ; REMEMBER WHERE
        STA     OLDZSP          ; WE CAME FROM

        RTS                     ; WHEW!

; ---
; PUT
; ---

; SET ITEM [ARG2] IN WORD-TABLE [ARG1] EQUAL TO [ARG3]

ZPUT:
        ASL     ARG2+LO         ; WORD-ALIGN [ARG2]
        ROL     ARG2+HI

        JSR     PCALC           ; GET ITEM ADDR INTO [I]
        LDA     ARG3+HI         ; STORE MSB OF [ARG3]
        STA     (I),Y           ; INTO MSB OF TABLE POSITION
        INY                     ; POINT TO LSB
        BNE     PUTLSB          ; BRANCH ALWAYS

; ----
; PUTB
; ----

; SET ITEM [ARG2] IN BYTE-TABLE [ARG1] EQUAL TO [ARG3]

ZPUTB:
        JSR     PCALC

; ENTRY FOR "PUT"

PUTLSB:
        LDA     ARG3+LO         ; GET LSB OF [ARG3]
        STA     (I),Y           ; STORE IN TABLE AT [Y]
        RTS

; ---------------------------
; CALC ITEM ADDRESS FOR "PUT"
; ---------------------------

PCALC:
        LDA     ARG2+LO         ; ADD ITEM OFFSET IN [ARG2]
        CLC                     ; TO TABLE ADDR IN [ARG1]
        ADC     ARG1+LO         ; TO FORM A POINTER
        STA     I+LO            ; IN [I]

        LDA     ARG2+HI         ; SAME FOR MSB
        ADC     ARG1+HI
        CLC
        ADC     ZCODE           ; MAKE IT ABSOLUTE
        STA     I+HI

        LDY     #0              ; ZERO FOR INDEXING
        RTS

; ----
; PUTP
; ----

; SET PROPERTY [ARG2] IN OBJECT [ARG1] EQUAL TO [ARG3]

ZPUTP:
        JSR     PROPB

PUTP1:
        JSR     PROPN
        CMP     ARG2+LO
        BEQ     PUTP2
        BCC     PNERR           ; ERROR IF LOWER

        JSR     PROPNX          ; TRY NEXT PROPERTY
        JMP     PUTP1

PUTP2:
        JSR     PROPL
        INY                     ; MAKE [Y] POINT TO 1ST PROPERTY BYTE
        TAX                     ; (SET FLAGS) IF LENGTH IN [A] = 0
        BEQ     PUTP3           ; PUT A BYTE
        CMP     #1              ; PUT A WORD IF [A] = 1
        BNE     PLERR           ; ELSE LENGTH IS BAD

        LDA     ARG3+HI         ; GET MSB OF PROPERTY
        STA     (I),Y           ; AND STORE IN OBJECT
        INY                     ; POINT TO LSB SLOT

PUTP3:
        LDA     ARG3+LO         ; FETCH LSB
        STA     (I),Y           ; AND STORE IN OBJECT
        RTS

; *** ERROR #10: BAD PROPERTY NUMBER ***

PNERR:
        LDA     #10
        JMP     ZERROR

; *** ERROR #11: PUTP PROPERTY LENGTH ***

PLERR:
        LDA     #11
        JMP     ZERROR

; ------
; PRINTC
; ------

; PRINT CHAR WITH ASCII VALUE IN [ARG1]

ZPRC:
        LDA     ARG1+LO         ; GRAB THE CHAR
        JMP     COUT            ; AND SHIP IT OUT

; ------
; PRINTN
; ------

; PRINT VALUE OF [ARG1] AS A SIGNED INTEGER

ZPRN:
        LDA     ARG1+LO         ; MOVE [ARG1] TO [QUOT]
        STA     QUOT+LO
        LDA     ARG1+HI
        STA     QUOT+HI

; PRINT [QUOT]

NUMBER:
        LDA     QUOT+HI         ; IF VALUE IS POSITIVE
        BPL     DIGCNT          ; CONTINUE

        LDA     #$2D            ; ELSE START WITH A MINUS SIGN
        JSR     COUT

        JSR     ABQUOT          ; AND CALC ABS([QUOT])

; COUNT # OF DECIMAL DIGITS

DIGCNT:
        LDA     #0              ; RESET
        STA     DIGITS          ; DIGIT INDEX

DGC:
        LDA     QUOT+LO         ; IS QUOTIENT
        ORA     QUOT+HI         ; ZERO YET?
        BEQ     PRNTN3          ; YES, READY TO PRINT

        LDA     #10             ; ELSE DIVIDE [QUOT]
        STA     REMAIN+LO       ; BY 10 (LSB)
        LDA     #0
        STA     REMAIN+HI       ; 10 (MSB)

        JSR     UDIV            ; UNSIGNED DIVIDE

        LDA     REMAIN+LO       ; FETCH LSB OF REMAINDER (THE DIGIT)
        PHA                     ; SAVE IT ON STACK
        INC     DIGITS          ; UPDATE DIGIT COUNT
        BNE     DGC             ; LOOP TILL QUOTIENT=0

PRNTN3:
        LDA     DIGITS          ; IF DIGIT COUNT IS NZ
        BNE     PRNTN4          ; CONTINUE

        LDA     #'0'            ; ELSE PRINT "0"
        JMP     COUT            ; AND RETURN

PRNTN4:
        PLA                     ; PULL A DIGIT OFF THE STACK
        CLC
        ADC     #'0'            ; CONVERT TO ASCII
        JSR     COUT            ; AND PRINT IT
        DEC     DIGITS          ; OUT OF DIGITS YET?
        BNE     PRNTN4          ; NO, KEEP LOOPING
        RTS

; ------
; RANDOM
; ------

; RETURN A RANDOM VALUE BETWEEN 0 AND [ARG1]

ZRAND:
        LDA     ARG1+LO         ; MAKE [ARG1] THE DIVISOR
        STA     ARG2+LO
        LDA     ARG1+HI
        STA     ARG2+HI

        JSR     RANDOM          ; GET RANDOM BYTES INTO [A] AND [X]
        STX     ARG1+LO         ; MAKE THEM THE DIVIDEND
        AND     #$7F            ; MAKE SURE MSB IS POSITIVE
        STA     ARG1+HI

        JSR     DIVIDE          ; SIGNED DIVIDE, [ARG1] / [ARG2]

        LDA     REMAIN+LO       ; MOVE REMAINDER
        STA     VALUE+LO        ; INTO [VALUE]
        LDA     REMAIN+HI
        STA     VALUE+HI

        JSR     INCVAL          ; INCREMENT [VALUE]
        JMP     PUTVAL          ; AND RETURN RESULT

; ----
; PUSH
; ----

; PUSH [ARG1] ONTO THE Z-STACK

ZPUSH:
        LDX     ARG1+LO
        LDA     ARG1+HI
        JMP     PUSHXA

; ---
; POP
; ---

; POP WORD OFF Z-STACK, STORE IN VARIABLE [ARG1]

ZPOP:
        JSR     POPVAL          ; VALUE INTO [VALUE]
        LDA     ARG1+LO         ; GET VARIABLE ID
        JMP     VARPUT          ; AND CHANGE THE VARIABLE
