;        PAGE
;        SBTTL   "--- 1-OPS ---"

; -----
; ZERO?
; -----

; [ARG1] = 0?

ZZERO:
        LDA     ARG1+LO
        ORA     ARG1+HI
        BEQ     PFINE

PYUCK:
        JMP     PREDF

; -----
; NEXT?
; -----

; RETURN "NEXT" POINTER IN OBJECT [ARG1];
; FAIL IF LAST AND RETURN ZERO

ZNEXT:
        LDA     ARG1+LO
        JSR     OBJLOC          ; GET OBJECT ADDR INTO [I]
        LDY     #5              ; POINT TO "NEXT" SLOT
        BNE     FIRST1

; ------
; FIRST?
; ------

; RETURN "FIRST" POINTER IN OBJECT [ARG1];
; FAIL IF LAST AND RETURN ZERO

ZFIRST:
        LDA     ARG1+LO
        JSR     OBJLOC          ; GET OBJECT ADDR INTO [I]
        LDY     #6              ; POINT TO "FIRST" SLOT

FIRST1:
        LDA     (I),Y           ; GET CONTENTS OF SLOT
        JSR     PUTBYT          ; PASS IT TO VARIABLE

        LDA     VALUE+LO        ; EXAMINE THE VALUE JUST "PUT"
        BEQ     PYUCK           ; FAIL IF IT WAS ZERO

PFINE:
        JMP     PREDS           ; ELSE REJOICE

; ---
; LOC
; ---

; RETURN THE OBJECT CONTAINING OBJECT [ARG1];
; RETURN ZERO IF NONE

ZLOC:
        LDA     ARG1+LO
        JSR     OBJLOC          ; GET ADDR OF OBJECT INTO [I]
        LDY     #4              ; POINT TO "LOC" SLOT
        LDA     (I),Y           ; GET THE BYTE
        JMP     PUTBYT          ; AND SHIP IT OUT

; ------
; PTSIZE
; ------

; RETURN LENGTH OF PROP TABLE [ARG1] IN BYTES

ZPTSIZ:
        LDA     ARG1+HI         ; MOVE ABS ADDR OF
        CLC                     ; THE PROP TABLE
        ADC     ZCODE           ; INTO [I]
        STA     I+HI

        LDA     ARG1+LO         ; DECREMENT THE
        SEC                     ; ADDRESS
        SBC     #1              ; WHILE MOVING LSB
        STA     I+LO
        BCS     PTZ0
        DEC     I+HI

PTZ0:
        LDY     #0              ; GET THE LENGTH
        JSR     PROPL           ; OF PROPERTY AT [I] INTO [A]

        CLC
        ADC     #1              ; INCREMENT RESULT
        JMP     PUTBYT          ; AND RETURN IT

; ---
; INC
; ---

; INCREMENT VARIABLE [ARG1]

ZINC:
        LDA     ARG1+LO
        JSR     VARGET          ; FETCH VARIABLE INTO [VALUE]
        JSR     INCVAL          ; INCREMENT IT
        JMP     ZD0

; ---
; DEC
; ---

; DECREMENT VARIABLE [ARG1]

ZDEC:
        LDA     ARG1+LO
        JSR     VARGET          ; FETCH VAR INTO [VALUE]
        JSR     DECVAL          ; DECREMENT IT

ZD0:
        LDA     ARG1+LO         ; PUT RESULT BACK
        JMP     VARPUT          ; INTO THE SAME VARIABLE

; ------
; PRINTB
; ------

; PRINT Z-STRING AT [ARG1]

ZPRB:
        LDA     ARG1+LO
        STA     I+LO
        LDA     ARG1+HI
        STA     I+HI

        JSR     SETWRD          ; MOVE Z-ADDR TO [MPC]
        JMP     PZSTR           ; AND PRINT

; ------
; REMOVE
; ------

; MOVE OBJECT [ARG1] INTO PSEUDO-OBJECT #0

ZREMOV:
        LDA     ARG1+LO         ; GET SOURCE OBJECT ADDR
        JSR     OBJLOC          ; INTO [I]

        LDA     I+LO            ; COPY THE SOURCE ADDR
        STA     J+LO            ; INTO [J]
        LDA     I+HI            ; FOR LATER REFERENCE
        STA     J+HI

        LDY     #4              ; POINT TO "LOC" SLOT
        LDA     (I),Y           ; GET THE DATA
        BEQ     REMVEX          ; SCRAM IF NO OBJECT

        JSR     OBJLOC          ; ELSE GET ADDR OF OBJECT [A] INTO [I]
        LDY     #6              ; POINT TO "FIRST" SLOT
        LDA     (I),Y           ; GRAB DATA
        CMP     ARG1+LO         ; IS THIS THE FIRST?
        BNE     REMVC1          ; NO, KEEP SEARCHING

        LDY     #5              ; ELSE COPY SOURCE'S "NEXT" SLOT
        LDA     (J),Y
        INY                     ; INTO DEST'S "FIRST" SLOT ([Y] = 6)
        STA     (I),Y
        BNE     REMVC2          ; BRANCH ALWAYS

REMVC1:
        JSR     OBJLOC
        LDY     #5              ; GET "NEXT"
        LDA     (I),Y
        CMP     ARG1+LO         ; FOUND IT?
        BNE     REMVC1          ; NO, KEEP TRYING

        LDY     #5              ; WHEN FOUND
        LDA     (J),Y           ; MOVE "NEXT" SLOT OF SOURCE
        STA     (I),Y           ; TO "NEXT" SLOT OF DEST

REMVC2:
        LDA     #0
        LDY     #4              ; CLEAR "LOC"
        STA     (J),Y
        INY                     ; AND "NEXT" SLOTS ([Y] = 5)
        STA     (J),Y           ; OF SOURCE OBJECT

REMVEX:
        RTS

; ------
; PRINTD
; ------

; PRINT SHORT DESCRIPTION OF OBJECT [ARG1]

ZPRD:
        LDA     ARG1+LO

; ENTRY POINT FOR "USL"

PRNTDC:
        JSR     OBJLOC          ; GET ADDR OF OBJECT INTO [I]
        LDY     #7              ; GET PROP TABLE POINTER
        LDA     (I),Y           ; FETCH MSB
        TAX                     ; SAVE IT HERE
        INY
        LDA     (I),Y           ; FETCH LSB
        STA     I+LO            ; STORE LSB
        STX     I+HI            ; AND MSB

        INC     I+LO            ; POINT PAST THE
        BNE     PDC0            ; LENGTH BYTE
        INC     I+HI

PDC0:
        JSR     SETWRD          ; CALC Z-STRING ADDR
        JMP     PZSTR           ; AND PRINT IT

; ------
; RETURN
; ------

; RETURN FROM "CALL" WITH VALUE [ARG1]

ZRET:
        LDA     OLDZSP          ; RE-SYNC THE
        STA     ZSP             ; Z-STACK POINTER

        JSR     POPVAL          ; POP # LOCALS INTO [X/A]
        STX     I+HI            ; SAVE HERE

; MAKE SURE [X] WAS COMPLEMENT OF [A] (BM 11/24/84)

        EOR     #$FF            ; COMPLEMENT [A]
        CMP     I+HI            ; MATCHED?
        BNE     STKERR          ; ERROR IF NOT

        TXA                     ; SET FLAGS; ANY LOCALS?
        BEQ     RET2            ; SKIP IF NOT

; RESTORE PUSHED LOCALS

        DEX                     ; ZERO-ALIGN
        TXA                     ; AND
        ASL     A               ; WORD-ALIGN # LOCALS
        STA     I+LO            ; FOR USE AS A STORAGE INDEX

RET1:
        JSR     POPVAL          ; POP A LOCAL INTO [X/A]

        LDY     I+LO            ; RETRIEVE STORAGE INDEX
        STA     LOCALS+HI,Y     ; STORE MSB OF LOCAL
        TXA                     ; MOVE LSB
        STA     LOCALS+LO,Y     ; AND STORE THAT TOO

        DEC     I+LO
        DEC     I+LO            ; UPDATE STORAGE INDEX

        DEC     I+HI            ; AND LOCALS COUNT
        BNE     RET1            ; POP TILL NO MORE LOCALS

; RESTORE OTHER VARIABLES

RET2:
        JSR     POPVAL          ; POP [ZPCH] AND [ZPCM]
        STX     ZPCM
        STA     ZPCH

        JSR     POPVAL          ; POP AND RESTORE
        STX     OLDZSP
        STA     ZPCL

        LDA     #0
        STA     ZPCFLG          ; ZPC CHANGED!

        JSR     A12VAL          ; MOVE [ARG1] TO [VALUE]
        JMP     PUTVAL          ; AND RETURN IT

; *** ERROR #15: Z-STACK DESTROYED ***

STKERR:
        LDA     #15             ; (BM 11/24/84)
        JMP     ZERROR

; ----
; JUMP
; ----

; JUMP TO Z-LOCATION IN [ARG1]

ZJUMP:
        JSR     A12VAL          ; MOVE [ARG1] TO [VALUE]
        JMP     PREDB3          ; A BRANCH THAT ALWAYS SUCCEEDS

; -----
; PRINT
; -----

; PRINT Z-STRING AT WORD POINTER [ARG1]

ZPRINT:
        LDA     ARG1+LO
        STA     I+LO
        LDA     ARG1+HI
        STA     I+HI

        JSR     SETSTR          ; CALC STRING ADDRESS
        JMP     PZSTR           ; AND PRINT IT

; -----
; VALUE
; -----

; RETURN VALUE OF VARIABLE [ARG1]

ZVALUE:
        LDA     ARG1+LO
        JSR     VARGET          ; GET THE VALUE
        JMP     PUTVAL          ; EASY ENOUGH

; ----
; BCOM
; ----

; COMPLEMENT [ARG1]

ZBCOM:
        LDA     ARG1+LO
        EOR     #$FF
        TAX
        LDA     ARG1+HI
        EOR     #$FF

; FALL THROUGH ...

; ---------------------
; RETURN VALUE IN [X/A]
; ---------------------

VEXIT:
        STX     VALUE+LO
        STA     VALUE+HI
        JMP     PUTVAL
