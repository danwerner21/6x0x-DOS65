;        PAGE
;        SBTTL   "--- OPCODE SUPPORT SUBROUTINES ---"

; -----------------------
; FETCH A SHORT IMMEDIATE
; -----------------------

GETSHT:
        LDA     #0              ; MSB IS ZERO
        BEQ     GETV            ; FETCH LSB FROM Z-CODE

; ----------------------
; FETCH A LONG IMMEDIATE
; ----------------------

GETLNG:
        JSR     NEXTPC          ; GRAB MSB

GETV:
        STA     VALUE+HI
        JSR     NEXTPC          ; GRAB LSB
        STA     VALUE+LO
        RTS

; ----------------
; FETCH A VARIABLE
; ----------------

; FROM INSIDE AN OPCODE (VARIABLE ID IN [A])

VARGET:
        TAX                     ; IF NON-ZERO,
        BNE     GETVR1          ; ACCESS A VARIABLE

        JSR     POPVAL          ; ELSE PULL VAR OFF Z-STACK
        JMP     PSHVAL          ; WITHOUT ALTERING STACK

; FROM THE MAIN LOOP (VARIABLE ID IN Z-CODE)

GETVAR:
        JSR     NEXTPC          ; GRAB VAR-TYPE BYTE
        BEQ     POPVAL          ; VALUE IS ON Z-STACK

; IS VARIABLE LOCAL OR GLOBAL?

GETVR1:
        CMP     #$10            ; IF >= 16,
        BCS     GETVRG          ; IT'S GLOBAL

; HANDLE A LOCAL VARIABLE

GETVRL:
        SEC
        SBC     #1              ; FORM A ZERO-ALIGNED
        ASL     A               ; WORD INDEX
        TAX                     ; INTO THE [LOCALS] TABLE

        LDA     LOCALS+LO,X     ; GRAB LSB
        STA     VALUE+LO
        LDA     LOCALS+HI,X     ; AND MSB
        STA     VALUE+HI
        RTS

; HANDLE A GLOBAL VARIABLE

GETVRG:
        JSR     GVCALC          ; GET ADDRESS OF GLOBAL INTO [I]
        LDA     (I),Y           ; MSB OF GLOBAL ([Y] = 0)
        STA     VALUE+HI
        INY                     ; = 1
        LDA     (I),Y           ; LSB OF GLOBAL
        STA     VALUE+LO        ; SAVE IT
        RTS                     ; AND WE'RE DONE

; ----------------------------------
; POP Z-STACK INTO [VALUE] AND [X/A]
; ----------------------------------

POPVAL:
        DEC     ZSP
        BEQ     UNDER           ; UNDERFLOW IF ZERO!

        LDY     ZSP             ; READ STACK POINTER
        LDX     ZSTAKL,Y        ; GRAB LSB OF STACK VALUE
        STX     VALUE+LO        ; GIVE TO [VALUE]
        LDA     ZSTAKH,Y        ; ALSO GRAB MSB
        STA     VALUE+HI        ; A SIMILAR FATE
        RTS

; *** ERROR #5 -- Z-STACK UNDERFLOW ***

UNDER:
        LDA     #5
        JMP     ZERROR

; -----------------------
; PUSH [VALUE] TO Z-STACK
; -----------------------

PSHVAL:
        LDX     VALUE+LO
        LDA     VALUE+HI

; ---------------------
; PUSH [X/A] TO Z-STACK
; ---------------------

PUSHXA:
        LDY     ZSP             ; READ STACK POINTER
        STA     ZSTAKH,Y        ; PUSH MSB IN [A]
        TXA
        STA     ZSTAKL,Y        ; AND LSB IN [X]

        INC     ZSP             ; UPDATE Z-STACK POINTER
        BEQ     OVER            ; OVERFLOW IF ZEROED!
        RTS

; *** ERROR #6 -- Z-STACK OVERFLOW ***

OVER:
        LDA     #6
        JMP     ZERROR

; --------------
; RETURN A VALUE
; --------------

; FROM WITHIN AN OPCODE (VARIABLE ID IN [A])

VARPUT:
        TAX                     ; IF ZERO,
        BNE     PUTVR1

        DEC     ZSP             ; FLUSH TOP WORD OFF STACK
        BNE     PSHVAL          ; AND REPLACE WITH [VALUE]
        BEQ     UNDER           ; ERROR IF [ZSP] BECAME ZERO!

; RETURN A ZERO

RET0:
        LDA     #0

; RETURN BYTE IN [A]

PUTBYT:
        STA     VALUE+LO
        LDA     #0
        STA     VALUE+HI        ; CLEAR MSB

; RETURN [VALUE]

PUTVAL:
        JSR     NEXTPC          ; GET VARIABLE ID BYTE
        BEQ     PSHVAL          ; [VALUE] GOES TO Z-STACK

; LOCAL OR GLOBAL VARIABLE?

PUTVR1:
        CMP     #$10            ; IF >= 16,
        BCS     PUTVLG          ; IT'S GLOBAL

; PUT A LOCAL VARIABLE

PUTVLL:
        SEC
        SBC     #1              ; FORM A ZERO-ALIGNED
        ASL     A               ; WORD INDEX
        TAX                     ; INTO THE [LOCALS] TABLE

        LDA     VALUE+LO        ; GRAB LSB
        STA     LOCALS+LO,X     ; SAVE IN LOCAL TABLE
        LDA     VALUE+HI        ; DO SAME TO
        STA     LOCALS+HI,X     ; MSB
        RTS

; RETURN A GLOBAL VARIABLE

PUTVLG:
        JSR     GVCALC
        LDA     VALUE+HI        ; GET MSB
        STA     (I),Y           ; STORE AS 1ST BYTE ([Y] = 0)
        INY                     ; = 1
        LDA     VALUE+LO        ; NOW GET LSB
        STA     (I),Y           ; STORE AS 2ND BYTE
        RTS

; -----------------------
; CALC GLOBAL WORD OFFSET
; -----------------------

; ENTRY: VAR-ID BYTE (16-255) IN [A]
; EXIT: ABSOLUTE ADDRESS OF GLOBAL VAR IN [I]
;       [Y] = 0 FOR INDEXING

GVCALC:
        SEC
        SBC     #$10            ; FORM A ZERO-ALIGNED INDEX
        LDY     #0              ; MAKE SURE MSB OF OFFSET AND [Y]
        STY     I+HI            ; ARE CLEARED

        ASL     A               ; MULTIPLY OFFSET BY 2
        ROL     I+HI            ; TO WORD-ALIGN IT

        CLC                     ; ADD OFFSET TO ADDR OF GLOBAL TABLE
        ADC     GLOBAL+LO       ; TO FORM THE ABSOLUTE
        STA     I+LO            ; ADDRESS OF THE
        LDA     I+HI            ; DESIRED GLOBAL VARIABLE
        ADC     GLOBAL+HI       ; STORE ADDRESS BACK IN [VAL]
        STA     I+HI            ; AS A POINTER

WCEX:
        RTS

; ---------------
; PREDICATE FAILS
; ---------------

PREDF:
        JSR     NEXTPC          ; GET 1ST BRANCH BYTE
        BPL     PREDB           ; DO BRANCH IF BIT 7 OFF

; -----------------------
; IGNORE PREDICATE BRANCH
; -----------------------

; ENTRY: 1ST BRANCH BYTE IN [A]

PREDNB:
        AND     #%01000000      ; TEST BIT 6
        BNE     WCEX            ; SHORT BRANCH IF SET
        JMP     NEXTPC          ; ELSE SKIP OVER 2ND BRANCH BYTE

; ------------------
; PREDICATE SUCCEEDS
; ------------------

PREDS:
        JSR     NEXTPC          ; GET 1ST BRANCH BYTE
        BPL     PREDNB          ; DON'T BRANCH IF BIT 7 CLEAR

; --------------------------
; PERFORM A PREDICATE BRANCH
; --------------------------

; ENTRY: 1ST PRED BYTE IN [A]

PREDB:
        TAX                     ; SAVE HERE
        AND     #%01000000      ; LONG OR SHORT BRANCH?
        BEQ     PREDLB          ; LONG IF BIT 6 IS CLEAR

; HANDLE A SHORT BRANCH

        TXA                     ; RESTORE PRED BYTE
        AND     #%00111111      ; FORM SHORT OFFSET
        STA     VALUE+LO        ; USE AS LSB OF BRANCH OFFSET
        LDA     #0
        STA     VALUE+HI        ; MSB OF OFFSET IS ZERO
        BEQ     PREDB1          ; DO THE BRANCH

; HANDLE A LONG BRANCH

PREDLB:
        TXA                     ; RESTORE 1ST PRED BYTE
        AND     #%00111111      ; FORM MSB OF OFFSET

        TAX                     ; SAVE HERE FOR REFERENCE

        AND     #%00100000      ; CHECK SIGN OF 14-BIT VALUE
        BEQ     DOB2            ; POSITIVE IF ZERO, SO USE [X]

        TXA                     ; ELSE RESTORE BYTE
        ORA     #%11100000      ; EXTEND THE SIGN BIT
        TAX                     ; BACK HERE FOR STORAGE

DOB2:
        STX     VALUE+HI
        JSR     NEXTPC          ; FETCH LSB OF 14-BIT OFFSET
        STA     VALUE+LO

; BRANCH TO Z-ADDRESS IN [VALUE]

PREDB1:
        LDA     VALUE+HI        ; CHECK MSB OF OFFSET
        BNE     PREDB3          ; DO BRANCH IF NZ

        LDA     VALUE+LO        ; IF LSB IS NON-ZERO,
        BNE     PREDB2          ; MAKE SURE IT ISN'T 1
        JMP     ZRFALS          ; ELSE DO AN "RFALSE"

PREDB2:
        CMP     #1              ; IF OFFSET = 1
        BNE     PREDB3
        JMP     ZRTRUE          ; DO AN "RTRUE"

; ENTRY POINT FOR "JUMP"

PREDB3:
        JSR     DECVAL          ; SUBTRACT 2 FROM THE OFFSET
        JSR     DECVAL          ; IN [VALUE]

        LDA     #0              ; CLEAR THE MSB
        STA     I+HI            ; OF [I]

        LDA     VALUE+HI        ; MAKE MSB OF OFFSET
        STA     I+LO            ; THE LSB OF [I]
        ASL     A               ; EXTEND THE SIGN OF OFFSET
        ROL     I+HI            ; INTO MSB OF [I]

        LDA     VALUE+LO        ; GET LSB OF OFFSET
        CLC
        ADC     ZPCL            ; ADD LOW 8 BITS OF ZPC
        BCC     PREDB5          ; IF OVERFLOWED,

        INC     I+LO            ; UPDATE UPPER 9 BITS
        BNE     PREDB5
        INC     I+HI

PREDB5:
        STA     ZPCL            ; UPDATE ZPC

        LDA     I+LO            ; IF UPPER 9 BITS ARE ZERO,
        ORA     I+HI            ; NO NEED TO CHANGE PAGES
        BEQ     ZNOOP

        LDA     I+LO            ; ELSE CALC NEW UPPER BITS
        CLC
        ADC     ZPCM
        STA     ZPCM

        LDA     I+HI
        ADC     ZPCH
        AND     #%00000001      ; USE ONLY BIT 0
        STA     ZPCH

        LDA     #0
        STA     ZPCFLG          ; [ZPC] NO LONGER VALID

; FALL THROUGH ...

; ----
; NOOP
; ----

ZNOOP:
        RTS

; -----------------
; DECREMENT [VALUE]
; -----------------

DECVAL:
        LDA     VALUE+LO
        SEC
        SBC     #1
        STA     VALUE+LO
        BCS     DVX
        DEC     VALUE+HI
DVX:
        RTS

; -----------------
; INCREMENT [VALUE]
; -----------------

INCVAL:
        INC     VALUE+LO
        BNE     IVX
        INC     VALUE+HI
IVX:
        RTS

; ----------------------
; MOVE [ARG1] TO [VALUE]
; ----------------------

A12VAL:
        LDA     ARG1+LO
        STA     VALUE+LO
        LDA     ARG1+HI
        STA     VALUE+HI
        RTS
