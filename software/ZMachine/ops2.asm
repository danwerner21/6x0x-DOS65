;        PAGE
;        SBTTL   "--- 2-OPS ---"

; -----
; LESS?
; -----

; [ARG1] < [ARG2]?

ZLESS:
        JSR     A12VAL          ; MOVE [ARG1] TO [VALUE]
        JMP     DLS0            ; MOVE [ARG2] TO [I] & COMPARE

; ------
; DLESS?
; ------

; DECREMENT [ARG1]; SUCCEED IF < [ARG2]

ZDLESS:
        JSR     ZDEC            ; MOVES ([ARG1]-1) TO [VALUE]

DLS0:
        LDA     ARG2+LO         ; MOVE [ARG2] TO [I]
        STA     I+LO
        LDA     ARG2+HI
        STA     I+HI

        JMP     COMPAR          ; COMPARE & RETURN

; -----
; GRTR?
; -----

; [ARG1] > [ARG2]?

ZGRTR:
        LDA     ARG1+LO         ; MOVE [ARG1] TO [I]
        STA     I+LO
        LDA     ARG1+HI
        STA     I+HI

        JMP     A2VAL           ; MOVE [ARG2] TO [VALUE] & COMPARE

; ------
; IGRTR?
; ------

; INCREMENT [ARG1]; SUCCEED IF GREATER THAN [ARG2]

ZIGRTR:
        JSR     ZINC            ; GET ([ARG1]+1) INTO [VALUE]

        LDA     VALUE+LO        ; MOVE [VALUE] TO [I]
        STA     I+LO
        LDA     VALUE+HI
        STA     I+HI

A2VAL:
        LDA     ARG2+LO         ; MOVE [ARG2] TO [VALUE]
        STA     VALUE+LO
        LDA     ARG2+HI
        STA     VALUE+HI

COMPAR:
        JSR     SCOMP           ; COMPARE [VALUE] AND [I]
        BCC     PGOOD
        BCS     PBAD

; -----------------
; SIGNED COMPARISON
; -----------------

; ENTRY: VALUES IN [VALUE] AND [I]

SCOMP:
        LDA     I+HI
        EOR     VALUE+HI
        BPL     SCMP
        LDA     I+HI
        CMP     VALUE+HI
        RTS

SCMP:
        LDA     VALUE+HI
        CMP     I+HI
        BNE     SCEX
        LDA     VALUE+LO
        CMP     I+LO
SCEX:
        RTS

; ---
; IN?
; ---

; IS OBJECT [ARG1] CONTAINED IN OBJECT [ARG2]?

ZIN:
        LDA     ARG1+LO
        JSR     OBJLOC          ; GET ADDR OF TARGET OBJECT INTO [I]

        LDY     #4              ; POINT TO "LOC" SLOT
        LDA     (I),Y           ; GET DATA
        CMP     ARG2+LO         ; IS IT THERE?
        BEQ     PGOOD           ; YES, SUCCEED

PBAD:
        JMP     PREDF           ; TOO BAD, CHUM ...

; ----
; BTST
; ----

; IS EVERY "ON" BIT IN [ARG1]
; ALSO "ON" IN [ARG2]?

ZBTST:
        LDA     ARG2+LO         ; FIRST CHECK LSBS
        AND     ARG1+LO
        CMP     ARG2+LO         ; LSBS MATCH?
        BNE     PBAD            ; NO, EXIT NOW

        LDA     ARG2+HI         ; ELSE CHECK MSBS
        AND     ARG1+HI
        CMP     ARG2+HI         ; MATCHED?
        BNE     PBAD            ; SORRY ...

PGOOD:
        JMP     PREDS

; ---
; BOR
; ---

; RETURN [ARG1] "OR" [ARG2]

ZBOR:
        LDA     ARG1+LO
        ORA     ARG2+LO
        TAX
        LDA     ARG1+HI
        ORA     ARG2+HI
        JMP     VEXIT

; ----
; BAND
; ----

; RETURN [ARG1] "AND" [ARG2]

ZBAND:
        LDA     ARG1+LO
        AND     ARG2+LO
        TAX
        LDA     ARG1+HI
        AND     ARG2+HI
        JMP     VEXIT

; -----
; FSET?
; -----

; IS FLAG [ARG1] SET IN OBJECT [ARG2]?

ZFSETP:
        JSR     FLAGSU          ; GET BITS INTO [K] AND [J]
        LDA     K+HI            ; DO MSBS
        AND     J+HI
        STA     K+HI

        LDA     K+LO            ; DO LSBS
        AND     J+LO

        ORA     K+HI            ; ANY BITS ON?
        BNE     PGOOD           ; TARGET BIT MUST BE ON
        JMP     PREDF

; ----
; FSET
; ----

; SET FLAG [ARG2] IN OBJECT [ARG1]

ZFSET:
        JSR     FLAGSU          ; GET BITS INTO [K] & [J], ADDR IN [I]

        LDY     #0
        LDA     K+HI            ; FIRST DO MSBS
        ORA     J+HI
        STA     (I),Y

        INY
        LDA     K+LO            ; THEN LSBS
        ORA     J+LO
        STA     (I),Y
        RTS

; ------
; FCLEAR
; ------

; CLEAR FLAG [ARG2] IN OBJECT [ARG1]

ZFCLR:
        JSR     FLAGSU          ; GETS BITS INTO [J] & [K], ADDR IN [I]

        LDY     #0
        LDA     J+HI            ; FETCH MSB
        EOR     #$FF            ; COMPLEMENT IT
        AND     K+HI            ; RUB OUT FLAG
        STA     (I),Y

        INY
        LDA     J+LO            ; SAME FOR LSB
        EOR     #$FF
        AND     K+LO
        STA     (I),Y
        RTS

; ---
; SET
; ---

; SET VARIABLE [ARG1] EQUAL TO [ARG2]

ZSET:
        LDA     ARG2+LO         ; MOVE THE VALUE
        STA     VALUE+LO        ; INTO [VALUE]
        LDA     ARG2+HI
        STA     VALUE+HI

        LDA     ARG1+LO         ; GET VARIABLE ID
        JMP     VARPUT          ; AND CHANGE THE VARIABLE

; ----
; MOVE
; ----

; MOVE OBJECT [ARG1] INTO OBJECT [ARG2]

ZMOVE:
        JSR     ZREMOV          ; REMOVE FIRST

        LDA     ARG1+LO
        JSR     OBJLOC          ; GET SOURCE OBJECT ADDR INTO [I]

        LDA     I+LO            ; COPY SOURCE ADDRESS
        STA     J+LO            ; INTO [J]
        LDA     I+HI
        STA     J+HI

        LDA     ARG2+LO         ; GET DEST OBJECT ID
        LDY     #4              ; POINT TO "LOC" SLOT OF SOURCE
        STA     (I),Y           ; AND MOVE IT IN

        JSR     OBJLOC          ; GET ADDR OF DEST OBJECT INTO [I]

        LDY     #6              ; POINT TO "FIRST" SLOT
        LDA     (I),Y           ; GET "FIRST" OF DEST
        TAX                     ; SAVE HERE FOR A MOMENT

        LDA     ARG1+LO         ; GET SOURCE OBJECT ID
        STA     (I),Y           ; MAKE IT "FIRST" OF DEST

        TXA                     ; RESTORE "FIRST" OF DEST
        BEQ     ZMVEX           ; SCRAM IF ZERO

        LDY     #5              ; MAKE "FIRST" OF DEST
        STA     (J),Y           ; THE "NEXT" OF SOURCE

ZMVEX:
        RTS

; ---
; GET
; ---

; RETURN ITEM [ARG2] IN WORD-TABLE [ARG1]

ZGET:
        JSR     WCALC           ; CALC ADDRESS
        JSR     GETBYT          ; GET 1ST BYTE (MSB)

DOGET:
        STA     VALUE+HI        ; SAVE MSB
        JSR     GETBYT          ; GET LSB
        STA     VALUE+LO        ; SAVE AND
        JMP     PUTVAL          ; HAND IT OVER

; ----
; GETB
; ----

; RETURN ITEM [ARG2] IN BYTE-TABLE AT [ARG1]

ZGETB:
        JSR     BCALC
        BEQ     DOGET           ; [A] = 0, SO CLEAR MSB OF [VALUE]

; --------------------
; CALC TABLE ADDRESSES
; --------------------

; WORD-ALIGNED ENTRY

WCALC:
        ASL     ARG2+LO         ; WORD-ALIGN FOR
        ROL     ARG2+HI         ; WORD ACCESS

; BYTE-ALIGNED ENTRY

BCALC:
        LDA     ARG2+LO         ; ADD BASE ADDR OF TABLE
        CLC                     ; TO ITEM
        ADC     ARG1+LO         ; INDEX
        STA     MPCL

        LDA     ARG2+HI         ; SAME FOR MSBS
        ADC     ARG1+HI
        STA     MPCM

        LDA     #0
        STA     MPCH            ; CLEAR TOP BIT
        STA     MPCFLG          ; & INVALIDATE [MPC]
        RTS

; ----
; GETP
; ----

; RETURN PROPERTY [ARG2] OF OBJECT [ARG1];
; IF NO PROP [ARG2], RETURN [ARG2]'TH ELEMENT OF OBJECT #0

ZGETP:
        JSR     PROPB

GETP1:
        JSR     PROPN
        CMP     ARG2+LO
        BEQ     GETP3
        BCC     GETP2

        JSR     PROPNX
        JMP     GETP1           ; TRY AGAIN WITH NEXT PROP

GETP2:
        LDA     ARG2+LO         ; GET PROPERTY #
        SEC                     ; ZERO-ALIGN IT
        SBC     #1
        ASL     A               ; WORD-ALIGN IT
        TAY                     ; USE AS AN INDEX
        LDA     (OBJTAB),Y      ; GET MSB OF PROPERTY
        STA     VALUE+HI
        INY
        LDA     (OBJTAB),Y      ; DO SAME WITH LSB
        STA     VALUE+LO
        JMP     PUTVAL          ; RETURN DEFAULT IN [VALUE]

GETP3:
        JSR     PROPL
        INY                     ; MAKE [Y] POINT TO 1ST BYTE OF PROP
        TAX                     ; (SET FLAGS) IF LENGTH IN [A] = 0
        BEQ     GETPB           ; GET A BYTE PROPERTY
        CMP     #1              ; IF LENGTH = 1
        BEQ     GETPW           ; GET A WORD PROPERTY

; *** ERROR #7: PROPERTY LENGTH ***

        LDA     #7
        JMP     ZERROR

; GET A 1-BYTE PROPERTY

GETPB:
        LDA     (I),Y           ; GET LSB INTO [A]
        LDX     #0              ; CLEAR MSB IN [X]
        BEQ     ETPEX

; GET A 2-BYTE PROPERTY

GETPW:
        LDA     (I),Y           ; GET MSB
        TAX                     ; INTO [X]
        INY                     ; POINT TO LSB
        LDA     (I),Y           ; GET IT INTO [A]

ETPEX:
        STA     VALUE+LO        ; STORE LSB
        STX     VALUE+HI        ; AND MSB
        JMP     PUTVAL

; -----
; GETPT
; -----

; RETURN POINTER TO PROP TABLE [ARG2]
; IN OBJECT [ARG1]

ZGETPT:
        JSR     PROPB

GETPT1:
        JSR     PROPN           ; RETURNS OFFSET IN [Y]
        CMP     ARG2+LO
        BEQ     GETPT2
        BCC     DORET
        JSR     PROPNX          ; TRY NEXT PROPERTY
        JMP     GETPT1

GETPT2:
        INC     I+LO
        BNE     GETPT3
        INC     I+HI

GETPT3:
        TYA                     ; FETCH OFFSET
        CLC
        ADC     I+LO            ; ADD LSB OF TABLE ADDRESS
        STA     VALUE+LO

        LDA     I+HI            ; AND MSB
        ADC     #0
        SEC                     ; STRIP OFF
        SBC     ZCODE           ; RELATIVE POINTER
        STA     VALUE+HI
        JMP     PUTVAL          ; AND RETURN

DORET:
        JMP     RET0            ; ELSE RETURN A ZERO

; -----
; NEXTP
; -----

; RETURN INDEX # OF PROP FOLLOWING PROP [ARG2] IN OBJECT [ARG1];
; RETURN ZERO IF LAST; RETURN FIRST IF [ARG2]=0; ERROR IF NONE

ZNEXTP:
        JSR     PROPB
        LDA     ARG2+LO         ; IF [ARG2]=0
        BEQ     NXTP3           ; RETURN "FIRST" SLOT

NXTP1:
        JSR     PROPN           ; FETCH PROPERTY #
        CMP     ARG2+LO         ; COMPARE TO TARGET #
        BEQ     NXTP2           ; FOUND IT!
        BCC     DORET           ; LAST PROP, SO RETURN ZERO
        JSR     PROPNX          ; ELSE TRY NEXT PROPERTY
        JMP     NXTP1

NXTP2:
        JSR     PROPNX          ; POINT TO FOLLOWING PROPERTY

NXTP3:
        JSR     PROPN           ; GET THE PROPERTY #
        JMP     PUTBYT          ; AND RETURN IT

; ---
; ADD
; ---

; RETURN [ARG1] + [ARG2]

ZADD:
        LDA     ARG1+LO         ; ADD LSBS
        CLC
        ADC     ARG2+LO
        TAX                     ; SAVE LSB HERE
        LDA     ARG1+HI         ; ADD MSBS
        ADC     ARG2+HI
        JMP     VEXIT

; ---
; SUB
; ---

; RETURN [ARG1] - [ARG2]

ZSUB:
        LDA     ARG1+LO         ; SUBTRACT LSBS
        SEC
        SBC     ARG2+LO
        TAX                     ; SAVE LSB HERE
        LDA     ARG1+HI         ; SUBTRACT MSBS
        SBC     ARG2+HI
        JMP     VEXIT           ; EXIT WITH [X]=LSB, [A]=MSB

; ---
; MUL
; ---

; RETURN [ARG1] * [ARG2]

ZMUL:
        JSR     MINIT           ; INIT THINGS

ZMLOOP:
        ROR     MTEMP+HI
        ROR     MTEMP+LO
        ROR     ARG2+HI
        ROR     ARG2+LO
        BCC     ZMNEXT

        LDA     ARG1+LO
        CLC
        ADC     MTEMP+LO
        STA     MTEMP+LO
        LDA     ARG1+HI
        ADC     MTEMP+HI
        STA     MTEMP+HI

ZMNEXT:
        DEX
        BPL     ZMLOOP

        LDX     ARG2+LO         ; PUT LSB OF PRODUCT
        LDA     ARG2+HI         ; AND MSB
        JMP     VEXIT           ; WHERE "VEXIT" EXPECTS THEM

; ---
; DIV
; ---

; RETURN QUOTIENT OF [ARG1] / [ARG2]

ZDIV:
        JSR     DIVIDE
        LDX     QUOT+LO
        LDA     QUOT+HI
        JMP     VEXIT

; ---
; MOD
; ---

; RETURN REMAINDER OF [ARG1] / [ARG2]

ZMOD:
        JSR     DIVIDE
        LDX     REMAIN+LO       ; FETCH THE REMAINDER
        LDA     REMAIN+HI       ; IN [REMAIN]
        JMP     VEXIT           ; AND RETURN IT

; ---------------
; SIGNED DIVISION
; ---------------

; ENTRY: DIVIDEND IN [ARG1], DIVISOR IN [ARG2]
; EXIT: QUOTIENT IN [QUOT], REMAINDER IN [REMAIN]

DIVIDE:
        LDA     ARG1+HI         ; SIGN OF REMAINDER
        STA     RSIGN           ; IS THE SIGN OF THE DIVIDEND
        EOR     ARG2+HI         ; SIGN OF QUOTIENT IS POSITIVE
        STA     QSIGN           ; IF SIGNS OF TERMS ARE THE SAME

        LDA     ARG1+LO         ; MOVE [ARG1] TO [QUOT]
        STA     QUOT+LO
        LDA     ARG1+HI
        STA     QUOT+HI         ; IF DIVIDEND IS POSITIVE
        BPL     ABSDIV          ; MOVE DIVISOR
        JSR     ABQUOT          ; ELSE CALC ABS(DIVIDEND) FIRST

ABSDIV:
        LDA     ARG2+LO
        STA     REMAIN+LO
        LDA     ARG2+HI
        STA     REMAIN+HI       ; IF REMAINDER IS POSITIVE
        BPL     GODIV           ; WE'RE READY TO DIVIDE
        JSR     ABREM           ; ELSE CALC ABS(DIVISOR)

GODIV:
        JSR     UDIV            ; DO UNSIGNED DIVIDE

        LDA     QSIGN           ; SHOULD QUOTIENT BE FLIPPED?
        BPL     RFLIP           ; NO, TEST REMAINDER
        JSR     ABQUOT          ; ELSE GET ABSOLUTE VALUE

RFLIP:
        LDA     RSIGN           ; SHOULD EMAINDER BE FLIPPED?
        BPL     DIVEX           ; NO, WE'RE DONE

; ELSE FALL THROUGH ...

; ----------------
; CALC ABS(REMAIN)
; ----------------

ABREM:
        LDA     #0
        SEC
        SBC     REMAIN+LO
        STA     REMAIN+LO
        LDA     #0
        SBC     REMAIN+HI
        STA     REMAIN+HI

DIVEX:
        RTS

; --------------
; CALC ABS(QUOT)
; --------------

ABQUOT:
        LDA     #0
        SEC
        SBC     QUOT+LO
        STA     QUOT+LO
        LDA     #0
        SBC     QUOT+HI
        STA     QUOT+HI
        RTS

; -----------------
; UNSIGNED DIVISION
; -----------------

; ENTRY: DIVIDEND IN [QUOT], DIVISOR IN [REMAIN]
; EXIT: QUOTIENT IN [QUOT], REMAINDER IN [REMAIN]

UDIV:
        LDA     REMAIN+LO       ; CHECK [REMAIN]
        ORA     REMAIN+HI       ; BEFORE PROCEEDING
        BEQ     DIVERR          ; CAN'T DIVIDE BY ZERO!

        JSR     MINIT           ; SET IT ALL UP

UDLOOP:
        ROL     QUOT+LO
        ROL     QUOT+HI
        ROL     MTEMP+LO
        ROL     MTEMP+HI

        LDA     MTEMP+LO
        SEC
        SBC     REMAIN+LO
        TAY                     ; SAVE HERE
        LDA     MTEMP+HI
        SBC     REMAIN+HI
        BCC     UDNEXT
        STY     MTEMP+LO
        STA     MTEMP+HI

UDNEXT:
        DEX
        BNE     UDLOOP

        ROL     QUOT+LO         ; SHIFT LAST CARRY FOR QUOTIENT
        ROL     QUOT+HI

        LDA     MTEMP+LO        ; MOVE REMAINDER
        STA     REMAIN+LO       ; INTO [REMAIN]
        LDA     MTEMP+HI
        STA     REMAIN+HI
        RTS

; *** ERROR #8: DIVISION BY ZERO ***

DIVERR:
        LDA     #8
        JMP     ZERROR

; ---------
; MATH INIT
; ---------

MINIT:
        LDX     #16             ; INIT LOOPING INDEX
        LDA     #0
        STA     MTEMP+LO        ; CLEAR TEMP
        STA     MTEMP+HI        ; REGISTER
        CLC                     ; AND CARRY
        RTS
