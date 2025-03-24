;        PAGE
;        SBTTL   "--- OBJECT & PROPERTY HANDLERS ---"

; ----------------------------------
; GET ABSOLUTE ADDRESS OF OBJECT [A]
; ----------------------------------

; EXIT: ADDRESS IN [I]

OBJLOC:
        STA     I+LO            ; SAVE LSB FOR ADDING

        LDX     #0              ; CLEAR MSB
        STX     I+HI            ; FOR SHIFTING

        ASL     A               ; MULTIPLY BY 8
        ROL     I+HI
        ASL     A
        ROL     I+HI
        ASL     A
        ROL     I+HI

        CLC                     ; ADD TO ITSELF
        ADC     I+LO            ; TO GET TIMES 9
        BCC     OBJ1
        INC     I+HI

OBJ1:
        CLC
        ADC     #53             ; NOW ADD 53
        BCC     OBJ2            ; (THE OBJECT TABLE OFFSET)
        INC     I+HI

OBJ2:
        CLC                     ; NEXT ADD THE ABS ADDR
        ADC     OBJTAB+LO       ; OF THE OBJECT TABLE
        STA     I+LO

        LDA     I+HI
        ADC     OBJTAB+HI
        STA     I+HI
        RTS

; -----------------------------
; GET ADDRESS OF PROPERTY TABLE
; -----------------------------

; EXIT: [I] HAS ABSOLUTE ADDR OF PROPERTY TABLE
;       [Y] HAS OFFSET TO START OF PROP IDS

PROPB:
        LDA     ARG1+LO
        JSR     OBJLOC
        LDY     #7
        LDA     (I),Y           ; GET MSB OF P-TABLE ADDRESS
        CLC
        ADC     ZCODE           ; MAKE IT ABSOLUTE
        TAX                     ; AND SAVE HERE
        INY
        LDA     (I),Y           ; NOW GET LSB
        STA     I+LO
        STX     I+HI            ; [I] NOW POINTS TO PROP TABLE

        LDY     #0
        LDA     (I),Y           ; GET LENGTH OF SHORT DESC
        ASL     A               ; WORD-ALIGN IT
        TAY                     ; EXPECTED HERE
        INY                     ; POINT JUST PAST THE DESCRIPTION
        RTS

; -------------------
; FETCH A PROPERTY ID
; -------------------

; ENTRY: LIKE "PROPB" EXIT

PROPN:
        LDA     (I),Y
        AND     #%00011111      ; MASK OUT LENGTH BITS
        RTS

; -------------------------------
; FETCH # BYTES IN PROPERTY VALUE
; -------------------------------

; ENTRY: LIKE "PROPB" EXIT

PROPL:
        LDA     (I),Y
        LSR     A               ; LENGTH IS IN
        LSR     A               ; BITS 7-5
        LSR     A               ; SO SHIFT INTO PLACE
        LSR     A
        LSR     A
        RTS

; ----------------------
; POINT TO NEXT PROPERTY
; ----------------------

; ENTRY: LIKE "PROPB" EXIT

PROPNX:
        JSR     PROPL           ; GET LENGTH OF CURRENT PROP
        TAX                     ; SAVE HERE

PPX:
        INY                     ; LOOP UNTIL
        DEX                     ; [Y] POINTS TO
        BPL     PPX             ; START OF NEXT PROP
        INY                     ; CORRECT ALIGNMENT
        RTS

; ----------------
; GET OBJECT FLAGS
; ----------------

; ENTRY: OBJECT # IN [ARG1], FLAG # IN [ARG2]
; EXIT: FLAG WORD IN [K], BIT ID IN [J],
;       FLAG WORD ADDRESS IN [I]

FLAGSU:
        LDA     ARG1+LO
        JSR     OBJLOC          ; GET OBJECT ADDR IN [I]

        LDA     ARG2+LO         ; LOOK AT FLAG ID
        CMP     #$10            ; FIRST SET OF FLAGS?
        BCC     FLS1            ; YES, ADDR IN [I] IS CORRECT

        SBC     #16             ; ELSE ZERO-ALIGN FLAG INDEX
        TAX                     ; SAVE IT HERE

        LDA     I+LO            ; ADD 2 TO ADDRESS IN [I]
        CLC                     ; TO POINT TO ADDRESS OF
        ADC     #2              ; 2ND FLAG WORD
        STA     I+LO
        BCC     FLS0
        INC     I+HI

FLS0:
        TXA                     ; RESTORE INDEX

FLS1:
        STA     K+LO            ; SAVE FLAG ID HERE

        LDX     #1              ; INIT THE
        STX     J+LO            ; FLAG WORD TO
        DEX                     ; $0001
        STX     J+HI

        LDA     #15             ; SUBTRACT THE BIT POSITION
        SEC                     ; FROM 15
        SBC     K+LO            ; TO GET THE SHIFT LOOP
        TAX                     ; INDEX
        BEQ     FLS2            ; EXIT NOW IF NO SHIFT NEEDED

FLSL:
        ASL     J+LO            ; SHIFT THE BIT
        ROL     J+HI            ; INTO POSITION
        DEX
        BNE     FLSL

FLS2:
        LDY     #0              ; MOVE THE FLAG WORD
        LDA     (I),Y           ; INTO [J]
        STA     K+HI            ; FIRST THE MSB
        INY
        LDA     (I),Y
        STA     K+LO            ; THEN THE LSB
        RTS
