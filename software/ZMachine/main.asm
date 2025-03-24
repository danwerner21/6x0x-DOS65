;        PAGE
;        SBTTL   "--- MAIN LOOP ---"

MLOOP:
        LDA     #0
        STA     NARGS           ; RESET # ARGUMENTS
        JSR     NEXTPC          ; GET NEXT INSTRUCTION INTO [A]
        STA     OPCODE          ; SAVE IT HERE

        .IFDEF  DEBUG
        STA     MBYTE
        LDA     #0              ; BREAKPOINT #0
        JSR     DOBUG
        LDA     MBYTE
        .ENDIF

; DECODE AN OPCODE

        TAX                     ; SET FLAGS
        BMI     DC0             ; IF POSITIVE,
        JMP     OP2             ; IT'S A 2-OP

DC0:
        CMP     #$B0
        BCS     DC1
        JMP     OP1             ; OR MAYBE A 1-OP

DC1:
        CMP     #$C0
        BCS     OPEXT
        JMP     OP0             ; PERHAPS A 0-OP

; --------------
; HANDLE AN X-OP
; --------------

OPEXT:
        JSR     NEXTPC          ; GRAB THE ARGUMENT ID BYTE
        STA     ABYTE           ; HOLD IT HERE

        LDX     #0
        STX     ADEX            ; INIT ARGUMENT INDEX
        BEQ     OPX1            ; JUMP TO TOP OF LOOP

OPX0:
        LDA     ABYTE           ; GET ARG BYTE
        ASL     A               ; SHIFT NEXT 2 ARG BITS
        ASL     A               ; INTO BITS 7 & 6
        STA     ABYTE           ; HOLD FOR LATER

OPX1:
        AND     #%11000000      ; MASK OUT GARBAGE BITS
        BNE     OPX2
        JSR     GETLNG          ; 00 = LONG IMMEDIATE
        JMP     OPXNXT

OPX2:
        CMP     #%01000000      ; IS IT A SHORT IMMEDIATE?
        BNE     OPX3            ; NO, KEEP GUESSING
        JSR     GETSHT          ; 01 = SHORT IMMEDIATE
        JMP     OPXNXT

OPX3:
        CMP     #%10000000      ; LAST TEST
        BNE     OPX4            ; 11 = NO MORE ARGUMENTS
        JSR     GETVAR          ; 10 = VARIABLE

OPXNXT:
        LDX     ADEX            ; RETRIEVE ARGUMENT INDEX
        LDA     VALUE+LO        ; GRAB LSB OF VALUE
        STA     ARG1+LO,X       ; STORE IN ARGUMENT TABLE
        LDA     VALUE+HI        ; GRAB MSB OF VALUE
        STA     ARG1+HI,X       ; STORE THAT, TOO

        INC     NARGS           ; UPDATE ARGUMENT COUNTER

        INX
        INX
        STX     ADEX            ; UPDATE INDEX
        CPX     #8              ; DONE 4 ARGUMENTS YET?
        BCC     OPX0            ; NO, GET SOME MORE

; ALL X-OP ARGUMENTS READY

OPX4:
        LDA     OPCODE          ; IS THIS
        CMP     #$E0            ; AN EXTENDED 2-OP?
        BCS     DOXOP           ; NO, IT'S A REAL X-OP
        JMP     OP2EX           ; ELSE TREAT IT LIKE A 2-OP

DOXOP:
        LDX     #<OPTX          ; GET ADDR OF X-OP TABLE
        LDY     #>OPTX          ; INTO [X/Y]
        AND     #%00011111      ; ISOLATE OP ID BITS
        CMP     #NOPSX          ; IS IT A LEGAL X-OP?
        BCC     DODIS           ; YUP; TIME TO DISPATCH IT

; *** ERROR #1 -- ILLEGAL X-OP ***

        LDA     #1
        JMP     ZERROR

; ---------------
; OPCODE DISPATCH
; ---------------

; ENTRY: MASKED OPCODE INDEX IN [A]
;        OP-TABLE ADDR IN X/Y (LSB/MSB)

DODIS:
        STX     I+LO            ; SAVE TABLE ADDRESS
        STY     I+HI            ; IN A POINTER

        ASL     A               ; WORD-ALIGN THE OP INDEX
        TAY
        LDA     (I),Y           ; GET LSB OF DISPATCH ADDRESS
        STA     GO+LO           ; INSTALL AS JSR OPERAND
        INY
        LDA     (I),Y           ; SAME WITH MSB
        STA     GO+HI

        .BYTE   $20             ; 6502 "JSR" OPCODE
GO:
        .WORD   $0000           ; DUMMY OPERAND BYTES

        JMP     MLOOP           ; GO BACK FOR ANOTHER OPCODE

; -------------
; HANDLE A 0-OP
; -------------

OP0:
        LDX     #<OPT0          ; GET 0-OP TABLE ADDR
        LDY     #>OPT0          ; INTO [X/Y]
        AND     #%00001111      ; ISOLATE 0-OP ID BITS
        CMP     #NOPS0          ; OUT OF RANGE?
        BCC     DODIS           ; NO, DISPATCH IT

; *** ERROR #2 -- ILLEGAL 0-OP ***

        LDA     #2
        JMP     ZERROR

; -------------
; HANDLE A 1-OP
; -------------

OP1:
        AND     #%00110000      ; ISOLATE ARGUMENT BITS
        BNE     OP1A
        JSR     GETLNG          ; 00 = LONG IMMEDIATE
        JMP     OP1EX

OP1A:
        CMP     #%00010000      ; TEST AGAIN
        BNE     OP1B
        JSR     GETSHT          ; 01 = SHORT IMMEDIATE
        JMP     OP1EX

OP1B:
        CMP     #%00100000      ; ONE MORE TEST
        BNE     BADOP1          ; UNDEFINED STATE!
        JSR     GETVAR          ; 10 = VARIABLE

OP1EX:
        JSR     V2A1            ; MOVE [VALUE] TO [ARG1], UPDATE [NARGS]
        LDX     #<OPT1          ; GET ADDR OF 1-OP TABLE
        LDY     #>OPT1          ; INTO [X/Y]
        LDA     OPCODE          ; RESTORE OPCODE
        AND     #%00001111      ; ISOLATE OP ID BITS
        CMP     #NOPS1          ; IF WITHIN RANGE,
        BCC     DODIS           ; EXECUTE THE 1-OP

; *** ERROR #3 -- ILLEGAL 1-OP ***

BADOP1:
        LDA     #3
        JMP     ZERROR

; -------------
; HANDLE A 2-OP
; -------------

OP2:
        AND     #%01000000      ; ISOLATE 1ST ARG BIT
        BNE     OP2A
        JSR     GETSHT          ; 0 = SHORT IMMEDIATE
        JMP     OP2B
OP2A:
        JSR     GETVAR          ; 1 = VARIABLE
OP2B:
        JSR     V2A1            ; [VALUE] TO [ARG1], UPDATE [NARGS]

        LDA     OPCODE          ; RESTORE OPCODE BYTE
        AND     #%00100000      ; ISOLATE 2ND ARG BIT
        BNE     OP2C
        JSR     GETSHT          ; 0 = SHORT IMMEDIATE
        JMP     OP2D
OP2C:
        JSR     GETVAR          ; 1 = VARIABLE
OP2D:
        LDA     VALUE+LO        ; MOVE 2ND [VALUE]
        STA     ARG2+LO         ; INTO [ARG2]
        LDA     VALUE+HI
        STA     ARG2+HI
        INC     NARGS           ; UPDATE ARGUMENT COUNT

; EXECUTE A 2-OP OR EXTENDED 2-OP

OP2EX:
        LDX     #<OPT2          ; LSB OF DISPATCH TABLE
        LDY     #>OPT2          ; MSB
        LDA     OPCODE          ; RESTORE OPCODE BYTE
        AND     #%00011111      ; ISOLATE OP ID BITS
        CMP     #NOPS2
        BCS     BADOP2          ; ERROR IF OUT OF RANGE
        JMP     DODIS           ; ELSE DISPATCH

; *** ERROR #4 -- ILLEGAL 2-OP ****

BADOP2:
        LDA     #4
        JMP     ZERROR

; --------------------------------------
; MOVE [VALUE] TO [ARG1], UPDATE [NARGS]
; --------------------------------------

V2A1:
        LDA     VALUE+LO
        STA     ARG1+LO
        LDA     VALUE+HI
        STA     ARG1+HI
        INC     NARGS
        RTS
