;        PAGE
;        SBTTL   "--- 0-OPS ---"

; -----
; RTRUE
; -----

; SIMULATE A "RETURN 1"

ZRTRUE:
        LDX     #1

ZRT0:
        LDA     #0

ZRT1:
        STX     ARG1+LO         ; GIVE TO
        STA     ARG1+HI         ; [ARG1]
        JMP     ZRET            ; AND DO THE RETURN

; ------
; RFALSE
; ------

; SIMULATE A "RETURN 0"

ZRFALS:
        LDX     #0
        BEQ     ZRT0

; ------
; PRINTI
; ------

; PRINT Z-STRING FOLLOWING THE OPCODE

ZPRI:
        LDA     ZPCH            ; MOVE [ZPC] INTO [MPC]
        STA     MPCH
        LDA     ZPCM
        STA     MPCM
        LDA     ZPCL
        STA     MPCL

        LDA     #0
        STA     MPCFLG          ; [MPC] NO LONGER VALID

        JSR     PZSTR           ; PRINT THE Z-STRING AT [MPC]

        LDX     #5              ; COPY STATE OF [MPC]
PRIL:
        LDA     MPC,X           ; INTO [ZPC]
        STA     ZPC,X
        DEX
        BPL     PRIL
        RTS

; ------
; PRINTR
; ------

; DO A "PRINTI," FOLLOWED BY "CRLF" AND "RTRUE"

ZPRR:
        JSR     ZPRI
        JSR     ZCRLF
        JMP     ZRTRUE

; ------
; RSTACK
; ------

; "RETURN" WITH VALUE ON STACK

ZRSTAK:
        JSR     POPVAL          ; GET VALUE INTO [X/A]
        JMP     ZRT1            ; AND GIVE IT TO "RETURN"

; ------
; VERIFY
; ------

; VERIFY GAME CODE ON DISK

ZVER:
        JSR     VERNUM          ; DISPLAY ZIP VERSION NUMBER

        LDX     #3
        LDA     #0
ZVR:
        STA     J+LO,X          ; CLEAR [J], [K]
        STA     MPC,X           ; [MPC] AND [MPCFLG]
        DEX
        BPL     ZVR

        LDA     #64             ; POINT [MPC] TO Z-ADDRESS $00040
        STA     MPCL            ; 1ST 64 BYTES AREN'T CHECKED

        LDA     ZBEGIN+ZLENTH   ; GET LENGTH OF Z-CODE
        STA     I+HI            ; IN WORDS
        LDA     ZBEGIN+ZLENTH+1 ; FIRST MSB
        STA     I+LO            ; THEN LSB

        ASL     I+LO            ; CONVERT Z-CODE LENGTH
        ROL     I+HI            ; TO # BYTES
        ROL     K+LO            ; TOP BIT IN [K+LO]

        LDA     #K+HI           ; PATCH THE "GETBYT" ROUTINE
        STA     PATCH           ; TO USE [K+HI]=0 INSTEAD OF [ZPURE]

VSUM:
        JSR     GETBYT          ; GET A Z-BYTE INTO [A]
        CLC
        ADC     J+LO            ; ADD IT TO SUM
        STA     J+LO            ; IN [J]
        BCC     VSUM0
        INC     J+HI

VSUM0:
        LDA     MPCL            ; END OF Z-CODE YET?
        CMP     I+LO            ; CHECK LSB
        BNE     VSUM

        LDA     MPCM            ; MIDDLE BYTE
        CMP     I+HI
        BNE     VSUM

        LDA     MPCH            ; AND HIGH BIT
        CMP     K+LO
        BNE     VSUM

        LDA     #ZPURE          ; UNPATCH "GETBYT"
        STA     PATCH

        LDA     ZBEGIN+ZCHKSM+1 ; GET LSB OF CHECKSUM
        CMP     J+LO            ; DOES IT MATCH?
        BNE     BADVER          ; NO, PREDICATE FAILS

        LDA     ZBEGIN+ZCHKSM   ; ELSE CHECK MSB
        CMP     J+HI            ; LOOK GOOD?
        BNE     BADVER          ; IF MATCHED,
        JMP     PREDS           ; GAME IS OKAY

BADVER:
        JMP     PREDF
