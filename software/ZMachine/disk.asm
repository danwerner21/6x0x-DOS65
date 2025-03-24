;        PAGE
;        SBTTL   "--- DISK ACCESS: DOS/65 ---"

; --------------
; OPEN DRIVE [A]
; WILL OPEN FILE SPECIFIED IN COMMAND LINE
; --------------
DOPEN:
        LDX     #12
:
        DEX
        LDA     DFLFCB,X
        STA     GAMEFCB,X
        CPX     #00
        BNE     :-
        CLC
        RTS
DOPEN_ERR:
        SEC
        RTS


; -------------------
; CLOSE CURRENT DRIVE
; -------------------

DCLOSE:
        RTS

;        LDA     #<DFLFCB
;        LDY     #>DFLFCB
;        LDX     #16
;        JSR     PEM

; FALL THROUGH ...


; ----------------
; DIVIDE [A] BY 10
; ----------------

; EXIT: QUOTIENT IN [X], REMAINDER IN [A]

DIV10:
        LDX     #0              ; START WITH ZERO QUOTIENT

D10L:
        CMP     #10             ; IF DIVISOR < 10,
        BCC     D10EX           ; WE'RE DONE
        SBC     #10             ; ELSE SUBTRACT ANOTHER 10
        INX                     ; UPDATE QUOTIENT
        BNE     D10L            ; BRANCH ALWAYS

D10EX:
        RTS
