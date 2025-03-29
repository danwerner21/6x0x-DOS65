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

        LDA     #<GAMEFCB       ; Open Extent
        LDY     #>GAMEFCB
        LDX     #15
        JSR     PEM
        CMP     #$FF
        BEQ     OPENDSKERR

        LDA     #<SPAREBYTES    ; Set Buffer Address to safe spot
        LDY     #>SPAREBYTES
        LDX     #26
        JSR     PEM

        LDA     #<GAMEFCB       ; Close Extent
        LDY     #>GAMEFCB
        LDX     #16
        JSR     PEM
        RTS

OPENDSKERR:

        LDX     #<OPENERR
        LDA     #>OPENERR
        LDY     #OPENERRL
        JSR     DLINE

        LDA     #14
        JMP     ZERROR

OPENERR:
        .BYTE   EOL,EOL,EOL,EOL
        .BYTE   "Unable to open game file."
        .BYTE   EOL
        .BYTE   "USAGE:  ZIP <GAMEFILE>"
        .BYTE   EOL
OPENERRL           = *-OPENERR

; -------------------
; CLOSE CURRENT DRIVE
; -------------------

DCLOSE:
        RTS

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
