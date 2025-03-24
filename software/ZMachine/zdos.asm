;        PAGE
;        SBTTL   "--- Z-DOS: DOS/65 ---"

; ---------------------
; GET Z-BLOCK FROM DISK
; ---------------------

; ENTRY: Z-BLOCK # IN [BLOCK]
;        TARGET PAGE IN [DBUFF+HI]
GETDSK:
        CLD
        CLC

        LDA     DBLOCK+LO       ; Load low byte of DBLOCK
        ASL     A               ; Multiply by 2
        STA     D65BLOCK+LO     ; Store low byte
        LDA     DBLOCK+HI       ; Load high byte of DBLOCK
        ROL     A               ; Rotate left (gets carry from previous ASL)
        STA     D65BLOCK+HI     ; Store high byte

; read 256 byte block (zblock should be file block*2)
; EXTENT = INTEGER.PART.OF(n/128)
; 16 bit shift x6
        LDA     D65BLOCK+LO     ; CALCULATE EXTENT
        ASL     A
        STA     DTEMP+LO
        LDA     D65BLOCK+HI
        ROL     A
        STA     GAMEFCB+12      ; = EXTENT (MAGIC!)

        LDA     D65BLOCK+LO     ; CALCULATE RECORD = n-(EXTENT*128)
        AND     #$7F
        STA     GAMEFCB+32      ; Store Record Number into FCB (Also Magic!)

        LDA     #<GAMEFCB       ; Open Extent
        LDY     #>GAMEFCB
        LDX     #15
        JSR     PEM
        CMP     #$FF
        BEQ     GETDSKERR

        LDA     #0              ; Set Buffer Address
        LDY     DBUFF+HI
        LDX     #26
        JSR     PEM

        LDA     #<GAMEFCB       ; Read First sector (128 bytes)
        LDY     #>GAMEFCB
        LDX     #20
        JSR     PEM
        CMP     #$FF
        BEQ     GETDSKERR

        LDA     #$80            ; Set Buffer Address
        LDY     DBUFF+HI
        LDX     #26
        JSR     PEM

        LDA     #<GAMEFCB       ; Read Second sector (128 bytes)
        LDY     #>GAMEFCB
        LDX     #20
        JSR     PEM
        CMP     #$FF
        BEQ     GETDSKERR

        LDA     #<SPAREBYTES    ; Set Buffer Address to safe spot
        LDY     #>SPAREBYTES
        LDX     #26
        JSR     PEM

        LDA     #<GAMEFCB       ; Close Extent
        LDY     #>GAMEFCB
        LDX     #16
        JSR     PEM

        INC     DBUFF+HI        ; POINT TO NEXT RAM PAGE
        INC     DBLOCK+LO       ; POINT TO NEXT
        BNE     :+              ; Z-BLOCK
        INC     DBLOCK+HI
:
        CLC
        RTS
GETDSKERR:
        LDA     #14
        JMP     ZERROR


; ENTRY FOR "RESTORE" ([TRACK], [SECTOR] & [DRIVE] PRE-ASSIGNED)

GETRES:
;        CLC                     ; CARRY CLEAR = "READ BLOCK"
;        JSR     DISK            ; GO DO IT!
;        BCS     DSXERR          ; ERROR IF CARRY SET
;
;        LDY     #0              ; MOVE CONTENTS OF [IOBUFF]
;GDKL:
;        LDA     IOBUFF,Y        ; TO THE
;        STA     (DBUFF),Y       ; TARGET PAGE IN [DBUFF]
;        INY
;        BNE     GDKL
;
;        INC     DBLOCK+LO       ; POINT TO NEXT
;        BNE     GDEX            ; Z-BLOCK
;        INC     DBLOCK+HI
;
;GDEX:
;        JMP     NXTSEC          ; POINT TO NEXT SECTOR & PAGE

; --------------------
; PUT [DBLOCK] TO DISK
; --------------------

;        PAGE TO WRITE IN [DBUFF]

PUTDSK:

        LDA     #0              ; Set Buffer Address
        LDY     DBUFF+HI
        LDX     #26
        JSR     PEM

        LDA     #<SAVEFCB       ; Write Record
        LDY     #>SAVEFCB
        LDX     #21
        JSR     PEM

        LDA     #80              ; Set Buffer Address
        LDY     DBUFF+HI
        LDX     #26
        JSR     PEM

        LDA     #<SAVEFCB       ; Write Record
        LDY     #>SAVEFCB
        LDX     #21
        JSR     PEM

        CMP     #$FF
        BEQ     WRTERR

        INC     DBUFF+HI        ; POINT TO NEXT RAM PAGE
        RTS
WRTERR:
        SEC
        RTS


DSXERR:
        LDA     #14

QERR:
        JMP     ZERROR

; -----------------------------
; SET UP SAVE & RESTORE SCREENS
; -----------------------------

SAVRES:
        JSR     ZCRLF           ; CLEAR THE BUFFER
        LDX     #0
        STX     SCRIPT          ; DISABLE SCRIPTING
        CLC
        RTS



; FALL THROUGH ...

; --------------------------------
; ACTIVATE CURSOR, CLEAR KEY QUEUE
; --------------------------------

CURSON:
        RTS

; -----------------------------
; GET SAVE & RESTORE PARAMETERS
; -----------------------------

FILENAME:
        .BYTE   EOL
        .BYTE   "File Name:"
FILENAMEL       = *-FILENAME

NO:
        .BYTE   "No"
NOL             = *-NO

YES:
        .BYTE   "Yes"
YESL            = *-YES




ALLSET:
        LDX     #<YES           ; PRINT "YES"
        LDA     #>YES
        LDY     #YESL
        JSR     DLINE


; ---------------------
; "PRESS RETURN" PROMPT
; ---------------------

RETURN:
        LDX     #<RTN
        LDA     #>RTN
        LDY     #RTNL
        JSR     DLINE           ; SHOW PROMPT

; ENTRY FOR QUIT/RESTART

GETRET:
        JSR     CURSON          ; ENABLE CURSOR

GTRT:
        JSR     GETKEY          ; WAIT FOR [RETURN]
        CMP     #EOL
        BEQ     RETEX
        JSR     BOOP            ; ACCEPT NO
        JMP     GTRT            ; SUBSTITUTES!

RETEX:
        RTS

RTN:
        .BYTE   EOL
        .BYTE   "Press [RETURN] to continue."
        .BYTE   EOL
        .BYTE   ">"
RTNL            = *-RTN


; -------------------------
; SET UP PHONEY STATUS LINE
; -------------------------

; ENTRY: TEXT SET UP FOR "DLINE"

SROOM:
        RTS

; ---------
; SAVE GAME
; ---------



ZSAVE:
        JSR     SAVRES          ; SET UP SCREEN

; get filename and place in FCB
; then open file for write/create

        LDX     #<FILENAME
        LDA     #>FILENAME
        LDY     #FILENAMEL
        JSR     DLINE           ; "FILENAME ..."

        LDA     #' '
        STA     SAVEFCB+1
        STA     SAVEFCB+2
        STA     SAVEFCB+3
        STA     SAVEFCB+4
        STA     SAVEFCB+5
        STA     SAVEFCB+6
        STA     SAVEFCB+7
        STA     SAVEFCB+8

        LDA     #<SPAREBYTES
        STA     ARG1
        LDA     #>SPAREBYTES
        STA     ARG1+1
        JSR     INPUT
        CMP     #10
        BCC     :+
        LDA     #9
:
        TAX
        DEX
SFNLOOP:
        LDA     SPAREBYTES,X

        CMP     #'0'
        BCS     :+
        LDA     #'_'
        JMP     SFNOK
:
        CMP     #':'
        BCC     SFNOK
        CMP     #'A'
        BCS     :+
        LDA     #'_'
        JMP     SFNOK
:
        CMP     #'Z'+1
        BCC     SFNOK
        CMP     #'a'
        BCS     :+
        LDA     #'_'
        JMP     SFNOK
:
        CMP     #'z'+1
        BCC     :+
        LDA     #'_'
        JMP     SFNOK
:
        SEC
        SBC     #$20
SFNOK:
        STA     SAVEFCB,X
        DEX
        BPL     SFNLOOP

        LDA     #0
        STA     SAVEFCB+0
        STA     SAVEFCB+12
        LDA     #'S'
        STA     SAVEFCB+9
        LDA     #'A'
        STA     SAVEFCB+10
        LDA     #'V'
        STA     SAVEFCB+11

        LDA     #<SAVEFCB       ; Open Extent
        LDY     #>SAVEFCB
        LDX     #22
        JSR     PEM
        CMP     #$FF
        BEQ     BADSAV

        JMP     DOSAVE          ; ERROR IF CARRY SET

BADSAV:
        JMP     PREDF           ; PREDICATE FAILS

DOSAVE:

; SAVE GAME PARAMETERS IN [BUFSAV]

        LDA     ZBEGIN+ZID      ; MOVE GAME ID
        STA     BUFSAV+0        ; INTO 1ST 2 BYTES
        LDA     ZBEGIN+ZID+1    ; OF THE AUX LINE BUFFER
        STA     BUFSAV+1

        LDA     ZSP             ; MOVE [ZSP]
        STA     BUFSAV+2        ; TO 3RD BYTE
        LDA     OLDZSP          ; MOVE [OLDZSP]
        STA     BUFSAV+3        ; TO 4TH

        LDX     #2              ; MOVE CONTENTS OF [ZPC]
ZPCSAV:
        LDA     ZPC,X           ; TO BYTES 5-7
        STA     BUFSAV+4,X      ; OF [BUFSAV]
        DEX
        BPL     ZPCSAV

; WRITE [LOCALS]/[BUFSAV] PAGE TO DISK

        LDA     #>LOCALS
        STA     DBUFF+HI        ; POINT TO THE PAGE
        JSR     PUTDSK          ; AND WRITE IT OUT
        BCS     BADSAV          ; CATCH WRITE ERROR HERE

; WRITE CONTENTS OF Z-STACK TO DISK

        LDA     #>ZSTAKL        ; POINT TO 1ST PAGE
        STA     DBUFF+HI
        JSR     PUTDSK          ; WRITE 1ST AND
        BCS     BADSAV
        JSR     PUTDSK          ; 2ND PAGE OF Z-STACK
        BCS     BADSAV

; WRITE ENTIRE GAME PRELOAD TO DISK

        LDA     ZCODE           ; POINT TO 1ST PAGE
        STA     DBUFF+HI        ; OF PRELOAD

        LDX     ZBEGIN+ZPURBT   ; GET # IMPURE PAGES
        INX                     ; USE FOR INDEXING
        STX     I+LO

LSAVE:
        JSR     PUTDSK
        BCS     BADSAV
        DEC     I+LO
        BNE     LSAVE

        LDA     #<SPAREBYTES              ; Set Buffer Address
        LDY     #>SPAREBYTES
        LDX     #26
        JSR     PEM

        LDA     #<SAVEFCB       ; Close
        LDY     #>SAVEFCB
        LDX     #16
        JSR     PEM

        JMP     PREDS           ; ELSE PREDICATE SUCCEEDS

; ------------
; RESTORE GAME
; ------------

all that is left is to handle the restore game.
then test the crap out of it.
would be good to also test on the actual hardware.

RES:
        .BYTE   "Restore Position"
        .BYTE   EOL
RESL            = *-RES

RSING:
        .BYTE   EOL
        .BYTE   "Restoring position "
RSPOS:
        .BYTE   "* ..."
        .BYTE   EOL
RSINGL          = *-RSING

ZREST:
        JSR     SAVRES

        LDX     #<RES
        LDA     #>RES
        LDY     #RESL
        JSR     SROOM           ; "RESTORE POSITION"

;        JSR     PARAMS          ; GET PARAMETERS
;       BCS     BADRES          ; ERROR IF CARRY SET

        LDX     #<RSING
        LDA     #>RSING
        LDY     #RSINGL
        JSR     DLINE           ; "RESTORING POSITION X ..."

; SAVE LOCALS IN CASE OF ERROR

        LDX     #31
LOCSAV:
        LDA     LOCALS,X        ; COPY ALL LOCALS
        STA     $0100,X         ; TO BOTTOM OF MACHINE STACK
        DEX
        BPL     LOCSAV

        LDA     #>LOCALS
        STA     DBUFF+HI
        JSR     GETRES          ; RETRIEVE 1ST BLOCK OF PRELOAD

        LDA     BUFSAV+0        ; DOES 1ST BYTE OF SAVED GAME ID
        CMP     ZBEGIN+ZID      ; MATCH THE CURRENT ID?
        BNE     WRONG           ; WRONG DISK IF NOT

        LDA     BUFSAV+1        ; WHAT ABOUT THE 2ND BYTE?
        CMP     ZBEGIN+ZID+1
        BEQ     RIGHT           ; CONTINUE IF BOTH BYTES MATCH

; HANDLE INCORRECT SAVE DISK

WRONG:
        LDX     #31             ; RESTORE ALL SAVED LOCALS
WR0:
        LDA     $0100,X
        STA     LOCALS,X
        DEX
        BPL     WR0

BADRES:
        JMP     PREDF           ; PREDICATE FAILS

; CONTINUE RESTORE

RIGHT:
        LDA     ZBEGIN+ZSCRIP   ; SAVE BOTH FLAG BYTES
        STA     I+LO
        LDA     ZBEGIN+ZSCRIP+1
        STA     I+HI

        LDA     #>ZSTAKL        ; RETRIEVE OLD CONTENTS OF
        STA     DBUFF+HI        ; Z-STACK
        JSR     GETRES          ; GET 1ST BLOCK OF Z-STACK
        JSR     GETRES          ; AND 2ND BLOCK

        LDA     ZCODE
        STA     DBUFF+HI
        JSR     GETRES          ; GET 1ST BLOCK OF PRELOAD

        LDA     I+LO            ; RESTORE THE STATE
        STA     ZBEGIN+ZSCRIP   ; OF THE FLAG WORD
        LDA     I+HI
        STA     ZBEGIN+ZSCRIP+1

        LDA     ZBEGIN+ZPURBT   ; GET # PAGES TO LOAD
        STA     I+LO

LREST:
        JSR     GETRES          ; FETCH THE REMAINDER
        DEC     I+LO            ; OF THE PRELOAD
        BNE     LREST

; RESTORE THE STATE OF THE SAVED GAME

        LDA     BUFSAV+2        ; RESTORE THE [ZSP]
        STA     ZSP
        LDA     BUFSAV+3        ; AND THE [OLDZSP]
        STA     OLDZSP

        LDX     #2              ; RESTORE THE [ZPC]
RESZPC:
        LDA     BUFSAV+4,X
        STA     ZPC,X
        DEX
        BPL     RESZPC

        LDA     #FALSE
        STA     ZPCFLG          ; INVALIDATE [ZPC]

        JMP     PREDS           ; PREDICATE SUCCEEDS

GAMEFCB:
        .BYTE   00,00,00,00,00,00,00,00
        .BYTE   00,00,00,00,00,00,00,00
        .BYTE   00,00,00,00,00,00,00,00
        .BYTE   00,00,00,00,00,00,00,00
        .BYTE   00

SAVEFCB:
        .BYTE   00,00,00,00,00,00,00,00
        .BYTE   00,00,00,00,00,00,00,00
        .BYTE   00,00,00,00,00,00,00,00
        .BYTE   00,00,00,00,00,00,00,00
        .BYTE   00

SPAREBYTES:
D65BLOCK:
        .RES    256
