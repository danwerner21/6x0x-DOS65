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
        LDA     DBLOCK+HI
        ROL     A
        STA     DTEMP+HI
        CLC
        LDA     DTEMP+LO
        ASL     A
        STA     DTEMP+LO
        LDA     DTEMP+HI
        ROL     A
        STA     GAMEFCB+12      ; = EXTENT (MAGIC!)
        LDA     D65BLOCK+LO     ; CALCULATE RECORD = n-(EXTENT*128)
        AND     #$3F
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


;        LDA     DBLOCK+LO       ; Z-BLOCK ID IS DIVIDEND
;        STA     DVD+LO
;        LDA     DBLOCK+HI
;        AND     #%00000001      ; MASK ALL BUT BIT 0
;        STA     DVD+HI          ; FOR 128K VIRTUAL LIMIT
;
;        LDA     #0
;        STA     DSOR+HI         ; CLEAR MSB DIVISOR
;        STA     DTEMP+LO        ; AND TEMP VARIABLE
;        STA     DTEMP+HI

;        LDX     #17             ; USE 17 SECTORS/TRACK
;        STX     DSOR+LO         ; LSB OF DIVISOR
;        DEX                     ; (= 16) INIT DIVIDE INDEX
;        CLC
;
;DVLP:
;        ROL     DVD+LO
;        ROL     DVD+HI
;        ROL     DTEMP+LO
;        ROL     DTEMP+HI
;
;       LDA     DTEMP+LO
;        SEC
;        SBC     DSOR+LO
;        TAY
;        LDA     DTEMP+HI
;        SBC     DSOR+HI
;        BCC     DVLP1
;        STY     DTEMP+LO
;        STA     DTEMP+HI
;
;DVLP1:
;        DEX
;        BNE     DVLP

;        ROL     DVD+LO          ; SHIFT LAST CARRY
;        ROL     DVD+HI          ; INTO QUOTIENT

;        LDA     DTEMP+LO        ; REMAINDER IN [DTEMP]
;        STA     SECTOR          ; IS SECTOR ID (0-16)

;        LDA     DVD+LO          ; QUOTIENT IN [DVD] IS TRACK ID
;        CLC
;        ADC     #5              ; Z-CODE STARTS ON TRACK 5

;        CMP     #17             ; BELOW TRACK 17?
;        BCC     DVLP2           ; YES, WE'RE DONE
;        CLC                     ; ELSE SKIP OVER
;        ADC     #1              ; TRACK 17
;        CMP     #36             ; OUT OF RANGE?
;        BCS     TRKERR          ; IF > 35, TRACK ERROR
;        CMP     #18             ; IS THIS TRACK 18?
;        BNE     DVLP2           ; NO, EXIT
;        INC     SECTOR          ; ELSE SKIP OVER
;        INC     SECTOR          ; 1ST 2 SECTORS
;DVLP2:
;        STA     TRACK

;	LDA	DBLOCK+LO	; GET LSB OF BLOCK ID
;	AND	#%00001111	; MASK TO GET
;	STA	SECTOR		; SECTOR # (0-15)

;	LDA	DBLOCK+HI	; GET MSB OF BLOCK ID
;	AND	#%00001111	; MASK OUT GARBAGE IN BITS 7-4
;	ASL	A		; SHIFT THE LOW NIBBLE
;	ASL	A		; INTO THE HIGH NIBBLE
;	ASL	A
;	ASL	A
;	STA	TRACK		; AND SAVE IT HERE FOR A MOMENT

;	LDA	DBLOCK+LO	; GET LSB OF BLOCK ID AGAIN
;	AND	#%11110000	; MASK OUT SECTOR #
;	LSR	A		; SHIFT THE HIGH NIBBLE
;	LSR	A		; INTO THE LOW NIBBLE
;	LSR	A
;	LSR	A
;	ORA	TRACK		; SUPERIMPOSE NEW HIGH NIBBLE

;	CLC
;	ADC	#5		; Z-CODE STARTS ON TRACK 5

;	CMP	#17		; BELOW TRACK 17?
;	BCC	TKOK		; USE AS-IS IF SO
;	CLC			; ELSE SKIP OVER
;	ADC	#1		; TRACK 17
;	CMP	#36		; ANYTHING HIGHER THAN TRACK 35
;	BCS	TRKERR		; IS AN ERROR
;	CMP	#18		; IS THIS TRACK 18?
;	BNE	TKOK		; NO, WE'RE DONE
;	INC	SECTOR		; ELSE ADD 2 TO
;	INC	SECTOR		; [SECTOR] TO AVOID DIRECTORY
;TKOK:	STA	TRACK

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

; ENTRY: [TRACK], [SECTOR] & [DRIVE] ASSIGNED
;        PAGE TO WRITE IN [DBUFF]

PUTDSK:
;        LDY     #0              ; MOVE PAGE AT [DBUFF]
;PTKL:
;        LDA     (DBUFF),Y       ; INTO
;        STA     IOBUFF,Y        ; [IOBUFF] FOR I/O
;        INY
;        BNE     PTKL
;
;        SEC                     ; CARRY SET = "WRITE BLOCK"
;        JSR     DISK
;        BCS     WRTERR          ; CARRY SET IF ERROR
;
;NXTSEC:
;        INC     SECTOR          ; POINT TO NEXT SECTOR
;        LDA     SECTOR
;        AND     #%00001111      ; OVEFLOWED?
;        BNE     SECTOK          ; CONTINUE IF NOT
;        INC     TRACK           ; ELSE UPDATE TRACK #
;SECTOK:
;       STA     SECTOR          ; AND SECTOR #
;
;        INC     DBUFF+HI        ; POINT TO NEXT RAM PAGE
;WRTERR:
        RTS

; *** ERROR #12: DISK ADDRESS OUT OF RANGE ***

;TRKERR:
;        LDA     #12
;        BNE     QERR

; *** ERROR #14: DRIVE ACCESS ***

DSXERR:
        LDA     #14

QERR:
        JMP     ZERROR

; -----------------------------
; SET UP SAVE & RESTORE SCREENS
; -----------------------------

SAVRES:
        JSR     ZCRLF           ; CLEAR THE BUFFER
        JSR     CLS

        LDX     #0
        STX     SCRIPT          ; DISABLE SCRIPTING
        LDY     #0
        CLC
;        JMP     PLOT            ; HOME CURSOR & RETURN

; -----------------
; DISPLAY A DEFAULT
; -----------------

; ENTRY: DEFAULT (0-8) IN [A]

DEFAL:
        .BYTE   " (Default = "
DEFNUM:
        .BYTE   "*) >"
DEFALL          = *-DEFAL

DODEF:
        CLC
        ADC     #'1'            ; CONVERT TO ASCII 1-9
        STA     DEFNUM          ; INSERT IN STRING

        LDX     #<DEFAL
        LDA     #>DEFAL
        LDY     #DEFALL
        JSR     DLINE           ; PRINT THE STRING

; FALL THROUGH ...

; --------------------------------
; ACTIVATE CURSOR, CLEAR KEY QUEUE
; --------------------------------

CURSON:
        RTS

; -----------------------------
; GET SAVE & RESTORE PARAMETERS
; -----------------------------

POSIT:
        .BYTE   EOL
        .BYTE   "Position 1-5"
POSITL          = *-POSIT

WDRIV:
        .BYTE   EOL
        .BYTE   "Drive 8 or 9"
WDRIVL          = *-WDRIV

MIND:
        .BYTE   EOL
        .BYTE   EOL
        .BYTE   "Position "
MPOS:
        .BYTE   "*, Drive "
MDRI:
        .BYTE   "*."
        .BYTE   EOL
        .BYTE   "Are you sure? (Y/N) >"
MINDL           = *-MIND

INSM:
        .BYTE   EOL
        .BYTE   "Insert SAVE disk into Drive "
SAVDRI:
        .BYTE   "*."
INSML           = *-INSM

YES:
        .BYTE   "YES"
        .BYTE   EOL
YESL            = *-YES

NO:
        .BYTE   "NO"
        .BYTE   EOL
NOL             = *-NO

PARAMS:
        LDX     #<POSIT
        LDA     #>POSIT
        LDY     #POSITL
        JSR     DLINE           ; "POSITION (1-5)"

; GET GAME POSITION

CHANGE:
        LDA     GPOSIT          ; SHOW THE CURRENT
        JSR     DODEF           ; DEFAULT POSITION

GETPOS:
        JSR     GETKEY          ; WAIT FOR A KEY
        CMP     #EOL            ; IF [RETURN],
        BEQ     POSSET          ; USE DEFAULT
        SEC
        SBC     #'1'            ; ELSE CONVERT ASCII TO BINARY
        CMP     #5              ; IF BELOW "6"
        BCC     SETPOS          ; MAKE IT THE NEW DEFAULT
        JSR     BOOP            ; ELSE RAZZ
        JMP     GETPOS          ; AND TRY AGAIN

POSSET:
        LDA     GPOSIT          ; USE DEFAULT

SETPOS:
        STA     TPOSIT          ; USE KEYPRESS
        CLC
        ADC     #'1'            ; CONVERT TO ASCII "1"-"5"
        STA     MPOS            ; STORE IN TEMP STRING
        STA     SVPOS
        STA     RSPOS
        JSR     LETTER          ; AND DISPLAY IT

; GET DRIVE ID

        LDX     #<WDRIV
        LDA     #>WDRIV
        LDY     #WDRIVL
        JSR     DLINE           ; "DRIVE 8 OR 9"

        LDA     GDRIVE          ; SHOW DEFAULT
        CLC                     ; CONVERT 0 OR 1
        ADC     #7              ; TO 7 OR 8
        JSR     DODEF           ; SO DEFAULT WILL BE CORRECT

GETDRV:
        JSR     GETKEY          ; GET A KEYPRESS
        CMP     #EOL            ; IF [RETURN],
        BEQ     DRVSET          ; USE DEFAULT
        SEC
        SBC     #'8'            ; CONVERT TO BINARY 0 OR 1
        CMP     #2              ; IF WITHIN RANGE,
        BCC     SETDRV          ; SET NEW DEFAULT
        JSR     BOOP
        JMP     GETDRV          ; ELSE TRY AGAIN

DRVSET:
        LDA     GDRIVE          ; USE DEFAULT

SETDRV:
        STA     TDRIVE          ; USE [A]
        CLC
        ADC     #'8'            ; CONVERT TO ASCII 8 OR 9
        STA     SAVDRI          ; STORE IN DRIVE STRING
        STA     MDRI            ; AND IN TEMP STRING
        JSR     LETTER          ; AND SHOW NEW SETTING

        LDX     #<MIND          ; SHOW TEMPORARY SETTINGS
        LDA     #>MIND
        LDY     #MINDL
        JSR     DLINE

        JSR     CURSON

GETYES:
        JSR     GETKEY
        CMP     #'Y'            ; IF REPLY IS "Y"
        BEQ     ALLSET          ; ACCEPT RESPONSES
        CMP     #'y'
        BEQ     ALLSET

        CMP     #'N'            ; IF REPLY IS N,
        BEQ     RETRY           ; DO A RETRY
        CMP     #'n'
        BEQ     RETRY

        JSR     BOOP            ; INSIST ON Y/RETURN
        JMP     GETYES          ; OR N

RETRY:
        LDX     #<NO            ; ELSE PRINT "NO"
        LDA     #>NO
        LDY     #NOL
        JSR     DLINE
        JMP     PARAMS          ; AND TRY AGAIN

ALLSET:
        LDX     #<YES           ; PRINT "YES"
        LDA     #>YES
        LDY     #YESL
        JSR     DLINE

;        LDA     TDRIVE          ; MAKE THE TEMPORARY DRIVE
;        STA     GDRIVE          ; THE DEFAULT DRIVE
;        LDA     TPOSIT          ; AND THE TEMP POSITION
;        STA     GPOSIT          ; THE DEFAULT POSITION

; CALC TRACK & SECTOR OF GAME POSITION

;        ASL     A               ; * 2
;        STA     TRACK           ; SAVE HERE FOR A MOMENT
;        ASL     A               ; * 4
;        CLC
;        ADC     TRACK           ; * 6 (6 TRACKS PER POSITION)
;        STA     TRACK
;        INC     TRACK           ; 1ST TRACK IS 1!
;        LDA     #0
;        STA     SECTOR          ; ALWAYS START ON SECTOR #0
;        LDA     GDRIVE          ; TRY TO OPEN SPECIFIED DRIVE
;        CLC
;        ADC     #8
;        JSR     DOPEN           ; THE DEFAULT DRIVE
;        BCS     PARERR          ; CARRY SET IF ERROR

SREADY:
        LDX     #<INSM
        LDA     #>INSM
        LDY     #INSML
        JSR     DLINE           ; "INSERT SAVE DISK IN DRIVE X."
        JSR     RETURN          ; "PRESS [RETURN] TO CONTINUE."
        CLC                     ; FOR SUCCESS
PARERR:
        RTS

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

; --------------------
; PROMPT FOR GAME DISK
; --------------------

GAME:
        .BYTE   EOL
        .BYTE   "Insert STORY disk into Drive 8."
GAMEL           = *-GAME

TOBOOT:
        LDA     #8
        JSR     DOPEN           ; CLOSE OLD, OPEN BOOT DRIVE

        LDX     #<GAME
        LDA     #>GAME
        LDY     #GAMEL
        JSR     DLINE           ; "INSERT STORY DISK IN DRIVE #8."

        JSR     RETURN          ; "PRESS [RETURN] TO CONTINUE:"
TBT0:
        LDA     #$FF            ; RE-ENABLE
        STA     SCRIPT          ; SCRIPTING
        JMP     CLS             ; CLEAR SCREEN & RETURN

; -------------------------
; SET UP PHONEY STATUS LINE
; -------------------------

; ENTRY: TEXT SET UP FOR "DLINE"

SROOM:
        RTS

; ---------
; SAVE GAME
; ---------

SAV:
        .BYTE   "Save Position"
        .BYTE   EOL
SAVL            = *-SAV

SVING:
        .BYTE   EOL
        .BYTE   "Saving position "
SVPOS:
        .BYTE   "* ..."
        .BYTE   EOL
SVINGL          = *-SVING

ZSAVE:
        JSR     SAVRES          ; SET UP SCREEN

        LDX     #<SAV
        LDA     #>SAV
        LDY     #SAVL
        JSR     SROOM           ; "SAVE POSITION"

        JSR     PARAMS          ; GET PARAMETERS
        BCC     DOSAVE          ; ERROR IF CARRY SET

BADSAV:
        JSR     TOBOOT          ; GET BOOT DISK
        JMP     PREDF           ; PREDICATE FAILS

DOSAVE:
        LDX     #<SVING
        LDA     #>SVING
        LDY     #SVINGL
        JSR     DLINE           ; "SAVING POSITION X ..."

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

        JSR     TOBOOT          ; PROMPT FOR GAME DISK
        JMP     PREDS           ; ELSE PREDICATE SUCCEEDS

; ------------
; RESTORE GAME
; ------------

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

        JSR     PARAMS          ; GET PARAMETERS
        BCS     BADRES          ; ERROR IF CARRY SET

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
        JSR     TOBOOT          ; PROMPT FOR GAME DISK
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

        JSR     TOBOOT          ; PROMPT FOR GAME DISK
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
