;__SCREEN EDITOR_________________________________________________________________________________________________________________
;
; 	DOS/65 SCREEN EDITOR (ANSI TERMINAL/CONSOLE)
;	DAN WERNER 5/31/2014
;
;  DWERNER 2/17/23   ADD CODE TO ENSURE "SAVE AS" FILENAMES ARE PROPER DOS/65 FILENAMES
;________________________________________________________________________________________________________________________________
;

DFLFCB          = $107          ;DEFAULT FCB
PEM             = $103          ;PEM ENTRY
BOOT            = $100          ;WARM BOOT
TEA             = $800          ;EXECUTION ORG
CCMLNG          = 2048          ;CCM LENGTH
CRSYM           = 32            ;CR SYMBOL
LASTROW         = 20
MAXCOL          = 80
PAGESIZE        = 100           ;PAGE UP/DOWN SIZE (100 * 16 = 1600 bytes)
CR              = 13            ;CARRIAGE RETURN
SPACE           = 32            ;SPACE CHARACTER
DEL             = 127           ;DELETE CHARACTER
EOFCHAR         = $1A           ;DOS/65 END-OF-FILE MARKER

;ZERO PAGE
NAMED           = $02           ;"TO" FILE NAMED IF <128
TOIND           = $03           ;"TO" FILE INDEX
FRMIND          = $05           ;"FROM" FILE INDEX
TEMP            = $08           ;
FCBPTR          = $09           ; POINTER TO FCB FOR FILE OPS
CSRPOSX         = $0B           ;
CSRPOSY         = $0C           ;
EDTPOS          = $0D           ;
TMPPOS          = $0F           ;
TEMP1           = $11           ;
CURRENTLEN      = $12           ;
MODE            = $14           ; 0= OVERWRITE MODE, 1= INSERT MODE

;MACROS
        .macro SAVE_EDITSTATE
        LDA     CSRPOSX
        PHA
        LDA     CSRPOSY
        PHA
        LDA     EDTPOS
        PHA
        LDA     EDTPOS+1
        PHA
        .endmacro

        .macro RESTORE_EDITSTATE
        PLA
        STA     EDTPOS+1
        PLA
        STA     EDTPOS
        PLA
        STA     CSRPOSY
        PLA
        STA     CSRPOSX
        .endmacro

        .macro INC_EDTPOS
        INC     EDTPOS
        BNE     :+
        INC     EDTPOS+1
:
        .endmacro

        .macro DEC_EDTPOS
        LDA     EDTPOS
        BNE     :+
        DEC     EDTPOS+1
:
        DEC     EDTPOS
        .endmacro

;MAIN PROGRAM
        .SEGMENT "TEA"
        .ORG    $0800

        JSR     CLEARBUFFER
        JSR     DOS65LOAD
        JSR     PAINTSCREEN

        LDA     #$00
        STA     MODE
        STA     CSRPOSX
        STA     CSRPOSY
        JSR     GOCSR
        LDA     #<BUFFER        ; SETUP BUFFER
        STA     EDTPOS          ; STORE DEST BUFFER
        LDA     #>BUFFER        ;
        STA     EDTPOS+1        ; STORE DEST BUFFER



EDITLOOP:
        JSR     READKB          ; GET USER INPUT
;
        CPY     #$FF            ; ERROR?
        BEQ     EDITLOOP        ; YES, IGNORE AND RE-READ

        CPY     #$01            ; IF Y=1 IT IS A FUNCTION KEY, PROCESS IT
        BNE     EDIT            ; OTHERWISE (Y=0 REGULAR CHAR) GOTO EDIT

        STA     TEMP            ; STORE COMMAND IN TEMP
        LDY     #$00            ;
EDITLOOP1A:
        LDA     COMMANDTABLE,Y  ; CHECK TOKEN
        CMP     TEMP            ; COMPARE
        BNE     EDITLOOP2A      ;
        INY
        LDA     COMMANDTABLE,Y  ; CHECK TOKEN
        STA     JMPTMP+1        ;
        INY
        LDA     COMMANDTABLE,Y  ; CHECK TOKEN
        STA     JMPTMP+2        ;
JMPTMP:
        JSR     $0000
        JMP     EDITLOOP
EDITLOOP2A:
        CMP     #$FF
        BEQ     EDITLOOP3A
        INY
        INY
        INY
        JMP     EDITLOOP1A

EDITLOOP3A:
        JMP     EDITLOOP

COMMANDTABLE:
        .BYTE   $00,<CRSRUP,>CRSRUP
        .BYTE   $01,<CRSRDN,>CRSRDN
        .BYTE   $02,<CRSRLEFT,>CRSRLEFT
        .BYTE   $03,<CRSRRIGHT,>CRSRRIGHT
        .BYTE   $05,<PAGEUP,>PAGEUP
        .BYTE   $06,<PAGEDOWN,>PAGEDOWN
        .BYTE   $08,<BACKSPACE,>BACKSPACE
        .BYTE   $13,<RETURN_TO_OS,>RETURN_TO_OS
        .BYTE   $14,<TOGGLEMODE,>TOGGLEMODE
        .BYTE   $30,<INSERTCR,>INSERTCR
        .BYTE   127,<DELETECHAR,>DELETECHAR
        .BYTE   $18,<DOS65SAVE,>DOS65SAVE
        .BYTE   $19,<DOS65SAVEAS,>DOS65SAVEAS
        .BYTE   $24,<TRUNCATE,>TRUNCATE
        .BYTE   $FF



RETURN_TO_OS:
        JSR     CLEARSCREEN
        JSR     READKB_XON
        JMP     $0100


;__EDIT__________________________________________________________________________________________________________________________
;
; 	EDIT THE FILE
;
;________________________________________________________________________________________________________________________________
;
EDIT:
        PHA                     ; STORE KEYSTROKE
        LDA     MODE            ; OVERWRITE OR INSERT?
        BEQ     EDIT_OM         ; OVERWRITE MODE
        JSR     INSERTCHAR      ; INSERT CHAR SPACE
EDIT_OM:
        LDY     #$00            ;
        LDA     (EDTPOS),Y      ; IS CRSR OVER A CR?
        CMP     #13             ;
        BNE     EDIT_OMB        ; NO, CONTINUE
        INY                     ;
        LDA     (EDTPOS),Y      ; IS NEXT POS A NULL?
        BNE     EDIT_OMC        ;
EDIT_OMA:
        PLA                     ;
        LDY     #$00            ;
        STA     (EDTPOS),Y      ; PUT CHAR IN EDIT BUFFER
        JSR     TOCONSOLE       ; PLACE CHAR ON SCREEN
        INC_EDTPOS              ; MOVE EDTPOS 1 TO THE RIGHT
        LDA     #13             ;
        STA     (EDTPOS),Y      ; PUT CHAR IN EDIT BUFFER
        JSR     TOCONSOLE       ; PLACE CHAR ON SCREEN
        INC_EDTPOS              ; MOVE EDTPOS 1 TO THE RIGHT
        JSR     CRSRLEFT        ; MOVE CSR BACK 1
        JSR     EDITCHECK       ;
        JMP     EDITLOOP        ;

EDIT_OMB:
        PLA
        LDY     #$00
        STA     (EDTPOS),Y      ; PUT CHAR IN EDIT BUFFER
        JSR     TOCONSOLE       ; PLACE CHAR ON SCREEN
        INC_EDTPOS              ; MOVE EDTPOS 1 TO THE RIGHT
        JSR     EDITCHECK       ;
        JMP     EDITLOOP

EDIT_OMC:

        LDA     CSRPOSY         ; HAVE WE REACHED THE END OF THE SCREEN?
        CMP     #LASTROW-1      ;
        BNE     EDIT_OMC1       ; NO
        JSR     SCROLLDOWN      ; SCROLL
EDIT_OMC1:
        JSR     INSERTLINE
        JMP     EDIT_OMA

;__EDITCHECK_____________________________________________________________________________________________________________________
;
; 	IF EDTPOS > CURRENTLEN CHANGE CURRENTLEN
;
;________________________________________________________________________________________________________________________________
;
EDITCHECK:
        PHA
        LDA     EDTPOS+1        ; COMPARE HIGH BYTES FIRST
        CMP     CURRENTLEN+1
        BCC     EDITCHECK_1     ; IF EDTPOS+1 < CURRENTLEN+1, EXIT (EDTPOS < CURRENTLEN)
        BNE     EDITCHECK_UPDATE; IF EDTPOS+1 > CURRENTLEN+1, UPDATE (EDTPOS > CURRENTLEN)
        LDA     EDTPOS          ; HIGH BYTES EQUAL, CHECK LOW BYTES
        CMP     CURRENTLEN
        BCC     EDITCHECK_1     ; IF EDTPOS < CURRENTLEN, EXIT

EDITCHECK_UPDATE:               ; Set WORD CURRENTLEN= WORD EDTPOS
        LDA     EDTPOS+1
        STA     CURRENTLEN+1
        LDA     EDTPOS
        STA     CURRENTLEN      ; INCRIMENT WORD CURRENTLEN
        INC     CURRENTLEN
        BNE     EDITCHECK_1
        INC     CURRENTLEN+1
EDITCHECK_1:
        PLA
        RTS

;__INSERTCR______________________________________________________________________________________________________________________
;
; 	INSERT CR AT THE CURRENT POSITION
;
;________________________________________________________________________________________________________________________________
;
INSERTCR:
        SAVE_EDITSTATE

        LDY     #$00            ; move editpos to the end of the screen and store count into INSERTCHTEMP
        STY     INSERTCHTEMP    ; INSERTCHTEMP=0
        LDX     CSRPOSX         ; x=current x pos
INSERTCR1:
        INX                     ; x++
        INC     INSERTCHTEMP    ; INSERTCHTEMP++
        INC_EDTPOS              ; MOVE EDTPOS 1 TO THE RIGHT
        CPX     #MAXCOL         ; are we at the end of the line?
        BNE     INSERTCR1       ; no, loop

INSERTCR2:
        LDA     #$00            ; INSERT A LINE
        STA     CSRPOSX         ;
        INC     CSRPOSY         ;
        JSR     GOCSR           ;
        JSR     INSERTLINE      ; INSERT A LINE

        RESTORE_EDITSTATE
        SAVE_EDITSTATE
        LDX     INSERTCHTEMP    ;

INSERTCR3:
        LDY     #$00
        LDA     (EDTPOS),Y      ;
        LDY     INSERTCHTEMP    ;
        STA     (EDTPOS),Y      ;

        JSR     TOCONSOLE       ; PLACE CHAR ON SCREEN
        LDA     #$00            ;
        TAY                     ;
        STA     (EDTPOS),Y      ;

        INC_EDTPOS              ; MOVE EDTPOS 1 TO THE RIGHT
        DEX                     ;
        BNE     INSERTCR3       ;

        RESTORE_EDITSTATE
        JSR     GOCSR

        LDY     #$00            ;
        LDA     #13             ;
        STA     (EDTPOS),Y      ; PUT CHAR IN EDIT BUFFER
        JSR     TOCONSOLE       ; PLACE CHAR ON SCREEN
        INC_EDTPOS              ; MOVE EDTPOS 1 TO THE RIGHT


        LDX     INSERTCHTEMP    ;
        DEX                     ;
INSERTCR4:
        TXA
        PHA
        LDA     (EDTPOS),Y      ; PUT CHAR IN EDIT BUFFER
        JSR     TOCONSOLE       ; PLACE CHAR ON SCREEN
        INC_EDTPOS              ; MOVE EDTPOS 1 TO THE RIGHT
        PLA                     ;
        TAX                     ;
        DEX                     ;
        BNE     INSERTCR4       ;

        JMP     GOCSR

INSERTCHTEMP:
        .BYTE   0               ;

;__TRUNCATE______________________________________________________________________________________________________________________
;
; 	TRUNCATE FILE AT THE CURRENT POSITION
;
;________________________________________________________________________________________________________________________________
;
TRUNCATE:
        SAVE_EDITSTATE

        LDY     #$00            ;
        LDA     #13             ;
        STA     (EDTPOS),Y      ; PUT CHAR IN EDIT BUFFER
        JSR     TOCONSOLE       ; PLACE CHAR ON SCREEN

TRUNCATE_LOOP:
        INC_EDTPOS
        LDA     #$00            ;
        TAY                     ;
        STA     (EDTPOS),Y      ; PUT CHAR IN EDIT BUFFER

        LDA     EDTPOS+1        ; COMPARE HIGH BYTES
        CMP     CURRENTLEN+1    ;
        BCC     TRUNCATE_LOOP   ; CONTINUE IF EDTPOS+1 < CURRENTLEN+1
        BNE     TRUNCATE_DONE   ; EXIT IF EDTPOS+1 > CURRENTLEN+1
        LDA     EDTPOS          ; HIGH BYTES EQUAL, CHECK LOW BYTES
        CMP     CURRENTLEN      ;
        BCC     TRUNCATE_LOOP   ; CONTINUE IF EDTPOS < CURRENTLEN
TRUNCATE_DONE:

        PLA                     ;
        STA     EDTPOS+1        ;
        STA     CURRENTLEN+1    ;
        PLA                     ;
        STA     EDTPOS          ;
        STA     CURRENTLEN      ;
        PLA                     ;
        STA     CSRPOSY         ;
        PLA                     ;
        STA     CSRPOSX         ;

        JSR     CHKPOS          ;
        JMP     PAGEUP          ;


;__INSERTLINE____________________________________________________________________________________________________________________
;
; 	INSERT ONE LINE AT THE CURRENT POSITION
;
;________________________________________________________________________________________________________________________________
;
INSERTLINE:
        JSR     EDITCHECK       ; VERIFY WE ARE NOT PAST EOF
        LDA     CURRENTLEN      ;
        STA     TMPPOS          ;
        LDA     CURRENTLEN+1    ;
        STA     TMPPOS+1        ; TMPPOS SHOULD BE BOTTOM OF COPY

; CHECK IF WE HAVE ROOM FOR 80 MORE BYTES BEFORE MODIFYING CURRENTLEN
; Calculate CURRENTLEN + 80 to check if result < Ram_top
        LDA     CURRENTLEN      ;
        CLC                     ;
        ADC     #MAXCOL         ; Add 80 to low byte
        STA     TEMP1           ; Save result low byte temporarily
        LDA     CURRENTLEN+1    ;
        ADC     #$00            ; Add carry to high byte
        STA     TEMP            ; Save result high byte temporarily
; Now TEMP:TEMP1 contains CURRENTLEN + 80
        CMP     #>Ram_top       ; Compare high byte with $B8
        BCC     INSERTLINE_OK   ; If less than, OK to proceed
        BNE     INSERTLINE_ABORT; If greater than, abort (not enough room)
        LDA     TEMP1           ; High bytes equal, check low byte
        CMP     #<Ram_top       ;
        BCS     INSERTLINE_ABORT; If >= Ram_top, abort (not enough room)
INSERTLINE_OK:
; Safe to update CURRENTLEN with pre-calculated result
        LDA     TEMP1           ;
        STA     CURRENTLEN      ;
        LDA     TEMP            ;
        STA     CURRENTLEN+1    ;
;
INSERTLINE1:
        LDY     #$00            ;
        LDA     (TMPPOS),Y      ;
        LDY     #MAXCOL         ;
        STA     (TMPPOS),Y      ;
        DEC     TMPPOS          ;
        LDA     TMPPOS          ;
        CMP     #$FF            ;
        BNE     INSERTLINE1A    ;
        DEC     TMPPOS+1        ;
INSERTLINE1A:
        LDA     TMPPOS+1        ; COMPARE HIGH BYTES
        CMP     EDTPOS+1        ;
        BCC     INSERTLINE1B    ; EXIT IF TMPPOS+1 < EDTPOS+1 (SAFETY CHECK)
        BNE     INSERTLINE1     ; CONTINUE IF TMPPOS+1 > EDTPOS+1
        LDA     TMPPOS          ; HIGH BYTES EQUAL, CHECK LOW BYTES
        CMP     EDTPOS          ;
        BCC     INSERTLINE1B    ; EXIT IF TMPPOS < EDTPOS (SAFETY CHECK)
        BNE     INSERTLINE1     ; CONTINUE IF TMPPOS > EDTPOS
INSERTLINE1B:                   ; EXIT WHEN TMPPOS <= EDTPOS
; TMPPOS IS NOW EDTPOS-1. SHIFT THE BYTE AT EDTPOS THAT WAS MISSED.
        INC     TMPPOS          ;
        BNE     INSERTLINE1C    ;
        INC     TMPPOS+1        ;
INSERTLINE1C:                   ; TMPPOS == EDTPOS NOW
        LDY     #$00            ;
        LDA     (TMPPOS),Y      ; READ BYTE AT EDTPOS
        LDY     #MAXCOL         ;
        STA     (TMPPOS),Y      ; SHIFT IT TO EDTPOS+80
; NULL-FILL FROM EDTPOS FOR MAXCOL BYTES
        LDX     #MAXCOL         ;
        LDY     #$00            ;
        LDA     #$00            ;
INSERTLINE2:
        STA     (TMPPOS),Y      ;
        INY                     ;
        DEX                     ;
        BNE     INSERTLINE2     ;

        SAVE_EDITSTATE
        LDY     #$00            ;

INSERTLINE3:
        LDA     (EDTPOS),Y      ;
        JSR     TOCONSOLE       ;
        INC_EDTPOS              ;
        LDA     CSRPOSY         ; HAVE WE REACHED THE END OF THE SCREEN?
        CMP     #LASTROW        ;
        BNE     INSERTLINE3     ; NO
        RESTORE_EDITSTATE
        JMP     GOCSR           ;

INSERTLINE_ABORT:
        RTS



;__DELETELINE____________________________________________________________________________________________________________________
;
; 	DELETE ONE LINE AT THE CURRENT POSITION
;
;________________________________________________________________________________________________________________________________
;
DELETELINE:
; CHECK THAT CURRENTLEN - MAXCOL WON'T UNDERFLOW PAST BUFFER
        LDA     CURRENTLEN      ;
        SEC                     ;
        SBC     #MAXCOL         ;
        STA     TEMP1           ; SAVE CANDIDATE LOW BYTE
        LDA     CURRENTLEN+1    ;
        SBC     #$00            ;
        CMP     #>BUFFER        ; COMPARE HIGH BYTE WITH BUFFER START
        BCC     DELETELINE_ABORT; WOULD GO BELOW BUFFER, ABORT
        BNE     DELETELINE_OK   ; ABOVE BUFFER PAGE, SAFE
        LDA     TEMP1           ; HIGH BYTES EQUAL, CHECK LOW BYTE
        CMP     #<BUFFER        ;
        BCC     DELETELINE_ABORT; WOULD GO BELOW BUFFER, ABORT
DELETELINE_OK:
        LDA     EDTPOS          ;
        STA     TMPPOS          ;
        LDA     EDTPOS+1        ;
        STA     TMPPOS+1        ; TMPPOS SHOULD BE TOP OF COPY

        LDA     CURRENTLEN      ; SET CURRENTLEN TO NEW TOP
        SEC                     ; (-80)
        SBC     #MAXCOL         ;
        STA     CURRENTLEN      ;
        LDA     CURRENTLEN+1    ;
        SBC     #$00            ;
        STA     CURRENTLEN+1    ;
;
DELETELINE1:
        LDY     #MAXCOL         ;
        LDA     (TMPPOS),Y      ;
        LDY     #$00            ;
        STA     (TMPPOS),Y      ;
        INC     TMPPOS          ;
        LDA     TMPPOS          ;
        BNE     DELETELINE1A    ;
        INC     TMPPOS+1        ;
DELETELINE1A:
        LDA     TMPPOS+1        ; COMPARE HIGH BYTES
        CMP     CURRENTLEN+1    ;
        BCC     DELETELINE1     ; CONTINUE IF TMPPOS+1 < CURRENTLEN+1
        BNE     DELETELINE1B    ; EXIT IF TMPPOS+1 > CURRENTLEN+1 (SAFETY CHECK)
        LDA     TMPPOS          ; HIGH BYTES EQUAL, CHECK LOW BYTES
        CMP     CURRENTLEN      ;
        BCC     DELETELINE1     ; CONTINUE IF TMPPOS < CURRENTLEN
DELETELINE1B:                   ; EXIT WHEN TMPPOS >= CURRENTLEN

        SAVE_EDITSTATE
        LDA     #$00            ;
        STA     CSRPOSX         ;
        JSR     GOCSR           ;
        LDY     #$00            ;
DELETELINE3:
        LDA     (EDTPOS),Y      ;
        JSR     TOCONSOLE       ;
        INC_EDTPOS              ;
        LDA     CSRPOSY         ; HAVE WE REACHED THE END OF THE SCREEN?
        CMP     #LASTROW        ;
        BNE     DELETELINE3     ; NO
        RESTORE_EDITSTATE
        JMP     GOCSR           ;

DELETELINE_ABORT:
        RTS

;__INSERTCHAR____________________________________________________________________________________________________________________
;
; 	INSERT ONE CHAR AT THE CURRENT POSITION
;
;________________________________________________________________________________________________________________________________
;
INSERTCHAR:
        LDA     EDTPOS          ; STORE CURRENT POSITION
        STA     TMPPOS          ;
        LDA     EDTPOS+1        ;
        STA     TMPPOS+1        ;

; FIND END OF LINE
        LDY     #$00            ;
INSERTCHAR1:
; FIX: Check full 16-bit boundary, not just high byte
        LDA     TMPPOS+1        ; CHECK BOUNDARY BEFORE READING
        CMP     #>Ram_top       ; are we at the end of RAM?
        BCC     INSERTCHAR1A    ; Less than high byte, safe
        BNE     :+              ; Greater than high byte, abort
        LDA     TMPPOS          ; High bytes equal, check low byte
        CMP     #<Ram_top       ;
        BCS     :+              ; At or past Ram_top, abort
        JMP     INSERTCHAR1A
:
        JMP     INSERTCHAR_ABORT
INSERTCHAR1A:
        LDA     (TMPPOS),Y      ; READ BYTE

        INC     TMPPOS          ; INC POINTER
        BNE     INSERTCHAR1B    ;
        INC     TMPPOS+1        ;
INSERTCHAR1B:
        CMP     #13             ; AT END?
        BEQ     INSERTCHAR2     ; FOUND END
        JMP     INSERTCHAR1     ;

INSERTCHAR2:
        LDA     (TMPPOS),Y      ; FREE SPACE AT END OF LINE?
        BEQ     INSERTCHAR3     ; YES, PROCEED WITH INSERT
; NO FREE SPACE - CHECK IF WE'RE AT EOF (TMPPOS >= CURRENTLEN)
        LDA     TMPPOS+1        ;
        CMP     CURRENTLEN+1    ;
        BCC     INSERTCHAR2A    ; TMPPOS+1 < CURRENTLEN+1, NOT AT EOF
        BNE     INSERTCHAR_ABORT; TMPPOS+1 > CURRENTLEN+1, PAST EOF - ABORT
        LDA     TMPPOS          ; HIGH BYTES EQUAL, CHECK LOW BYTES
        CMP     CURRENTLEN      ;
        BCS     INSERTCHAR_ABORT; TMPPOS >= CURRENTLEN, AT OR PAST EOF - ABORT
INSERTCHAR2A:
        JSR     INSERTCHAR5     ; NOT AT EOF, INSERT A LINE

INSERTCHAR3:
        DEC     TMPPOS          ; DEC TMPPOS
        LDA     TMPPOS          ;
        CMP     #$FF            ;
        BNE     INSERTCHAR3A    ;
        DEC     TMPPOS+1        ;
INSERTCHAR3A:
        LDY     #$00            ; MOVE EVERYTHING UP ONE POS
        LDA     (TMPPOS),Y      ;
        INY                     ;
        STA     (TMPPOS),Y      ;
        LDA     TMPPOS+1        ; AT CURRENT POSITION?
        CMP     EDTPOS+1        ;
        BNE     INSERTCHAR3     ; NO, LOOP
        LDA     TMPPOS          ;
        CMP     EDTPOS          ;
        BNE     INSERTCHAR3     ; NO, LOOP

        LDY     #$00            ; CLEAR VACATED POSITION SO EDIT_OM
        LDA     #$00            ; DOES NOT SEE STALE CR FROM PRE-SHIFT
        STA     (EDTPOS),Y      ;

        SAVE_EDITSTATE
        LDY     #$00            ;
        LDX     #MAXCOL         ; SAFETY: LIMIT REDRAW TO ONE LINE (80 CHARS)
INSERTCHAR4:
        LDA     (EDTPOS),Y      ;
        PHA                     ;
        JSR     TOCONSOLE       ;
        INC_EDTPOS              ;
        PLA                     ;
        CMP     #13             ; HAVE WE REACHED THE END OF THE ROW?
        BEQ     INSERTCHAR4_DONE; YES, EXIT
        DEX                     ; DECREMENT SAFETY COUNTER
        BNE     INSERTCHAR4     ; CONTINUE IF NOT EXHAUSTED
INSERTCHAR4_DONE:
        RESTORE_EDITSTATE
        JMP     GOCSR           ;
INSERTCHAR_ABORT:
        RTS                     ;
INSERTCHAR5:
        LDA     CSRPOSY         ; HAVE WE REACHED THE END OF THE SCREEN?
        CMP     #LASTROW-1      ;
        BNE     INSERTCHAR5A    ; NO
        JSR     SCROLLDOWN      ; SCROLL
INSERTCHAR5A:
        LDA     EDTPOS          ;
        PHA                     ;
        LDA     EDTPOS+1        ;
        PHA                     ;
        LDA     CSRPOSX         ;
        PHA                     ;
        LDA     CSRPOSY         ;
        PHA                     ;
        LDA     #$00            ;
        STA     CSRPOSX         ;
        INC     CSRPOSY         ;
        JSR     GOCSR           ;

        LDA     TMPPOS          ;
        STA     EDTPOS          ;
        LDA     TMPPOS+1        ;
        STA     EDTPOS+1        ;

        JSR     INSERTLINE      ;
        PLA                     ;
        STA     CSRPOSY         ;
        PLA                     ;
        STA     CSRPOSX         ;
        PLA                     ;
        STA     EDTPOS+1        ;
        PLA                     ;
        STA     EDTPOS          ;
        JMP     GOCSR           ;




;__BACKSPACE_____________________________________________________________________________________________________________________
;
; 	PERFORM BACKSPACE FUNCTION AT THE CURRENT POSITION
;
;________________________________________________________________________________________________________________________________
;
BACKSPACE:
        LDA     EDTPOS          ; CHECK IF AT BUFFER START
        BNE     BACKSPACE1      ;
        LDA     EDTPOS+1        ;
        CMP     #>BUFFER        ;
        BNE     BACKSPACE1      ;
        RTS                     ; AT BUFFER START, DO NOTHING
BACKSPACE1:
        JSR     CRSRLEFT
        JMP     DELETECHAR


;__DELETECHAR____________________________________________________________________________________________________________________
;
; 	DELETE ONE CHAR AT THE CURRENT POSITION
;
;________________________________________________________________________________________________________________________________
;
DELETECHAR:
; CHECK IF WE'RE AT OR PAST EOF (NOTHING TO DELETE)
        LDA     EDTPOS+1        ;
        CMP     CURRENTLEN+1    ;
        BCC     DELETECHAR0A    ; EDTPOS+1 < CURRENTLEN+1, OK
        BEQ     DELETECHAR0B    ; HIGH BYTES EQUAL, CHECK LOW BYTE
        JMP     DELETECHAR_ABORT; EDTPOS+1 > CURRENTLEN+1, PAST EOF
DELETECHAR0B:
        LDA     EDTPOS          ;
        CMP     CURRENTLEN      ;
        BCC     DELETECHAR0A    ; EDTPOS < CURRENTLEN, OK
        JMP     DELETECHAR_ABORT; EDTPOS >= CURRENTLEN, AT/PAST EOF

DELETECHAR0A:
; Save the character we're about to delete to determine repaint strategy
        LDY     #$00            ;
        LDA     (EDTPOS),Y      ; Read character at cursor
        STA     TEMP            ; Save it for later

; SPECIAL CASE: CR AT COLUMN 0 = EMPTY LINE
; BYTE-SHIFT WOULD CORRUPT THE 80-COLUMN LINE STRUCTURE
; USE DELETELINE TO PROPERLY REMOVE THE ENTIRE 80-BYTE LINE
        CMP     #13             ; DELETING A CR?
        BNE     DELETECHAR_SHIFTA; NO, NORMAL DELETE

        LDA     CSRPOSX         ; AT COLUMN 0? (EMPTY LINE)
        BNE     DELETECHAR_MERGE; NO, MERGE WITH NEXT LINE
        JMP     DELETELINE      ; YES, REMOVE ENTIRE EMPTY LINE
DELETECHAR_SHIFTA:
        JMP     DELETECHAR_SHIFT
;
; DELETECHAR_MERGE - MERGE CURRENT LINE WITH NEXT LINE WHEN DELETING CR
;
; WHEN THE USER DELETES A CR AT COLUMN > 0, WE MUST:
;   1. COPY NEXT LINE'S CONTENT TO CURRENT LINE (REPLACING CR + NULLS)
;   2. WRITE NEW CR AFTER MERGED CONTENT
;   3. NULL-FILL REMAINDER OF CURRENT LINE
;   4. DELETE THE NEXT LINE (SHIFT BUFFER UP BY MAXCOL, REDUCE CURRENTLEN)
;   5. FULL SCREEN REPAINT
;
DELETECHAR_MERGE:
; CALCULATE NEXT LINE START: TMPPOS = EDTPOS + (MAXCOL - CSRPOSX)
        LDA     #MAXCOL         ;
        SEC                     ;
        SBC     CSRPOSX         ; A = DISTANCE TO NEXT LINE START
        CLC                     ;
        ADC     EDTPOS          ;
        STA     TMPPOS          ;
        LDA     EDTPOS+1        ;
        ADC     #$00            ;
        STA     TMPPOS+1        ; TMPPOS = START OF NEXT LINE

; CHECK IF NEXT LINE EXISTS (TMPPOS < CURRENTLEN)
        LDA     TMPPOS+1        ;
        CMP     CURRENTLEN+1    ;
        BCC     DELETECHAR_MERGE_OK; TMPPOS < CURRENTLEN
        BNE     DELETECHAR_MERGE_ABORT; TMPPOS > CURRENTLEN, NO NEXT LINE
        LDA     TMPPOS          ;
        CMP     CURRENTLEN      ;
        BCC     DELETECHAR_MERGE_OK; TMPPOS < CURRENTLEN
DELETECHAR_MERGE_ABORT:
        JMP     DELETECHAR_ABORT; NO NEXT LINE, NOTHING TO MERGE

DELETECHAR_MERGE_OK:
; COUNT CONTENT ON NEXT LINE (SCAN FOR CR)
        LDY     #$00            ;
DELETECHAR_MERGE_COUNT:
        LDA     (TMPPOS),Y      ;
        CMP     #CR             ; FOUND CR?
        BEQ     DELETECHAR_MERGE_COUNTED
        INY                     ;
        CPY     #MAXCOL         ; SAFETY: DON'T SCAN PAST LINE
        BNE     DELETECHAR_MERGE_COUNT
DELETECHAR_MERGE_COUNTED:
; Y = NUMBER OF CONTENT BYTES ON NEXT LINE (BEFORE CR)
        STY     TEMP1           ; SAVE NEXT LINE CONTENT LENGTH

; CHECK IF MERGE FITS: CSRPOSX + NEXT_CONTENT < MAXCOL
; (NEED ROOM FOR AT LEAST THE CR)
        TYA                     ;
        CLC                     ;
        ADC     CSRPOSX         ;
        CMP     #MAXCOL         ; CSRPOSX + NEXT_CONTENT >= MAXCOL?
        BCS     DELETECHAR_MERGE_ABORT; YES, CAN'T MERGE - TOO LONG

; COPY NEXT LINE CONTENT TO CURRENT LINE AT CURSOR POSITION
        LDY     #$00            ;
        LDX     TEMP1           ; COUNTER = NEXT LINE CONTENT LENGTH
        BEQ     DELETECHAR_MERGE_NOCOPY; NOTHING TO COPY (NEXT LINE EMPTY)
DELETECHAR_MERGE_COPY:
        LDA     (TMPPOS),Y      ; READ FROM NEXT LINE
        STA     (EDTPOS),Y      ; WRITE TO CURRENT LINE AT CR POSITION
        INY                     ;
        DEX                     ;
        BNE     DELETECHAR_MERGE_COPY

DELETECHAR_MERGE_NOCOPY:
; WRITE CR AFTER MERGED CONTENT
        LDA     #CR             ;
        STA     (EDTPOS),Y      ; CR AT EDTPOS + NEXT_CONTENT
        INY                     ;

; NULL-FILL REST OF CURRENT LINE TO NEXT LINE BOUNDARY
; FILL FROM CURRENT Y TO (MAXCOL - CSRPOSX)
        LDA     #MAXCOL         ;
        SEC                     ;
        SBC     CSRPOSX         ; A = DISTANCE FROM EDTPOS TO NEXT LINE
        STA     TEMP1           ; REUSE TEMP1 AS FILL LIMIT
        LDA     #$00            ;
DELETECHAR_MERGE_FILL:
        CPY     TEMP1           ; REACHED NEXT LINE BOUNDARY?
        BCS     DELETECHAR_MERGE_DEL; YES, DONE FILLING
        STA     (EDTPOS),Y      ; WRITE NULL
        INY                     ;
        BNE     DELETECHAR_MERGE_FILL; ALWAYS TAKEN (Y < 80)

DELETECHAR_MERGE_DEL:
; DELETE THE NEXT LINE BY SHIFTING BUFFER UP BY MAXCOL
; TMPPOS ALREADY POINTS TO NEXT LINE START
; REDUCE CURRENTLEN BY MAXCOL
        LDA     CURRENTLEN      ;
        SEC                     ;
        SBC     #MAXCOL         ;
        STA     CURRENTLEN      ;
        LDA     CURRENTLEN+1    ;
        SBC     #$00            ;
        STA     CURRENTLEN+1    ;

; SHIFT LOOP: COPY FROM TMPPOS+MAXCOL TO TMPPOS
DELETECHAR_MERGE_SHIFT:
        LDA     TMPPOS+1        ; CHECK IF DONE
        CMP     CURRENTLEN+1    ;
        BCC     DELETECHAR_MERGE_SHIFT1; TMPPOS < CURRENTLEN, CONTINUE
        BNE     DELETECHAR_MERGE_DONE; TMPPOS > CURRENTLEN, DONE
        LDA     TMPPOS          ;
        CMP     CURRENTLEN      ;
        BCS     DELETECHAR_MERGE_DONE; TMPPOS >= CURRENTLEN, DONE
DELETECHAR_MERGE_SHIFT1:
        LDY     #MAXCOL         ;
        LDA     (TMPPOS),Y      ; READ FROM TMPPOS + MAXCOL
        LDY     #$00            ;
        STA     (TMPPOS),Y      ; WRITE TO TMPPOS
        INC     TMPPOS          ;
        BNE     DELETECHAR_MERGE_SHIFT
        INC     TMPPOS+1        ;
        JMP     DELETECHAR_MERGE_SHIFT

DELETECHAR_MERGE_DONE:
        JMP     DELETECHAR_FULLREPAINT

DELETECHAR_SHIFT:
        LDA     EDTPOS          ; STORE CURRENT POSITION
        STA     TMPPOS          ;
        LDA     EDTPOS+1        ;
        STA     TMPPOS+1        ;

; MOVE CHARS BACK ONE SPACE UNTIL CR OR EOF
DELETECHAR1:
; First, check if next byte (TMPPOS+1) is at or past EOF
; Calculate TMPPOS+1 for comparison
        LDA     TMPPOS          ;
        CLC                     ;
        ADC     #$01            ;
        STA     TEMP1           ; TEMP1 = low byte of TMPPOS+1
        LDA     TMPPOS+1        ;
        ADC     #$00            ;
; A now has high byte of TMPPOS+1
        CMP     CURRENTLEN+1    ;
        BCC     DELETECHAR1A    ; TMPPOS+1 < CURRENTLEN, OK to read
        BEQ     DELETECHAR1AB   ; HIGH BYTES EQUAL, CHECK LOW BYTE
        JMP     DELETECHAR1_EOF ; TMPPOS+1 > CURRENTLEN, past EOF
DELETECHAR1AB:
        LDA     TEMP1           ;
        CMP     CURRENTLEN      ;
        BCC     DELETECHAR1A    ; TMPPOS+1 < CURRENTLEN, OK to read
        JMP     DELETECHAR1_EOF ; TMPPOS+1 >= CURRENTLEN, at/past EOF

DELETECHAR1A:
; Safe to read from TMPPOS+1
        LDY     #$01            ;
        LDA     (TMPPOS),Y      ; Read byte from next position
        CMP     #13             ; CHECK FOR CR
        BEQ     DELETECHAR1C    ; FOUND CR, HANDLE IT
        CMP     #$00            ; CHECK FOR NULL
        BNE     DELETECHAR1B    ;
        LDA     #32             ; REPLACE NULLS WITH SPACES

DELETECHAR1B:
        LDY     #$00            ;
        STA     (TMPPOS),Y      ; Store at current position
        INC     TMPPOS          ; INC POINTER
        BNE     DELETECHAR1     ; NO OVERFLOW, CONTINUE
        INC     TMPPOS+1        ; OVERFLOW, INC HIGH BYTE
        JMP     DELETECHAR1     ; CONTINUE LOOP

DELETECHAR1C:                   ; FOUND CR
        LDY     #$00            ;
        STA     (TMPPOS),Y      ; Store CR at current position
        INC     TMPPOS          ; INC POINTER TO POINT PAST CR
        BNE     DELETECHAR1D    ; NO OVERFLOW, CONTINUE
        INC     TMPPOS+1        ; OVERFLOW, INC HIGH BYTE
DELETECHAR1D:
        JMP     DELETECHAR2     ; EXIT TO CLEANUP

DELETECHAR1_EOF:                ; REACHED EOF WITHOUT FINDING CR
; Just null out current position and exit
        LDA     #$00            ;
        LDY     #$00            ;
        STA     (TMPPOS),Y      ;
        JMP     DELETECHAR2     ; EXIT TO CLEANUP




DELETECHAR2:
        LDA     #$00
        STA     (TMPPOS),Y      ;

; CHECK IF WE DELETED A CR (line merge) OR NULL (blank line) - REQUIRES FULL REPAINT
        LDA     TEMP            ; Get the character we deleted
        CMP     #13             ; Was it a CR?
        BEQ     DELETECHAR_FULLREPAINT; Yes, need full repaint
        CMP     #$00            ; Was it a null (blank line)?
        BEQ     DELETECHAR_FULLREPAINT; Yes, need full repaint
        JMP     DELETECHAR_FAST ; Regular character, use fast update

; REPAINT FROM CURRENT ROW TO BOTTOM (for deleting CR/null that merges lines)
DELETECHAR_FULLREPAINT:
        SAVE_EDITSTATE

; BACK UP EDTPOS TO COLUMN 0 OF CURRENT ROW
        LDA     CSRPOSX
        TAX
        BEQ     DELETECHAR_FP1
DELETECHAR_FP0:
        DEC_EDTPOS
        DEX
        BNE     DELETECHAR_FP0
DELETECHAR_FP1:
        LDA     #$00
        STA     CSRPOSX
        JSR     GOCSR           ; POSITION CURSOR TO COL 0 OF CURRENT ROW
        LDY     #$00
DELETECHAR_FP2:
        LDA     (EDTPOS),Y
        JSR     TOCONSOLE
        INC_EDTPOS
        LDA     CSRPOSY         ; HAVE WE REACHED THE END OF THE SCREEN?
        CMP     #LASTROW
        BNE     DELETECHAR_FP2

        RESTORE_EDITSTATE
        JMP     GOCSR           ;

; FAST DISPLAY UPDATE: USE ANSI DELETE-CHAR (for mid-line deletes)
DELETECHAR_FAST:
        LDA     CSRPOSX         ; remember starting column to avoid wrapping on the short redraw
        STA     DELETECHARTMP

        SAVE_EDITSTATE

; delete char at cursor (terminal shifts remainder left)
        LDA     #<DELCHARSEQ
        LDY     #>DELCHARSEQ
        LDX     #9
        JSR     PEM

; redraw rest of line (stop at CR or line end)
        LDA     #MAXCOL-1
        SEC
        SBC     DELETECHARTMP   ; remaining cols before wrap
        STA     TEMP1
        LDY     #$00
        LDX     #MAXCOL         ; limit redraw to full line width
DELETECHAR4:
        LDA     (EDTPOS),Y      ; Load character
        CMP     #13
        BNE     DELETECHAR4_NOTCR
        JSR     TOCONSOLE       ; RENDER THE CR MARKER BEFORE EXITING
        JMP     DELETECHAR4B    ; THEN CLEAN UP REST OF LINE
DELETECHAR4_NOTCR:
        PHA                     ; Push character to preserve it
        TYA                     ; Check if Y >= TEMP1
        CMP     TEMP1
        BCS     DELETECHAR4A    ; Exit if would wrap
        PLA                     ; Pop character back
        JSR     TOCONSOLE       ; Output it
        INY
        DEX
        BNE     DELETECHAR4
        BEQ     DELETECHAR4B    ; Exit when counter exhausted
DELETECHAR4A:                   ; Exit path that cleans stack
        PLA                     ; Pop unused character
DELETECHAR4B:

; clean the rest of the line to avoid artifacts
        LDA     #<ERASEEOLSEQ
        LDY     #>ERASEEOLSEQ
        LDX     #9
        JSR     PEM

; restore cursor/buffer state
        RESTORE_EDITSTATE
        JMP     GOCSR           ;
DELETECHAR_ABORT:
        RTS                     ;

DELETECHARTMP:
        .BYTE   0


;__TOGGLEMODE_____________________________________________________________________________________________________________________
;
; 	TOGGLE INSERT/OVERWRITE MODE
;
;________________________________________________________________________________________________________________________________
;
TOGGLEMODE:

        LDA     MODE            ;
        BNE     TOGGLEMODEOW    ;

TOGGLEMODEIM:
        LDA     #$01            ;
        STA     MODE            ;
        LDA     #<IMMODE        ;
        LDY     #>IMMODE        ;
        LDX     #9              ;
        JSR     PEM             ;
        JMP     GOCSR           ;
IMMODE:
        .BYTE   $1B,'[','2','3',';','6','2','H'
        .BYTE   $1B,'[','7','m'
        .BYTE   "IM"
        .BYTE   $1B,'[','0','m','$'
TOGGLEMODEOW:
        LDA     #$00            ;
        STA     MODE            ;
        LDA     #<OWMODE        ;
        LDY     #>OWMODE        ;
        LDX     #9              ;
        JSR     PEM             ;
        JMP     GOCSR           ;

OWMODE:
        .BYTE   $1B,'[','2','3',';','6','2','H'
        .BYTE   $1B,'[','7','m'
        .BYTE   "OM"
        .BYTE   $1B,'[','0','m','$'



;__CRSRUP________________________________________________________________________________________________________________________
;
; 	MOVE THE CURSOR UP ONE POSITION, DEC THE BUFFER POINTER AND SCROLL UP IF NECESSARY
;
;________________________________________________________________________________________________________________________________
;
CRSRUP:
        LDA     EDTPOS          ;
        BNE     CRSRUP1         ; CHECK TO SEE IF POINTER IS AT THE VERY TOP OF THE
        LDA     EDTPOS+1        ; BUFFER.  IF IT IS, DO NOTHING.
        CMP     #>BUFFER        ;
        BNE     CRSRUP1         ;
        RTS                     ;
CRSRUP1:
        LDX     #$00            ;
        LDA     CSRPOSY         ; FIRST LINE?
        BNE     CRSRUP2         ;
        JMP     SCROLLUP        ; YES, SCROLL UP

CRSRUP2:
        DEC_EDTPOS              ; MOVE POINTER BACK ONE
        INX                     ; INC COUNTER
        CPX     #MAXCOL         ; HAVE WE GONE BACK MAXCOL POSITIONS?
        BNE     CRSRUP2         ; NO, LOOP

        DEC     CSRPOSY         ;
        JMP     CHKPOS          ; WE ARE DONE! POSITION THE CURSOR AND EXIT


;__CRSRDN________________________________________________________________________________________________________________________
;
; 	MOVE THE CURSOR DOWN ONE POSITION, INC THE BUFFER POINTER AND SCROLL IF NECESSARY
;
;________________________________________________________________________________________________________________________________
;
CRSRDN:

        LDA     CSRPOSY         ; HAVE WE REACHED THE END OF THE SCREEN?
        CMP     #LASTROW-1      ;
        BNE     CRSRDN1         ; NO, JUST MOVE DOWN
        JSR     SCROLLDOWN      ; SCROLL

CRSRDN1:
        LDX     #$00
CRSRDN2:
        INC_EDTPOS              ; MOVE EDTPOS 1 TO THE RIGHT
        INX
        CPX     #MAXCOL
        BEQ     CRSRDN3         ; YES, DONE
        LDA     EDTPOS+1        ; WILL NEXT MOVE SEND US PAST THE END OF RAM?
        CMP     #>Ram_top       ; are we at the end of RAM?
        BCC     CRSRDN2A        ; Less than high byte, OK to continue
        BNE     CRSRDN4         ; Greater than high byte, stop
        LDA     EDTPOS          ; High bytes equal, check low byte
        CMP     #<Ram_top       ;
        BCS     CRSRDN4         ; At or past Ram_top, stop

CRSRDN2A:
        JMP     CRSRDN2         ; NO, KEEP GOING
CRSRDN3:
        INC     CSRPOSY         ;
        JMP     CHKPOS          ; WE ARE DONE! POSITION THE CURSOR AND EXIT
CRSRDN4:
        JMP     PAINTSCREEN0    ;



;__CRSRLEFT______________________________________________________________________________________________________________________
;
; 	MOVE THE CURSOR LEFT ONE POSITION, DEC THE BUFFER POINTER AND SCROLL UP IF NECESSARY
;
;________________________________________________________________________________________________________________________________
;
CRSRLEFT:
        LDA     EDTPOS          ;
        BNE     CRSRLEFT1       ; CHECK TO SEE IF POINTER IS AT THE VERY TOP OF THE
        LDA     EDTPOS+1        ; BUFFER.  IF IT IS, DO NOTHING.
        CMP     #>BUFFER        ;
        BNE     CRSRLEFT1       ;
        RTS                     ;
CRSRLEFT1:
        DEC     CSRPOSX         ; DEC THE CURSOR POSITION
        LDA     CSRPOSX         ; CHECK TO SEE IF WE WERE AT THE LEFT MOST POSITION
        CMP     #$FF            ;
        BEQ     CRSRLEFT2       ; IF WE WERE, DO SOME SPECIAL STUFF
        DEC_EDTPOS              ; MOVE EDTPOS 1 TO THE LEFT
        JMP     CHKPOS          ; POSITION THE CURSOR
CRSRLEFT2:                      ; CURSOR IS AT THE LEFT MOST LINE
        LDA     #MAXCOL-1       ;
        STA     CSRPOSX         ;
        TAX                     ;
CRSRLEFT2A:
        INC_EDTPOS              ;
        DEX                     ;
        BNE     CRSRLEFT2A      ;
        JMP     CRSRUP          ; MOVE UP ONE LINE


;__CRSRRIGHT_____________________________________________________________________________________________________________________
;
; 	MOVE THE CURSOR RIGHT ONE POSITION, INC THE BUFFER POINTER AND SCROLL IF NECESSARY
;
;________________________________________________________________________________________________________________________________
;
CRSRRIGHT:
        LDA     CSRPOSY         ; IS CURSOR ON LAST POSITION OF SCREEN?
        CMP     #LASTROW-1      ; IF NOT, BRANCH TO CSRRIGHT1, OTHERWISE
        BNE     CRSRRIGHT1      ; SCROLL SCREEN
        LDA     CSRPOSX         ;
        CMP     #MAXCOL-1       ;
        BNE     CRSRRIGHT1      ;
;
        JSR     SCROLLDOWN      ;
;
CRSRRIGHT1:                     ;
        JSR     GOCSR           ; POSITION CURSOR
        LDY     #$00            ;
        LDA     (EDTPOS),Y      ; GET CHAR FROM EDIT BUFFER
        JSR     TOCONSOLE       ; PLACE CHAR ON SCREEN
;
        INC_EDTPOS              ; MOVE EDTPOS 1 TO THE RIGHT
;
        LDA     EDTPOS+1        ;
        CMP     #>Ram_top       ; are we at the end of RAM?
        BCC     CRSRRIGHT2      ; Less than high byte, OK to continue
        BNE     CRSRRIGHT_BACK  ; Greater than high byte, back off
        LDA     EDTPOS          ; High bytes equal, check low byte
        CMP     #<Ram_top       ;
        BCC     CRSRRIGHT2      ; Less than Ram_top, OK to continue
CRSRRIGHT_BACK:
        DEC_EDTPOS              ; OTHERWISE BACK POINTER OFF ONE
; AND PLACE CURSOR BACK ONE POSITION
        DEC     CSRPOSX         ;
        LDA     CSRPOSX         ;
        CMP     #$FF            ;
        BNE     CRSRRIGHT2      ;
        DEC     CSRPOSY         ;
        LDA     #MAXCOL-1       ; FIX: Valid range is 0-79, not 0-80
        STA     CSRPOSX         ; DDW 11/29/2014 (FROM Y)
CRSRRIGHT2:
        JMP     CHKPOS



;__SCROLLUP_____________________________________________________________________________________________________________________
;
; 	SCROLL UP ONE LINE
;
;
;
;
;________________________________________________________________________________________________________________________________
;
SCROLLUP:
        LDA     EDTPOS+1        ; CHECK HIGH BYTE, IF NOT EQUAL
        CMP     #>BUFFER        ; NO WAY WE ARE AT START OF BUFFER
        BNE     SCROLLUPSTART   ; SO SCROLL
        LDA     EDTPOS          ; CHECK LOW BYTE
        CMP     #MAXCOL+1       ; IF WE ARE NOT ON TOP LINE
        BCS     SCROLLUPSTART   ; SCROLL
        RTS                     ; IF NOT, DO NOTHING

SCROLLUPSTART:
        LDA     #<SCROLLUPT     ;
        LDY     #>SCROLLUPT
        LDX     #9              ;
        JSR     PEM             ;

        LDA     CSRPOSY
        PHA
        LDA     CSRPOSX
        PHA

        LDA     #MAXCOL
        CLC
        ADC     CSRPOSX
        TAX
SCROLLUP1:
        DEC_EDTPOS
        DEX
        BNE     SCROLLUP1

        LDX     #$00
        STX     CSRPOSX
        STX     CSRPOSY
        JSR     GOCSR

        LDA     EDTPOS
        PHA
        LDA     EDTPOS+1
        PHA

        DEC_EDTPOS
        LDY     #$00
        LDX     #MAXCOL
SCROLLUP2:
        INC_EDTPOS
        LDA     (EDTPOS),Y
        JSR     TOCONSOLE
        DEX
        BNE     SCROLLUP2

        PLA
        STA     EDTPOS+1
        PLA
        STA     EDTPOS

        PLA
        STA     CSRPOSX         ;
        TAX
        PLA
        STA     CSRPOSY         ;
        JSR     GOCSR           ;

        TXA                     ; ADD CSRPOSX TO EDTPOS IN ONE STEP
        CLC
        ADC     EDTPOS
        STA     EDTPOS
        BCC     SCROLLUP4
        INC     EDTPOS+1
SCROLLUP4:
        JMP     CHKPOS

SCROLLUPT:
        .BYTE   $1B,'[','3',';','2','2','r',$1B,'[','3',';','1','H',$1B,'M',$1B,'[','r','$'

;__SCROLLDOWN___________________________________________________________________________________________________________________
;
; 	SCROLL DOWN ONE LINE
;
;
;
;________________________________________________________________________________________________________________________________
;
SCROLLDOWN:
        LDA     #<SCROLLDOWNT
        LDY     #>SCROLLDOWNT
        LDX     #9              ;
        JSR     PEM             ;


        SAVE_EDITSTATE

        LDA     #MAXCOL
        SEC
        SBC     CSRPOSX
        TAX
        DEX
        BEQ     SCROLLDOWN1A
SCROLLDOWN1:
        INC_EDTPOS
        DEX
        BNE     SCROLLDOWN1
SCROLLDOWN1A:
        LDX     #$00
        STX     CSRPOSX
        JSR     GOCSR

        LDY     #$00
        LDX     #MAXCOL
SCROLLDOWN2:
        INC_EDTPOS
        LDA     (EDTPOS),Y
        JSR     TOCONSOLE
        DEX
        BNE     SCROLLDOWN2

        RESTORE_EDITSTATE
        DEC     CSRPOSY

        JMP     CHKPOS

SCROLLDOWNT:
        .BYTE   $1B,'[','3',';','2','2','r',$1B,'[','2','2',';','1','H',$1B,'D',$1B,'[','r','$'



;__CHKPOS________________________________________________________________________________________________________________________
;
; 	ENSURE THAT THE CURSOR IS ON A VALID EDITING POSITION
;	IF NOT, FIND ONE!
;
;________________________________________________________________________________________________________________________________
;
CHKPOS:
        LDY     #$00
        LDA     (EDTPOS),Y
        CMP     #$00
        BNE     CHKPOS2

        LDA     EDTPOS+1        ; CHECK HIGH BYTE, IF NOT EQUAL
        CMP     #>BUFFER        ; NO WAY WE ARE AT START OF BUFFER
        BNE     CHKPOS1         ; SO GO ON
        LDA     EDTPOS          ; CHECK LOW BYTE (BUFFER is page-aligned, low byte = $00)
        BNE     CHKPOS1         ; IF WE ARE NOT AT TOP, CONTINUE BACKING UP
        JMP     GOCSR           ; AT BUFFER START, POSITION CURSOR AND RETURN
CHKPOS1:
        DEC_EDTPOS              ; DEC THE EDIT POSITION
        DEC     CSRPOSX         ; DEC THE CURSOR POSITION
        LDA     CSRPOSX         ; ARE WE PAST THE START OF THE ROW?
        CMP     #$FF
        BEQ     CHKPOS1A        ; YES, GO UP ONE LINE
        JMP     CHKPOS          ; NO, LOOP

CHKPOS1A:
        LDA     #MAXCOL-1       ; LOAD X AT MAXCOL
        STA     CSRPOSX
        DEC     CSRPOSY
        LDA     CSRPOSY
        CMP     #$FF
        BNE     CHKPOS1B
        LDA     #$00
        STA     CSRPOSY
        JMP     SCROLLUP

CHKPOS1B:
        JMP     CHKPOS
CHKPOS2:
        JMP     GOCSR






;__PAGEDOWN_______________________________________________________________________________________________________________________
;
; 	PERFORM PAGE DOWN FUNCTION
;
;
;________________________________________________________________________________________________________________________________
;
PAGEDOWN:
        LDA     EDTPOS          ;
        BNE     PAGEDOWNA       ; CHECK TO SEE IF POINTER IS AT THE VERY TOP OF THE
        LDA     EDTPOS+1        ; BUFFER.
        CMP     #>BUFFER        ;
        BNE     PAGEDOWNA       ;
        JMP     PAGEDOWN1       ;
PAGEDOWNA:
        LDA     CSRPOSX
        BEQ     PAGEDOWN1
        DEC_EDTPOS
        DEC     CSRPOSX
        JMP     PAGEDOWN

PAGEDOWN1:
        LDX     #PAGESIZE
PAGEDOWN2:
        LDY     #$10
PAGEDOWN2A:
        INC_EDTPOS

        LDA     EDTPOS+1        ; WILL NEXT MOVE SEND US PAST THE END OF RAM?
        CMP     #>Ram_top       ; are we at the end of RAM?
        BCC     PAGEDOWN2B      ; Less than high byte, OK to continue
        BNE     PAGEDOWN2C      ; Greater than high byte, stop
        LDA     EDTPOS          ; High bytes equal, check low byte
        CMP     #<Ram_top       ;
        BCS     PAGEDOWN2C      ; At or past Ram_top, stop
PAGEDOWN2B:
        DEY
        BNE     PAGEDOWN2A
        DEX
        BNE     PAGEDOWN2
PAGEDOWN2C:
        LDA     EDTPOS
        PHA
        LDA     EDTPOS+1
        PHA
        JSR     PAINTSCREEN1
        PLA
        STA     EDTPOS+1
        PLA
        STA     EDTPOS
        LDA     #$00
        STA     CSRPOSX
        STA     CSRPOSY
        JMP     GOCSR





;__PAGEUP________________________________________________________________________________________________________________________
;
; 	PERFORM PAGE UP FUNCTION
;
;
;________________________________________________________________________________________________________________________________
;
PAGEUP:
        LDA     EDTPOS          ;
        BNE     PAGEUPA         ; CHECK TO SEE IF POINTER IS AT THE VERY TOP OF THE
        LDA     EDTPOS+1        ; BUFFER.
        CMP     #>BUFFER        ;
        BNE     PAGEUPA         ;
        RTS
PAGEUPA:
        LDA     CSRPOSX
        BEQ     PAGEUP1
        DEC_EDTPOS
        DEC     CSRPOSX
        JMP     PAGEUP

PAGEUP1:
        LDX     #PAGESIZE
PAGEUP2:
        LDY     #$10
PAGEUP2A:
        DEC_EDTPOS

        LDA     EDTPOS          ;
        BNE     PAGEUP2B        ; CHECK TO SEE IF POINTER IS AT THE VERY TOP OF THE
        LDA     EDTPOS+1        ; BUFFER.
        CMP     #>BUFFER        ;
        BNE     PAGEUP2B        ;
        JMP     PAGEUP2C        ;
PAGEUP2B:
        DEY
        BNE     PAGEUP2A
        DEX
        BNE     PAGEUP2
PAGEUP2C:
        LDA     EDTPOS
        PHA
        LDA     EDTPOS+1
        PHA
        JSR     PAINTSCREEN1
        PLA
        STA     EDTPOS+1
        PLA
        STA     EDTPOS
        LDA     #$00
        STA     CSRPOSX
        STA     CSRPOSY
        JMP     GOCSR


;__PAINTSCREEN___________________________________________________________________________________________________________________
;
; 	PAINT THE INITIAL EDIT SCREEN
;
;
;
;________________________________________________________________________________________________________________________________
;
PAINTSCREEN:
        JSR     CLEARSCREEN

PAINTSCREEN0:
        LDA     #<BUFFER        ; SETUP BUFFER
        STA     EDTPOS          ; STORE DEST BUFFER
        LDA     #>BUFFER        ;
        STA     EDTPOS+1        ; STORE DEST BUFFER

PAINTSCREEN1:
        LDA     #$00
        STA     CSRPOSX
        STA     CSRPOSY
        JSR     GOCSR


PAINTSCREEN_LOOP:
        LDY     #$00
        LDA     (EDTPOS),Y
        JSR     TOCONSOLE

        INC     EDTPOS
        BNE     PAINTSCREEN_LOOPA
        INC     EDTPOS+1

PAINTSCREEN_LOOPA:
        LDA     CSRPOSY
        CMP     #LASTROW
        BNE     PAINTSCREEN_LOOP

        LDA     #$00
        STA     CSRPOSX
        STA     CSRPOSY
        JSR     GOCSR

        LDY     #$00

PAINTSCREEN_LOOPB:
        LDA     DFLFCB+1,Y      ; SKIP DRIVE BYTE, COPY FCB[1..8] FILENAME
        STA     SCREENFORMAT1,Y
        INY
        CPY     #8
        BNE     PAINTSCREEN_LOOPB
        LDA     #'.'            ; Y=8
        STA     SCREENFORMAT1,Y ; SCREENFORMAT1[8] = '.'
        LDA     DFLFCB+9        ; EXTENSION CHAR 1
        STA     SCREENFORMAT1+9
        LDA     DFLFCB+10       ; EXTENSION CHAR 2
        STA     SCREENFORMAT1+10
        LDA     DFLFCB+11       ; EXTENSION CHAR 3
        STA     SCREENFORMAT1+11




        LDA     #<SCREENFORMAT
        LDY     #>SCREENFORMAT
        LDX     #9              ;
        JSR     PEM             ;
        LDA     #<SCREENFORMAT1
        LDY     #>SCREENFORMAT1
        LDX     #9              ;
        JMP     PEM             ;


SCREENFORMAT:
        .BYTE   $1B,'[','0','0',';','0','0','H'
        .BYTE   $1B,'[','7','m'
        .BYTE   "DOS/65 SCREEN EDITOR"
        .BYTE   "                    "
        .BYTE   "                    ",'$'
SCREENFORMAT1:
        .BYTE   "                     "
        .BYTE   "____________________"
        .BYTE   "____________________"
        .BYTE   "____________________"
        .BYTE   "____________________"
        .BYTE   $1B,'[','2','3',';','0','0','H'
        .BYTE   "  F3=QUIT  F4=MODE  F7=SAVE "
        .BYTE   " F8=SAVE AS  F12=TRUNC.     "
        .BYTE   "     OM                "
        .BYTE   $1B,'[','0','m','$'



;__CLEARSCREEN___________________________________________________________________________________________________________________
;
; 	CLEAR THE SCREEN
;
;
;
;________________________________________________________________________________________________________________________________
;
CLEARSCREEN:
        LDA     #<CLEARSCREENT
        LDY     #>CLEARSCREENT
        LDX     #9              ;
        JMP     PEM             ;
CLEARSCREENT:
        .BYTE   $1B,'[','2','J','$'


;__GOCSR_________________________________________________________________________________________________________________________
;
; 	PLACE THE PHYSICAL CURSOR ON TO CSRPOSY AND CSRPOSX
;
;
;
;________________________________________________________________________________________________________________________________
;
GOCSR:
        PHA
        TYA
        PHA
        TXA
        PHA
        LDY     CSRPOSY
        LDA     DECIMAL1+3,Y
        AND     #$0F
        CLC
        ADC     #'0'
        STA     GOCSRT+3
        LDA     DECIMAL1+3,Y
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #$0F
        CLC
        ADC     #'0'
        STA     GOCSRT+2

        LDY     CSRPOSX
        LDA     DECIMAL1+1,Y
        AND     #$0F
        CLC
        ADC     #'0'
        STA     GOCSRT+6
        LDA     DECIMAL1+1,Y
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #$0F
        CLC
        ADC     #'0'
        STA     GOCSRT+5
        LDA     #<GOCSRT
        LDY     #>GOCSRT
        LDX     #9              ;
        JSR     PEM             ;
        PLA
        TAX
        PLA
        TAY
        PLA
        RTS
GOCSRT:
        .BYTE   $1B,'[','0','0',';','0','0','H','$'

;__TOCONSOLE_____________________________________________________________________________________________________________________
;
; 	TRANSLATE BUFFER TO CONSOLE
;	A: CHAR TO OUTPUT
;
;
;________________________________________________________________________________________________________________________________
;
TOCONSOLE:
        STA     TEMP
        TXA
        PHA
        TYA
        PHA
        LDA     TEMP
        CMP     #$0D            ;  IS CHAR A <CR>?
        BNE     TOCONSOLE1      ;  NO, CONTINUE
        LDA     #<PRINTCR
        LDY     #>PRINTCR
        LDX     #9              ;
        JSR     PEM             ;
        JMP     TOCONSOLE1B     ;
TOCONSOLE1:
        CMP     #$00            ; IS NULL?
        BNE     TOCONSOLE1A     ;  NO, CONTINUE
        LDA     #32             ; CHANGE NULL TO SPACE
TOCONSOLE1A:
        LDX     #2              ;  OUTPUT THE CHAR TO THE CONSOLE
        JSR     PEM             ;
TOCONSOLE1B:
        INC     CSRPOSX         ;  MOVE THE CURSOR
        LDA     CSRPOSX         ;  ARE WE AT THE END OF THE LINE?
        CMP     #MAXCOL         ;
        BNE     TOCONSOLE2      ;  NO, EXIT

        LDA     #$00            ; MOVE CRSR TO START OF LINE
        STA     CSRPOSX         ;
        INC     CSRPOSY         ; ADVANCE TO NEXT ROW
        JSR     GOCSR           ; MOVE CRSR
TOCONSOLE2:
        PLA
        TAY
        PLA
        TAX
        RTS                     ; DONE

PRINTCR:
        .BYTE   $1B,'[','7','m','<',$1B,'[','0','m','$'

DELCHARSEQ:
        .BYTE   $1B,'[','P','$' ; ANSI delete char

ERASEEOLSEQ:
        .BYTE   $1B,'[','K','$' ; ANSI erase to end of line


;__READKB________________________________________________________________________________________________________________________
;
; 	READ THE KEYBOARD
;	A: DATA
;	Y: COMMAND
;		0 IS REGULAR CHAR
;		1 IS FUNCTION KEY
;			00=CRSR UP
;			01=CRSR DN
;			02=CRSR LEFT
;			03=CRSR RIGHT
;			04=PG DN
;			05=PG UP
;			08=BACKSPACE
;			$30=CR
;			$11-$24= FUNCTION KEYS
;		$FF IS NULL OP
;________________________________________________________________________________________________________________________________
;
READKB:
        JSR     READKB_XON

        LDA     #$00            ;
        LDY     #$FF            ;

        LDX     #6              ;
        JSR     PEM             ;

        CMP     #$1B            ;
        BEQ     READKB_ESC      ;

        CMP     #127            ;
        BEQ     READKB_DEL      ;

        CMP     #32             ;
        BCS     READKB_CHAR     ;

        CMP     #$0D            ;
        BEQ     READKB_CR       ;

        CMP     #$08            ;
        BEQ     READKB_BS       ;

        JMP     READKB_ERR      ;

READKB_CR:
        JSR     READKB_XOFF
        LDY     #01
        LDA     #$30
        RTS

READKB_CHAR:
        JSR     READKB_XOFF
        LDY     #00
        RTS

READKB_BS:
        JSR     READKB_XOFF
        LDY     #01
        LDA     #08
        RTS

READKB_DEL:
        JSR     READKB_XOFF
        LDY     #01
        LDA     #127
        RTS

READKB_ERR:
        JSR     READKB_XOFF
        LDY     #$FF
        LDA     #$00
        RTS

READKB_ESC:
        LDX     #6              ;
        JSR     PEM             ;

        CMP     #79             ; ANSI
        BNE     :+
        JMP     READKB_ANSIKEYS
:
        CMP     #'['            ; VT100
        BNE     READKB_ERR

        LDX     #6              ;
        JSR     PEM             ;

        PHA
        AND     #$F0
        TAX
        PLA
        CPX     #$30
        BNE     READKB_NOTFKEY
        AND     #$0F
        STA     TEMP
        LDX     #6              ;
        JSR     PEM             ;
        CMP     #'~'
        BNE     READKB_FKEY1
        LDA     #$00
        JMP     READKB_FKEY2
READKB_FKEY1:
        PHA

        LDX     #6              ;
        JSR     PEM             ;
        ASL     TEMP
        ASL     TEMP
        ASL     TEMP
        ASL     TEMP

        PLA
        AND     #$0F
READKB_FKEY2:
        CLC
        ADC     TEMP
        LDY     #01
        JMP     READKB_XOFF


READKB_NOTFKEY:
        CMP     #'A'
        BNE     READKB_NOTUP
        LDY     #01
        LDA     #00
        JMP     READKB_XOFF

READKB_NOTUP:
        CMP     #'B'
        BNE     READKB_NOTDN
        LDY     #01
        LDA     #01
        JMP     READKB_XOFF
READKB_NOTDN:
        CMP     #'D'
        BNE     READKB_NOTLEFT
        LDY     #01
        LDA     #02
        JMP     READKB_XOFF
READKB_NOTLEFT:
        CMP     #'C'
        BNE     READKB_NOTRIGHT
        LDY     #01
        LDA     #03
        JMP     READKB_XOFF
READKB_NOTRIGHT:
        JSR     READKB_XOFF
        JMP     READKB_ERR

READKB_XON:
        PHA
        LDA     #17
        JMP     READKB_XOFF1
READKB_XOFF:
        PHA
        LDA     #19
READKB_XOFF1:
        LDX     #2              ;  OUTPUT THE CHAR TO THE CONSOLE
        JSR     PEM             ;
        PLA
        RTS

READKB_ANSIKEYS:
        LDX     #6              ;
        JSR     PEM             ;
        PHA
        AND     #$F0
        CMP     #$50
        BNE     READKB_ANSI_ERR ; NOT A FUNCTION KEY, ERROR
        PLA
        AND     #$0F
        ORA     #$10
        CLC
        ADC     #$01
        CMP     #$1A
        BNE     :+
        LDA     #$24
:
        CMP     #$18
        BNE     :+
        LDA     #$19
:
        CMP     #$17
        BNE     :+
        LDA     #$18
:
        LDY     #01
        JMP     READKB_XOFF

READKB_ANSI_ERR:
        PLA                     ; CLEAN UP STACKED CHAR
        JMP     READKB_ERR


;__CLEARBUFFER___________________________________________________________________________________________________________________
;
; 	CLEAR THE EDIT BUFFER
;
;
;
;________________________________________________________________________________________________________________________________
;
CLEARBUFFER:
        LDA     #<BUFFER        ; SETUP BUFFER
        STA     EDTPOS          ;
        LDA     #>BUFFER        ;
        STA     EDTPOS+1        ;
        LDY     #$00            ;

        LDA     #$00
CLEARBUFFER1:
        STA     (EDTPOS),Y
        INY
        BNE     CLEARBUFFER1
        INC     EDTPOS+1
        LDX     EDTPOS+1
        CPX     #>Ram_top       ; are we at the end of RAM?
        BCC     CLEARBUFFER1    ; Less than high byte, continue
        BNE     CLEARBUFFER_DONE; Greater than high byte, done
        LDX     EDTPOS          ; High bytes equal, check low byte
        CPX     #<Ram_top       ;
        BCC     CLEARBUFFER1    ; Less than Ram_top, continue
CLEARBUFFER_DONE:
        RTS


;__DOS65LOAD_____________________________________________________________________________________________________________________
;
; 	LOAD A DOS/65 FILE INTO THE EDIT BUFFER
;		FILE IS LOADED INTO 80 COLUMN LINES TO SIMPLIFY SCROLLING
;
;
;________________________________________________________________________________________________________________________________
;
DOS65LOAD:
        LDA     #0              ; clear
        STA     DFLFCB+32       ; record number
        LDA     #<DFLFCB        ; Open File
        LDY     #>DFLFCB        ;
        LDX     #15             ;
        JSR     PEM             ;
        CMP     #$FF            ; error?, if NOT, continue
        BNE     DOS65LOAD_1     ;

        LDA     #<BUFFER        ; SETUP BUFFER
        STA     FCBPTR          ; STORE DEST BUFFER IN FCBPTR
        LDA     #>BUFFER        ;
        STA     FCBPTR+1        ; STORE DEST BUFFER IN FCBPTR
        LDA     FCBPTR          ;
        STA     CURRENTLEN      ;
        LDA     FCBPTR+1        ;
        STA     CURRENTLEN+1    ;
        LDA     #13
        JMP     DOS65LOAD_STOREBT; OPEN EDITOR, BLANK FILE

DOS65LOAD_ERR:
        LDA     #<FILEERROR2    ; NO, ERROR OUT
        LDY     #>FILEERROR2
        LDX     #9              ; Print error message
        JSR     PEM             ;
        JMP     BOOT            ; WARM BOOT DOS/65
DOS65LOAD_1:
        LDA     #0              ; clear
        STA     DFLFCB+32       ; record number
        STA     CURRENTLEN      ; LENGTH COUNTER
        LDA     #<BUFFER        ; SETUP BUFFER
        STA     FCBPTR          ; STORE DEST BUFFER IN FCBPTR
        LDA     #>BUFFER        ;
        STA     FCBPTR+1        ; STORE DEST BUFFER IN FCBPTR

        LDA     #<FCBBUFFER     ; SETUP BUFFER
        STA     TMPPOS          ;
        LDY     #>FCBBUFFER
        STY     TMPPOS+1        ;
        LDX     #26             ; Setup Buffer
        JSR     PEM

DOS65LOAD_2:
        LDA     #<DFLFCB        ; READ record buffer from disk file
        LDY     #>DFLFCB        ;
        LDX     #20             ;
        JSR     PEM             ;
        CMP     #$00            ; is error or EOF
        BEQ     DOS65LOAD_3     ;
        CMP     #$01            ; EOF?
        BEQ     DOS65LOAD_EOF   ; YES, END
        JMP     DOS65LOAD_ERR   ; ERROR, do error handling
DOS65LOAD_3:
        LDX     #$00            ; CLEAR COUNTERS
        LDY     #$00            ;
DOS65LOAD_3A:
        LDA     (TMPPOS),Y      ; LOAD BYTE FROM BUFFER
        INY                     ; INC BUFFER COUNTER
        CMP     #$1A            ; IF EOF, SIGNAL END
        BEQ     DOS65LOAD_4     ;
        CMP     #$0D            ; IF CR PLACE IT IN BUFFER AND PAD WITH SPACES TO MAXCOL
        BEQ     DOS65LOAD_3D    ;
        CMP     #32             ; IF UNPRINTABLE, DO NOT PLACE IN BUFFER
        BCC     DOS65LOAD_3C    ;
        CMP     #127            ;
        BCS     DOS65LOAD_3C    ;
        JSR     DOS65LOAD_STOREBT; STORE THE BYTE IN THE BUFFER
DOS65LOAD_3C:
        CPY     #$80            ; ARE WE AT THE END OF THE SOURCE BUFFER?
        BNE     DOS65LOAD_3A    ; NO, LOOP

        JMP     DOS65LOAD_2     ; KEEP GOING!

DOS65LOAD_3D:                   ; IN ORDER TO MAKE SCROLLING EASIER <CR> LINES NEED PADDED TO MAXCOL
        JSR     DOS65LOAD_STOREBT; STORE THE <CR> BYTE
DOS65LOAD_3D1:
        LDA     CURRENTLEN      ;
        BEQ     DOS65LOAD_3D2   ; ALREADY WRAPPED TO 0 (79-CHAR LINE), NO PADDING NEEDED
        CMP     #MAXCOL         ;
        BEQ     DOS65LOAD_3D2   ;
        LDA     #$00            ;
        STA     (FCBPTR,X)      ; PLACE CHAR IN BUFFER
        INC     CURRENTLEN      ; INC THE LENGTH COUNTER
        JSR     DOS65LOAD_STOREBT1; INC THE BUFFER POINTER
        JMP     DOS65LOAD_3D1   ;
DOS65LOAD_3D2:
        LDA     #$00            ;
        STA     CURRENTLEN      ;
        JMP     DOS65LOAD_3C    ;
DOS65LOAD_EOF:
        LDA     #13
        JSR     DOS65LOAD_STOREBT

DOS65LOAD_4:
        LDA     #<DFLFCB        ; CLOSE FILE
        LDY     #>DFLFCB        ;
        LDX     #16             ;
        JSR     PEM             ;
        LDA     FCBPTR          ;
        STA     CURRENTLEN      ;
        LDA     FCBPTR+1        ;
        STA     CURRENTLEN+1    ;
        RTS


DOS65LOAD_STOREBT:
        STA     (FCBPTR,X)      ; PLACE CHAR IN BUFFER
        INC     CURRENTLEN      ; INC THE LENGTH COUNTER
        LDA     CURRENTLEN      ;
        CMP     #MAXCOL         ; WRAP THE COUNTER ON MAXCOL
        BNE     DOS65LOAD_STOREBT1;
        LDA     #$00            ;
        STA     CURRENTLEN      ;
DOS65LOAD_STOREBT1:
        INC     FCBPTR          ; add to base counter
        BNE     DOS65LOAD_STOREBT2
        INC     FCBPTR+1        ; yes, carry, inc high byte
        LDA     FCBPTR+1        ;
        CMP     #>Ram_top       ; are we at the end of RAM?
        BCC     DOS65LOAD_STOREBT2; Less than high byte, OK
        BNE     DOS65LOAD_STOREBTE; Greater than high byte, error
        LDA     FCBPTR          ; High bytes equal, check low byte
        CMP     #<Ram_top       ;
        BCS     DOS65LOAD_STOREBTE; At or past Ram_top, error
DOS65LOAD_STOREBT2:
        RTS
DOS65LOAD_STOREBTE:
        JSR     DOS65LOAD_4     ; CLOSE FILE
        LDA     #<FILEERROR3    ; NO, ERROR OUT
        LDY     #>FILEERROR3
        LDX     #9              ; Print error message
        JSR     PEM             ;
        JMP     BOOT            ; WARM BOOT DOS/65


;__DOS65SAVEAS_____________________________________________________________________________________________________________________
;
; 	SAVE A DOS/65 FILE FROM THE EDIT BUFFER TO A NEW FILE NAME
;
;
;
;________________________________________________________________________________________________________________________________
;
DOS65SAVEAS:
        JSR     CLEARSCREEN

        LDA     #<SAVESCREENFORMAT
        LDY     #>SAVESCREENFORMAT
        LDX     #9              ;
        JSR     PEM             ;
        LDA     #<SAVESCREENFORMAT1
        LDY     #>SAVESCREENFORMAT1
        LDX     #9              ;
        JSR     PEM             ;
DOS65SAVEAS1:
        LDA     #<SAVESCREENFORMAT2
        LDY     #>SAVESCREENFORMAT2
        LDX     #9              ;
        JSR     PEM             ;

        LDX     #13
        LDA     #' '
DOS65SAVEAS2:
        STA     FNBUFFER,X
        DEX
        BNE     DOS65SAVEAS2
        STA     FNBUFFER        ; CLEAR BYTE 0 AS WELL
        JSR     GETFILENAME
        JSR     DOS65FCBPREP
        CMP     #$FF
        BEQ     DOS65SAVEAS1

        JSR     DOS65SAVE
        LDA     #$00
        STA     CSRPOSX
        STA     CSRPOSY
        STA     EDTPOS
        STA     EDTPOS+1
        STA     TMPPOS
        STA     TMPPOS+1
        JMP     PAINTSCREEN

SAVESCREENFORMAT:
        .BYTE   $1B,'[','0','0',';','0','0','H'
        .BYTE   $1B,'[','7','m'
        .BYTE   "DOS/65 SCREEN EDITOR"
        .BYTE   "                    "
        .BYTE   "                    ",'$'

SAVESCREENFORMAT1:
        .BYTE   "                    "
        .BYTE   "____________________"
        .BYTE   "____________________"
        .BYTE   "____________________"
        .BYTE   "____________________"
        .BYTE   $1B,'[','2','3',';','0','0','H'
        .BYTE   "                            "
        .BYTE   "                            "
        .BYTE   "     OM                "
        .BYTE   $1B,'[','0','m','$'
SAVESCREENFORMAT2:
        .BYTE   $1B,'[','1','1',';','1','0','H'
        .BYTE   "                                   "
        .BYTE   $1B,'[','1','0',';','0','0','H'
        .BYTE   "  NEW FILE NAME: $"

FNBUFFER:
        .BYTE   "                  "

;__GETFILENAME___________________________________________________________________________________________________________________
;
; POPULATE FNBUFFER WITH ONLY VALID FILENAME CHARACTERS
;________________________________________________________________________________________________________________________________
;
GETFILENAME:
        JSR     READKB_XON
        LDY     #$00
GETFILENAME_LOOP:
        LDX     #6
        JSR     PEM             ;
        CMP     #8
        BEQ     GETFILENAME_BACKSPACE
        CMP     #13
        BEQ     GETFILENAME_END
        CMP     #33
        BCC     GETFILENAME_LOOP
        CMP     #40
        BCC     :+
        CMP     #43
        BEQ     :+
        CMP     #45
        BEQ     :+
        CMP     #46
        BEQ     :+
        CMP     #48
        BCC     GETFILENAME_LOOP
        CMP     #59
        BCC     :+
        CMP     #64
        BCC     GETFILENAME_LOOP
        CMP     #91
        BCC     :+
        CMP     #97
        BCC     GETFILENAME_LOOP
        CMP     #123
        BCC     GETFILENAME_TOUPPER
        JMP     GETFILENAME_LOOP
:
        PHA
        LDX     #2
        JSR     PEM
        TYA
        TAX
        PLA
        STA     FNBUFFER,X
        INY
        CPY     #14
        BNE     GETFILENAME_LOOP
GETFILENAME_END:
        TYA
        TAX
        LDA     #$00
        STA     FNBUFFER,X
        RTS
GETFILENAME_TOUPPER:
        SEC
        SBC     #32
        JMP     :-
GETFILENAME_BACKSPACE:
        CPY     #00
        BEQ     GETFILENAME_LOOP
        LDA     #8
        LDX     #2
        JSR     PEM
        LDA     #32
        LDX     #2
        JSR     PEM
        LDA     #8
        LDX     #2
        JSR     PEM
        DEY
        TYA
        TAX
        LDA     #32
        STA     FNBUFFER,X
        JMP     GETFILENAME_LOOP






;__DOS65SAVE_____________________________________________________________________________________________________________________
;
; 	SAVE A DOS/65 FILE FROM THE EDIT BUFFER
;
;
;
;________________________________________________________________________________________________________________________________
;
DOS65SAVE:
        LDA     #0              ; clear
        STA     DFLFCB+32       ; record number
        LDA     #<DFLFCB        ; Open File
        LDY     #>DFLFCB        ;
        LDX     #19             ;
        JSR     PEM             ;

        LDA     #0              ; clear
        STA     DFLFCB+32       ; record number
        LDA     #<DFLFCB        ; ATTEMPT TO CREATE File
        LDY     #>DFLFCB        ;
        LDX     #22             ;
        JSR     PEM             ;
        CMP     #$FF            ; error?, if NOT, continue
        BNE     DOS65SAVE_1     ;

DOS65SAVE_ERR:
        LDA     #<FILEERROR2A   ; NO, ERROR OUT
        LDY     #>FILEERROR2A
        LDX     #9              ; Print error message
        JMP     PEM             ; RETURN AFTER PRINT
DOS65SAVE_1:
        LDA     #0              ; clear
        STA     DFLFCB+32       ; record number
        LDA     #<BUFFER        ; SETUP BUFFER
        STA     FCBPTR          ; STORE SRC BUFFER IN FCBPTR
        LDA     #>BUFFER        ;
        STA     FCBPTR+1        ; STORE SRC BUFFER IN FCBPTR

        LDA     #<FCBBUFFER     ; SETUP BUFFER
        STA     TMPPOS          ;
        LDY     #>FCBBUFFER
        STY     TMPPOS+1        ;
        LDX     #26             ; Setup Buffer
        JSR     PEM

        LDX     #$00            ;
        STX     TEMP1           ;
DOS65SAVE_2:
        LDA     FCBPTR+1        ; COMPARE HIGH BYTES
        CMP     CURRENTLEN+1    ;
        BCC     DOS65SAVE_2B    ; FCBPTR+1 < CURRENTLEN+1, USE ACTUAL DATA
        BNE     DOS65SAVE_2EOF  ; FCBPTR+1 > CURRENTLEN+1, PAST END
        LDA     FCBPTR          ; HIGH BYTES EQUAL, CHECK LOW BYTES
        CMP     CURRENTLEN      ;
        BCC     DOS65SAVE_2B    ; FCBPTR < CURRENTLEN, USE ACTUAL DATA
        BEQ     DOS65SAVE_2B    ; FCBPTR == CURRENTLEN, USE ACTUAL DATA
DOS65SAVE_2EOF:
        LDA     #$1A            ; WE ARE AT END SO PAD WITH EOF
        JMP     DOS65SAVE_2C    ;

DOS65SAVE_2B:
        LDY     #$00
        LDA     (FCBPTR),Y      ; GET NEXT BUFFER BYTE
        INC     FCBPTR          ; INC 16 BIT BUFFER POINTER
        BNE     DOS65SAVE_2B1   ;
        INC     FCBPTR+1        ;
DOS65SAVE_2B1:
        CMP     #$00            ; IS NULL?
        BEQ     DOS65SAVE_2     ; YES SKIP
        CMP     #10             ; IS LF?
        BEQ     DOS65SAVE_2     ; YES SKIP

DOS65SAVE_2C:
        LDY     TEMP1           ;
        STA     (TMPPOS),Y      ; NO STORE IN BUFFER
        INC     TEMP1           ; INC BUFFER POINTER
        CMP     #13             ; IF NOT CR
        BNE     DOS65SAVE_2C1   ; CONTINUE
        JSR     DOS65SAVE_LF    ; OTHERWISE INSERT LF
DOS65SAVE_2C1:
        LDX     TEMP1           ;
        CPX     #$80            ; PAST END?
        BNE     DOS65SAVE_2     ; NO LOOP TILL BUFFER IS FULL

DOS65SAVE_2D:
        LDA     #<DFLFCB        ; WRITE record buffer from disk file
        LDY     #>DFLFCB        ;
        LDX     #21             ;
        JSR     PEM             ;
        CMP     #$00            ; is SUCCESSFUL?
        BEQ     DOS65SAVE_2D1   ;
        JMP     DOS65SAVE_ERR   ; ERROR, do error handling
DOS65SAVE_2D1:

        LDA     FCBPTR+1        ;
        CMP     CURRENTLEN+1    ; AT END?
        BCC     DOS65SAVE_3     ; FCBPTR+1 < CURRENTLEN+1, CONTINUE
        BNE     DOS65SAVE_4     ; FCBPTR+1 > CURRENTLEN+1, CLOSE FILE
        LDA     FCBPTR          ; HIGH BYTES EQUAL, CHECK LOW BYTES
        CMP     CURRENTLEN      ;
        BCC     DOS65SAVE_3     ; FCBPTR < CURRENTLEN, CONTINUE
        JMP     DOS65SAVE_4     ; FCBPTR >= CURRENTLEN, CLOSE FILE

DOS65SAVE_3:
        LDX     #$00            ;
        STX     TEMP1           ;
        JMP     DOS65SAVE_2     ; SAVE NEXT SECTOR

DOS65SAVE_4:
        LDA     #<DFLFCB        ; CLOSE FILE
        LDY     #>DFLFCB        ;
        LDX     #16             ;
        JMP     PEM             ; CLOSE AND RETURN


DOS65SAVE_LF:

        LDX     TEMP1           ;
        CPX     #$80            ; PAST END?
        BNE     DOS65SAVE_LFA   ; NO
        JSR     DOS65SAVE_LF1   ;
DOS65SAVE_LFA:
        LDY     TEMP1           ; WRITE LF
        LDA     #10             ;
        STA     (TMPPOS),Y      ; NO STORE IN BUFFER
        INC     TEMP1           ; INC BUFFER POINTER
        RTS

DOS65SAVE_LF1:
        LDA     #<DFLFCB        ; WRITE record buffer from disk file
        LDY     #>DFLFCB        ;
        LDX     #21             ;
        JSR     PEM             ;
        CMP     #$00            ; is SUCCESSFUL?
        BEQ     DOS65SAVE_LF1A  ;
        JMP     DOS65SAVE_ERR   ; ERROR, do error handling
DOS65SAVE_LF1A:
        LDX     #$00            ;
        STX     TEMP1           ;
        RTS

DOS65FCBPREP:
        LDA     FNBUFFER+1      ; GET ":"
        CMP     #':'            ;
        BEQ     DOS65FCBPREP_1  ; YES, IT WAS A DRIVE, CONTINUE
        LDA     #<FILEERROR1    ; NO, ERROR OUT
        LDY     #>FILEERROR1
        LDX     #9
        JSR     PEM
        LDA     #$FF
        RTS
DOS65FCBPREP_1:
        LDA     FNBUFFER        ; GET DRIVE LETTER
        SEC                     ;
        SBC     #64             ; PARSE DRIVE NUMBER
        STA     DFLFCB+0        ; STORE IT IN FCB

        LDY     #$01            ; POINT Y TO FCB FILE NAME
        LDA     #$20            ; LOAD SPACE CHAR INTO A
DOS65FCBPREP_1A:                ; BLANK OUT FCB
        STA     DFLFCB,Y        ;
        INY                     ;
        CPY     #$09            ; IS DONE
        BNE     DOS65FCBPREP_1A ; NO, LOOP
        LDY     #$01            ; POINT Y TO FCB FILE NAME
        LDX     #$00            ;
DOS65FCBPREP_2:                 ; COPY FILE NAME PARAMETER INTO FCB
        LDA     FNBUFFER+2,X    ;
        INX                     ;
        CMP     #'.'            ;
        BEQ     DOS65FCBPREP_3  ;
        STA     DFLFCB,Y        ;
        INY                     ;
        CPY     #$09            ;
        BEQ     DOS65FCBPREP_3  ;
        JMP     DOS65FCBPREP_2  ;
DOS65FCBPREP_3:                 ;
        LDA     FNBUFFER+2,X    ;
        STA     DFLFCB+9        ;
        LDA     FNBUFFER+3,X    ;
        STA     DFLFCB+10       ;
        LDA     FNBUFFER+4,X    ;
        STA     DFLFCB+11       ;
        LDA     #$00            ;
        RTS                     ;


FILEERROR1:
        .BYTE   $0D,$0A,$0D,$0A,"** NO DRIVE SPECIFIED, TRY AGAIN"
        .BYTE   $0D,$0A,'$'
FILEERROR2A:
        .BYTE   $1B,'[','0','1',';','1','0','H'
        .BYTE   $1B,'[','7','m'
FILEERROR2:
        .BYTE   "** DOS/65 ERROR, OPERATION ABORTED"
        .BYTE   $1B,'[','0','m'
        .BYTE   $0D,$0A,'$'
FILEERROR3:
        .BYTE   "** FILE TOO LARGE, OPERATION ABORTED"
        .BYTE   $0D,$0A,'$'
DECIMAL1:
        .BYTE   $00,$01,$02,$03,$04,$05,$06,$07,$08,$09
        .BYTE   $10,$11,$12,$13,$14,$15,$16,$17,$18,$19
        .BYTE   $20,$21,$22,$23,$24,$25,$26,$27,$28,$29
        .BYTE   $30,$31,$32,$33,$34,$35,$36,$37,$38,$39
        .BYTE   $40,$41,$42,$43,$44,$45,$46,$47,$48,$49
        .BYTE   $50,$51,$52,$53,$54,$55,$56,$57,$58,$59
        .BYTE   $60,$61,$62,$63,$64,$65,$66,$67,$68,$69
        .BYTE   $70,$71,$72,$73,$74,$75,$76,$77,$78,$79
        .BYTE   $80,$81,$82,$83,$84,$85,$86,$87,$88,$89
        .BYTE   $90,$91,$92,$93,$94,$95,$96,$97,$98,$99
FCBBUFFER:
        .RES    $80
        .ALIGN  256
BUFFER:
        .BYTE   00              ; start of user RAM (set as needed, should be page aligned)
Ram_top         = $B800         ; end of user RAM+1 (set as needed, should be page aligned)

        .END
