;        PAGE
;        SBTTL   "--- MACHINE-DEPENDENT I/O: DOS/65 ---"

; ----------------------------
; FETCH ASCII KEYCODE INTO [A]
; ----------------------------

; EXIT: ASCII IN [A] & [IOCHAR]

GETKEY:
        TXA                     ; SAVE [X] & [Y]
        PHA
        TYA
        PHA
GKEY1:
        LDX     #6              ;  GET KEY INTO [A]
        JSR     PEM             ;
        CMP     #$00
        BEQ     GKEY1           ; NOT IF CODE WAS ZERO

        CMP     #EOL            ; EOL?
        BEQ     GOODKEY
        CMP     #BACKSP         ; BACKSPACE?
        BEQ     GOODKEY
        CMP     #SPACE          ; ANYTHING ELSE < "SPACE"
        BCC     BADKEY          ; IS BAD

        CMP     #'z'+1
        BCS     BADKEY
        CMP     #'a'
        BCS     GOODKEY

        CMP     #'A'            ; TO ASCII LOWER CASE
        BCC     GOODKEY
        CMP     #'Z'+1
        BCS     BADKEY
        CLC
        ADC     #$20
        JMP     GOODKEY

BADKEY:
        JSR     BOOP            ; REJECT BAD KEYPRESS
        JMP     GKEY1           ; AND TRY AGAIN

GOODKEY:
        STA     IOCHAR          ; SAVE KEYCODE HERE
        PLA                     ; RESTORE [X] & [Y]
        TAY
        PLA
        TAX
        LDA     IOCHAR          ; GET KEYCODE HERE

        RTS

; -------------------------
; OUTPUT AN ASCII CHARACTER
; -------------------------
CHAR:
LETTER:
        STA     IOCHAR          ; SAVE HERE
        PHA                     ; SAVE [A]
        TXA                     ; SAVE [X] & [Y]
        PHA
        TYA
        PHA
        LDA     IOCHAR
        CMP     #13
        BNE     :+
        INC     LINCNT
        LDA     #10             ;  IS A CR, SO ADD A LF
        LDX     #2              ;
        JSR     PEM             ;
        LDA     #13
:
        LDX     #2              ;  OUTPUT CHAR FROM [A]
        JSR     PEM             ;

        PLA                     ; RESTORE [X] & [Y]
        TAY
        PLA
        TAX
        PLA
        RTS



; ---------------------
; FETCH A LINE OF INPUT
; ---------------------

; ENTRY: ABS ADDR OF READ BUFFER IN [ARG1]
; EXIT: # CHARS READ IN [A]

INPUT:
        JSR     LINOUT          ; FLUSH [LBUFF]

        JSR     CURSON          ; ACTIVATE CURSOR, CLEAR KEY QUEUE

        LDY     #0
        STY     LINCNT          ; RESET LINE COUNT

INLOOP:
        JSR     GETKEY          ; GET ASCII INTO [A] AND [IOCHAR]

        CMP     #EOL            ; EOL?
        BEQ     ENDLIN          ; LINE DONE IF SO
        CMP     #BACKSP         ; BACKSPACE?
        BEQ     BACKUP          ; SPECIAL HANDLING

        STA     LBUFF,Y         ; ELSE ADD CHAR TO INPUT BUFFER
        INY                     ; NEXT POSITION IN LINE

SHOWIT:
        JSR     CHAR            ; SEND TO SCREEN

        CPY     #77             ; 2 SCREEN LINES FULL?
        BCC     INLOOP          ; NO, GET ANOTHER CHAR

; HANDLE LINE OVERFLOW

NOMORE:
        JSR     GETKEY
        CMP     #EOL            ; IF EOL,
        BEQ     ENDLIN          ; WRAP UP THE LINE
        CMP     #BACKSP         ; BACKSPACE
        BEQ     BACKUP          ; IS OKAY TOO
        JSR     BOOP            ; ELSE COMPLAIN
        JMP     NOMORE          ; AND INSIST

; HANDLE BACKSPACE

BACKUP:
        DEY                     ; BACK UP THE POINTER
        BPL     SHOWIT          ; SEND BS IF NOT START OF LINE
        JSR     BOOP            ; ELSE SCREAM WITH PAIN
        LDY     #0              ; RESET POINTER
        BEQ     INLOOP          ; AND WAIT FOR SOMETHING BETTER

; HANDLE END OF LINE

ENDLIN:
        STA     LBUFF,Y         ; SHIP EOL TO BUFFER
        INY                     ; UPDATE INDEX
        STY     LINLEN          ; SAVE HERE FOR "READ"
        STY     PRLEN           ; AND HERE FOR "PPRINT"
        LDX     #0
        JSR     CHAR            ; AND SEND EOL TO SCREEN

; MOVE [LBUFF] TO [ARG1] W/LC CONVERSION

LEX1:
        LDA     LBUFF-1,Y       ; GET A CHAR FROM [LBUFF]
        STA     (ARG1),Y        ; MOVE CHAR TO INPUT BUFFER AT [ARG1]
        DEY                     ; LOOP TILL
        BNE     LEX1            ; ALL CHARS MOVED ("BNE" 8/14/85 BM)

        JSR     PPRINT          ; SCRIPT [LBUFF] IF ENABLED

        LDA     LINLEN          ; RESTORE # CHARS
        RTS                     ; INTO [A]

; -----------------------
; DIRECT PRINT LINE [X/A]
; -----------------------

; ENTRY: STRING ADDRESS IN [X/A] (LSB/MSB)
;        STRING LENGTH IN [Y]

DLINE:
        STX     STRING+LO       ; DROP STRING ADDRESS
        STA     STRING+HI       ; INTO DUMMY BYTES

        LDX     #0              ; INIT CHAR-FETCH INDEX

DOUT:
        .BYTE   $BD             ; 6502 "LDA nnnn,X" OPCODE
STRING:
        .WORD   $0000           ; DUMMY OPERAND BYTES
        JSR     CHAR
        INX
        DEY                     ; LOOP TILL
        BNE     DOUT            ; OUT OF CHARS
        RTS

; -----------------------
; SEND [LBUFF] TO PRINTER
; -----------------------

; ENTRY: LENTH OF LINE IN [PRLEN]
; NOW WITH IMPROVED ERROR PROTECTION! (BM 11/24/84)

SFLAG:
        .BYTE   0               ; PREVIOUS SCRIPT MODE (BM 5/14/85)

PPRINT:
        LDA     SCRIPT          ; SCRIPTING INTERNALLY ENABLED?
        BEQ     PEX             ; NO, SCRAM IMMEDIATELY

        LDA     ZBEGIN+ZSCRIP+1 ; CHECK SCRIPT FLAG
        AND     #%00000001      ; SCRIPTING ON?
        BEQ     PP3             ; NO, CHECK FOR "UNSCRIPT"

        LDA     PSTAT           ; CHECK PRINTER STATUS
        BMI     PEX             ; CAN'T OPEN IF NEGATIVE
        BNE     PP1             ; ALREADY OPEN, SCRIPT THE LINE

; OPEN THE PRINTER FOR OUTPUT
        LDX     #1
        STX     PSTAT           ; SET STATUS TO "PRINTER OPENED" (1)

PP1:
        LDY     #0              ; INIT INDEX
PP2:

        LDA     LBUFF,Y
        LDX     #5              ;  OUTPUT CHAR FROM [A] TO PRINTER (LST)
        JSR     PEM             ;
        INY
        DEC     PRLEN
        BNE     PP2
        BEQ     PEX             ; RESET & RETURN

; CHECK FOR "UNSCRIPT"

PP3:
        LDA     PSTAT           ; CHECK PRINTER STATUS
        BEQ     PEX             ; EXIT IF PRINTER WAS OFF
        BMI     PEX             ; OR UNOPENABLE

PCLOSE:
        LDA     #0              ; RESET PRINTER STATUS FLAG
        STA     PSTAT           ; TO "CLOSED"

PEX:
        RTS

; ------------
; SPLIT SCREEN
; ------------
; NO SPLIT SCREEN IN DOS/65
ZSPLIT:
OFFSPL:
        RTS

; ------
; SCREEN
; ------

; GO TO TOP WINDOW IF [A] = 0
; GO TO BOTTOM IF [A] = 1
; IGNORE IF SPLIT NOT ENABLED OR [A] <> 0 OR 1
ZSCRN:
; SET TO TOP WINDOW
TOTOP:
; SET TO BOTTOM WINDOW
TOBOT:
DOSCRN:
        RTS

; ---------
; RAZZ USER
; ---------

BOOP:
        LDA     #7
        LDX     #2
        JSR     PEM
        RTS


PLOT:
; ENTRY: [X] = X COORD
;        [Y] = Y COORD
; Convert X coordinate to 2 ASCII digits in PLOTCOLVAL
        TXA                     ; Get X value
        LDX     #1              ; Start with ones digit
; Divide by 10 repeatedly to get digits
:
        CMP     #10
        BCC     :+              ; Branch if < 10
        SBC     #10
        INX                     ; Count number of 10s
        JMP     :-
:
        CLC
        ADC     #'0'            ; Convert remainder to ASCII
        STA     PLOTCOLVAL+1    ; Store ones digit

        TXA                     ; Get tens digit count
        CLC
        ADC     #'0'-1          ; Convert to ASCII (-1 since we started at 1)
        STA     PLOTCOLVAL      ; Store tens digit
; Convert Y coordinate to 2 ASCII digits in PLOTROWVAL
        TYA                     ; Get Y value
        LDX     #1              ; Start with ones digit
; Divide by 10 repeatedly to get digits
:
        CMP     #10
        BCC     :+              ; Branch if < 10
        SBC     #10
        INX                     ; Count number of 10s
        JMP     :-
:
        CLC
        ADC     #'0'            ; Convert remainder to ASCII
        STA     PLOTROWVAL+1    ; Store ones digit

        TXA                     ; Get tens digit count
        CLC
        ADC     #'0'-1          ; Convert to ASCII (-1 since we started at 1)
        STA     PLOTROWVAL      ; Store tens digit
;
        LDX     #<PLOTSTRING
        LDA     #>PLOTSTRING
        LDY     #8
        JSR     DLINE
        RTS
PLOTSTRING:
        .BYTE   27,"["
PLOTROWVAL:
        .BYTE   "00"
        .BYTE   ";"
PLOTCOLVAL:
        .BYTE   "00"
        .BYTE   "H"

; ------------------------
; CLEAR SCREEN
; ------------------------

CLS:
        LDA     #20
        STA     LMAX
        LDX     #<CLSSTRING
        LDA     #>CLSSTRING
        LDY     #4
        JSR     DLINE
        LDA     #0
        STA     LINCNT          ; RESET LINE COUNTER
        LDX     #0
        LDY     #YSIZE
        JSR     PLOT
        RTS
CLSSTRING:
        .BYTE   27,"[2J"

BOLD:
        LDX     #<BOLDSTRING
        LDA     #>BOLDSTRING
        LDY     #4
        JSR     DLINE
        RTS
BOLDSTRING:
        .BYTE   27,"[1m"

UNBOLD:
        LDX     #<UNBOLDSTRING
        LDA     #>UNBOLDSTRING
        LDY     #4
        JSR     DLINE
        RTS
UNBOLDSTRING:
        .BYTE   27,"[0m"

REVERSE:
        LDX     #<REVERSESTRING
        LDA     #>REVERSESTRING
        LDY     #4
        JSR     DLINE
        RTS
REVERSESTRING:
        .BYTE   27,"[7m"
