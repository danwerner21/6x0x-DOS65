;        PAGE
;        SBTTL   "--- READ HANDLER ---"

; ----
; READ
; ----

; READ LINE INTO TABLE [ARG1]; PARSE INTO TABLE [ARG2]

ZREAD:
        JSR     ZUSL            ; UPDATE THE STATUS LINE

        LDA     ARG1+HI         ; MAKE THE TABLE ADDRESSES
        CLC                     ; ABSOLUTE
        ADC     ZCODE           ; LSBS NEED NOT CHANGE
        STA     ARG1+HI

        LDA     ARG2+HI
        CLC
        ADC     ZCODE
        STA     ARG2+HI

        JSR     INPUT           ; READ LINE; RETURN LENGTH IN [A]
        STA     LINLEN          ; SAVE # CHARS IN LINE

        LDA     #0
        STA     WRDLEN          ; INIT # CHARS IN WORD COUNTER

        LDY     #1              ; POINT TO "# WORDS READ" SLOT
        STA     (ARG2),Y        ; AND CLEAR IT ([A] = 0)

        STY     SOURCE          ; INIT SOURCE TABLE PNTR ([Y] = 1)
        INY                     ; = 2
        STY     RESULT          ; AND RESULT TABLE POINTER

; MAIN LOOP STARTS HERE

READL:
        LDY     #0              ; POINT TO "MAX WORDS" SLOT
        LDA     (ARG2),Y        ; AND READ IT
        BEQ     RLERR           ; PATCH IF ZERO
        CMP     #60             ; OKAY IF < 60
        BCC     RL0

RLERR:
        LDA     #59             ; MAXIMUM VALUE IS 59 (BM 5/14/85)
        STA     (ARG2),Y

RL0:
        INY                     ; POINT TO "# WORDS READ" SLOT
        CMP     (ARG2),Y        ; TOO MANY WORDS?
        BCC     RLEX            ; EXIT IF SO (BM 5/1/85)

RL1:
        LDA     LINLEN
        ORA     WRDLEN          ; OUT OF CHARS AND WORDS?
        BNE     RL2             ; NOT YET
RLEX:
        RTS                     ; ELSE EXIT

RL2:
        LDA     WRDLEN          ; GET WORD LENGTH
        CMP     #6              ; 6 CHARS DONE?
        BCC     RL3             ; NO, KEEP GOING
        JSR     FLUSHW          ; ELSE FLUSH REMAINDER OF WORD

RL3:
        LDA     WRDLEN          ; GET WORD LENGTH AGAIN
        BNE     READL2          ; CONTINUE IF NOT FIRST CHAR

; START A NEW WORD

        LDX     #5              ; CLEAR Z-WORD INPUT BUFFER
RLL:
        STA     IN,X            ; [A] = 0
        DEX
        BPL     RLL

        JSR     EFIND           ; GET BASE ADDRESS INTO [ENTRY]
        LDA     SOURCE          ; STORE THE START POS OF THE WORD
        LDY     #3              ; INTO THE "WORD START" SLOT
        STA     (ENTRY),Y       ; OF THE RESULT TABLE

        TAY
        LDA     (ARG1),Y        ; GET A CHAR FROM SOURCE BUFFER
        JSR     SIB             ; IS IT A SELF-INSERTING BREAK?
        BCS     DOSIB           ; YES IF CARRY WAS SET

        JSR     NORM            ; IS IT A "NORMAL" BREAK?
        BCC     READL2          ; NO, CONTINUE

        INC     SOURCE          ; ELSE FLUSH THE STRANDED BREAK
        DEC     LINLEN          ; UPDATE # CHARS LEFT IN LINE
        JMP     READL           ; AND LOOP

READL2:
        LDA     LINLEN          ; OUT OF CHARS YET?
        BEQ     READL3          ; LOOKS THAT WAY

        LDY     SOURCE
        LDA     (ARG1),Y        ; ELSE GRAB NEXT CHAR
        JSR     BREAK           ; IS IT A BREAK?
        BCS     READL3          ; YES IF CARRY WAS SET

        LDX     WRDLEN          ; ELSE STORE THE CHAR
        STA     IN,X            ; INTO THE INPUT BUFFER

        DEC     LINLEN          ; ONE LESS CHAR IN LINE
        INC     WRDLEN          ; ONE MORE IN WORD
        INC     SOURCE          ; POINT TO NEXT CHAR IN SOURCE
        JMP     READL           ; AND LOOP BACK

DOSIB:
        STA     IN              ; PUT THE BREAK INTO 1ST WORD SLOT
        DEC     LINLEN          ; ONE LESS CHAR IN LINE
        INC     WRDLEN          ; ONE MORE IN WORD BUFFER
        INC     SOURCE          ; POINT TO NEXT SOURCE CHAR

READL3:
        LDA     WRDLEN          ; ANY CHARS IN WORD YET?
        BEQ     READL           ; APPARENTLY NOT, SO LOOP BACK

        JSR     EFIND           ; GET ENTRY ADDR INTO [ENTRY]
        LDA     WRDLEN          ; GET ACTUAL LNGTH OF WORD
        LDY     #2              ; STORE IT IN "WORD LENGTH" SLOT
        STA     (ENTRY),Y       ; OF THE CURRENT ENTRY

        JSR     CONZST          ; CONVERT ASCII IN [IN] TO Z-STRING
        JSR     FINDW           ; AND LOOK IT UP IN VOCABULARY

        LDY     #1
        LDA     (ARG2),Y        ; FETCH THE # WORDS READ
        CLC
        ADC     #1              ; INCREMENT IT
        STA     (ARG2),Y        ; AND UPDATE

        JSR     EFIND           ; MAKE [ENTRY] POINT TO ENTRY

        LDY     #0
        STY     WRDLEN          ; CLEAR # CHARS IN WORD
        LDA     VALUE+HI        ; GET MSB OF VOCAB ENTRY ADDRESS
        STA     (ENTRY),Y       ; AND STORE IN 1ST SLOT OF ENTRY
        INY
        LDA     VALUE+LO        ; ALSO STORE LSB IN 2ND SLOT
        STA     (ENTRY),Y

        LDA     RESULT          ; UPDATE THE
        CLC                     ; RESULT TABLE POINTER
        ADC     #4              ; SO IT POINTS TO THE
        STA     RESULT          ; NEXT ENTRY

        JMP     READL           ; AND LOOP BACK

; -----------------------------------
; FIND BASE ADDR OF RESULT ENTRY SLOT
; -----------------------------------

EFIND:
        LDA     ARG2+LO         ; LSB OF RESULT TABLE BASE
        CLC
        ADC     RESULT          ; AND CURRENT POINTER
        STA     ENTRY+LO        ; SAVE IN [ENTRY]
        LDA     ARG2+HI         ; ALSO ADD MSB
        ADC     #0
        STA     ENTRY+HI
        RTS

; ----------
; FLUSH WORD
; ----------

FLUSHW:
        LDA     LINLEN          ; ANY CHARS LEFT IN LINE?
        BEQ     FLEX            ; NO, SCRAM

        LDY     SOURCE          ; GET CURRENT CHAR POINTER
        LDA     (ARG1),Y        ; AND GRAB A CHAR
        JSR     BREAK           ; IS IT A BREAK?
        BCS     FLEX            ; EXIT IF SO
        DEC     LINLEN          ; ELSE UPDATE CHAR COUNT
        INC     WRDLEN          ; AND WORD-CHAR COUNT
        INC     SOURCE          ; AND CHAR POINTER
        BNE     FLUSHW          ; AND LOOP BACK (ALWAYS)

FLEX:
        RTS

; ---------------------------------
; IS CHAR IN [A] ANY TYPE OF BREAK?
; ---------------------------------

BREAK:
        JSR     SIB             ; CHECK FOR A SIB FIRST
        BCS     FBRK            ; EXIT NOW IF MATCHED

; ELSE FALL THROUGH ...

; --------------------------------
; IS CHAR IN [A] A "NORMAL" BREAK?
; --------------------------------

NORM:
        LDX     #NBRKS-1        ; NUMBER OF "NORMAL" BREAKS
NBL:
        CMP     BRKTBL,X        ; MATCHED?
        BEQ     FBRK            ; YES, EXIT
        DEX
        BPL     NBL             ; NO, KEEP LOOKING
        CLC                     ; NO MATCH, CLEAR CARRY
        RTS                     ; AND RETURN

; ------------------
; NORMAL BREAK CHARS
; ------------------

BRKTBL:
        .BYTE   "!?,."          ; IN ORDER OF
        .BYTE   EOL             ; ASCENDING FREQUENCY
        .BYTE   SPACE           ; SPACE CHAR IS TESTED FIRST FOR SPEED

NBRKS           = *-BRKTBL      ; # NORMAL BREAKS

; ---------------------
; IS CHAR IN [A] A SIB?
; ---------------------

SIB:
        TAX                     ; SAVE TEST CHAR
        LDY     #0              ; 1ST BYTE IN VOCAB TABLE
        LDA     (VOCAB),Y       ; HAS # SIBS
        TAY                     ; USE AS AN INDEX
        TXA                     ; RESTORE TEST CHAR
SBL:
        CMP     (VOCAB),Y       ; MATCHED?
        BEQ     FBRK            ; YES, REPORT IT
        DEY
        BNE     SBL             ; ELSE KEEP LOOPING
        CLC                     ; NO MATCH, SO
        RTS                     ; EXIT WITH CARRY CLEAR

FBRK:
        SEC                     ; EXIT WITH CARRY SET
        RTS                     ; IF MATCHED WITH A BREAK CHAR

; -----------------
; VOCABULARY SEARCH
; -----------------

; ENTRY: 4-BYTE TARGET Z-WORD IN [OUT]
; EXIT: ABS ENTRY ADDRESS IN [VALUE] IF FOUND;
;       OTHERWISE [VALUE] = 0

FINDW:
        LDY     #0              ; GET # SIBS
        LDA     (VOCAB),Y       ; IN VOCAB TABLE
        CLC                     ; INCREMENT IT
        ADC     #1              ; FOR PROPER ALIGNMENT
        ADC     VOCAB+LO        ; NOW ADD THE BASE ADDR OF THE TABLE
        STA     VALUE+LO        ; TO GET THE ACTUAL BASE ADDR
        LDA     VOCAB+HI        ; OF THE VOCAB ENTRIES
        ADC     #0              ; WHICH IS SAVED
        STA     VALUE+HI        ; IN [VALUE]

        LDA     (VALUE),Y       ; GET # BYTES PER ENTRY ([Y] = 0)
        STA     ESIZE           ; SAVE IT HERE

        JSR     INCVAL          ; POINT TO NEXT BYTE
        LDA     (VALUE),Y       ; GET # ENTRIES IN TABLE (MSB)
        STA     NENTS+HI        ; AND STUFF IT IN [NENTS]

        JSR     INCVAL          ; NEXT BYTE
        LDA     (VALUE),Y       ; DON'T FORGET THE LSB!
        STA     NENTS+LO

        JSR     INCVAL          ; [VALUE] NOW POINTS TO 1ST ENTRY

; BEGIN THE SEARCH!

FWL1:
        LDY     #0
        LDA     (VALUE),Y       ; GET 1ST BYTE OF ENTRY
        CMP     OUT             ; MATCHED 1ST BYTE OF TARGET?
        BNE     WNEXT           ; NO, SKIP TO NEXT WORD

        INY
        LDA     (VALUE),Y
        CMP     OUT+1           ; 2ND BYTE MATCHED?
        BNE     WNEXT           ; NOPE

        INY
        LDA     (VALUE),Y
        CMP     OUT+2           ; 3RD BYTE?
        BNE     WNEXT           ; SORRY ...

        INY
        LDA     (VALUE),Y
        CMP     OUT+3           ; LAST BYTE
        BEQ     FWSUCC          ; FOUND IT!

WNEXT:
        LDA     ESIZE           ; GET ENTRY SIZE
        CLC                     ; AND ADD IT TO ENTRY ADDRESS
        ADC     VALUE+LO        ; TO MAKE [VALUE]
        STA     VALUE+LO        ; POINT TO THE NEXT ENTRY
        BCC     WNX
        INC     VALUE+HI

WNX:
        LDA     NENTS+LO        ; DECREMENT THE
        SEC                     ; ENTRY COUNTER
        SBC     #1
        STA     NENTS+LO
        BCS     WNX1
        DEC     NENTS+HI

WNX1:
        ORA     NENTS+HI        ; KEEP SEARCHING
        BNE     FWL1            ; UNTIL COUNT IS ZERO

        STA     VALUE+LO
        STA     VALUE+HI
        RTS                     ; THEN RETURN WITH [VALUE] = 0

; ENTRY MATCHED!

FWSUCC:
        LDA     VALUE+HI        ; CONVERT ABSOLUTE ENTRY ADDRESS
        SEC                     ; IN [VALUE]
        SBC     ZCODE           ; TO RELATIVE Z-ADDRESS
        STA     VALUE+HI        ; LSB NEEDN'T CHANGE
        RTS
