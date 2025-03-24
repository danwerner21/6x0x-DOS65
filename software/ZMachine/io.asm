;        PAGE
;        SBTTL   "--- GAME I/O: DOS/65 ---"

; --------------
; INTERNAL ERROR
; --------------

; ENTRY: ERROR CODE IN [A]

ERRM:
        .BYTE   "Internal error "
ENUMB:
        .BYTE   "00."
ERRML           = *-ERRM

ZERROR:
        LDY     #1              ; CONVERT ERROR BYTE IN [A]
ECON:
        JSR     DIV10           ; TO ASCII DECIMAL IN [ENUMB]
        ORA     #'0'
        STA     ENUMB,Y
        TXA
        DEY
        BPL     ECON

        JSR     ZCRLF           ; CLEAR BUFFER
        LDA     #0
        STA     SCRIPT          ; DISABLE SCRIPTING

        LDX     #<ERRM
        LDA     #>ERRM
        LDY     #ERRML
        JSR     DLINE

; FALL THROUGH ...

; ----
; QUIT
; ----

ZQUIT:
        JSR     ZCRLF           ; FLUSH BUFFER

        LDX     #<TOQ
        LDA     #>TOQ
        LDY     #TOQL
        JSR     DLINE           ; "END OF STORY"
        LDX     #0
        JMP     PEM

TOQ:
        .BYTE   "End of session."
        .BYTE   EOL
TOQL            = *-TOQ

; -------
; RESTART
; -------

ZSTART:
        JSR     ZCRLF

        LDA     ZBEGIN+ZSCRIP+1 ; SAVE SCRIPT STATE
        AND     #%00000001      ; FOR LATER
        STA     SFLAG           ; (BM 5/14/85)

        JMP     WARM1           ; AND DO A WARMSTART

; --------------------
; PRINT VERSION NUMBER
; --------------------

VERS:
        .BYTE   "DOS/65 Version F"
        .BYTE   EOL
VERSL           = *-VERS

VERNUM:
        JSR     ZCRLF

        LDX     #<VERS
        LDA     #>VERS
        LDY     #VERSL
        JMP     DLINE

; --------------------------
; RETURN TOP RAM PAGE IN [A]
; --------------------------

MEMTOP:
        LDA     #$CF            ; IT'S A GIVEN
        RTS

; --------------------------------
; RETURN RANDOM BYTES IN [A] & [X]
; --------------------------------
RANDOM:
        CLC
        LDA     #$41
        ADC     state+0
        STA     state+0
        ADC     state+1
        STA     state+1
        ADC     state+2
        STA     state+2
        ADC     state+3
        STA     state+3
        ADC     state+4
        ASL
        ADC     state+3
        STA     state+4
        EOR     state+2
        TAX
        CLC
        LDA     #$41
        ADC     state1+0
        STA     state1+0
        ADC     state1+1
        STA     state1+1
        ADC     state1+2
        STA     state1+2
        ADC     state1+3
        STA     state1+3
        ADC     state1+4
        ASL
        ADC     state1+3
        STA     state1+4
        EOR     state1+2

        STA     state1
        INC     state
        INC     state1+1
        STX     state+1
        INC     state1+2
        INC     state1+3
        INC     state+3
        INC     state+4
        RTS
; RANDOM SEEDS
state:
        .BYTE   $00,$00,$00,$00,$00
state1:
        .BYTE   $00,$00,$00,$00,$00



; -------------------
; Z-PRINT A CHARACTER
; -------------------

; ENTRY: ASCII CHAR IN [A]

COUT:
        CMP     #EOL            ; IF EOL,
        BEQ     ZCRLF           ; DO IT!
        CMP     #SPACE          ; IGNORE ALL OTHER
        BCC     CEX             ; CONTROLS

        LDX     LENGTH          ; ELSE GET LINE POINTER
        STA     LBUFF,X         ; ADD CHAR TO BUFFER
        CPX     #XSIZE          ; END OF LINE?
        BCS     FLUSH           ; YES, FLUSH THE LINE
        INC     LENGTH          ; ELSE UPDATE POINTER

CEX:
        RTS

; -------------------
; FLUSH OUTPUT BUFFER
; -------------------

; ENTRY: LENGTH OF BUFFER IN [X]

FLUSH:
        LDA     #SPACE

FL0:
        CMP     LBUFF,X         ; FIND LAST SPACE CHAR
        BEQ     FL1             ; IN THE LINE
        DEX
        BNE     FL0             ; IF NONE FOUND,
        LDX     #XSIZE          ; FLUSH ENTIRE LINE

FL1:
        STX     OLDLEN          ; SAVE OLD LINE POS HERE
        STX     LENGTH          ; MAKE IT THE NEW LINE LENGTH

        JSR     ZCRLF           ; PRINT LINE UP TO LAST SPACE

; START NEW LINE WITH REMAINDER OF OLD

        LDX     OLDLEN          ; GET OLD LINE POS
        LDY     #0              ; START NEW LINE AT BEGINNING
FL2:
        INX
        CPX     #XSIZE          ; CONTINUE IF
        BCC     FL3             ; INSIDE OR
        BEQ     FL3             ; AT END OF LINE
        STY     LENGTH          ; ELSE SET NEW LINE LENGTH
        RTS

FL3:
        LDA     LBUFF,X         ; GET CHAR FROM OLD LINE
        STA     LBUFF,Y         ; MOVE TO START OF NEW LINE
        INY                     ; UPDATE LENGTH OF NEW LINE
        BNE     FL2

; ---------------
; CARRIAGE RETURN
; ---------------

ZCRLF:
        INC     LINCNT          ; NEW LINE GOING OUT
        LDA     LINCNT          ; IS IT TIME TO
        CMP     LMAX            ; PRINT "MORE" YET?
        BCC     CR1             ; NO, CONTINUE

; SCREEN FULL; PRINT "MORE"

        JSR     ZUSL            ; UPDATE STATUS LINE

        LDA     #0
        STA     LINCNT          ; RESET LINE COUNTER
        JSR     BOLD
        LDX     #<MORE
        LDA     #>MORE
        LDY     #MOREL
        JSR     DLINE           ; PRINT "MORE" DIRECTLY
        JSR     UNBOLD
ZCR0:
        JSR     GETKEY          ; GET KEYPRESS
        LDX     #<MCLR
        LDA     #>MCLR
        LDY     #MCLRL
        JSR     DLINE           ; RUB OUT "MORE"

CR1:
        LDX     LENGTH
        LDA     #EOL            ; INSTALL EOL AT
        STA     LBUFF,X         ; END OF CURRENT LINE
        INC     LENGTH          ; UPDATE LINE LENGTH

LINOUT:
        LDY     LENGTH          ; IF BUFFER EMPTY,
        BEQ     LINEX           ; DON'T PRINT ANYTHING

        STY     PRLEN           ; SAVE LENGTH HERE FOR "PPRINT"
        LDX     #0              ; SEND CONTENTS OF [LBUFF]
LOUT:
        LDA     LBUFF,X         ; TO SCREEN
        JSR     CHAR
        INX
        DEY
        BNE     LOUT

        JSR     PPRINT          ; PRINT [LBUFF] IF ENABLED

LINEX:
        LDA     #0              ; RESET LINE LENGTH
        STA     LENGTH          ; TO ZERO
        RTS                     ; AND RETURN

MORE:
        .BYTE   "[MORE]"
MOREL           = *-MORE

MCLR:
        .BYTE   08,08,08,08,08,08,"      ",08,08,08,08,08,08
MCLRL           = *-MCLR

; ----------------------
; UPDATE THE STATUS LINE
; ----------------------

SCORE:
        .BYTE   "Score: "
SCOREL          = *-SCORE

CLOCK:
        .BYTE   "Time: "
CLOCKL          = *-CLOCK


ZUSL:
        LDA     LENGTH          ; SAVE ALL
        PHA                     ; STRING-PRINTING
        LDA     MPCH            ; VARIABLES
        PHA
        LDA     MPCM
        PHA
        LDA     MPCL
        PHA
        LDA     TSET
        PHA
        LDA     PSET
        PHA
        LDA     ZWORD+HI
        PHA
        LDA     ZWORD+LO
        PHA
        LDA     ZFLAG
        PHA
        LDA     DIGITS
        PHA

        LDX     #XSIZE
USL0:
        LDA     LBUFF,X         ; MOVE CONTENTS OF [LBUFF]
        STA     BUFSAV,X        ; TO [BUFSAV]
        LDA     #SPACE          ; CLEAR
        STA     LBUFF,X         ; [LBUFF] WITH SPACES
        DEX
        BPL     USL0

        LDA     #0
        STA     LENGTH          ; RESET LINE LENGTH
        STA     SCRIPT          ; DISABLE SCRIPTING
        JSR     REVERSE
        LDX     #0
        LDy     #0
        JSR     PLOT
;
; PRINT ROOM DESCRIPTION
;
        LDA     #16             ; GLOBAL VAR #16 (ROOM ID)
        JSR     GETVRG          ; GET IT INTO [VALUE]
        LDA     VALUE+LO
        JSR     PRNTDC          ; PRINT SHORT ROOM DESCRIPTION

        LDA     #24             ; MOVE LINE INDEX UP
        STA     LENGTH          ; TO TIME/SCORE POSITION

        LDA     #17             ; GLOBAL VAR #17 (SCORE/HOURS)
        JSR     GETVRG          ; GET IT INTO [VALUE]

        LDA     TIMEFL          ; GET MODE FLAG
        BNE     DOTIME          ; USE TIME MODE IF NON-ZERO
;
; PRINT "SCORE"
;
        LDA     #'S'
        JSR     COUT
        LDA     #'c'
        JSR     COUT
        LDA     #'o'
        JSR     COUT
        LDA     #'r'
        JSR     COUT
        LDA     #'e'
        JSR     COUT
        LDA     #':'
        JSR     COUT
        LDA     #SPACE
        JSR     COUT

        LDA     VALUE+LO        ; MOVE SCORE VALUE
        STA     QUOT+LO         ; INTO [QUOT]
        LDA     VALUE+HI        ; FOR PRINTING
        STA     QUOT+HI
        JSR     NUMBER          ; PRINT SCORE VALUE IN DECIMAL
;
        LDA     #'/'            ; PRINT A SLASH
        BNE     MOVMIN          ; BRANCH ALWAYS

; PRINT "TIME"

DOTIME:
        LDA     #'T'
        JSR     COUT
        LDA     #'i'
        JSR     COUT
        LDA     #'m'
        JSR     COUT
        LDA     #'e'
        JSR     COUT
        LDA     #':'
        JSR     COUT
        LDA     #SPACE
        JSR     COUT
;
        LDA     VALUE+LO        ; 00 IS REALLY 24
        BNE     DT0
        LDA     #24
DT0:
        CMP     #13             ; IS HOURS > 12,
        BCC     DT1
        SBC     #12             ; CONVERT TO 1-12
DT1:
        STA     QUOT+LO         ; MOVE FOR PRINTING
        LDA     #0
        STA     QUOT+HI         ; CLEAR MSB
        JSR     NUMBER

        LDA     #':'            ; COLON
MOVMIN:
        JSR     COUT            ; PRINT SLASH OR COLON

        LDA     #18             ; GLOBAL VAR #18 (MOVES/MINUTES)
        JSR     GETVRG          ; GET IT INTO [VALUE]
        LDA     VALUE+LO        ; MOVE TO [QUOT]
        STA     QUOT+LO         ; FOR EVENTUAL PRINTING
        LDA     VALUE+HI
        STA     QUOT+HI

        LDA     TIMEFL          ; WHICH MODE?
        BNE     DOMINS          ; TIME IF NZ
;
; PRINT NUMBER OF MOVES
;
        JSR     NUMBER          ; SHOW # MOVES
        JMP     STATEX          ; ALL DONE
;
; PRINT MINUTES
;
DOMINS:
        LDA     VALUE+LO        ; CHECK MINUTES
        CMP     #10             ; IF MORE THAN TEN
        BCS     DOM0            ; CONTINUE

        LDA     #'0'            ; ELSE PRINT A
        JSR     COUT            ; PADDING "0" FIRST

DOM0:
        JSR     NUMBER          ; SHOW MINUTES

        LDA     #SPACE
        JSR     COUT            ; SEPARATE THINGS
;
        LDA     #17             ; CHECK "HOURS" AGAIN
        JSR     GETVRG
        LDA     VALUE+LO
        CMP     #12             ; PAST NOON?
        BCS     DOPM            ; YES, PRINT "PM"

        LDA     #'A'            ; ELSE PRINT "AM"
        BNE     DOXM            ; BRANCH ALWAYS

DOPM:
        LDA     #'P'

DOXM:
        JSR     COUT
        LDA     #'M'
        JSR     COUT
;
; STATUS LINE READY
;
STATEX:
        LDA     #XSIZE          ; PRINT THE ENTIRE
        STA     LENGTH          ; STATUS LINE
        JSR     CR1

        LDX     #XSIZE          ; RESTORE OLD [LBUFF]
USLX:
        LDA     BUFSAV,X
        STA     LBUFF,X
        DEX
        BPL     USLX

        PLA                     ; RESTORE ALL
        STA     DIGITS          ; SAVED VARIABLES
        PLA
        STA     ZFLAG
        PLA
        STA     ZWORD+LO
        PLA
        STA     ZWORD+HI
        PLA
        STA     PSET
        PLA
        STA     TSET
        PLA
        STA     MPCL
        PLA
        STA     MPCM
        PLA
        STA     MPCH
        PLA
        STA     LENGTH

        LDX     #0            ; RESTORE CURSOR
        LDY     #YSIZE
        CLC
        JSR     PLOT
;
        LDX     #$FF
        STX     SCRIPT          ; RE-ENABLE SCRIPTING
        INX                     ; = 0
        STX     MPCFLG          ; INVALIDATE [MPC]
        INX                     ; = 1
        JSR     UNBOLD
        RTS
