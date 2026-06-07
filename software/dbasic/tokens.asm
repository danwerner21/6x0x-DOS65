; token values needed for BASIC

; primary command tokens (can start a statement)

TK_END          = $80           ; END token
TK_FOR          = TK_END+1      ; FOR token
TK_NEXT         = TK_FOR+1      ; NEXT token
TK_DATA         = TK_NEXT+1     ; DATA token
TK_INPUT        = TK_DATA+1     ; INPUT token
TK_DIM          = TK_INPUT+1    ; DIM token
TK_DIR          = TK_DIM+1      ; DIR token
TK_READ         = TK_DIR+1      ; READ token
TK_LET          = TK_READ+1     ; LET token
TK_DEC          = TK_LET+1      ; DEC token
TK_GOTO         = TK_DEC+1      ; GOTO token
TK_RUN          = TK_GOTO+1     ; RUN token
TK_IF           = TK_RUN+1      ; IF token
TK_RESTORE      = TK_IF+1       ; RESTORE token
TK_GOSUB        = TK_RESTORE+1  ; GOSUB token
TK_RETURN       = TK_GOSUB+1    ; RETURN token
TK_REM          = TK_RETURN+1   ; REM token
TK_STOP         = TK_REM+1      ; STOP token
TK_ON           = TK_STOP+1     ; ON token
TK_NULL         = TK_ON+1       ; NULL token
TK_INC          = TK_NULL+1     ; INC token
TK_WAIT         = TK_INC+1      ; WAIT token
TK_LOAD         = TK_WAIT+1     ; LOAD token
TK_SAVE         = TK_LOAD+1     ; SAVE token
TK_DEF          = TK_SAVE+1     ; DEF token
TK_PATTERN      = TK_DEF+1      ; PATTERN token
TK_PLOT         = TK_PATTERN+1  ; PLOT token
TK_POKE         = TK_PLOT+1     ; POKE token
TK_SPOKE        = TK_POKE+1     ; SPOKE token
TK_DOKE         = TK_SPOKE+1    ; DOKE token
TK_CALL         = TK_DOKE+1     ; CALL token
TK_DO           = TK_CALL+1     ; DO token
TK_LOOP         = TK_DO+1       ; LOOP token
TK_PRINT        = TK_LOOP+1     ; PRINT token
TK_CONT         = TK_PRINT+1    ; CONT token
TK_LINE         = TK_CONT+1     ; LINE token
TK_LIST         = TK_LINE+1     ; LIST token
TK_CLEAR        = TK_LIST+1     ; CLEAR token
TK_NEW          = TK_CLEAR+1    ; NEW token
TK_WIDTH        = TK_NEW+1      ; WIDTH token
TK_GET          = TK_WIDTH+1    ; GET token
TK_SWAP         = TK_GET+1      ; SWAP token
TK_BITSET       = TK_SWAP+1     ; BITSET token
TK_BITCLR       = TK_BITSET+1   ; BITCLR token
TK_KILL         = TK_BITCLR+1   ; KILL token
TK_SCREEN       = TK_KILL+1     ; SCREEN token
TK_SCRCLR       = TK_SCREEN+1   ; SCRCLR token
TK_COLOR        = TK_SCRCLR+1   ; COLOR token
TK_SOUND        = TK_COLOR+1    ; SOUND token
TK_NOISE        = TK_SOUND+1    ; NOISE token
TK_TONE         = TK_NOISE+1    ; TONE token
TK_VOLUME       = TK_TONE+1     ; VOLUME token
TK_VOICE        = TK_VOLUME+1   ; VOICE token
TK_LOCATE       = TK_VOICE+1    ; LOCATE token
TK_OPEN         = TK_LOCATE+1   ; OPEN token
TK_CLOSE        = TK_OPEN+1     ; CLOSE token
TK_FILEREAD     = TK_CLOSE+1    ; FILEREAD token
TK_FILEWRITE    = TK_FILEREAD+1 ; FILEWRITE token
TK_LPRINT       = TK_FILEWRITE+1; LPRINT token
TK_LLIST        = TK_LPRINT+1   ; LLIST token
TK_DELETE       = TK_LLIST+1    ; DELETE token
TK_RENAME       = TK_DELETE+1   ; RENAME token


; secondary command tokens, can't start a statement

TK_TAB          = TK_RENAME+1   ; TAB token
TK_ELSE         = TK_TAB+1      ; ELSE token
TK_TO           = TK_ELSE+1     ; TO token
TK_FN           = TK_TO+1       ; FN token
TK_SPC          = TK_FN+1       ; SPC token
TK_THEN         = TK_SPC+1      ; THEN token
TK_NOT          = TK_THEN+1     ; NOT token
TK_STEP         = TK_NOT+1      ; STEP token
TK_UNTIL        = TK_STEP+1     ; UNTIL token
TK_WHILE        = TK_UNTIL+1    ; WHILE token
TK_OFF          = TK_WHILE+1    ; OFF token

; opperator tokens

TK_PLUS         = TK_OFF+1      ; + token
TK_MINUS        = TK_PLUS+1     ; - token
TK_MUL          = TK_MINUS+1    ; * token
TK_DIV          = TK_MUL+1      ; / token
TK_POWER        = TK_DIV+1      ; ^ token
TK_AND          = TK_POWER+1    ; AND token
TK_EOR          = TK_AND+1      ; EOR token
TK_OR           = TK_EOR+1      ; OR token
TK_RSHIFT       = TK_OR+1       ; RSHIFT token
TK_LSHIFT       = TK_RSHIFT+1   ; LSHIFT token
TK_GT           = TK_LSHIFT+1   ; > token
TK_EQUAL        = TK_GT+1       ; = token
TK_LT           = TK_EQUAL+1    ; < token

; functions tokens

TK_SGN          = TK_LT+1       ; SGN token
TK_INT          = TK_SGN+1      ; INT token
TK_ABS          = TK_INT+1      ; ABS token
TK_USR          = TK_ABS+1      ; USR token
TK_FRE          = TK_USR+1      ; FRE token
TK_POS          = TK_FRE+1      ; POS token
TK_SQR          = TK_POS+1      ; SQR token
TK_RND          = TK_SQR+1      ; RND token
TK_LOG          = TK_RND+1      ; LOG token
TK_EXP          = TK_LOG+1      ; EXP token
TK_COS          = TK_EXP+1      ; COS token
TK_SIN          = TK_COS+1      ; SIN token
TK_TAN          = TK_SIN+1      ; TAN token
TK_ATN          = TK_TAN+1      ; ATN token
TK_PEEK         = TK_ATN+1      ; PEEK token
TK_SPEEK        = TK_PEEK+1     ; SPEEK token
TK_DEEK         = TK_SPEEK+1    ; DEEK token
TK_SADD         = TK_DEEK+1     ; SADD token
TK_LEN          = TK_SADD+1     ; LEN token
TK_STRS         = TK_LEN+1      ; STR$ token
TK_VAL          = TK_STRS+1     ; VAL token
TK_ASC          = TK_VAL+1      ; ASC token
TK_UCASES       = TK_ASC+1      ; UCASE$ token
TK_LCASES       = TK_UCASES+1   ; LCASE$ token
TK_CHRS         = TK_LCASES+1   ; CHR$ token
TK_HEXS         = TK_CHRS+1     ; HEX$ token
TK_BINS         = TK_HEXS+1     ; BIN$ token
TK_BITTST       = TK_BINS+1     ; BITTST token
TK_MAX          = TK_BITTST+1   ; MAX token
TK_MIN          = TK_MAX+1      ; MIN token
TK_PI           = TK_MIN+1      ; PI token
TK_SECOND       = TK_PI+1       ; SECOND token
TK_MINUTE       = TK_SECOND+1   ; MINUTE token
TK_HOUR         = TK_MINUTE+1   ; HOUR token
TK_DAY          = TK_HOUR+1     ; DAY token
TK_MONTH        = TK_DAY+1      ; MONTH token
TK_YEAR         = TK_MONTH+1    ; YEAR token
TK_VPTR         = TK_YEAR+1     ; VARPTR token
TK_LEFTS        = TK_VPTR+1     ; LEFT$ token
TK_RIGHTS       = TK_LEFTS+1    ; RIGHT$ token
TK_MIDS         = TK_RIGHTS+1   ; MID$ token
TK_CON          = TK_MIDS+1     ; CON token

LAB_CTBL:
        .WORD   LAB_END-1       ; END
        .WORD   LAB_FOR-1       ; FOR
        .WORD   LAB_NEXT-1      ; NEXT
        .WORD   LAB_DATA-1      ; DATA
        .WORD   LAB_INPUT-1     ; INPUT
        .WORD   LAB_DIM-1       ; DIM
        .WORD   LAB_DIR-1       ; DIR
        .WORD   LAB_READ-1      ; READ
        .WORD   LAB_LET-1       ; LET
        .WORD   LAB_DEC-1       ; DEC			new command
        .WORD   LAB_GOTO-1      ; GOTO
        .WORD   LAB_RUN-1       ; RUN
        .WORD   LAB_IF-1        ; IF
        .WORD   LAB_RESTORE-1   ; RESTORE		modified command
        .WORD   LAB_GOSUB-1     ; GOSUB
        .WORD   LAB_RETURN-1    ; RETURN
        .WORD   LAB_REM-1       ; REM
        .WORD   LAB_STOP-1      ; STOP
        .WORD   LAB_ON-1        ; ON			modified command
        .WORD   LAB_NULL-1      ; NULL		        modified command
        .WORD   LAB_INC-1       ; INC			new command
        .WORD   LAB_WAIT-1      ; WAIT
        .WORD   V_LOAD-1        ; LOAD
        .WORD   V_SAVE-1        ; SAVE
        .WORD   LAB_DEF-1       ; DEF
        .WORD   LAB_PATTERN-1   ; PATTERN
        .WORD   LAB_PLOT-1      ; PLOT
        .WORD   LAB_POKE-1      ; POKE
        .WORD   LAB_SPOKE-1     ; SPOKE                 NEW COMMAND
        .WORD   LAB_DOKE-1      ; DOKE		        new command
        .WORD   LAB_CALL-1      ; CALL		        new command
        .WORD   LAB_DO-1        ; DO		        new command
        .WORD   LAB_LOOP-1      ; LOOP		        new command
        .WORD   LAB_PRINT-1     ; PRINT
        .WORD   LAB_CONT-1      ; CONT
        .WORD   LAB_LINE-1      ; LINE
        .WORD   LAB_LIST-1      ; LIST
        .WORD   LAB_CLEAR-1     ; CLEAR
        .WORD   LAB_NEW-1       ; NEW
        .WORD   LAB_WDTH-1      ; WIDTH		        new command
        .WORD   LAB_GET-1       ; GET			new command
        .WORD   LAB_SWAP-1      ; SWAP		        new command
        .WORD   LAB_BITSET-1    ; BITSET		new command
        .WORD   LAB_BITCLR-1    ; BITCLR		new command
        .WORD   LAB_KILL-1      ; KILL			new command
        .WORD   LAB_SCREEN-1    ; SCREEN		new command
        .WORD   LAB_SCRCLR-1    ; SCRCLR		new command
        .WORD   LAB_COLOR-1     ; COLOR 		new command
        .WORD   LAB_SOUND-1     ; SOUND		        new command
        .WORD   LAB_NOISE-1     ; NOISE		        new command
        .WORD   LAB_TONE-1      ; TONE		        new command
        .WORD   LAB_VOLUME-1    ; VOLUME		new command
        .WORD   LAB_VOICE-1     ; VOICE		        new command
        .WORD   LAB_LOCATE-1    ; LOCATE		new command
        .WORD   V_OPEN-1        ; OPEN  		new command
        .WORD   V_CLOSE-1       ; CLOSE 		new command
        .WORD   V_FILEREAD-1    ; FILEREAD 		new command
        .WORD   V_FILEWRITE-1   ; FILEWRITE 		new command
        .WORD   V_LPRINT-1      ; LPRINT 		new command
        .WORD   V_LLIST-1       ; LLIST 		new command
        .WORD   V_DELETE-1      ; DELETE 		new command
        .WORD   V_RENAME-1      ; RENAME 		new command

; function pre process routine table

LAB_FTPL:
LAB_FTPM        = LAB_FTPL+$01
        .WORD   LAB_PPFN-1      ; SGN(n)	process numeric expression in ()
        .WORD   LAB_PPFN-1      ; INT(n)		"
        .WORD   LAB_PPFN-1      ; ABS(n)		"
        .WORD   LAB_EVEZ-1      ; USR(x)	process any expression
        .WORD   LAB_1BF7-1      ; FRE(x)		"
        .WORD   LAB_1BF7-1      ; POS(x)		"
        .WORD   LAB_PPFN-1      ; SQR(n)	process numeric expression in ()
        .WORD   LAB_PPFN-1      ; RND(n)		"
        .WORD   LAB_PPFN-1      ; LOG(n)		"
        .WORD   LAB_PPFN-1      ; EXP(n)		"
        .WORD   LAB_PPFN-1      ; COS(n)		"
        .WORD   LAB_PPFN-1      ; SIN(n)		"
        .WORD   LAB_PPFN-1      ; TAN(n)		"
        .WORD   LAB_PPFN-1      ; ATN(n)		"
        .WORD   LAB_PPFN-1      ; PEEK(n)		"
        .WORD   LAB_PPFN-1      ; SPEEK(n)		"
        .WORD   LAB_PPFN-1      ; DEEK(n)		"
        .WORD   $0000           ; SADD()	none
        .WORD   LAB_PPFS-1      ; LEN($)	process string expression in ()
        .WORD   LAB_PPFN-1      ; STR$(n)	process numeric expression in ()
        .WORD   LAB_PPFS-1      ; VAL($)	process string expression in ()
        .WORD   LAB_PPFS-1      ; ASC($)		"
        .WORD   LAB_PPFS-1      ; UCASE$($)		"
        .WORD   LAB_PPFS-1      ; LCASE$($)		"
        .WORD   LAB_PPFN-1      ; CHR$(n)	process numeric expression in ()
        .WORD   LAB_BHSS-1      ; HEX$(n)		"
        .WORD   LAB_BHSS-1      ; BIN$(n)		"
        .WORD   $0000           ; BITTST()	none
        .WORD   LAB_MMPP-1      ; MAX()	process numeric expression
        .WORD   LAB_MMPP-1      ; MIN()		"
        .WORD   LAB_PPBI-1      ; PI		advance pointer
        .WORD   LAB_PSECOND-1   ; SECOND	"
        .WORD   LAB_PMINUTE-1   ; MINUTE	"
        .WORD   LAB_PHOUR-1     ; HOUR  	"
        .WORD   LAB_PDAY-1      ; DAY   	"
        .WORD   LAB_PMONTH-1    ; MONTH	        "
        .WORD   LAB_PYEAR-1     ; YEAR	        "
        .WORD   $0000           ; VARPTR()	none
        .WORD   LAB_LRMS-1      ; LEFT$()	process string expression
        .WORD   LAB_LRMS-1      ; RIGHT$()	"
        .WORD   LAB_LRMS-1      ; MID$()	"
        .WORD   LAB_PPFN-1      ; CON()		"

; action addresses for functions

LAB_FTBL:
LAB_FTBM        = LAB_FTBL+$01
        .WORD   LAB_SGN-1       ; SGN()
        .WORD   LAB_INT-1       ; INT()
        .WORD   LAB_ABS-1       ; ABS()
        .WORD   LAB_USR-1       ; USR()
        .WORD   LAB_FRE-1       ; FRE()
        .WORD   LAB_POS-1       ; POS()
        .WORD   LAB_SQR-1       ; SQR()
        .WORD   LAB_RND-1       ; RND()		modified function
        .WORD   LAB_LOG-1       ; LOG()
        .WORD   LAB_EXP-1       ; EXP()
        .WORD   LAB_COS-1       ; COS()
        .WORD   LAB_SIN-1       ; SIN()
        .WORD   LAB_TAN-1       ; TAN()
        .WORD   LAB_ATN-1       ; ATN()
        .WORD   LAB_PEEK-1      ; PEEK()
        .WORD   LAB_SPEEK-1     ; SPEEK()               NEW FUNCTION
        .WORD   LAB_DEEK-1      ; DEEK()		new function
        .WORD   LAB_SADD-1      ; SADD()		new function
        .WORD   LAB_LENS-1      ; LEN()
        .WORD   LAB_STRS-1      ; STR$()
        .WORD   LAB_VAL-1       ; VAL()
        .WORD   LAB_ASC-1       ; ASC()
        .WORD   LAB_UCASE-1     ; UCASE$()		new function
        .WORD   LAB_LCASE-1     ; LCASE$()		new function
        .WORD   LAB_CHRS-1      ; CHR$()
        .WORD   LAB_HEXS-1      ; HEX$()		new function
        .WORD   LAB_BINS-1      ; BIN$()		new function
        .WORD   LAB_BTST-1      ; BITTST()		new function
        .WORD   LAB_MAX-1       ; MAX()		new function
        .WORD   LAB_MIN-1       ; MIN()		new function
        .WORD   LAB_PI-1        ; PI			new function
        .WORD   LAB_SECOND-1    ; SECOND	new function
        .WORD   LAB_MINUTE-1    ; MINUTE	new function
        .WORD   LAB_HOUR-1      ; HOUR	        new function
        .WORD   LAB_DAY-1       ; DAY	        new function
        .WORD   LAB_MONTH-1     ; MONTH	        new function
        .WORD   LAB_YEAR-1      ; YEAR	        new function
        .WORD   LAB_VARPTR-1    ; VARPTR()		new function
        .WORD   LAB_LEFT-1      ; LEFT$()
        .WORD   LAB_RIGHT-1     ; RIGHT$()
        .WORD   LAB_MIDS-1      ; MID$()
        .WORD   LAB_CON-1       ; CON()

; hierarchy and action addresses for operator

LAB_OPPT:
        .BYTE   $79             ; +
        .WORD   LAB_ADD-1
        .BYTE   $79             ; -
        .WORD   LAB_SUBTRACT-1
        .BYTE   $7B             ; *
        .WORD   LAB_MULTIPLY-1
        .BYTE   $7B             ; /
        .WORD   LAB_DIVIDE-1
        .BYTE   $7F             ; ^
        .WORD   LAB_POWER-1
        .BYTE   $50             ; AND
        .WORD   LAB_AND-1
        .BYTE   $46             ; EOR			new operator
        .WORD   LAB_EOR-1
        .BYTE   $46             ; OR
        .WORD   LAB_OR-1
        .BYTE   $56             ; >>			new operator
        .WORD   LAB_RSHIFT-1
        .BYTE   $56             ; <<			new operator
        .WORD   LAB_LSHIFT-1
        .BYTE   $7D             ; >
        .WORD   LAB_GTHAN-1
        .BYTE   $5A             ; =
        .WORD   LAB_EQUAL-1
        .BYTE   $64             ; <
        .WORD   LAB_LTHAN-1

; keywords start with ..
; this is the first character table and must be in alphabetic order

TAB_1STC:
        .BYTE   "*"
        .BYTE   "+"
        .BYTE   "-"
        .BYTE   "/"
        .BYTE   "<"
        .BYTE   "="
        .BYTE   ">"
        .BYTE   "?"
        .BYTE   "A"
        .BYTE   "B"
        .BYTE   "C"
        .BYTE   "D"
        .BYTE   "E"
        .BYTE   "F"
        .BYTE   "G"
        .BYTE   "H"
        .BYTE   "I"
        .BYTE   "K"
        .BYTE   "L"
        .BYTE   "M"
        .BYTE   "N"
        .BYTE   "O"
        .BYTE   "P"
        .BYTE   "R"
        .BYTE   "S"
        .BYTE   "T"
        .BYTE   "U"
        .BYTE   "V"
        .BYTE   "W"
        .BYTE   "^"
        .BYTE   $00             ; table terminator

; pointers to keyword tables

TAB_CHRT:
        .WORD   TAB_STAR        ; table for "*"
        .WORD   TAB_PLUS        ; table for "+"
        .WORD   TAB_MNUS        ; table for "-"
        .WORD   TAB_SLAS        ; table for "/"
        .WORD   TAB_LESS        ; table for "<"
        .WORD   TAB_EQUL        ; table for "="
        .WORD   TAB_MORE        ; table for ">"
        .WORD   TAB_QEST        ; table for "?"
        .WORD   TAB_ASCA        ; table for "A"
        .WORD   TAB_ASCB        ; table for "B"
        .WORD   TAB_ASCC        ; table for "C"
        .WORD   TAB_ASCD        ; table for "D"
        .WORD   TAB_ASCE        ; table for "E"
        .WORD   TAB_ASCF        ; table for "F"
        .WORD   TAB_ASCG        ; table for "G"
        .WORD   TAB_ASCH        ; table for "H"
        .WORD   TAB_ASCI        ; table for "I"
        .WORD   TAB_ASCK        ; table for "K"
        .WORD   TAB_ASCL        ; table for "L"
        .WORD   TAB_ASCM        ; table for "M"
        .WORD   TAB_ASCN        ; table for "N"
        .WORD   TAB_ASCO        ; table for "O"
        .WORD   TAB_ASCP        ; table for "P"
        .WORD   TAB_ASCR        ; table for "R"
        .WORD   TAB_ASCS        ; table for "S"
        .WORD   TAB_ASCT        ; table for "T"
        .WORD   TAB_ASCU        ; table for "U"
        .WORD   TAB_ASCV        ; table for "V"
        .WORD   TAB_ASCW        ; table for "W"
        .WORD   TAB_ASCY        ; table for "Y"
        .WORD   TAB_POWR        ; table for "^"

; tables for each start character, note if a longer keyword with the same start
; letters as a shorter one exists then it must come first, else the list is in
; alphabetical order as follows ..

; [keyword,token
; [keyword,token]]
; end marker (#$00)

TAB_STAR:
        .BYTE   TK_MUL,$00      ; *
TAB_PLUS:
        .BYTE   TK_PLUS,$00     ; +
TAB_MNUS:
        .BYTE   TK_MINUS,$00    ; -
TAB_SLAS:
        .BYTE   TK_DIV,$00      ; /
TAB_LESS:
LBB_LSHIFT:
        .BYTE   "<",TK_LSHIFT   ; <<	note - "<<" must come before "<"
        .BYTE   TK_LT           ; <
        .BYTE   $00
TAB_EQUL:
        .BYTE   TK_EQUAL,$00    ; =
TAB_MORE:
LBB_RSHIFT:
        .BYTE   ">",TK_RSHIFT   ; >>	note - ">>" must come before ">"
        .BYTE   TK_GT           ; >
        .BYTE   $00
TAB_QEST:
        .BYTE   TK_PRINT,$00    ; ?
TAB_ASCA:
LBB_ABS:
        .BYTE   "BS(",TK_ABS    ; ABS(
LBB_AND:
        .BYTE   "ND",TK_AND     ; AND
LBB_ASC:
        .BYTE   "SC(",TK_ASC    ; ASC(
LBB_ATN:
        .BYTE   "TN(",TK_ATN    ; ATN(
        .BYTE   $00
TAB_ASCB:
LBB_BINS:
        .BYTE   "IN$(",TK_BINS  ; BIN$(
LBB_BITCLR:
        .BYTE   "ITCLR",TK_BITCLR; BITCLR
LBB_BITSET:
        .BYTE   "ITSET",TK_BITSET; BITSET
LBB_BITTST:
        .BYTE   "ITTST(",TK_BITTST
; BITTST(
        .BYTE   $00
TAB_ASCC:
LBB_CALL:
        .BYTE   "ALL",TK_CALL   ; CALL
LBB_CHRS:
        .BYTE   "HR$(",TK_CHRS  ; CHR$(
LBB_CLEAR:
        .BYTE   "LEAR",TK_CLEAR ; CLEAR
LBB_CLOSE:
        .BYTE   "LOSE",TK_CLOSE ; CLOSE
LBB_COLOR:
        .BYTE   "OLOR",TK_COLOR ; COLOR
LBB_CON:
        .BYTE   "ON(",TK_CON    ;CON
LBB_CONT:
        .BYTE   "ONT",TK_CONT   ; CONT
LBB_COS:
        .BYTE   "OS(",TK_COS    ; COS(
        .BYTE   $00
TAB_ASCD:
LBB_DATA:
        .BYTE   "ATA",TK_DATA   ; DATA
LBB_DAY:
        .BYTE   "AY",TK_DAY     ; DAY
LBB_DEC:
        .BYTE   "EC",TK_DEC     ; DEC
LBB_DEEK:
        .BYTE   "EEK(",TK_DEEK  ; DEEK(
LBB_DEF:
        .BYTE   "EF",TK_DEF     ; DEF
LBB_DELETE:
        .BYTE   "ELETE",TK_DELETE; DELETE
LBB_DIM:
        .BYTE   "IM",TK_DIM     ; DIM
LBB_DIR:
        .BYTE   "IR",TK_DIR     ; DIR
LBB_DOKE:
        .BYTE   "OKE",TK_DOKE   ; DOKE note - "DOKE" must come before "DO"
LBB_DO:
        .BYTE   "O",TK_DO       ; DO
        .BYTE   $00
TAB_ASCE:
LBB_ELSE:
        .BYTE   "LSE",TK_ELSE   ; ELSE
LBB_END:
        .BYTE   "ND",TK_END     ; END
LBB_EOR:
        .BYTE   "OR",TK_EOR     ; EOR
LBB_EXP:
        .BYTE   "XP(",TK_EXP    ; EXP(
        .BYTE   $00
TAB_ASCF:
LBB_FILEREAD:
        .BYTE   "ILEREAD",TK_FILEREAD; FILEREAD
LBB_FILEWRITE:
        .BYTE   "ILEWRITE",TK_FILEWRITE; FILEWRITE
LBB_FN:
        .BYTE   "N",TK_FN       ; FN
LBB_FOR:
        .BYTE   "OR",TK_FOR     ; FOR
LBB_FRE:
        .BYTE   "RE(",TK_FRE    ; FRE(
        .BYTE   $00
TAB_ASCG:
LBB_GET:
        .BYTE   "ET",TK_GET     ; GET
LBB_GOSUB:
        .BYTE   "OSUB",TK_GOSUB ; GOSUB
LBB_GOTO:
        .BYTE   "OTO",TK_GOTO   ; GOTO
        .BYTE   $00
TAB_ASCH:
LBB_HEXS:
        .BYTE   "EX$(",TK_HEXS  ; HEX$(
LBB_HOUR:
        .BYTE   "OUR",TK_HOUR   ; HOUR
        .BYTE   $00
TAB_ASCI:
LBB_IF:
        .BYTE   "F",TK_IF       ; IF
LBB_INC:
        .BYTE   "NC",TK_INC     ; INC
LBB_INPUT:
        .BYTE   "NPUT",TK_INPUT ; INPUT
LBB_INT:
        .BYTE   "NT(",TK_INT    ; INT(
        .BYTE   $00
TAB_ASCK:
LBB_KILL:
        .BYTE   "ILL",TK_KILL   ; KILL
TAB_ASCL:
LBB_LCASES:
        .BYTE   "CASE$(",TK_LCASES
; LCASE$(
LBB_LEFTS:
        .BYTE   "EFT$(",TK_LEFTS; LEFT$(
LBB_LEN:
        .BYTE   "EN(",TK_LEN    ; LEN(
LBB_LET:
        .BYTE   "ET",TK_LET     ; LET
LBB_LINE:
        .BYTE   "INE",TK_LINE   ; LINE
LBB_LIST:
        .BYTE   "IST",TK_LIST   ; LIST
LBB_LLIST:
        .BYTE   "LIST",TK_LLIST ; LLIST
LBB_LOAD:
        .BYTE   "OAD",TK_LOAD   ; LOAD
LBB_LOCATE:
        .BYTE   "OCATE",TK_LOCATE; LOCATE
LBB_LOG:
        .BYTE   "OG(",TK_LOG    ; LOG(
LBB_LOOP:
        .BYTE   "OOP",TK_LOOP   ; LOOP
LBB_LPRINT:
        .BYTE   "PRINT",TK_LPRINT; LPRINT
        .BYTE   $00
TAB_ASCM:
LBB_MAX:
        .BYTE   "AX(",TK_MAX    ; MAX(
LBB_MIDS:
        .BYTE   "ID$(",TK_MIDS  ; MID$(
LBB_MIN:
        .BYTE   "IN(",TK_MIN    ; MIN(
LBB_MINUTE:
        .BYTE   "INUTE",TK_MINUTE; MINUTE
LBB_MONTH:
        .BYTE   "ONTH",TK_MONTH ; MONTH
        .BYTE   $00
TAB_ASCN:
LBB_NEW:
        .BYTE   "EW",TK_NEW     ; NEW
LBB_NEXT:
        .BYTE   "EXT",TK_NEXT   ; NEXT
LBB_NOISE:
        .BYTE   "OISE",TK_NOISE ; NOISE
LBB_NOT:
        .BYTE   "OT",TK_NOT     ; NOT
LBB_NULL:
        .BYTE   "ULL",TK_NULL   ; NULL
        .BYTE   $00
TAB_ASCO:
LBB_OFF:
        .BYTE   "FF",TK_OFF     ; OFF
LBB_ON:
        .BYTE   "N",TK_ON       ; ON
LBB_OPEN:
        .BYTE   "PEN",TK_OPEN   ; OPEN
LBB_OR:
        .BYTE   "R",TK_OR       ; OR
        .BYTE   $00
TAB_ASCP:
LBB_PATTERN:
        .BYTE   "ATTERN",TK_PATTERN; PATTERN
LBB_PEEK:
        .BYTE   "EEK(",TK_PEEK  ; PEEK(
LBB_PI:
        .BYTE   "I",TK_PI       ; PI
LBB_PLOT:
        .BYTE   "LOT",TK_PLOT   ; PLOT
LBB_POKE:
        .BYTE   "OKE",TK_POKE   ; POKE
LBB_POS:
        .BYTE   "OS(",TK_POS    ; POS(
LBB_PRINT:
        .BYTE   "RINT",TK_PRINT ; PRINT
        .BYTE   $00
TAB_ASCR:
LBB_READ:
        .BYTE   "EAD",TK_READ   ; READ
LBB_REM:
        .BYTE   "EM",TK_REM     ; REM
LBB_RENAME:
        .BYTE   "ENAME",TK_RENAME; RENAME
LBB_RESTORE:
        .BYTE   "ESTORE",TK_RESTORE
; RESTORE
LBB_RETURN:
        .BYTE   "ETURN",TK_RETURN; RETURN
LBB_RIGHTS:
        .BYTE   "IGHT$(",TK_RIGHTS
; RIGHT$(
LBB_RND:
        .BYTE   "ND(",TK_RND    ; RND(
LBB_RUN:
        .BYTE   "UN",TK_RUN     ; RUN
        .BYTE   $00
TAB_ASCS:
LBB_SADD:
        .BYTE   "ADD(",TK_SADD  ; SADD(
LBB_SAVE:
        .BYTE   "AVE",TK_SAVE   ; SAVE
LBB_SCRCLR:
        .BYTE   "CRCLR",TK_SCRCLR; SCRCLR
LBB_SCREEN:
        .BYTE   "CREEN",TK_SCREEN; SCREEN
LBB_SECOND:
        .BYTE   "ECOND",TK_SECOND; SECOND
LBB_SGN:
        .BYTE   "GN(",TK_SGN    ; SGN(
LBB_SIN:
        .BYTE   "IN(",TK_SIN    ; SIN(
LBB_SOUND:
        .BYTE   "OUND",TK_SOUND ; SOUND
LBB_SPC:
        .BYTE   "PC(",TK_SPC    ; SPC(
LBB_SPEEK:
        .BYTE   "PEEK(",TK_SPEEK; SPEEK(
LBB_SPOKE:
        .BYTE   "POKE",TK_SPOKE ; SPOKE
LBB_SQR:
        .BYTE   "QR(",TK_SQR    ; SQR(
LBB_STEP:
        .BYTE   "TEP",TK_STEP   ; STEP
LBB_STOP:
        .BYTE   "TOP",TK_STOP   ; STOP
LBB_STRS:
        .BYTE   "TR$(",TK_STRS  ; STR$(
LBB_SWAP:
        .BYTE   "WAP",TK_SWAP   ; SWAP
        .BYTE   $00
TAB_ASCT:
LBB_TAB:
        .BYTE   "AB(",TK_TAB    ; TAB(
LBB_TAN:
        .BYTE   "AN(",TK_TAN    ; TAN(
LBB_THEN:
        .BYTE   "HEN",TK_THEN   ; THEN
LBB_TONE:
        .BYTE   "ONE",TK_TONE   ; TONE
LBB_TO:
        .BYTE   "O",TK_TO       ; TO
        .BYTE   $00
TAB_ASCU:
LBB_UCASES:
        .BYTE   "CASE$(",TK_UCASES
; UCASE$(
LBB_UNTIL:
        .BYTE   "NTIL",TK_UNTIL ; UNTIL
LBB_USR:
        .BYTE   "SR(",TK_USR    ; USR(
        .BYTE   $00
TAB_ASCV:
LBB_VAL:
        .BYTE   "AL(",TK_VAL    ; VAL(
LBB_VPTR:
        .BYTE   "ARPTR(",TK_VPTR; VARPTR(
LBB_VOLUME:
        .BYTE   "OLUME",TK_VOLUME; VOLUME
LBB_VOICE:
        .BYTE   "OICE",TK_VOICE ; VOICE
        .BYTE   $00
TAB_ASCW:
LBB_WAIT:
        .BYTE   "AIT",TK_WAIT   ; WAIT
LBB_WHILE:
        .BYTE   "HILE",TK_WHILE ; WHILE
LBB_WIDTH:
        .BYTE   "IDTH",TK_WIDTH ; WIDTH
        .BYTE   $00
TAB_ASCY:
LBB_YEAR:
        .BYTE   "EAR",TK_YEAR   ; YEAR
        .BYTE   $00
TAB_POWR:
        .BYTE   TK_POWER,$00    ; ^

; new decode table for LIST
; Table is ..
; byte - keyword length, keyword first character
; word - pointer to rest of keyword from dictionary

; note if length is 1 then the pointer is ignored

LAB_KEYT:
        .BYTE   3,'E'
        .WORD   LBB_END         ; END
        .BYTE   3,'F'
        .WORD   LBB_FOR         ; FOR
        .BYTE   4,'N'
        .WORD   LBB_NEXT        ; NEXT
        .BYTE   4,'D'
        .WORD   LBB_DATA        ; DATA
        .BYTE   5,'I'
        .WORD   LBB_INPUT       ; INPUT
        .BYTE   3,'D'
        .WORD   LBB_DIM         ; DIM
        .BYTE   3,'D'
        .WORD   LBB_DIR         ; DIR
        .BYTE   4,'R'
        .WORD   LBB_READ        ; READ
        .BYTE   3,'L'
        .WORD   LBB_LET         ; LET
        .BYTE   3,'D'
        .WORD   LBB_DEC         ; DEC
        .BYTE   4,'G'
        .WORD   LBB_GOTO        ; GOTO
        .BYTE   3,'R'
        .WORD   LBB_RUN         ; RUN
        .BYTE   2,'I'
        .WORD   LBB_IF          ; IF
        .BYTE   7,'R'
        .WORD   LBB_RESTORE     ; RESTORE
        .BYTE   5,'G'
        .WORD   LBB_GOSUB       ; GOSUB
        .BYTE   6,'R'
        .WORD   LBB_RETURN      ; RETURN
        .BYTE   3,'R'
        .WORD   LBB_REM         ; REM
        .BYTE   4,'S'
        .WORD   LBB_STOP        ; STOP
        .BYTE   2,'O'
        .WORD   LBB_ON          ; ON
        .BYTE   4,'N'
        .WORD   LBB_NULL        ; NULL
        .BYTE   3,'I'
        .WORD   LBB_INC         ; INC
        .BYTE   4,'W'
        .WORD   LBB_WAIT        ; WAIT
        .BYTE   4,'L'
        .WORD   LBB_LOAD        ; LOAD
        .BYTE   4,'S'
        .WORD   LBB_SAVE        ; SAVE
        .BYTE   3,'D'
        .WORD   LBB_DEF         ; DEF
        .BYTE   7,'P'
        .WORD   LBB_PATTERN     ; PATTERN
        .BYTE   4,'P'
        .WORD   LBB_PLOT        ; PLOT
        .BYTE   4,'P'
        .WORD   LBB_POKE        ; POKE
        .BYTE   5,'S'
        .WORD   LBB_SPOKE       ; SPOKE
        .BYTE   4,'D'
        .WORD   LBB_DOKE        ; DOKE
        .BYTE   4,'C'
        .WORD   LBB_CALL        ; CALL
        .BYTE   2,'D'
        .WORD   LBB_DO          ; DO
        .BYTE   4,'L'
        .WORD   LBB_LOOP        ; LOOP
        .BYTE   5,'P'
        .WORD   LBB_PRINT       ; PRINT
        .BYTE   4,'C'
        .WORD   LBB_CONT        ; CONT
        .BYTE   4,'L'
        .WORD   LBB_LINE        ; LINE
        .BYTE   4,'L'
        .WORD   LBB_LIST        ; LIST
        .BYTE   5,'C'
        .WORD   LBB_CLEAR       ; CLEAR
        .BYTE   3,'N'
        .WORD   LBB_NEW         ; NEW
        .BYTE   5,'W'
        .WORD   LBB_WIDTH       ; WIDTH
        .BYTE   3,'G'
        .WORD   LBB_GET         ; GET
        .BYTE   4,'S'
        .WORD   LBB_SWAP        ; SWAP
        .BYTE   6,'B'
        .WORD   LBB_BITSET      ; BITSET
        .BYTE   6,'B'
        .WORD   LBB_BITCLR      ; BITCLR
        .BYTE   4,'K'
        .WORD   LBB_KILL        ; KILL
        .BYTE   6,'S'
        .WORD   LBB_SCREEN      ; SCREEN
        .BYTE   6,'S'
        .WORD   LBB_SCRCLR      ; SCRCLR
        .BYTE   5,'C'
        .WORD   LBB_COLOR       ; COLOR
        .BYTE   5,'S'
        .WORD   LAB_SOUND-1     ; SOUND
        .BYTE   5,'N'
        .WORD   LAB_NOISE-1     ; NOISE
        .BYTE   4,'T'
        .WORD   LAB_TONE-1      ; TONE
        .BYTE   6,'V'
        .WORD   LAB_VOLUME-1    ; VOLUME
        .BYTE   5,'V'
        .WORD   LAB_VOICE-1     ; VOICE
        .BYTE   6,'L'
        .WORD   LBB_LOCATE      ; LOCATE
        .BYTE   4,'O'
        .WORD   LBB_OPEN        ; OPEN
        .BYTE   5,'C'
        .WORD   LBB_CLOSE       ; CLOSE
        .BYTE   8,'F'
        .WORD   LBB_FILEREAD    ; FILEREAD
        .BYTE   9,'F'
        .WORD   LBB_FILEWRITE   ; FILEWRITE
        .BYTE   6,'L'
        .WORD   LBB_LPRINT      ; LPRINT
        .BYTE   5,'L'
        .WORD   LBB_LLIST       ; LLIST
        .BYTE   6,'D'
        .WORD   LBB_DELETE      ; DELETE
        .BYTE   6,'R'
        .WORD   LBB_RENAME      ; RENAME


; secondary commands (can't start a statement)

        .BYTE   4,'T'
        .WORD   LBB_TAB         ; TAB
        .BYTE   4,'E'
        .WORD   LBB_ELSE        ; ELSE
        .BYTE   2,'T'
        .WORD   LBB_TO          ; TO
        .BYTE   2,'F'
        .WORD   LBB_FN          ; FN
        .BYTE   4,'S'
        .WORD   LBB_SPC         ; SPC
        .BYTE   4,'T'
        .WORD   LBB_THEN        ; THEN
        .BYTE   3,'N'
        .WORD   LBB_NOT         ; NOT
        .BYTE   4,'S'
        .WORD   LBB_STEP        ; STEP
        .BYTE   5,'U'
        .WORD   LBB_UNTIL       ; UNTIL
        .BYTE   5,'W'
        .WORD   LBB_WHILE       ; WHILE
        .BYTE   3,'O'
        .WORD   LBB_OFF         ; OFF

; opperators

        .BYTE   1,'+'
        .WORD   $0000           ; +
        .BYTE   1,'-'
        .WORD   $0000           ; -
        .BYTE   1,'*'
        .WORD   $0000           ; *
        .BYTE   1,'/'
        .WORD   $0000           ; /
        .BYTE   1,'^'
        .WORD   $0000           ; ^
        .BYTE   3,'A'
        .WORD   LBB_AND         ; AND
        .BYTE   3,'E'
        .WORD   LBB_EOR         ; EOR
        .BYTE   2,'O'
        .WORD   LBB_OR          ; OR
        .BYTE   2,'>'
        .WORD   LBB_RSHIFT      ; >>
        .BYTE   2,'<'
        .WORD   LBB_LSHIFT      ; <<
        .BYTE   1,'>'
        .WORD   $0000           ; >
        .BYTE   1,'='
        .WORD   $0000           ; =
        .BYTE   1,'<'
        .WORD   $0000           ; <

; functions

        .BYTE   4,'S'           ;
        .WORD   LBB_SGN         ; SGN
        .BYTE   4,'I'           ;
        .WORD   LBB_INT         ; INT
        .BYTE   4,'A'           ;
        .WORD   LBB_ABS         ; ABS
        .BYTE   4,'U'           ;
        .WORD   LBB_USR         ; USR
        .BYTE   4,'F'           ;
        .WORD   LBB_FRE         ; FRE
        .BYTE   4,'P'           ;
        .WORD   LBB_POS         ; POS
        .BYTE   4,'S'           ;
        .WORD   LBB_SQR         ; SQR
        .BYTE   4,'R'           ;
        .WORD   LBB_RND         ; RND
        .BYTE   4,'L'           ;
        .WORD   LBB_LOG         ; LOG
        .BYTE   4,'E'           ;
        .WORD   LBB_EXP         ; EXP
        .BYTE   4,'C'           ;
        .WORD   LBB_COS         ; COS
        .BYTE   4,'S'           ;
        .WORD   LBB_SIN         ; SIN
        .BYTE   4,'T'           ;
        .WORD   LBB_TAN         ; TAN
        .BYTE   4,'A'           ;
        .WORD   LBB_ATN         ; ATN
        .BYTE   5,'P'           ;
        .WORD   LBB_PEEK        ; PEEK
        .BYTE   6,'S'           ;
        .WORD   LBB_SPEEK       ; SPEEK
        .BYTE   5,'D'           ;
        .WORD   LBB_DEEK        ; DEEK
        .BYTE   5,'S'           ;
        .WORD   LBB_SADD        ; SADD
        .BYTE   4,'L'           ;
        .WORD   LBB_LEN         ; LEN
        .BYTE   5,'S'           ;
        .WORD   LBB_STRS        ; STR$
        .BYTE   4,'V'           ;
        .WORD   LBB_VAL         ; VAL
        .BYTE   4,'A'           ;
        .WORD   LBB_ASC         ; ASC
        .BYTE   7,'U'           ;
        .WORD   LBB_UCASES      ; UCASE$
        .BYTE   7,'L'           ;
        .WORD   LBB_LCASES      ; LCASE$
        .BYTE   5,'C'           ;
        .WORD   LBB_CHRS        ; CHR$
        .BYTE   5,'H'           ;
        .WORD   LBB_HEXS        ; HEX$
        .BYTE   5,'B'           ;
        .WORD   LBB_BINS        ; BIN$
        .BYTE   7,'B'           ;
        .WORD   LBB_BITTST      ; BITTST
        .BYTE   4,'M'           ;
        .WORD   LBB_MAX         ; MAX
        .BYTE   4,'M'           ;
        .WORD   LBB_MIN         ; MIN
        .BYTE   2,'P'           ;
        .WORD   LBB_PI          ; PI
        .BYTE   6,'S'           ;
        .WORD   LBB_SECOND      ; SECOND
        .BYTE   6,'M'           ;
        .WORD   LBB_MINUTE      ; MINUTE
        .BYTE   4,'H'           ;
        .WORD   LBB_HOUR        ; HOUR
        .BYTE   3,'D'           ;
        .WORD   LBB_DAY         ; DAY
        .BYTE   5,'M'           ;
        .WORD   LBB_MONTH       ; MONTH
        .BYTE   4,'Y'           ;
        .WORD   LBB_YEAR        ; YEAR
        .BYTE   7,'V'           ;
        .WORD   LBB_VPTR        ; VARPTR
        .BYTE   6,'L'           ;
        .WORD   LBB_LEFTS       ; LEFT$
        .BYTE   7,'R'           ;
        .WORD   LBB_RIGHTS      ; RIGHT$
        .BYTE   5,'M'           ;
        .WORD   LBB_MIDS        ; MID$
        .BYTE   4,'C'           ;
        .WORD   LBB_CON         ; CON
