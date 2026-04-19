; definitions.asm — constants, token codes, p-code opcodes, macros

; ---------------------------------------------------------------------------
; DOS/65 system entry points
; ---------------------------------------------------------------------------
PEM_ENTRY       = $0103         ; JSR here; function # in X
WARM_BOOT       = $0100
DEFAULT_FCB     = $0107         ; DOS/65 dflfcb (parsed argument FCB)
DMA_BUF         = $0128         ; DOS/65 dflbuf (default DMA buffer)

FARFUNCT        = $32           ; zero-page: FARCALL function number
DO_FARCALL      = $FFF0

; PEM function numbers
PEM_CONIN       = 1             ; console input with echo (blocking)
PEM_CONOUT      = 2             ; console output (A=char)
PEM_DIRCON      = 6             ; direct console I/O (BLOCKING — use fn 11 first)
PEM_CONSTAT     = 11            ; console status (non-blocking)
PEM_OPEN        = 15
PEM_CLOSE       = 16
PEM_SEARCH      = 17
PEM_DELETE      = 19
PEM_READ        = 20
PEM_WRITE       = 21
PEM_MAKE        = 22

; FARCALL function numbers
FC_CHROUT       = 19            ; output char (A=char)
FC_RDKBD        = 20            ; read keyboard non-blocking (A=key or $FF)
FC_SETCUR       = 37            ; set cursor (X=col, Y=row)
FC_CLRSCR       = 38            ; clear screen
FC_SETCOLOR     = 39            ; set color (X=fg|bg, Y=cursor)
FC_SCROLL       = 56            ; scroll down
FC_HIDECUR      = 58            ; unpaint cursor
FC_SHOWCUR      = 59            ; paint cursor

; ---------------------------------------------------------------------------
; P-code opcodes
; ---------------------------------------------------------------------------

; --- Constants ---
OP_LDCI         = $00           ; push sign-extended byte constant
OP_LDCW         = $01           ; push word constant (lo, hi)
OP_LDCC         = $02           ; push char constant
OP_LDCB         = $03           ; push boolean constant (0/1)
OP_LDCS         = $04           ; push string constant (len + bytes)
OP_LDCN         = $05           ; push NIL

; --- Local variable access ---
OP_LDL          = $10           ; push 16-bit local (byte offset from MP+8)
OP_STL          = $11           ; pop into 16-bit local
OP_LDA_L        = $12           ; push address of local
OP_LDB_L        = $13           ; push byte/char/bool local
OP_STB_L        = $14           ; pop byte into local

; --- Global variable access ---
OP_LDG          = $18           ; push 16-bit global (word offset from BASE)
OP_STG          = $19           ; pop into 16-bit global
OP_LDA_G        = $1A           ; push address of global
OP_LDB_G        = $1B           ; push byte global
OP_STB_G        = $1C           ; pop byte into global

; --- Indirect / array ---
OP_LDIND        = $20           ; TOS=addr → push word at addr
OP_STIND        = $21           ; NOS=addr, TOS=val → store
OP_LDB_IND      = $22           ; TOS=addr → push byte at addr
OP_STB_IND      = $23           ; NOS=addr, TOS=val → store byte
OP_INDEX        = $24           ; array index: addr=NOS+TOS*elemsize (word follows)

; --- Integer arithmetic ---
OP_ADI          = $30
OP_SBI          = $31
OP_MPI          = $32
OP_DVI          = $33
OP_MOD          = $34
OP_NGI          = $35           ; negate
OP_ABI          = $36           ; absolute value
OP_SQI          = $37           ; square

; --- Logical / bitwise ---
OP_LAND         = $38
OP_LOR          = $39
OP_LNOT         = $3A           ; logical NOT ($0000 ↔ $FFFF)
OP_BNOT         = $3B           ; bitwise complement

; --- Comparison (push $FFFF=true, $0000=false) ---
OP_EQUI         = $40
OP_NEQI         = $41
OP_LESI         = $42
OP_LEQI         = $43
OP_GTRI         = $44
OP_GEQI         = $45
OP_EQUB         = $46           ; byte equality
OP_EQUS         = $47           ; string equality

; --- Control flow ---
OP_UJP          = $50           ; unconditional jump (signed word offset)
OP_FJP          = $51           ; jump if false (pop TOS)
OP_TJP          = $52           ; jump if true  (pop TOS)
OP_IXPJP        = $53           ; case jump table

; --- Procedure/function calls ---
OP_CALL         = $60           ; call (signed word offset from current IPC)
OP_CALI         = $61           ; call absolute address (word)
OP_RET          = $62           ; return from procedure
OP_RETF         = $63           ; return from function (leave result on stack)
OP_MRKSTK       = $64           ; set up activation record (byte: local size)
OP_DEPSTK       = $65           ; tear down activation record

; --- Heap ---
OP_NEW          = $70           ; allocate (word size follows); push pointer
OP_DISP         = $71           ; free pointer on TOS

; --- I/O ---
OP_WRITI        = $80           ; write integer
OP_WRITC        = $81           ; write character
OP_WRITB        = $82           ; write boolean
OP_WRITS        = $83           ; write string
OP_WRITLN       = $84           ; write newline
OP_READI        = $85           ; read integer
OP_READC        = $86           ; read character
OP_READS        = $87           ; read string

; --- Stack manipulation ---
OP_DUP          = $90
OP_POP          = $91
OP_SWAP         = $92
OP_MOVS         = $93           ; copy n bytes (byte count follows)

; --- Halt ---
OP_HALT         = $FF

; ---------------------------------------------------------------------------
; .PCD file magic / version
; ---------------------------------------------------------------------------
PCD_MAGIC_0     = $50           ; 'P'
PCD_MAGIC_1     = $43           ; 'C'
PCD_VERSION     = $01

; PCD header offsets
PCD_MAGIC       = 0
PCD_VER         = 2
PCD_CODESZ      = 4             ; word: code section size
PCD_GLOBSZ      = 6             ; word: global data size
PCD_STRSZ       = 8             ; word: string pool size
PCD_ENTRY       = 10            ; word: entry point offset into code
PCD_HEADER_SZ   = 12

; ---------------------------------------------------------------------------
; Token codes (used by compiler lexer/parser)
; ---------------------------------------------------------------------------
TOK_EOF         = 0
TOK_IDENT       = 1
TOK_INT         = 2
TOK_STRING      = 3
TOK_CHAR        = 4

; Operators / punctuation
TOK_PLUS        = $10           ; +
TOK_MINUS       = $11           ; -
TOK_STAR        = $12           ; *
TOK_SLASH       = $13           ; /
TOK_EQ          = $14           ; =
TOK_NEQ         = $15           ; <>
TOK_LT          = $16           ; <
TOK_GT          = $17           ; >
TOK_LEQ         = $18           ; <=
TOK_GEQ         = $19           ; >=
TOK_ASSIGN      = $1A           ; :=
TOK_LPAREN      = $1B           ; (
TOK_RPAREN      = $1C           ; )
TOK_LBRACK      = $1D           ; [
TOK_RBRACK      = $1E           ; ]
TOK_COMMA       = $1F           ; ,
TOK_SEMICOLON   = $20           ; ;
TOK_COLON       = $21           ; :
TOK_DOT         = $22           ; .
TOK_DOTDOT      = $23           ; ..
TOK_CARET       = $24           ; ^

; Keywords
TOK_AND         = $40
TOK_ARRAY       = $41
TOK_BEGIN       = $42
TOK_CASE        = $43
TOK_CONST       = $44
TOK_DIV         = $45
TOK_DO          = $46
TOK_DOWNTO      = $47
TOK_ELSE        = $48
TOK_END         = $49
TOK_FOR         = $4A
TOK_FUNCTION    = $4B
TOK_IF          = $4C
TOK_MOD_KW      = $4D
TOK_NIL         = $4E
TOK_NOT         = $4F
TOK_OF          = $50
TOK_OR          = $51
TOK_PROCEDURE   = $52
TOK_PROGRAM     = $53
TOK_RECORD      = $54
TOK_REPEAT      = $55
TOK_THEN        = $56
TOK_TO          = $57
TOK_TYPE        = $58
TOK_UNTIL       = $59
TOK_VAR         = $5A
TOK_WHILE       = $5B
TOK_WITH        = $5C

; ---------------------------------------------------------------------------
; Useful macros
; ---------------------------------------------------------------------------

; Call a FARCALL function that does NOT use A as input (clobbers A).
; Use for FC_CLRSCR, FC_SCROLL, FC_SETCUR, FC_SETCOLOR, etc.
.macro FARCALL func
        lda     #func
        sta     FARFUNCT
        jsr     DO_FARCALL
.endmacro

; Output the character already in A via FARCALL #2.
; PEM_CONOUT requires A=char when DO_FARCALL is called, so we must
; set FARFUNCT first, then restore the char before the jsr.
.macro CHROUT
        pha
        lda     #PEM_CONOUT
        sta     FARFUNCT
        pla
        jsr     DO_FARCALL
.endmacro

; Call PEM with function number in X
.macro PEM func
        ldx     #func
        jsr     PEM_ENTRY
.endmacro
