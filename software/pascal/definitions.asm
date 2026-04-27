; definitions.asm — constants, token codes, p-code opcodes, macros

; ---------------------------------------------------------------------------
; DOS/65 system entry points
; ---------------------------------------------------------------------------
PEM_ENTRY       = $0103         ; JSR here; function # in X
WARM_BOOT       = $0100
DEFAULT_FCB     = $0107         ; DOS/65 dflfcb (parsed argument FCB)
DMA_BUF         = $0128         ; DOS/65 dflbuf (default DMA buffer)

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
PEM_SETDMA      = 26            ; set DMA buffer address (A=lo, Y=hi)

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
OP_STR          = $66           ; pop word, store at MP+AR_RET_VAL (function result)
OP_MRKA         = $67           ; mark+gather: bytes pcount, lsize_extra.
                                ; pcount values on TOS become local slots 0..pcount-1
                                ; in the new frame; lsize_extra reserves body-local space.

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
OP_WRITR        = $88           ; write real (signed fixed-point, scale 100)
OP_READR        = $89           ; read real  (signed fixed-point, scale 100)

; --- Stack manipulation ---
OP_DUP          = $90
OP_POP          = $91
OP_SWAP         = $92
OP_MOVS         = $93           ; copy n bytes (byte count follows)

; --- String built-ins ---
OP_LEN          = $A0           ; TOS=strptr -> TOS=length (int)
OP_POS          = $A1           ; NOS=substr, TOS=str -> TOS=1-based pos (0=not found)
OP_COPY         = $A2           ; NNOS=str, NOS=index, TOS=count -> TOS=result strptr
OP_CONCAT       = $A3           ; NOS=s1, TOS=s2 -> TOS=result strptr
OP_INSET        = $A4           ; NOS=elem, TOS=setmask -> TOS=membership bool
OP_MPR          = $A5           ; real multiply (signed fixed-point, scale 100)
OP_DVR          = $A6           ; real divide   (signed fixed-point, scale 100)

; --- TEXT file I/O ---
; All file ops take a file-struct pointer (NOS / TOS).
OP_FASSGN       = $B0           ; NOS=fileptr, TOS=strptr (filename) -> set FCB
OP_FRESET       = $B1           ; TOS=fileptr -> open existing for reading
OP_FREWRT       = $B2           ; TOS=fileptr -> create/truncate for writing
OP_FCLOSE       = $B3           ; TOS=fileptr -> flush and close
OP_FWRC         = $B4           ; NOS=fileptr, TOS=char -> append char
OP_FWRS         = $B5           ; NOS=fileptr, TOS=strptr -> append string
OP_FWRI         = $B6           ; NOS=fileptr, TOS=int   -> append decimal
OP_FWLN         = $B7           ; TOS=fileptr -> append CR+LF
OP_FRDC         = $B8           ; NOS=fileptr, TOS=charvar addr -> read char
OP_FRDI         = $B9           ; NOS=fileptr, TOS=intvar  addr -> read decimal int
OP_FRDLN        = $BA           ; TOS=fileptr -> skip remaining chars to end-of-line
OP_FEOF         = $BB           ; TOS=fileptr -> push EOF flag (0=false, $FFFF=true)
OP_FAPPND       = $BC           ; TOS=fileptr -> open/create for append at EOF
OP_FRDS         = $BD           ; NOS=fileptr, TOS=strvar addr -> read string to EOL
OP_FWRB         = $BE           ; NOS=fileptr, TOS=bool -> append "TRUE"/"FALSE"
OP_FEOLN        = $BF           ; TOS=fileptr -> push EOLN flag (true if at CR/LF/EOF)
OP_FWRR         = $C0           ; NOS=fileptr, TOS=real -> append fixed-point decimal
OP_FRDR         = $C1           ; NOS=fileptr, TOS=realvar addr -> read fixed-point decimal

; TEXT-file struct layout (168 bytes per variable)
F_FCB           = 0             ; 36 bytes
F_BUF           = 36            ; 128 bytes
F_MODE          = 164           ; 0=closed, 1=read, 2=write
F_POS           = 165           ; 0..127 buffer position
F_EOF           = 166           ; 0=more data, 1=EOF reached
F_SPARE         = 167
FILE_STRUCT_SZ  = 168

F_MODE_CLOSED   = 0
F_MODE_READ     = 1
F_MODE_WRITE    = 2

CTRL_Z          = $1A           ; CP/M text-file end-of-data marker

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
TOK_REAL        = 5

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
TOK_IN          = $4D
TOK_MOD_KW      = $4E
TOK_NIL         = $4F
TOK_NOT         = $50
TOK_OF          = $51
TOK_OR          = $52
TOK_PROCEDURE   = $53
TOK_PROGRAM     = $54
TOK_RECORD      = $55
TOK_REPEAT      = $56
TOK_SET         = $57
TOK_THEN        = $58
TOK_TO          = $59
TOK_TYPE        = $5A
TOK_UNTIL       = $5B
TOK_VAR         = $5C
TOK_WHILE       = $5D
TOK_WITH        = $5E
TOK_IMPLEMENTATION = $5F
TOK_INTERFACE   = $60
TOK_UNIT        = $61
TOK_USES        = $62

; ---------------------------------------------------------------------------
; Useful macros
; ---------------------------------------------------------------------------

; Call PEM with function number in X
.macro PEM func
        ldx     #func
        jsr     PEM_ENTRY
.endmacro
