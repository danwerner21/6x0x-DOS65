.PC02
; lexer.asm — Pascal tokenizer
;
; Reads source file through 128-byte sector buffer.
; Produces token stream via next_token (sets tok_type + value vars in ZP).

; ---------------------------------------------------------------------------
; Keyword table — each entry: length byte, ASCII chars, token code
; Searched linearly; 18 keywords.
; ---------------------------------------------------------------------------
        .segment        "CODE"

keyword_table:
; length, chars..., token
        .BYTE   3, "AND",       TOK_AND
        .BYTE   5, "ARRAY",     TOK_ARRAY
        .BYTE   5, "BEGIN",     TOK_BEGIN
        .BYTE   4, "CASE",      TOK_CASE
        .BYTE   5, "CONST",     TOK_CONST
        .BYTE   3, "DIV",       TOK_DIV
        .BYTE   2, "DO",        TOK_DO
        .BYTE   6, "DOWNTO",    TOK_DOWNTO
        .BYTE   4, "ELSE",      TOK_ELSE
        .BYTE   3, "END",       TOK_END
        .BYTE   3, "FOR",       TOK_FOR
        .BYTE   8, "FUNCTION",  TOK_FUNCTION
        .BYTE   2, "IF",        TOK_IF
        .BYTE   14, "IMPLEMENTATION", TOK_IMPLEMENTATION
        .BYTE   2, "IN",        TOK_IN
        .BYTE   9, "INTERFACE", TOK_INTERFACE
        .BYTE   3, "MOD",       TOK_MOD_KW
        .BYTE   3, "NIL",       TOK_NIL
        .BYTE   3, "NOT",       TOK_NOT
        .BYTE   2, "OF",        TOK_OF
        .BYTE   2, "OR",        TOK_OR
        .BYTE   9, "PROCEDURE", TOK_PROCEDURE
        .BYTE   7, "PROGRAM",   TOK_PROGRAM
        .BYTE   6, "RECORD",    TOK_RECORD
        .BYTE   6, "REPEAT",    TOK_REPEAT
        .BYTE   3, "SET",       TOK_SET
        .BYTE   4, "THEN",      TOK_THEN
        .BYTE   2, "TO",        TOK_TO
        .BYTE   4, "TYPE",      TOK_TYPE
        .BYTE   4, "UNIT",      TOK_UNIT
        .BYTE   5, "UNTIL",     TOK_UNTIL
        .BYTE   4, "USES",      TOK_USES
        .BYTE   3, "VAR",       TOK_VAR
        .BYTE   5, "WHILE",     TOK_WHILE
        .BYTE   4, "WITH",      TOK_WITH
        .BYTE   0               ; sentinel

; Identifier buffer (max 63 chars + length byte at [0])
ident_buf:
        .RES    64

; Saved-name buffer for parser (e.g. var-decl name preservation across
; parse_type_spec which clobbers ident_buf).
save_name_buf:
        .RES    16

; Saved symbol info — used by parse_assign_or_call to preserve symbol
; entry fields across next_token + parse_expression, which clobber tmp3.
sym_save_kind:
        .RES    1
sym_save_type:
        .RES    1               ; SYM entry offset 17 (data type: TY_*)
sym_save_scope:
        .RES    1
sym_save_off:
        .RES    2
sym_save_pcount:
        .RES    1                       ; SYM_PROC param count (entry offset 21)
sym_save_vmask:
        .RES    1               ; SYM_PROC/SYM_FUNC VAR-param bitmap (entry offset 22)
sym_save_lsize:
        .RES    1               ; SYM_PROC/SYM_FUNC total local-area size (entry offset 23)
sym_param_types:
        .RES    8               ; formal parameter types 0..7 copied from entry bytes 24..31

; ARRAY type scratch — set by parse_type_spec when it parses an ARRAY type;
; read by parse_var_decls to compute storage size and adjusted offset.
array_lo:
        .RES    2               ; lower bound (signed word)
array_hi:
        .RES    2               ; upper bound (signed word)
array_elem_ty:
        .RES    1               ; element base type (TY_INT, TY_CHAR, TY_BOOL …)

; RECORD type scratch — set by parse_type_spec when it parses a RECORD type
; (or resolves a TYPE alias of a RECORD).  Read by parse_var_decls and by
; parse_type_decls to copy into the new SYM_VAR/SYM_TYPE entry.
record_size:
        .RES    2               ; total record size in bytes
record_first_field:
        .RES    1               ; index of first field in field_table
record_field_count:
        .RES    1               ; number of fields in this record
ptr_meta_cur:
        .RES    1               ; current pointer-type metadata index

; Procedure-declaration scratch (parse_proc_decl; not nestable in this build).
proc_param_count:
        .RES    1               ; running count while parsing param list
proc_entry_idx:
        .RES    2               ; symtab entry pointer (legacy name; for backpatch)
param_var_mask:
        .RES    1               ; bitmap of which params are VAR (bit i ↔ param i, max 8)
group_is_var:
        .RES    1               ; 1 while parsing the names of a VAR-prefixed group
local_alloc_off:
        .RES    1               ; next free local-AR offset; init = pcount*2 at proc entry
uses_saved_tok:
        .RES    1               ; saved top-level lookahead token while USES imports other source files

; Function-call scratch (parse_factor @sym_func_call; nested fn calls in args
; not yet supported — see TODO).
fcall_pcount:
        .RES    1
fcall_lo:
        .RES    1
fcall_hi:
        .RES    1
fcall_type:
        .RES    1
fcall_vmask:
        .RES    1               ; the called proc/func's VAR-param bitmap
fcall_lsize:
        .RES    1               ; the called proc/func's total local-area size (params+locals)
fcall_param_types:
        .RES    8               ; formal parameter types 0..7 for the active function call

; Record-chain scratch (parse_statement / parse_factor). Used while walking
; dotted field access through nested RECORD values.
field_chain_type:
        .RES    1
field_chain_first:
        .RES    1
field_chain_count:
        .RES    1
field_chain_ptrmeta:
        .RES    1

; SET literal scratch (compile-time only). SET values are 16-bit masks over
; element values 0..15 in this build.
set_lit_mask:
        .RES    2
set_lit_lo:
        .RES    1
set_lit_hi:
        .RES    1
set_lit_cur:
        .RES    1
real_frac_count:
        .RES    1               ; lexer scratch while tokenising fixed-point REAL literals

; Main-program entry-point patch slot. compile_program emits a UJP at code
; offset 0; top-level BEGIN/init blocks patch it to the first startup body
; that should run. Hi byte = $FF means "already patched / none pending"
; (sentinel — never a valid patch addr since cg_pc starts at 0).
main_jmp_patch:
        .RES    2

; Pending tail-jump patch for the most recently emitted top-level body.
; Each startup body (unit init or main BEGIN) ends with a UJP placeholder so
; later top-level bodies can be chained together without falling through into
; intervening procedure/function code.
body_chain_patch:
        .RES    2

; ---------------------------------------------------------------------------
; lexer_init — reset lexer state; call before compile_program
; ---------------------------------------------------------------------------
lexer_init:
        LDA     #1
        STA     lex_line
        LDA     #0
        STA     lex_line+1
        STA     lex_col
        STA     src_buf_pos
        STA     src_buf_end
; prime lex_char
        JSR     lexer_getc
        RTS

; ---------------------------------------------------------------------------
; next_token — advance to next token, set tok_type
; On return:
;   tok_type  = token code
;   tok_ival  = integer value (if TOK_INT)
;   ident_buf = identifier string (if TOK_IDENT)
; ---------------------------------------------------------------------------
next_token:
; skip whitespace and comments
@skip:
        LDA     lex_char
        BEQ     @eof
        CMP     #' '
        BEQ     @ws
        CMP     #$09            ; TAB
        BEQ     @ws
        CMP     #$0D            ; CR — treat as whitespace, LF counts the line
        BEQ     @ws
        CMP     #$0A            ; LF
        BEQ     @nl
        CMP     #'{'            ; Pascal comment start
        BEQ     @comment
        JMP     @not_ws
@ws:
        JSR     lexer_getc
        BRA     @skip
@nl:
        INC     lex_line
        BNE     :+
        INC     lex_line+1
:
        LDA     #0
        STA     lex_col
        JSR     lexer_getc
        BRA     @skip
@comment:
; skip { ... }
@cloop:
        JSR     lexer_getc
        LDA     lex_char        ; PLY inside lexer_getc clobbers Z; reload to test EOF
        BEQ     @eof
        CMP     #'}'
        BNE     @cloop
        JSR     lexer_getc
        BRA     @skip
@eof:
        LDA     #TOK_EOF
        STA     tok_type
        RTS

@not_ws:
; letter → identifier or keyword
        JSR     is_letter
        BCC     :+
        JMP     @ident
:

; digit → integer literal
        JSR     is_digit
        BCC     :+
        JMP     @number
:

; string → string literal
        CMP     #$27            ; single quote
        BNE     :+
        JMP     @string
:

; operators and punctuation
        JMP     @punct

; --- Identifier ---
@ident:
        LDY     #0
@id_loop:
        LDA     lex_char
        JSR     to_upper
        STA     ident_buf+1,y
        INY
        CPY     #63
        BEQ     @id_done
        JSR     lexer_getc
        LDA     lex_char
        JSR     is_letter
        BCS     @id_loop
        JSR     is_digit
        BCS     @id_loop
@id_done:
        TYA
        STA     ident_buf       ; length
; check against keyword table
        JSR     lookup_keyword
        STA     tok_type
        RTS

; --- Numeric literal (integer or fixed-point REAL) ---
@number:
        LDA     #0
        STA     tok_ival_lo
        STA     tok_ival_hi
@num_loop:
        LDA     lex_char
        SEC
        SBC     #'0'
        JSR     mul10_tokval    ; tokval = tokval*10 + digit
        JSR     lexer_getc
        LDA     lex_char
        JSR     is_digit
        BCS     @num_loop
        LDA     lex_char
        CMP     #'.'
        BNE     @num_int
        LDA     src_buf_pos
        CMP     src_buf_end
        BCS     @num_int               ; dot at sector boundary -> treat as integer
        TAY
        LDA     DMA_BUF,y
        CMP     #'.'
        BEQ     @num_int               ; 1..10 range syntax
        CMP     #'0'
        BCC     @num_int
        CMP     #'9'+1
        BCS     @num_int
        LDA     #0
        STA     real_frac_count
        JSR     lexer_getc             ; consume '.' -> first fractional digit
@real_loop:
        LDA     lex_char
        JSR     is_digit
        BCC     @real_done
        LDA     real_frac_count
        CMP     #2
        BCS     @real_skip_digit
        LDA     lex_char
        SEC
        SBC     #'0'
        JSR     mul10_tokval
        INC     real_frac_count
@real_skip_digit:
        JSR     lexer_getc
        BRA     @real_loop
@real_done:
        LDA     real_frac_count
        CMP     #0
        BNE     :+
        LDA     #0
        JSR     mul10_tokval
        LDA     #0
        JSR     mul10_tokval
        BRA     @num_real
:       CMP     #1
        BNE     @num_real
        LDA     #0
        JSR     mul10_tokval
@num_real:
        LDA     #TOK_REAL
        STA     tok_type
        RTS
@num_int:
        LDA     #TOK_INT
        STA     tok_type
        RTS

; --- String literal ---
@string:
        JSR     lexer_getc      ; skip opening quote
        LDY     #0
@str_loop:
        LDA     lex_char
        BEQ     @str_done       ; EOF
        CMP     #$27
        BEQ     @str_close
        STA     ident_buf+1,y
        INY
        JSR     lexer_getc
        BRA     @str_loop
@str_close:
        JSR     lexer_getc      ; skip closing quote
; check for '' (escaped quote)
        LDA     lex_char
        CMP     #$27
        BNE     @str_done
        LDA     #$27
        STA     ident_buf+1,y
        INY
        JSR     lexer_getc
        BRA     @str_loop
@str_done:
        TYA
        STA     ident_buf
        LDA     #TOK_STRING
        STA     tok_type
        RTS

; --- Punctuation / operators ---
@punct:
        LDX     #0
        LDA     lex_char
        CMP     #'+'
        BNE     :+
        LDX     #TOK_PLUS
:
        CMP     #'-'
        BNE     :+
        LDX     #TOK_MINUS
:
        CMP     #'*'
        BNE     :+
        LDX     #TOK_STAR
:
        CMP     #'/'
        BNE     :+
        LDX     #TOK_SLASH
:
        CMP     #'='
        BNE     :+
        LDX     #TOK_EQ
:
        CMP     #'('
        BNE     :+
        LDX     #TOK_LPAREN
:
        CMP     #')'
        BNE     :+
        LDX     #TOK_RPAREN
:
        CMP     #'['
        BNE     :+
        LDX     #TOK_LBRACK
:
        CMP     #']'
        BNE     :+
        LDX     #TOK_RBRACK
:
        CMP     #','
        BNE     :+
        LDX     #TOK_COMMA
:
        CMP     #';'
        BNE     :+
        LDX     #TOK_SEMICOLON
:
        CMP     #'^'
        BNE     :+
        LDX     #TOK_CARET
:
        CMP     #'<'
        BNE     @not_lt
        JSR     lexer_getc
        LDA     lex_char
        CMP     #'='
        BEQ     @leq
        CMP     #'>'
        BEQ     @neq
        LDA     #TOK_LT
        STA     tok_type
        RTS
@leq:
        JSR     lexer_getc
        LDA     #TOK_LEQ
        STA     tok_type
        RTS
@neq:
        JSR     lexer_getc
        LDA     #TOK_NEQ
        STA     tok_type
        RTS
@not_lt:
        CMP     #'>'
        BNE     @not_gt
        JSR     lexer_getc
        LDA     lex_char
        CMP     #'='
        BEQ     @geq
        LDA     #TOK_GT
        STA     tok_type
        RTS
@geq:
        JSR     lexer_getc
        LDA     #TOK_GEQ
        STA     tok_type
        RTS
@not_gt:
        CMP     #':'
        BNE     @not_colon
        JSR     lexer_getc
        LDA     lex_char
        CMP     #'='
        BEQ     @assign
        LDA     #TOK_COLON
        STA     tok_type
        RTS
@assign:
        JSR     lexer_getc
        LDA     #TOK_ASSIGN
        STA     tok_type
        RTS
@not_colon:
        CMP     #'.'
        BNE     @not_dot
        JSR     lexer_getc
        LDA     lex_char
        CMP     #'.'
        BEQ     @dotdot
        LDA     #TOK_DOT
        STA     tok_type
        RTS
@dotdot:
        JSR     lexer_getc
        LDA     #TOK_DOTDOT
        STA     tok_type
        RTS
@not_dot:
; If a single-byte operator matched above, X holds its TOK_* code.
; Otherwise X=0 and the character is unknown — skip and retry.
        CPX     #0
        BEQ     @unknown
        TXA
        STA     tok_type
        JSR     lexer_getc
        RTS
@unknown:
        JSR     lexer_getc
        JMP     next_token

; ---------------------------------------------------------------------------
; lexer_getc — fetch next character into lex_char
; Refills sector buffer via file_read_sector when empty
; ---------------------------------------------------------------------------
lexer_getc:
        PHY                     ; preserve caller's Y across refill + read
        LDA     src_buf_pos
        CMP     src_buf_end
        BCC     @from_buf       ; still have data
; refill buffer — set tmp1 to source FCB first
; (other code paths leave tmp1 pointing at output FCB)
        LDA     src_fcb
        STA     tmp1
        LDA     src_fcb+1
        STA     tmp1+1
        JSR     file_read_sector
        CMP     #0
        BNE     @eof_char
        LDA     #128
        STA     src_buf_end
        LDA     #0
        STA     src_buf_pos
@from_buf:
        LDY     src_buf_pos
        LDA     DMA_BUF,y
        INC     src_buf_pos
        STA     lex_char
        INC     lex_col
        PLY
        RTS
@eof_char:
        LDA     #0
        STA     lex_char
        PLY
        RTS

; ---------------------------------------------------------------------------
; lookup_keyword — check ident_buf against keyword_table
; Returns: A = keyword token code, or TOK_IDENT if not found
; ---------------------------------------------------------------------------
lookup_keyword:
        LDA     #<keyword_table
        STA     tmp2
        LDA     #>keyword_table
        STA     tmp2+1
@next_kw:
        LDY     #0
        LDA     (tmp2),y        ; keyword length (0 = sentinel)
        BEQ     @not_found
        CMP     ident_buf       ; compare lengths first
        BNE     @advance
        TAX                     ; X = length
        LDY     #1
@cmp_loop:
        LDA     (tmp2),y
        CMP     ident_buf,y
        BNE     @advance
        INY
        DEX
        BNE     @cmp_loop
; match: token code is at offset length+1
        LDY     ident_buf
        INY
        LDA     (tmp2),y
        RTS
@advance:
; skip to next entry: advance tmp2 by (length+2)
; (Y may be >0 if we got here from @cmp_loop — reset to 0 first)
        LDY     #0
        LDA     (tmp2),y        ; length byte
        CLC
        ADC     #2              ; +1 for length byte, +1 for token byte
        CLC
        ADC     tmp2
        STA     tmp2
        BCC     :+
        INC     tmp2+1
:
        BRA     @next_kw
@not_found:
        LDA     #TOK_IDENT
        RTS

; ---------------------------------------------------------------------------
; is_letter — test lex_char; carry set if A-Z or a-z
; ---------------------------------------------------------------------------
is_letter:
        LDA     lex_char
        CMP     #'A'
        BCC     @no
        CMP     #'Z'+1
        BCC     @yes
        CMP     #'a'
        BCC     @no
        CMP     #'z'+1
        BCC     @yes
        CMP     #'_'
        BEQ     @yes
@no:
        CLC
        RTS
@yes:
        SEC
        RTS

; ---------------------------------------------------------------------------
; is_digit — test lex_char; carry set if 0-9
; ---------------------------------------------------------------------------
is_digit:
        LDA     lex_char
        CMP     #'0'
        BCC     @no
        CMP     #'9'+1
        BCS     @no
        SEC
        RTS
@no:
        CLC
        RTS

; ---------------------------------------------------------------------------
; to_upper — convert A to uppercase (A-Z,a-z only, others pass through)
; ---------------------------------------------------------------------------
to_upper:
        CMP     #'a'
        BCC     @done
        CMP     #'z'+1
        BCS     @done
        AND     #$DF
@done:
        RTS

; ---------------------------------------------------------------------------
; mul10_tokval — tokval = tokval*10 + A  (A = digit 0-9)
; ---------------------------------------------------------------------------
mul10_tokval:
        PHA
; multiply tok_ival by 10: x10 = x*8 + x*2
        LDA     tok_ival_lo
        ASL                     ; *2
        STA     tmp2
        LDA     tok_ival_hi
        ROL
        STA     tmp2+1
        LDA     tmp2            ; *4
        ASL
        STA     tmp3
        LDA     tmp2+1
        ROL
        STA     tmp3+1
        LDA     tmp3            ; *8
        ASL
        STA     tmp3
        LDA     tmp3+1
        ROL
        STA     tmp3+1
; *10 = *8 + *2
        LDA     tmp3
        CLC
        ADC     tmp2
        STA     tok_ival_lo
        LDA     tmp3+1
        ADC     tmp2+1
        STA     tok_ival_hi
; add digit
        PLA
        CLC
        ADC     tok_ival_lo
        STA     tok_ival_lo
        BCC     :+
        INC     tok_ival_hi
:
        RTS
