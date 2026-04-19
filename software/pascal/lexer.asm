.PC02
; lexer.asm — Pascal tokenizer
;
; Reads source file through 128-byte sector buffer.
; Produces token stream via next_token (sets tok_type + value vars in ZP).

; ---------------------------------------------------------------------------
; Keyword table — each entry: length byte, ASCII chars, token code
; Searched linearly; 18 keywords.
; ---------------------------------------------------------------------------
.segment "CODE"

keyword_table:
        ; length, chars..., token
        .byte 3, "AND",       TOK_AND
        .byte 5, "ARRAY",     TOK_ARRAY
        .byte 5, "BEGIN",     TOK_BEGIN
        .byte 4, "CASE",      TOK_CASE
        .byte 5, "CONST",     TOK_CONST
        .byte 3, "DIV",       TOK_DIV
        .byte 2, "DO",        TOK_DO
        .byte 6, "DOWNTO",    TOK_DOWNTO
        .byte 4, "ELSE",      TOK_ELSE
        .byte 3, "END",       TOK_END
        .byte 3, "FOR",       TOK_FOR
        .byte 8, "FUNCTION",  TOK_FUNCTION
        .byte 2, "IF",        TOK_IF
        .byte 3, "MOD",       TOK_MOD_KW
        .byte 3, "NIL",       TOK_NIL
        .byte 3, "NOT",       TOK_NOT
        .byte 2, "OF",        TOK_OF
        .byte 2, "OR",        TOK_OR
        .byte 9, "PROCEDURE", TOK_PROCEDURE
        .byte 7, "PROGRAM",   TOK_PROGRAM
        .byte 6, "RECORD",    TOK_RECORD
        .byte 6, "REPEAT",    TOK_REPEAT
        .byte 4, "THEN",      TOK_THEN
        .byte 2, "TO",        TOK_TO
        .byte 4, "TYPE",      TOK_TYPE
        .byte 5, "UNTIL",     TOK_UNTIL
        .byte 3, "VAR",       TOK_VAR
        .byte 5, "WHILE",     TOK_WHILE
        .byte 4, "WITH",      TOK_WITH
        .byte 0                         ; sentinel

; Identifier buffer (max 63 chars + length byte at [0])
ident_buf:      .res 64

; Saved-name buffer for parser (e.g. var-decl name preservation across
; parse_type_spec which clobbers ident_buf).
save_name_buf:  .res 16

; Saved symbol info — used by parse_assign_or_call to preserve symbol
; entry fields across next_token + parse_expression, which clobber tmp3.
sym_save_kind:  .res 1
sym_save_scope: .res 1
sym_save_off:   .res 2

; ---------------------------------------------------------------------------
; lexer_init — reset lexer state; call before compile_program
; ---------------------------------------------------------------------------
lexer_init:
        lda     #1
        sta     lex_line
        lda     #0
        sta     lex_line+1
        sta     lex_col
        sta     src_buf_pos
        sta     src_buf_end
        ; prime lex_char
        jsr     lexer_getc
        rts

; ---------------------------------------------------------------------------
; next_token — advance to next token, set tok_type
; On return:
;   tok_type  = token code
;   tok_ival  = integer value (if TOK_INT)
;   ident_buf = identifier string (if TOK_IDENT)
; ---------------------------------------------------------------------------
next_token:
        ; skip whitespace and comments
@skip:  lda     lex_char
        beq     @eof
        cmp     #' '
        beq     @ws
        cmp     #$09            ; TAB
        beq     @ws
        cmp     #$0D            ; CR — treat as whitespace, LF counts the line
        beq     @ws
        cmp     #$0A            ; LF
        beq     @nl
        cmp     #'{'            ; Pascal comment start
        beq     @comment
        jmp     @not_ws
@ws:    jsr     lexer_getc
        bra     @skip
@nl:    inc     lex_line
        bne     :+
        inc     lex_line+1
:       lda     #0
        sta     lex_col
        jsr     lexer_getc
        bra     @skip
@comment:
        ; skip { ... }
@cloop: jsr     lexer_getc
        beq     @eof
        cmp     #'}'
        bne     @cloop
        jsr     lexer_getc
        bra     @skip
@eof:   lda     #TOK_EOF
        sta     tok_type
        rts

@not_ws:
        ; letter → identifier or keyword
        jsr     is_letter
        bcs     @ident

        ; digit → integer literal
        jsr     is_digit
        bcs     @number

        ; string → string literal
        cmp     #$27            ; single quote
        beq     @string

        ; operators and punctuation
        jmp     @punct

; --- Identifier ---
@ident: ldy     #0
@id_loop:
        lda     lex_char
        jsr     to_upper
        sta     ident_buf+1,y
        iny
        cpy     #63
        beq     @id_done
        jsr     lexer_getc
        lda     lex_char
        jsr     is_letter
        bcs     @id_loop
        jsr     is_digit
        bcs     @id_loop
@id_done:
        tya
        sta     ident_buf       ; length
        ; check against keyword table
        jsr     lookup_keyword
        sta     tok_type
        rts

; --- Integer literal ---
@number:
        lda     #0
        sta     tok_ival_lo
        sta     tok_ival_hi
@num_loop:
        lda     lex_char
        sec
        sbc     #'0'
        jsr     mul10_tokval    ; tokval = tokval*10 + digit
        jsr     lexer_getc
        lda     lex_char
        jsr     is_digit
        bcs     @num_loop
        lda     #TOK_INT
        sta     tok_type
        rts

; --- String literal ---
@string:
        jsr     lexer_getc      ; skip opening quote
        ldy     #0
@str_loop:
        lda     lex_char
        beq     @str_done       ; EOF
        cmp     #$27
        beq     @str_close
        sta     ident_buf+1,y
        iny
        jsr     lexer_getc
        bra     @str_loop
@str_close:
        jsr     lexer_getc      ; skip closing quote
        ; check for '' (escaped quote)
        lda     lex_char
        cmp     #$27
        bne     @str_done
        lda     #$27
        sta     ident_buf+1,y
        iny
        jsr     lexer_getc
        bra     @str_loop
@str_done:
        tya
        sta     ident_buf
        lda     #TOK_STRING
        sta     tok_type
        rts

; --- Punctuation / operators ---
@punct:
        ldx     #0
        lda     lex_char
        cmp     #'+'
        bne     :+
        ldx     #TOK_PLUS
:       cmp     #'-'
        bne     :+
        ldx     #TOK_MINUS
:       cmp     #'*'
        bne     :+
        ldx     #TOK_STAR
:       cmp     #'/'
        bne     :+
        ldx     #TOK_SLASH
:       cmp     #'='
        bne     :+
        ldx     #TOK_EQ
:       cmp     #'('
        bne     :+
        ldx     #TOK_LPAREN
:       cmp     #')'
        bne     :+
        ldx     #TOK_RPAREN
:       cmp     #'['
        bne     :+
        ldx     #TOK_LBRACK
:       cmp     #']'
        bne     :+
        ldx     #TOK_RBRACK
:       cmp     #','
        bne     :+
        ldx     #TOK_COMMA
:       cmp     #';'
        bne     :+
        ldx     #TOK_SEMICOLON
:       cmp     #'^'
        bne     :+
        ldx     #TOK_CARET
:       cmp     #'<'
        bne     @not_lt
        jsr     lexer_getc
        lda     lex_char
        cmp     #'='
        beq     @leq
        cmp     #'>'
        beq     @neq
        lda     #TOK_LT
        sta     tok_type
        rts
@leq:   jsr     lexer_getc
        lda     #TOK_LEQ
        sta     tok_type
        rts
@neq:   jsr     lexer_getc
        lda     #TOK_NEQ
        sta     tok_type
        rts
@not_lt:
        cmp     #'>'
        bne     @not_gt
        jsr     lexer_getc
        lda     lex_char
        cmp     #'='
        beq     @geq
        lda     #TOK_GT
        sta     tok_type
        rts
@geq:   jsr     lexer_getc
        lda     #TOK_GEQ
        sta     tok_type
        rts
@not_gt:
        cmp     #':'
        bne     @not_colon
        jsr     lexer_getc
        lda     lex_char
        cmp     #'='
        beq     @assign
        lda     #TOK_COLON
        sta     tok_type
        rts
@assign:
        jsr     lexer_getc
        lda     #TOK_ASSIGN
        sta     tok_type
        rts
@not_colon:
        cmp     #'.'
        bne     @not_dot
        jsr     lexer_getc
        lda     lex_char
        cmp     #'.'
        beq     @dotdot
        lda     #TOK_DOT
        sta     tok_type
        rts
@dotdot:
        jsr     lexer_getc
        lda     #TOK_DOTDOT
        sta     tok_type
        rts
@not_dot:
        ; If a single-byte operator matched above, X holds its TOK_* code.
        ; Otherwise X=0 and the character is unknown — skip and retry.
        cpx     #0
        beq     @unknown
        txa
        sta     tok_type
        jsr     lexer_getc
        rts
@unknown:
        jsr     lexer_getc
        jmp     next_token

; ---------------------------------------------------------------------------
; lexer_getc — fetch next character into lex_char
; Refills sector buffer via file_read_sector when empty
; ---------------------------------------------------------------------------
lexer_getc:
        phy                     ; preserve caller's Y across refill + read
        lda     src_buf_pos
        cmp     src_buf_end
        bcc     @from_buf       ; still have data
        ; refill buffer — set tmp1 to source FCB first
        ; (other code paths leave tmp1 pointing at output FCB)
        lda     src_fcb
        sta     tmp1
        lda     src_fcb+1
        sta     tmp1+1
        jsr     file_read_sector
        cmp     #0
        bne     @eof_char
        lda     #128
        sta     src_buf_end
        lda     #0
        sta     src_buf_pos
@from_buf:
        ldy     src_buf_pos
        lda     DMA_BUF,y
        inc     src_buf_pos
        sta     lex_char
        inc     lex_col
        ply
        rts
@eof_char:
        lda     #0
        sta     lex_char
        ply
        rts

; ---------------------------------------------------------------------------
; lookup_keyword — check ident_buf against keyword_table
; Returns: A = keyword token code, or TOK_IDENT if not found
; ---------------------------------------------------------------------------
lookup_keyword:
        lda     #<keyword_table
        sta     tmp2
        lda     #>keyword_table
        sta     tmp2+1
@next_kw:
        ldy     #0
        lda     (tmp2),y        ; keyword length (0 = sentinel)
        beq     @not_found
        cmp     ident_buf       ; compare lengths first
        bne     @advance
        tax                     ; X = length
        ldy     #1
@cmp_loop:
        lda     (tmp2),y
        cmp     ident_buf,y
        bne     @advance
        iny
        dex
        bne     @cmp_loop
        ; match: token code is at offset length+1
        ldy     ident_buf
        iny
        lda     (tmp2),y
        rts
@advance:
        ; skip to next entry: advance tmp2 by (length+2)
        ; (Y may be >0 if we got here from @cmp_loop — reset to 0 first)
        ldy     #0
        lda     (tmp2),y        ; length byte
        clc
        adc     #2              ; +1 for length byte, +1 for token byte
        clc
        adc     tmp2
        sta     tmp2
        bcc     :+
        inc     tmp2+1
:       bra     @next_kw
@not_found:
        lda     #TOK_IDENT
        rts

; ---------------------------------------------------------------------------
; is_letter — test lex_char; carry set if A-Z or a-z
; ---------------------------------------------------------------------------
is_letter:
        lda     lex_char
        cmp     #'A'
        bcc     @no
        cmp     #'Z'+1
        bcc     @yes
        cmp     #'a'
        bcc     @no
        cmp     #'z'+1
        bcc     @yes
        cmp     #'_'
        beq     @yes
@no:    clc
        rts
@yes:   sec
        rts

; ---------------------------------------------------------------------------
; is_digit — test lex_char; carry set if 0-9
; ---------------------------------------------------------------------------
is_digit:
        lda     lex_char
        cmp     #'0'
        bcc     @no
        cmp     #'9'+1
        bcs     @no
        sec
        rts
@no:    clc
        rts

; ---------------------------------------------------------------------------
; to_upper — convert A to uppercase (A-Z,a-z only, others pass through)
; ---------------------------------------------------------------------------
to_upper:
        cmp     #'a'
        bcc     @done
        cmp     #'z'+1
        bcs     @done
        and     #$DF
@done:  rts

; ---------------------------------------------------------------------------
; mul10_tokval — tokval = tokval*10 + A  (A = digit 0-9)
; ---------------------------------------------------------------------------
mul10_tokval:
        pha
        ; multiply tok_ival by 10: x10 = x*8 + x*2
        lda     tok_ival_lo
        asl                     ; *2
        sta     tmp2
        lda     tok_ival_hi
        rol
        sta     tmp2+1
        lda     tmp2            ; *4
        asl
        sta     tmp3
        lda     tmp2+1
        rol
        sta     tmp3+1
        lda     tmp3            ; *8
        asl
        sta     tmp3
        lda     tmp3+1
        rol
        sta     tmp3+1
        ; *10 = *8 + *2
        lda     tmp3
        clc
        adc     tmp2
        sta     tok_ival_lo
        lda     tmp3+1
        adc     tmp2+1
        sta     tok_ival_hi
        ; add digit
        pla
        clc
        adc     tok_ival_lo
        sta     tok_ival_lo
        bcc     :+
        inc     tok_ival_hi
:       rts
