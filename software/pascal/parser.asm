; parser.asm — recursive-descent Pascal parser + single-pass code generator
;
; Entry point: compile_program (called from pascal_main)
;
; This is a skeleton. Each parse routine calls next_token and emits p-code
; directly (no AST nodes). Phase 3 fleshes out the full grammar.

.segment "CODE"

; ---------------------------------------------------------------------------
; expect — verify current token is `expected_tok`, advance, error if not
; expected_tok passed in X; error string pointer in tmp3 (lo) and A (hi)
; ---------------------------------------------------------------------------
expect:
        lda     tok_type
        cpx     tok_type
        beq     @ok
        ; mismatch — report error using tmp3 as message pointer
        lda     tmp3
        sta     tmp0
        lda     tmp3+1
        sta     tmp0+1
        jsr     compile_error
        ; attempt recovery: skip tokens until we see the expected one or EOF
@recover:
        lda     tok_type
        beq     @ok             ; EOF — stop recovery
        cpx     tok_type
        beq     @ok
        jsr     next_token
        bra     @recover
@ok:    jsr     next_token
        rts

; Helper macros — not ca65 .macros since they'd be complex; use inline calls.

; ---------------------------------------------------------------------------
; compile_program — top-level: PROGRAM name ; block .
; ---------------------------------------------------------------------------
compile_program:
        jsr     symtab_init
        ; expect PROGRAM keyword
        lda     tok_type
        cmp     #TOK_PROGRAM
        bne     @no_hdr         ; allow missing PROGRAM header (lenient)
        jsr     next_token
        ; program name (identifier)
        jsr     next_token
        ; semicolon
        lda     tok_type
        cmp     #TOK_SEMICOLON
        bne     :+
        jsr     next_token
:
@no_hdr:
        ; Emit MRKSTK 0 as prologue placeholder (patched later with actual local count)
        ; For global scope we skip this; globals live at pm_base.
        jsr     parse_block
        ; Emit HALT at end of main program
        jsr     emit_HALT
        rts

; ---------------------------------------------------------------------------
; parse_block — { const_part } { type_part } { var_part } { proc_part } statement_part
; ---------------------------------------------------------------------------
parse_block:
@again:
        lda     tok_type
        cmp     #TOK_CONST
        beq     @const
        cmp     #TOK_TYPE
        beq     @type
        cmp     #TOK_VAR
        beq     @var
        cmp     #TOK_PROCEDURE
        beq     @proc
        cmp     #TOK_FUNCTION
        beq     @func
        cmp     #TOK_BEGIN
        beq     @body
        rts                     ; nothing recognisable

@const: jsr     next_token
        jsr     parse_const_decls
        bra     @again
@type:  jsr     next_token
        jsr     parse_type_decls
        bra     @again
@var:   jsr     next_token
        jsr     parse_var_decls
        bra     @again
@proc:  jsr     next_token
        jsr     parse_proc_decl
        bra     @again
@func:  jsr     next_token
        jsr     parse_func_decl
        bra     @again
@body:  jsr     parse_compound_stmt
        rts

; ---------------------------------------------------------------------------
; parse_const_decls — name = value ; { name = value ; }
; Phase 1 stub: just skip to semicolons
; ---------------------------------------------------------------------------
parse_const_decls:
@loop:  lda     tok_type
        cmp     #TOK_IDENT
        bne     @done
        ; save const name
        ; TODO: add to symbol table as SYM_CONST
        jsr     next_token      ; skip name
        jsr     next_token      ; skip '='
        jsr     next_token      ; skip value (simple literal assumed)
        jsr     next_token      ; skip ';'
        bra     @loop
@done:  rts

; ---------------------------------------------------------------------------
; parse_type_decls — stub
; ---------------------------------------------------------------------------
parse_type_decls:
@loop:  lda     tok_type
        cmp     #TOK_IDENT
        bne     @done
        jsr     next_token      ; skip name
        jsr     next_token      ; skip '='
        ; TODO: parse type definition
        jsr     next_token      ; skip for now
        jsr     next_token      ; skip ';'
        bra     @loop
@done:  rts

; ---------------------------------------------------------------------------
; parse_var_decls — name { , name } : type ; { ... }
; Allocates global storage for each declared variable
; ---------------------------------------------------------------------------
parse_var_decls:
@decl:  lda     tok_type
        cmp     #TOK_IDENT
        bne     @done
        ; save the variable name (ident_buf will be clobbered by parse_type_spec)
        ldy     #15
@savename:
        lda     ident_buf,y
        sta     save_name_buf,y
        dey
        bpl     @savename
        ; consume the name
        jsr     next_token
        ; TODO: handle comma-separated names
        ; expect ':'
        lda     tok_type
        cmp     #TOK_COLON
        bne     :+
        jsr     next_token
:       ; parse type
        jsr     parse_type_spec ; returns type code in A
        pha
        ; expect ';'
        lda     tok_type
        cmp     #TOK_SEMICOLON
        bne     :+
        jsr     next_token
:       ; swap ident_buf ↔ save_name_buf so symtab_add sees the saved name,
        ; while preserving the lexer's most recent ident_buf in save_name_buf
        jsr     swap_ident_save
        pla                     ; type code
        tax                     ; TY_*
        jsr     codegen_alloc_global  ; sets tmp2 = offset
        lda     #SYM_VAR
        jsr     symtab_add
        ; swap back so ident_buf holds whatever the lexer just read
        jsr     swap_ident_save
        bra     @decl
@done:  rts

; swap 16 bytes between ident_buf and save_name_buf
swap_ident_save:
        ldy     #15
@sw:    lda     ident_buf,y
        ldx     save_name_buf,y
        sta     save_name_buf,y
        txa
        sta     ident_buf,y
        dey
        bpl     @sw
        rts

; ---------------------------------------------------------------------------
; parse_type_spec — consume type token(s), return TY_* code in A
; ---------------------------------------------------------------------------
parse_type_spec:
        lda     tok_type
        cmp     #TOK_IDENT
        bne     @not_ident
        ; check for built-in type names
        lda     ident_buf       ; length
        cmp     #7
        bne     @chk3
        ; "INTEGER" (7 chars)
        lda     ident_buf+1
        cmp     #'I'
        bne     @chk_bool
        jsr     next_token
        lda     #TY_INT
        rts
@chk_bool:
        lda     ident_buf+1
        cmp     #'B'
        bne     @chk_str
        jsr     next_token
        lda     #TY_BOOL
        rts
@chk_str:
        jsr     next_token
        lda     #TY_INT         ; default
        rts
@chk3:  cmp     #4
        bne     @chk6
        ; "CHAR" (4)
        lda     ident_buf+1
        cmp     #'C'
        bne     @chk3b
        jsr     next_token
        lda     #TY_CHAR
        rts
@chk3b: jsr     next_token
        lda     #TY_INT
        rts
@chk6:  cmp     #6
        bne     @default_type
        ; "STRING" (6)
        jsr     next_token
        lda     #TY_STRING
        rts
@default_type:
        jsr     next_token
        lda     #TY_INT
        rts
@not_ident:
        ; ARRAY or RECORD — skip for Phase 1
        lda     tok_type
        cmp     #TOK_ARRAY
        bne     :+
        jsr     next_token      ; 'OF'
        jsr     next_token      ; base type
        jsr     next_token
        lda     #TY_ARRAY
        rts
:       jsr     next_token
        lda     #TY_INT
        rts

; ---------------------------------------------------------------------------
; parse_proc_decl / parse_func_decl — stubs for Phase 3
; ---------------------------------------------------------------------------
parse_proc_decl:
        ; skip to END or next top-level keyword
        jsr     next_token      ; proc name
@skip:  lda     tok_type
        beq     @done
        cmp     #TOK_SEMICOLON
        beq     @found_semi
        jsr     next_token
        bra     @skip
@found_semi:
        jsr     next_token
        ; parse the block for this proc
        jsr     parse_block
        ; expect END ;
        lda     tok_type
        cmp     #TOK_SEMICOLON
        bne     :+
        jsr     next_token
:
@done:  rts

parse_func_decl:
        jmp     parse_proc_decl ; same structure for now

; ---------------------------------------------------------------------------
; parse_compound_stmt — BEGIN { statement ; } END
; ---------------------------------------------------------------------------
parse_compound_stmt:
        ; consume BEGIN
        lda     tok_type
        cmp     #TOK_BEGIN
        bne     @done
        jsr     next_token
@loop:  lda     tok_type
        cmp     #TOK_END
        beq     @end
        cmp     #TOK_EOF
        beq     @done
        ; remember tok before parse_statement to detect no-progress
        sta     tmp3
        jsr     parse_statement
        lda     tok_type
        cmp     #TOK_SEMICOLON
        beq     @consume_semi
        cmp     #TOK_END
        beq     @end
        cmp     #TOK_EOF
        beq     @done
        ; if parse_statement didn't consume anything, force-advance to avoid
        ; an infinite loop on an unrecognised token
        cmp     tmp3
        bne     @loop
        jsr     next_token
        bra     @loop
@consume_semi:
        jsr     next_token
        bra     @loop
@end:   jsr     next_token      ; consume END
@done:  rts

; ---------------------------------------------------------------------------
; parse_statement — dispatch based on current token
; ---------------------------------------------------------------------------
parse_statement:
        lda     tok_type
        cmp     #TOK_BEGIN
        beq     @compound
        cmp     #TOK_IF
        beq     @if
        cmp     #TOK_WHILE
        beq     @while
        cmp     #TOK_FOR
        beq     @for
        cmp     #TOK_REPEAT
        beq     @repeat
        cmp     #TOK_IDENT
        beq     @assign_or_call
        rts                     ; empty statement

@compound:
        jmp     parse_compound_stmt

@assign_or_call:
        jsr     parse_assign_or_call
        rts

@if:    jsr     parse_if
        rts

@while: jsr     parse_while
        rts

@for:   jsr     parse_for
        rts

@repeat:
        jsr     parse_repeat
        rts

; ---------------------------------------------------------------------------
; parse_assign_or_call — identifier already consumed by caller (in ident_buf)
; If next is ':=' → assignment
; If next is '(' → procedure call
; If identifier is WRITE/WRITELN → built-in I/O
; ---------------------------------------------------------------------------
parse_assign_or_call:
        ; check for WRITE / WRITELN built-ins
        lda     ident_buf       ; length
        cmp     #5
        bne     @chk_writeln
        ; "WRITE" (5)
        lda     ident_buf+1
        cmp     #'W'
        bne     @lookup_sym
        jsr     next_token
        jsr     parse_write_args
        rts
@chk_writeln:
        cmp     #7
        bne     @lookup_sym
        ; "WRITELN" (7)
        lda     ident_buf+1
        cmp     #'W'
        bne     @lookup_sym
        jsr     next_token
        lda     tok_type
        cmp     #TOK_LPAREN
        bne     @just_nl
        jsr     parse_write_args
@just_nl:
        jsr     emit_WRITLN
        rts

@lookup_sym:
        ; look up identifier in symbol table
        jsr     symtab_find
        bcs     @found_sym
        ; undefined — error
        lda     #<err_undef
        sta     tmp0
        lda     #>err_undef
        sta     tmp0+1
        jsr     compile_error
        ; skip to semicolon / end
        jsr     next_token
        rts

@found_sym:
        ; tmp3 = symbol entry pointer. Snapshot the fields we need
        ; before any next_token / parse_expression call clobbers tmp3.
        ldy     #16
        lda     (tmp3),y
        sta     sym_save_kind
        ldy     #20
        lda     (tmp3),y
        sta     sym_save_scope
        ldy     #18
        lda     (tmp3),y
        sta     sym_save_off
        ldy     #19
        lda     (tmp3),y
        sta     sym_save_off+1
        lda     sym_save_kind
        cmp     #SYM_VAR
        beq     @do_assign
        cmp     #SYM_PROC
        beq     @do_call
        rts

@do_assign:
        ; expect ':='
        jsr     next_token
        lda     tok_type
        cmp     #TOK_ASSIGN
        bne     :+
        jsr     next_token
:       ; parse expression
        jsr     parse_expression
        ; emit STG or STL based on saved scope
        lda     sym_save_scope
        bne     @local_store
        ; global store
        lda     sym_save_off+1
        sta     scratch
        lda     sym_save_off
        jsr     emit_STG
        rts
@local_store:
        lda     sym_save_off
        jsr     emit_STL
        rts

@do_call:
        ; TODO: push arguments, CALL
        jsr     next_token      ; consume '('
        ; skip args for now
@skip_args:
        lda     tok_type
        cmp     #TOK_RPAREN
        beq     @call_done
        cmp     #TOK_EOF
        beq     @call_done
        jsr     next_token
        bra     @skip_args
@call_done:
        jsr     next_token      ; consume ')'
        rts

; ---------------------------------------------------------------------------
; parse_write_args — WRITE/WRITELN argument list
; ---------------------------------------------------------------------------
parse_write_args:
        lda     tok_type
        cmp     #TOK_LPAREN
        bne     @done
        jsr     next_token
@arg:   lda     tok_type
        cmp     #TOK_RPAREN
        beq     @close
        cmp     #TOK_EOF
        beq     @done
        jsr     parse_expression
        ; emit appropriate WRIT* opcode based on last inferred type
        ; (Phase 1: emit WRITI as default; Phase 3 tracks types)
        jsr     emit_WRITI
        lda     tok_type
        cmp     #TOK_COMMA
        bne     @close
        jsr     next_token
        bra     @arg
@close: jsr     next_token      ; consume ')'
@done:  rts

; ---------------------------------------------------------------------------
; parse_if — IF expr THEN stmt [ ELSE stmt ]
; ---------------------------------------------------------------------------
parse_if:
        jsr     next_token      ; consume IF
        jsr     parse_expression
        ; emit FJP with placeholder; save patch addr on 6502 stack
        ; (parse_statement will clobber tmp2 / scratch)
        jsr     emit_FJP        ; A=patch_lo, scratch=patch_hi
        pha                     ; save FJP patch lo
        lda     scratch
        pha                     ; save FJP patch hi
        ; THEN
        lda     tok_type
        cmp     #TOK_THEN
        bne     :+
        jsr     next_token
:       jsr     parse_statement
        ; check for ELSE
        lda     tok_type
        cmp     #TOK_ELSE
        bne     @patch_then
        ; emit UJP over else branch
        jsr     emit_UJP        ; A=patch_lo, scratch=patch_hi
        pha                     ; save UJP patch lo
        lda     scratch
        pha                     ; save UJP patch hi
        ; restore FJP patch addr into tmp2 and patch it to here
        ; (need to dig under the UJP entries on the stack)
        ; stack top → bottom: UJP_hi, UJP_lo, FJP_hi, FJP_lo
        pla                     ; UJP hi
        sta     sym_save_off    ; reuse sym_save_off as temp byte
        pla                     ; UJP lo
        sta     sym_save_off+1
        pla                     ; FJP hi
        sta     tmp2+1
        pla                     ; FJP lo
        sta     tmp2
        jsr     patch_jump      ; patch FJP → start of else branch
        ; push UJP patch addr back for after-else patching
        lda     sym_save_off+1  ; UJP lo
        pha
        lda     sym_save_off    ; UJP hi
        pha
        jsr     next_token      ; consume ELSE
        jsr     parse_statement
        ; patch UJP to here
        pla                     ; UJP hi
        sta     tmp2+1
        pla                     ; UJP lo
        sta     tmp2
        jsr     patch_jump
        rts
@patch_then:
        ; no ELSE — restore FJP patch addr and patch to here
        pla                     ; FJP hi
        sta     tmp2+1
        pla                     ; FJP lo
        sta     tmp2
        jsr     patch_jump
        rts

; ---------------------------------------------------------------------------
; parse_while — WHILE expr DO stmt
; ---------------------------------------------------------------------------
parse_while:
        jsr     next_token      ; consume WHILE
        ; save loop top
        lda     cg_pc
        sta     tmp2
        lda     cg_pc+1
        sta     tmp2+1
        ; we'll need loop_top later; save on 6502 stack
        lda     tmp2+1
        pha
        lda     tmp2
        pha
        jsr     parse_expression
        jsr     emit_FJP        ; exit if false
        pha
        lda     scratch
        pha
        ; DO
        lda     tok_type
        cmp     #TOK_DO
        bne     :+
        jsr     next_token
:       jsr     parse_statement
        ; emit UJP back to loop top
        lda     #OP_UJP
        jsr     emit_byte
        ; offset = loop_top - (cg_pc + 2)
        pla
        sta     tmp3            ; FJP patch hi
        pla
        sta     tmp3+1          ; FJP patch lo — note: reversed from earlier push
        ; compute back-edge offset
        pla                     ; loop_top lo
        sta     tmp2
        pla                     ; loop_top hi
        sta     tmp2+1
        ; delta = loop_top - (cg_pc + 2)
        sec
        lda     tmp2
        sbc     cg_pc
        sta     scratch
        lda     tmp2+1
        sbc     cg_pc+1
        sta     scratch+1
        lda     scratch
        sec
        sbc     #2
        sta     scratch
        bcs     :+
        dec     scratch+1
:       lda     scratch
        jsr     emit_byte
        lda     scratch+1
        jsr     emit_byte
        ; patch FJP exit
        ; tmp3+1:tmp3 holds FJP patch addr (note push order was lo,hi → pla gets hi first)
        ; Rebuild properly:
        lda     tmp3+1
        sta     tmp2
        lda     tmp3
        sta     tmp2+1
        jsr     patch_jump
        rts

; ---------------------------------------------------------------------------
; parse_for — FOR id := expr TO|DOWNTO expr DO stmt
;
; Codegen layout:
;   <init>:   parse expr1; STG/STL var
;             parse expr2; STG hidden_global   (limit, evaluated once)
;   loop_top: LDG/LDL var; LDG hidden_global
;             LEQI (TO) | GEQI (DOWNTO)
;             FJP exit
;             <body>
;             LDG/LDL var; LDCI 1; ADI|SBI; STG/STL var
;             UJP loop_top
;   exit:
;
; State on 6502 stack across body:
;   var_off_lo, var_off_hi, var_scope, dir, lim_off_hi, lim_off_lo
; for_loop_top / for_patch live in zp (FOR is not nestable in this build).
; ---------------------------------------------------------------------------
parse_for:
        jsr     next_token              ; consume FOR

        lda     tok_type
        cmp     #TOK_IDENT
        beq     :+
        rts                              ; not an ident — silently bail
:       jsr     symtab_find
        bcs     :+
        jsr     next_token               ; consume the bad ident
        rts
:
        ; snapshot var info into sym_save_*
        ldy     #20
        lda     (tmp3),y
        sta     sym_save_scope
        ldy     #18
        lda     (tmp3),y
        sta     sym_save_off
        ldy     #19
        lda     (tmp3),y
        sta     sym_save_off+1
        jsr     next_token               ; consume identifier

        ; push var info: scope, off_hi, off_lo (top)
        lda     sym_save_scope
        pha
        lda     sym_save_off+1
        pha
        lda     sym_save_off
        pha

        ; ':='
        lda     tok_type
        cmp     #TOK_ASSIGN
        bne     :+
        jsr     next_token
:       jsr     parse_expression         ; initial value on TOS

        ; restore sym_save_* (parse_expression may have clobbered them)
        pla
        sta     sym_save_off
        pla
        sta     sym_save_off+1
        pla
        sta     sym_save_scope
        jsr     for_emit_store_var

        ; re-push var info for use after loop
        lda     sym_save_scope
        pha
        lda     sym_save_off+1
        pha
        lda     sym_save_off
        pha

        ; direction byte: 0 = TO, 1 = DOWNTO
        lda     #0
        ldx     tok_type
        cpx     #TOK_DOWNTO
        bne     :+
        lda     #1
:       pha                              ; push dir
        jsr     next_token               ; consume TO / DOWNTO

        ; parse limit expression and store to a hidden global
        jsr     parse_expression
        jsr     codegen_alloc_global     ; tmp2 = hidden offset
        lda     tmp2+1
        sta     scratch
        lda     tmp2
        jsr     emit_STG                 ; consume limit value
        ; push limit offset: hi, lo (top)
        lda     tmp2+1
        pha
        lda     tmp2
        pha

        ; consume DO
        lda     tok_type
        cmp     #TOK_DO
        bne     :+
        jsr     next_token
:
        ; --- loop top ---
        lda     cg_pc
        sta     for_loop_top
        lda     cg_pc+1
        sta     for_loop_top+1

        ; emit LDG/LDL var — restore sym_save_* via TSX peek
        ; stack from top: lim_lo, lim_hi, dir, var_lo, var_hi, var_scope
        tsx
        lda     $0104,x                  ; var_lo
        sta     sym_save_off
        lda     $0105,x                  ; var_hi
        sta     sym_save_off+1
        lda     $0106,x                  ; var_scope
        sta     sym_save_scope
        jsr     for_emit_load_var

        ; emit LDG limit — limit hi at $0102,x, lo at $0101,x
        tsx
        lda     $0102,x
        sta     scratch
        lda     $0101,x
        jsr     emit_LDG

        ; comparison: TO → LEQI (var <= limit), DOWNTO → GEQI
        tsx
        lda     $0103,x                  ; dir
        beq     @cmp_to
        jsr     emit_GEQI
        bra     @cmp_done
@cmp_to:
        jsr     emit_LEQI
@cmp_done:

        ; FJP exit placeholder
        jsr     emit_FJP                 ; A=patch_lo, scratch=patch_hi
        sta     for_patch
        lda     scratch
        sta     for_patch+1

        ; --- body ---
        jsr     parse_statement

        ; --- pop state in reverse push order ---
        pla                              ; lim_lo (discard)
        pla                              ; lim_hi (discard)
        pla                              ; direction
        sta     sym_save_kind            ; reuse for direction storage
        pla                              ; var_lo
        sta     sym_save_off
        pla                              ; var_hi
        sta     sym_save_off+1
        pla                              ; var_scope
        sta     sym_save_scope

        ; emit increment / decrement: LDG var, LDCI 1, ADI|SBI, STG var
        jsr     for_emit_load_var
        lda     #1
        jsr     emit_LDCI
        lda     sym_save_kind            ; direction
        beq     @inc
        jsr     emit_SBI
        bra     @inc_done
@inc:   jsr     emit_ADI
@inc_done:
        jsr     for_emit_store_var

        ; emit UJP back to loop_top
        ; offset = for_loop_top - (cg_pc_after_opcode + 2)
        lda     #OP_UJP
        jsr     emit_byte
        sec
        lda     for_loop_top
        sbc     cg_pc
        sta     scratch
        lda     for_loop_top+1
        sbc     cg_pc+1
        sta     scratch+1
        sec
        lda     scratch
        sbc     #2
        sta     scratch
        bcs     :+
        dec     scratch+1
:       lda     scratch
        jsr     emit_byte
        lda     scratch+1
        jsr     emit_byte

        ; patch FJP exit
        lda     for_patch
        sta     tmp2
        lda     for_patch+1
        sta     tmp2+1
        jsr     patch_jump
        rts

; ---------------------------------------------------------------------------
; for_emit_load_var / for_emit_store_var — emit LDG/LDL or STG/STL based on
; sym_save_scope (0 = global) and sym_save_off (offset).
; ---------------------------------------------------------------------------
for_emit_load_var:
        lda     sym_save_scope
        bne     @local
        lda     sym_save_off+1
        sta     scratch
        lda     sym_save_off
        jmp     emit_LDG
@local: lda     sym_save_off
        jmp     emit_LDL

for_emit_store_var:
        lda     sym_save_scope
        bne     @local
        lda     sym_save_off+1
        sta     scratch
        lda     sym_save_off
        jmp     emit_STG
@local: lda     sym_save_off
        jmp     emit_STL

; ---------------------------------------------------------------------------
; parse_repeat — REPEAT stmt { ; stmt } UNTIL expr
; ---------------------------------------------------------------------------
parse_repeat:
        jsr     next_token      ; consume REPEAT
        ; save top
        lda     cg_pc
        pha
        lda     cg_pc+1
        pha
@loop:  lda     tok_type
        cmp     #TOK_UNTIL
        beq     @until
        cmp     #TOK_EOF
        beq     @done
        jsr     parse_statement
        lda     tok_type
        cmp     #TOK_SEMICOLON
        bne     @loop
        jsr     next_token
        bra     @loop
@until: jsr     next_token      ; consume UNTIL
        jsr     parse_expression
        ; FJP back to top
        lda     #OP_FJP
        jsr     emit_byte
        pla
        sta     tmp2+1
        pla
        sta     tmp2
        sec
        lda     tmp2
        sbc     cg_pc
        sta     scratch
        lda     tmp2+1
        sbc     cg_pc+1
        sta     scratch+1
        lda     scratch
        sec
        sbc     #2
        sta     scratch
        bcs     :+
        dec     scratch+1
:       lda     scratch
        jsr     emit_byte
        lda     scratch+1
        jsr     emit_byte
@done:  rts

; ---------------------------------------------------------------------------
; parse_expression — full expression with operator precedence
; Phase 1: simple additive + comparison; no precedence climbing yet
; ---------------------------------------------------------------------------
parse_expression:
        jsr     parse_simple_expr
        ; check for relational operator
        lda     tok_type
        cmp     #TOK_EQ
        beq     @rel_eq
        cmp     #TOK_NEQ
        beq     @rel_neq
        cmp     #TOK_LT
        beq     @rel_lt
        cmp     #TOK_GT
        beq     @rel_gt
        cmp     #TOK_LEQ
        beq     @rel_leq
        cmp     #TOK_GEQ
        beq     @rel_geq
        rts
@rel_eq:  jsr     next_token
          jsr     parse_simple_expr
          jsr     emit_EQUI
          rts
@rel_neq: jsr     next_token
          jsr     parse_simple_expr
          jsr     emit_NEQI
          rts
@rel_lt:  jsr     next_token
          jsr     parse_simple_expr
          jsr     emit_LESI
          rts
@rel_gt:  jsr     next_token
          jsr     parse_simple_expr
          jsr     emit_GTRI
          rts
@rel_leq: jsr     next_token
          jsr     parse_simple_expr
          jsr     emit_LEQI
          rts
@rel_geq: jsr     next_token
          jsr     parse_simple_expr
          jsr     emit_GEQI
          rts

; ---------------------------------------------------------------------------
; parse_simple_expr — term { (+|-|OR) term }
; ---------------------------------------------------------------------------
parse_simple_expr:
        ; check for unary minus
        lda     tok_type
        cmp     #TOK_MINUS
        bne     :+
        jsr     next_token
        jsr     parse_term
        jsr     emit_NGI
        rts
:       jsr     parse_term
@addop: lda     tok_type
        cmp     #TOK_PLUS
        beq     @add
        cmp     #TOK_MINUS
        beq     @sub
        cmp     #TOK_OR
        beq     @or
        rts
@add:   jsr     next_token
        jsr     parse_term
        jsr     emit_ADI
        bra     @addop
@sub:   jsr     next_token
        jsr     parse_term
        jsr     emit_SBI
        bra     @addop
@or:    jsr     next_token
        jsr     parse_term
        jsr     emit_LOR
        bra     @addop

; ---------------------------------------------------------------------------
; parse_term — factor { (*|DIV|MOD|AND) factor }
; ---------------------------------------------------------------------------
parse_term:
        jsr     parse_factor
@mulop: lda     tok_type
        cmp     #TOK_STAR
        beq     @mul
        cmp     #TOK_DIV
        beq     @div
        cmp     #TOK_DIV_KW
        beq     @div
        cmp     #TOK_MOD_KW
        beq     @mod
        cmp     #TOK_AND
        beq     @and
        rts
@mul:   jsr     next_token
        jsr     parse_factor
        jsr     emit_MPI
        bra     @mulop
@div:   jsr     next_token
        jsr     parse_factor
        jsr     emit_DVI
        bra     @mulop
@mod:   jsr     next_token
        jsr     parse_factor
        jsr     emit_MOD
        bra     @mulop
@and:   jsr     next_token
        jsr     parse_factor
        jsr     emit_LAND
        bra     @mulop

TOK_DIV_KW = TOK_DIV            ; alias

; ---------------------------------------------------------------------------
; parse_factor — literal | variable | ( expr ) | NOT factor
; ---------------------------------------------------------------------------
parse_factor:
        lda     tok_type
        cmp     #TOK_INT
        beq     @int_lit
        cmp     #TOK_CHAR
        beq     @char_lit
        cmp     #TOK_STRING
        beq     @str_lit
        cmp     #TOK_IDENT
        beq     @ident_or_call
        cmp     #TOK_LPAREN
        beq     @paren
        cmp     #TOK_NOT
        bne     :+
        jmp     @not_expr
:       cmp     #TOK_NIL
        bne     :+
        jmp     @nil
:       rts

@int_lit:
        lda     tok_ival_lo
        pha
        lda     tok_ival_hi
        bne     @big_int
        pla
        jsr     emit_LDCI       ; fits in signed byte? check later; use LDCI for now
        jsr     next_token
        rts
@big_int:
        sta     scratch
        pla
        jsr     emit_LDCW
        jsr     next_token
        rts

@char_lit:
        lda     tok_ival_lo
        jsr     emit_LDCC
        jsr     next_token
        rts

@str_lit:
        ; TODO: add string to string pool, emit LDCS with pool address
        ; For now: emit address as LDCW placeholder
        lda     #0
        sta     scratch
        jsr     emit_LDCW
        jsr     next_token
        rts

@ident_or_call:
        ; Look up BEFORE next_token — next_token may overwrite ident_buf
        ; if the following token is an identifier or keyword (e.g. J THEN).
        jsr     symtab_find
        bcs     @sym_ok
        jsr     next_token      ; consume ident
        lda     #0
        sta     scratch
        jsr     emit_LDCW       ; undefined: emit 0
        rts
@sym_ok:
        ; snapshot scope and offset before next_token clobbers tmp3
        ldy     #20
        lda     (tmp3),y
        sta     sym_save_scope
        ldy     #18
        lda     (tmp3),y
        sta     sym_save_off
        ldy     #19
        lda     (tmp3),y
        sta     sym_save_off+1
        jsr     next_token      ; consume ident
        lda     sym_save_scope
        bne     @local_load
        lda     sym_save_off+1
        sta     scratch
        lda     sym_save_off
        jsr     emit_LDG
        rts
@local_load:
        lda     sym_save_off
        jsr     emit_LDL
        rts

@paren: jsr     next_token      ; consume '('
        jsr     parse_expression
        lda     tok_type
        cmp     #TOK_RPAREN
        bne     :+
        jsr     next_token
:       rts

@not_expr:
        jsr     next_token
        jsr     parse_factor
        jsr     emit_LNOT
        rts

@nil:   jsr     emit_LDCN
        jsr     next_token
        rts
