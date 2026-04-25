; parser.asm — recursive-descent Pascal parser + single-pass code generator
;
; Entry point: compile_program (called from pascal_main)
;
; This is a skeleton. Each parse routine calls next_token and emits p-code
; directly (no AST nodes). Phase 3 fleshes out the full grammar.

        .segment        "CODE"

; ---------------------------------------------------------------------------
; expect — verify current token is `expected_tok`, advance, error if not
; expected_tok passed in X; error string pointer in tmp3 (lo) and A (hi)
; ---------------------------------------------------------------------------
expect:
        LDA     tok_type
        CPX     tok_type
        BEQ     @ok
; mismatch — report error using tmp3 as message pointer
        LDA     tmp3
        STA     tmp0
        LDA     tmp3+1
        STA     tmp0+1
        JSR     compile_error
; attempt recovery: skip tokens until we see the expected one or EOF
@recover:
        LDA     tok_type
        BEQ     @ok             ; EOF — stop recovery
        CPX     tok_type
        BEQ     @ok
        JSR     next_token
        BRA     @recover
@ok:
        JSR     next_token
        RTS

; Helper macros — not ca65 .macros since they'd be complex; use inline calls.

; ---------------------------------------------------------------------------
; compile_program — top-level: PROGRAM name ; block .
; ---------------------------------------------------------------------------
compile_program:
        JSR     symtab_init
; expect PROGRAM keyword
        LDA     tok_type
        CMP     #TOK_PROGRAM
        BNE     @no_hdr         ; allow missing PROGRAM header (lenient)
        JSR     next_token
; program name (identifier)
        JSR     next_token
; semicolon
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     :+
        JSR     next_token
:
@no_hdr:
; Emit a forward UJP at code offset 0; parse_block patches it to point
; to the main BEGIN block once all proc bodies have been emitted ahead
; of it. The .PCD entry point stays at offset 0 (start of code = UJP).
        JSR     emit_UJP        ; A=patch_lo, scratch=patch_hi
        STA     main_jmp_patch
        LDA     scratch
        STA     main_jmp_patch+1
        JSR     parse_block
; Emit HALT at end of main program
        JSR     emit_HALT
        RTS

; ---------------------------------------------------------------------------
; parse_block — { const_part } { type_part } { var_part } { proc_part } statement_part
; ---------------------------------------------------------------------------
parse_block:
@again:
        LDA     tok_type
        CMP     #TOK_CONST
        BEQ     @const
        CMP     #TOK_TYPE
        BEQ     @type
        CMP     #TOK_VAR
        BEQ     @var
        CMP     #TOK_PROCEDURE
        BEQ     @proc
        CMP     #TOK_FUNCTION
        BEQ     @func
        CMP     #TOK_BEGIN
        BEQ     @body
        RTS                     ; nothing recognisable

@const:
        JSR     next_token
        JSR     parse_const_decls
        BRA     @again
@type:
        JSR     next_token
        JSR     parse_type_decls
        BRA     @again
@var:
        JSR     next_token
        JSR     parse_var_decls
        BRA     @again
@proc:
        JSR     next_token
        JSR     parse_proc_decl
        BRA     @again
@func:
        JSR     next_token
        JSR     parse_func_decl
        BRA     @again
@body:  ; If at global scope and the main UJP hasn't been patched yet, do so
        ; before emitting any main-body code. main_jmp_patch+1 = $FF means
        ; "already patched" (or no UJP emitted, e.g. early-error path).
        LDA     scope_depth
        BNE     @body_emit
        LDA     main_jmp_patch+1
        CMP     #$FF
        BEQ     @body_emit
        LDA     main_jmp_patch
        STA     tmp2
        LDA     main_jmp_patch+1
        STA     tmp2+1
        JSR     patch_jump
        LDA     #$FF
        STA     main_jmp_patch+1
@body_emit:
        JSR     parse_compound_stmt
        RTS

; ---------------------------------------------------------------------------
; parse_const_decls — name = [-]INT_LIT ;  { name = [-]INT_LIT ; }
; Each constant goes into the symbol table as SYM_CONST with the literal
; value stored at offsets 18-19; references emit LDCW with that value.
; ---------------------------------------------------------------------------
parse_const_decls:
@loop:
        LDA     tok_type
        CMP     #TOK_IDENT
        BEQ     @have_ident
        RTS                     ; done — no more constants
@have_ident:
; save const name into save_name_buf
        LDY     #15
@savename:
        LDA     ident_buf,y
        STA     save_name_buf,y
        DEY
        BPL     @savename
        JSR     next_token      ; consume name
; expect '='
        LDA     tok_type
        CMP     #TOK_EQ
        BNE     :+
        JSR     next_token
:       ; optional unary minus — record in scratch (next_token preserves it)
        LDA     #0
        STA     scratch
        LDA     tok_type
        CMP     #TOK_MINUS
        BNE     @no_minus
        LDA     #$FF
        STA     scratch
        JSR     next_token
@no_minus:
        ; expect INT literal
        LDA     tok_type
        CMP     #TOK_INT
        BEQ     @have_int
        ; bad value — store 0 and try to recover
        LDA     #0
        STA     tmp2
        STA     tmp2+1
        BRA     @add_const
@have_int:
        LDA     tok_ival_lo
        STA     tmp2
        LDA     tok_ival_hi
        STA     tmp2+1
        JSR     next_token      ; consume INT
@add_const:
; apply negate flag if set
        LDA     scratch
        BEQ     @no_neg
        SEC
        LDA     #0
        SBC     tmp2
        STA     tmp2
        LDA     #0
        SBC     tmp2+1
        STA     tmp2+1
@no_neg:
; install in symbol table — value lives at offsets 18-19
        JSR     swap_ident_save
        LDA     #SYM_CONST
        LDX     #TY_INT
        JSR     symtab_add
        JSR     swap_ident_save
; expect ';'
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     @loop
        JSR     next_token
        BRA     @loop

; ---------------------------------------------------------------------------
; parse_type_decls — name = type_spec ;  { ... }
; Each binding registers a SYM_TYPE entry whose offset-17 byte holds the
; base TY_* code.  parse_type_spec resolves later references back to this
; base type via symtab_find.
; ---------------------------------------------------------------------------
parse_type_decls:
@loop:
        LDA     tok_type
        CMP     #TOK_IDENT
        BEQ     :+
        RTS                     ; done — no more type decls
:       ; save type name into save_name_buf (parse_type_spec clobbers ident_buf)
        LDY     #15
@savename:
        LDA     ident_buf,y
        STA     save_name_buf,y
        DEY
        BPL     @savename
        JSR     next_token      ; consume name
; expect '='
        LDA     tok_type
        CMP     #TOK_EQ
        BNE     :+
        JSR     next_token
:       ; parse the type definition; A = TY_* code
        JSR     parse_type_spec
        STA     scratch         ; save base type code
; bring the saved name back into ident_buf for symtab_add
        JSR     swap_ident_save
; SYM_TYPE has no storage; offset bytes are unused (overwritten below
; for record types, which need to remember size + field-table location).
        LDA     #0
        STA     tmp2
        STA     tmp2+1
        LDA     #SYM_TYPE
        LDX     scratch
        JSR     symtab_add
; If this is a RECORD alias, stash record_size in bytes 18-19 and
; first_field/field_count in bytes 22-23 so later var-decls and field
; lookups can recover it.
        LDA     scratch
        CMP     #TY_RECORD
        BNE     @td_no_rec
        LDA     record_size
        LDY     #18
        STA     (tmp3),y
        LDA     record_size+1
        LDY     #19
        STA     (tmp3),y
        LDA     record_first_field
        LDY     #22
        STA     (tmp3),y
        LDA     record_field_count
        LDY     #23
        STA     (tmp3),y
@td_no_rec:
; expect ';'
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     @loop
        JSR     next_token
        BRA     @loop

; ---------------------------------------------------------------------------
; parse_var_decls — name { , name } : type ;  { ... }
; All names on one line share the same type, allocated in order.
; ---------------------------------------------------------------------------
parse_var_decls:
@decl_loop:
        LDA     tok_type
        CMP     #TOK_IDENT
        BEQ     :+
        RTS                     ; done — no more var decls
:       ; --- collect comma-separated names into var_name_buf ---
        LDA     #0
        STA     var_name_count
@collect:
        LDA     tok_type
        CMP     #TOK_IDENT
        BNE     @end_collect
        LDA     var_name_count
        CMP     #8
        BCS     @skip_save
        ; tmp2 = var_name_buf + count*16   (count<8 → fits without manual carry)
        ASL
        ASL
        ASL
        ASL                     ; *16
        CLC
        ADC     #<var_name_buf
        STA     tmp2
        LDA     #0
        ADC     #>var_name_buf
        STA     tmp2+1
        LDY     #15
@cp_in:
        LDA     ident_buf,y
        STA     (tmp2),y
        DEY
        BPL     @cp_in
        INC     var_name_count
@skip_save:
        JSR     next_token      ; consume name
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @end_collect
        JSR     next_token      ; consume ','
        BRA     @collect
@end_collect:
; expect ':'
        LDA     tok_type
        CMP     #TOK_COLON
        BNE     :+
        JSR     next_token
:       ; parse type — A = TY_*
        JSR     parse_type_spec
        STA     scratch         ; stash type
; --- add each collected name to symbol table ---
; NOTE: consume ';' AFTER the add loop so next_token doesn't clobber
; ident_buf before @cp_out restores the saved name.
        LDX     #0
@add_loop:
        CPX     var_name_count
        BCS     @post_add
        PHX                     ; preserve loop counter across symtab_add
; tmp2 = var_name_buf + X*16
        TXA
        ASL
        ASL
        ASL
        ASL
        CLC
        ADC     #<var_name_buf
        STA     tmp2
        LDA     #0
        ADC     #>var_name_buf
        STA     tmp2+1
; copy slot → ident_buf so symtab_add sees the right name
        LDY     #15
@cp_out:
        LDA     (tmp2),Y
        STA     ident_buf,y
        DEY
        BPL     @cp_out
; allocate storage — global if at top level, else from proc local AR
        LDA     scope_depth
        BEQ     @gv_alloc
; local scalar: tmp2 = local_alloc_off; bump by 2
        LDA     local_alloc_off
        STA     tmp2
        LDA     #0
        STA     tmp2+1
        CLC
        LDA     local_alloc_off
        ADC     #2
        STA     local_alloc_off
        BRA     @do_va_add
@gv_alloc:
        LDA     scratch
        CMP     #TY_ARRAY
        BEQ     @gv_array
        CMP     #TY_RECORD
        BEQ     @gv_record
        CMP     #TY_TEXT
        BEQ     @gv_text
        JSR     codegen_alloc_global    ; scalar: 2 bytes, offset in tmp2
        BRA     @do_va_add
@gv_array:
        JSR     codegen_alloc_array_global  ; array: right size, adj offset in tmp2
        BRA     @do_va_add
@gv_record:
        JSR     codegen_alloc_record_global ; record: record_size bytes, offset in tmp2
        BRA     @do_va_add
@gv_text:
        JSR     codegen_alloc_text_global   ; TEXT file: 168 bytes, offset in tmp2
@do_va_add:
        LDA     scratch         ; type code
        TAX
        LDA     #SYM_VAR
        JSR     symtab_add      ; tmp3 = pointer to new entry
; type-specific metadata in entry bytes 22-23
        LDA     scratch
        CMP     #TY_ARRAY
        BNE     @va_chk_rec
        LDA     array_elem_ty
        LDY     #22
        STA     (tmp3),y
        LDA     #2              ; elemsize = 2 (word)
        LDY     #23
        STA     (tmp3),y
        BRA     @va_done
@va_chk_rec:
        CMP     #TY_RECORD
        BNE     @va_done
        LDA     record_first_field
        LDY     #22
        STA     (tmp3),y
        LDA     record_field_count
        LDY     #23
        STA     (tmp3),y
@va_done:
        PLX
        INX
        JMP     @add_loop       ; jmp — body grew past bra range
@post_add:
; now safe to consume ';' — ident_buf already used
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     @pva_next
        JSR     next_token
@pva_next:
        JMP     @decl_loop

; swap 16 bytes between ident_buf and save_name_buf
swap_ident_save:
        LDY     #15
@sw:
        LDA     ident_buf,y
        LDX     save_name_buf,y
        STA     save_name_buf,y
        TXA
        STA     ident_buf,y
        DEY
        BPL     @sw
        RTS

; ---------------------------------------------------------------------------
; parse_type_spec — consume type token(s), return TY_* code in A
; ---------------------------------------------------------------------------
parse_type_spec:
        LDA     tok_type
        CMP     #TOK_IDENT
        BEQ     :+
        JMP     @not_ident
:
; First check for a user-defined SYM_TYPE alias — built-ins like
; INTEGER/CHAR/BOOLEAN aren't in the symtab so this lookup falls
; through harmlessly when there's no user binding.
        JSR     symtab_find
        BCC     @builtin
        LDY     #16
        LDA     (tmp3),y
        CMP     #SYM_TYPE
        BNE     @builtin
        LDY     #17
        LDA     (tmp3),y
        STA     scratch         ; preserve across next_token
; If alias resolves to a RECORD, copy the record metadata back into the
; record_* globals so the caller (parse_var_decls) sees it.
        CMP     #TY_RECORD
        BNE     @sty_no_rec
        LDY     #18
        LDA     (tmp3),y
        STA     record_size
        LDY     #19
        LDA     (tmp3),y
        STA     record_size+1
        LDY     #22
        LDA     (tmp3),y
        STA     record_first_field
        LDY     #23
        LDA     (tmp3),y
        STA     record_field_count
@sty_no_rec:
        JSR     next_token
        LDA     scratch
        RTS
@builtin:
; check for built-in type names
        LDA     ident_buf       ; length
        CMP     #7
        BNE     @chk3
; "INTEGER" (7 chars)
        LDA     ident_buf+1
        CMP     #'I'
        BNE     @chk_bool
        JSR     next_token
        LDA     #TY_INT
        RTS
@chk_bool:
        LDA     ident_buf+1
        CMP     #'B'
        BNE     @chk_str
        JSR     next_token
        LDA     #TY_BOOL
        RTS
@chk_str:
        JSR     next_token
        LDA     #TY_INT         ; default
        RTS
@chk3:
        CMP     #4
        BNE     @chk6
; 4-char built-in types: "CHAR" or "TEXT"
        LDA     ident_buf+1
        CMP     #'C'
        BNE     @chk_text
        JSR     next_token
        LDA     #TY_CHAR
        RTS
@chk_text:
        CMP     #'T'
        BNE     @chk3b
        JSR     next_token
        LDA     #TY_TEXT
        RTS
@chk3b:
        JSR     next_token
        LDA     #TY_INT
        RTS
@chk6:
        CMP     #6
        BNE     @default_type
; "STRING" (6)
        JSR     next_token
        LDA     #TY_STRING
        RTS
@default_type:
        JSR     next_token
        LDA     #TY_INT
        RTS
@not_ident:
        LDA     tok_type
        CMP     #TOK_ARRAY
        BNE     @not_array
; --- ARRAY [lo..hi] OF basetype ---
        JSR     next_token      ; consume ARRAY → expect '['
        LDA     tok_type
        CMP     #TOK_LBRACK
        BNE     :+
        JSR     next_token      ; consume '['
:
; parse lo bound (integer literal)
        LDA     tok_ival_lo
        STA     array_lo
        LDA     tok_ival_hi
        STA     array_lo+1
        LDA     tok_type
        CMP     #TOK_INT
        BNE     :+
        JSR     next_token      ; consume lo
:
; expect '..'
        LDA     tok_type
        CMP     #TOK_DOTDOT
        BNE     :+
        JSR     next_token      ; consume '..'
:
; parse hi bound
        LDA     tok_ival_lo
        STA     array_hi
        LDA     tok_ival_hi
        STA     array_hi+1
        LDA     tok_type
        CMP     #TOK_INT
        BNE     :+
        JSR     next_token      ; consume hi
:
; expect ']'
        LDA     tok_type
        CMP     #TOK_RBRACK
        BNE     :+
        JSR     next_token      ; consume ']'
:
; expect 'OF'
        LDA     tok_type
        CMP     #TOK_OF
        BNE     :+
        JSR     next_token      ; consume 'OF'
:
; parse element type (recursive — handles named types, built-ins)
        JSR     parse_type_spec
        STA     array_elem_ty
        LDA     #TY_ARRAY
        RTS
@not_array:
        LDA     tok_type
        CMP     #TOK_RECORD
        BEQ     :+
        JMP     @not_record
:
; --- RECORD field { ; field } END ---
; Each field group: name { , name } : type
; All fields are stored as 2-byte slots; offsets are 0, 2, 4, ...
        JSR     next_token              ; consume RECORD
        LDA     #0
        STA     record_size
        STA     record_size+1
        STA     record_field_count
        LDA     field_table_count
        STA     record_first_field
@rec_loop:
        LDA     tok_type
        CMP     #TOK_END
        BNE     :+
        JMP     @rec_end
:
        CMP     #TOK_IDENT
        BEQ     :+
        JMP     @rec_end                ; safety — bail on unexpected token
:
; --- collect comma-separated field names into var_name_buf ---
        LDA     #0
        STA     var_name_count
@rec_collect:
        LDA     tok_type
        CMP     #TOK_IDENT
        BNE     @rec_endcol
        LDA     var_name_count
        CMP     #8
        BCS     @rec_skip_save
        ASL
        ASL
        ASL
        ASL                             ; *16
        CLC
        ADC     #<var_name_buf
        STA     tmp2
        LDA     #0
        ADC     #>var_name_buf
        STA     tmp2+1
        LDY     #15
@rec_cpin:
        LDA     ident_buf,y
        STA     (tmp2),y
        DEY
        BPL     @rec_cpin
        INC     var_name_count
@rec_skip_save:
        JSR     next_token              ; consume name
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @rec_endcol
        JSR     next_token              ; consume ','
        BRA     @rec_collect
@rec_endcol:
; expect ':'
        LDA     tok_type
        CMP     #TOK_COLON
        BNE     :+
        JSR     next_token
:       ; parse field type — recurse; result in A
        JSR     parse_type_spec
        STA     scratch                 ; field type code
; --- add each collected name to field_table at current record_size ---
        LDX     #0
@rec_addf:
        CPX     var_name_count
        BCS     @rec_endf
        PHX
        TXA
        ASL
        ASL
        ASL
        ASL                             ; *16
        CLC
        ADC     #<var_name_buf
        STA     tmp2
        LDA     #0
        ADC     #>var_name_buf
        STA     tmp2+1
        LDY     #15
@rec_cpout:
        LDA     (tmp2),y
        STA     ident_buf,y
        DEY
        BPL     @rec_cpout
; field_table_add: A=offset (record_size lo), X=type
        LDA     record_size
        LDX     scratch
        JSR     field_table_add
        INC     record_field_count
; advance record_size by 2
        CLC
        LDA     record_size
        ADC     #2
        STA     record_size
        BCC     :+
        INC     record_size+1
:
        PLX
        INX
        JMP     @rec_addf
@rec_endf:
; expect ';' between groups, or END
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     @rec_check_end
        JSR     next_token              ; consume ';'
        JMP     @rec_loop
@rec_check_end:
        LDA     tok_type
        CMP     #TOK_END
        BEQ     :+
        JMP     @rec_loop               ; recover — try again
:
@rec_end:
        JSR     next_token              ; consume END
        LDA     #TY_RECORD
        RTS
@not_record:
        JSR     next_token
        LDA     #TY_INT
        RTS

; ---------------------------------------------------------------------------
; parse_arg_lvalue — used for VAR-by-reference call arguments.  Expects the
; current token to be an IDENT naming a variable; consumes the ident and
; emits an opcode that pushes the variable's address.
;
;   SYM_VAR    global → OP_LDA_G <wordoff>
;   SYM_VAR    local  → OP_LDA_L <byteoff>
;   SYM_VARREF (passing a VAR param through) → OP_LDL <byteoff>   (the
;              local slot itself already holds the upstream address)
;
; Anything else (literal, function call, undefined name) is silently ignored
; — the argument slot will hold whatever was last on the stack.  Pascal
; semantics say it's a compile error, but we emit no diagnostic for now.
; ---------------------------------------------------------------------------
parse_arg_lvalue:
        LDA     tok_type
        CMP     #TOK_IDENT
        BEQ     :+
        RTS
:
        JSR     symtab_find
        BCS     :+
        RTS                     ; undefined — emit nothing
:       ; Snapshot kind, scope, offset before next_token clobbers tmp3.
        LDY     #16
        LDA     (tmp3),y
        PHA                     ; kind
        LDY     #20
        LDA     (tmp3),y
        PHA                     ; scope
        LDY     #19
        LDA     (tmp3),y
        PHA                     ; off hi
        LDY     #18
        LDA     (tmp3),y
        PHA                     ; off lo (top)
        JSR     next_token      ; consume ident
        PLA                     ; off lo
        TAX
        PLA                     ; off hi
        STA     scratch
        PLA                     ; scope
        TAY
        PLA                     ; kind
        CMP     #SYM_VARREF
        BEQ     @lv_passthrough
        CPY     #0
        BNE     @lv_local
        TXA
        JMP     emit_LDA_G
@lv_local:
        TXA
        JMP     emit_LDA_L
@lv_passthrough:
        TXA
        JMP     emit_LDL        ; slot already holds caller's address

; ---------------------------------------------------------------------------
; parse_param_list — assumes '(' has already been consumed.  Parses zero or
; more comma/semicolon-separated parameter groups, then ')'.  Each parameter
; is added to symtab as SYM_VAR with offset = proc_param_count*2 and the
; group's type code.  proc_param_count is incremented per param.
;
; Pascal supports two separator forms:
;   ( A, B: INTEGER )            — names sharing one type, COMMA separated
;   ( A: INTEGER; B: CHAR )      — different types, SEMICOLON between groups
;   ( A, B: INTEGER; C: CHAR )   — both forms mixed
;
; Reuses var_name_buf (128 bytes / 8 names per group).  Safe because the
; param list is fully parsed before parse_block runs (which is what calls
; parse_var_decls).
; ---------------------------------------------------------------------------
parse_param_list:
        LDA     #0
        STA     param_var_mask  ; reset bitmap (set by caller too — paranoia)
@group_loop:
; A leading VAR keyword marks the entire group as by-reference.
        LDA     #0
        STA     group_is_var
        LDA     tok_type
        CMP     #TOK_VAR
        BNE     @no_var_kw
        LDA     #1
        STA     group_is_var
        JSR     next_token      ; consume VAR
@no_var_kw:
        LDA     tok_type
        CMP     #TOK_IDENT
        BEQ     @have_ident     ; jmp trampoline — body is too big for bra
        JMP     @done_params
@have_ident:
; --- collect comma-separated names of this group ---
        LDA     #0
        STA     var_name_count
@gcollect:
        LDA     tok_type
        CMP     #TOK_IDENT
        BNE     @gend
        LDA     var_name_count
        CMP     #8
        BCS     @gskip          ; buffer full — silently drop name
        ASL
        ASL
        ASL
        ASL
        CLC
        ADC     #<var_name_buf
        STA     tmp2
        LDA     #0
        ADC     #>var_name_buf
        STA     tmp2+1
        LDY     #15
@gcp:
        LDA     ident_buf,y
        STA     (tmp2),y
        DEY
        BPL     @gcp
        INC     var_name_count
@gskip:
        JSR     next_token      ; consume name
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @gend
        JSR     next_token      ; consume ','
        BRA     @gcollect
@gend:
; expect ':'
        LDA     tok_type
        CMP     #TOK_COLON
        BNE     :+
        JSR     next_token
:
        JSR     parse_type_spec
        STA     scratch         ; stash type for this group
; --- add each collected name as SYM_VAR ---
        LDX     #0
@addloop:
        CPX     var_name_count
        BCS     @groupdone
        PHX
; copy var_name_buf[X*16] → ident_buf
        TXA
        ASL
        ASL
        ASL
        ASL
        CLC
        ADC     #<var_name_buf
        STA     tmp2
        LDA     #0
        ADC     #>var_name_buf
        STA     tmp2+1
        LDY     #15
@cpout:
        LDA     (tmp2),y
        STA     ident_buf,y
        DEY
        BPL     @cpout
; offset for this param = proc_param_count * 2
        LDA     proc_param_count
        ASL
        STA     tmp2
        LDA     #0
        STA     tmp2+1
; If this is a VAR group, set the matching bit in param_var_mask
; and use SYM_VARREF instead of SYM_VAR.
        LDA     group_is_var
        BEQ     @use_var_kind
; set bit (1 << proc_param_count) in param_var_mask (max 8 params)
        LDY     proc_param_count
        CPY     #8
        BCS     @skip_mask      ; >8 params: silently drop bit
        LDA     #1
@shl:
        CPY     #0
        BEQ     @done_shl
        ASL
        DEY
        BRA     @shl
@done_shl:
        ORA     param_var_mask
        STA     param_var_mask
@skip_mask:
        LDA     scratch         ; type
        TAX
        LDA     #SYM_VARREF
        BRA     @do_add
@use_var_kind:
        LDA     scratch         ; type
        TAX
        LDA     #SYM_VAR
@do_add:
        JSR     symtab_add
        INC     proc_param_count
        PLX
        INX
        JMP     @addloop        ; bra would be out of range now
@groupdone:
; SEMICOLON → another group; else fall through to ')'
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     @done_params
        JSR     next_token      ; consume ';'
        JMP     @group_loop
@done_params:
        LDA     tok_type
        CMP     #TOK_RPAREN
        BNE     :+
        JSR     next_token      ; consume ')'
:
        RTS

; ---------------------------------------------------------------------------
; parse_proc_decl — PROCEDURE name [ ( params ) ] ; <block> ;
; The PROCEDURE keyword has already been consumed by parse_block @proc.
;
; Phase B: value parameters supported.  Each parameter occupies 2 bytes in
; the AR at MP+AR_LOCALS+offset and is stored into the symbol table as
; SYM_VAR in the inner scope.  Param-count is recorded on the proc's symbol
; entry (offset 21) so callers know how many args to push.
; ---------------------------------------------------------------------------
parse_proc_decl:
; expect identifier (proc name)
        LDA     tok_type
        CMP     #TOK_IDENT
        BEQ     @have_name
        RTS
@have_name:
; Save outer local_alloc_off on 6502 stack — nested procs would
; otherwise clobber the enclosing proc's allocator state.
        LDA     local_alloc_off
        PHA

; save proc name into save_name_buf (ident_buf gets clobbered later)
        LDY     #15
@savename:
        LDA     ident_buf,y
        STA     save_name_buf,y
        DEY
        BPL     @savename

        JSR     next_token      ; consume name

; Register proc in symbol table NOW so we have an entry to update
; with the param count once the param list has been parsed.
; Offset (proc entry point) = current cg_pc.
; Remember the symbol-table index for backpatch.
        LDA     symtab_count
        STA     proc_entry_idx
        LDA     symtab_count+1
        STA     proc_entry_idx+1
        JSR     swap_ident_save ; ident_buf <-> save_name_buf
        LDA     cg_pc
        STA     tmp2
        LDA     cg_pc+1
        STA     tmp2+1
        LDA     #SYM_PROC
        LDX     #TY_NONE
        JSR     symtab_add
        JSR     swap_ident_save

; Enter scope: params and any locals belong to the proc's frame.
        JSR     symtab_enter_scope

; init param count and VAR-mask
        LDA     #0
        STA     proc_param_count
        STA     param_var_mask

; Optional parameter list
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @backpatch_pcount
        JSR     next_token      ; consume '('
        JSR     parse_param_list

@backpatch_pcount:
; Write param count into proc's symbol entry (offset 21).
; entry addr = SYMTAB_BASE + proc_entry_idx * 32
        LDA     proc_entry_idx
        ASL
        ASL
        ASL
        ASL
        ASL                     ; *32 (low byte)
        CLC
        ADC     #<SYMTAB_BASE
        STA     tmp3
        LDA     proc_entry_idx+1
        ROL
        ADC     #>SYMTAB_BASE
        STA     tmp3+1
        LDY     #21
        LDA     proc_param_count
        STA     (tmp3),y
        LDY     #22
        LDA     param_var_mask
        STA     (tmp3),y

; Initialize local_alloc_off to start right after the params.
; VAR declarations inside the body extend this; final value is
; backpatched into entry offset 23 below as the MRKSTK size.
        LDA     proc_param_count
        ASL
        STA     local_alloc_off

; expect ';'
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     :+
        JSR     next_token
:
; No callee prologue: the caller emits MRKSTK + arg-stores + CALL,
; so by the time we land at the proc entry the activation record
; (and its param slots) already exists.

; Recurse: proc body may have its own const/type/var/proc/begin block.
        JSR     parse_block

; Epilogue
        JSR     emit_RET

; Backpatch total local-area size (params + declared locals) into
; the proc's symbol entry at offset 23.  The caller reads this and
; emits MRKSTK <lsize> so the AR is large enough to hold all locals
; below pm_sp (otherwise value-stack pushes corrupt local slots).
        LDA     proc_entry_idx
        ASL
        ASL
        ASL
        ASL
        ASL
        CLC
        ADC     #<SYMTAB_BASE
        STA     tmp3
        LDA     proc_entry_idx+1
        ROL
        ADC     #>SYMTAB_BASE
        STA     tmp3+1
        LDY     #23
        LDA     local_alloc_off
        STA     (tmp3),y

        JSR     symtab_leave_scope

; Consume trailing ';' after END
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     :+
        JSR     next_token
:
        PLA
        STA     local_alloc_off ; restore outer allocator state
        RTS

; ---------------------------------------------------------------------------
; parse_func_decl — FUNCTION name [ ( params ) ] : RETURN_TYPE ; <block> ;
; The FUNCTION keyword has already been consumed by parse_block @func.
; Mirrors parse_proc_decl but registers SYM_FUNC, parses the return type,
; adds a SYM_RETVAL inner-scope binding (so `name := expr` in the body
; emits OP_STR), and emits OP_RETF as the epilogue.
; ---------------------------------------------------------------------------
parse_func_decl:
; expect identifier (func name)
        LDA     tok_type
        CMP     #TOK_IDENT
        BEQ     @have_name
        RTS
@have_name:
; Save outer local_alloc_off on 6502 stack — nested func/proc would
; otherwise clobber the enclosing frame's allocator state.
        LDA     local_alloc_off
        PHA

; save func name into save_name_buf
        LDY     #15
@savename:
        LDA     ident_buf,y
        STA     save_name_buf,y
        DEY
        BPL     @savename

        JSR     next_token      ; consume name

; Register SYM_FUNC NOW so we have an entry to backpatch.
; Type backpatched after we parse `: RETURN_TYPE`.
        LDA     symtab_count
        STA     proc_entry_idx
        LDA     symtab_count+1
        STA     proc_entry_idx+1
        JSR     swap_ident_save ; ident_buf <-> save_name_buf
        LDA     cg_pc
        STA     tmp2
        LDA     cg_pc+1
        STA     tmp2+1
        LDA     #SYM_FUNC
        LDX     #TY_NONE        ; placeholder
        JSR     symtab_add
        JSR     swap_ident_save

; Enter scope: params, locals, and SYM_RETVAL belong to fn frame.
        JSR     symtab_enter_scope

; init param count and VAR-mask
        LDA     #0
        STA     proc_param_count
        STA     param_var_mask

; Optional parameter list
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @after_params
        JSR     next_token      ; consume '('
        JSR     parse_param_list

@after_params:
; Expect ':' then return type
        LDA     tok_type
        CMP     #TOK_COLON
        BNE     :+
        JSR     next_token
:
        JSR     parse_type_spec
        STA     scratch         ; save return type code

; Recompute SYM_FUNC entry pointer from proc_entry_idx and backpatch
; offset 17 (return type) and offset 21 (param count).
        LDA     proc_entry_idx
        ASL
        ASL
        ASL
        ASL
        ASL
        CLC
        ADC     #<SYMTAB_BASE
        STA     tmp3
        LDA     proc_entry_idx+1
        ROL
        ADC     #>SYMTAB_BASE
        STA     tmp3+1
        LDY     #17
        LDA     scratch
        STA     (tmp3),y
        LDY     #21
        LDA     proc_param_count
        STA     (tmp3),y
        LDY     #22
        LDA     param_var_mask
        STA     (tmp3),y

; Initialize local_alloc_off to start right after the params.
; VAR declarations in the body extend this; final value is
; backpatched into entry offset 23 as the MRKSTK size.
        LDA     proc_param_count
        ASL
        STA     local_alloc_off

; Add SYM_RETVAL binding: same name as function; assignments to
; this name in the body emit OP_STR (writes MP+AR_RET_VAL).
; save_name_buf still holds the function name (parse_param_list uses
; var_name_buf, not save_name_buf).  Swap it into ident_buf.
        JSR     swap_ident_save
        LDA     #0
        STA     tmp2
        STA     tmp2+1
        LDA     #SYM_RETVAL
        LDX     #TY_NONE
        JSR     symtab_add

; Expect ';' before body
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     :+
        JSR     next_token
:
        JSR     parse_block

        JSR     emit_RETF

; Backpatch total local-area size into the func's symbol entry at
; offset 23 (params + declared body locals).  Caller emits MRKSTK
; with this value so the AR holds all locals below pm_sp.
        LDA     proc_entry_idx
        ASL
        ASL
        ASL
        ASL
        ASL
        CLC
        ADC     #<SYMTAB_BASE
        STA     tmp3
        LDA     proc_entry_idx+1
        ROL
        ADC     #>SYMTAB_BASE
        STA     tmp3+1
        LDY     #23
        LDA     local_alloc_off
        STA     (tmp3),y

        JSR     symtab_leave_scope

; Consume trailing ';' after END
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     :+
        JSR     next_token
:
        PLA
        STA     local_alloc_off ; restore outer allocator state
        RTS

; ---------------------------------------------------------------------------
; parse_compound_stmt — BEGIN { statement ; } END
; ---------------------------------------------------------------------------
parse_compound_stmt:
; consume BEGIN
        LDA     tok_type
        CMP     #TOK_BEGIN
        BNE     @done
        JSR     next_token
@loop:
        LDA     tok_type
        CMP     #TOK_END
        BEQ     @end
        CMP     #TOK_EOF
        BEQ     @done
; remember tok before parse_statement to detect no-progress
        STA     tmp3
        JSR     parse_statement
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BEQ     @consume_semi
        CMP     #TOK_END
        BEQ     @end
        CMP     #TOK_EOF
        BEQ     @done
; if parse_statement didn't consume anything, force-advance to avoid
; an infinite loop on an unrecognised token
        CMP     tmp3
        BNE     @loop
        JSR     next_token
        BRA     @loop
@consume_semi:
        JSR     next_token
        BRA     @loop
@end:
        JSR     next_token      ; consume END
@done:
        RTS

; ---------------------------------------------------------------------------
; parse_statement — dispatch based on current token
; ---------------------------------------------------------------------------
parse_statement:
        LDA     tok_type
        CMP     #TOK_BEGIN
        BEQ     @compound
        CMP     #TOK_IF
        BEQ     @if
        CMP     #TOK_WHILE
        BEQ     @while
        CMP     #TOK_FOR
        BEQ     @for
        CMP     #TOK_REPEAT
        BEQ     @repeat
        CMP     #TOK_IDENT
        BEQ     @assign_or_call
        RTS                     ; empty statement

@compound:
        JMP     parse_compound_stmt

@assign_or_call:
        JSR     parse_assign_or_call
        RTS

@if:
        JSR     parse_if
        RTS

@while:
        JSR     parse_while
        RTS

@for:
        JSR     parse_for
        RTS

@repeat:
        JSR     parse_repeat
        RTS

; ---------------------------------------------------------------------------
; parse_assign_or_call — identifier already consumed by caller (in ident_buf)
; If next is ':=' → assignment
; If next is '(' → procedure call
; If identifier is WRITE/WRITELN → built-in I/O
; ---------------------------------------------------------------------------
parse_assign_or_call:
; Dispatch built-in I/O procedures by length+initial letter.  RECORD/REPEAT
; are reserved keywords so they never reach this dispatch as identifiers.
;   len 4 : READ
;   len 5 : WRITE  | RESET  | CLOSE
;   len 6 : READLN | ASSIGN
;   len 7 : WRITELN| REWRITE
        LDA     ident_buf       ; length
        CMP     #4
        BEQ     @chk_read
        CMP     #5
        BEQ     @chk_len5
        CMP     #6
        BEQ     @chk_len6
        CMP     #7
        BEQ     @chk_len7
        JMP     @lookup_sym

@chk_len5:
        LDA     ident_buf+1
        CMP     #'W'
        BEQ     @chk_write
        CMP     #'R'
        BEQ     @chk_reset
        CMP     #'C'
        BEQ     @chk_close
        JMP     @lookup_sym

@chk_len6:
        LDA     ident_buf+1
        CMP     #'R'
        BEQ     @chk_readln
        CMP     #'A'
        BEQ     @chk_assign
        JMP     @lookup_sym

@chk_len7:
        LDA     ident_buf+1
        CMP     #'W'
        BEQ     @chk_writeln
        CMP     #'R'
        BEQ     @chk_rewrite
        JMP     @lookup_sym

@chk_write:
; "WRITE" (5)
        JSR     next_token
        JSR     parse_write_args        ; X=0 console, X=1 file (file ptr left)
        CPX     #0
        BEQ     @w_done
        JSR     emit_POP                ; discard leftover file ptr
@w_done:
        RTS

@chk_writeln:
; "WRITELN" (7) — first ident_buf char already known 'W'
        JSR     next_token
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @just_nl
        JSR     parse_write_args
        CPX     #0
        BEQ     @just_nl
        JMP     emit_FWLN               ; file newline; consumes file ptr
@just_nl:
        JMP     emit_WRITLN

@chk_read:
; "READ" (4)
        LDA     ident_buf+1
        CMP     #'R'
        BNE     @lookup_sym_jmp
        JSR     next_token
        JSR     parse_read_args
        CPX     #0
        BEQ     @r_done
        JSR     emit_POP                ; discard leftover file ptr
@r_done:
        RTS

@chk_readln:
; "READLN" (6).  Console: read each var.  File: read each var, then skip EOL.
        JSR     next_token
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @rd_done
        JSR     parse_read_args
        CPX     #0
        BEQ     @rd_done
        JSR     emit_FRDLN              ; consumes file ptr, skips to EOL
@rd_done:
        RTS

@chk_assign:
; "ASSIGN" (6)
        JMP     parse_builtin_assign
@chk_reset:
; "RESET" (5)
        JMP     parse_builtin_reset
@chk_rewrite:
; "REWRITE" (7)
        JMP     parse_builtin_rewrite
@chk_close:
; "CLOSE" (5)
        JMP     parse_builtin_close

@lookup_sym_jmp:
        JMP     @lookup_sym

@lookup_sym:
; look up identifier in symbol table
        JSR     symtab_find
        BCS     @found_sym
; undefined — error
        LDA     #<err_undef
        STA     tmp0
        LDA     #>err_undef
        STA     tmp0+1
        JSR     compile_error
; skip to semicolon / end
        JSR     next_token
        RTS

@found_sym:
; tmp3 = symbol entry pointer. Snapshot the fields we need
; before any next_token / parse_expression call clobbers tmp3.
        LDY     #16
        LDA     (tmp3),y
        STA     sym_save_kind
        LDY     #17
        LDA     (tmp3),y
        STA     sym_save_type
        LDY     #20
        LDA     (tmp3),y
        STA     sym_save_scope
        LDY     #18
        LDA     (tmp3),y
        STA     sym_save_off
        LDY     #19
        LDA     (tmp3),y
        STA     sym_save_off+1
        LDY     #21
        LDA     (tmp3),y
        STA     sym_save_pcount
        LDY     #22
        LDA     (tmp3),y
        STA     sym_save_vmask
        LDY     #23
        LDA     (tmp3),y
        STA     sym_save_lsize
        LDA     sym_save_kind
        CMP     #SYM_VAR
        BEQ     @do_assign
        CMP     #SYM_PROC
        BNE     :+
        JMP     @do_call
:
        CMP     #SYM_RETVAL
        BEQ     @do_retval_assign
        CMP     #SYM_VARREF
        BEQ     @do_varref_assign
        RTS

@do_retval_assign:
; `funcname := expr` inside function body — emit OP_STR which writes
; the popped value to the current AR's AR_RET_VAL slot.
        JSR     next_token      ; consume function name
        LDA     tok_type
        CMP     #TOK_ASSIGN
        BNE     :+
        JSR     next_token      ; consume ':='
:
        JSR     parse_expression
        JSR     emit_STR
        RTS

@do_varref_assign:
; VAR-by-reference param assignment: `paramname := expr` where the
; param's local slot holds an address.  Emit:
;   LDL <off>          ; push address (the value passed by caller)
;   <expr>             ; push value
;   STIND              ; pop val, pop addr, store
        JSR     next_token      ; consume param name
        LDA     tok_type
        CMP     #TOK_ASSIGN
        BNE     :+
        JSR     next_token      ; consume ':='
:
        LDA     sym_save_off
        JSR     emit_LDL        ; push address from local slot
        JSR     parse_expression
        JSR     emit_STIND
        RTS

@do_assign:
        JSR     next_token      ; consume variable name identifier
; check for compound-type access (array subscript or record field)
        LDA     sym_save_type
        CMP     #TY_ARRAY
        BEQ     @array_assign
        CMP     #TY_RECORD
        BNE     :+
        JMP     @record_assign
:
; scalar assignment — expect ':='
        LDA     tok_type
        CMP     #TOK_ASSIGN
        BNE     :+
        JSR     next_token
:       JSR     parse_expression
        LDA     sym_save_scope
        BNE     @local_store
        LDA     sym_save_off+1
        STA     scratch
        LDA     sym_save_off
        JSR     emit_STG
        RTS
@local_store:
        LDA     sym_save_off
        JSR     emit_STL
        RTS

@array_assign:
; a[i] := expr
; emit base-address push
        LDA     sym_save_scope
        BNE     @aarr_local
        LDA     sym_save_off+1
        STA     scratch
        LDA     sym_save_off
        JSR     emit_LDA_G
        BRA     @aarr_idx
@aarr_local:
        LDA     sym_save_off
        JSR     emit_LDA_L
@aarr_idx:
; consume '[', parse index expr, consume ']'
        LDA     tok_type
        CMP     #TOK_LBRACK
        BNE     :+
        JSR     next_token
:       JSR     parse_expression
        LDA     tok_type
        CMP     #TOK_RBRACK
        BNE     :+
        JSR     next_token
:
; emit INDEX 2 0  (elemsize = 2 words)
        LDA     #0
        STA     scratch
        LDA     #2
        JSR     emit_INDEX
; consume ':='
        LDA     tok_type
        CMP     #TOK_ASSIGN
        BNE     :+
        JSR     next_token
:
; parse and emit RHS expression
        JSR     parse_expression
; store: NOS=element_addr TOS=value → STIND
        JSR     emit_STIND
        RTS

@record_assign:
; v.field := expr
; emit base-address push
        LDA     sym_save_scope
        BNE     @rec_la_local
        LDA     sym_save_off+1
        STA     scratch
        LDA     sym_save_off
        JSR     emit_LDA_G
        BRA     @rec_la_dot
@rec_la_local:
        LDA     sym_save_off
        JSR     emit_LDA_L
@rec_la_dot:
; consume '.'
        LDA     tok_type
        CMP     #TOK_DOT
        BNE     :+
        JSR     next_token
:
; field name now in ident_buf — look up
        LDA     sym_save_vmask  ; first_field_idx
        LDX     sym_save_lsize  ; field_count
        JSR     field_lookup_in_record
        BCS     :+
        JMP     @rec_la_undef
:
; A=field offset (0..); X=field type (unused on LHS)
        BEQ     @rec_la_no_off
        JSR     emit_LDCI
        JSR     emit_ADI
@rec_la_no_off:
        JSR     next_token      ; consume field name
; expect ':='
        LDA     tok_type
        CMP     #TOK_ASSIGN
        BNE     :+
        JSR     next_token
:
        JSR     parse_expression
        JSR     emit_STIND
        RTS
@rec_la_undef:
        LDA     #<err_undef
        STA     tmp0
        LDA     #>err_undef
        STA     tmp0+1
        JSR     compile_error
        RTS

@do_call:
; Phase B: value parameters (INTEGER words, 2 bytes each).
; sym_save_off       = proc's code address
; sym_save_pcount    = declared parameter count
;
; Calling sequence emitted:
;   MRKSTK <pcount*2>            ; reserves AR + param slots
;   for each arg i:
;       <expr>                   ; pushes value to TOS
;       STL <i*2>                ; pop into local slot i
;   CALL <proc>                  ; jump (saves IPC into AR)
;
; The procedure body reads params via LDL because they were added
; to the symbol table as SYM_VAR with scope=inner (proc) at
; offsets 0, 2, 4, ...
        JSR     next_token      ; consume the proc-name identifier
; emit MRKSTK with full local-area size (params + body locals).
; sym_save_lsize was captured at @found_sym from entry offset 23.
        LDA     sym_save_lsize
        JSR     emit_MRKSTK
; if no params, optionally consume "()" then emit CALL
        LDA     sym_save_pcount
        BEQ     @no_args
; expect '(' for arg list
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @emit_call      ; missing '(' — caller mistake; emit CALL anyway
        JSR     next_token      ; consume '('
        LDX     #0              ; X = current arg index
@arg_loop:
; If sym_save_vmask bit X is set, this arg is VAR (push address).
; Otherwise parse_expression (push value).
        PHX
        LDA     #1
@vbit:
        CPX     #0
        BEQ     @vdone
        ASL
        DEX
        BRA     @vbit
@vdone:
        PLX                     ; restore loop counter
        AND     sym_save_vmask
        BEQ     @arg_value
        PHX
        JSR     parse_arg_lvalue
        PLX
        BRA     @arg_stored
@arg_value:
        PHX
        JSR     parse_expression
        PLX
@arg_stored:
; emit STL <X*2> — store popped value (or address) into local slot X
        TXA
        ASL
        JSR     emit_STL
        INX
        CPX     sym_save_pcount
        BCS     @done_args      ; X >= pcount — done
; expect comma between args
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @done_args
        JSR     next_token      ; consume ','
        JMP     @arg_loop       ; jmp — body grew past bra range
@done_args:
        LDA     tok_type
        CMP     #TOK_RPAREN
        BNE     @emit_call
        JSR     next_token      ; consume ')'
        BRA     @emit_call
@no_args:
; No declared params — accept and skip "()" if present.
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @emit_call
        JSR     next_token      ; consume '('
        LDA     tok_type
        CMP     #TOK_RPAREN
        BNE     @emit_call
        JSR     next_token      ; consume ')'
@emit_call:
; Emit CALL <delta>: A=target lo, scratch=target hi
        LDA     sym_save_off+1
        STA     scratch
        LDA     sym_save_off
        JSR     emit_CALL
        RTS

; ---------------------------------------------------------------------------
; parse_write_args — WRITE/WRITELN argument list
; ---------------------------------------------------------------------------
parse_write_args:
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @done
        JSR     next_token
@arg:
        LDA     tok_type
        CMP     #TOK_RPAREN
        BEQ     @close
        CMP     #TOK_EOF
        BEQ     @done
        JSR     parse_expression
; Dispatch WRIT* based on expr_type set by parse_expression.
        LDA     expr_type
        CMP     #TY_STRING
        BNE     :+
        JSR     emit_WRITS
        BRA     @next
:
        CMP     #TY_CHAR
        BNE     :+
        JSR     emit_WRITC
        BRA     @next
:
        CMP     #TY_BOOL
        BNE     :+
        JSR     emit_WRITB
        BRA     @next
:
        JSR     emit_WRITI      ; default: integer
@next:
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @close
        JSR     next_token
        BRA     @arg
@close:
        JSR     next_token      ; consume ')'
@done:
        RTS

; ---------------------------------------------------------------------------
; parse_read_args — READ/READLN argument list.  Each argument must be a
; variable identifier.  For each, push the variable's address (LDA_G /
; LDA_L / LDL passthrough for VAR-ref params) then emit OP_READI which
; pops the address, reads a line from the console, parses a decimal
; integer, and stores it as a 16-bit word at that address.
; ---------------------------------------------------------------------------
parse_read_args:
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @rdone
        JSR     next_token      ; consume '('
@rarg:
        LDA     tok_type
        CMP     #TOK_RPAREN
        BEQ     @rclose
        CMP     #TOK_EOF
        BEQ     @rdone
        CMP     #TOK_IDENT
        BNE     @rclose         ; not an ident — bail
        JSR     parse_arg_lvalue; push address of var
        JSR     emit_READI      ; pop addr, read int, store word
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @rclose
        JSR     next_token
        BRA     @rarg
@rclose:
        LDA     tok_type
        CMP     #TOK_RPAREN
        BNE     @rdone
        JSR     next_token      ; consume ')'
@rdone:
        RTS

; ---------------------------------------------------------------------------
; parse_if — IF expr THEN stmt [ ELSE stmt ]
; ---------------------------------------------------------------------------
parse_if:
        JSR     next_token      ; consume IF
        JSR     parse_expression
; emit FJP with placeholder; save patch addr on 6502 stack
; (parse_statement will clobber tmp2 / scratch)
        JSR     emit_FJP        ; A=patch_lo, scratch=patch_hi
        PHA                     ; save FJP patch lo
        LDA     scratch
        PHA                     ; save FJP patch hi
; THEN
        LDA     tok_type
        CMP     #TOK_THEN
        BNE     :+
        JSR     next_token
:
        JSR     parse_statement
; check for ELSE
        LDA     tok_type
        CMP     #TOK_ELSE
        BNE     @patch_then
; emit UJP over else branch
        JSR     emit_UJP        ; A=patch_lo, scratch=patch_hi
        PHA                     ; save UJP patch lo
        LDA     scratch
        PHA                     ; save UJP patch hi
; restore FJP patch addr into tmp2 and patch it to here
; (need to dig under the UJP entries on the stack)
; stack top → bottom: UJP_hi, UJP_lo, FJP_hi, FJP_lo
        PLA                     ; UJP hi
        STA     sym_save_off    ; reuse sym_save_off as temp byte
        PLA                     ; UJP lo
        STA     sym_save_off+1
        PLA                     ; FJP hi
        STA     tmp2+1
        PLA                     ; FJP lo
        STA     tmp2
        JSR     patch_jump      ; patch FJP → start of else branch
; push UJP patch addr back for after-else patching
        LDA     sym_save_off+1  ; UJP lo
        PHA
        LDA     sym_save_off    ; UJP hi
        PHA
        JSR     next_token      ; consume ELSE
        JSR     parse_statement
; patch UJP to here
        PLA                     ; UJP hi
        STA     tmp2+1
        PLA                     ; UJP lo
        STA     tmp2
        JSR     patch_jump
        RTS
@patch_then:
; no ELSE — restore FJP patch addr and patch to here
        PLA                     ; FJP hi
        STA     tmp2+1
        PLA                     ; FJP lo
        STA     tmp2
        JSR     patch_jump
        RTS

; ---------------------------------------------------------------------------
; parse_while — WHILE expr DO stmt
; ---------------------------------------------------------------------------
parse_while:
        JSR     next_token      ; consume WHILE
; save loop top
        LDA     cg_pc
        STA     tmp2
        LDA     cg_pc+1
        STA     tmp2+1
; we'll need loop_top later; save on 6502 stack
        LDA     tmp2+1
        PHA
        LDA     tmp2
        PHA
        JSR     parse_expression
        JSR     emit_FJP        ; exit if false
        PHA
        LDA     scratch
        PHA
; DO
        LDA     tok_type
        CMP     #TOK_DO
        BNE     :+
        JSR     next_token
:
        JSR     parse_statement
; emit UJP back to loop top
        LDA     #OP_UJP
        JSR     emit_byte
; offset = loop_top - (cg_pc + 2)
        PLA
        STA     tmp3            ; FJP patch hi
        PLA
        STA     tmp3+1          ; FJP patch lo — note: reversed from earlier push
; compute back-edge offset
        PLA                     ; loop_top lo
        STA     tmp2
        PLA                     ; loop_top hi
        STA     tmp2+1
; delta = loop_top - (cg_pc + 2)
        SEC
        LDA     tmp2
        SBC     cg_pc
        STA     scratch
        LDA     tmp2+1
        SBC     cg_pc+1
        STA     scratch+1
        LDA     scratch
        SEC
        SBC     #2
        STA     scratch
        BCS     :+
        DEC     scratch+1
:
        LDA     scratch
        JSR     emit_byte
        LDA     scratch+1
        JSR     emit_byte
; patch FJP exit
; tmp3+1:tmp3 holds FJP patch addr (note push order was lo,hi → pla gets hi first)
; Rebuild properly:
        LDA     tmp3+1
        STA     tmp2
        LDA     tmp3
        STA     tmp2+1
        JSR     patch_jump
        RTS

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
        JSR     next_token      ; consume FOR

        LDA     tok_type
        CMP     #TOK_IDENT
        BEQ     :+
        RTS                     ; not an ident — silently bail
:
        JSR     symtab_find
        BCS     :+
        JSR     next_token      ; consume the bad ident
        RTS
:
; snapshot var info into sym_save_*
        LDY     #20
        LDA     (tmp3),y
        STA     sym_save_scope
        LDY     #18
        LDA     (tmp3),y
        STA     sym_save_off
        LDY     #19
        LDA     (tmp3),y
        STA     sym_save_off+1
        JSR     next_token      ; consume identifier

; push var info: scope, off_hi, off_lo (top)
        LDA     sym_save_scope
        PHA
        LDA     sym_save_off+1
        PHA
        LDA     sym_save_off
        PHA

; ':='
        LDA     tok_type
        CMP     #TOK_ASSIGN
        BNE     :+
        JSR     next_token
:
        JSR     parse_expression; initial value on TOS

; restore sym_save_* (parse_expression may have clobbered them)
        PLA
        STA     sym_save_off
        PLA
        STA     sym_save_off+1
        PLA
        STA     sym_save_scope
        JSR     for_emit_store_var

; re-push var info for use after loop
        LDA     sym_save_scope
        PHA
        LDA     sym_save_off+1
        PHA
        LDA     sym_save_off
        PHA

; direction byte: 0 = TO, 1 = DOWNTO
        LDA     #0
        LDX     tok_type
        CPX     #TOK_DOWNTO
        BNE     :+
        LDA     #1
:
        PHA                     ; push dir
        JSR     next_token      ; consume TO / DOWNTO

; parse limit expression and store to a hidden global
        JSR     parse_expression
        JSR     codegen_alloc_global; tmp2 = hidden offset
; push the offset NOW — emit_STG will clobber tmp2
        LDA     tmp2+1
        PHA                     ; lim_hi
        LDA     tmp2
        PHA                     ; lim_lo (top)
; reload from stack peek for emit_STG (A = lo, scratch = hi)
        TSX
        LDA     $0102,x
        STA     scratch
        LDA     $0101,x
        JSR     emit_STG

; consume DO
        LDA     tok_type
        CMP     #TOK_DO
        BNE     :+
        JSR     next_token
:
; --- loop top ---
        LDA     cg_pc
        STA     for_loop_top
        LDA     cg_pc+1
        STA     for_loop_top+1

; emit LDG/LDL var — restore sym_save_* via TSX peek
; stack from top: lim_lo, lim_hi, dir, var_lo, var_hi, var_scope
        TSX
        LDA     $0104,x         ; var_lo
        STA     sym_save_off
        LDA     $0105,x         ; var_hi
        STA     sym_save_off+1
        LDA     $0106,x         ; var_scope
        STA     sym_save_scope
        JSR     for_emit_load_var

; emit LDG limit — limit hi at $0102,x, lo at $0101,x
        TSX
        LDA     $0102,x
        STA     scratch
        LDA     $0101,x
        JSR     emit_LDG

; comparison: TO → LEQI (var <= limit), DOWNTO → GEQI
        TSX
        LDA     $0103,x         ; dir
        BEQ     @cmp_to
        JSR     emit_GEQI
        BRA     @cmp_done
@cmp_to:
        JSR     emit_LEQI
@cmp_done:

; FJP exit placeholder
        JSR     emit_FJP        ; A=patch_lo, scratch=patch_hi
        STA     for_patch
        LDA     scratch
        STA     for_patch+1

; --- body ---
        JSR     parse_statement

; --- pop state in reverse push order ---
        PLA                     ; lim_lo (discard)
        PLA                     ; lim_hi (discard)
        PLA                     ; direction
        STA     sym_save_kind   ; reuse for direction storage
        PLA                     ; var_lo
        STA     sym_save_off
        PLA                     ; var_hi
        STA     sym_save_off+1
        PLA                     ; var_scope
        STA     sym_save_scope

; emit increment / decrement: LDG var, LDCI 1, ADI|SBI, STG var
        JSR     for_emit_load_var
        LDA     #1
        JSR     emit_LDCI
        LDA     sym_save_kind   ; direction
        BEQ     @inc
        JSR     emit_SBI
        BRA     @inc_done
@inc:
        JSR     emit_ADI
@inc_done:
        JSR     for_emit_store_var

; emit UJP back to loop_top
; offset = for_loop_top - (cg_pc_after_opcode + 2)
        LDA     #OP_UJP
        JSR     emit_byte
        SEC
        LDA     for_loop_top
        SBC     cg_pc
        STA     scratch
        LDA     for_loop_top+1
        SBC     cg_pc+1
        STA     scratch+1
        SEC
        LDA     scratch
        SBC     #2
        STA     scratch
        BCS     :+
        DEC     scratch+1
:
        LDA     scratch
        JSR     emit_byte
        LDA     scratch+1
        JSR     emit_byte

; patch FJP exit
        LDA     for_patch
        STA     tmp2
        LDA     for_patch+1
        STA     tmp2+1
        JSR     patch_jump
        RTS

; ---------------------------------------------------------------------------
; for_emit_load_var / for_emit_store_var — emit LDG/LDL or STG/STL based on
; sym_save_scope (0 = global) and sym_save_off (offset).
; ---------------------------------------------------------------------------
for_emit_load_var:
        LDA     sym_save_scope
        BNE     @local
        LDA     sym_save_off+1
        STA     scratch
        LDA     sym_save_off
        JMP     emit_LDG
@local:
        LDA     sym_save_off
        JMP     emit_LDL

for_emit_store_var:
        LDA     sym_save_scope
        BNE     @local
        LDA     sym_save_off+1
        STA     scratch
        LDA     sym_save_off
        JMP     emit_STG
@local:
        LDA     sym_save_off
        JMP     emit_STL

; ---------------------------------------------------------------------------
; parse_repeat — REPEAT stmt { ; stmt } UNTIL expr
; ---------------------------------------------------------------------------
parse_repeat:
        JSR     next_token      ; consume REPEAT
; save top
        LDA     cg_pc
        PHA
        LDA     cg_pc+1
        PHA
@loop:
        LDA     tok_type
        CMP     #TOK_UNTIL
        BEQ     @until
        CMP     #TOK_EOF
        BEQ     @done
        JSR     parse_statement
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     @loop
        JSR     next_token
        BRA     @loop
@until:
        JSR     next_token      ; consume UNTIL
        JSR     parse_expression
; FJP back to top
        LDA     #OP_FJP
        JSR     emit_byte
        PLA
        STA     tmp2+1
        PLA
        STA     tmp2
        SEC
        LDA     tmp2
        SBC     cg_pc
        STA     scratch
        LDA     tmp2+1
        SBC     cg_pc+1
        STA     scratch+1
        LDA     scratch
        SEC
        SBC     #2
        STA     scratch
        BCS     :+
        DEC     scratch+1
:
        LDA     scratch
        JSR     emit_byte
        LDA     scratch+1
        JSR     emit_byte
@done:
        RTS

; ---------------------------------------------------------------------------
; parse_expression — full expression with operator precedence
; Phase 1: simple additive + comparison; no precedence climbing yet
; ---------------------------------------------------------------------------
parse_expression:
        JSR     parse_simple_expr
; check for relational operator
        LDA     tok_type
        CMP     #TOK_EQ
        BEQ     @rel_eq
        CMP     #TOK_NEQ
        BEQ     @rel_neq
        CMP     #TOK_LT
        BEQ     @rel_lt
        CMP     #TOK_GT
        BEQ     @rel_gt
        CMP     #TOK_LEQ
        BEQ     @rel_leq
        CMP     #TOK_GEQ
        BEQ     @rel_geq
        RTS                     ; expr_type set by parse_simple_expr
@rel_eq:
        JSR     next_token
        JSR     parse_simple_expr
        JSR     emit_EQUI
        JMP     @rel_done
@rel_neq:
        JSR     next_token
        JSR     parse_simple_expr
        JSR     emit_NEQI
        JMP     @rel_done
@rel_lt:
        JSR     next_token
        JSR     parse_simple_expr
        JSR     emit_LESI
        JMP     @rel_done
@rel_gt:
        JSR     next_token
        JSR     parse_simple_expr
        JSR     emit_GTRI
        JMP     @rel_done
@rel_leq:
        JSR     next_token
        JSR     parse_simple_expr
        JSR     emit_LEQI
        JMP     @rel_done
@rel_geq:
        JSR     next_token
        JSR     parse_simple_expr
        JSR     emit_GEQI
@rel_done:
        LDA     #TY_BOOL
        STA     expr_type
        RTS

; ---------------------------------------------------------------------------
; parse_simple_expr — term { (+|-|OR) term }
; ---------------------------------------------------------------------------
parse_simple_expr:
; check for unary minus
        LDA     tok_type
        CMP     #TOK_MINUS
        BNE     :+
        JSR     next_token
        JSR     parse_term
        JSR     emit_NGI
        LDA     #TY_INT
        STA     expr_type
        RTS
:
        JSR     parse_term
@addop:
        LDA     tok_type
        CMP     #TOK_PLUS
        BEQ     @add
        CMP     #TOK_MINUS
        BEQ     @sub
        CMP     #TOK_OR
        BEQ     @or
        RTS
@add:
        JSR     next_token
        JSR     parse_term
        JSR     emit_ADI
        LDA     #TY_INT
        STA     expr_type
        BRA     @addop
@sub:
        JSR     next_token
        JSR     parse_term
        JSR     emit_SBI
        LDA     #TY_INT
        STA     expr_type
        BRA     @addop
@or:
        JSR     next_token
        JSR     parse_term
        JSR     emit_LOR
        LDA     #TY_BOOL
        STA     expr_type
        BRA     @addop

; ---------------------------------------------------------------------------
; parse_term — factor { (*|DIV|MOD|AND) factor }
; ---------------------------------------------------------------------------
parse_term:
        JSR     parse_factor
@mulop:
        LDA     tok_type
        CMP     #TOK_STAR
        BEQ     @mul
        CMP     #TOK_DIV
        BEQ     @div
        CMP     #TOK_DIV_KW
        BEQ     @div
        CMP     #TOK_MOD_KW
        BEQ     @mod
        CMP     #TOK_AND
        BEQ     @and
        RTS
@mul:
        JSR     next_token
        JSR     parse_factor
        JSR     emit_MPI
        LDA     #TY_INT
        STA     expr_type
        BRA     @mulop
@div:
        JSR     next_token
        JSR     parse_factor
        JSR     emit_DVI
        LDA     #TY_INT
        STA     expr_type
        BRA     @mulop
@mod:
        JSR     next_token
        JSR     parse_factor
        JSR     emit_MOD
        LDA     #TY_INT
        STA     expr_type
        BRA     @mulop
@and:
        JSR     next_token
        JSR     parse_factor
        JSR     emit_LAND
        LDA     #TY_BOOL
        STA     expr_type
        BRA     @mulop

TOK_DIV_KW      = TOK_DIV       ; alias

; ---------------------------------------------------------------------------
; parse_factor — literal | variable | ( expr ) | NOT factor
; ---------------------------------------------------------------------------
parse_factor:
        LDA     tok_type
        CMP     #TOK_INT
        BNE     :+
        JMP     @int_lit
:
        CMP     #TOK_CHAR
        BNE     :+
        JMP     @char_lit
:
        CMP     #TOK_STRING
        BNE     :+
        JMP     @str_lit
:
        CMP     #TOK_IDENT
        BNE     :+
        JMP     @ident_or_call
:
        CMP     #TOK_LPAREN
        BNE     :+
        JMP     @paren
:
        CMP     #TOK_NOT
        BNE     :+
        JMP     @not_expr
:
        CMP     #TOK_NIL
        BNE     :+
        JMP     @nil
:
        RTS

@int_lit:
        LDA     tok_ival_lo
        PHA
        LDA     tok_ival_hi
        BNE     @big_int
        PLA
        BMI     @big_int_lo     ; lo's bit 7 set — LDCI would sign-extend wrongly
        JSR     emit_LDCI
        JSR     next_token
        LDA     #TY_INT
        STA     expr_type
        RTS
@big_int_lo:
        PHA
        LDA     #0
@big_int:
        STA     scratch
        PLA
        JSR     emit_LDCW
        JSR     next_token
        LDA     #TY_INT
        STA     expr_type
        RTS

@char_lit:
        LDA     tok_ival_lo
        JSR     emit_LDCC
        JSR     next_token
        LDA     #TY_CHAR
        STA     expr_type
        RTS

@str_lit:
; Inline string: emit OP_LDCS + length + chars.  Runtime pushes the
; address of the length byte as a Pascal-string pointer.
        JSR     emit_LDCS
        JSR     next_token
        LDA     #TY_STRING
        STA     expr_type
        RTS

@ident_or_call:
; First check for string built-ins (LENGTH/POS/COPY/CONCAT) by length+initial.
; These take precedence over user identifiers (matching WRITE/READ handling
; in parse_assign_or_call).
        LDA     ident_buf
        CMP     #3
        BEQ     @bi_chk_pos
        CMP     #4
        BEQ     @bi_chk_copy
        CMP     #6
        BEQ     @bi_chk_len_or_cat
        BRA     @do_lookup
@bi_chk_pos:
        LDA     ident_buf+1
        CMP     #'P'
        BNE     @do_lookup
        LDA     ident_buf+2
        CMP     #'O'
        BNE     @do_lookup
        LDA     ident_buf+3
        CMP     #'S'
        BNE     @do_lookup
        JMP     parse_builtin_pos
@bi_chk_copy:
        LDA     ident_buf+1
        CMP     #'C'
        BNE     @do_lookup
        LDA     ident_buf+2
        CMP     #'O'
        BNE     @do_lookup
        LDA     ident_buf+3
        CMP     #'P'
        BNE     @do_lookup
        LDA     ident_buf+4
        CMP     #'Y'
        BNE     @do_lookup
        JMP     parse_builtin_copy
@bi_chk_len_or_cat:
        LDA     ident_buf+1
        CMP     #'L'
        BEQ     @bi_try_length
        CMP     #'C'
        BNE     @do_lookup
; CONCAT? check 'O','N','C','A','T'
        LDA     ident_buf+2
        CMP     #'O'
        BNE     @do_lookup
        LDA     ident_buf+3
        CMP     #'N'
        BNE     @do_lookup
        JMP     parse_builtin_concat
@bi_try_length:
        LDA     ident_buf+2
        CMP     #'E'
        BNE     @do_lookup
        LDA     ident_buf+3
        CMP     #'N'
        BNE     @do_lookup
        JMP     parse_builtin_length
@do_lookup:
; Look up BEFORE next_token — next_token may overwrite ident_buf
; if the following token is an identifier or keyword (e.g. J THEN).
        JSR     symtab_find
        BCS     @sym_ok
        JSR     next_token      ; consume ident
        LDA     #0
        STA     scratch
        JSR     emit_LDCW       ; undefined: emit 0
        LDA     #TY_INT
        STA     expr_type
        RTS
@sym_ok:
; Snapshot kind/type/scope/offset/pcount onto the 6502 stack —
; sym_save_* would conflict with parse_assign_or_call when this
; @ident_or_call runs inside a nested parse_expression.
        LDY     #22
        LDA     (tmp3),y
        STA     fcall_vmask     ; (only meaningful for SYM_FUNC)
        LDY     #23
        LDA     (tmp3),y
        STA     fcall_lsize     ; full local-area size for MRKSTK
        LDY     #21
        LDA     (tmp3),y
        PHA                     ; pcount (deepest)
        LDY     #16
        LDA     (tmp3),y
        PHA                     ; kind
        LDY     #17
        LDA     (tmp3),y
        STA     expr_type       ; data type code
        LDY     #20
        LDA     (tmp3),y
        PHA                     ; scope
        LDY     #19
        LDA     (tmp3),y
        PHA                     ; off hi
        LDY     #18
        LDA     (tmp3),y
        PHA                     ; off lo (top)
        JSR     next_token      ; consume ident (may overwrite ident_buf/tmp3)
        PLA                     ; off lo
        TAX                     ; X = off lo
        PLA                     ; off hi
        STA     scratch         ; scratch = off hi
        PLA                     ; scope
        TAY                     ; Y = scope (preserved across kind test)
        PLA                     ; kind
        CMP     #SYM_FUNC
        BNE     :+
        JMP     @sym_func_call
:
        CMP     #SYM_CONST
        BNE     :+
        JMP     @const_emit
:
        CMP     #SYM_VARREF
        BNE     :+
        JMP     @varref_load
:
; SYM_VAR (default; PROC also falls through here for now)
        PLA                     ; discard pcount
        CPY     #0
        BNE     @local_load
; global SYM_VAR — check for array subscript, record field, or TEXT file
        LDA     expr_type
        CMP     #TY_ARRAY
        BEQ     @arr_rhs_global
        CMP     #TY_RECORD
        BEQ     @rec_rhs_global
        CMP     #TY_TEXT
        BEQ     @text_rhs_global
        TXA
        JSR     emit_LDG
        RTS
@text_rhs_global:
        TXA                     ; push struct address, leave expr_type=TY_TEXT
        JMP     emit_LDA_G
@arr_rhs_global:
        TXA                     ; A=off_lo, scratch=off_hi still valid
        JSR     emit_LDA_G
        BRA     @arr_rhs_index
@rec_rhs_global:
        TXA                     ; A=off_lo, scratch=off_hi
        JSR     emit_LDA_G
        BRA     @rec_rhs_field
@local_load:
        LDA     expr_type
        CMP     #TY_ARRAY
        BEQ     @arr_rhs_local
        CMP     #TY_RECORD
        BEQ     @rec_rhs_local
        CMP     #TY_TEXT
        BEQ     @text_rhs_local
        TXA
        JSR     emit_LDL
        RTS
@text_rhs_local:
        TXA
        JMP     emit_LDA_L
@arr_rhs_local:
        TXA
        JSR     emit_LDA_L
@arr_rhs_index:
; parse [index], emit INDEX, LDIND
        LDA     tok_type
        CMP     #TOK_LBRACK
        BNE     :+
        JSR     next_token
:       JSR     parse_expression    ; clobbers expr_type, scratch, tmp*
        LDA     tok_type
        CMP     #TOK_RBRACK
        BNE     :+
        JSR     next_token
:       LDA     #0
        STA     scratch
        LDA     #2
        JSR     emit_INDEX
        JSR     emit_LDIND
        LDA     #TY_INT
        STA     expr_type
        RTS
@rec_rhs_local:
        TXA
        JSR     emit_LDA_L
@rec_rhs_field:
; consume '.'
        LDA     tok_type
        CMP     #TOK_DOT
        BNE     :+
        JSR     next_token
:
; field name now in ident_buf — look up
        LDA     fcall_vmask     ; first_field_idx (saved from byte 22)
        LDX     fcall_lsize     ; field_count (saved from byte 23)
        JSR     field_lookup_in_record
        BCC     @rec_rhs_nf
        PHA                     ; save offset
        TXA
        STA     scratch         ; save field type (preserved across emits)
        PLA                     ; offset back; PLA sets Z for the BEQ below
        BEQ     @rec_rhs_no_off
        JSR     emit_LDCI
        JSR     emit_ADI
@rec_rhs_no_off:
        JSR     next_token      ; consume field name
        JSR     emit_LDIND
        LDA     scratch
        STA     expr_type       ; field's type drives WRITE dispatch
        RTS
@rec_rhs_nf:
        LDA     #<err_undef
        STA     tmp0
        LDA     #>err_undef
        STA     tmp0+1
        JSR     compile_error
        JSR     next_token      ; consume bad field name
        JSR     emit_LDIND      ; keep stack balanced
        LDA     #TY_INT
        STA     expr_type
        RTS
@varref_load:
; SYM_VARREF: local slot holds an address. Emit LDL then LDIND.
        PLA                     ; discard pcount
        TXA
        JSR     emit_LDL        ; push address from slot
        JSR     emit_LDIND      ; deref → push word
        RTS
@const_emit:
; CONST: offset bytes hold the literal value. X=lo, scratch=hi.
        PLA                     ; discard pcount
        TXA
        JSR     emit_LDCW
        RTS
@sym_func_call:
; SYM_FUNC: emit MRKSTK + arg-stores + CALL.  Result is left on TOS
; by op_RETF.  expr_type is the function's return type (already set).
; X=off lo, scratch=off hi, top of 6502 stack = pcount.
; NOTE: not nest-safe — calling a function inside another function
; call's argument list will clobber fcall_*.  TODO: fix later.
        STX     fcall_lo
        LDA     scratch
        STA     fcall_hi
        LDA     expr_type
        STA     fcall_type      ; preserve across parse_expression
        PLA                     ; pcount
        STA     fcall_pcount
; emit MRKSTK <full local-area size> (params + body locals).
; fcall_lsize was captured at @sym_ok from entry offset 23.
        LDA     fcall_lsize
        JSR     emit_MRKSTK
        LDA     fcall_pcount
        BEQ     @fc_no_args
; expect '('
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @fc_emit_call
        JSR     next_token      ; consume '('
        LDX     #0              ; X = arg index
@fc_arg_loop:
; Bit X of fcall_vmask → VAR (push address); else value.
        PHX
        LDA     #1
@fcvbit:
        CPX     #0
        BEQ     @fcvdone
        ASL
        DEX
        BRA     @fcvbit
@fcvdone:
        PLX
        AND     fcall_vmask
        BEQ     @fc_arg_value
        PHX
        JSR     parse_arg_lvalue
        PLX
        BRA     @fc_arg_stored
@fc_arg_value:
        PHX
        JSR     parse_expression
        PLX
@fc_arg_stored:
        TXA
        ASL
        JSR     emit_STL        ; store popped value (or address) into local slot X
        INX
        CPX     fcall_pcount
        BCS     @fc_done_args
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @fc_done_args
        JSR     next_token      ; consume ','
        JMP     @fc_arg_loop    ; jmp — body grew past bra range
@fc_done_args:
        LDA     tok_type
        CMP     #TOK_RPAREN
        BNE     @fc_emit_call
        JSR     next_token      ; consume ')'
        BRA     @fc_emit_call
@fc_no_args:
; Accept optional "()"
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @fc_emit_call
        JSR     next_token
        LDA     tok_type
        CMP     #TOK_RPAREN
        BNE     @fc_emit_call
        JSR     next_token
@fc_emit_call:
        LDA     fcall_hi
        STA     scratch
        LDA     fcall_lo
        JSR     emit_CALL
; restore expr_type (parse_expression in args may have changed it)
        LDA     fcall_type
        STA     expr_type
        RTS

@paren:
        JSR     next_token      ; consume '('
        JSR     parse_expression
        LDA     tok_type
        CMP     #TOK_RPAREN
        BNE     :+
        JSR     next_token
:
        RTS                     ; expr_type already set by parse_expression

@not_expr:
        JSR     next_token
        JSR     parse_factor
        JSR     emit_LNOT
        LDA     #TY_BOOL
        STA     expr_type
        RTS

@nil:
        JSR     emit_LDCN
        JSR     next_token
        LDA     #TY_PTR
        STA     expr_type
        RTS

; ---------------------------------------------------------------------------
; String built-in helpers — entered from parse_factor's @ident_or_call.
; The identifier is still the current token on entry.
; ---------------------------------------------------------------------------

; Skip current token if it matches A; otherwise just fall through.
; Used to consume optional punctuation like '(' / ')' / ',' without aborting
; on a missing token (compile_error elsewhere will catch malformed code).
parse_eat_token:
        CMP     tok_type
        BNE     :+
        JMP     next_token
:       RTS

parse_builtin_length:
        JSR     next_token              ; consume LENGTH
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_expression        ; pushes string ptr
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        JSR     emit_LEN
        LDA     #TY_INT
        STA     expr_type
        RTS

parse_builtin_pos:
        JSR     next_token              ; consume POS
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_expression        ; substr
        LDA     #TOK_COMMA
        JSR     parse_eat_token
        JSR     parse_expression        ; mainstr
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        JSR     emit_POS
        LDA     #TY_INT
        STA     expr_type
        RTS

parse_builtin_copy:
        JSR     next_token              ; consume COPY
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_expression        ; src string
        LDA     #TOK_COMMA
        JSR     parse_eat_token
        JSR     parse_expression        ; index
        LDA     #TOK_COMMA
        JSR     parse_eat_token
        JSR     parse_expression        ; count
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        JSR     emit_COPY
        LDA     #TY_STRING
        STA     expr_type
        RTS

; CONCAT(s1, s2 [, s3, ...]) — emit chained CONCAT2.  Each pair-merge writes
; into the next round-robin work buffer, so 3+ args work as long as the
; intermediate buffer isn't recycled before being consumed.
parse_builtin_concat:
        JSR     next_token              ; consume CONCAT
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_expression        ; first arg on stack
        LDA     tok_type
        CMP     #TOK_COMMA
        BEQ     @cat_more
; Single-arg CONCAT — just leave the string on the stack (rare/edge case).
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        LDA     #TY_STRING
        STA     expr_type
        RTS
@cat_more:
        JSR     next_token              ; consume ','
        JSR     parse_expression        ; next arg
        JSR     emit_CONCAT             ; merge previous + next
        LDA     tok_type
        CMP     #TOK_COMMA
        BEQ     @cat_more
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        LDA     #TY_STRING
        STA     expr_type
        RTS
