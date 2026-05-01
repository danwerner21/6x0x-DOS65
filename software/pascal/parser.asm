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
; compile_program — top-level dispatcher for PROGRAM or UNIT sources.
; ---------------------------------------------------------------------------
compile_program:
        JSR     symtab_init
        LDA     #0
        STA     unit_section
        LDA     tok_type
        CMP     #TOK_UNIT
        BEQ     @unit_src
        JMP     @program_src
@unit_src:
        JMP     compile_unit
@program_src:
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
        JSR     patch_startup_chain_if_needed
        JSR     emit_HALT
        RTS

; patch_startup_chain_if_needed — patch any pending top-level startup jumps
; to the current cg_pc. This handles both the entry UJP (jump to the first
; unit-init/main body) and the tail UJP emitted after each prior top-level
; body (jump to the next one, or eventually HALT).
patch_startup_chain_if_needed:
        LDA     main_jmp_patch+1
        CMP     #$FF
        BEQ     @pscii_tail
        LDA     main_jmp_patch
        STA     tmp2
        LDA     main_jmp_patch+1
        STA     tmp2+1
        JSR     patch_jump
        LDA     #$FF
        STA     main_jmp_patch+1
@pscii_tail:
        LDA     body_chain_patch+1
        CMP     #$FF
        BEQ     @pscii_done
        LDA     body_chain_patch
        STA     tmp2
        LDA     body_chain_patch+1
        STA     tmp2+1
        JSR     patch_jump
        LDA     #$FF
        STA     body_chain_patch+1
@pscii_done:
        RTS

; ---------------------------------------------------------------------------
; compile_unit — UNIT name ; INTERFACE ... IMPLEMENTATION ... [BEGIN ...] END.
; UNIT sources may be standalone roots or USES-imported. The INTERFACE
; section registers globals and routine headings; IMPLEMENTATION provides the
; bodies plus an optional init block. Imported units compile inline into the
; current .PCD while the startup jump chain preserves init order.
; ---------------------------------------------------------------------------
compile_unit:
        JSR     next_token              ; consume UNIT
        LDA     tok_type
        CMP     #TOK_IDENT
        BNE     :+
        LDA     unit_import_mode
        BNE     @cu_name_done
        LDX     used_unit_count
        CPX     #USED_UNIT_SLOTS
        BCS     @cu_name_done
        PHX
        JSR     used_unit_slot_ptr
        LDY     #15
@cu_root_name_copy:
        LDA     ident_buf,y
        STA     (tmp2),y
        DEY
        BPL     @cu_root_name_copy
        PLX
        INC     used_unit_count
@cu_name_done:
        JSR     next_token              ; consume unit name
:       LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     :+
        JSR     next_token
:       LDA     tok_type
        CMP     #TOK_INTERFACE
        BEQ     @cu_have_iface
        LDA     #<err_unexpected
        STA     tmp0
        LDA     #>err_unexpected
        STA     tmp0+1
        JSR     compile_error
        BRA     @cu_after_iface_kw
@cu_have_iface:
        JSR     next_token              ; consume INTERFACE
@cu_after_iface_kw:
        LDA     unit_import_mode
        BNE     @cu_iface_ready
        JSR     emit_UJP
        STA     main_jmp_patch
        LDA     scratch
        STA     main_jmp_patch+1
@cu_iface_ready:
        LDA     #1
        STA     unit_section
        JSR     parse_interface_block
        LDA     tok_type
        CMP     #TOK_IMPLEMENTATION
        BEQ     @cu_have_impl
        LDA     #<err_unexpected
        STA     tmp0
        LDA     #>err_unexpected
        STA     tmp0+1
        JSR     compile_error
        BRA     @cu_impl_start
@cu_have_impl:
        JSR     next_token              ; consume IMPLEMENTATION
@cu_impl_start:
        LDA     #2
        STA     unit_section
        JSR     parse_block
        LDA     unit_import_mode
        BNE     @cu_done
        JSR     patch_startup_chain_if_needed
        JSR     emit_HALT
@cu_done:
        LDA     #0
        STA     unit_section
        RTS

; ---------------------------------------------------------------------------
; parse_interface_block — top-level INTERFACE declarations only.
; Mirrors parse_block but PROCEDURE/FUNCTION consume headings without bodies.
; Stops when IMPLEMENTATION (or anything unrecognised) is reached.
; ---------------------------------------------------------------------------
parse_interface_block:
@pib_again:
        LDA     tok_type
        CMP     #TOK_USES
        BEQ     @pib_uses
        CMP     #TOK_CONST
        BEQ     @pib_const
        CMP     #TOK_TYPE
        BEQ     @pib_type
        CMP     #TOK_VAR
        BEQ     @pib_var
        CMP     #TOK_PROCEDURE
        BEQ     @pib_proc
        CMP     #TOK_FUNCTION
        BEQ     @pib_func
        RTS
@pib_uses:
        JSR     parse_uses_clause
        BRA     @pib_again
@pib_const:
        JSR     next_token
        JSR     parse_const_decls
        BRA     @pib_again
@pib_type:
        JSR     next_token
        JSR     parse_type_decls
        BRA     @pib_again
@pib_var:
        JSR     next_token
        JSR     parse_var_decls
        BRA     @pib_again
@pib_proc:
        JSR     next_token
        JSR     parse_proc_iface_decl
        BRA     @pib_again
@pib_func:
        JSR     next_token
        JSR     parse_func_iface_decl
        BRA     @pib_again

; ---------------------------------------------------------------------------
; import_unit_by_name — ident_buf names a UNIT source file to compile into the
; current output. Repeated/nested imports are de-duplicated globally.
; ---------------------------------------------------------------------------
import_unit_by_name:
        LDX     #0
@iun_scan:
        CPX     used_unit_count
        BCS     @iun_add
        PHX
        JSR     used_unit_slot_ptr
        LDY     #0
        LDA     (tmp2),y
        CMP     ident_buf
        BNE     @iun_next
        STA     scratch
        LDY     #1
@iun_cmp:
        LDA     (tmp2),y
        CMP     ident_buf,y
        BNE     @iun_next
        DEC     scratch
        BEQ     @iun_found
        INY
        BRA     @iun_cmp
@iun_next:
        PLX
        INX
        BRA     @iun_scan
@iun_found:
        PLX
        RTS
@iun_add:
        CPX     #USED_UNIT_SLOTS
        BCC     :+
        LDA     #<err_too_many
        STA     tmp0
        LDA     #>err_too_many
        STA     tmp0+1
        JSR     compile_error
        RTS
:       PHX
        JSR     used_unit_slot_ptr
        LDY     #15
@iun_copy_name:
        LDA     ident_buf,y
        STA     (tmp2),y
        DEY
        BPL     @iun_copy_name
        PLX
        INC     used_unit_count
        JSR     source_push_current
        BCC     @iun_pushed
        LDA     #<err_too_many
        STA     tmp0
        LDA     #>err_too_many
        STA     tmp0+1
        JSR     compile_error
        RTS
@iun_pushed:
        JSR     build_named_src_fcb
        BCC     :+
        JSR     source_pop_current
        RTS
:       LDA     #<comp_src_fcb
        STA     src_fcb
        STA     tmp1
        LDA     #>comp_src_fcb
        STA     src_fcb+1
        STA     tmp1+1
        JSR     file_open
        CMP     #$FF
        BNE     @iun_open_ok
        LDA     #<err_nofile
        STA     tmp0
        LDA     #>err_nofile
        STA     tmp0+1
        JSR     compile_error
        JSR     source_pop_current
        RTS
@iun_open_ok:
        INC     unit_import_mode
        JSR     lexer_init
        JSR     next_token
        JSR     compile_unit
        DEC     unit_import_mode
        LDA     #<comp_src_fcb
        STA     tmp1
        LDA     #>comp_src_fcb
        STA     tmp1+1
        JSR     file_close
        JSR     source_pop_current
        RTS

; ---------------------------------------------------------------------------
; parse_uses_clause — top-level USES unit1, unit2, ... ;
; Collects the unit names from the current source first, then compiles each
; imported UNIT source while the current file context is suspended.
; ---------------------------------------------------------------------------
parse_uses_clause:
        JSR     next_token              ; consume USES
        LDA     #0
        STA     uses_name_count
@puc_collect:
        LDA     tok_type
        CMP     #TOK_IDENT
        BNE     @puc_done_collect
        LDX     uses_name_count
        CPX     #USES_NAME_SLOTS
        BCS     @puc_skip_store
        PHX
        JSR     uses_name_slot_ptr
        LDY     #15
@puc_store:
        LDA     ident_buf,y
        STA     (tmp2),y
        DEY
        BPL     @puc_store
        PLX
        INC     uses_name_count
        BRA     @puc_after_store
@puc_skip_store:
        LDA     #<err_too_many
        STA     tmp0
        LDA     #>err_too_many
        STA     tmp0+1
        JSR     compile_error
@puc_after_store:
        JSR     next_token              ; consume unit name
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @puc_done_collect
        JSR     next_token
        BRA     @puc_collect
@puc_done_collect:
        LDA     tok_type
        PHA
        LDX     #0
@puc_import_loop:
        CPX     uses_name_count
        BCS     @puc_resume
        PHX
        JSR     uses_name_slot_ptr
        LDY     #15
@puc_restore_name:
        LDA     (tmp2),y
        STA     ident_buf,y
        DEY
        BPL     @puc_restore_name
        JSR     import_unit_by_name
        PLX
        INX
        JMP     @puc_import_loop
@puc_resume:
        PLA
        CMP     #TOK_SEMICOLON
        BNE     @puc_no_semi
        JSR     next_token
        RTS
@puc_no_semi:
        STA     tok_type
        RTS

; ---------------------------------------------------------------------------
; parse_block — { const_part } { type_part } { var_part } { proc_part } statement_part
; ---------------------------------------------------------------------------
parse_block:
@again:
        LDA     tok_type
        CMP     #TOK_USES
        BNE     @blk_chk_const
        LDA     scope_depth
        BNE     @blk_chk_const
        JSR     parse_uses_clause
        BRA     @again
@blk_chk_const:
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
@body:  ; If at global scope, patch the startup chain to this body's start
        ; before emitting any top-level init/main code.
        LDA     scope_depth
        BNE     @body_emit
        JSR     patch_startup_chain_if_needed
@body_emit:
        JSR     parse_compound_stmt
        LDA     scope_depth
        BNE     @body_done
        JSR     emit_UJP
        STA     body_chain_patch
        LDA     scratch
        STA     body_chain_patch+1
@body_done:
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
        LDA     #TY_INT
        STA     expr_type
        ; expect INT/REAL literal
        LDA     tok_type
        CMP     #TOK_INT
        BEQ     @have_int
        CMP     #TOK_REAL
        BEQ     @have_real
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
        BRA     @add_const
@have_real:
        LDA     tok_ival_lo
        STA     tmp2
        LDA     tok_ival_hi
        STA     tmp2+1
        LDA     #TY_REAL
        STA     expr_type
        JSR     next_token      ; consume REAL
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
        LDX     expr_type
        JSR     symtab_add
        JSR     swap_ident_save
; expect ';'
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BEQ     @pcd_semi
        JMP     @loop
@pcd_semi:
        JSR     next_token
        JMP     @loop

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
        BEQ     @ptd_semi
        JMP     @loop
@ptd_semi:
        JSR     next_token
        JMP     @loop

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
        BCC     :+
        JMP     @post_add       ; out-of-range branch — use jmp
:
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
; local: tmp2 = local_alloc_off; bump by per-type size
        LDA     local_alloc_off
        STA     tmp2
        LDA     #0
        STA     tmp2+1
        LDA     scratch                 ; type code
        CMP     #TY_RECORD
        BEQ     @lv_rec
; default scalar / pointer / etc — 2 bytes
        CLC
        LDA     local_alloc_off
        ADC     #2
        STA     local_alloc_off
        BRA     @do_va_add
@lv_rec:
        CLC
        LDA     local_alloc_off
        ADC     record_size
        STA     local_alloc_off
        ; record_size+1 is normally 0 (records < 256 bytes) but
        ; propagate the carry to be safe.
        LDA     #0
        ADC     record_size+1
        BEQ     @do_va_add              ; no high-byte growth
        ; would overflow byte counter; not handled — abort cleanly
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

; field_name_slot_ptr â€” tmp2 = pointer to the current RECORD depth's
; field-name slot. A = slot index (0..7).
field_name_slot_ptr:
        ASL
        ASL
        ASL
        ASL                             ; slot * 16
        STA     tmp2
        LDA     #0
        STA     tmp2+1
        LDX     field_depth
        DEX                             ; 1-based depth -> 0-based slice index
@fnsp_depth:
        CPX     #0
        BEQ     @fnsp_base
        CLC
        LDA     tmp2
        ADC     #$80
        STA     tmp2
        BCC     :+
        INC     tmp2+1
:       DEX
        BRA     @fnsp_depth
@fnsp_base:
        CLC
        LDA     tmp2
        ADC     #<field_name_buf
        STA     tmp2
        LDA     tmp2+1
        ADC     #>field_name_buf
        STA     tmp2+1
        RTS

; uses_name_slot_ptr — tmp2 = pointer to USES-name slot X (0..7)
uses_name_slot_ptr:
        TXA
        ASL
        ASL
        ASL
        ASL
        CLC
        ADC     #<uses_name_buf
        STA     tmp2
        LDA     #0
        ADC     #>uses_name_buf
        STA     tmp2+1
        RTS

; used_unit_slot_ptr — tmp2 = pointer to imported-unit slot X (0..7)
used_unit_slot_ptr:
        TXA
        ASL
        ASL
        ASL
        ASL
        CLC
        ADC     #<used_unit_buf
        STA     tmp2
        LDA     #0
        ADC     #>used_unit_buf
        STA     tmp2+1
        RTS

; ptr_meta_alloc — create one pointer-type metadata entry from the most
; recently parsed pointee type. Input A = pointee TY_*.
; For pointee TY_RECORD / TY_ARRAY, the current record_* / array_* globals
; are consulted for size and aux metadata.
; Returns: ptr_meta_cur = allocated metadata index, or $FF if full.
ptr_meta_alloc:
        PHA
        LDX     ptr_meta_count
        CPX     #PTR_META_MAX
        BCC     :+
        PLA
        LDA     #$FF
        STA     ptr_meta_cur
        RTS
:       PLA
        STA     ptr_meta_type,x
        CMP     #TY_PTR
        BEQ     @pma_ptr
        CMP     #TY_RECORD
        BEQ     @pma_record
        CMP     #TY_ARRAY
        BEQ     @pma_array
        CMP     #TY_TEXT
        BEQ     @pma_text
@pma_scalar:
        LDA     #2
        STA     ptr_meta_size_lo,x
        LDA     #0
        STA     ptr_meta_size_hi,x
        STA     ptr_meta_aux0,x
        STA     ptr_meta_aux1,x
        STA     ptr_meta_aux2,x
        JMP     @pma_done
@pma_text:
        LDA     #168
        STA     ptr_meta_size_lo,x
        LDA     #0
        STA     ptr_meta_size_hi,x
        STA     ptr_meta_aux0,x
        STA     ptr_meta_aux1,x
        STA     ptr_meta_aux2,x
        JMP     @pma_done
@pma_ptr:
        LDA     #2
        STA     ptr_meta_size_lo,x
        LDA     #0
        STA     ptr_meta_size_hi,x
        LDA     ptr_meta_cur
        STA     ptr_meta_aux0,x
        LDA     #0
        STA     ptr_meta_aux1,x
        STA     ptr_meta_aux2,x
        JMP     @pma_done
@pma_record:
        LDA     record_size
        STA     ptr_meta_size_lo,x
        LDA     record_size+1
        STA     ptr_meta_size_hi,x
        LDA     record_first_field
        STA     ptr_meta_aux0,x
        LDA     record_field_count
        STA     ptr_meta_aux1,x
        LDA     #0
        STA     ptr_meta_aux2,x
        JMP     @pma_done
@pma_array:
; size = (hi - lo + 1) * 2  (all array elements are word-sized here)
        LDA     array_hi
        SEC
        SBC     array_lo
        STA     tmp0
        LDA     array_hi+1
        SBC     array_lo+1
        STA     tmp0+1
        INC     tmp0
        BNE     :+
        INC     tmp0+1
:       ASL     tmp0
        ROL     tmp0+1
        LDA     tmp0
        STA     ptr_meta_size_lo,x
        LDA     tmp0+1
        STA     ptr_meta_size_hi,x
        LDA     array_elem_ty
        STA     ptr_meta_aux0,x
        LDA     #2
        STA     ptr_meta_aux1,x
        LDA     #0
        STA     ptr_meta_aux2,x
@pma_done:
        TXA
        STA     ptr_meta_cur
        INC     ptr_meta_count
        RTS

; store_proc_param_type — record the current parameter's type code in the
; owning proc/func entry at byte 24+proc_param_count (up to 8 params).
store_proc_param_type:
        PHA
        LDY     proc_param_count
        CPY     #8
        BCS     @sppt_drop
        LDA     proc_entry_idx
        STA     tmp3
        LDA     proc_entry_idx+1
        STA     tmp3+1
        TYA
        CLC
        ADC     #24
        TAY
        PLA
        STA     (tmp3),y
        RTS
@sppt_drop:
        PLA
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
; 4-char built-in types: "CHAR", "TEXT", or "REAL"
        LDA     ident_buf+1
        CMP     #'C'
        BNE     @chk_text
        JSR     next_token
        LDA     #TY_CHAR
        RTS
@chk_text:
        CMP     #'T'
        BNE     @chk_real
        JSR     next_token
        LDA     #TY_TEXT
        RTS
@chk_real:
        CMP     #'R'
        BNE     @chk3b
        JSR     next_token
        LDA     #TY_REAL
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
        CMP     #TOK_SET
        BEQ     :+
        JMP     @not_set
:
; --- SET OF lo..hi ---
; v1 stores sets as 16-bit masks over element values 0..15, so the
; declared bounds must fit within that range. The bounds are validated
; here but not otherwise retained for later type-checking.
        JSR     next_token              ; consume SET
        LDA     tok_type
        CMP     #TOK_OF
        BNE     :+
        JSR     next_token              ; consume OF
:
        LDA     tok_type
        CMP     #TOK_INT
        BNE     @set_bad
        LDA     tok_ival_hi
        BNE     @set_bad
        LDA     tok_ival_lo
        STA     set_lit_lo
        CMP     #16
        BCS     @set_bad
        JSR     next_token
        LDA     tok_type
        CMP     #TOK_DOTDOT
        BNE     @set_bad
        JSR     next_token
        LDA     tok_type
        CMP     #TOK_INT
        BNE     @set_bad
        LDA     tok_ival_hi
        BNE     @set_bad
        LDA     tok_ival_lo
        STA     set_lit_hi
        CMP     #16
        BCS     @set_bad
        CMP     set_lit_lo
        BCC     @set_bad
        JSR     next_token
        LDA     #TY_SET
        RTS
@set_bad:
        LDA     #<err_type
        STA     tmp0
        LDA     #>err_type
        STA     tmp0+1
        JSR     compile_error
        LDA     tok_type
        CMP     #TOK_INT
        BNE     :+
        JSR     next_token
:       LDA     #TY_SET
        RTS
@not_set:
        LDA     tok_type
        CMP     #TOK_RECORD
        BEQ     :+
        JMP     @not_record
:
; --- RECORD field { ; field } END ---
; Each field group: name { , name } : type
; All fields are stored as 2-byte slots; offsets are 0, 2, 4, ...
        JSR     next_token              ; consume RECORD
        INC     field_depth             ; mark new fields as belonging
                                        ; to this nesting level
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
; --- collect comma-separated field names into field_name_buf ---
;     (separate from var_name_buf so an inline RECORD inside a VAR
;      decl doesn't clobber the outer variable names being collected)
        LDX     field_depth
        DEX
        LDA     #0
        STA     field_name_count,x
@rec_collect:
        LDA     tok_type
        CMP     #TOK_IDENT
        BNE     @rec_endcol
        LDX     field_depth
        DEX
        LDA     field_name_count,x
        CMP     #8
        BCS     @rec_skip_save
        JSR     field_name_slot_ptr
        LDY     #15
@rec_cpin:
        LDA     ident_buf,y
        STA     (tmp2),y
        DEY
        BPL     @rec_cpin
        LDX     field_depth
        DEX
        INC     field_name_count,x
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
:       ; parse field type — recurse; result in A.  Save outer record
; bookkeeping (size + first_field + field_count) so a nested RECORD
; can use the same globals without losing our state.  After the inner
; parse, capture the inner's metadata into nest_save_* and restore.
        LDA     record_size
        PHA
        LDA     record_size+1
        PHA
        LDA     record_first_field
        PHA
        LDA     record_field_count
        PHA
        JSR     parse_type_spec         ; may recurse into RECORD
        STA     scratch                 ; field type code
; capture inner's metadata for @rec_addf (only meaningful for TY_RECORD)
        LDA     record_size
        STA     nest_save_size
        LDA     record_size+1
        STA     nest_save_size+1
        LDA     record_first_field
        STA     nest_save_first
        LDA     record_field_count
        STA     nest_save_count
; restore outer record bookkeeping
        PLA
        STA     record_field_count
        PLA
        STA     record_first_field
        PLA
        STA     record_size+1
        PLA
        STA     record_size
; --- add each collected name to field_table at current record_size ---
        LDX     #0
@rec_addf:
        TXA
        LDY     field_depth
        DEY
        CMP     field_name_count,y
        BCS     @rec_endf
        PHX
        TXA
        JSR     field_name_slot_ptr
        LDY     #15
@rec_cpout:
        LDA     (tmp2),y
        STA     ident_buf,y
        DEY
        BPL     @rec_cpout
; If the previous field group's type was an inline RECORD, its fields
; got appended to field_table BEFORE this outer field. Bump
; record_first_field past those leading slots so the lookup-scan starts
; at the first outer-depth field instead of an inner one. Only applies
; when no outer fields have been recorded yet.
        LDA     record_field_count
        BNE     :+
        LDA     field_table_count
        STA     record_first_field
:
; field_table_add: A=offset (record_size lo), X=type
;   Returns: A = newly-assigned field index
        LDA     record_size
        LDX     scratch
        JSR     field_table_add
; If this field's type is TY_RECORD, fold the inner record's
; first_field/count (saved in nest_save_*) into the parallel
; nested arrays so chained access can traverse it.
        PHA                             ; save field index
        LDX     scratch                 ; field type
        CPX     #TY_RECORD
        BNE     @rec_no_nest
        TAX                             ; X = field index (A still holds it)
        LDA     nest_save_first
        STA     field_nested_first,x
        LDA     nest_save_count
        STA     field_nested_count,x
@rec_no_nest:
        PLA                             ; restore (no longer needed)
        INC     record_field_count
; advance record_size by this field's size:
;   record fields → nest_save_size (set by inner parse)
;   anything else → 2 (all scalars are word-sized)
        LDX     scratch
        CPX     #TY_RECORD
        BEQ     @rec_adv_rec
        CLC
        LDA     record_size
        ADC     #2
        STA     record_size
        BCC     :+
        INC     record_size+1
:       BRA     @rec_advanced
@rec_adv_rec:
        CLC
        LDA     record_size
        ADC     nest_save_size
        STA     record_size
        LDA     record_size+1
        ADC     nest_save_size+1
        STA     record_size+1
@rec_advanced:
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
        DEC     field_depth             ; leaving this nesting level
; record_field_count was per-named-field; recompute as the slot span so
; field_lookup_in_record covers any inline-record sub-fields that got
; physically interleaved between this record's named fields.
        SEC
        LDA     field_table_count
        SBC     record_first_field
        STA     record_field_count
        LDA     #TY_RECORD
        RTS
@not_record:
        LDA     tok_type
        CMP     #TOK_CARET
        BNE     @not_caret
; --- ^BASETYPE — pointer type. Element type recorded but not yet
; type-checked; v1 only supports pointer-to-INTEGER (2-byte cell).
        JSR     next_token              ; consume '^'
        JSR     parse_type_spec         ; consume base type, result in A
        ; (base type code intentionally discarded — bump allocator
        ;  always grants 2 bytes per NEW; revisit when records land)
        LDA     #TY_PTR
        RTS
@not_caret:
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
; with_lookup_field — search active WITH contexts for ident_buf as an
; unqualified field name. Innermost WITH wins.
; Returns: carry set if found, A=field offset, X=field type
;          carry clear if not found
; Side effects on hit:
;   with_lookup_off/type      = matched field offset/type
;   with_lookup_base_lo/hi    = hidden global holding the WITH base address
;   with_lookup_first/count   = nested-record metadata for this field
; ---------------------------------------------------------------------------
with_lookup_field:
        LDA     with_depth
        BNE     :+
        CLC
        RTS
:       LDX     with_depth
        DEX
@wlf_ctx:
        STX     tmp1
        LDY     with_field_count,x
        LDA     with_first_field,x
        STY     scratch
        LDX     scratch
        JSR     field_lookup_in_record
        BCC     @wlf_next
        STA     with_lookup_off
        TXA
        STA     with_lookup_type
        LDX     tmp3
        LDA     field_nested_first,x
        STA     with_lookup_first
        LDA     field_nested_count,x
        STA     with_lookup_count
        LDX     tmp1
        LDA     with_base_lo,x
        STA     with_lookup_base_lo
        LDA     with_base_hi,x
        STA     with_lookup_base_hi
        LDA     with_lookup_off
        LDX     with_lookup_type
        SEC
        RTS
@wlf_next:
        LDX     tmp1
        CPX     #0
        BEQ     @wlf_nf
        DEX
        BRA     @wlf_ctx
@wlf_nf:
        CLC
        RTS

; emit_with_field_base — push the selected WITH record's base address, then
; add the matched field offset (with_lookup_off) if nonzero.
emit_with_field_base:
        LDA     with_lookup_base_hi
        STA     scratch
        LDA     with_lookup_base_lo
        JSR     emit_LDG
        LDA     with_lookup_off
        BEQ     @ewfb_done
        JSR     emit_LDCI
        JSR     emit_ADI
@ewfb_done:
        RTS

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
        BCC     :+
        JMP     @groupdone
:
        PHX
        LDA     scratch
        JSR     store_proc_param_type
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
        LDA     scratch
        CMP     #TY_ARRAY
        BNE     @pl_chk_rec
        LDA     array_elem_ty
        LDY     #22
        STA     (tmp3),y
        LDA     #2
        LDY     #23
        STA     (tmp3),y
        BRA     @pl_meta_done
@pl_chk_rec:
        CMP     #TY_RECORD
        BNE     @pl_meta_done
        LDA     record_first_field
        LDY     #22
        STA     (tmp3),y
        LDA     record_field_count
        LDY     #23
        STA     (tmp3),y
@pl_meta_done:
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
; parse_param_sig_list — parse a PROCEDURE/FUNCTION signature parameter list
; without creating SYM_VAR entries. Used by UNIT interface headings so the
; exported routine can record only param-count / VAR-mask metadata.
; ---------------------------------------------------------------------------
parse_param_sig_list:
        LDA     #0
        STA     param_var_mask
@psig_group:
        LDA     #0
        STA     group_is_var
        LDA     tok_type
        CMP     #TOK_VAR
        BNE     @psig_no_var
        LDA     #1
        STA     group_is_var
        JSR     next_token
@psig_no_var:
        LDA     tok_type
        CMP     #TOK_IDENT
        BEQ     @psig_have_ident
        JMP     @psig_done
@psig_have_ident:
        LDA     #0
        STA     var_name_count
@psig_collect:
        LDA     tok_type
        CMP     #TOK_IDENT
        BNE     @psig_end_names
        INC     var_name_count
        JSR     next_token
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @psig_end_names
        JSR     next_token
        BRA     @psig_collect
@psig_end_names:
        LDA     tok_type
        CMP     #TOK_COLON
        BNE     :+
        JSR     next_token
:       JSR     parse_type_spec
        STA     scratch
        LDX     #0
@psig_add:
        CPX     var_name_count
        BCS     @psig_group_done
        LDA     scratch
        JSR     store_proc_param_type
        LDA     group_is_var
        BEQ     @psig_inc
        LDY     proc_param_count
        CPY     #8
        BCS     @psig_inc
        LDA     #1
@psig_shl:
        CPY     #0
        BEQ     @psig_shl_done
        ASL
        DEY
        BRA     @psig_shl
@psig_shl_done:
        ORA     param_var_mask
        STA     param_var_mask
@psig_inc:
        INC     proc_param_count
        INX
        BRA     @psig_add
@psig_group_done:
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     @psig_done
        JSR     next_token
        JMP     @psig_group
@psig_done:
        LDA     tok_type
        CMP     #TOK_RPAREN
        BNE     :+
        JSR     next_token
:       RTS

; ---------------------------------------------------------------------------
; parse_proc_iface_decl — PROCEDURE heading in a UNIT INTERFACE section.
; Registers a global SYM_PROC with placeholder code offset 0 and fills
; param-count / VAR-mask metadata for later callers and implementation.
; ---------------------------------------------------------------------------
parse_proc_iface_decl:
        LDA     tok_type
        CMP     #TOK_IDENT
        BEQ     :+
        RTS
:       LDA     #0
        STA     proc_param_count
        STA     param_var_mask
        STA     tmp2
        STA     tmp2+1
        LDA     #SYM_PROC
        LDX     #TY_NONE
        JSR     symtab_add
        LDA     tmp3
        STA     proc_entry_idx
        LDA     tmp3+1
        STA     proc_entry_idx+1
        JSR     next_token              ; consume name
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @ppid_patch
        JSR     next_token
        JSR     parse_param_sig_list
@ppid_patch:
        LDA     proc_entry_idx
        STA     tmp3
        LDA     proc_entry_idx+1
        STA     tmp3+1
        LDY     #21
        LDA     proc_param_count
        STA     (tmp3),y
        LDY     #22
        LDA     param_var_mask
        STA     (tmp3),y
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     :+
        JSR     next_token
:       RTS

; ---------------------------------------------------------------------------
; parse_func_iface_decl — FUNCTION heading in a UNIT INTERFACE section.
; Registers a global SYM_FUNC with placeholder code offset 0 and patches
; the return type plus param-count / VAR-mask metadata.
; ---------------------------------------------------------------------------
parse_func_iface_decl:
        LDA     tok_type
        CMP     #TOK_IDENT
        BEQ     :+
        RTS
:       LDA     #0
        STA     proc_param_count
        STA     param_var_mask
        STA     tmp2
        STA     tmp2+1
        LDA     #SYM_FUNC
        LDX     #TY_NONE
        JSR     symtab_add
        LDA     tmp3
        STA     proc_entry_idx
        LDA     tmp3+1
        STA     proc_entry_idx+1
        JSR     next_token              ; consume name
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @pfid_after_params
        JSR     next_token
        JSR     parse_param_sig_list
@pfid_after_params:
        LDA     tok_type
        CMP     #TOK_COLON
        BNE     :+
        JSR     next_token
:       JSR     parse_type_spec
        STA     scratch
        LDA     proc_entry_idx
        STA     tmp3
        LDA     proc_entry_idx+1
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
        LDA     tok_type
        CMP     #TOK_SEMICOLON
        BNE     :+
        JSR     next_token
:       RTS

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

; UNIT implementation bodies may be preceded by an INTERFACE heading.
; Reuse that global SYM_PROC entry instead of creating a duplicate.
        LDA     scope_depth
        BNE     @new_entry
        LDA     unit_section
        CMP     #2
        BNE     @new_entry
        JSR     symtab_find
        BCC     @new_entry
        LDY     #16
        LDA     (tmp3),y
        CMP     #SYM_PROC
        BNE     @new_entry
        LDY     #20
        LDA     (tmp3),y
        BNE     @new_entry
        LDA     tmp3
        STA     proc_entry_idx
        LDA     tmp3+1
        STA     proc_entry_idx+1
        JSR     next_token
        JMP     @enter_scope
@new_entry:

        JSR     next_token      ; consume name

; Register proc in symbol table NOW so we have an entry to update
; with the param count once the param list has been parsed.
; Offset (proc entry point) = current cg_pc.
        JSR     swap_ident_save ; ident_buf <-> save_name_buf
        LDA     cg_pc
        STA     tmp2
        LDA     cg_pc+1
        STA     tmp2+1
        LDA     #SYM_PROC
        LDX     #TY_NONE
        JSR     symtab_add
        LDA     tmp3
        STA     proc_entry_idx
        LDA     tmp3+1
        STA     proc_entry_idx+1
        JSR     swap_ident_save

; Enter scope: params and any locals belong to the proc's frame.
@enter_scope:
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
        LDA     proc_entry_idx
        STA     tmp3
        LDA     proc_entry_idx+1
        STA     tmp3+1
        LDY     #18
        LDA     cg_pc
        STA     (tmp3),y
        INY
        LDA     cg_pc+1
        STA     (tmp3),y
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
        STA     tmp3
        LDA     proc_entry_idx+1
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

; UNIT implementation bodies may bind to an INTERFACE-declared SYM_FUNC.
        LDA     scope_depth
        BNE     @new_entry
        LDA     unit_section
        CMP     #2
        BNE     @new_entry
        JSR     symtab_find
        BCC     @new_entry
        LDY     #16
        LDA     (tmp3),y
        CMP     #SYM_FUNC
        BNE     @new_entry
        LDY     #20
        LDA     (tmp3),y
        BNE     @new_entry
        LDA     tmp3
        STA     proc_entry_idx
        LDA     tmp3+1
        STA     proc_entry_idx+1
        JSR     next_token
        JMP     @enter_scope
@new_entry:

        JSR     next_token      ; consume name

; Register SYM_FUNC NOW so we have an entry to backpatch.
; Type backpatched after we parse `: RETURN_TYPE`.
        JSR     swap_ident_save ; ident_buf <-> save_name_buf
        LDA     cg_pc
        STA     tmp2
        LDA     cg_pc+1
        STA     tmp2+1
        LDA     #SYM_FUNC
        LDX     #TY_NONE        ; placeholder
        JSR     symtab_add
        LDA     tmp3
        STA     proc_entry_idx
        LDA     tmp3+1
        STA     proc_entry_idx+1
        JSR     swap_ident_save

; Enter scope: params, locals, and SYM_RETVAL belong to fn frame.
@enter_scope:
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
        STA     tmp3
        LDA     proc_entry_idx+1
        STA     tmp3+1
        LDY     #18
        LDA     cg_pc
        STA     (tmp3),y
        INY
        LDA     cg_pc+1
        STA     (tmp3),y
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
        LDX     scratch
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
        STA     tmp3
        LDA     proc_entry_idx+1
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
        BEQ     @pcs_force_advance
        JMP     @loop
@pcs_force_advance:
        JSR     next_token
        JMP     @loop
@consume_semi:
        JSR     next_token
        JMP     @loop
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
        CMP     #TOK_WITH
        BEQ     @with
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

@with:
        JSR     parse_with_stmt
        RTS

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
; parse_with_stmt — WITH record_expr { , record_expr } DO statement
; Each selector gets a hidden global word holding the runtime record base
; address. The compile-time WITH context stack records the matching field
; table span so unqualified identifiers can resolve as fields.
; ---------------------------------------------------------------------------
parse_with_stmt:
        LDA     with_depth
        PHA                             ; restore point after the body
        JSR     next_token              ; consume WITH
@pws_item:
        JSR     parse_expression
        LDA     expr_type
        CMP     #TY_RECORD
        BEQ     @pws_record
        LDA     #<err_type
        STA     tmp0
        LDA     #>err_type
        STA     tmp0+1
        JSR     compile_error
        JSR     emit_POP                ; discard non-record expression result
        BRA     @pws_after_item
@pws_record:
        LDA     #2
        JSR     codegen_alloc_global    ; tmp2 = hidden word for base addr
        LDA     tmp2
        PHA
        LDA     tmp2+1
        PHA
        LDA     tmp2+1
        STA     scratch
        LDA     tmp2
        JSR     emit_STG                ; save selector address
        LDX     with_depth
        CPX     #WITH_DEPTHS
        BCC     @pws_push
        LDA     #<err_too_many
        STA     tmp0
        LDA     #>err_too_many
        STA     tmp0+1
        JSR     compile_error
        BRA     @pws_discard_hidden
@pws_push:
        PLA
        STA     with_base_hi,x
        PLA
        STA     with_base_lo,x
        LDA     expr_record_first
        STA     with_first_field,x
        LDA     expr_record_count
        STA     with_field_count,x
        INC     with_depth
        BRA     @pws_after_item
@pws_discard_hidden:
        PLA
        PLA
@pws_after_item:
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @pws_need_do
        JSR     next_token
        BRA     @pws_item
@pws_need_do:
        LDA     tok_type
        CMP     #TOK_DO
        BEQ     @pws_do
        LDA     #<err_expected_do
        STA     tmp0
        LDA     #>err_expected_do
        STA     tmp0+1
        JSR     compile_error
        BRA     @pws_body
@pws_do:
        JSR     next_token              ; consume DO
@pws_body:
        JSR     parse_statement
        PLA
        STA     with_depth
        RTS

; ---------------------------------------------------------------------------
; parse_with_field_assign — current token is the field IDENT already matched
; by with_lookup_field. Handles scalar fields, pointer-field dereference, and
; chained nested-record assignments such as `WITH R DO A.X := 1`.
; ---------------------------------------------------------------------------
parse_with_field_assign:
        JSR     next_token              ; consume field name
        JSR     emit_with_field_base    ; push &field / &subrecord
        LDA     with_lookup_type
        CMP     #TY_RECORD
        BEQ     @pwfa_record
        CMP     #TY_PTR
        BNE     @pwfa_scalar
        LDA     tok_type
        CMP     #TOK_CARET
        BNE     @pwfa_scalar
        JSR     emit_LDIND              ; load pointer value from field cell
        JSR     next_token              ; consume '^'
        LDA     tok_type
        CMP     #TOK_ASSIGN
        BNE     :+
        JSR     next_token
:       JSR     parse_expression
        JSR     emit_STIND
        RTS
@pwfa_scalar:
        LDA     tok_type
        CMP     #TOK_ASSIGN
        BNE     :+
        JSR     next_token
:       JSR     parse_expression
        JSR     emit_STIND
        RTS
@pwfa_record:
        LDA     tok_type
        CMP     #TOK_DOT
        BEQ     @pwfa_chain_prep
        LDA     #<err_type
        STA     tmp0
        LDA     #>err_type
        STA     tmp0+1
        JSR     compile_error
        JSR     emit_POP                ; discard the subrecord address
        RTS
@pwfa_chain_prep:
        LDA     with_lookup_first
        STA     fcall_vmask
        LDA     with_lookup_count
        STA     fcall_lsize
@pwfa_chain:
        LDA     tok_type
        CMP     #TOK_DOT
        BNE     :+
        JSR     next_token
:
        LDA     fcall_vmask
        LDX     fcall_lsize
        JSR     field_lookup_in_record
        BCC     @pwfa_nf
        PHA
        TXA
        STA     field_chain_type
        LDA     tmp3
        TAX
        LDA     field_nested_first,x
        STA     field_chain_first
        LDA     field_nested_count,x
        STA     field_chain_count
        PLA
        BEQ     @pwfa_no_off
        JSR     emit_LDCI
        JSR     emit_ADI
@pwfa_no_off:
        JSR     next_token              ; consume nested field name
        LDA     field_chain_type
        CMP     #TY_RECORD
        BNE     @pwfa_leaf
        LDA     tok_type
        CMP     #TOK_DOT
        BNE     @pwfa_leaf
        LDA     field_chain_first
        STA     fcall_vmask
        LDA     field_chain_count
        STA     fcall_lsize
        BRA     @pwfa_chain
@pwfa_leaf:
        LDA     tok_type
        CMP     #TOK_ASSIGN
        BNE     :+
        JSR     next_token
:       JSR     parse_expression
        JSR     emit_STIND
        RTS
@pwfa_nf:
        LDA     #<err_undef
        STA     tmp0
        LDA     #>err_undef
        STA     tmp0+1
        JSR     compile_error
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
;   len 3 : NEW
;   len 4 : READ
;   len 5 : WRITE  | RESET  | CLOSE
;   len 6 : READLN | ASSIGN | APPEND
;   len 7 : WRITELN| REWRITE | DISPOSE
        LDA     ident_buf       ; length
        CMP     #3
        BEQ     @chk_new
        CMP     #4
        bne     :+
        jmp     @chk_read
:
        CMP     #5
        BEQ     @chk_len5
        CMP     #6
        BEQ     @chk_len6
        CMP     #7
        BEQ     @chk_len7
        JMP     @lookup_sym

@chk_new:
; "NEW" (3)
        LDA     ident_buf+1
        CMP     #'N'
        beq     :+
        JMP     @lookup_sym
:
        LDA     ident_buf+2
        CMP     #'E'
        beq     :+
        JMP     @lookup_sym
:
        LDA     ident_buf+3
        CMP     #'W'
        BEQ     :+
        JMP     @lookup_sym
:
        JMP     parse_builtin_new

@chk_len5:
        LDA     ident_buf+1
        CMP     #'W'
        BEQ     @chk_write
        CMP     #'R'
        BNE     :+
        JMP     @chk_reset
:
        CMP     #'C'
        BNE     :+
        JMP     @chk_close
:
        JMP     @lookup_sym

@chk_len6:
        LDA     ident_buf+1
        CMP     #'R'
        BEQ     @chk_readln
        CMP     #'A'
        BEQ     @chk_len6_a
        JMP     @lookup_sym
@chk_len6_a:
        LDA     ident_buf+2
        CMP     #'P'
        BNE     :+
        JMP     @chk_append
:
        CMP     #'S'
        BEQ     @chk_assign
        JMP     @lookup_sym

@chk_len7:
        LDA     ident_buf+1
        CMP     #'W'
        BEQ     @chk_writeln
        CMP     #'R'
        BEQ     @chk_rewrite
        CMP     #'D'
        BEQ     @chk_dispose
        JMP     @lookup_sym

@chk_dispose:
; "DISPOSE" (7) — verify I,S,P,O,S,E
        LDA     ident_buf+2
        CMP     #'I'
        BNE     @lookup_sym_jmp
        LDA     ident_buf+3
        CMP     #'S'
        BNE     @lookup_sym_jmp
        LDA     ident_buf+4
        CMP     #'P'
        BNE     @lookup_sym_jmp
        JMP     parse_builtin_dispose

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
@chk_append:
; "APPEND" (6)
        JMP     parse_builtin_append
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
; WITH fields behave like an injected inner scope. Check them before the
; real symbol table so `WITH R DO X := 1` binds `X` to the record field.
        JSR     with_lookup_field
        BCC     @lookup_symtab
        JSR     parse_with_field_assign
        RTS

@lookup_symtab:
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
        LDY     #24
        LDX     #0
@copy_ptypes:
        LDA     (tmp3),y
        STA     sym_param_types,x
        INY
        INX
        CPX     #8
        BCC     @copy_ptypes
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
        LDA     sym_save_type
        JSR     coerce_expr_to_target_type
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
        LDA     sym_save_type
        JSR     coerce_expr_to_target_type
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
; ptr^ := expr — pointer dereference assignment
        CMP     #TY_PTR
        BNE     :+
        LDA     tok_type
        CMP     #TOK_CARET
        BNE     :+
        JMP     @ptr_deref_assign
:
; scalar assignment — expect ':='
        LDA     tok_type
        CMP     #TOK_ASSIGN
        BNE     :+
        JSR     next_token
:       JSR     parse_expression
        LDA     sym_save_type
        JSR     coerce_expr_to_target_type
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
        LDA     sym_save_vmask
        PHA
        JSR     parse_expression
        PLA
        JSR     coerce_expr_to_target_type
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
; Chained LHS: TOS = &cur_record_base. Walk into nested records as long
; as the field is RECORD-typed and another '.' follows; the final scalar
; field becomes the STIND target.
        LDA     sym_save_vmask
        STA     fcall_vmask     ; cur first_field
        LDA     sym_save_lsize
        STA     fcall_lsize     ; cur field count
@rla_chain:
; consume '.'
        LDA     tok_type
        CMP     #TOK_DOT
        BNE     :+
        JSR     next_token
:
; look up field name in current record
        LDA     fcall_vmask
        LDX     fcall_lsize
        JSR     field_lookup_in_record
        BCS     :+
        JMP     @rec_la_undef
:
; A=offset, X=type, tmp3=matched index
        PHA
        TXA
        STA     field_chain_type
        LDA     tmp3
        TAX
        LDA     field_nested_first,x
        STA     field_chain_first
        LDA     field_nested_count,x
        STA     field_chain_count
        PLA
        BEQ     @rla_no_off
        JSR     emit_LDCI
        JSR     emit_ADI
@rla_no_off:
        JSR     next_token      ; consume field name
; If field is RECORD and next is '.', chain another level
        LDA     field_chain_type
        CMP     #TY_RECORD
        BNE     @rla_leaf
        LDA     tok_type
        CMP     #TOK_DOT
        BNE     @rla_leaf
        LDA     field_chain_first
        STA     fcall_vmask
        LDA     field_chain_count
        STA     fcall_lsize
        BRA     @rla_chain
@rla_leaf:
; expect ':='; TOS holds &leaf_field
        LDA     tok_type
        CMP     #TOK_ASSIGN
        BNE     :+
        JSR     next_token
:
        LDA     field_chain_type
        PHA
        JSR     parse_expression
        PLA
        JSR     coerce_expr_to_target_type
        JSR     emit_STIND
        RTS
@rec_la_undef:
        LDA     #<err_undef
        STA     tmp0
        LDA     #>err_undef
        STA     tmp0+1
        JSR     compile_error
        RTS

@ptr_deref_assign:
; p^ := expr  — push p's value (the heap address), parse RHS, then STIND.
; sym_save_scope/off identify the pointer variable.
        LDA     sym_save_scope
        BNE     @pda_local
        LDA     sym_save_off+1
        STA     scratch
        LDA     sym_save_off
        JSR     emit_LDG                ; push pointer value (addr)
        BRA     @pda_after_load
@pda_local:
        LDA     sym_save_off
        JSR     emit_LDL                ; push pointer value (addr)
@pda_after_load:
        JSR     next_token              ; consume '^'
        LDA     tok_type
        CMP     #TOK_ASSIGN
        BNE     :+
        JSR     next_token              ; consume ':='
:       JSR     parse_expression        ; push RHS value
        JSR     emit_STIND              ; *(addr) := value
        RTS

@do_call:
; Phase B: value parameters (INTEGER words, 2 bytes each).
; sym_save_off       = proc's code address
; sym_save_pcount    = declared parameter count
;
; Calling sequence emitted:
;   for each arg i:
;       <expr>                   ; pushes value to TOS (uses CALLER's MP)
;   MRKA <pcount> <lsize_extra>  ; reframe + gather args into local slots
;   CALL <proc>                  ; jump (saves IPC into AR)
;
; Args are evaluated BEFORE MRKA so any LDL/LDA_L inside arg expressions
; reads the caller's frame.  MRKA then shifts the pcount values up by
; AR_LOCALS so they land at local slots 0..pcount-1 of the new frame.
; The procedure body reads params via LDL because they were added to the
; symbol table as SYM_VAR with scope=inner (proc) at offsets 0, 2, 4, ...
        JSR     next_token      ; consume the proc-name identifier
; if no params, optionally consume "()" then emit MRKA + CALL
        LDA     sym_save_pcount
        BEQ     @no_args
; expect '(' for arg list
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @emit_call      ; missing '(' — caller mistake; emit anyway
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
        TXA
        PHA
        LDA     sym_param_types,x
        JSR     coerce_expr_to_target_type
        PLA
        TAX
@arg_stored:
; Value (or address for VAR) left on stack — MRKA gathers later.
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
; Emit MRKA <pcount> <lsize_extra>; lsize_extra = sym_save_lsize - 2*pcount.
        LDA     sym_save_lsize
        SEC
        SBC     sym_save_pcount
        SBC     sym_save_pcount
        TAX                     ; X = lsize_extra (body locals only)
        LDA     sym_save_pcount ; A = pcount
        JSR     emit_MRKA
; Emit CALL <delta>: A=target lo, scratch=target hi
        LDA     sym_save_off
        ORA     sym_save_off+1
        BNE     :+
        LDA     #<err_undef
        STA     tmp0
        LDA     #>err_undef
        STA     tmp0+1
        JSR     compile_error
:       LDA     sym_save_off+1
        STA     scratch
        LDA     sym_save_off
        JSR     emit_CALL
        RTS

; ---------------------------------------------------------------------------
; parse_write_args — WRITE/WRITELN argument list.
;   Returns X=0 if console mode (caller emits WRITLN normally),
;           X=1 if file mode (file ptr left on stack — caller emits FWLN
;               or POP to consume it).
; ---------------------------------------------------------------------------
parse_write_args:
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @done_console
        JSR     next_token              ; consume '('
        LDA     tok_type
        CMP     #TOK_RPAREN
        BEQ     @close_console
        JSR     parse_expression        ; parse first arg
; If first arg is TY_TEXT, switch to file mode (leave file ptr on stack).
        LDA     expr_type
        CMP     #TY_TEXT
        BEQ     @file_mode
; Console mode — emit appropriate WRIT* for first arg, then loop.
        JSR     pw_emit_console
@cons_loop:
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @close_console
        JSR     next_token              ; consume ','
        JSR     parse_expression
        JSR     pw_emit_console
        BRA     @cons_loop
@close_console:
        LDA     tok_type
        CMP     #TOK_RPAREN
        BNE     :+
        JSR     next_token              ; consume ')'
:
@done_console:
        LDX     #0
        RTS

@file_mode:
@file_loop:
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @close_file
        JSR     next_token              ; consume ','
        JSR     emit_DUP                ; dup file ptr (consumed by FWRX)
        JSR     parse_expression        ; arg on top of stack
        JSR     pw_emit_file
        BRA     @file_loop
@close_file:
        LDA     tok_type
        CMP     #TOK_RPAREN
        BNE     :+
        JSR     next_token              ; consume ')'
:
        LDX     #1
        RTS

; pw_emit_console — emit WRITS/WRITC/WRITB/WRITI based on expr_type
pw_emit_console:
        LDA     expr_type
        CMP     #TY_STRING
        BNE     :+
        JMP     emit_WRITS
:
        CMP     #TY_CHAR
        BNE     :+
        JMP     emit_WRITC
:
        CMP     #TY_BOOL
        BNE     :+
        JMP     emit_WRITB
:
        CMP     #TY_REAL
        BNE     :+
        JMP     emit_WRITR
:
        JMP     emit_WRITI

; pw_emit_file — emit FWRS/FWRC/FWRB/FWRI based on expr_type
pw_emit_file:
        LDA     expr_type
        CMP     #TY_STRING
        BNE     :+
        JMP     emit_FWRS
:
        CMP     #TY_CHAR
        BNE     :+
        JMP     emit_FWRC
:
        CMP     #TY_BOOL
        BNE     :+
        JMP     emit_FWRB
:
        CMP     #TY_REAL
        BNE     :+
        JMP     emit_FWRR
:
        JMP     emit_FWRI

; emit_console_read_arg — current token must be an IDENT naming the target
; variable. CHAR targets compile as:
;   <addr> READC STIND
; so the zero-extended character from OP_READC is stored into the variable's
; 2-byte cell. Everything else currently uses READI.
emit_console_read_arg:
        JSR     symtab_find
        BCC     @cra_int
        LDY     #17
        LDA     (tmp3),y
        CMP     #TY_REAL
        BNE     :+
        JSR     parse_arg_lvalue
        JSR     emit_READR
        JMP     emit_STIND
:
        CMP     #TY_CHAR
        BNE     @cra_int
        JSR     parse_arg_lvalue
        JSR     emit_READC
        JMP     emit_STIND
@cra_int:
        JSR     parse_arg_lvalue
        JMP     emit_READI

; ---------------------------------------------------------------------------
; parse_read_args — READ/READLN argument list.  Each argument must be a
; variable identifier. Console mode dispatches by target type:
;   CHAR    -> READC + STIND
;   default -> READI
; File mode still routes through FRDC / FRDS / FRDI based on target type.
; ---------------------------------------------------------------------------
parse_read_args:
        LDA     tok_type
        CMP     #TOK_LPAREN
        BNE     @rdone_console
        JSR     next_token              ; consume '('
        LDA     tok_type
        CMP     #TOK_RPAREN
        BEQ     @rclose_console
        CMP     #TOK_IDENT
        BNE     @rclose_console
; Peek: is the first ident a TY_TEXT variable?
        JSR     symtab_find
        BCC     @cons_first
        LDY     #17
        LDA     (tmp3),y
        CMP     #TY_TEXT
        BEQ     @file_first
@cons_first:
        JSR     emit_console_read_arg
@cons_loop:
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @rclose_console
        JSR     next_token
        LDA     tok_type
        CMP     #TOK_IDENT
        BNE     @rclose_console
        JSR     emit_console_read_arg
        BRA     @cons_loop
@rclose_console:
        LDA     tok_type
        CMP     #TOK_RPAREN
        BNE     :+
        JSR     next_token              ; consume ')'
:
@rdone_console:
        LDX     #0
        RTS

@file_first:
; Push the file ptr (struct address) by parsing the ident as an expression —
; for TY_TEXT this emits LDA_G/LDA_L which pushes the struct's address.
        JSR     parse_expression
@file_loop:
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @close_file
        JSR     next_token              ; consume ','
        LDA     tok_type
        CMP     #TOK_IDENT
        BNE     @close_file
; Peek var type so we know whether to emit FRDC, FRDS, or FRDI.
        JSR     symtab_find
        BCC     @file_int_default
        LDY     #17
        LDA     (tmp3),y
        CMP     #TY_CHAR
        BNE     @file_chk_str
        JSR     emit_DUP                ; dup file ptr
        JSR     parse_arg_lvalue
        JSR     emit_FRDC
        BRA     @file_loop
@file_chk_str:
        CMP     #TY_STRING
        BNE     @file_chk_real
        JSR     emit_DUP                ; dup file ptr
        JSR     parse_arg_lvalue
        JSR     emit_FRDS
        BRA     @file_loop
@file_chk_real:
        CMP     #TY_REAL
        BNE     @file_int_default
        JSR     emit_DUP                ; dup file ptr
        JSR     parse_arg_lvalue
        JSR     emit_FRDR
        BRA     @file_loop
@file_int_default:
        JSR     emit_DUP                ; dup file ptr
        JSR     parse_arg_lvalue
        JSR     emit_FRDI
        BRA     @file_loop
@close_file:
        LDA     tok_type
        CMP     #TOK_RPAREN
        BNE     :+
        JSR     next_token              ; consume ')'
:
        LDX     #1
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
        STA     tmp1+1
        PLA                     ; UJP lo
        STA     tmp1
        PLA                     ; FJP hi
        STA     tmp2+1
        PLA                     ; FJP lo
        STA     tmp2
        JSR     patch_jump      ; patch FJP → start of else branch
; push UJP patch addr back for after-else patching
        LDA     tmp1            ; UJP lo
        PHA
        LDA     tmp1+1          ; UJP hi
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
; REAL / INTEGER coercion helpers
; ---------------------------------------------------------------------------

emit_promote_tos_int_to_real:
        LDA     #100
        JSR     emit_LDCI
        JMP     emit_MPI

emit_promote_tos_real_to_int:
        LDA     #100
        JSR     emit_LDCI
        JMP     emit_DVI

emit_promote_nos_int_to_real:
        JSR     emit_SWAP
        JSR     emit_promote_tos_int_to_real
        JMP     emit_SWAP

emit_promote_nos_real_to_int:
        JSR     emit_SWAP
        JSR     emit_promote_tos_real_to_int
        JMP     emit_SWAP

; coerce_expr_to_target_type — A=target TY_*, expr_type=current TOS type.
; Only INTEGER<->REAL coercions are performed; other combinations are left as-is.
coerce_expr_to_target_type:
        TAX
        CPX     #TY_REAL
        BEQ     @cett_real
        CPX     #TY_INT
        BEQ     @cett_int
        RTS
@cett_real:
        LDA     expr_type
        CMP     #TY_INT
        BNE     @cett_done
        JSR     emit_promote_tos_int_to_real
        LDA     #TY_REAL
        STA     expr_type
@cett_done:
        RTS
@cett_int:
        LDA     expr_type
        CMP     #TY_REAL
        BNE     @cett_done2
        JSR     emit_promote_tos_real_to_int
        LDA     #TY_INT
        STA     expr_type
@cett_done2:
        RTS

; coerce_binary_numeric_types — A=lhs type, expr_type=rhs type.
; Promotes one INTEGER operand to REAL when the other side is REAL.
; Returns A=result type (TY_INT or TY_REAL).
coerce_binary_numeric_types:
        CMP     #TY_REAL
        BEQ     @cbnt_lhs_real
        LDA     expr_type
        CMP     #TY_REAL
        BEQ     @cbnt_lhs_int_rhs_real
        LDA     #TY_INT
        RTS
@cbnt_lhs_real:
        LDA     expr_type
        CMP     #TY_INT
        BNE     @cbnt_real_done
        JSR     emit_promote_tos_int_to_real
@cbnt_real_done:
        LDA     #TY_REAL
        RTS
@cbnt_lhs_int_rhs_real:
        JSR     emit_promote_nos_int_to_real
        LDA     #TY_REAL
        RTS

; coerce_binary_to_real — A=lhs type, expr_type=rhs type.
; Ensures both operands are REAL and returns A=TY_REAL.
coerce_binary_to_real:
        CMP     #TY_REAL
        BEQ     @cbtr_lhs_real
        LDA     expr_type
        CMP     #TY_REAL
        BEQ     @cbtr_lhs_int_rhs_real
        JSR     emit_promote_tos_int_to_real
        JSR     emit_promote_nos_int_to_real
        LDA     #TY_REAL
        RTS
@cbtr_lhs_real:
        LDA     expr_type
        CMP     #TY_REAL
        BEQ     @cbtr_done
        JSR     emit_promote_tos_int_to_real
@cbtr_done:
        LDA     #TY_REAL
        RTS
@cbtr_lhs_int_rhs_real:
        JSR     emit_promote_nos_int_to_real
        LDA     #TY_REAL
        RTS

; coerce_binary_to_int — A=lhs type, expr_type=rhs type.
; Truncates REAL operands to INTEGER and returns A=TY_INT.
coerce_binary_to_int:
        CMP     #TY_REAL
        BEQ     @cbti_lhs_real
        LDA     expr_type
        CMP     #TY_REAL
        BEQ     @cbti_rhs_real
        LDA     #TY_INT
        RTS
@cbti_lhs_real:
        JSR     emit_promote_nos_real_to_int
        LDA     expr_type
        CMP     #TY_REAL
        BNE     @cbti_done
        JSR     emit_promote_tos_real_to_int
@cbti_done:
        LDA     #TY_INT
        RTS
@cbti_rhs_real:
        JSR     emit_promote_tos_real_to_int
        LDA     #TY_INT
        RTS

; ---------------------------------------------------------------------------
; parse_expression — full expression with operator precedence
; Phase 1: simple additive + comparison; no precedence climbing yet
; ---------------------------------------------------------------------------
parse_expression:
        JSR     parse_simple_expr
; check for relational operator
        LDA     tok_type
        CMP     #TOK_IN
        BEQ     @rel_in
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
@rel_in:
        JSR     next_token
        JSR     parse_simple_expr
        JSR     emit_INSET
        JMP     @rel_done
@rel_eq:
        LDA     expr_type
        PHA
        JSR     next_token
        JSR     parse_simple_expr
        PLA
        JSR     coerce_binary_numeric_types
        JSR     emit_EQUI
        JMP     @rel_done
@rel_neq:
        LDA     expr_type
        PHA
        JSR     next_token
        JSR     parse_simple_expr
        PLA
        JSR     coerce_binary_numeric_types
        JSR     emit_NEQI
        JMP     @rel_done
@rel_lt:
        LDA     expr_type
        PHA
        JSR     next_token
        JSR     parse_simple_expr
        PLA
        JSR     coerce_binary_numeric_types
        JSR     emit_LESI
        JMP     @rel_done
@rel_gt:
        LDA     expr_type
        PHA
        JSR     next_token
        JSR     parse_simple_expr
        PLA
        JSR     coerce_binary_numeric_types
        JSR     emit_GTRI
        JMP     @rel_done
@rel_leq:
        LDA     expr_type
        PHA
        JSR     next_token
        JSR     parse_simple_expr
        PLA
        JSR     coerce_binary_numeric_types
        JSR     emit_LEQI
        JMP     @rel_done
@rel_geq:
        LDA     expr_type
        PHA
        JSR     next_token
        JSR     parse_simple_expr
        PLA
        JSR     coerce_binary_numeric_types
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
        LDA     expr_type
        PHA
        JSR     next_token
        JSR     parse_term
        PLA
        CMP     #TY_SET
        BEQ     @add_set
        JSR     coerce_binary_numeric_types
        PHA
        JSR     emit_ADI
        PLA
        STA     expr_type
        BRA     @addop
@add_set:
        JSR     emit_LOR
        LDA     #TY_SET
        STA     expr_type
        BRA     @addop
@sub:
        LDA     expr_type
        PHA
        JSR     next_token
        JSR     parse_term
        PLA
        CMP     #TY_SET
        BEQ     @sub_set
        JSR     coerce_binary_numeric_types
        PHA
        JSR     emit_SBI
        PLA
        STA     expr_type
        BRA     @addop
@sub_set:
        JSR     emit_BNOT
        JSR     emit_LAND
        LDA     #TY_SET
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
        CMP     #TOK_SLASH
        BEQ     @rdiv
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
        LDA     expr_type
        PHA
        JSR     next_token
        JSR     parse_factor
        PLA
        CMP     #TY_SET
        BEQ     @mul_set
        JSR     coerce_binary_numeric_types
        CMP     #TY_REAL
        BEQ     @mul_real
        JSR     emit_MPI
        LDA     #TY_INT
        STA     expr_type
        JMP     @mulop
@mul_real:
        JSR     emit_MPR
        LDA     #TY_REAL
        STA     expr_type
        JMP     @mulop
@mul_set:
        JSR     emit_LAND
        LDA     #TY_SET
        STA     expr_type
        JMP     @mulop
@rdiv:
        LDA     expr_type
        PHA
        JSR     next_token
        JSR     parse_factor
        PLA
        JSR     coerce_binary_to_real
        JSR     emit_DVR
        LDA     #TY_REAL
        STA     expr_type
        JMP     @mulop
@div:
        LDA     expr_type
        PHA
        JSR     next_token
        JSR     parse_factor
        PLA
        JSR     coerce_binary_to_int
        JSR     emit_DVI
        LDA     #TY_INT
        STA     expr_type
        JMP     @mulop
@mod:
        LDA     expr_type
        PHA
        JSR     next_token
        JSR     parse_factor
        PLA
        JSR     coerce_binary_to_int
        JSR     emit_MOD
        LDA     #TY_INT
        STA     expr_type
        JMP     @mulop
@and:
        JSR     next_token
        JSR     parse_factor
        JSR     emit_LAND
        LDA     #TY_BOOL
        STA     expr_type
        JMP     @mulop

TOK_DIV_KW      = TOK_DIV       ; alias

; maybe_deref_loaded_ptr — a pointer value is already on the stack.
; If the following token is '^', consume it and load the pointed-to word.
; v1 pointer dereference still assumes pointer-to-INTEGER.
maybe_deref_loaded_ptr:
        LDA     expr_type
        CMP     #TY_PTR
        BNE     @mdlp_done
        LDA     tok_type
        CMP     #TOK_CARET
        BNE     @mdlp_done
        JSR     next_token
        JSR     emit_LDIND
        LDA     #TY_INT
        STA     expr_type
@mdlp_done:
        RTS

; parse_with_field_load — current token is the field IDENT already matched by
; with_lookup_field. Emits code for the field expression and leaves expr_type
; set appropriately. Record-valued results stay as addresses on the stack.
parse_with_field_load:
        JSR     next_token              ; consume field name
        JSR     emit_with_field_base
        LDA     with_lookup_type
        STA     expr_type
        CMP     #TY_RECORD
        BEQ     @pwfl_record
        JSR     emit_LDIND
        JMP     maybe_deref_loaded_ptr
@pwfl_record:
        LDA     tok_type
        CMP     #TOK_DOT
        BEQ     @pwfl_chain_prep
        LDA     with_lookup_first
        STA     expr_record_first
        LDA     with_lookup_count
        STA     expr_record_count
        RTS
@pwfl_chain_prep:
        LDA     with_lookup_first
        STA     fcall_vmask
        LDA     with_lookup_count
        STA     fcall_lsize
@pwfl_chain:
        LDA     tok_type
        CMP     #TOK_DOT
        BNE     :+
        JSR     next_token
:
        LDA     fcall_vmask
        LDX     fcall_lsize
        JSR     field_lookup_in_record
        BCC     @pwfl_nf
        PHA
        TXA
        STA     field_chain_type
        LDA     tmp3
        TAX
        LDA     field_nested_first,x
        STA     field_chain_first
        LDA     field_nested_count,x
        STA     field_chain_count
        PLA
        BEQ     @pwfl_no_off
        JSR     emit_LDCI
        JSR     emit_ADI
@pwfl_no_off:
        JSR     next_token              ; consume nested field name
        LDA     field_chain_type
        CMP     #TY_RECORD
        BNE     @pwfl_done_chain
        LDA     tok_type
        CMP     #TOK_DOT
        BNE     @pwfl_done_chain
        LDA     field_chain_first
        STA     fcall_vmask
        LDA     field_chain_count
        STA     fcall_lsize
        BRA     @pwfl_chain
@pwfl_done_chain:
        LDA     field_chain_type
        STA     expr_type
        CMP     #TY_RECORD
        BEQ     @pwfl_record_result
        JSR     emit_LDIND
        JMP     maybe_deref_loaded_ptr
@pwfl_record_result:
        LDA     field_chain_first
        STA     expr_record_first
        LDA     field_chain_count
        STA     expr_record_count
        RTS
@pwfl_nf:
        LDA     #<err_undef
        STA     tmp0
        LDA     #>err_undef
        STA     tmp0+1
        JSR     compile_error
        JSR     next_token
        JSR     emit_LDIND
        LDA     #TY_INT
        STA     expr_type
        RTS

; ---------------------------------------------------------------------------
; SET literal helpers
; ---------------------------------------------------------------------------
set_lit_type_error:
        LDA     #<err_type
        STA     tmp0
        LDA     #>err_type
        STA     tmp0+1
        JSR     compile_error
        RTS

; set_lit_add_value — A=value 0..15. ORs the matching bit into set_lit_mask.
set_lit_add_value:
        TAX
        LDA     #1
        STA     tmp0
        LDA     #0
        STA     tmp0+1
@slav_shift:
        CPX     #0
        BEQ     @slav_merge
        ASL     tmp0
        ROL     tmp0+1
        DEX
        BRA     @slav_shift
@slav_merge:
        LDA     set_lit_mask
        ORA     tmp0
        STA     set_lit_mask
        LDA     set_lit_mask+1
        ORA     tmp0+1
        STA     set_lit_mask+1
        RTS

; parse_set_literal — current token is '['. Supports [] and constant integer
; items/ranges like [1,3,5] and [3..6]. Elements must be 0..15 in this build.
parse_set_literal:
        LDA     #0
        STA     set_lit_mask
        STA     set_lit_mask+1
        JSR     next_token              ; consume '['
        LDA     tok_type
        CMP     #TOK_RBRACK
        BNE     @psl_item
        JMP     @psl_close
@psl_item:
        LDA     tok_type
        CMP     #TOK_INT
        BNE     @psl_bad_item
        LDA     tok_ival_hi
        BNE     @psl_bad_item
        LDA     tok_ival_lo
        CMP     #16
        BCS     @psl_bad_item
        STA     set_lit_lo
        JSR     next_token              ; consume item lo
        LDA     tok_type
        CMP     #TOK_DOTDOT
        BNE     @psl_single
        JSR     next_token              ; consume ..
        LDA     tok_type
        CMP     #TOK_INT
        BNE     @psl_bad_range
        LDA     tok_ival_hi
        BNE     @psl_bad_range
        LDA     tok_ival_lo
        CMP     #16
        BCS     @psl_bad_range
        STA     set_lit_hi
        CMP     set_lit_lo
        BCC     @psl_bad_range
        JSR     next_token              ; consume item hi
        LDA     set_lit_lo
        STA     set_lit_cur
@psl_range_loop:
        LDA     set_lit_cur
        JSR     set_lit_add_value
        LDA     set_lit_cur
        CMP     set_lit_hi
        BEQ     @psl_after_item
        INC     set_lit_cur
        BRA     @psl_range_loop
@psl_single:
        LDA     set_lit_lo
        JSR     set_lit_add_value
        BRA     @psl_after_item
@psl_bad_item:
        JSR     set_lit_type_error
        JSR     next_token
        BRA     @psl_after_item
@psl_bad_range:
        JSR     set_lit_type_error
        LDA     tok_type
        CMP     #TOK_INT
        BNE     @psl_after_item
        JSR     next_token
@psl_after_item:
        LDA     tok_type
        CMP     #TOK_COMMA
        BNE     @psl_done_items
        JSR     next_token
        BRA     @psl_item
@psl_done_items:
        LDA     tok_type
        CMP     #TOK_RBRACK
        BEQ     @psl_close
        LDA     #<err_expected_rb
        STA     tmp0
        LDA     #>err_expected_rb
        STA     tmp0+1
        JSR     compile_error
        BRA     @psl_emit
@psl_close:
        JSR     next_token              ; consume ']'
@psl_emit:
        LDA     set_lit_mask+1
        STA     scratch
        LDA     set_lit_mask
        JSR     emit_LDCW
        LDA     #TY_SET
        STA     expr_type
        RTS

; ---------------------------------------------------------------------------
; parse_factor — literal | set-literal | variable | ( expr ) | NOT factor
; ---------------------------------------------------------------------------
parse_factor:
        LDA     tok_type
        CMP     #TOK_INT
        BNE     :+
        JMP     @int_lit
:
        CMP     #TOK_REAL
        BNE     :+
        JMP     @real_lit
:
        CMP     #TOK_CHAR
        BNE     :+
        JMP     @char_lit
:
        CMP     #TOK_STRING
        BNE     :+
        JMP     @str_lit
:
        CMP     #TOK_LBRACK
        BNE     :+
        JMP     @set_lit
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

@real_lit:
        LDA     tok_ival_hi
        STA     scratch
        LDA     tok_ival_lo
        JSR     emit_LDCW
        JSR     next_token
        LDA     #TY_REAL
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

@set_lit:
        JSR     parse_set_literal
        RTS

@ident_or_call:
; First check for built-ins (LENGTH/POS/COPY/CONCAT/EOF/EOLN/TRUE/FALSE)
; by length+initial.  These take precedence over user identifiers
; (matching WRITE/READ handling in parse_assign_or_call).  All dispatch
; branches use JMPs because the target blocks are scattered too far for
; ±127 byte BEQ range.
        LDA     ident_buf
        CMP     #3
        BNE     @bi_dn3
        JMP     @bi_chk3
@bi_dn3:
        CMP     #4
        BNE     @bi_dn4
        JMP     @bi_chk4
@bi_dn4:
        CMP     #5
        BNE     @bi_dn5
        JMP     @bi_chk5
@bi_dn5:
        CMP     #6
        BNE     @bi_dn6
        JMP     @bi_chk_len_or_cat
@bi_dn6:
        JMP     @do_lookup
@bi_chk4:
; 4-letter built-ins: COPY, EOLN, TRUE
        LDA     ident_buf+1
        CMP     #'C'
        BNE     :+
        JMP     @bi_chk_copy
:
        CMP     #'E'
        BEQ     @bi_try_eoln
        CMP     #'T'
        BEQ     @bi_try_true
        JMP     @do_lookup
@bi_try_eoln:
        LDA     ident_buf+2
        CMP     #'O'
        BNE     @bi4_no
        LDA     ident_buf+3
        CMP     #'L'
        BNE     @bi4_no
        LDA     ident_buf+4
        CMP     #'N'
        BNE     @bi4_no
        JMP     parse_builtin_eoln
@bi_try_true:
        LDA     ident_buf+2
        CMP     #'R'
        BNE     @bi4_no
        LDA     ident_buf+3
        CMP     #'U'
        BNE     @bi4_no
        LDA     ident_buf+4
        CMP     #'E'
        BNE     @bi4_no
        JMP     parse_builtin_true
@bi4_no:
        JMP     @do_lookup
@bi_chk5:
; 5-letter built-ins: FALSE
        LDA     ident_buf+1
        CMP     #'F'
        BNE     @bi5_no
        LDA     ident_buf+2
        CMP     #'A'
        BNE     @bi5_no
        LDA     ident_buf+3
        CMP     #'L'
        BNE     @bi5_no
        LDA     ident_buf+4
        CMP     #'S'
        BNE     @bi5_no
        LDA     ident_buf+5
        CMP     #'E'
        BNE     @bi5_no
        JMP     parse_builtin_false
@bi5_no:
        JMP     @do_lookup
@bi_chk3:
; 3-letter built-ins: POS, EOF
        LDA     ident_buf+1
        CMP     #'P'
        BEQ     @bi_chk_pos
        CMP     #'E'
        BEQ     @bi_chk_eof
        BRA     @do_lookup
@bi_chk_eof:
        LDA     ident_buf+2
        CMP     #'O'
        BNE     @do_lookup
        LDA     ident_buf+3
        CMP     #'F'
        BNE     @do_lookup
        JMP     parse_builtin_eof
@bi_chk_pos:
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
        JSR     with_lookup_field
        BCC     @lookup_symtab
        JMP     parse_with_field_load
@lookup_symtab:
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
        LDY     #24
        LDX     #0
@copy_fptypes:
        LDA     (tmp3),y
        STA     fcall_param_types,x
        INY
        INX
        CPX     #8
        BCC     @copy_fptypes
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
        JMP     @maybe_deref_ptr
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
        ; fall through to @maybe_deref_ptr
@maybe_deref_ptr:
        JMP     maybe_deref_loaded_ptr
@text_rhs_local:
        TXA
        JMP     emit_LDA_L
@arr_rhs_local:
        TXA
        JSR     emit_LDA_L
@arr_rhs_index:
; parse [index], emit INDEX, LDIND
        LDA     fcall_vmask
        PHA
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
        PLA
        STA     expr_type
        RTS
@rec_rhs_local:
        TXA
        JSR     emit_LDA_L
@rec_rhs_field:
; Chained field access loop: TOS holds &cur_record_base.
; fcall_vmask = current record's first_field, fcall_lsize = its count.
; Each iteration consumes ".name" and walks one level deeper.  When the
; resolved field is itself a RECORD and another '.' follows, repeat;
; otherwise emit LDIND on the final scalar address.
        LDA     tok_type
        CMP     #TOK_DOT
        BEQ     @rrf_chain
        LDA     fcall_vmask
        STA     expr_record_first
        LDA     fcall_lsize
        STA     expr_record_count
        RTS
@rrf_chain:
; consume '.'
        LDA     tok_type
        CMP     #TOK_DOT
        BNE     :+
        JSR     next_token
:
; field name now in ident_buf — look up in current record
        LDA     fcall_vmask     ; first_field_idx
        LDX     fcall_lsize     ; field_count
        JSR     field_lookup_in_record
        BCC     @rec_rhs_nf
; A=offset, X=type, tmp3=matched index. Stash all three across emits.
        PHA                     ; save offset
        TXA
        STA     field_chain_type
        LDA     tmp3
        TAX
        LDA     field_nested_first,x
        STA     field_chain_first
        LDA     field_nested_count,x
        STA     field_chain_count
        PLA                     ; offset back; PLA sets Z
        BEQ     @rrf_no_off
        JSR     emit_LDCI
        JSR     emit_ADI
@rrf_no_off:
        JSR     next_token              ; consume field name
; If field type is RECORD AND next token is '.', chain into it.
        LDA     field_chain_type
        CMP     #TY_RECORD
        BNE     @rrf_done_chain
        LDA     tok_type
        CMP     #TOK_DOT
        BNE     @rrf_done_chain
        LDA     field_chain_first
        STA     fcall_vmask
        LDA     field_chain_count
        STA     fcall_lsize
        BRA     @rrf_chain
@rrf_done_chain:
; Final step: scalar fields → LDIND value; record fields stay as
; addresses (so calling code sees them as TY_RECORD pointers).
        LDA     field_chain_type
        CMP     #TY_RECORD
        BEQ     @rrf_rec_result
        JSR     emit_LDIND
        LDA     field_chain_type
        STA     expr_type
        JMP     maybe_deref_loaded_ptr
@rrf_rec_result:
        LDA     field_chain_first
        STA     expr_record_first
        LDA     field_chain_count
        STA     expr_record_count
        LDA     field_chain_type
        STA     expr_type
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
; SYM_VARREF: local slot holds an address. Compound types keep that
; address as their expression result; scalars load through it.
        PLA                     ; discard pcount
        TXA
        JSR     emit_LDL        ; push address from slot
        LDA     expr_type
        CMP     #TY_RECORD
        BEQ     @vrf_addr
        CMP     #TY_ARRAY
        BEQ     @vrf_addr
        CMP     #TY_TEXT
        BEQ     @vrf_addr
        JSR     emit_LDIND      ; deref → push word
        JMP     maybe_deref_loaded_ptr
@vrf_addr:
        CMP     #TY_RECORD
        BNE     :+
        LDA     fcall_vmask
        STA     expr_record_first
        LDA     fcall_lsize
        STA     expr_record_count
:       RTS
@const_emit:
; CONST: offset bytes hold the literal value. X=lo, scratch=hi.
        PLA                     ; discard pcount
        TXA
        JSR     emit_LDCW
        RTS
@sym_func_call:
; SYM_FUNC: emit args, then MRKA + CALL.  Result is left on TOS by op_RETF.
; expr_type is the function's return type (already set).
; X=off lo, scratch=off hi, top of 6502 stack = pcount.
; NOTE: still not nest-safe inside one arg expression — fcall_pcount and
; fcall_vmask live in zero-page across the loop and a nested function-call
; arg would clobber them.  Pre-existing limitation; tracked as TODO.
;
; Args are evaluated BEFORE MRKA so LDL/LDA_L inside arg expressions
; addresses the caller's MP (not the callee's).  MRKA then shifts the
; pcount pushed values up by AR_LOCALS into local slots 0..pcount-1.
;
; lsize_extra is computed up-front and stashed on the 6502 hw stack so
; record-field lookup inside an arg (which scratches fcall_lsize) cannot
; corrupt the MRKA operand we emit at the end.
        STX     fcall_lo
        LDA     scratch
        STA     fcall_hi
        LDA     expr_type
        STA     fcall_type      ; preserve across parse_expression
        PLA                     ; pcount
        STA     fcall_pcount
; Compute lsize_extra now (before any parse_expression call may clobber
; fcall_lsize) and push it onto the 6502 hw stack.
        LDA     fcall_lsize
        SEC
        SBC     fcall_pcount
        SBC     fcall_pcount
        PHA                     ; saved lsize_extra
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
        TXA
        PHA
        LDA     fcall_param_types,x
        JSR     coerce_expr_to_target_type
        PLA
        TAX
@fc_arg_stored:
; Value (or address for VAR) left on stack — MRKA gathers them later.
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
; Emit MRKA <pcount> <lsize_extra>; pop saved lsize_extra from hw stack
; (fcall_lsize may have been clobbered by record-field lookup in args).
        PLA
        TAX                     ; X = saved lsize_extra
        LDA     fcall_pcount    ; A = pcount
        JSR     emit_MRKA
; Emit CALL.
        LDA     fcall_lo
        ORA     fcall_hi
        BNE     :+
        LDA     #<err_undef
        STA     tmp0
        LDA     #>err_undef
        STA     tmp0+1
        JSR     compile_error
:       LDA     fcall_hi
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

; ---------------------------------------------------------------------------
; File built-in helpers — entered from parse_assign_or_call (statement form)
; or parse_factor's @ident_or_call (EOF as expression).
; ---------------------------------------------------------------------------

; ASSIGN(filevar, namestring)
parse_builtin_assign:
        JSR     next_token              ; consume ASSIGN
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_expression        ; file ptr (TY_TEXT)
        LDA     #TOK_COMMA
        JSR     parse_eat_token
        JSR     parse_expression        ; filename string
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        JMP     emit_FASSGN

parse_builtin_reset:
        JSR     next_token              ; consume RESET
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_expression
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        JMP     emit_FRESET

parse_builtin_rewrite:
        JSR     next_token              ; consume REWRITE
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_expression
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        JMP     emit_FREWRT

parse_builtin_append:
        JSR     next_token              ; consume APPEND
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_expression
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        JMP     emit_FAPPND

parse_builtin_close:
        JSR     next_token              ; consume CLOSE
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_expression
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        JMP     emit_FCLOSE

parse_builtin_eof:
        JSR     next_token              ; consume EOF
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_expression        ; file ptr (TY_TEXT)
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        JSR     emit_FEOF
        LDA     #TY_BOOL
        STA     expr_type
        RTS

parse_builtin_eoln:
        JSR     next_token              ; consume EOLN
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_expression        ; file ptr (TY_TEXT)
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        JSR     emit_FEOLN
        LDA     #TY_BOOL
        STA     expr_type
        RTS

; NEW(p) — bump-allocate 2 bytes (pointer-to-INTEGER, v1) and store the
; new heap address into pointer variable p.
;   parse_arg_lvalue pushes &p; emit_NEW pushes the fresh pointer; STIND
;   takes (NOS=addr, TOS=val) and writes val to addr.
parse_builtin_new:
        JSR     next_token              ; consume NEW
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_arg_lvalue        ; push &p
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        LDA     #0
        STA     scratch                 ; size hi = 0
        LDA     #2                      ; v1: always allocate 2 bytes
        JSR     emit_NEW
        JMP     emit_STIND

; DISPOSE(p) — bump allocator can't free, so just discard the pointer.
parse_builtin_dispose:
        JSR     next_token              ; consume DISPOSE
        LDA     #TOK_LPAREN
        JSR     parse_eat_token
        JSR     parse_expression        ; push p (value)
        LDA     #TOK_RPAREN
        JSR     parse_eat_token
        JMP     emit_DISP

; TRUE / FALSE — predefined boolean constants.  Push 1/0 as TY_BOOL so
; pw_emit_file routes to FWRB ("TRUE"/"FALSE") and pw_emit_console to WRITB.
parse_builtin_true:
        JSR     next_token              ; consume TRUE
        LDA     #1
        JSR     emit_LDCB
        LDA     #TY_BOOL
        STA     expr_type
        RTS

parse_builtin_false:
        JSR     next_token              ; consume FALSE
        LDA     #0
        JSR     emit_LDCB
        LDA     #TY_BOOL
        STA     expr_type
        RTS
