; zeropage.asm — zero-page variable layout for PASCAL.COM and PRUN.COM
;
; Avoid $00-$2F (DOS/65 system), $32 (FARFUNCT), $39-$3A (DOS/65).
; Safe range: $50-$EF

        .segment        "ZEROPAGE"

        .RES    $50             ; skip $00-$4F (DOS/65 system zero page)

; ---------------------------------------------------------------------------
; P-machine registers — start at $50 as noted in comments below
; ---------------------------------------------------------------------------

pm_ipc:
        .RES    2               ; $50-$51  interpreter program counter
pm_sp:
        .RES    2               ; $52-$53  p-machine stack pointer
pm_mp:
        .RES    2               ; $54-$55  mark/frame pointer
pm_base:
        .RES    2               ; $56-$57  global variable base
pm_np:
        .RES    2               ; $58-$59  heap top (grows downward)

; General-purpose temporaries
tmp0:
        .RES    2               ; $5A-$5B
tmp1:
        .RES    2               ; $5C-$5D
tmp2:
        .RES    2               ; $5E-$5F
tmp3:
        .RES    2               ; $60-$61

; Byte-width scratch (often used by macros)
scratch:
        .RES    1               ; $62

; ---------------------------------------------------------------------------
; Compiler-specific zero-page variables (only used by PASCAL.COM)
; ---------------------------------------------------------------------------

; Lexer state
lex_line:
        .RES    2               ; $63-$64  current source line number
lex_col:
        .RES    1               ; $65      current column
lex_char:
        .RES    1               ; $66      lookahead character
tok_type:
        .RES    1               ; $67      current token type
tok_ival_lo:
        .RES    1               ; $68      integer token value (lo)
tok_ival_hi:
        .RES    1               ; $69      integer token value (hi)

; Parser / code generator state
cg_pc:
        .RES    2               ; $6A-$6B  current emit offset into code buffer
cg_globals:
        .RES    2               ; $6C-$6D  next available global variable offset
scope_depth:
        .RES    1               ; $6E      current lexical nesting depth (0=global)
sym_chain:
        .RES    2               ; $6F-$70  pointer into symbol table chain

; FCB pointers (compiler I/O)
src_fcb:
        .RES    2               ; $71-$72  pointer to source FCB
out_fcb:
        .RES    2               ; $73-$74  pointer to output FCB
src_buf_pos:
        .RES    1               ; $75      position within 128-byte sector buffer
src_buf_end:
        .RES    1               ; $76      bytes valid in sector buffer

; FOR-loop scratch (parse_for; not nestable)
for_loop_top:
        .RES    2               ; $77-$78  cg_pc of loop test (back-jump target)
for_patch:
        .RES    2               ; $79-$7A  FJP exit patch address

; Result type of the most recently parsed expression (TY_INT/TY_CHAR/
; TY_BOOL/TY_STRING/etc.).  Used by parse_write_args to dispatch the
; right WRIT* opcode.  Set by parse_factor / parse_simple_expr /
; parse_expression as each rule produces its result.
expr_type:
        .RES    1               ; $7B
