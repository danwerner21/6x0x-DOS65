; zeropage.asm — zero-page variable layout for PASCAL.COM and PRUN.COM
;
; Avoid $00-$2F (DOS/65 system), $32 (FARFUNCT), $39-$3A (DOS/65).
; Safe range: $50-$EF

.segment "ZEROPAGE"

        .res    $50             ; skip $00-$4F (DOS/65 system zero page)

; ---------------------------------------------------------------------------
; P-machine registers — start at $50 as noted in comments below
; ---------------------------------------------------------------------------

pm_ipc:         .res 2          ; $50-$51  interpreter program counter
pm_sp:          .res 2          ; $52-$53  p-machine stack pointer
pm_mp:          .res 2          ; $54-$55  mark/frame pointer
pm_base:        .res 2          ; $56-$57  global variable base
pm_np:          .res 2          ; $58-$59  heap top (grows downward)

; General-purpose temporaries
tmp0:           .res 2          ; $5A-$5B
tmp1:           .res 2          ; $5C-$5D
tmp2:           .res 2          ; $5E-$5F
tmp3:           .res 2          ; $60-$61

; Byte-width scratch (often used by macros)
scratch:        .res 1          ; $62

; ---------------------------------------------------------------------------
; Compiler-specific zero-page variables (only used by PASCAL.COM)
; ---------------------------------------------------------------------------

; Lexer state
lex_line:       .res 2          ; $63-$64  current source line number
lex_col:        .res 1          ; $65      current column
lex_char:       .res 1          ; $66      lookahead character
tok_type:       .res 1          ; $67      current token type
tok_ival_lo:    .res 1          ; $68      integer token value (lo)
tok_ival_hi:    .res 1          ; $69      integer token value (hi)

; Parser / code generator state
cg_pc:          .res 2          ; $6A-$6B  current emit offset into code buffer
cg_globals:     .res 2          ; $6C-$6D  next available global variable offset
scope_depth:    .res 1          ; $6E      current lexical nesting depth (0=global)
sym_chain:      .res 2          ; $6F-$70  pointer into symbol table chain

; FCB pointers (compiler I/O)
src_fcb:        .res 2          ; $71-$72  pointer to source FCB
out_fcb:        .res 2          ; $73-$74  pointer to output FCB
src_buf_pos:    .res 1          ; $75      position within 128-byte sector buffer
src_buf_end:    .res 1          ; $76      bytes valid in sector buffer

; FOR-loop scratch (parse_for; not nestable)
for_loop_top:   .res 2          ; $77-$78  cg_pc of loop test (back-jump target)
for_patch:      .res 2          ; $79-$7A  FJP exit patch address
