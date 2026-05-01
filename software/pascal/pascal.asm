; pascal.asm — Pascal/65 Compiler for DOS/65
;
; Usage: PASCAL <sourcename>
;   Reads <sourcename>.PAS, writes <sourcename>.PCD
;
; Phase 1 entry point and top-level driver only.
; Lexer, parser, symbol table, and code generator are in separate files
; included at the bottom of this file.

.PC02   ; enable 65C02 (bra/phx/plx) for write_pcd etc.
        ; lexer.asm also asserts this; included files
        ; pick it up after lexer.asm runs.

        .include        "DEFINITIONS.ASM"
        .include        "ZEROPAGE.ASM"

; ---------------------------------------------------------------------------
; FCBs and file buffers
; ---------------------------------------------------------------------------
        .segment        "CPMDATA"

comp_src_fcb:
        .RES    36              ; source file FCB (.PAS)
comp_out_fcb:
        .RES    36              ; output file FCB (.PCD)

; 128-byte sector buffer for source reads
src_sector:
        .RES    128

; Comma-list buffer for parse_var_decls — collects names "A, B, C" before
; the type is known so each can be added to the symbol table with the
; same type code.  8 slots × 16 bytes; excess names are silently dropped.
var_name_count:
        .RES    1
var_name_buf:
        .RES    128

; Field table for RECORD types.  32 entries × 16 bytes each:
;   [0]    field name length (1-12)
;   [1-12] field name chars (uppercase, padded with spaces)
;   [13]   byte offset within the record
;   [14]   field data type (TY_INT/TY_CHAR/TY_BOOL)
;   [15]   reserved
field_table_count:
        .RES    1
field_table:
        .RES    512             ; 32 entries × 16 bytes

; Per-field "nested" metadata, parallel to field_table.  Only meaningful
; when the field's type is TY_RECORD: holds the nested record's
; first_field index and field count so chained access (`a.outer.inner`)
; can traverse into the inner record.  Anonymous and named nested
; records are both supported here.
field_nested_first:
        .RES    32
field_nested_count:
        .RES    32
field_ptr_meta:
        .RES    32

; Separate buffer for collecting field names inside a RECORD declaration.
; Distinct from var_name_buf so inline `VAR x: RECORD ... END` doesn't
; lose the outer "x" while the field group is being parsed. Each active
; RECORD nesting depth gets its own slice so an inner anonymous RECORD
; doesn't overwrite the outer field names that still need to be added.
FIELD_NAME_DEPTHS = 8
field_name_count:
        .RES    FIELD_NAME_DEPTHS
field_name_buf:
        .RES    128 * FIELD_NAME_DEPTHS

; Scratch slots used by @rec_addf to remember the most recently-parsed
; field's nested-record metadata.  Populated by the wrapper around the
; recursive parse_type_spec call when the field type is TY_RECORD.
nest_save_first:
        .RES    1
nest_save_count:
        .RES    1
nest_save_size:
        .RES    2

; Nesting depth tracked by parse_type_spec @rec_loop. 0 = outside any
; RECORD; bumped on entry, decremented on exit. Stored into byte 15 of
; each field_table entry by field_table_add so field_lookup_in_record
; can skip slots that belong to a deeper inline record. Without this
; an outer record whose later fields follow an inline RECORD would
; have an interleaved layout that count-based scan can't navigate.
field_depth:
        .RES    1
field_lookup_depth:
        .RES    1

; Pointer-type metadata table. Each pointer type remembers the pointee
; type, its allocation size for NEW, and a few aux bytes that are
; interpreted by pointee type:
;   TY_RECORD -> aux0=first_field, aux1=field_count
;   TY_ARRAY  -> aux0=elem_type,   aux1=elem_size (currently always 2)
;   TY_PTR    -> aux0=child ptr-meta index
PTR_META_MAX = 32
ptr_meta_count:
        .RES    1
ptr_meta_type:
        .RES    PTR_META_MAX
ptr_meta_size_lo:
        .RES    PTR_META_MAX
ptr_meta_size_hi:
        .RES    PTR_META_MAX
ptr_meta_aux0:
        .RES    PTR_META_MAX
ptr_meta_aux1:
        .RES    PTR_META_MAX
ptr_meta_aux2:
        .RES    PTR_META_MAX

; Active WITH contexts. Each entry stores a hidden global word that holds
; the selected record's runtime base address, plus that record's first-field
; index/count so unqualified identifiers can resolve as fields.
WITH_DEPTHS = 8
with_depth:
        .RES    1
with_base_lo:
        .RES    WITH_DEPTHS
with_base_hi:
        .RES    WITH_DEPTHS
with_first_field:
        .RES    WITH_DEPTHS
with_field_count:
        .RES    WITH_DEPTHS

; Record-valued expressions leave an address on the stack and publish the
; matching field-table span here so WITH selectors can reuse it.
expr_record_first:
        .RES    1
expr_record_count:
        .RES    1
expr_ptr_meta:
        .RES    1

; Scratch filled by with_lookup_field before parse_with_field_{load,assign}.
with_lookup_off:
        .RES    1
with_lookup_type:
        .RES    1
with_lookup_base_lo:
        .RES    1
with_lookup_base_hi:
        .RES    1
with_lookup_first:
        .RES    1
with_lookup_count:
        .RES    1
with_lookup_ptrmeta:
        .RES    1

; Top-level source mode: 0=plain program, 1=UNIT interface, 2=UNIT
; implementation. Used so interface routine headings can be rebound to
; their later implementation bodies without creating duplicate globals.
unit_section:
        .RES    1
unit_import_mode:
        .RES    1               ; 0=standalone source, nonzero=USES-import nesting depth

; Names collected from a single top-level USES clause. Separate from
; var_name_buf because imported unit compilation reuses var_name_buf.
USES_NAME_SLOTS = 8
uses_name_count:
        .RES    1
uses_name_buf:
        .RES    16 * USES_NAME_SLOTS

; Imported-unit de-duplication table. Names are stored in uppercase
; ident_buf layout (length byte + 15 chars) so nested/repeated USES
; clauses compile a unit at most once.
USED_UNIT_SLOTS = 8
used_unit_count:
        .RES    1
used_unit_buf:
        .RES    16 * USED_UNIT_SLOTS

; Source-context stack for nested USES imports. Each entry saves the
; current source FCB, buffered DMA sector, and lexer position so the
; compiler can suspend one file, compile another, then resume cleanly.
SRC_CTX_DEPTHS = 4
src_ctx_depth:
        .RES    1
src_ctx_buf_pos:
        .RES    SRC_CTX_DEPTHS
src_ctx_buf_end:
        .RES    SRC_CTX_DEPTHS
src_ctx_line_lo:
        .RES    SRC_CTX_DEPTHS
src_ctx_line_hi:
        .RES    SRC_CTX_DEPTHS
src_ctx_col:
        .RES    SRC_CTX_DEPTHS
src_ctx_char:
        .RES    SRC_CTX_DEPTHS
src_ctx_fcb:
        .RES    36 * SRC_CTX_DEPTHS
src_ctx_dma:
        .RES    128 * SRC_CTX_DEPTHS

; Code generation buffer — p-code accumulates here until file write.
; Keep this above the live CPMDATA working set used by the parser.
; This holds up to ~32KB of generated p-code.
CODEBUF_BASE    = $4900
CODEBUF_MAXSZ   = $8000         ; 32 KB

; Symbol table area
SYMTAB_BASE     = $8000
SYMTAB_MAXSZ    = $1800         ; 6 KB (≈200 entries at ~30 bytes each)

; String pool for identifier strings and string literals
STRPOOL_BASE    = $9800
STRPOOL_MAXSZ   = $0800         ; 2 KB

; ---------------------------------------------------------------------------
; Entry point
; ---------------------------------------------------------------------------
        .segment        "TEA"

        JMP     pascal_main

        .segment        "CODE"

pascal_main:
; print banner
        LDA     #<msg_banner
        STA     tmp0
        LDA     #>msg_banner
        STA     tmp0+1
        JSR     console_print_sz

; build source FCB from DEFAULT_FCB ($0107) argument
        JSR     build_src_fcb
        BCC     :+
        JMP     @no_file
:
; print "Compiling: <name>"
        LDA     #<msg_compiling
        STA     tmp0
        LDA     #>msg_compiling
        STA     tmp0+1
        JSR     console_print_sz

; open source file
        LDA     #<comp_src_fcb
        STA     src_fcb
        LDA     #>comp_src_fcb
        STA     src_fcb+1
        LDA     #<comp_src_fcb
        STA     tmp1
        LDA     #>comp_src_fcb
        STA     tmp1+1
        JSR     file_open
        CMP     #$FF            ; PEM open: $FF=not found, 0-3=dir entry
        BNE     :+
        JMP     @no_file
:

; create output .PCD file
        JSR     build_out_fcb
        LDA     #<comp_out_fcb
        STA     tmp1
        LDA     #>comp_out_fcb
        STA     tmp1+1
        JSR     file_make
        CMP     #$FF            ; PEM make: $FF=dir full, 0-3=dir entry
        BNE     :+
        JMP     @out_err
:

; initialise compiler state
        JSR     compiler_init

; run compilation
        JSR     compile_program

; write .PCD file
        JSR     write_pcd

; close files
        LDA     #<comp_src_fcb
        STA     tmp1
        LDA     #>comp_src_fcb
        STA     tmp1+1
        JSR     file_close
        LDA     #<comp_out_fcb
        STA     tmp1
        LDA     #>comp_out_fcb
        STA     tmp1+1
        JSR     file_close

; print result
        LDA     error_count
        ORA     error_count+1
        BNE     @had_errors
        LDA     #<msg_ok
        STA     tmp0
        LDA     #>msg_ok
        STA     tmp0+1
        JSR     console_print_sz
        JMP     WARM_BOOT

@had_errors:
        LDA     error_count
        STA     tmp0
        LDA     error_count+1
        STA     tmp0+1
        JSR     console_print_dec
        LDA     #<msg_errors
        STA     tmp0
        LDA     #>msg_errors
        STA     tmp0+1
        JSR     console_print_sz
        JMP     WARM_BOOT

@no_file:
        LDA     #<err_nofile
        STA     tmp0
        LDA     #>err_nofile
        STA     tmp0+1
        JSR     console_print_sz
        JMP     WARM_BOOT

@out_err:
        LDA     #<err_outfile
        STA     tmp0
        LDA     #>err_outfile
        STA     tmp0+1
        JSR     console_print_sz
        JMP     WARM_BOOT

; ---------------------------------------------------------------------------
; build_src_fcb — populate comp_src_fcb from DEFAULT_FCB ($0107), force ".PAS"
; DOS/65 CCM copies the parsed argument FCB to dflfcb=$0107, not $005C.
; Returns: carry clear = ok, carry set = no filename
; ---------------------------------------------------------------------------
build_src_fcb:
        LDX     #0
@copy:
        LDA     DEFAULT_FCB,x
        STA     comp_src_fcb,x
        INX
        CPX     #12
        BCC     @copy
        LDA     #0
@zero:
        STA     comp_src_fcb,x
        INX
        CPX     #36
        BCC     @zero
        LDA     comp_src_fcb+1
        CMP     #' '
        BEQ     @noname
        LDA     #'P'
        STA     comp_src_fcb+9
        LDA     #'A'
        STA     comp_src_fcb+10
        LDA     #'S'
        STA     comp_src_fcb+11
        CLC
        RTS
@noname:
        SEC
        RTS

; ---------------------------------------------------------------------------
; build_out_fcb — copy filename from comp_src_fcb, change extension to "PCD"
; ---------------------------------------------------------------------------
build_out_fcb:
        LDX     #0
@copy:
        LDA     comp_src_fcb,x
        STA     comp_out_fcb,x
        INX
        CPX     #36
        BCC     @copy
        LDA     #'P'
        STA     comp_out_fcb+9
        LDA     #'C'
        STA     comp_out_fcb+10
        LDA     #'D'
        STA     comp_out_fcb+11
        RTS

; ---------------------------------------------------------------------------
; build_named_src_fcb — populate comp_src_fcb from ident_buf, force ".PAS"
; The identifier is uppercased already by the lexer. Names longer than 8
; chars are truncated to fit the FCB filename field.
; Returns: carry clear = ok, carry set = empty name
; ---------------------------------------------------------------------------
build_named_src_fcb:
        LDA     ident_buf
        BNE     :+
        SEC
        RTS
:       LDX     #0
        LDA     #0
@zero:
        STA     comp_src_fcb,x
        INX
        CPX     #36
        BCC     @zero
        LDX     #1
        LDA     #' '
@pad_name:
        STA     comp_src_fcb,x
        INX
        CPX     #12
        BCC     @pad_name
        LDX     #0
@copy_name:
        CPX     ident_buf
        BCS     @set_ext
        CPX     #8
        BCS     @set_ext
        LDA     ident_buf+1,x
        STA     comp_src_fcb+1,x
        INX
        BRA     @copy_name
@set_ext:
        LDA     #'P'
        STA     comp_src_fcb+9
        LDA     #'A'
        STA     comp_src_fcb+10
        LDA     #'S'
        STA     comp_src_fcb+11
        CLC
        RTS

; source_ctx_fcb_ptr — A=stack index, returns tmp0 -> saved FCB slot
source_ctx_fcb_ptr:
        STA     scratch
        LDA     #<src_ctx_fcb
        STA     tmp0
        LDA     #>src_ctx_fcb
        STA     tmp0+1
@scfp_loop:
        LDA     scratch
        BEQ     @scfp_done
        CLC
        LDA     tmp0
        ADC     #36
        STA     tmp0
        BCC     :+
        INC     tmp0+1
:       DEC     scratch
        BRA     @scfp_loop
@scfp_done:
        RTS

; source_ctx_dma_ptr — A=stack index, returns tmp0 -> saved DMA slot
source_ctx_dma_ptr:
        STA     scratch
        LDA     #<src_ctx_dma
        STA     tmp0
        LDA     #>src_ctx_dma
        STA     tmp0+1
@scdp_loop:
        LDA     scratch
        BEQ     @scdp_done
        CLC
        LDA     tmp0
        ADC     #$80
        STA     tmp0
        BCC     :+
        INC     tmp0+1
:       DEC     scratch
        BRA     @scdp_loop
@scdp_done:
        RTS

; ---------------------------------------------------------------------------
; source_push_current / source_pop_current — save and restore the active
; source-file context around USES-imported unit compilation.
; source_push_current returns carry set if the context stack is full.
; ---------------------------------------------------------------------------
source_push_current:
        LDX     src_ctx_depth
        CPX     #SRC_CTX_DEPTHS
        BCC     @spc_room
        SEC
        RTS
@spc_room:
        LDA     src_buf_pos
        STA     src_ctx_buf_pos,x
        LDA     src_buf_end
        STA     src_ctx_buf_end,x
        LDA     lex_line
        STA     src_ctx_line_lo,x
        LDA     lex_line+1
        STA     src_ctx_line_hi,x
        LDA     lex_col
        STA     src_ctx_col,x
        LDA     lex_char
        STA     src_ctx_char,x
        TXA
        JSR     source_ctx_fcb_ptr
        LDY     #0
@spc_fcb_copy:
        LDA     comp_src_fcb,y
        STA     (tmp0),y
        INY
        CPY     #36
        BCC     @spc_fcb_copy
        TXA
        JSR     source_ctx_dma_ptr
        LDY     #0
@spc_dma_copy:
        LDA     DMA_BUF,y
        STA     (tmp0),y
        INY
        CPY     #128
        BCC     @spc_dma_copy
        INC     src_ctx_depth
        CLC
        RTS

source_pop_current:
        LDA     src_ctx_depth
        BEQ     @spop_done
        DEC     src_ctx_depth
        LDX     src_ctx_depth
        LDA     src_ctx_buf_pos,x
        STA     src_buf_pos
        LDA     src_ctx_buf_end,x
        STA     src_buf_end
        LDA     src_ctx_line_lo,x
        STA     lex_line
        LDA     src_ctx_line_hi,x
        STA     lex_line+1
        LDA     src_ctx_col,x
        STA     lex_col
        LDA     src_ctx_char,x
        STA     lex_char
        TXA
        JSR     source_ctx_fcb_ptr
        LDY     #0
@spop_fcb_copy:
        LDA     (tmp0),y
        STA     comp_src_fcb,y
        INY
        CPY     #36
        BCC     @spop_fcb_copy
        TXA
        JSR     source_ctx_dma_ptr
        LDY     #0
@spop_dma_copy:
        LDA     (tmp0),y
        STA     DMA_BUF,y
        INY
        CPY     #128
        BCC     @spop_dma_copy
        LDA     #<comp_src_fcb
        STA     src_fcb
        LDA     #>comp_src_fcb
        STA     src_fcb+1
@spop_done:
        RTS

; ---------------------------------------------------------------------------
; compiler_init — zero-out compiler state
; ---------------------------------------------------------------------------
error_count:
        .WORD   0

compiler_init:
        LDA     #0
        STA     error_count
        STA     error_count+1
; IPC starts at 0 (offset into code buffer)
        STA     cg_pc
        STA     cg_pc+1
; globals start at 0
        STA     cg_globals
        STA     cg_globals+1
; scope depth 0 = global
        STA     scope_depth
        STA     with_depth
        STA     expr_record_first
        STA     expr_record_count
        STA     unit_section
        STA     unit_import_mode
        STA     uses_name_count
        STA     used_unit_count
        STA     src_ctx_depth
        LDA     #$FF
        STA     expr_ptr_meta
; main entry-point patch slot — sentinel hi byte = $FF means "none/done"
        STA     main_jmp_patch+1
        STA     body_chain_patch+1
; init lexer and prime first token
        JSR     lexer_init
        JSR     next_token
        RTS

; ---------------------------------------------------------------------------
; compile_error — print error with line/col; increment error_count
; tmp0 = pointer to error string
; ---------------------------------------------------------------------------
compile_error:
; save caller's message pointer (tmp0) into tmp3
        LDA     tmp0
        STA     tmp3
        LDA     tmp0+1
        STA     tmp3+1
; print "Line "
        LDA     #<msg_line
        STA     tmp0
        LDA     #>msg_line
        STA     tmp0+1
        JSR     console_print_sz
; print line number
        LDA     lex_line
        STA     tmp0
        LDA     lex_line+1
        STA     tmp0+1
        JSR     console_print_dec
; print ": "
        LDA     #':'
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        LDA     #' '
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
; print the error message text
        LDA     tmp3
        STA     tmp0
        LDA     tmp3+1
        STA     tmp0+1
        JSR     console_print_sz
        JSR     console_newline
; increment error count
        INC     error_count
        BNE     :+
        INC     error_count+1
:
        RTS

; ---------------------------------------------------------------------------
; write_pcd — write .PCD header + entire code buffer to output file
;   Sector 0:        12-byte header  + first 116 bytes of code  (padded)
;   Sectors 1..N-1:  128 bytes of code each                     (last padded)
;
; Uses tmp0 = source pointer (into CODEBUF_BASE),
;      tmp3 = bytes remaining to write,
;      tmp1 = FCB pointer (re-loaded before each file_write_sector).
; ---------------------------------------------------------------------------
write_pcd:
; --- src_ptr = CODEBUF_BASE; remaining = cg_pc ---
        LDA     #<CODEBUF_BASE
        STA     tmp0
        LDA     #>CODEBUF_BASE
        STA     tmp0+1
        LDA     cg_pc
        STA     tmp3
        LDA     cg_pc+1
        STA     tmp3+1

; --- header bytes 0..11 directly into DMA_BUF ---
        LDA     #PCD_MAGIC_0
        STA     DMA_BUF+0
        LDA     #PCD_MAGIC_1
        STA     DMA_BUF+1
        LDA     #PCD_VERSION
        STA     DMA_BUF+2
        LDA     #0
        STA     DMA_BUF+3
        LDA     cg_pc
        STA     DMA_BUF+PCD_CODESZ
        LDA     cg_pc+1
        STA     DMA_BUF+PCD_CODESZ+1
        LDA     cg_globals
        STA     DMA_BUF+PCD_GLOBSZ
        LDA     cg_globals+1
        STA     DMA_BUF+PCD_GLOBSZ+1
        LDA     #0
        STA     DMA_BUF+PCD_STRSZ
        STA     DMA_BUF+PCD_STRSZ+1
        STA     DMA_BUF+PCD_ENTRY
        STA     DMA_BUF+PCD_ENTRY+1

; --- first sector: copy code into DMA_BUF[12..127] ---
        LDY     #0              ; src offset within this batch
        LDX     #PCD_HEADER_SZ  ; dest offset in DMA
@fill1:
        CPX     #128
        BCS     @write1
        LDA     tmp3
        ORA     tmp3+1
        BEQ     @pad1
        LDA     (tmp0),y
        STA     DMA_BUF,x
        INX
        INY
; remaining--
        LDA     tmp3
        BNE     :+
        DEC     tmp3+1
:
        DEC     tmp3
        BRA     @fill1
@pad1:
        CPX     #128
        BCS     @write1
        LDA     #0
        STA     DMA_BUF,x
        INX
        BRA     @pad1
@write1:
; advance src by Y (bytes consumed from this batch)
        STY     scratch
        CLC
        LDA     tmp0
        ADC     scratch
        STA     tmp0
        BCC     :+
        INC     tmp0+1
:
        LDA     #<comp_out_fcb
        STA     tmp1
        LDA     #>comp_out_fcb
        STA     tmp1+1
        JSR     file_write_sector

; --- additional sectors (full 128 bytes each, last pad with 0) ---
@next_sec:
        LDA     tmp3
        ORA     tmp3+1
        BEQ     @done
        LDY     #0
@fill2:
        CPY     #128
        BCS     @write2
        LDA     tmp3
        ORA     tmp3+1
        BEQ     @pad2
        LDA     (tmp0),y
        STA     DMA_BUF,y
        INY
        LDA     tmp3
        BNE     :+
        DEC     tmp3+1
:
        DEC     tmp3
        BRA     @fill2
@pad2:
        CPY     #128
        BCS     @write2
        LDA     #0
        STA     DMA_BUF,y
        INY
        BRA     @pad2
@write2:
; advance src by 128 (Y is the count copied from src this sector;
; padded bytes at the tail are not source bytes, but remaining is
; already 0 in that case and the loop will exit).
        STY     scratch
        CLC
        LDA     tmp0
        ADC     scratch
        STA     tmp0
        BCC     :+
        INC     tmp0+1
:
        LDA     #<comp_out_fcb
        STA     tmp1
        LDA     #>comp_out_fcb
        STA     tmp1+1
        JSR     file_write_sector
        BRA     @next_sec
@done:
        RTS

; ---------------------------------------------------------------------------
; Include sub-modules (compiler phases)
; ---------------------------------------------------------------------------
        .include        "LEXER.ASM"
        .include        "PARSER.ASM"
        .include        "SYMTAB.ASM"
        .include        "CODEGEN.ASM"
        .include        "IOLIB.ASM"
        .include        "MESSAGES.ASM"
