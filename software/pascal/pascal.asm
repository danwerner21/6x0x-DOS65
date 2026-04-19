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

; Code generation buffer — p-code accumulates here until file write
; Overlaps the CPMDATA area above $2400 after FCBs.
; This holds up to ~32KB of generated p-code.
CODEBUF_BASE    = $3000
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
        LDA     #'1'            ; DBG: source file opened
        JSR     dbg_putc

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
        LDA     #'2'            ; DBG: output file made
        JSR     dbg_putc

; initialise compiler state
        JSR     compiler_init
        LDA     #'3'            ; DBG: compiler_init done
        JSR     dbg_putc

; run compilation
        JSR     compile_program
        LDA     #'4'            ; DBG: compile_program done
        JSR     dbg_putc
; DBG: print cg_pc (bytes emitted)
        LDA     #'='
        JSR     dbg_putc
        LDA     cg_pc
        STA     tmp0
        LDA     cg_pc+1
        STA     tmp0+1
        JSR     console_print_dec
        LDA     #' '
        JSR     dbg_putc

; write .PCD file
        JSR     write_pcd
        LDA     #'5'            ; DBG: write_pcd done
        JSR     dbg_putc

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
; main entry-point patch slot — sentinel hi byte = $FF means "none/done"
        LDA     #$FF
        STA     main_jmp_patch+1
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
; dbg_putc — print A as a single character via PEM #2
; ---------------------------------------------------------------------------
dbg_putc:
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
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
