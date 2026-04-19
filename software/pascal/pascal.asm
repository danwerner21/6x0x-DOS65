; pascal.asm — Pascal/65 Compiler for DOS/65
;
; Usage: PASCAL <sourcename>
;   Reads <sourcename>.PAS, writes <sourcename>.PCD
;
; Phase 1 entry point and top-level driver only.
; Lexer, parser, symbol table, and code generator are in separate files
; included at the bottom of this file.

.include "definitions.asm"
.include "zeropage.asm"

; ---------------------------------------------------------------------------
; FCBs and file buffers
; ---------------------------------------------------------------------------
.segment "CPMDATA"

comp_src_fcb:   .res 36         ; source file FCB (.PAS)
comp_out_fcb:   .res 36         ; output file FCB (.PCD)

; 128-byte sector buffer for source reads
src_sector:     .res 128

; Output sector buffer (128-byte sectors written to .PCD)
out_sector:     .res 128
out_sec_pos:    .byte 0         ; next write position in out_sector (0..127)

; Code generation buffer — p-code accumulates here until file write
; Overlaps the CPMDATA area above $2000 after FCBs.
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
.segment "TEA"

        jmp     pascal_main

.segment "CODE"

pascal_main:
        ; print banner
        lda     #<msg_banner
        sta     tmp0
        lda     #>msg_banner
        sta     tmp0+1
        jsr     console_print_sz

        ; build source FCB from DEFAULT_FCB ($0107) argument
        jsr     build_src_fcb
        bcc     :+
        jmp     @no_file
:
        ; print "Compiling: <name>"
        lda     #<msg_compiling
        sta     tmp0
        lda     #>msg_compiling
        sta     tmp0+1
        jsr     console_print_sz

        ; open source file
        lda     #<comp_src_fcb
        sta     src_fcb
        lda     #>comp_src_fcb
        sta     src_fcb+1
        lda     #<comp_src_fcb
        sta     tmp1
        lda     #>comp_src_fcb
        sta     tmp1+1
        jsr     file_open
        cmp     #$FF            ; PEM open: $FF=not found, 0-3=dir entry
        bne     :+
        jmp     @no_file
:       lda     #'1'            ; DBG: source file opened
        jsr     dbg_putc

        ; create output .PCD file
        jsr     build_out_fcb
        lda     #<comp_out_fcb
        sta     tmp1
        lda     #>comp_out_fcb
        sta     tmp1+1
        jsr     file_make
        cmp     #$FF            ; PEM make: $FF=dir full, 0-3=dir entry
        bne     :+
        jmp     @out_err
:
        lda     #'2'            ; DBG: output file made
        jsr     dbg_putc

        ; initialise compiler state
        jsr     compiler_init
        lda     #'3'            ; DBG: compiler_init done
        jsr     dbg_putc

        ; run compilation
        jsr     compile_program
        lda     #'4'            ; DBG: compile_program done
        jsr     dbg_putc
        ; DBG: print cg_pc (bytes emitted)
        lda     #'='
        jsr     dbg_putc
        lda     cg_pc
        sta     tmp0
        lda     cg_pc+1
        sta     tmp0+1
        jsr     console_print_dec
        lda     #' '
        jsr     dbg_putc

        ; write .PCD file
        jsr     write_pcd
        lda     #'5'            ; DBG: write_pcd done
        jsr     dbg_putc

        ; close files
        lda     #<comp_src_fcb
        sta     tmp1
        lda     #>comp_src_fcb
        sta     tmp1+1
        jsr     file_close
        lda     #<comp_out_fcb
        sta     tmp1
        lda     #>comp_out_fcb
        sta     tmp1+1
        jsr     file_close

        ; print result
        lda     error_count
        ora     error_count+1
        bne     @had_errors
        lda     #<msg_ok
        sta     tmp0
        lda     #>msg_ok
        sta     tmp0+1
        jsr     console_print_sz
        jmp     WARM_BOOT

@had_errors:
        lda     error_count
        sta     tmp0
        lda     error_count+1
        sta     tmp0+1
        jsr     console_print_dec
        lda     #<msg_errors
        sta     tmp0
        lda     #>msg_errors
        sta     tmp0+1
        jsr     console_print_sz
        jmp     WARM_BOOT

@no_file:
        lda     #<err_nofile
        sta     tmp0
        lda     #>err_nofile
        sta     tmp0+1
        jsr     console_print_sz
        jmp     WARM_BOOT

@out_err:
        lda     #<err_outfile
        sta     tmp0
        lda     #>err_outfile
        sta     tmp0+1
        jsr     console_print_sz
        jmp     WARM_BOOT

; ---------------------------------------------------------------------------
; build_src_fcb — populate comp_src_fcb from DEFAULT_FCB ($0107), force ".PAS"
; DOS/65 CCM copies the parsed argument FCB to dflfcb=$0107, not $005C.
; Returns: carry clear = ok, carry set = no filename
; ---------------------------------------------------------------------------
build_src_fcb:
        ldx     #0
@copy:  lda     DEFAULT_FCB,x
        sta     comp_src_fcb,x
        inx
        cpx     #12
        bcc     @copy
        lda     #0
@zero:  sta     comp_src_fcb,x
        inx
        cpx     #36
        bcc     @zero
        lda     comp_src_fcb+1
        cmp     #' '
        beq     @noname
        lda     #'P'
        sta     comp_src_fcb+9
        lda     #'A'
        sta     comp_src_fcb+10
        lda     #'S'
        sta     comp_src_fcb+11
        clc
        rts
@noname:
        sec
        rts

; ---------------------------------------------------------------------------
; build_out_fcb — copy filename from comp_src_fcb, change extension to "PCD"
; ---------------------------------------------------------------------------
build_out_fcb:
        ldx     #0
@copy:  lda     comp_src_fcb,x
        sta     comp_out_fcb,x
        inx
        cpx     #36
        bcc     @copy
        lda     #'P'
        sta     comp_out_fcb+9
        lda     #'C'
        sta     comp_out_fcb+10
        lda     #'D'
        sta     comp_out_fcb+11
        rts

; ---------------------------------------------------------------------------
; compiler_init — zero-out compiler state
; ---------------------------------------------------------------------------
error_count:    .word 0

compiler_init:
        lda     #0
        sta     error_count
        sta     error_count+1
        ; IPC starts at 0 (offset into code buffer)
        sta     cg_pc
        sta     cg_pc+1
        ; globals start at 0
        sta     cg_globals
        sta     cg_globals+1
        ; scope depth 0 = global
        sta     scope_depth
        ; init lexer and prime first token
        jsr     lexer_init
        jsr     next_token
        rts

; ---------------------------------------------------------------------------
; compile_error — print error with line/col; increment error_count
; tmp0 = pointer to error string
; ---------------------------------------------------------------------------
compile_error:
        ; save caller's message pointer (tmp0) into tmp3
        lda     tmp0
        sta     tmp3
        lda     tmp0+1
        sta     tmp3+1
        ; print "Line "
        lda     #<msg_line
        sta     tmp0
        lda     #>msg_line
        sta     tmp0+1
        jsr     console_print_sz
        ; print line number
        lda     lex_line
        sta     tmp0
        lda     lex_line+1
        sta     tmp0+1
        jsr     console_print_dec
        ; print ": "
        lda     #':'
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        lda     #' '
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        ; print the error message text
        lda     tmp3
        sta     tmp0
        lda     tmp3+1
        sta     tmp0+1
        jsr     console_print_sz
        jsr     console_newline
        ; increment error count
        inc     error_count
        bne     :+
        inc     error_count+1
:       rts

; ---------------------------------------------------------------------------
; write_pcd — write .PCD header + code buffer to output file
; ---------------------------------------------------------------------------
write_pcd:
        ; build header in out_sector
        lda     #PCD_MAGIC_0
        sta     out_sector+0
        lda     #PCD_MAGIC_1
        sta     out_sector+1
        lda     #PCD_VERSION
        sta     out_sector+2
        lda     #0
        sta     out_sector+3
        ; code size
        lda     cg_pc
        sta     out_sector+PCD_CODESZ
        lda     cg_pc+1
        sta     out_sector+PCD_CODESZ+1
        ; global size
        lda     cg_globals
        sta     out_sector+PCD_GLOBSZ
        lda     cg_globals+1
        sta     out_sector+PCD_GLOBSZ+1
        ; string pool size (placeholder)
        lda     #0
        sta     out_sector+PCD_STRSZ
        sta     out_sector+PCD_STRSZ+1
        ; entry point = 0 (main starts at beginning of code)
        sta     out_sector+PCD_ENTRY
        sta     out_sector+PCD_ENTRY+1
        ; zero remainder of first output sector
        ldx     #PCD_HEADER_SZ
@zero:  sta     out_sector,x
        inx
        cpx     #128
        bcc     @zero

        ; copy code buffer header into sector (bytes after header)
        ldy     #PCD_HEADER_SZ
        ldx     #0
@fill:  lda     CODEBUF_BASE,x
        sta     out_sector,y
        inx
        iny
        cpy     #128
        bcc     @fill

        ; write first sector
        ; (copy out_sector → DMA_BUF, then PEM write)
        ldy     #0
@dma:   lda     out_sector,y
        sta     DMA_BUF,y
        iny
        cpy     #128
        bcc     @dma
        lda     #<comp_out_fcb
        sta     tmp1
        lda     #>comp_out_fcb
        sta     tmp1+1
        jsr     file_write_sector

        ; TODO: write additional sectors for code > (128-PCD_HEADER_SZ) bytes

        rts

; ---------------------------------------------------------------------------
; dbg_putc — print A as a single character via PEM #2
; ---------------------------------------------------------------------------
dbg_putc:
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        rts

; ---------------------------------------------------------------------------
; Include sub-modules (compiler phases)
; ---------------------------------------------------------------------------
.include "lexer.asm"
.include "parser.asm"
.include "symtab.asm"
.include "codegen.asm"
.include "iolib.asm"
.include "messages.asm"
