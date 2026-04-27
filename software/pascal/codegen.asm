; codegen.asm — p-code emitter for Pascal/65 compiler
;
; All generated p-code accumulates in CODEBUF_BASE.
; cg_pc (zero page) tracks the next write offset.

        .segment        "CODE"

; ---------------------------------------------------------------------------
; emit_byte — append byte A to code buffer
; ---------------------------------------------------------------------------
emit_byte:
        PHA                     ; save byte to emit
; check buffer full
        LDA     cg_pc+1
        CMP     #>CODEBUF_MAXSZ
        BCC     :+
        LDA     cg_pc
        CMP     #<CODEBUF_MAXSZ
        BCC     :+
; error: p-code buffer full
        PLA
        LDA     #<err_pcd_full
        STA     tmp0
        LDA     #>err_pcd_full
        STA     tmp0+1
        JSR     compile_error
        RTS
:
; store at CODEBUF_BASE + cg_pc
        LDA     cg_pc
        CLC
        ADC     #<CODEBUF_BASE
        STA     tmp2
        LDA     cg_pc+1
        ADC     #>CODEBUF_BASE
        STA     tmp2+1
        PLA
        LDY     #0
        STA     (tmp2),y
; advance cg_pc
        INC     cg_pc
        BNE     :+
        INC     cg_pc+1
:
        RTS

; ---------------------------------------------------------------------------
; emit_word — append word (lo then hi) from tmp2 to code buffer
; ---------------------------------------------------------------------------
emit_word:
        LDA     tmp2
        JSR     emit_byte
        LDA     tmp2+1
        JSR     emit_byte
        RTS

; ---------------------------------------------------------------------------
; emit_opcode — emit single opcode byte in A
; ---------------------------------------------------------------------------
emit_opcode:
        JSR     emit_byte
        RTS

; --- Constant emitters ---

emit_LDCI:                      ; push sign-extended byte in A
        PHA
        LDA     #OP_LDCI
        JSR     emit_byte
        PLA
        JSR     emit_byte
        RTS

emit_LDCW:                      ; push word: lo in A, hi in scratch
        PHA
        LDA     #OP_LDCW
        JSR     emit_byte
        PLA
        JSR     emit_byte
        LDA     scratch
        JSR     emit_byte
        RTS

emit_LDCC:                      ; push char in A
        PHA
        LDA     #OP_LDCC
        JSR     emit_byte
        PLA
        JSR     emit_byte
        RTS

emit_LDCB:                      ; push boolean in A (0=false, 1=true)
        PHA
        LDA     #OP_LDCB
        JSR     emit_byte
        PLA
        JSR     emit_byte
        RTS

emit_LDCN:
        LDA     #OP_LDCN
        JSR     emit_byte
        RTS

; emit_LDCS — emit OP_LDCS followed by length byte and string bytes.
; Source string lives in ident_buf (length at [0], chars at [1..N]).
; The runtime will inline-fetch length+chars and push the address of
; the length byte as a Pascal-style string pointer.
emit_LDCS:
        LDA     #OP_LDCS
        JSR     emit_byte
        LDA     ident_buf       ; length
        STA     scratch         ; counter / length cache
        JSR     emit_byte       ; emit length byte
        LDX     #0
@loop:
        CPX     scratch
        BCS     @done
        LDA     ident_buf+1,x
        PHX
        JSR     emit_byte       ; clobbers tmp2 only
        PLX
        INX
        BRA     @loop
@done:
        RTS

; --- Local variable emitters ---

emit_LDL:                       ; byte offset in A
        PHA
        LDA     #OP_LDL
        JSR     emit_byte
        PLA
        JSR     emit_byte
        RTS

emit_STL:
        PHA
        LDA     #OP_STL
        JSR     emit_byte
        PLA
        JSR     emit_byte
        RTS

emit_LDA_L:
        PHA
        LDA     #OP_LDA_L
        JSR     emit_byte
        PLA
        JSR     emit_byte
        RTS

; --- Global variable emitters ---

emit_LDG:                       ; word offset: lo in A, hi in scratch
        PHA
        LDA     #OP_LDG
        JSR     emit_byte
        PLA
        JSR     emit_byte
        LDA     scratch
        JSR     emit_byte
        RTS

emit_STG:
        PHA
        LDA     #OP_STG
        JSR     emit_byte
        PLA
        JSR     emit_byte
        LDA     scratch
        JSR     emit_byte
        RTS

emit_LDA_G:                     ; word offset: lo in A, hi in scratch
        PHA
        LDA     #OP_LDA_G
        JSR     emit_byte
        PLA
        JSR     emit_byte
        LDA     scratch
        JSR     emit_byte
        RTS

; --- Indirect (VAR-param) emitters ---

emit_LDIND:
        LDA     #OP_LDIND
        JMP     emit_byte

emit_STIND:
        LDA     #OP_STIND
        JMP     emit_byte

; emit_INDEX — OP_INDEX + 2-byte element size word (lo in A, hi in scratch)
emit_INDEX:
        PHA
        LDA     #OP_INDEX
        JSR     emit_byte
        PLA
        JSR     emit_byte
        LDA     scratch
        JSR     emit_byte
        RTS

; emit_NEW — OP_NEW + 2-byte allocation size (lo in A, hi in scratch)
emit_NEW:
        PHA
        LDA     #OP_NEW
        JSR     emit_byte
        PLA
        JSR     emit_byte
        LDA     scratch
        JSR     emit_byte
        RTS

; emit_DISP — OP_DISP (no operand)
emit_DISP:
        LDA     #OP_DISP
        JMP     emit_byte

; ---------------------------------------------------------------------------
; codegen_alloc_array_global — allocate ARRAY [array_lo..array_hi] in globals
; Element size fixed at 2 (all scalars are word-sized).
; Returns: tmp2 = adjusted_offset = raw_base - lo*2
;          cg_globals advanced by (hi - lo + 1) * 2
; Clobbers: tmp2, tmp3
; ---------------------------------------------------------------------------
codegen_alloc_array_global:
; count = hi - lo + 1  (16-bit)
        LDA     array_hi
        SEC
        SBC     array_lo
        STA     tmp2
        LDA     array_hi+1
        SBC     array_lo+1
        STA     tmp2+1
        INC     tmp2
        BNE     :+
        INC     tmp2+1
:
; alloc_size = count * 2
        ASL     tmp2
        ROL     tmp2+1
; save raw_base = cg_globals before advancing
        LDA     cg_globals
        STA     tmp3
        LDA     cg_globals+1
        STA     tmp3+1
; advance cg_globals by alloc_size (tmp2)
        LDA     cg_globals
        CLC
        ADC     tmp2
        STA     cg_globals
        LDA     cg_globals+1
        ADC     tmp2+1
        STA     cg_globals+1
; adjusted_offset = raw_base (tmp3) - lo*2 (tmp2 reused)
        LDA     array_lo
        ASL
        STA     tmp2
        LDA     array_lo+1
        ROL
        STA     tmp2+1
        LDA     tmp3
        SEC
        SBC     tmp2
        STA     tmp2
        LDA     tmp3+1
        SBC     tmp2+1
        STA     tmp2+1
        RTS

; ---------------------------------------------------------------------------
; codegen_alloc_record_global — reserve record_size bytes in globals
; Returns: tmp2 = offset of record's first byte
;          cg_globals advanced by record_size
; ---------------------------------------------------------------------------
codegen_alloc_record_global:
        LDA     cg_globals
        STA     tmp2
        LDA     cg_globals+1
        STA     tmp2+1
        CLC
        LDA     cg_globals
        ADC     record_size
        STA     cg_globals
        LDA     cg_globals+1
        ADC     record_size+1
        STA     cg_globals+1
        RTS

; ---------------------------------------------------------------------------
; codegen_alloc_text_global — reserve FILE_STRUCT_SZ (168) bytes for a TEXT
; variable.  Returns tmp2 = offset of struct's first byte; cg_globals bumped.
; ---------------------------------------------------------------------------
codegen_alloc_text_global:
        LDA     cg_globals
        STA     tmp2
        LDA     cg_globals+1
        STA     tmp2+1
        CLC
        LDA     cg_globals
        ADC     #FILE_STRUCT_SZ
        STA     cg_globals
        LDA     cg_globals+1
        ADC     #0
        STA     cg_globals+1
        RTS

; --- Arithmetic emitters ---

emit_ADI:
        LDA     #OP_ADI
        JMP     emit_byte

emit_SBI:
        LDA     #OP_SBI
        JMP     emit_byte

emit_MPI:
        LDA     #OP_MPI
        JMP     emit_byte

emit_DVI:
        LDA     #OP_DVI
        JMP     emit_byte

emit_MPR:
        LDA     #OP_MPR
        JMP     emit_byte

emit_DVR:
        LDA     #OP_DVR
        JMP     emit_byte

emit_MOD:
        LDA     #OP_MOD
        JMP     emit_byte

emit_NGI:
        LDA     #OP_NGI
        JMP     emit_byte

; --- Comparison emitters ---

emit_EQUI:
        LDA     #OP_EQUI
        JMP     emit_byte

emit_NEQI:
        LDA     #OP_NEQI
        JMP     emit_byte

emit_LESI:
        LDA     #OP_LESI
        JMP     emit_byte

emit_LEQI:
        LDA     #OP_LEQI
        JMP     emit_byte

emit_GTRI:
        LDA     #OP_GTRI
        JMP     emit_byte

emit_GEQI:
        LDA     #OP_GEQI
        JMP     emit_byte

; --- Logical ---

emit_LAND:
        LDA     #OP_LAND
        JMP     emit_byte

emit_LOR:
        LDA     #OP_LOR
        JMP     emit_byte

emit_LNOT:
        LDA     #OP_LNOT
        JMP     emit_byte

emit_BNOT:
        LDA     #OP_BNOT
        JMP     emit_byte

emit_INSET:
        LDA     #OP_INSET
        JMP     emit_byte

; --- Jump emitters ---
; Returns: A/scratch = offset of the word operand in code buffer
; (needed for backpatching)

emit_UJP:                       ; emit UJP with placeholder offset
        LDA     #OP_UJP
        JSR     emit_byte
        LDA     cg_pc           ; save patch address
        PHA
        LDA     cg_pc+1
        PHA
        LDA     #$00
        JSR     emit_byte       ; placeholder lo
        LDA     #$00
        JSR     emit_byte       ; placeholder hi
        PLA
        STA     scratch
        PLA                     ; lo of patch address
        RTS                     ; caller: save A/scratch as patch point

emit_FJP:
        LDA     #OP_FJP
        JSR     emit_byte
        LDA     cg_pc
        PHA
        LDA     cg_pc+1
        PHA
        LDA     #$00
        JSR     emit_byte
        LDA     #$00
        JSR     emit_byte
        PLA
        STA     scratch
        PLA
        RTS

emit_TJP:
        LDA     #OP_TJP
        JSR     emit_byte
        LDA     cg_pc
        PHA
        LDA     cg_pc+1
        PHA
        LDA     #$00
        JSR     emit_byte
        LDA     #$00
        JSR     emit_byte
        PLA
        STA     scratch
        PLA
        RTS

; ---------------------------------------------------------------------------
; patch_jump — fill in jump offset at patch address stored in tmp2 (word)
; The offset is computed as: (current cg_pc) - (patch_addr + 2)
; i.e. relative to instruction after the operand word
; ---------------------------------------------------------------------------
patch_jump:
; delta = cg_pc - (patch_addr + 2)
;   compute (cg_pc - 2) first, then subtract patch_addr
        LDA     cg_pc
        SEC
        SBC     #2
        STA     scratch         ; lo of (cg_pc - 2)
        LDA     cg_pc+1
        SBC     #0
        STA     scratch+1       ; hi of (cg_pc - 2)
        SEC
        LDA     scratch
        SBC     tmp2
        PHA
        LDA     scratch+1
        SBC     tmp2+1
        STA     scratch+1       ; high byte of delta
        PLA
        STA     scratch         ; low byte of delta
; write delta at patch address (CODEBUF_BASE + tmp2)
        LDA     tmp2
        CLC
        ADC     #<CODEBUF_BASE
        STA     tmp3
        LDA     tmp2+1
        ADC     #>CODEBUF_BASE
        STA     tmp3+1
        LDY     #0
        LDA     scratch
        STA     (tmp3),y
        INY
        LDA     scratch+1
        STA     (tmp3),y
        RTS

; --- I/O emitters ---

emit_WRITI:
        LDA     #OP_WRITI
        JMP     emit_byte

emit_WRITC:
        LDA     #OP_WRITC
        JMP     emit_byte

emit_WRITB:
        LDA     #OP_WRITB
        JMP     emit_byte

emit_WRITS:
        LDA     #OP_WRITS
        JMP     emit_byte

emit_WRITLN:
        LDA     #OP_WRITLN
        JMP     emit_byte

emit_READI:
        LDA     #OP_READI
        JMP     emit_byte

emit_WRITR:
        LDA     #OP_WRITR
        JMP     emit_byte

emit_READR:
        LDA     #OP_READR
        JMP     emit_byte

emit_READC:
        LDA     #OP_READC
        JMP     emit_byte

; --- Stack ---

emit_DUP:
        LDA     #OP_DUP
        JMP     emit_byte

emit_POP:
        LDA     #OP_POP
        JMP     emit_byte

emit_SWAP:
        LDA     #OP_SWAP
        JMP     emit_byte

; --- Calls ---

emit_MRKSTK:                    ; local size in A
        PHA
        LDA     #OP_MRKSTK
        JSR     emit_byte
        PLA
        JMP     emit_byte

; emit_MRKA — A=pcount, X=lsize_extra
; Emits OP_MRKA + pcount byte + lsize_extra byte.  See op_MRKA in prun.asm
; for runtime semantics.
emit_MRKA:
        PHX                     ; save lsize_extra
        PHA                     ; save pcount
        LDA     #OP_MRKA
        JSR     emit_byte
        PLA                     ; pcount
        JSR     emit_byte
        PLA                     ; lsize_extra
        JMP     emit_byte

emit_RET:
        LDA     #OP_RET
        JMP     emit_byte

emit_RETF:
        LDA     #OP_RETF
        JMP     emit_byte

emit_STR:
        LDA     #OP_STR
        JMP     emit_byte

; emit_CALL — A = target absolute code-buffer addr lo, scratch = hi
; Emits OP_CALL + signed 16-bit offset = target - (cg_pc_after_operand)
emit_CALL:
        PHA                     ; target lo
        LDA     scratch
        PHA                     ; target hi
        LDA     #OP_CALL
        JSR     emit_byte       ; clobbers tmp2; preserves scratch & 6502 stack
; compute offset = target - cg_pc - 2 (16-bit)
        PLA                     ; target hi
        TAX
        PLA                     ; target lo
        SEC
        SBC     #2
        PHA                     ; partial lo
        TXA                     ; target hi
        SBC     #0
        STA     scratch         ; partial hi
        PLA                     ; partial lo
        SEC
        SBC     cg_pc
        PHA                     ; offset lo
        LDA     scratch
        SBC     cg_pc+1
        STA     scratch         ; offset hi
        PLA
        JSR     emit_byte       ; emit offset lo
        LDA     scratch
        JSR     emit_byte       ; emit offset hi
        RTS

emit_HALT:
        LDA     #OP_HALT
        JMP     emit_byte

; --- String built-ins ---

emit_LEN:
        LDA     #OP_LEN
        JMP     emit_byte

emit_POS:
        LDA     #OP_POS
        JMP     emit_byte

emit_COPY:
        LDA     #OP_COPY
        JMP     emit_byte

emit_CONCAT:
        LDA     #OP_CONCAT
        JMP     emit_byte

; --- TEXT file I/O ---

emit_FASSGN:
        LDA     #OP_FASSGN
        JMP     emit_byte
emit_FRESET:
        LDA     #OP_FRESET
        JMP     emit_byte
emit_FREWRT:
        LDA     #OP_FREWRT
        JMP     emit_byte
emit_FCLOSE:
        LDA     #OP_FCLOSE
        JMP     emit_byte
emit_FWRC:
        LDA     #OP_FWRC
        JMP     emit_byte
emit_FWRS:
        LDA     #OP_FWRS
        JMP     emit_byte
emit_FWRI:
        LDA     #OP_FWRI
        JMP     emit_byte
emit_FWRR:
        LDA     #OP_FWRR
        JMP     emit_byte
emit_FWLN:
        LDA     #OP_FWLN
        JMP     emit_byte
emit_FRDC:
        LDA     #OP_FRDC
        JMP     emit_byte
emit_FRDI:
        LDA     #OP_FRDI
        JMP     emit_byte
emit_FRDR:
        LDA     #OP_FRDR
        JMP     emit_byte
emit_FRDLN:
        LDA     #OP_FRDLN
        JMP     emit_byte
emit_FEOF:
        LDA     #OP_FEOF
        JMP     emit_byte
emit_FAPPND:
        LDA     #OP_FAPPND
        JMP     emit_byte
emit_FRDS:
        LDA     #OP_FRDS
        JMP     emit_byte
emit_FWRB:
        LDA     #OP_FWRB
        JMP     emit_byte
emit_FEOLN:
        LDA     #OP_FEOLN
        JMP     emit_byte

; ---------------------------------------------------------------------------
; codegen_alloc_global — reserve N bytes in global area; return offset in tmp2
; N in A
; ---------------------------------------------------------------------------
codegen_alloc_global:
        LDA     cg_globals
        STA     tmp2
        LDA     cg_globals+1
        STA     tmp2+1
; advance by N (passed in A — caller puts size here)
; For now allocate 2 bytes (word) per variable
        LDA     cg_globals
        CLC
        ADC     #2
        STA     cg_globals
        BCC     :+
        INC     cg_globals+1
:
        RTS
