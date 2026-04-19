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

; --- Calls ---

emit_MRKSTK:                    ; local size in A
        PHA
        LDA     #OP_MRKSTK
        JSR     emit_byte
        PLA
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
