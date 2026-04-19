; codegen.asm — p-code emitter for Pascal/65 compiler
;
; All generated p-code accumulates in CODEBUF_BASE.
; cg_pc (zero page) tracks the next write offset.

.segment "CODE"

; ---------------------------------------------------------------------------
; emit_byte — append byte A to code buffer
; ---------------------------------------------------------------------------
emit_byte:
        pha                     ; save byte to emit
        ; check buffer full
        lda     cg_pc+1
        cmp     #>CODEBUF_MAXSZ
        bcc     :+
        lda     cg_pc
        cmp     #<CODEBUF_MAXSZ
        bcc     :+
        ; error: p-code buffer full
        pla
        lda     #<err_pcd_full
        sta     tmp0
        lda     #>err_pcd_full
        sta     tmp0+1
        jsr     compile_error
        rts
:
        ; store at CODEBUF_BASE + cg_pc
        lda     cg_pc
        clc
        adc     #<CODEBUF_BASE
        sta     tmp2
        lda     cg_pc+1
        adc     #>CODEBUF_BASE
        sta     tmp2+1
        pla
        ldy     #0
        sta     (tmp2),y
        ; advance cg_pc
        inc     cg_pc
        bne     :+
        inc     cg_pc+1
:       rts

; ---------------------------------------------------------------------------
; emit_word — append word (lo then hi) from tmp2 to code buffer
; ---------------------------------------------------------------------------
emit_word:
        lda     tmp2
        jsr     emit_byte
        lda     tmp2+1
        jsr     emit_byte
        rts

; ---------------------------------------------------------------------------
; emit_opcode — emit single opcode byte in A
; ---------------------------------------------------------------------------
emit_opcode:
        jsr     emit_byte
        rts

; --- Constant emitters ---

emit_LDCI:                      ; push sign-extended byte in A
        pha
        lda     #OP_LDCI
        jsr     emit_byte
        pla
        jsr     emit_byte
        rts

emit_LDCW:                      ; push word: lo in A, hi in scratch
        pha
        lda     #OP_LDCW
        jsr     emit_byte
        pla
        jsr     emit_byte
        lda     scratch
        jsr     emit_byte
        rts

emit_LDCC:                      ; push char in A
        pha
        lda     #OP_LDCC
        jsr     emit_byte
        pla
        jsr     emit_byte
        rts

emit_LDCB:                      ; push boolean in A (0=false, 1=true)
        pha
        lda     #OP_LDCB
        jsr     emit_byte
        pla
        jsr     emit_byte
        rts

emit_LDCN:
        lda     #OP_LDCN
        jsr     emit_byte
        rts

; --- Local variable emitters ---

emit_LDL:                       ; byte offset in A
        pha
        lda     #OP_LDL
        jsr     emit_byte
        pla
        jsr     emit_byte
        rts

emit_STL:
        pha
        lda     #OP_STL
        jsr     emit_byte
        pla
        jsr     emit_byte
        rts

emit_LDA_L:
        pha
        lda     #OP_LDA_L
        jsr     emit_byte
        pla
        jsr     emit_byte
        rts

; --- Global variable emitters ---

emit_LDG:                       ; word offset: lo in A, hi in scratch
        pha
        lda     #OP_LDG
        jsr     emit_byte
        pla
        jsr     emit_byte
        lda     scratch
        jsr     emit_byte
        rts

emit_STG:
        pha
        lda     #OP_STG
        jsr     emit_byte
        pla
        jsr     emit_byte
        lda     scratch
        jsr     emit_byte
        rts

; --- Arithmetic emitters ---

emit_ADI:
        lda     #OP_ADI
        jmp     emit_byte

emit_SBI:
        lda     #OP_SBI
        jmp     emit_byte

emit_MPI:
        lda     #OP_MPI
        jmp     emit_byte

emit_DVI:
        lda     #OP_DVI
        jmp     emit_byte

emit_MOD:
        lda     #OP_MOD
        jmp     emit_byte

emit_NGI:
        lda     #OP_NGI
        jmp     emit_byte

; --- Comparison emitters ---

emit_EQUI:
        lda     #OP_EQUI
        jmp     emit_byte

emit_NEQI:
        lda     #OP_NEQI
        jmp     emit_byte

emit_LESI:
        lda     #OP_LESI
        jmp     emit_byte

emit_LEQI:
        lda     #OP_LEQI
        jmp     emit_byte

emit_GTRI:
        lda     #OP_GTRI
        jmp     emit_byte

emit_GEQI:
        lda     #OP_GEQI
        jmp     emit_byte

; --- Logical ---

emit_LAND:
        lda     #OP_LAND
        jmp     emit_byte

emit_LOR:
        lda     #OP_LOR
        jmp     emit_byte

emit_LNOT:
        lda     #OP_LNOT
        jmp     emit_byte

; --- Jump emitters ---
; Returns: A/scratch = offset of the word operand in code buffer
; (needed for backpatching)

emit_UJP:                       ; emit UJP with placeholder offset
        lda     #OP_UJP
        jsr     emit_byte
        lda     cg_pc           ; save patch address
        pha
        lda     cg_pc+1
        pha
        lda     #$00
        jsr     emit_byte       ; placeholder lo
        lda     #$00
        jsr     emit_byte       ; placeholder hi
        pla
        sta     scratch
        pla                     ; lo of patch address
        rts                     ; caller: save A/scratch as patch point

emit_FJP:
        lda     #OP_FJP
        jsr     emit_byte
        lda     cg_pc
        pha
        lda     cg_pc+1
        pha
        lda     #$00
        jsr     emit_byte
        lda     #$00
        jsr     emit_byte
        pla
        sta     scratch
        pla
        rts

emit_TJP:
        lda     #OP_TJP
        jsr     emit_byte
        lda     cg_pc
        pha
        lda     cg_pc+1
        pha
        lda     #$00
        jsr     emit_byte
        lda     #$00
        jsr     emit_byte
        pla
        sta     scratch
        pla
        rts

; ---------------------------------------------------------------------------
; patch_jump — fill in jump offset at patch address stored in tmp2 (word)
; The offset is computed as: (current cg_pc) - (patch_addr + 2)
; i.e. relative to instruction after the operand word
; ---------------------------------------------------------------------------
patch_jump:
        ; delta = cg_pc - (patch_addr + 2)
        ;   compute (cg_pc - 2) first, then subtract patch_addr
        lda     cg_pc
        sec
        sbc     #2
        sta     scratch         ; lo of (cg_pc - 2)
        lda     cg_pc+1
        sbc     #0
        sta     scratch+1       ; hi of (cg_pc - 2)
        sec
        lda     scratch
        sbc     tmp2
        pha
        lda     scratch+1
        sbc     tmp2+1
        sta     scratch+1       ; high byte of delta
        pla
        sta     scratch         ; low byte of delta
        ; write delta at patch address (CODEBUF_BASE + tmp2)
        lda     tmp2
        clc
        adc     #<CODEBUF_BASE
        sta     tmp3
        lda     tmp2+1
        adc     #>CODEBUF_BASE
        sta     tmp3+1
        ldy     #0
        lda     scratch
        sta     (tmp3),y
        iny
        lda     scratch+1
        sta     (tmp3),y
        rts

; --- I/O emitters ---

emit_WRITI:
        lda     #OP_WRITI
        jmp     emit_byte

emit_WRITC:
        lda     #OP_WRITC
        jmp     emit_byte

emit_WRITB:
        lda     #OP_WRITB
        jmp     emit_byte

emit_WRITS:
        lda     #OP_WRITS
        jmp     emit_byte

emit_WRITLN:
        lda     #OP_WRITLN
        jmp     emit_byte

emit_READI:
        lda     #OP_READI
        jmp     emit_byte

emit_READC:
        lda     #OP_READC
        jmp     emit_byte

; --- Stack ---

emit_DUP:
        lda     #OP_DUP
        jmp     emit_byte

emit_POP:
        lda     #OP_POP
        jmp     emit_byte

; --- Calls ---

emit_MRKSTK:                    ; local size in A
        pha
        lda     #OP_MRKSTK
        jsr     emit_byte
        pla
        jmp     emit_byte

emit_RET:
        lda     #OP_RET
        jmp     emit_byte

emit_RETF:
        lda     #OP_RETF
        jmp     emit_byte

emit_HALT:
        lda     #OP_HALT
        jmp     emit_byte

; ---------------------------------------------------------------------------
; codegen_alloc_global — reserve N bytes in global area; return offset in tmp2
; N in A
; ---------------------------------------------------------------------------
codegen_alloc_global:
        lda     cg_globals
        sta     tmp2
        lda     cg_globals+1
        sta     tmp2+1
        ; advance by N (passed in A — caller puts size here)
        ; For now allocate 2 bytes (word) per variable
        lda     cg_globals
        clc
        adc     #2
        sta     cg_globals
        bcc     :+
        inc     cg_globals+1
:       rts
