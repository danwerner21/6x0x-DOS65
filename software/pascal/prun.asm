; prun.asm — P-Code Runtime for DOS/65
;
; Usage: PRUN <filename>   (loads <filename>.PCD and executes it)
;
; Memory layout at runtime:
;   $0800-$17FF  this interpreter (code, dispatch tables, FCB, iolib, msgs)
;   $1800-$1FFF  p-code loaded from .PCD (2 KB)
;   $2000-$7FFF  p-machine value stack (grows up)
;   $8000-$AFFF  heap (grows down from $B000)
;   $B000-$B7DF  globals + string pool

.PC02                                   ; enable 65C02 (BRA/PHX/PHY) for string built-ins

        .include        "DEFINITIONS.ASM"
        .include        "ZEROPAGE.ASM"

; ---------------------------------------------------------------------------
; Runtime memory layout constants
; ---------------------------------------------------------------------------
; PCODE_BASE must lie above the end of the CODE segment so that loading
; a .PCD file does not overwrite iolib/messages or the dispatch table.
; CODE now reaches past $24DD, so move PCODE_BASE to $3000 for comfortable
; headroom; STACK_BASE at $5000 leaves 8 KB for p-code.
PCODE_BASE      = $3000         ; where p-code is loaded
STACK_BASE      = $5000         ; bottom of p-machine value stack
STACK_TOP       = $8000         ; top of stack (grows up)
HEAP_TOP        = $B000         ; heap grows down from here
GLOBALS_BASE    = $B000         ; global variable area (below heap top)

; Activation record offsets from MP
AR_DYN_LINK     = 0             ; 2 bytes: saved MP
AR_RET_ADDR     = 2             ; 2 bytes: saved IPC
AR_STAT_LINK    = 4             ; 2 bytes: static link
AR_RET_VAL      = 6             ; 2 bytes: function return value slot
AR_LOCALS       = 8             ; start of local variables

; ---------------------------------------------------------------------------
; P-code load buffer (at PCODE_BASE, filled by loader)
; Actual code lives at runtime in $1000 range.
; FCB and other data live in CODE segment (placed by ld65 after dispatch
; tables) — keeping them out of the $2000+ stack region.
; ---------------------------------------------------------------------------

        .segment        "TEA"

; ---------------------------------------------------------------------------
; Entry point — DOS/65 loads .COM at $0800 and jumps here
; ---------------------------------------------------------------------------
        JMP     prun_main

; ---------------------------------------------------------------------------
; prun_main
; ---------------------------------------------------------------------------
        .segment        "CODE"
prun_main:
; print banner
        LDA     #<msg_rt_banner
        STA     tmp0
        LDA     #>msg_rt_banner
        STA     tmp0+1
        JSR     console_print_sz

; build FCB from DEFAULT_FCB (DOS/65 fills $0107 with first argument)
        JSR     build_pcd_fcb   ; copy $0107 → pcd_fcb, append .PCD extension
        BCS     @no_file

; open the .PCD file
        LDA     #<pcd_fcb
        STA     tmp1
        LDA     #>pcd_fcb
        STA     tmp1+1
        JSR     file_open
        CMP     #$FF            ; PEM open returns $FF=not found, 0-3=dir entry
        BEQ     @no_file

; load p-code into memory starting at PCODE_BASE
        JSR     load_pcd
        BCS     @load_err

; execute
        JSR     prun_execute

; clean exit
        LDA     #<pcd_fcb
        STA     tmp1
        LDA     #>pcd_fcb
        STA     tmp1+1
        JSR     file_close
        LDA     #<msg_rt_done
        STA     tmp0
        LDA     #>msg_rt_done
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

@load_err:
        LDA     #<err_rt_magic
        STA     tmp0
        LDA     #>err_rt_magic
        STA     tmp0+1
        JSR     console_print_sz
        JMP     WARM_BOOT

; ---------------------------------------------------------------------------
; build_pcd_fcb — copy DEFAULT_FCB ($0107) to pcd_fcb, force extension "PCD"
; DOS/65 CCM copies the parsed argument FCB to dflfcb=$0107, not $005C.
; Returns: carry clear = ok, carry set = no filename given
; ---------------------------------------------------------------------------
build_pcd_fcb:
        LDX     #0
@copy:
        LDA     DEFAULT_FCB,x
        STA     pcd_fcb,x
        INX
        CPX     #12
        BCC     @copy
; zero the rest of FCB
        LDA     #0
@zero:
        STA     pcd_fcb,x
        INX
        CPX     #36
        BCC     @zero
; check filename is non-blank
        LDA     pcd_fcb+1       ; first char of filename field
        CMP     #' '
        BEQ     @noname
; force extension bytes 9-11 to "PCD"
        LDA     #'P'
        STA     pcd_fcb+9
        LDA     #'C'
        STA     pcd_fcb+10
        LDA     #'D'
        STA     pcd_fcb+11
        CLC
        RTS
@noname:
        SEC
        RTS

; ---------------------------------------------------------------------------
; load_pcd — read .PCD header, validate, load code + globals + strings
; Returns: carry clear = ok, carry set = error
; ---------------------------------------------------------------------------
load_pcd:
; ensure tmp1 points to pcd_fcb for all file I/O calls
        LDA     #<pcd_fcb
        STA     tmp1
        LDA     #>pcd_fcb
        STA     tmp1+1
; Read first sector (contains header + start of code)
        JSR     file_read_sector
        CMP     #0
        BNE     @err

; validate magic
        LDA     DMA_BUF+0
        CMP     #PCD_MAGIC_0
        BNE     @err
        LDA     DMA_BUF+1
        CMP     #PCD_MAGIC_1
        BNE     @err

; entry point — preserved in tmp3 for prun_execute
        LDA     DMA_BUF+PCD_ENTRY
        STA     tmp3
        LDA     DMA_BUF+PCD_ENTRY+1
        STA     tmp3+1

; remaining bytes still to copy = code size
        LDA     DMA_BUF+PCD_CODESZ
        STA     tmp2
        LDA     DMA_BUF+PCD_CODESZ+1
        STA     tmp2+1

; dest = PCODE_BASE - PCD_HEADER_SZ so that for the first sector
; (tmp0),Y with Y=12..127 lands at PCODE_BASE+0..115.
        SEC
        LDA     #<PCODE_BASE
        SBC     #PCD_HEADER_SZ
        STA     tmp0
        LDA     #>PCODE_BASE
        SBC     #0
        STA     tmp0+1

        LDY     #PCD_HEADER_SZ
@cloop:
        LDA     tmp2
        ORA     tmp2+1
        BEQ     @done_ok
        CPY     #128
        BCC     @cbyte
; sector exhausted — advance dest by 128 and read the next one
        CLC
        LDA     tmp0
        ADC     #128
        STA     tmp0
        BCC     :+
        INC     tmp0+1
:
        JSR     file_read_sector
        CMP     #0
        BNE     @err
        LDY     #0
@cbyte:
        LDA     DMA_BUF,y
        STA     (tmp0),y
        INY
        LDA     tmp2
        BNE     :+
        DEC     tmp2+1
:
        DEC     tmp2
        JMP     @cloop

@done_ok:
        CLC
        RTS
@err:
        SEC
        RTS

; ---------------------------------------------------------------------------
; prun_execute — main fetch-decode-execute loop
; ---------------------------------------------------------------------------
prun_execute:
; initialise p-machine registers
        LDA     #<PCODE_BASE
        CLC
        ADC     tmp3            ; entry offset lo
        STA     pm_ipc
        LDA     #>PCODE_BASE
        ADC     tmp3+1
        STA     pm_ipc+1

        LDA     #<STACK_BASE
        STA     pm_sp
        LDA     #>STACK_BASE
        STA     pm_sp+1

        LDA     #<STACK_BASE    ; first frame at bottom of stack
        STA     pm_mp
        LDA     #>STACK_BASE
        STA     pm_mp+1

        LDA     #<GLOBALS_BASE
        STA     pm_base
        LDA     #>GLOBALS_BASE
        STA     pm_base+1

        LDA     #<HEAP_TOP
        STA     pm_np
        LDA     #>HEAP_TOP
        STA     pm_np+1

        LDA     #0                      ; reset string work-buffer cursor
        STA     str_work_idx

; ---------------------------------------------------------------------------
; Fetch-decode-execute inner loop
; ---------------------------------------------------------------------------
prun_loop:
; fetch opcode at pm_ipc
        LDY     #0
        LDA     (pm_ipc),y
; advance IPC
        INC     pm_ipc
        BNE     :+
        INC     pm_ipc+1
:
; dispatch via jump table
        TAX                     ; X = opcode
        LDA     dispatch_lo,x
        STA     tmp0
        LDA     dispatch_hi,x
        STA     tmp0+1
        JMP     (tmp0)

; ---------------------------------------------------------------------------
; Stack helpers
; push 16-bit value in A(lo) / scratch(hi) onto p-machine stack
; ---------------------------------------------------------------------------
pm_push:
        LDY     #0
        STA     (pm_sp),y
        INY
        LDA     scratch
        STA     (pm_sp),y
        CLC
        LDA     pm_sp
        ADC     #2
        STA     pm_sp
        BCC     :+
        INC     pm_sp+1
:
        RTS

; pop 16-bit value from p-machine stack → A(lo) / scratch(hi)
pm_pop:
        SEC
        LDA     pm_sp
        SBC     #2
        STA     pm_sp
        BCS     :+
        DEC     pm_sp+1
:
        LDY     #0
        LDA     (pm_sp),y
        PHA
        INY
        LDA     (pm_sp),y
        STA     scratch
        PLA
        RTS

; peek at TOS without popping → A(lo) / scratch(hi)
pm_peek:
        LDA     pm_sp
        SEC
        SBC     #2
        STA     tmp1
        LDA     pm_sp+1
        SBC     #0
        STA     tmp1+1
        LDY     #0
        LDA     (tmp1),y
        PHA
        INY
        LDA     (tmp1),y
        STA     scratch
        PLA
        RTS

; fetch inline byte at IPC (advances IPC), returns in A
pm_fetch_byte:
        LDY     #0
        LDA     (pm_ipc),y
        INC     pm_ipc
        BNE     :+
        INC     pm_ipc+1
:
        RTS

; fetch inline word (lo then hi) at IPC, returns lo in A, hi in scratch
pm_fetch_word:
        JSR     pm_fetch_byte
        PHA
        JSR     pm_fetch_byte
        STA     scratch
        PLA
        RTS

; ---------------------------------------------------------------------------
; Runtime error handler
; ---------------------------------------------------------------------------
rt_error:
; tmp0 already points to error string (caller's responsibility)
        JSR     console_print_sz
        JMP     WARM_BOOT

; ---------------------------------------------------------------------------
; Opcode handlers
; ---------------------------------------------------------------------------

; OP_HALT ($FF) — stop execution
op_HALT:
        RTS                     ; return to prun_main

; OP_LDCI ($00) — push sign-extended byte constant
op_LDCI:
        JSR     pm_fetch_byte
        PHA
; sign-extend: if bit 7 set, high byte = $FF else $00
        AND     #$80
        BEQ     :+
        LDA     #$FF
        BNE     :++
:
        LDA     #$00
:
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_LDCW ($01) — push word constant
op_LDCW:
        JSR     pm_fetch_word   ; A=lo, scratch=hi
        JSR     pm_push
        JMP     prun_loop

; OP_LDCC ($02) — push char constant (zero-extended)
op_LDCC:
        JSR     pm_fetch_byte
        PHA
        LDA     #0
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_LDCB ($03) — push boolean constant
op_LDCB:
        JSR     pm_fetch_byte
        CMP     #0              ; pm_fetch_byte's Z reflects pm_ipc INC, not A
        BEQ     :+
        LDA     #$FF            ; TRUE = $FFFF
        STA     scratch
        LDA     #$FF
        JSR     pm_push
        JMP     prun_loop
:
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_LDCN ($05) — push NIL ($0000)
op_LDCN:
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_LDCS ($04) — inline string constant.  IPC currently points at the
; length byte (after opcode fetch).  Push that address as a Pascal-style
; string pointer, then advance IPC past length+chars.
op_LDCS:
; save current IPC as the string pointer
        LDA     pm_ipc
        STA     tmp1
        LDA     pm_ipc+1
        STA     tmp1+1
; advance IPC by (length + 1)
        LDY     #0
        LDA     (pm_ipc),y      ; length
        SEC                     ; +1 for the length byte
        ADC     pm_ipc
        STA     pm_ipc
        BCC     :+
        INC     pm_ipc+1
:       ; push the saved string pointer
        LDA     tmp1+1
        STA     scratch
        LDA     tmp1
        JSR     pm_push
        JMP     prun_loop

; OP_LDL ($10) — push 16-bit local var
op_LDL:
        JSR     pm_fetch_byte   ; byte offset
        CLC
        ADC     #AR_LOCALS      ; offset from MP
        PHA
        LDA     pm_mp
        CLC
        PLA
        ADC     pm_mp
        STA     tmp1
        LDA     pm_mp+1
        ADC     #0
        STA     tmp1+1
        LDY     #0
        LDA     (tmp1),y
        PHA
        INY
        LDA     (tmp1),y
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_STL ($11) — pop into 16-bit local var
op_STL:
        JSR     pm_fetch_byte   ; byte offset
        CLC
        ADC     #AR_LOCALS
        CLC
        ADC     pm_mp
        STA     tmp1
        LDA     pm_mp+1
        ADC     #0
        STA     tmp1+1
        JSR     pm_pop          ; A=lo, scratch=hi
        LDY     #0
        STA     (tmp1),y
        INY
        LDA     scratch
        STA     (tmp1),y
        JMP     prun_loop

; OP_LDG ($18) — push 16-bit global var (word offset from pm_base)
op_LDG:
        JSR     pm_fetch_word   ; A=lo offset, scratch=hi offset
        CLC
        ADC     pm_base
        STA     tmp1
        LDA     scratch
        ADC     pm_base+1
        STA     tmp1+1
        LDY     #0
        LDA     (tmp1),y
        PHA
        INY
        LDA     (tmp1),y
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_STG ($19) — pop into 16-bit global var
op_STG:
        JSR     pm_fetch_word   ; A=lo offset, scratch=hi offset
        CLC
        ADC     pm_base
        STA     tmp1
        LDA     scratch
        ADC     pm_base+1
        STA     tmp1+1
        JSR     pm_pop          ; A=lo, scratch=hi
        LDY     #0
        STA     (tmp1),y
        INY
        LDA     scratch
        STA     (tmp1),y
        JMP     prun_loop

; OP_LDA_L ($12) — push effective address of local (pm_mp + AR_LOCALS + offset)
op_LDA_L:
        JSR     pm_fetch_byte   ; A = byte offset
        CLC
        ADC     #AR_LOCALS
        CLC
        ADC     pm_mp           ; A = lo of pm_mp + (offset+AR_LOCALS)
        PHA
        LDA     pm_mp+1
        ADC     #0              ; +carry
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_LDA_G ($1A) — push effective address of global (pm_base + offset)
op_LDA_G:
        JSR     pm_fetch_word   ; A=lo offset, scratch=hi offset
        CLC
        ADC     pm_base
        PHA
        LDA     scratch
        ADC     pm_base+1
        STA     scratch         ; hi of address
        PLA                     ; lo of address
        JSR     pm_push
        JMP     prun_loop

; OP_LDIND ($20) — TOS=addr → push word at addr
op_LDIND:
        JSR     pm_pop          ; A=lo addr, scratch=hi addr
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDY     #0
        LDA     (tmp1),y
        PHA
        INY
        LDA     (tmp1),y
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_STIND ($21) — NOS=addr, TOS=val → store word at addr
op_STIND:
        JSR     pm_pop          ; pop val: A=lo, scratch=hi
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop          ; pop addr: A=lo, scratch=hi
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDY     #0
        LDA     tmp2
        STA     (tmp1),y
        INY
        LDA     tmp2+1
        STA     (tmp1),y
        JMP     prun_loop

; OP_INDEX ($24) — array element address
; Reads 2 operand bytes: element size (word, lo then hi)
; Stack: NOS=base_addr, TOS=index → pushes base_addr + index*elemsize
op_INDEX:
        JSR     pm_fetch_byte   ; elemsize lo
        STA     tmp2
        JSR     pm_fetch_byte   ; elemsize hi (ignored — always 0 in practice)
        JSR     pm_pop          ; TOS = index: A=lo, scratch=hi
        STA     tmp3
        LDA     scratch
        STA     tmp3+1
        JSR     pm_pop          ; NOS = base addr: A=lo, scratch=hi
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
; offset = index * elemsize  (16-bit × 8-bit shift-and-add, low 16 bits)
; accumulate in tmp0:tmp0+1
        LDA     #0
        STA     tmp0
        STA     tmp0+1
        LDA     tmp2            ; 8-bit elemsize (multiplier)
@idx_mul:
        LSR                     ; shift multiplier right; bit 0 → carry
        BCC     @idx_skip       ; bit was 0 — no add this round
        PHA                     ; save shifted multiplier
        CLC
        LDA     tmp0
        ADC     tmp3
        STA     tmp0
        LDA     tmp0+1
        ADC     tmp3+1
        STA     tmp0+1
        PLA                     ; restore shifted multiplier
@idx_skip:
        ASL     tmp3            ; shift multiplicand (index) left for next bit
        ROL     tmp3+1
        BNE     @idx_mul        ; loop while remaining multiplier bits ≠ 0
        CMP     #0              ; also check A=0 (BNE doesn't re-check A after PLA)
        BNE     @idx_mul
; result = base + offset
        LDA     tmp1
        CLC
        ADC     tmp0
        STA     tmp0
        LDA     tmp1+1
        ADC     tmp0+1
        STA     scratch
        LDA     tmp0
        JSR     pm_push
        JMP     prun_loop

; OP_ADI ($30) — integer add
op_ADI:
        JSR     pm_pop          ; b: A=lo, scratch=hi
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop          ; a: A=lo, scratch=hi
        CLC
        ADC     tmp2            ; a.lo + b.lo
        PHA
        LDA     scratch
        ADC     tmp2+1          ; a.hi + b.hi + carry
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_SBI ($31) — integer subtract
op_SBI:
        JSR     pm_pop          ; b: A=lo, scratch=hi
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop          ; a: A=lo, scratch=hi
        SEC
        SBC     tmp2            ; a.lo - b.lo
        PHA
        LDA     scratch
        SBC     tmp2+1          ; a.hi - b.hi - borrow
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_NGI ($35) — negate TOS
op_NGI:
        JSR     pm_pop          ; A=lo, scratch=hi
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        LDA     #0
        SEC
        SBC     tmp2            ; negate lo
        PHA
        LDA     #0
        SBC     tmp2+1          ; negate hi
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_LAND ($38) — pop two, push bitwise AND
op_LAND:
        JSR     pm_pop
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop
        AND     tmp2
        PHA
        LDA     scratch
        AND     tmp2+1
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_LOR ($39) — pop two, push bitwise OR
op_LOR:
        JSR     pm_pop
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop
        ORA     tmp2
        PHA
        LDA     scratch
        ORA     tmp2+1
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_LNOT ($3A) — logical NOT: $0000 → $FFFF, anything else → $0000
op_LNOT:
        JSR     pm_pop
        ORA     scratch
        BEQ     @lnot_true
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop
@lnot_true:
        LDA     #$FF
        STA     scratch
        LDA     #$FF
        JSR     pm_push
        JMP     prun_loop

; OP_BNOT ($3B) — bitwise complement
op_BNOT:
        JSR     pm_pop
        EOR     #$FF
        PHA
        LDA     scratch
        EOR     #$FF
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; ---------------------------------------------------------------------------
; mul/div helpers — operate on tmp2 (a) and tmp1 (b), result in tmp2
; ---------------------------------------------------------------------------

; mul16: tmp2 = tmp2 * tmp1 (low 16 bits, unsigned)
mul16:
        LDA     #0
        STA     scratch         ; result lo
        STA     scratch+1       ; result hi
        LDX     #16
@bit:
        LSR     tmp1+1
        ROR     tmp1
        BCC     @no_add
        CLC
        LDA     scratch
        ADC     tmp2
        STA     scratch
        LDA     scratch+1
        ADC     tmp2+1
        STA     scratch+1
@no_add:
        ASL     tmp2
        ROL     tmp2+1
        DEX
        BNE     @bit
        LDA     scratch
        STA     tmp2
        LDA     scratch+1
        STA     tmp2+1
        RTS

; udiv16: tmp2 = tmp2 / tmp1, scratch = tmp2 mod tmp1 (unsigned)
udiv16:
        LDA     #0
        STA     scratch         ; remainder lo
        STA     scratch+1       ; remainder hi
        LDX     #16
@bit:
        ASL     tmp2            ; shift dividend left, top bit into rem
        ROL     tmp2+1
        ROL     scratch
        ROL     scratch+1
; try subtract divisor from remainder
        LDA     scratch
        SEC
        SBC     tmp1
        TAY
        LDA     scratch+1
        SBC     tmp1+1
        BCC     @no_sub
        STA     scratch+1
        STY     scratch
        INC     tmp2            ; quotient bit
@no_sub:
        DEX
        BNE     @bit
        RTS

; mul_tmp2_by10: tmp2 = tmp2 * 10 (16-bit, unsigned low word)
mul_tmp2_by10:
        ASL     tmp2
        ROL     tmp2+1
        LDA     tmp2
        STA     tmp0
        LDA     tmp2+1
        STA     tmp0+1
        ASL     tmp2
        ROL     tmp2+1
        ASL     tmp2
        ROL     tmp2+1
        CLC
        LDA     tmp2
        ADC     tmp0
        STA     tmp2
        LDA     tmp2+1
        ADC     tmp0+1
        STA     tmp2+1
        RTS

; accum_digit_tmp2: tmp2 = tmp2*10 + A (digit 0..9)
accum_digit_tmp2:
        PHA
        JSR     mul_tmp2_by10
        PLA
        CLC
        ADC     tmp2
        STA     tmp2
        LDA     #0
        ADC     tmp2+1
        STA     tmp2+1
        RTS

; mul16_to32: unsigned tmp2 * tmp1 -> real32_res[0..3]
mul16_to32:
        LDA     #0
        STA     real32_res0
        STA     real32_res1
        STA     real32_res2
        STA     real32_res3
        LDA     tmp2
        STA     real32_mcand0
        LDA     tmp2+1
        STA     real32_mcand1
        LDA     #0
        STA     real32_mcand2
        STA     real32_mcand3
        LDX     #16
@m32_bit:
        LSR     tmp1+1
        ROR     tmp1
        BCC     @m32_no_add
        CLC
        LDA     real32_res0
        ADC     real32_mcand0
        STA     real32_res0
        LDA     real32_res1
        ADC     real32_mcand1
        STA     real32_res1
        LDA     real32_res2
        ADC     real32_mcand2
        STA     real32_res2
        LDA     real32_res3
        ADC     real32_mcand3
        STA     real32_res3
@m32_no_add:
        ASL     real32_mcand0
        ROL     real32_mcand1
        ROL     real32_mcand2
        ROL     real32_mcand3
        DEX
        BNE     @m32_bit
        RTS

; udiv32by16: unsigned real32_res[0..3] / tmp1 -> quotient in real32_res,
; remainder in real32_rem0..1.
udiv32by16:
        LDA     #0
        STA     real32_rem0
        STA     real32_rem1
        LDX     #32
@d32_bit:
        ASL     real32_res0
        ROL     real32_res1
        ROL     real32_res2
        ROL     real32_res3
        ROL     real32_rem0
        ROL     real32_rem1
        LDA     real32_rem0
        SEC
        SBC     tmp1
        TAY
        LDA     real32_rem1
        SBC     tmp1+1
        BCC     @d32_no_sub
        STA     real32_rem1
        STY     real32_rem0
        INC     real32_res0
        BNE     @d32_no_sub
        INC     real32_res1
        BNE     @d32_no_sub
        INC     real32_res2
        BNE     @d32_no_sub
        INC     real32_res3
@d32_no_sub:
        DEX
        BNE     @d32_bit
        RTS

; real_split_abs_tmp0: tmp0 = signed fixed-point REAL (scale 100)
; -> tmp0 = absolute integer part, real_frac = fractional part 0..99,
;    real_sign = 0/1.
real_split_abs_tmp0:
        LDA     #0
        STA     real_sign
        LDA     tmp0+1
        BPL     @rsat_abs
        LDA     #1
        STA     real_sign
        SEC
        LDA     #0
        SBC     tmp0
        STA     tmp0
        LDA     #0
        SBC     tmp0+1
        STA     tmp0+1
@rsat_abs:
        LDA     tmp0
        STA     tmp2
        LDA     tmp0+1
        STA     tmp2+1
        LDA     #100
        STA     tmp1
        LDA     #0
        STA     tmp1+1
        JSR     udiv16
        LDA     tmp2
        STA     tmp0
        LDA     tmp2+1
        STA     tmp0+1
        LDA     scratch
        STA     real_frac
        RTS

; file_write_uint_helper: tmp0 = non-negative integer, tmp1 = file ptr
file_write_uint_helper:
        LDA     tmp0
        STA     fwi_val
        LDA     tmp0+1
        STA     fwi_val+1
        LDX     #0
@fwuh_div_loop:
        LDA     fwi_val
        STA     tmp0
        LDA     fwi_val+1
        STA     tmp0+1
        PHX
        JSR     div16_by10
        PLX
        LDA     scratch
        CLC
        ADC     #'0'
        STA     fwi_buf,x
        INX
        LDA     tmp0
        STA     fwi_val
        LDA     tmp0+1
        STA     fwi_val+1
        ORA     fwi_val
        BNE     @fwuh_div_loop
@fwuh_emit:
        DEX
        LDA     fwi_buf,x
        PHX
        JSR     file_write_char_helper
        PLX
        CPX     #0
        BNE     @fwuh_emit
        RTS

real_print_console_helper:
        JSR     real_split_abs_tmp0
        LDA     real_sign
        BEQ     :+
        LDA     #'-'
        JSR     console_putc
:       JSR     console_print_dec
        LDA     #'.'
        JSR     console_putc
        LDA     real_frac
        STA     tmp0
        LDA     #0
        STA     tmp0+1
        JSR     div16_by10
        LDA     tmp0
        CLC
        ADC     #'0'
        JSR     console_putc
        LDA     scratch
        CLC
        ADC     #'0'
        JSR     console_putc
        RTS

; sign16: returns A=0 if tmp2>=0, A=$FF if negative
sign16:
        LDA     tmp2+1
        AND     #$80
        BEQ     @pos
        LDA     #$FF
        RTS
@pos:
        LDA     #0
        RTS

; neg_tmp2: tmp2 = -tmp2 (16-bit two's complement)
neg_tmp2:
        SEC
        LDA     #0
        SBC     tmp2
        STA     tmp2
        LDA     #0
        SBC     tmp2+1
        STA     tmp2+1
        RTS

; OP_MPI ($32) — integer multiply (low 16 bits, sign doesn't matter)
op_MPI:
        JSR     pm_pop          ; b → tmp1
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        JSR     pm_pop          ; a → tmp2
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     mul16           ; tmp2 = a*b
        LDA     tmp2
        LDX     tmp2+1
        STX     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_DVI ($33) — signed integer divide (truncate toward zero)
op_DVI:
        JSR     pm_pop          ; b → tmp1
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        JSR     pm_pop          ; a → tmp2
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
; compute result sign = sign(a) XOR sign(b)
        LDA     tmp2+1
        EOR     tmp1+1
        AND     #$80
        PHA                     ; save result sign on 6502 stack
; abs(a)
        BIT     tmp2+1
        BPL     :+
        JSR     neg_tmp2
:       ; abs(b) — temporarily move tmp1 to tmp2 area? simpler: inline negate of tmp1
        BIT     tmp1+1
        BPL     :+
        SEC
        LDA     #0
        SBC     tmp1
        STA     tmp1
        LDA     #0
        SBC     tmp1+1
        STA     tmp1+1
:
        JSR     udiv16          ; tmp2 = quotient
        PLA                     ; result sign
        BEQ     :+
        JSR     neg_tmp2
:
        LDA     tmp2
        LDX     tmp2+1
        STX     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_MOD ($34) — signed modulo: a MOD b = a - (a DIV b)*b
op_MOD:
        JSR     pm_pop          ; b → tmp1
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        JSR     pm_pop          ; a → tmp2
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
; result sign follows dividend (a) for Pascal MOD
        LDA     tmp2+1
        AND     #$80
        PHA
        BIT     tmp2+1
        BPL     :+
        JSR     neg_tmp2
:
        BIT     tmp1+1
        BPL     :+
        SEC
        LDA     #0
        SBC     tmp1
        STA     tmp1
        LDA     #0
        SBC     tmp1+1
        STA     tmp1+1
:
        JSR     udiv16          ; remainder in scratch
        LDA     scratch
        STA     tmp2
        LDA     scratch+1
        STA     tmp2+1
        PLA
        BEQ     :+
        JSR     neg_tmp2
:
        LDA     tmp2
        LDX     tmp2+1
        STX     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_MPR ($A5) — signed fixed-point REAL multiply (scale 100)
op_MPR:
        JSR     pm_pop          ; b → tmp1
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        JSR     pm_pop          ; a → tmp2
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        LDA     tmp2+1
        EOR     tmp1+1
        AND     #$80
        BEQ     :+
        LDA     #1
        STA     real_sign
        BRA     @mpr_abs
:       LDA     #0
        STA     real_sign
@mpr_abs:
        BIT     tmp2+1
        BPL     :+
        JSR     neg_tmp2
:       BIT     tmp1+1
        BPL     :+
        SEC
        LDA     #0
        SBC     tmp1
        STA     tmp1
        LDA     #0
        SBC     tmp1+1
        STA     tmp1+1
:       JSR     mul16_to32
        LDA     #100
        STA     tmp1
        LDA     #0
        STA     tmp1+1
        JSR     udiv32by16
        LDA     real32_res0
        STA     tmp2
        LDA     real32_res1
        STA     tmp2+1
        LDA     real_sign
        BEQ     :+
        JSR     neg_tmp2
:       LDA     tmp2
        LDX     tmp2+1
        STX     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_DVR ($A6) — signed fixed-point REAL divide (scale 100)
op_DVR:
        JSR     pm_pop          ; b → tmp1
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        JSR     pm_pop          ; a → tmp2
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        LDA     tmp2+1
        EOR     tmp1+1
        AND     #$80
        BEQ     :+
        LDA     #1
        STA     real_sign
        BRA     @dvr_abs
:       LDA     #0
        STA     real_sign
@dvr_abs:
        BIT     tmp2+1
        BPL     :+
        JSR     neg_tmp2
:       BIT     tmp1+1
        BPL     :+
        SEC
        LDA     #0
        SBC     tmp1
        STA     tmp1
        LDA     #0
        SBC     tmp1+1
        STA     tmp1+1
:       LDA     tmp1
        STA     tmp3
        LDA     tmp1+1
        STA     tmp3+1
        LDA     tmp3
        ORA     tmp3+1
        BNE     :+
        LDA     #<err_rt_div0
        STA     tmp0
        LDA     #>err_rt_div0
        STA     tmp0+1
        JMP     rt_error
:       LDA     #100
        STA     tmp1
        LDA     #0
        STA     tmp1+1
        JSR     mul16_to32
        LDA     tmp3
        STA     tmp1
        LDA     tmp3+1
        STA     tmp1+1
        JSR     udiv32by16
        LDA     real32_res0
        STA     tmp2
        LDA     real32_res1
        STA     tmp2+1
        LDA     real_sign
        BEQ     :+
        JSR     neg_tmp2
:       LDA     tmp2
        LDX     tmp2+1
        STX     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_EQUI ($40) — integer equal
op_EQUI:
        JSR     pm_pop          ; b
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop          ; a
        CMP     tmp2
        BNE     @false
        LDA     scratch
        CMP     tmp2+1
        BNE     @false
        LDA     #$FF
        STA     scratch
        JSR     pm_push
        JMP     prun_loop
@false:
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_LESI ($42) — a < b  (signed)
op_LESI:
        JSR     pm_pop          ; b → tmp2
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop          ; a
; signed compare: (a - b), check N and V
        SEC
        SBC     tmp2
        LDA     scratch
        SBC     tmp2+1
; N XOR V indicates a < b
        BVS     @ov
        BMI     @true
        BPL     @false2
@ov:
        BPL     @true
@false2:
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop
@true:
        LDA     #$FF
        STA     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_NEQI ($41) — integer not-equal
op_NEQI:
        JSR     pm_pop
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop
        CMP     tmp2
        BNE     @true
        LDA     scratch
        CMP     tmp2+1
        BNE     @true
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop
@true:
        LDA     #$FF
        STA     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_LEQI ($43) — a <= b  (signed)
op_LEQI:
        JSR     pm_pop          ; b → tmp2
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop          ; a (lo=A, hi=scratch)
        SEC
        SBC     tmp2            ; A = a_lo - b_lo
        TAX                     ; preserve low result for zero-test
        LDA     scratch
        SBC     tmp2+1          ; A = a_hi - b_hi (with borrow)
; if a < b → N XOR V == 1
        BVS     @ov
        BMI     @true
        BNE     @false3         ; positive non-zero hi → a > b
        CPX     #0
        BEQ     @true           ; a == b
        JMP     @false3
@ov:
        BPL     @true           ; a < b under overflow
@false3:
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop
@true:
        LDA     #$FF
        STA     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_GTRI ($44) — a > b  (signed) ; equivalent to b < a
op_GTRI:
        JSR     pm_pop          ; b
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop          ; a
; compute b - a  (i.e. swap operands of LESI)
; flip via: result_true if a > b, i.e. (a - b) > 0
        SEC
        SBC     tmp2            ; lo
        TAX
        LDA     scratch
        SBC     tmp2+1          ; hi with borrow
        BVS     @ov
        BMI     @false4         ; a < b
        BNE     @true           ; positive non-zero hi → a > b
        CPX     #0
        BEQ     @false4         ; a == b
        JMP     @true
@ov:
        BPL     @false4
@true:
        LDA     #$FF
        STA     scratch
        JSR     pm_push
        JMP     prun_loop
@false4:
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_GEQI ($45) — a >= b  (signed) ; equivalent to NOT (a < b)
op_GEQI:
        JSR     pm_pop          ; b
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop          ; a
        SEC
        SBC     tmp2
        LDA     scratch
        SBC     tmp2+1
        BVS     @ov
        BMI     @false5         ; a < b
        JMP     @true
@ov:
        BPL     @false5
@true:
        LDA     #$FF
        STA     scratch
        JSR     pm_push
        JMP     prun_loop
@false5:
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_UJP ($50) — unconditional jump (signed word offset from current IPC)
op_UJP:
        JSR     pm_fetch_word   ; A=lo, scratch=hi (signed offset)
        CLC
        ADC     pm_ipc
        STA     pm_ipc
        LDA     scratch
        ADC     pm_ipc+1
        STA     pm_ipc+1
        JMP     prun_loop

; OP_FJP ($51) — jump if TOS is false (0)
op_FJP:
        JSR     pm_fetch_word
        PHA
        LDA     scratch
        PHA
        JSR     pm_pop          ; condition
        ORA     scratch         ; zero?
        BNE     @no_jump        ; not zero = true, no jump
        PLA                     ; hi offset
        STA     scratch
        PLA                     ; lo offset
        CLC
        ADC     pm_ipc
        STA     pm_ipc
        LDA     scratch
        ADC     pm_ipc+1
        STA     pm_ipc+1
        JMP     prun_loop
@no_jump:
        PLA
        PLA
        JMP     prun_loop

; OP_TJP ($52) — jump if TOS is true (≠0)
op_TJP:
        JSR     pm_fetch_word
        PHA
        LDA     scratch
        PHA
        JSR     pm_pop
        ORA     scratch
        BEQ     @no_jump2
        PLA
        STA     scratch
        PLA
        CLC
        ADC     pm_ipc
        STA     pm_ipc
        LDA     scratch
        ADC     pm_ipc+1
        STA     pm_ipc+1
        JMP     prun_loop
@no_jump2:
        PLA
        PLA
        JMP     prun_loop

; OP_MRKSTK ($64) — reserve activation record on the value stack
; Operand: 1 byte = size of local area in bytes (excluding AR header)
;
; Layout written: at new MP (= old SP):
;   +0  saved MP (dynamic link)
;   +2  return IPC (set by CALL, not here)
;   +4  static link (unused for now)
;   +6  return value slot
;   +8.. locals
; New MP = old SP; new SP = old SP + AR_LOCALS + local_size.
op_MRKSTK:
        JSR     pm_fetch_byte   ; A = local size
        STA     tmp2

; save old MP at (SP)+AR_DYN_LINK
        LDY     #AR_DYN_LINK
        LDA     pm_mp
        STA     (pm_sp),y
        INY
        LDA     pm_mp+1
        STA     (pm_sp),y

; new MP = current SP
        LDA     pm_sp
        STA     pm_mp
        LDA     pm_sp+1
        STA     pm_mp+1

; new SP = SP + AR_LOCALS + local_size
        LDA     pm_sp
        CLC
        ADC     #AR_LOCALS
        STA     pm_sp
        BCC     :+
        INC     pm_sp+1
:
        LDA     pm_sp
        CLC
        ADC     tmp2
        STA     pm_sp
        BCC     :+
        INC     pm_sp+1
:
        JMP     prun_loop

; ---------------------------------------------------------------------------
; OP_MRKA ($67) — mark stack and gather args.
; Operands: pcount (1 byte), lsize_extra (1 byte).
; The caller pushed pcount 16-bit args to the value stack BEFORE MRKA, so
; their bytes occupy [pm_sp - 2*pcount .. pm_sp - 1].  MRKA shifts those
; bytes UP by AR_LOCALS to make room for the AR header, sets up new MP
; pointing at where the args used to start, and reserves AR_LOCALS +
; 2*pcount + lsize_extra bytes of frame.  After MRKA, args occupy local
; slots 0..pcount-1; LDL/STL with offsets 0,2,..,2*(pcount-1) reach them.
; This lets the caller evaluate args under its OWN MP, fixing the bug
; where MRKSTK changed MP before LDL could load caller-frame locals.
; ---------------------------------------------------------------------------
op_MRKA:
        JSR     pm_fetch_byte           ; A = pcount
        STA     tmp2                    ; tmp2 = pcount (≤ 8)
        JSR     pm_fetch_byte           ; A = lsize_extra
        STA     tmp3                    ; tmp3 = lsize_extra

; new_mp = pm_sp - 2*pcount  (start of arg block, becomes new AR base)
        LDA     pm_sp
        SEC
        SBC     tmp2
        SBC     tmp2
        STA     tmp0                    ; tmp0 = new_mp lo
        LDA     pm_sp+1
        SBC     #0
        SBC     #0
        STA     tmp0+1                  ; tmp0+1 = new_mp hi

; Shift 2*pcount bytes UP by AR_LOCALS to make room for the AR header.
;   src = (tmp0)
;   dst = (tmp0)+AR_LOCALS  (alias tmp1)
        LDA     tmp0
        CLC
        ADC     #AR_LOCALS
        STA     tmp1
        LDA     tmp0+1
        ADC     #0
        STA     tmp1+1

        LDA     tmp2
        ASL                             ; A = 2*pcount (bytes to copy)
        BEQ     @mrka_no_copy
        TAY                             ; Y = byte count (1..16)
@mrka_copy:
        DEY
        LDA     (tmp0),y
        STA     (tmp1),y
        TYA
        BNE     @mrka_copy
@mrka_no_copy:

; Initialize AR header: AR_DYN_LINK <- old MP.
        LDY     #AR_DYN_LINK
        LDA     pm_mp
        STA     (tmp0),y
        INY
        LDA     pm_mp+1
        STA     (tmp0),y

; pm_mp = new_mp
        LDA     tmp0
        STA     pm_mp
        LDA     tmp0+1
        STA     pm_mp+1

; pm_sp = new_mp + AR_LOCALS + 2*pcount + lsize_extra
        LDA     pm_mp
        CLC
        ADC     #AR_LOCALS
        STA     pm_sp
        LDA     pm_mp+1
        ADC     #0
        STA     pm_sp+1

        LDA     pm_sp
        CLC
        ADC     tmp2
        STA     pm_sp
        LDA     pm_sp+1
        ADC     #0
        STA     pm_sp+1
        LDA     pm_sp
        CLC
        ADC     tmp2
        STA     pm_sp
        LDA     pm_sp+1
        ADC     #0
        STA     pm_sp+1

        LDA     pm_sp
        CLC
        ADC     tmp3
        STA     pm_sp
        LDA     pm_sp+1
        ADC     #0
        STA     pm_sp+1

        JMP     prun_loop

; OP_CALL ($60) — call procedure: signed 16-bit offset relative to IPC after operand
; Saves return IPC into the new frame's AR_RET_ADDR slot, then jumps.
op_CALL:
        JSR     pm_fetch_word   ; A = offset lo, scratch = offset hi
        PHA                     ; save offset lo
; save current IPC (= return address) into MP+AR_RET_ADDR
        LDY     #AR_RET_ADDR
        LDA     pm_ipc
        STA     (pm_mp),y
        INY
        LDA     pm_ipc+1
        STA     (pm_mp),y
; pm_ipc += offset (signed)
        PLA                     ; offset lo
        CLC
        ADC     pm_ipc
        STA     pm_ipc
        LDA     scratch
        ADC     pm_ipc+1
        STA     pm_ipc+1
        JMP     prun_loop

; OP_RET ($62) — return from procedure
;   pm_sp ← pm_mp           (collapse frame; discards locals + any junk above)
;   pm_ipc ← MP+AR_RET_ADDR
;   pm_mp  ← MP+AR_DYN_LINK  (saved old MP)
op_RET:
; collapse stack first (preserves AR header — still readable via pm_mp)
        LDA     pm_mp
        STA     pm_sp
        LDA     pm_mp+1
        STA     pm_sp+1
; restore IPC into tmp1 before clobbering MP
        LDY     #AR_RET_ADDR
        LDA     (pm_mp),y
        STA     tmp1
        INY
        LDA     (pm_mp),y
        STA     tmp1+1
; restore old MP from AR_DYN_LINK
        LDY     #AR_DYN_LINK+1
        LDA     (pm_mp),y
        PHA                     ; old MP hi
        LDY     #AR_DYN_LINK
        LDA     (pm_mp),y
        STA     pm_mp
        PLA
        STA     pm_mp+1
; install IPC
        LDA     tmp1
        STA     pm_ipc
        LDA     tmp1+1
        STA     pm_ipc+1
        JMP     prun_loop

; OP_RETF ($63) — return from function: capture AR_RET_VAL, collapse frame,
; restore IPC/MP, then push the captured value onto caller's stack.
op_RETF:
; capture return value into tmp0 BEFORE collapsing
        LDY     #AR_RET_VAL
        LDA     (pm_mp),y
        STA     tmp0
        INY
        LDA     (pm_mp),y
        STA     tmp0+1
; collapse stack to MP
        LDA     pm_mp
        STA     pm_sp
        LDA     pm_mp+1
        STA     pm_sp+1
; restore IPC into tmp1
        LDY     #AR_RET_ADDR
        LDA     (pm_mp),y
        STA     tmp1
        INY
        LDA     (pm_mp),y
        STA     tmp1+1
; restore old MP from AR_DYN_LINK
        LDY     #AR_DYN_LINK+1
        LDA     (pm_mp),y
        PHA
        LDY     #AR_DYN_LINK
        LDA     (pm_mp),y
        STA     pm_mp
        PLA
        STA     pm_mp+1
; install IPC
        LDA     tmp1
        STA     pm_ipc
        LDA     tmp1+1
        STA     pm_ipc+1
; push captured return value (lo in A, hi in scratch)
        LDA     tmp0+1
        STA     scratch
        LDA     tmp0
        JSR     pm_push
        JMP     prun_loop

; OP_STR ($66) — pop word, store at MP+AR_RET_VAL (function result slot)
op_STR:
        JSR     pm_pop          ; A=lo, scratch=hi
        LDY     #AR_RET_VAL
        STA     (pm_mp),y
        INY
        LDA     scratch
        STA     (pm_mp),y
        JMP     prun_loop

; OP_WRITI ($80) — pop integer, print decimal
op_WRITI:
        JSR     pm_pop          ; A=lo, scratch=hi
        STA     tmp0
        LDA     scratch
        STA     tmp0+1
        JSR     console_print_dec
        JMP     prun_loop

; OP_WRITR ($88) — pop REAL, print fixed-point decimal with 2 digits
op_WRITR:
        JSR     pm_pop
        STA     tmp0
        LDA     scratch
        STA     tmp0+1
        JSR     real_print_console_helper
        JMP     prun_loop

; OP_WRITC ($81) — pop char, print
op_WRITC:
        JSR     pm_pop
        JSR     console_putc
        JMP     prun_loop

; OP_WRITB ($82) — pop boolean, print TRUE/FALSE
op_WRITB:
        JSR     pm_pop
        ORA     scratch
        BEQ     @false3
        LDA     #<str_true
        STA     tmp0
        LDA     #>str_true
        STA     tmp0+1
        JSR     console_print_sz
        JMP     prun_loop
@false3:
        LDA     #<str_false
        STA     tmp0
        LDA     #>str_false
        STA     tmp0+1
        JSR     console_print_sz
        JMP     prun_loop

; OP_WRITS ($83) — TOS = address of length-prefixed string, print it
op_WRITS:
        JSR     pm_pop
        STA     tmp0
        LDA     scratch
        STA     tmp0+1
        JSR     console_print_pstr
        JMP     prun_loop

; OP_WRITLN ($84) — newline
op_WRITLN:
        JSR     console_newline
        JMP     prun_loop

; OP_READI ($85) — TOS = address of INTEGER variable.  Read a line from
; the console, parse a signed decimal integer, store as a 16-bit word at
; the popped address.
op_READI:
        JSR     pm_pop          ; A=lo addr, scratch=hi addr
        STA     tmp1            ; (zero-page) destination pointer
        LDA     scratch
        STA     tmp1+1
; read one line into read_line_buf
        LDA     #<read_line_buf
        STA     tmp0
        LDA     #>read_line_buf
        STA     tmp0+1
        LDX     #31             ; max line length
        JSR     console_read_line; Y = length on return
        STY     read_len
        JSR     console_newline ; PEM CONIN echoes CR but no LF
; parse signed decimal from read_line_buf into tmp2:tmp2+1
        LDA     #0
        STA     tmp2
        STA     tmp2+1
        STA     read_neg
        LDY     #0
@rd_skip:
        CPY     read_len
        BCS     @rd_apply_sign  ; empty / all-space → 0
        LDA     read_line_buf,y
        CMP     #' '
        BNE     @rd_chk_sign
        INY
        JMP     @rd_skip
@rd_chk_sign:
        CMP     #'-'
        BNE     @rd_chk_plus
        LDA     #1
        STA     read_neg
        INY
        JMP     @rd_digits
@rd_chk_plus:
        CMP     #'+'
        BNE     @rd_digits
        INY
@rd_digits:
        CPY     read_len
        BCS     @rd_apply_sign
        LDA     read_line_buf,y
        CMP     #'0'
        BCC     @rd_apply_sign
        CMP     #'9'+1
        BCS     @rd_apply_sign
; tmp2 *= 10  (using temp = tmp2*2; tmp2 = (tmp2*2)*4 + temp)
        ASL     tmp2
        ROL     tmp2+1
        LDA     tmp2
        STA     read_save2
        LDA     tmp2+1
        STA     read_save2+1
        ASL     tmp2
        ROL     tmp2+1
        ASL     tmp2
        ROL     tmp2+1
        CLC
        LDA     tmp2
        ADC     read_save2
        STA     tmp2
        LDA     tmp2+1
        ADC     read_save2+1
        STA     tmp2+1
; tmp2 += digit
        LDA     read_line_buf,y
        SEC
        SBC     #'0'
        CLC
        ADC     tmp2
        STA     tmp2
        LDA     #0
        ADC     tmp2+1
        STA     tmp2+1
        INY
        JMP     @rd_digits
@rd_apply_sign:
        LDA     read_neg
        BEQ     @rd_store
        SEC
        LDA     #0
        SBC     tmp2
        STA     tmp2
        LDA     #0
        SBC     tmp2+1
        STA     tmp2+1
@rd_store:
        LDY     #0
        LDA     tmp2
        STA     (tmp1),y
        INY
        LDA     tmp2+1
        STA     (tmp1),y
        JMP     prun_loop

; OP_READR ($89) — read a console line, parse fixed-point REAL, push it.
op_READR:
        LDA     #<read_line_buf
        STA     tmp0
        LDA     #>read_line_buf
        STA     tmp0+1
        LDX     #31
        JSR     console_read_line
        STY     read_len
        JSR     console_newline
        LDA     #0
        STA     tmp2
        STA     tmp2+1
        STA     read_neg
        STA     read_frac_count
        STA     read_seen_dot
        LDY     #0
@rr_skip:
        CPY     read_len
        BCS     @rr_scale
        LDA     read_line_buf,y
        CMP     #' '
        BNE     @rr_chk_sign
        INY
        BRA     @rr_skip
@rr_chk_sign:
        CMP     #'-'
        BNE     @rr_chk_plus
        LDA     #1
        STA     read_neg
        INY
        BRA     @rr_loop
@rr_chk_plus:
        CMP     #'+'
        BNE     @rr_loop
        INY
@rr_loop:
        CPY     read_len
        BCS     @rr_scale
        LDA     read_line_buf,y
        CMP     #'.'
        BNE     @rr_digit
        LDA     read_seen_dot
        BNE     @rr_scale
        LDA     #1
        STA     read_seen_dot
        INY
        BRA     @rr_loop
@rr_digit:
        CMP     #'0'
        BCC     @rr_scale
        CMP     #'9'+1
        BCS     @rr_scale
        LDA     read_seen_dot
        BEQ     @rr_accum
        LDA     read_frac_count
        CMP     #2
        BCS     @rr_skip_digit
@rr_accum:
        LDA     read_line_buf,y
        SEC
        SBC     #'0'
        JSR     accum_digit_tmp2
        LDA     read_seen_dot
        BEQ     @rr_after_digit
        INC     read_frac_count
@rr_after_digit:
        INY
        BRA     @rr_loop
@rr_skip_digit:
        INY
        BRA     @rr_loop
@rr_scale:
        LDA     read_frac_count
        BNE     @rr_chk_frac1
        JSR     mul_tmp2_by10
        JSR     mul_tmp2_by10
        BRA     @rr_apply
@rr_chk_frac1:
        CMP     #1
        BNE     @rr_apply
        JSR     mul_tmp2_by10
@rr_apply:
        LDA     read_neg
        BEQ     @rr_push
        JSR     neg_tmp2
@rr_push:
        LDA     tmp2
        LDX     tmp2+1
        STX     scratch
        JSR     pm_push
        JMP     prun_loop

read_line_buf:
        .RES    32
read_len:
        .RES    1
read_neg:
        .RES    1
read_frac_count:
        .RES    1
read_seen_dot:
        .RES    1
read_save2:
        .RES    2

; OP_READC ($86) — read char from console, push
op_READC:
        PEM     PEM_CONIN
        PHA
        LDA     #0
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_DUP ($90) — duplicate TOS
op_DUP:
        JSR     pm_peek
        JSR     pm_push
        JMP     prun_loop

; OP_POP ($91) — discard TOS
op_POP:
        JSR     pm_pop
        JMP     prun_loop

; OP_SWAP ($92) — swap TOS and NOS
op_SWAP:
        JSR     pm_pop
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDA     tmp2
        LDX     tmp2+1
        STX     scratch
        JSR     pm_push
        LDA     tmp1
        LDX     tmp1+1
        STX     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_NEW ($70) — allocate inline-size bytes from heap, push pointer
; Bump allocator: pm_np grows down from HEAP_TOP. DISPOSE is a no-op.
op_NEW:
        JSR     pm_fetch_word   ; A=size lo, scratch=size hi
        STA     tmp0
        LDA     scratch
        STA     tmp0+1
        SEC
        LDA     pm_np
        SBC     tmp0
        STA     pm_np
        LDA     pm_np+1
        SBC     tmp0+1
        STA     pm_np+1
        LDA     pm_np+1
        STA     scratch
        LDA     pm_np
        JSR     pm_push
        JMP     prun_loop

; OP_DISP ($71) — discard pointer (bump allocator cannot free)
op_DISP:
        JSR     pm_pop
        JMP     prun_loop

; ---------------------------------------------------------------------------
; String built-in support
;
; Pascal-style strings: length byte at offset 0, characters at offsets 1..N.
; COPY/CONCAT results live in one of three rotating 256-byte work buffers
; placed in unused heap-area memory.  Round-robin allocation lets simple
; nested expressions like CONCAT(COPY(s,1,3), 'x') work; deeper nesting
; eventually recycles a buffer and corrupts an earlier result.
; ---------------------------------------------------------------------------
STR_WORK_BASE   = $AD00
STR_WORK_COUNT  = 3

str_work_idx:
        .RES    1

; next_work_buf — return current work-buffer addr in tmp1, advance idx
next_work_buf:
        LDA     str_work_idx
        CLC
        ADC     #>STR_WORK_BASE
        STA     tmp1+1
        LDA     #0
        STA     tmp1
        INC     str_work_idx
        LDA     str_work_idx
        CMP     #STR_WORK_COUNT
        BCC     :+
        LDA     #0
        STA     str_work_idx
:       RTS

; OP_LEN ($A0) — TOS = string ptr → TOS = length (int)
op_LEN:
        JSR     pm_pop          ; A=lo, scratch=hi
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDY     #0
        LDA     (tmp1),y        ; length byte
        PHA
        LDA     #0
        STA     scratch
        PLA
        JSR     pm_push
        JMP     prun_loop

; OP_POS ($A1) — NOS=substr ptr, TOS=mainstr ptr → TOS=1-based position
op_POS:
        JSR     pm_pop          ; mainstr → tmp2
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop          ; substr → tmp1
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
; If substr length is 0, return 0
        LDY     #0
        LDA     (tmp1),y
        BNE     :+
        JMP     @pos_zero
:
; sublen in pos_sublen, mainlen in pos_mainlen
        STA     pos_sublen
        LDY     #0
        LDA     (tmp2),y
        STA     pos_mainlen
; If sublen > mainlen, return 0
        LDA     pos_sublen
        CMP     pos_mainlen
        BEQ     :+
        BCS     @pos_zero
:
; pos_start = 1; loop while (pos_start + sublen - 1) <= mainlen
        LDA     #1
        STA     pos_start
@pos_outer:
; max_start = mainlen - sublen + 1
        LDA     pos_mainlen
        SEC
        SBC     pos_sublen
        CLC
        ADC     #1              ; A = max valid start
        CMP     pos_start
        BCC     @pos_zero       ; pos_start > max_start → not found
; compare sublen bytes at main[pos_start..] vs sub[1..]
        LDX     #0              ; substr offset (0..sublen-1, +1 to skip len byte)
@pos_cmp:
        CPX     pos_sublen
        BEQ     @pos_found
        TXA
        CLC
        ADC     pos_start       ; main index = pos_start + X
        TAY
        LDA     (tmp2),y        ; main[pos_start+X]
        STA     pos_save
        TXA
        CLC
        ADC     #1              ; sub index = X + 1
        TAY
        LDA     (tmp1),y        ; sub[X+1]
        CMP     pos_save
        BNE     @pos_next
        INX
        BRA     @pos_cmp
@pos_next:
        INC     pos_start
        BRA     @pos_outer
@pos_found:
        LDA     pos_start
        STA     pos_save
        LDA     #0
        STA     scratch
        LDA     pos_save
        JSR     pm_push
        JMP     prun_loop
@pos_zero:
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop

pos_sublen:    .RES 1
pos_mainlen:   .RES 1
pos_start:     .RES 1
pos_save:      .RES 1

; OP_COPY ($A2) — NNOS=str, NOS=index (1-based), TOS=count → TOS=result strptr
op_COPY:
        JSR     pm_pop          ; count
        STA     copy_count
        JSR     pm_pop          ; index
        STA     copy_index
        JSR     pm_pop          ; source ptr
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     next_work_buf   ; tmp1 = dest buffer
; clamp index: if index < 1 or index > srclen, result is empty
        LDY     #0
        LDA     (tmp2),y
        STA     copy_srclen
        LDA     copy_index
        BEQ     @copy_empty
        CMP     copy_srclen
        BEQ     :+
        BCS     @copy_empty
:
; available = srclen - index + 1
        LDA     copy_srclen
        SEC
        SBC     copy_index
        CLC
        ADC     #1
        STA     copy_avail
; actual = min(count, available)
        LDA     copy_count
        CMP     copy_avail
        BCC     :+
        LDA     copy_avail
:       STA     copy_actual
; copy actual bytes from src[index..] into dest[1..]
        LDX     #0
@copy_loop:
        CPX     copy_actual
        BEQ     @copy_done
        TXA
        CLC
        ADC     copy_index      ; src offset = X + index
        TAY
        LDA     (tmp2),y
        STA     copy_save
        TXA
        CLC
        ADC     #1              ; dest offset = X + 1
        TAY
        LDA     copy_save
        STA     (tmp1),y
        INX
        BRA     @copy_loop
@copy_done:
        LDA     copy_actual
        LDY     #0
        STA     (tmp1),y        ; write length byte
        BRA     @copy_push
@copy_empty:
        LDA     #0
        LDY     #0
        STA     (tmp1),y
@copy_push:
        LDA     tmp1+1
        STA     scratch
        LDA     tmp1
        JSR     pm_push
        JMP     prun_loop

copy_count:    .RES 1
copy_index:    .RES 1
copy_srclen:   .RES 1
copy_avail:    .RES 1
copy_actual:   .RES 1
copy_save:     .RES 1

; OP_CONCAT ($A3) — NOS=s1, TOS=s2 → TOS=result strptr (s1 then s2)
op_CONCAT:
        JSR     pm_pop          ; s2 → tmp2
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
; stash s2 ptr in cat_s2 because next_work_buf and pm_pop touch tmp1
        LDA     tmp2
        STA     cat_s2
        LDA     tmp2+1
        STA     cat_s2+1
        JSR     pm_pop          ; s1 → tmp2
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        LDA     tmp2
        STA     cat_s1
        LDA     tmp2+1
        STA     cat_s1+1
        JSR     next_work_buf   ; tmp1 = dest
; copy s1 chars
        LDA     cat_s1
        STA     tmp2
        LDA     cat_s1+1
        STA     tmp2+1
        LDY     #0
        LDA     (tmp2),y
        STA     cat_s1len
        TAX                     ; X = remaining
        LDY     #1              ; src offset
        LDA     #1
        STA     cat_dstoff
@cat_s1_loop:
        CPX     #0
        BEQ     @cat_s2_start
        LDA     (tmp2),y
        STA     cat_save
        PHX
        PHY
        LDY     cat_dstoff
        LDA     cat_save
        STA     (tmp1),y
        INC     cat_dstoff
        PLY
        PLX
        INY
        DEX
        BRA     @cat_s1_loop
@cat_s2_start:
        LDA     cat_s2
        STA     tmp2
        LDA     cat_s2+1
        STA     tmp2+1
        LDY     #0
        LDA     (tmp2),y
        STA     cat_s2len
        TAX
        LDY     #1
@cat_s2_loop:
        CPX     #0
        BEQ     @cat_done
        LDA     (tmp2),y
        STA     cat_save
        PHX
        PHY
        LDY     cat_dstoff
        LDA     cat_save
        STA     (tmp1),y
        INC     cat_dstoff
        PLY
        PLX
        INY
        DEX
        BRA     @cat_s2_loop
@cat_done:
; total length = s1len + s2len (capped at 255)
        CLC
        LDA     cat_s1len
        ADC     cat_s2len
        BCC     :+
        LDA     #255
:       LDY     #0
        STA     (tmp1),y
        LDA     tmp1+1
        STA     scratch
        LDA     tmp1
        JSR     pm_push
        JMP     prun_loop

cat_s1:        .RES 2
cat_s2:        .RES 2
cat_s1len:     .RES 1
cat_s2len:     .RES 1
cat_dstoff:    .RES 1
cat_save:      .RES 1

; OP_INSET ($A4) — NOS=elem, TOS=setmask → TOS=membership bool
op_INSET:
        JSR     pm_pop          ; setmask
        STA     tmp2
        LDA     scratch
        STA     tmp2+1
        JSR     pm_pop          ; elem
        STA     tmp1
        LDA     scratch
        BNE     @inset_false
        LDA     tmp1
        CMP     #16
        BCS     @inset_false
        TAX
        LDA     #1
        STA     tmp1
        LDA     #0
        STA     tmp1+1
@inset_shift:
        CPX     #0
        BEQ     @inset_check
        ASL     tmp1
        ROL     tmp1+1
        DEX
        BRA     @inset_shift
@inset_check:
        LDA     tmp1
        AND     tmp2
        STA     tmp0
        LDA     tmp1+1
        AND     tmp2+1
        ORA     tmp0
        BEQ     @inset_false
        LDA     #$FF
        STA     scratch
        LDA     #$FF
        JSR     pm_push
        JMP     prun_loop
@inset_false:
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop

; ---------------------------------------------------------------------------
; TEXT file I/O handlers
;
; A TEXT variable is a 168-byte struct in globals:
;   F_FCB   (0..35)   FCB used by PEM
;   F_BUF   (36..163) 128-byte sector buffer
;   F_MODE  (164)     0=closed, 1=read, 2=write
;   F_POS   (165)     next byte index in F_BUF (0..128)
;   F_EOF   (166)     non-zero once EOF / CTRL-Z encountered
;
; The file ptr passed on the p-machine stack is the struct base address.
; The PEM DMA pointer is set to (struct + F_BUF) before each sector I/O,
; so multiple open files don't trample each other's buffers.
; ---------------------------------------------------------------------------

; Helper: write one char (in A) to file whose struct ptr is in tmp1.
; Trashes tmp2, X, Y. Preserves tmp1, tmp3, scratch.
file_write_char_helper:
        STA     fwrc_char
        CLC
        LDA     tmp1
        ADC     #F_BUF
        STA     tmp2
        LDA     tmp1+1
        ADC     #0
        STA     tmp2+1
        LDY     #F_POS
        LDA     (tmp1),y
        TAY
        LDA     fwrc_char
        STA     (tmp2),y
        INY
        TYA
        LDY     #F_POS
        STA     (tmp1),y
        CMP     #128
        BNE     @fwch_done
        LDA     tmp2
        LDY     tmp2+1
        JSR     file_setdma
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_WRITE
        JSR     PEM_ENTRY
        LDA     #0
        LDY     #F_POS
        STA     (tmp1),y
@fwch_done:
        RTS

; Helper: read one char from file in tmp1, returns it in A (0 if F_EOF set).
; Then peeks the next char (loading next sector if needed) and sets F_EOF
; when no more data is available — so EOF(F) reflects the correct state
; *before* the next READ.  Trashes tmp2, X, Y.  Preserves tmp1, tmp3.
file_read_char_helper:
        LDY     #F_EOF
        LDA     (tmp1),y
        BEQ     :+
        LDA     #0
        RTS
:
; Read current char at buf[F_POS] (RESET pre-loaded sector 0; this routine
; is responsible for the post-read peek that triggers any future refill).
        CLC
        LDA     tmp1
        ADC     #F_BUF
        STA     tmp2
        LDA     tmp1+1
        ADC     #0
        STA     tmp2+1
        LDY     #F_POS
        LDA     (tmp1),y
        TAY
        LDA     (tmp2),y
        STA     frch_char
        INY
        TYA
        LDY     #F_POS
        STA     (tmp1),y
        JSR     file_check_eof_helper
        LDA     frch_char
        RTS

; Helper: peek buf[F_POS], refilling the sector first if F_POS == 128.
; Sets F_EOF if no more data is available or peeked byte is CTRL-Z.
; Does not advance F_POS.  Trashes tmp2, X, Y.  Preserves tmp1, tmp3.
file_check_eof_helper:
        LDY     #F_POS
        LDA     (tmp1),y
        CMP     #128
        BCC     @fce_peek
; sector exhausted — try to load next one
        CLC
        LDA     tmp1
        ADC     #F_BUF
        PHA
        LDA     tmp1+1
        ADC     #0
        TAY
        PLA
        JSR     file_setdma
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_READ
        JSR     PEM_ENTRY
        CMP     #0
        BEQ     @fce_pos0
        LDA     #1
        LDY     #F_EOF
        STA     (tmp1),y
        RTS
@fce_pos0:
        LDA     #0
        LDY     #F_POS
        STA     (tmp1),y
@fce_peek:
        CLC
        LDA     tmp1
        ADC     #F_BUF
        STA     tmp2
        LDA     tmp1+1
        ADC     #0
        STA     tmp2+1
        LDY     #F_POS
        LDA     (tmp1),y
        TAY
        LDA     (tmp2),y
        CMP     #CTRL_Z
        BNE     @fce_done
        LDA     #1
        LDY     #F_EOF
        STA     (tmp1),y
@fce_done:
        RTS

; OP_FASSGN ($B0) — NOS=fileptr, TOS=strptr → set FCB
op_FASSGN:
        JSR     pm_pop                  ; filename strptr → tmp3
        STA     tmp3
        LDA     scratch
        STA     tmp3+1
        JSR     pm_pop                  ; file ptr → tmp1
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
; clear FCB[0..35]
        LDY     #0
        TYA
@fas_clr:
        STA     (tmp1),y
        INY
        CPY     #36
        BCC     @fas_clr
; pad name+ext (FCB[1..11]) with spaces
        LDA     #' '
        LDY     #1
@fas_pad:
        STA     (tmp1),y
        INY
        CPY     #12
        BCC     @fas_pad
; walk source string
        LDY     #0
        LDA     (tmp3),y                ; length
        STA     fas_remain
        LDA     #1
        STA     fas_si
        LDA     #1
        STA     fas_di
@fas_name_loop:
        LDA     fas_remain
        BEQ     @fas_zero
        LDY     fas_si
        LDA     (tmp3),y
        INC     fas_si
        DEC     fas_remain
        CMP     #'.'
        BEQ     @fas_to_ext
        CMP     #'a'
        BCC     @fas_store_n
        CMP     #'{'
        BCS     @fas_store_n
        AND     #$DF
@fas_store_n:
        LDX     fas_di
        CPX     #9
        BCS     @fas_name_loop
        LDY     fas_di
        STA     (tmp1),y
        INC     fas_di
        BRA     @fas_name_loop
@fas_to_ext:
        LDA     #9
        STA     fas_di
@fas_ext_loop:
        LDA     fas_remain
        BEQ     @fas_zero
        LDY     fas_si
        LDA     (tmp3),y
        INC     fas_si
        DEC     fas_remain
        CMP     #'a'
        BCC     @fas_store_e
        CMP     #'{'
        BCS     @fas_store_e
        AND     #$DF
@fas_store_e:
        LDX     fas_di
        CPX     #12
        BCS     @fas_ext_loop
        LDY     fas_di
        STA     (tmp1),y
        INC     fas_di
        BRA     @fas_ext_loop
@fas_zero:
        LDA     #0
        LDY     #F_MODE
        STA     (tmp1),y
        LDY     #F_POS
        STA     (tmp1),y
        LDY     #F_EOF
        STA     (tmp1),y
        JMP     prun_loop

; OP_FRESET ($B1) — TOS=fileptr → open existing for reading, prefill buffer
op_FRESET:
        JSR     pm_pop
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDA     #0
        LDY     #32
        STA     (tmp1),y                ; restart sequential I/O at record 0
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_OPEN
        JSR     PEM_ENTRY
        CMP     #$FF
        BNE     @fre_open_ok
; open failed — leave file in EOF/closed state
        LDA     #F_MODE_CLOSED
        LDY     #F_MODE
        STA     (tmp1),y
        LDA     #1
        LDY     #F_EOF
        STA     (tmp1),y
        LDA     #128
        LDY     #F_POS
        STA     (tmp1),y
        JMP     prun_loop
@fre_open_ok:
; set DMA to file's internal buffer
        CLC
        LDA     tmp1
        ADC     #F_BUF
        PHA
        LDA     tmp1+1
        ADC     #0
        TAY
        PLA
        JSR     file_setdma
; pre-read first sector to detect immediate EOF
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_READ
        JSR     PEM_ENTRY
        CMP     #0
        BEQ     @fre_have
        LDA     #1
        LDY     #F_EOF
        STA     (tmp1),y
        LDA     #128
        LDY     #F_POS
        STA     (tmp1),y
        BRA     @fre_set_mode
@fre_have:
        LDA     #0
        LDY     #F_EOF
        STA     (tmp1),y
        LDY     #F_POS
        STA     (tmp1),y
; lookahead: set F_EOF immediately if buf[0] is CTRL-Z
        JSR     file_check_eof_helper
@fre_set_mode:
        LDA     #F_MODE_READ
        LDY     #F_MODE
        STA     (tmp1),y
        JMP     prun_loop

; OP_FREWRT ($B2) — TOS=fileptr → create/truncate for writing
op_FREWRT:
        JSR     pm_pop
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDA     #0
        LDY     #32
        STA     (tmp1),y                ; fresh file starts at record 0
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_MAKE
        JSR     PEM_ENTRY
        CLC
        LDA     tmp1
        ADC     #F_BUF
        PHA
        LDA     tmp1+1
        ADC     #0
        TAY
        PLA
        JSR     file_setdma
        LDA     #F_MODE_WRITE
        LDY     #F_MODE
        STA     (tmp1),y
        LDA     #0
        LDY     #F_POS
        STA     (tmp1),y
        LDY     #F_EOF
        STA     (tmp1),y
        JMP     prun_loop

; OP_FCLOSE ($B3) — TOS=fileptr → flush (write mode) and close
op_FCLOSE:
        JSR     pm_pop
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDY     #F_MODE
        LDA     (tmp1),y
        CMP     #F_MODE_WRITE
        BNE     @fclo_just_close
        LDY     #F_POS
        LDA     (tmp1),y
        BEQ     @fclo_just_close        ; nothing pending in buffer
; pad buf[F_POS..127] with CTRL-Z, then write final sector
        CLC
        LDA     tmp1
        ADC     #F_BUF
        STA     tmp2
        LDA     tmp1+1
        ADC     #0
        STA     tmp2+1
        LDY     #F_POS
        LDA     (tmp1),y
        TAY
@fclo_pad:
        CPY     #128
        BCS     @fclo_write
        LDA     #CTRL_Z
        STA     (tmp2),y
        INY
        BRA     @fclo_pad
@fclo_write:
        LDA     tmp2
        LDY     tmp2+1
        JSR     file_setdma
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_WRITE
        JSR     PEM_ENTRY
@fclo_just_close:
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_CLOSE
        JSR     PEM_ENTRY
        LDA     #F_MODE_CLOSED
        LDY     #F_MODE
        STA     (tmp1),y
        JSR     file_setdma_default
        JMP     prun_loop

; OP_FWRC ($B4) — NOS=fileptr, TOS=char → append char
op_FWRC:
        JSR     pm_pop                  ; char val (lo byte)
        PHA
        JSR     pm_pop                  ; file ptr
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        PLA
        JSR     file_write_char_helper
        JMP     prun_loop

; OP_FWRS ($B5) — NOS=fileptr, TOS=strptr → append string
op_FWRS:
        JSR     pm_pop                  ; string ptr → tmp3
        STA     tmp3
        LDA     scratch
        STA     tmp3+1
        JSR     pm_pop                  ; file ptr → tmp1
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDY     #0
        LDA     (tmp3),y                ; length
        STA     fws_remain
        LDA     #1
        STA     fws_idx
@fws_loop:
        LDA     fws_remain
        BEQ     @fws_done
        LDY     fws_idx
        LDA     (tmp3),y
        PHA
        INC     fws_idx
        DEC     fws_remain
        PLA
        JSR     file_write_char_helper
        BRA     @fws_loop
@fws_done:
        JMP     prun_loop

; OP_FWRI ($B6) — NOS=fileptr, TOS=int → append decimal
op_FWRI:
        JSR     pm_pop                  ; int
        STA     fwi_val
        LDA     scratch
        STA     fwi_val+1
        JSR     pm_pop                  ; file ptr
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDA     fwi_val+1
        BPL     @fwi_pos
        LDA     #'-'
        JSR     file_write_char_helper
        SEC
        LDA     #0
        SBC     fwi_val
        STA     fwi_val
        LDA     #0
        SBC     fwi_val+1
        STA     fwi_val+1
@fwi_pos:
        LDA     fwi_val
        STA     tmp0
        LDA     fwi_val+1
        STA     tmp0+1
        JSR     file_write_uint_helper
        JMP     prun_loop

; OP_FWRR ($C0) — NOS=fileptr, TOS=REAL → append fixed-point decimal
op_FWRR:
        JSR     pm_pop
        STA     tmp0
        LDA     scratch
        STA     tmp0+1
        JSR     pm_pop
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDA     tmp1
        STA     real_file_ptr
        LDA     tmp1+1
        STA     real_file_ptr+1
        JSR     real_split_abs_tmp0
        LDA     real_file_ptr
        STA     tmp1
        LDA     real_file_ptr+1
        STA     tmp1+1
        LDA     real_sign
        BEQ     :+
        LDA     #'-'
        JSR     file_write_char_helper
:       JSR     file_write_uint_helper
        LDA     #'.'
        JSR     file_write_char_helper
        LDA     real_frac
        STA     tmp0
        LDA     #0
        STA     tmp0+1
        JSR     div16_by10
        LDA     tmp0
        CLC
        ADC     #'0'
        JSR     file_write_char_helper
        LDA     scratch
        CLC
        ADC     #'0'
        JSR     file_write_char_helper
        JMP     prun_loop

; OP_FWLN ($B7) — TOS=fileptr → CR + LF
op_FWLN:
        JSR     pm_pop
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDA     #$0D
        JSR     file_write_char_helper
        LDA     #$0A
        JSR     file_write_char_helper
        JMP     prun_loop

; OP_FRDC ($B8) — NOS=fileptr, TOS=charvar addr → read 1 char
op_FRDC:
        JSR     pm_pop                  ; charvar addr → tmp3
        STA     tmp3
        LDA     scratch
        STA     tmp3+1
        JSR     pm_pop                  ; file ptr → tmp1
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        JSR     file_read_char_helper
        LDY     #0
        STA     (tmp3),y
        JMP     prun_loop

; OP_FRDI ($B9) — NOS=fileptr, TOS=intvar addr → read decimal
op_FRDI:
        JSR     pm_pop                  ; intvar addr → tmp3
        STA     tmp3
        LDA     scratch
        STA     tmp3+1
        JSR     pm_pop                  ; file ptr → tmp1
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDA     #0
        STA     fri_val
        STA     fri_val+1
        STA     fri_neg
@fri_skipws:
        JSR     file_read_char_helper
        STA     fri_cur
        LDY     #F_EOF
        LDA     (tmp1),y
        BEQ     @fri_chk_ws
        JMP     @fri_apply
@fri_chk_ws:
        LDA     fri_cur
        CMP     #' '
        BEQ     @fri_skipws
        CMP     #$09
        BEQ     @fri_skipws
        CMP     #$0D
        BEQ     @fri_skipws
        CMP     #$0A
        BEQ     @fri_skipws
        CMP     #'-'
        BNE     @fri_chk_plus
        LDA     #1
        STA     fri_neg
        JSR     file_read_char_helper
        STA     fri_cur
        BRA     @fri_dig_check
@fri_chk_plus:
        CMP     #'+'
        BNE     @fri_dig_check
        JSR     file_read_char_helper
        STA     fri_cur
@fri_dig_check:
        LDA     fri_cur
        CMP     #'0'
        BCC     @fri_apply
        CMP     #'9'+1
        BCS     @fri_apply
@fri_dig_loop:
; fri_val = fri_val*10 + (digit-'0')
        ASL     fri_val
        ROL     fri_val+1
        LDA     fri_val
        STA     tmp0
        LDA     fri_val+1
        STA     tmp0+1
        ASL     fri_val
        ROL     fri_val+1
        ASL     fri_val
        ROL     fri_val+1
        CLC
        LDA     fri_val
        ADC     tmp0
        STA     fri_val
        LDA     fri_val+1
        ADC     tmp0+1
        STA     fri_val+1
        LDA     fri_cur
        SEC
        SBC     #'0'
        CLC
        ADC     fri_val
        STA     fri_val
        LDA     #0
        ADC     fri_val+1
        STA     fri_val+1
        JSR     file_read_char_helper
        STA     fri_cur
        LDY     #F_EOF
        LDA     (tmp1),y
        BNE     @fri_apply
        LDA     fri_cur
        CMP     #'0'
        BCC     @fri_apply
        CMP     #'9'+1
        BCS     @fri_apply
        BRA     @fri_dig_loop
@fri_apply:
        LDA     fri_neg
        BEQ     @fri_store
        SEC
        LDA     #0
        SBC     fri_val
        STA     fri_val
        LDA     #0
        SBC     fri_val+1
        STA     fri_val+1
@fri_store:
        LDY     #0
        LDA     fri_val
        STA     (tmp3),y
        INY
        LDA     fri_val+1
        STA     (tmp3),y
        JMP     prun_loop

; OP_FRDR ($C1) — NOS=fileptr, TOS=realvar addr → read fixed-point decimal
op_FRDR:
        JSR     pm_pop                  ; realvar addr → tmp3
        STA     tmp3
        LDA     scratch
        STA     tmp3+1
        JSR     pm_pop                  ; file ptr → tmp1
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDA     #0
        STA     tmp0
        STA     tmp0+1
        STA     frr_neg
        STA     frr_frac_count
        STA     frr_seen_dot
@frr_skipws:
        JSR     file_read_char_helper
        STA     frr_cur
        LDY     #F_EOF
        LDA     (tmp1),y
        BEQ     @frr_chk_ws
        JMP     @frr_scale
@frr_chk_ws:
        LDA     frr_cur
        CMP     #' '
        BEQ     @frr_skipws
        CMP     #$09
        BEQ     @frr_skipws
        CMP     #$0D
        BEQ     @frr_skipws
        CMP     #$0A
        BEQ     @frr_skipws
        CMP     #'-'
        BNE     @frr_chk_plus
        LDA     #1
        STA     frr_neg
        JSR     file_read_char_helper
        STA     frr_cur
        BRA     @frr_loop
@frr_chk_plus:
        CMP     #'+'
        BNE     @frr_loop
        JSR     file_read_char_helper
        STA     frr_cur
@frr_loop:
        LDY     #F_EOF
        LDA     (tmp1),y
        BNE     @frr_scale
        LDA     frr_cur
        CMP     #'.'
        BNE     @frr_digit
        LDA     frr_seen_dot
        BNE     @frr_scale
        LDA     #1
        STA     frr_seen_dot
        JSR     file_read_char_helper
        STA     frr_cur
        BRA     @frr_loop
@frr_digit:
        LDA     frr_cur
        CMP     #'0'
        BCC     @frr_scale
        CMP     #'9'+1
        BCS     @frr_scale
        LDA     frr_seen_dot
        BEQ     @frr_accum
        LDA     frr_frac_count
        CMP     #2
        BCS     @frr_skip_digit
@frr_accum:
        LDA     tmp0
        STA     tmp2
        LDA     tmp0+1
        STA     tmp2+1
        LDA     frr_cur
        SEC
        SBC     #'0'
        JSR     accum_digit_tmp2
        LDA     tmp2
        STA     tmp0
        LDA     tmp2+1
        STA     tmp0+1
        LDA     frr_seen_dot
        BEQ     @frr_after_digit
        INC     frr_frac_count
@frr_after_digit:
        JSR     file_read_char_helper
        STA     frr_cur
        BRA     @frr_loop
@frr_skip_digit:
        JSR     file_read_char_helper
        STA     frr_cur
        BRA     @frr_loop
@frr_scale:
        LDA     frr_frac_count
        BNE     @frr_chk_frac1
        LDA     tmp0
        STA     tmp2
        LDA     tmp0+1
        STA     tmp2+1
        JSR     mul_tmp2_by10
        JSR     mul_tmp2_by10
        LDA     tmp2
        STA     tmp0
        LDA     tmp2+1
        STA     tmp0+1
        BRA     @frr_apply
@frr_chk_frac1:
        CMP     #1
        BNE     @frr_apply
        LDA     tmp0
        STA     tmp2
        LDA     tmp0+1
        STA     tmp2+1
        JSR     mul_tmp2_by10
        LDA     tmp2
        STA     tmp0
        LDA     tmp2+1
        STA     tmp0+1
@frr_apply:
        LDA     frr_neg
        BEQ     @frr_store
        LDA     tmp0
        STA     tmp2
        LDA     tmp0+1
        STA     tmp2+1
        JSR     neg_tmp2
        LDA     tmp2
        STA     tmp0
        LDA     tmp2+1
        STA     tmp0+1
@frr_store:
        LDY     #0
        LDA     tmp0
        STA     (tmp3),y
        INY
        LDA     tmp0+1
        STA     (tmp3),y
        JMP     prun_loop

; OP_FRDLN ($BA) — TOS=fileptr → skip remaining chars to end-of-line (LF)
op_FRDLN:
        JSR     pm_pop
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
@frln_loop:
        LDY     #F_EOF
        LDA     (tmp1),y
        BEQ     :+
        JMP     prun_loop
:
        JSR     file_read_char_helper
        CMP     #$0A
        BEQ     @frln_done
        BRA     @frln_loop
@frln_done:
        JMP     prun_loop

; OP_FEOF ($BB) — TOS=fileptr → push BOOL ($FFFF=true, $0000=false)
op_FEOF:
        JSR     pm_pop
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDY     #F_EOF
        LDA     (tmp1),y
        BEQ     @feof_false
        LDA     #$FF
        STA     scratch
        LDA     #$FF
        JSR     pm_push
        JMP     prun_loop
@feof_false:
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop

; OP_FAPPND ($BC) — TOS=fileptr → open/create for append at EOF.
; Existing files are scanned to EOF using the normal buffered read path so
; F_BUF holds the last sector and F_POS lands at the first CTRL-Z (or 128
; when the file ended on a full sector boundary). For a partial final sector,
; FCB[32] is backed up so the next PEM_WRITE updates that last record first.
op_FAPPND:
        JSR     pm_pop
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDA     #0
        LDY     #32
        STA     (tmp1),y                ; scan existing file from record 0
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_OPEN
        JSR     PEM_ENTRY
        CMP     #$FF
        BNE     @fap_open_ok
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_MAKE
        JSR     PEM_ENTRY
        JMP     @fap_empty_write
@fap_open_ok:
        CLC
        LDA     tmp1
        ADC     #F_BUF
        PHA
        LDA     tmp1+1
        ADC     #0
        TAY
        PLA
        JSR     file_setdma
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_READ
        JSR     PEM_ENTRY
        CMP     #0
        BNE     @fap_empty_write
        LDA     #0
        LDY     #F_EOF
        STA     (tmp1),y
        LDY     #F_POS
        STA     (tmp1),y
        JSR     file_check_eof_helper
@fap_scan:
        LDY     #F_EOF
        LDA     (tmp1),y
        BNE     @fap_at_eof
        JSR     file_read_char_helper
        BRA     @fap_scan
@fap_at_eof:
        LDY     #F_POS
        LDA     (tmp1),y
        CMP     #128
        BEQ     @fap_new_sector
        LDY     #32
        LDA     (tmp1),y
        BEQ     @fap_set_write
        SEC
        SBC     #1
        STA     (tmp1),y
        BRA     @fap_set_write
@fap_new_sector:
        LDA     #0
        LDY     #F_POS
        STA     (tmp1),y
@fap_set_write:
        LDA     #0
        LDY     #F_EOF
        STA     (tmp1),y
        LDA     #F_MODE_WRITE
        LDY     #F_MODE
        STA     (tmp1),y
        JMP     prun_loop
@fap_empty_write:
        CLC
        LDA     tmp1
        ADC     #F_BUF
        PHA
        LDA     tmp1+1
        ADC     #0
        TAY
        PLA
        JSR     file_setdma
        LDA     #0
        LDY     #F_POS
        STA     (tmp1),y
        LDY     #F_EOF
        STA     (tmp1),y
        LDA     #F_MODE_WRITE
        LDY     #F_MODE
        STA     (tmp1),y
        JMP     prun_loop

; OP_FRDS ($BD) — NOS=fileptr, TOS=strvar addr → read line into a work buffer;
; store the buffer pointer at strvar.  Skips CR, stops on LF/EOF.
; STRING vars hold a 16-bit pointer (matching LDCS / CONCAT semantics).
FRDS_BUF        = $AC00                  ; dedicated 256-byte buffer below STR_WORK_BASE
op_FRDS:
        JSR     pm_pop                  ; strvar addr → tmp3
        STA     tmp3
        LDA     scratch
        STA     tmp3+1
        JSR     pm_pop                  ; file ptr → tmp1
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDA     #0
        STA     frs_idx
@frs_loop:
        LDY     #F_EOF
        LDA     (tmp1),y
        BNE     @frs_done
; peek char at buf[F_POS] without consuming
        CLC
        LDA     tmp1
        ADC     #F_BUF
        STA     tmp2
        LDA     tmp1+1
        ADC     #0
        STA     tmp2+1
        LDY     #F_POS
        LDA     (tmp1),y
        TAY
        LDA     (tmp2),y
        CMP     #$0D
        BEQ     @frs_done               ; leave CR for READLN
        CMP     #$0A
        BEQ     @frs_done               ; leave LF for READLN
; not at EOL — consume and append
        JSR     file_read_char_helper
        STA     frs_char
        LDA     frs_idx
        CMP     #255
        BCS     @frs_loop               ; clamp (drop char)
        INC     frs_idx
        LDY     frs_idx
        LDA     frs_char
        STA     FRDS_BUF,y
        BRA     @frs_loop
@frs_done:
        LDA     frs_idx
        STA     FRDS_BUF                ; length byte at offset 0
; store buffer ptr ($AC00) into strvar at tmp3
        LDA     #<FRDS_BUF
        LDY     #0
        STA     (tmp3),y
        INY
        LDA     #>FRDS_BUF
        STA     (tmp3),y
        JMP     prun_loop

; OP_FWRB ($BE) — NOS=fileptr, TOS=bool → append "TRUE"/"FALSE"
op_FWRB:
        JSR     pm_pop                  ; bool value
        STA     tmp2
        LDA     scratch
        ORA     tmp2
        STA     tmp2                    ; tmp2 = nonzero if true
        JSR     pm_pop                  ; file ptr
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDA     tmp2
        BEQ     @fwb_false
; "TRUE"
        LDA     #'T'
        JSR     file_write_char_helper
        LDA     #'R'
        JSR     file_write_char_helper
        LDA     #'U'
        JSR     file_write_char_helper
        LDA     #'E'
        JSR     file_write_char_helper
        JMP     prun_loop
@fwb_false:
        LDA     #'F'
        JSR     file_write_char_helper
        LDA     #'A'
        JSR     file_write_char_helper
        LDA     #'L'
        JSR     file_write_char_helper
        LDA     #'S'
        JSR     file_write_char_helper
        LDA     #'E'
        JSR     file_write_char_helper
        JMP     prun_loop

; OP_FEOLN ($BF) — TOS=fileptr → push BOOL: true if EOF, CR, or LF is next
op_FEOLN:
        JSR     pm_pop
        STA     tmp1
        LDA     scratch
        STA     tmp1+1
        LDY     #F_EOF
        LDA     (tmp1),y
        BNE     @feol_true
; peek current char without consuming
        CLC
        LDA     tmp1
        ADC     #F_BUF
        STA     tmp2
        LDA     tmp1+1
        ADC     #0
        STA     tmp2+1
        LDY     #F_POS
        LDA     (tmp1),y
        TAY
        LDA     (tmp2),y
        CMP     #$0D
        BEQ     @feol_true
        CMP     #$0A
        BEQ     @feol_true
        LDA     #0
        STA     scratch
        JSR     pm_push
        JMP     prun_loop
@feol_true:
        LDA     #$FF
        STA     scratch
        LDA     #$FF
        JSR     pm_push
        JMP     prun_loop

; --- File I/O scratch storage ---
fas_remain:    .RES 1
fas_si:        .RES 1
fas_di:        .RES 1
fwrc_char:     .RES 1
fws_remain:    .RES 1
fws_idx:       .RES 1
fwi_val:       .RES 2
fwi_buf:       .RES 6
frch_char:     .RES 1
fri_val:       .RES 2
fri_neg:       .RES 1
fri_cur:       .RES 1
frs_idx:       .RES 1
frs_char:      .RES 1
real32_res0:   .RES 1
real32_res1:   .RES 1
real32_res2:   .RES 1
real32_res3:   .RES 1
real32_mcand0: .RES 1
real32_mcand1: .RES 1
real32_mcand2: .RES 1
real32_mcand3: .RES 1
real32_rem0:   .RES 1
real32_rem1:   .RES 1
real_sign:     .RES 1
real_frac:     .RES 1
real_file_ptr: .RES 2
frr_neg:       .RES 1
frr_frac_count:.RES 1
frr_seen_dot:  .RES 1
frr_cur:       .RES 1

; ---------------------------------------------------------------------------
; Unimplemented opcode handler
; ---------------------------------------------------------------------------
op_UNIMP:
; X still holds the offending opcode at dispatch time
        STX     tmp0
        LDA     #0
        STA     tmp0+1
        PHX                     ; preserve for after banner
        LDA     #<err_rt_opcode
        STA     tmp0
        LDA     #>err_rt_opcode
        STA     tmp0+1
        JSR     console_print_sz
        PLX
        STX     tmp0
        LDA     #0
        STA     tmp0+1
        JSR     console_print_dec
        LDA     #13
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        LDA     #10
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        JMP     WARM_BOOT

; ---------------------------------------------------------------------------
; String literals for boolean output
; ---------------------------------------------------------------------------
str_true:
        .BYTE   "TRUE",0
str_false:
        .BYTE   "FALSE",0
msg_rt_banner:
        .BYTE   "PRUN v0.1 - Pascal P-Code Runtime for DOS/65",13,10,0

; ---------------------------------------------------------------------------
; Dispatch table — 256 entries of (lo, hi) pointer pairs
; ---------------------------------------------------------------------------
        .align  256
dispatch_lo:
; $00-$07
        .BYTE   <op_LDCI,  <op_LDCW,  <op_LDCC,  <op_LDCB
        .BYTE   <op_LDCS,  <op_LDCN,  <op_UNIMP, <op_UNIMP
; $08-$0F
        .REPEAT 8
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $10-$17
        .BYTE   <op_LDL,   <op_STL,   <op_LDA_L, <op_UNIMP
        .REPEAT 4
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $18-$2F
        .BYTE   <op_LDG,   <op_STG,   <op_LDA_G, <op_UNIMP   ; $18-$1B
        .BYTE   <op_UNIMP, <op_UNIMP, <op_UNIMP, <op_UNIMP   ; $1C-$1F
        .BYTE   <op_LDIND, <op_STIND, <op_UNIMP, <op_UNIMP   ; $20-$23
        .BYTE   <op_INDEX                                      ; $24
        .REPEAT 11
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $30-$3F
        .BYTE   <op_ADI,   <op_SBI,   <op_MPI,   <op_DVI
        .BYTE   <op_MOD,   <op_NGI,   <op_UNIMP, <op_UNIMP   ; $36 ABI, $37 SQI
        .BYTE   <op_LAND,  <op_LOR,   <op_LNOT,  <op_BNOT    ; $38-$3B
        .REPEAT 4
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $40-$4F
        .BYTE   <op_EQUI,  <op_NEQI,  <op_LESI,  <op_LEQI
        .BYTE   <op_GTRI,  <op_GEQI,  <op_UNIMP, <op_UNIMP
        .REPEAT 8
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $50-$5F
        .BYTE   <op_UJP,   <op_FJP,   <op_TJP,   <op_UNIMP
        .REPEAT 12
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $60-$6F
        .BYTE   <op_CALL,  <op_UNIMP, <op_RET,   <op_RETF
        .BYTE   <op_MRKSTK,<op_UNIMP, <op_STR,   <op_MRKA
        .REPEAT 8
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $70-$7F
        .BYTE   <op_NEW,   <op_DISP,  <op_UNIMP, <op_UNIMP
        .REPEAT 12
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $80-$8F
        .BYTE   <op_WRITI, <op_WRITC, <op_WRITB, <op_WRITS
        .BYTE   <op_WRITLN,<op_READI, <op_READC, <op_UNIMP
        .BYTE   <op_WRITR, <op_READR
        .REPEAT 6
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $90-$9F
        .BYTE   <op_DUP,   <op_POP,   <op_SWAP,  <op_UNIMP
        .REPEAT 12
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $A0-$A3 string built-ins
        .BYTE   <op_LEN,   <op_POS,   <op_COPY,  <op_CONCAT
; $A4-$AF
        .BYTE   <op_INSET, <op_MPR,   <op_DVR
        .REPEAT 9
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $B0-$BF TEXT file I/O
        .BYTE   <op_FASSGN,<op_FRESET,<op_FREWRT,<op_FCLOSE
        .BYTE   <op_FWRC,  <op_FWRS,  <op_FWRI,  <op_FWLN
        .BYTE   <op_FRDC,  <op_FRDI,  <op_FRDLN, <op_FEOF
        .BYTE   <op_FAPPND,<op_FRDS,  <op_FWRB,  <op_FEOLN
; $C0-$FE
        .BYTE   <op_FWRR,  <op_FRDR
        .REPEAT 61
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $FF
        .BYTE   <op_HALT

dispatch_hi:
; $00-$07
        .BYTE   >op_LDCI,  >op_LDCW,  >op_LDCC,  >op_LDCB
        .BYTE   >op_LDCS,  >op_LDCN,  >op_UNIMP, >op_UNIMP
; $08-$0F
        .REPEAT 8
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $10-$17
        .BYTE   >op_LDL,   >op_STL,   >op_LDA_L, >op_UNIMP
        .REPEAT 4
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $18-$2F
        .BYTE   >op_LDG,   >op_STG,   >op_LDA_G, >op_UNIMP   ; $18-$1B
        .BYTE   >op_UNIMP, >op_UNIMP, >op_UNIMP, >op_UNIMP   ; $1C-$1F
        .BYTE   >op_LDIND, >op_STIND, >op_UNIMP, >op_UNIMP   ; $20-$23
        .BYTE   >op_INDEX                                      ; $24
        .REPEAT 11
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $30-$3F
        .BYTE   >op_ADI,   >op_SBI,   >op_MPI,   >op_DVI
        .BYTE   >op_MOD,   >op_NGI,   >op_UNIMP, >op_UNIMP   ; $36 ABI, $37 SQI
        .BYTE   >op_LAND,  >op_LOR,   >op_LNOT,  >op_BNOT    ; $38-$3B
        .REPEAT 4
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $40-$4F
        .BYTE   >op_EQUI,  >op_NEQI,  >op_LESI,  >op_LEQI
        .BYTE   >op_GTRI,  >op_GEQI,  >op_UNIMP, >op_UNIMP
        .REPEAT 8
            .BYTE   >op_UNIMP
        .ENDREPEAT
; $50-$5F
        .BYTE   >op_UJP,   >op_FJP,   >op_TJP,   >op_UNIMP
        .REPEAT 12
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $60-$6F
        .BYTE   >op_CALL,  >op_UNIMP, >op_RET,   >op_RETF
        .BYTE   >op_MRKSTK,>op_UNIMP, >op_STR,   >op_MRKA
        .REPEAT 8
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $70-$7F
        .BYTE   >op_NEW,   >op_DISP,  >op_UNIMP, >op_UNIMP
        .REPEAT 12
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $80-$8F
        .BYTE   >op_WRITI, >op_WRITC, >op_WRITB, >op_WRITS
        .BYTE   >op_WRITLN,>op_READI, >op_READC, >op_UNIMP
        .BYTE   >op_WRITR, >op_READR
        .REPEAT 6
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $90-$9F
        .BYTE   >op_DUP,   >op_POP,   >op_SWAP,  >op_UNIMP
        .REPEAT 12
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $A0-$A3 string built-ins
        .BYTE   >op_LEN,   >op_POS,   >op_COPY,  >op_CONCAT
; $A4-$AF
        .BYTE   >op_INSET, >op_MPR,   >op_DVR
        .REPEAT 9
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $B0-$BF TEXT file I/O
        .BYTE   >op_FASSGN,>op_FRESET,>op_FREWRT,>op_FCLOSE
        .BYTE   >op_FWRC,  >op_FWRS,  >op_FWRI,  >op_FWLN
        .BYTE   >op_FRDC,  >op_FRDI,  >op_FRDLN, >op_FEOF
        .BYTE   >op_FAPPND, >op_FRDS,  >op_FWRB,  >op_FEOLN
; $C0-$FE
        .BYTE   >op_FWRR,  >op_FRDR
        .REPEAT 61
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $FF
        .BYTE   >op_HALT

; ---------------------------------------------------------------------------
; FCB for the .PCD file (placed in CODE segment, well below STACK_BASE)
; ---------------------------------------------------------------------------
pcd_fcb:
        .RES    36              ; FCB (36 bytes as per CP/M/DOS65)

; ---------------------------------------------------------------------------
; Include shared I/O library
; ---------------------------------------------------------------------------
.include    "IOLIB.ASM"
.include    "MESSAGES.ASM"
