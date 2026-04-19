; prun.asm — P-Code Runtime for DOS/65
;
; Usage: PRUN <filename>   (loads <filename>.PCD and executes it)
;
; Memory layout at runtime:
;   $0800-$13FF  this interpreter (code, dispatch tables, FCB, iolib, msgs)
;   $1400-$1FFF  p-code loaded from .PCD (3 KB)
;   $2000-$7FFF  p-machine value stack (grows up)
;   $8000-$AFFF  heap (grows down from $B000)
;   $B000-$B7DF  globals + string pool

.include "definitions.asm"
.include "zeropage.asm"

; ---------------------------------------------------------------------------
; Runtime memory layout constants
; ---------------------------------------------------------------------------
; PCODE_BASE must lie above the end of the CODE segment so that loading
; a .PCD file does not overwrite iolib/messages. CODE currently ends near
; $12CD; $1400 gives a safety margin for growth.
PCODE_BASE      = $1400         ; where p-code is loaded
STACK_BASE      = $2000         ; bottom of p-machine value stack
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

.segment "TEA"

; ---------------------------------------------------------------------------
; Entry point — DOS/65 loads .COM at $0800 and jumps here
; ---------------------------------------------------------------------------
        jmp     prun_main

; ---------------------------------------------------------------------------
; prun_main
; ---------------------------------------------------------------------------
.segment "CODE"
prun_main:
        ; print banner
        lda     #<msg_rt_banner
        sta     tmp0
        lda     #>msg_rt_banner
        sta     tmp0+1
        jsr     console_print_sz

        ; build FCB from DEFAULT_FCB (DOS/65 fills $0107 with first argument)
        jsr     build_pcd_fcb   ; copy $0107 → pcd_fcb, append .PCD extension
        bcs     @no_file

        ; open the .PCD file
        lda     #<pcd_fcb
        sta     tmp1
        lda     #>pcd_fcb
        sta     tmp1+1
        jsr     file_open
        cmp     #$FF            ; PEM open returns $FF=not found, 0-3=dir entry
        beq     @no_file

        ; load p-code into memory starting at PCODE_BASE
        jsr     load_pcd
        bcs     @load_err

        ; execute
        jsr     prun_execute

        ; clean exit
        lda     #<pcd_fcb
        sta     tmp1
        lda     #>pcd_fcb
        sta     tmp1+1
        jsr     file_close
        lda     #<msg_rt_done
        sta     tmp0
        lda     #>msg_rt_done
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

@load_err:
        lda     #<err_rt_magic
        sta     tmp0
        lda     #>err_rt_magic
        sta     tmp0+1
        jsr     console_print_sz
        jmp     WARM_BOOT

; ---------------------------------------------------------------------------
; build_pcd_fcb — copy DEFAULT_FCB ($0107) to pcd_fcb, force extension "PCD"
; DOS/65 CCM copies the parsed argument FCB to dflfcb=$0107, not $005C.
; Returns: carry clear = ok, carry set = no filename given
; ---------------------------------------------------------------------------
build_pcd_fcb:
        ldx     #0
@copy:  lda     DEFAULT_FCB,x
        sta     pcd_fcb,x
        inx
        cpx     #12
        bcc     @copy
        ; zero the rest of FCB
        lda     #0
@zero:  sta     pcd_fcb,x
        inx
        cpx     #36
        bcc     @zero
        ; check filename is non-blank
        lda     pcd_fcb+1       ; first char of filename field
        cmp     #' '
        beq     @noname
        ; force extension bytes 9-11 to "PCD"
        lda     #'P'
        sta     pcd_fcb+9
        lda     #'C'
        sta     pcd_fcb+10
        lda     #'D'
        sta     pcd_fcb+11
        clc
        rts
@noname:
        sec
        rts

; ---------------------------------------------------------------------------
; load_pcd — read .PCD header, validate, load code + globals + strings
; Returns: carry clear = ok, carry set = error
; ---------------------------------------------------------------------------
load_pcd:
        ; ensure tmp1 points to pcd_fcb for all file I/O calls
        lda     #<pcd_fcb
        sta     tmp1
        lda     #>pcd_fcb
        sta     tmp1+1
        ; Read first sector (contains header + start of code)
        jsr     file_read_sector
        cmp     #0
        bne     @err

        ; validate magic
        lda     DMA_BUF+0
        cmp     #PCD_MAGIC_0
        bne     @err
        lda     DMA_BUF+1
        cmp     #PCD_MAGIC_1
        bne     @err

        ; entry point — preserved in tmp3 for prun_execute
        lda     DMA_BUF+PCD_ENTRY
        sta     tmp3
        lda     DMA_BUF+PCD_ENTRY+1
        sta     tmp3+1

        ; remaining bytes still to copy = code size
        lda     DMA_BUF+PCD_CODESZ
        sta     tmp2
        lda     DMA_BUF+PCD_CODESZ+1
        sta     tmp2+1

        ; dest = PCODE_BASE - PCD_HEADER_SZ so that for the first sector
        ; (tmp0),Y with Y=12..127 lands at PCODE_BASE+0..115.
        sec
        lda     #<PCODE_BASE
        sbc     #PCD_HEADER_SZ
        sta     tmp0
        lda     #>PCODE_BASE
        sbc     #0
        sta     tmp0+1

        ldy     #PCD_HEADER_SZ
@cloop: lda     tmp2
        ora     tmp2+1
        beq     @done_ok
        cpy     #128
        bcc     @cbyte
        ; sector exhausted — advance dest by 128 and read the next one
        clc
        lda     tmp0
        adc     #128
        sta     tmp0
        bcc     :+
        inc     tmp0+1
:       jsr     file_read_sector
        cmp     #0
        bne     @err
        ldy     #0
@cbyte: lda     DMA_BUF,y
        sta     (tmp0),y
        iny
        lda     tmp2
        bne     :+
        dec     tmp2+1
:       dec     tmp2
        jmp     @cloop

@done_ok:
        clc
        rts
@err:   sec
        rts

; ---------------------------------------------------------------------------
; prun_execute — main fetch-decode-execute loop
; ---------------------------------------------------------------------------
prun_execute:
        ; initialise p-machine registers
        lda     #<PCODE_BASE
        clc
        adc     tmp3            ; entry offset lo
        sta     pm_ipc
        lda     #>PCODE_BASE
        adc     tmp3+1
        sta     pm_ipc+1

        lda     #<STACK_BASE
        sta     pm_sp
        lda     #>STACK_BASE
        sta     pm_sp+1

        lda     #<STACK_BASE    ; first frame at bottom of stack
        sta     pm_mp
        lda     #>STACK_BASE
        sta     pm_mp+1

        lda     #<GLOBALS_BASE
        sta     pm_base
        lda     #>GLOBALS_BASE
        sta     pm_base+1

        lda     #<HEAP_TOP
        sta     pm_np
        lda     #>HEAP_TOP
        sta     pm_np+1

; ---------------------------------------------------------------------------
; Fetch-decode-execute inner loop
; ---------------------------------------------------------------------------
prun_loop:
        ; fetch opcode at pm_ipc
        ldy     #0
        lda     (pm_ipc),y
        ; advance IPC
        inc     pm_ipc
        bne     :+
        inc     pm_ipc+1
:
        ; dispatch via jump table
        tax                     ; X = opcode
        lda     dispatch_lo,x
        sta     tmp0
        lda     dispatch_hi,x
        sta     tmp0+1
        jmp     (tmp0)

; ---------------------------------------------------------------------------
; Stack helpers
; push 16-bit value in A(lo) / scratch(hi) onto p-machine stack
; ---------------------------------------------------------------------------
pm_push:
        ldy     #0
        sta     (pm_sp),y
        iny
        lda     scratch
        sta     (pm_sp),y
        clc
        lda     pm_sp
        adc     #2
        sta     pm_sp
        bcc     :+
        inc     pm_sp+1
:       rts

; pop 16-bit value from p-machine stack → A(lo) / scratch(hi)
pm_pop:
        sec
        lda     pm_sp
        sbc     #2
        sta     pm_sp
        bcs     :+
        dec     pm_sp+1
:       ldy     #0
        lda     (pm_sp),y
        pha
        iny
        lda     (pm_sp),y
        sta     scratch
        pla
        rts

; peek at TOS without popping → A(lo) / scratch(hi)
pm_peek:
        lda     pm_sp
        sec
        sbc     #2
        sta     tmp1
        lda     pm_sp+1
        sbc     #0
        sta     tmp1+1
        ldy     #0
        lda     (tmp1),y
        pha
        iny
        lda     (tmp1),y
        sta     scratch
        pla
        rts

; fetch inline byte at IPC (advances IPC), returns in A
pm_fetch_byte:
        ldy     #0
        lda     (pm_ipc),y
        inc     pm_ipc
        bne     :+
        inc     pm_ipc+1
:       rts

; fetch inline word (lo then hi) at IPC, returns lo in A, hi in scratch
pm_fetch_word:
        jsr     pm_fetch_byte
        pha
        jsr     pm_fetch_byte
        sta     scratch
        pla
        rts

; ---------------------------------------------------------------------------
; Runtime error handler
; ---------------------------------------------------------------------------
rt_error:
        ; tmp0 already points to error string (caller's responsibility)
        jsr     console_print_sz
        jmp     WARM_BOOT

; ---------------------------------------------------------------------------
; Opcode handlers
; ---------------------------------------------------------------------------

; OP_HALT ($FF) — stop execution
op_HALT:
        rts                     ; return to prun_main

; OP_LDCI ($00) — push sign-extended byte constant
op_LDCI:
        jsr     pm_fetch_byte
        pha
        ; sign-extend: if bit 7 set, high byte = $FF else $00
        and     #$80
        beq     :+
        lda     #$FF
        bne     :++
:       lda     #$00
:       sta     scratch
        pla
        jsr     pm_push
        jmp     prun_loop

; OP_LDCW ($01) — push word constant
op_LDCW:
        jsr     pm_fetch_word   ; A=lo, scratch=hi
        jsr     pm_push
        jmp     prun_loop

; OP_LDCC ($02) — push char constant (zero-extended)
op_LDCC:
        jsr     pm_fetch_byte
        pha
        lda     #0
        sta     scratch
        pla
        jsr     pm_push
        jmp     prun_loop

; OP_LDCB ($03) — push boolean constant
op_LDCB:
        jsr     pm_fetch_byte
        beq     :+
        lda     #$FF            ; TRUE = $FFFF
        sta     scratch
        lda     #$FF
        jsr     pm_push
        jmp     prun_loop
:       lda     #0
        sta     scratch
        jsr     pm_push
        jmp     prun_loop

; OP_LDCN ($05) — push NIL ($0000)
op_LDCN:
        lda     #0
        sta     scratch
        jsr     pm_push
        jmp     prun_loop

; OP_LDL ($10) — push 16-bit local var
op_LDL:
        jsr     pm_fetch_byte   ; byte offset
        clc
        adc     #AR_LOCALS      ; offset from MP
        pha
        lda     pm_mp
        clc
        pla
        adc     pm_mp
        sta     tmp1
        lda     pm_mp+1
        adc     #0
        sta     tmp1+1
        ldy     #0
        lda     (tmp1),y
        pha
        iny
        lda     (tmp1),y
        sta     scratch
        pla
        jsr     pm_push
        jmp     prun_loop

; OP_STL ($11) — pop into 16-bit local var
op_STL:
        jsr     pm_fetch_byte   ; byte offset
        clc
        adc     #AR_LOCALS
        clc
        adc     pm_mp
        sta     tmp1
        lda     pm_mp+1
        adc     #0
        sta     tmp1+1
        jsr     pm_pop          ; A=lo, scratch=hi
        ldy     #0
        sta     (tmp1),y
        iny
        lda     scratch
        sta     (tmp1),y
        jmp     prun_loop

; OP_LDG ($18) — push 16-bit global var (word offset from pm_base)
op_LDG:
        jsr     pm_fetch_word   ; A=lo offset, scratch=hi offset
        clc
        adc     pm_base
        sta     tmp1
        lda     scratch
        adc     pm_base+1
        sta     tmp1+1
        ldy     #0
        lda     (tmp1),y
        pha
        iny
        lda     (tmp1),y
        sta     scratch
        pla
        jsr     pm_push
        jmp     prun_loop

; OP_STG ($19) — pop into 16-bit global var
op_STG:
        jsr     pm_fetch_word   ; A=lo offset, scratch=hi offset
        clc
        adc     pm_base
        sta     tmp1
        lda     scratch
        adc     pm_base+1
        sta     tmp1+1
        jsr     pm_pop          ; A=lo, scratch=hi
        ldy     #0
        sta     (tmp1),y
        iny
        lda     scratch
        sta     (tmp1),y
        jmp     prun_loop

; OP_ADI ($30) — integer add
op_ADI:
        jsr     pm_pop          ; b: A=lo, scratch=hi
        sta     tmp2
        lda     scratch
        sta     tmp2+1
        jsr     pm_pop          ; a: A=lo, scratch=hi
        clc
        adc     tmp2            ; a.lo + b.lo
        pha
        lda     scratch
        adc     tmp2+1          ; a.hi + b.hi + carry
        sta     scratch
        pla
        jsr     pm_push
        jmp     prun_loop

; OP_SBI ($31) — integer subtract
op_SBI:
        jsr     pm_pop          ; b: A=lo, scratch=hi
        sta     tmp2
        lda     scratch
        sta     tmp2+1
        jsr     pm_pop          ; a: A=lo, scratch=hi
        sec
        sbc     tmp2            ; a.lo - b.lo
        pha
        lda     scratch
        sbc     tmp2+1          ; a.hi - b.hi - borrow
        sta     scratch
        pla
        jsr     pm_push
        jmp     prun_loop

; OP_NGI ($35) — negate TOS
op_NGI:
        jsr     pm_pop          ; A=lo, scratch=hi
        sta     tmp2
        lda     scratch
        sta     tmp2+1
        lda     #0
        sec
        sbc     tmp2            ; negate lo
        pha
        lda     #0
        sbc     tmp2+1          ; negate hi
        sta     scratch
        pla
        jsr     pm_push
        jmp     prun_loop

; ---------------------------------------------------------------------------
; mul/div helpers — operate on tmp2 (a) and tmp1 (b), result in tmp2
; ---------------------------------------------------------------------------

; mul16: tmp2 = tmp2 * tmp1 (low 16 bits, unsigned)
mul16:
        lda     #0
        sta     scratch         ; result lo
        sta     scratch+1       ; result hi
        ldx     #16
@bit:   lsr     tmp1+1
        ror     tmp1
        bcc     @no_add
        clc
        lda     scratch
        adc     tmp2
        sta     scratch
        lda     scratch+1
        adc     tmp2+1
        sta     scratch+1
@no_add:
        asl     tmp2
        rol     tmp2+1
        dex
        bne     @bit
        lda     scratch
        sta     tmp2
        lda     scratch+1
        sta     tmp2+1
        rts

; udiv16: tmp2 = tmp2 / tmp1, scratch = tmp2 mod tmp1 (unsigned)
udiv16:
        lda     #0
        sta     scratch         ; remainder lo
        sta     scratch+1       ; remainder hi
        ldx     #16
@bit:   asl     tmp2            ; shift dividend left, top bit into rem
        rol     tmp2+1
        rol     scratch
        rol     scratch+1
        ; try subtract divisor from remainder
        lda     scratch
        sec
        sbc     tmp1
        tay
        lda     scratch+1
        sbc     tmp1+1
        bcc     @no_sub
        sta     scratch+1
        sty     scratch
        inc     tmp2            ; quotient bit
@no_sub:
        dex
        bne     @bit
        rts

; sign16: returns A=0 if tmp2>=0, A=$FF if negative
sign16:
        lda     tmp2+1
        and     #$80
        beq     @pos
        lda     #$FF
        rts
@pos:   lda     #0
        rts

; neg_tmp2: tmp2 = -tmp2 (16-bit two's complement)
neg_tmp2:
        sec
        lda     #0
        sbc     tmp2
        sta     tmp2
        lda     #0
        sbc     tmp2+1
        sta     tmp2+1
        rts

; OP_MPI ($32) — integer multiply (low 16 bits, sign doesn't matter)
op_MPI:
        jsr     pm_pop          ; b → tmp1
        sta     tmp1
        lda     scratch
        sta     tmp1+1
        jsr     pm_pop          ; a → tmp2
        sta     tmp2
        lda     scratch
        sta     tmp2+1
        jsr     mul16           ; tmp2 = a*b
        lda     tmp2
        ldx     tmp2+1
        stx     scratch
        jsr     pm_push
        jmp     prun_loop

; OP_DVI ($33) — signed integer divide (truncate toward zero)
op_DVI:
        jsr     pm_pop          ; b → tmp1
        sta     tmp1
        lda     scratch
        sta     tmp1+1
        jsr     pm_pop          ; a → tmp2
        sta     tmp2
        lda     scratch
        sta     tmp2+1
        ; compute result sign = sign(a) XOR sign(b)
        lda     tmp2+1
        eor     tmp1+1
        and     #$80
        pha                     ; save result sign on 6502 stack
        ; abs(a)
        bit     tmp2+1
        bpl     :+
        jsr     neg_tmp2
:       ; abs(b) — temporarily move tmp1 to tmp2 area? simpler: inline negate of tmp1
        bit     tmp1+1
        bpl     :+
        sec
        lda     #0
        sbc     tmp1
        sta     tmp1
        lda     #0
        sbc     tmp1+1
        sta     tmp1+1
:       jsr     udiv16          ; tmp2 = quotient
        pla                     ; result sign
        beq     :+
        jsr     neg_tmp2
:       lda     tmp2
        ldx     tmp2+1
        stx     scratch
        jsr     pm_push
        jmp     prun_loop

; OP_MOD ($34) — signed modulo: a MOD b = a - (a DIV b)*b
op_MOD:
        jsr     pm_pop          ; b → tmp1
        sta     tmp1
        lda     scratch
        sta     tmp1+1
        jsr     pm_pop          ; a → tmp2
        sta     tmp2
        lda     scratch
        sta     tmp2+1
        ; result sign follows dividend (a) for Pascal MOD
        lda     tmp2+1
        and     #$80
        pha
        bit     tmp2+1
        bpl     :+
        jsr     neg_tmp2
:       bit     tmp1+1
        bpl     :+
        sec
        lda     #0
        sbc     tmp1
        sta     tmp1
        lda     #0
        sbc     tmp1+1
        sta     tmp1+1
:       jsr     udiv16          ; remainder in scratch
        lda     scratch
        sta     tmp2
        lda     scratch+1
        sta     tmp2+1
        pla
        beq     :+
        jsr     neg_tmp2
:       lda     tmp2
        ldx     tmp2+1
        stx     scratch
        jsr     pm_push
        jmp     prun_loop

; OP_EQUI ($40) — integer equal
op_EQUI:
        jsr     pm_pop          ; b
        sta     tmp2
        lda     scratch
        sta     tmp2+1
        jsr     pm_pop          ; a
        cmp     tmp2
        bne     @false
        lda     scratch
        cmp     tmp2+1
        bne     @false
        lda     #$FF
        sta     scratch
        jsr     pm_push
        jmp     prun_loop
@false: lda     #0
        sta     scratch
        jsr     pm_push
        jmp     prun_loop

; OP_LESI ($42) — a < b  (signed)
op_LESI:
        jsr     pm_pop          ; b → tmp2
        sta     tmp2
        lda     scratch
        sta     tmp2+1
        jsr     pm_pop          ; a
        ; signed compare: (a - b), check N and V
        sec
        sbc     tmp2
        lda     scratch
        sbc     tmp2+1
        ; N XOR V indicates a < b
        bvs     @ov
        bmi     @true
        bpl     @false2
@ov:    bpl     @true
@false2:
        lda     #0
        sta     scratch
        jsr     pm_push
        jmp     prun_loop
@true:  lda     #$FF
        sta     scratch
        jsr     pm_push
        jmp     prun_loop

; OP_NEQI ($41) — integer not-equal
op_NEQI:
        jsr     pm_pop
        sta     tmp2
        lda     scratch
        sta     tmp2+1
        jsr     pm_pop
        cmp     tmp2
        bne     @true
        lda     scratch
        cmp     tmp2+1
        bne     @true
        lda     #0
        sta     scratch
        jsr     pm_push
        jmp     prun_loop
@true:  lda     #$FF
        sta     scratch
        jsr     pm_push
        jmp     prun_loop

; OP_LEQI ($43) — a <= b  (signed)
op_LEQI:
        jsr     pm_pop          ; b → tmp2
        sta     tmp2
        lda     scratch
        sta     tmp2+1
        jsr     pm_pop          ; a (lo=A, hi=scratch)
        sec
        sbc     tmp2            ; A = a_lo - b_lo
        tax                     ; preserve low result for zero-test
        lda     scratch
        sbc     tmp2+1          ; A = a_hi - b_hi (with borrow)
        ; if a < b → N XOR V == 1
        bvs     @ov
        bmi     @true
        bne     @false3         ; positive non-zero hi → a > b
        cpx     #0
        beq     @true           ; a == b
        jmp     @false3
@ov:    bpl     @true           ; a < b under overflow
@false3:
        lda     #0
        sta     scratch
        jsr     pm_push
        jmp     prun_loop
@true:  lda     #$FF
        sta     scratch
        jsr     pm_push
        jmp     prun_loop

; OP_GTRI ($44) — a > b  (signed) ; equivalent to b < a
op_GTRI:
        jsr     pm_pop          ; b
        sta     tmp2
        lda     scratch
        sta     tmp2+1
        jsr     pm_pop          ; a
        ; compute b - a  (i.e. swap operands of LESI)
        ; flip via: result_true if a > b, i.e. (a - b) > 0
        sec
        sbc     tmp2            ; lo
        tax
        lda     scratch
        sbc     tmp2+1          ; hi with borrow
        bvs     @ov
        bmi     @false4         ; a < b
        bne     @true           ; positive non-zero hi → a > b
        cpx     #0
        beq     @false4         ; a == b
        jmp     @true
@ov:    bpl     @false4
@true:  lda     #$FF
        sta     scratch
        jsr     pm_push
        jmp     prun_loop
@false4:
        lda     #0
        sta     scratch
        jsr     pm_push
        jmp     prun_loop

; OP_GEQI ($45) — a >= b  (signed) ; equivalent to NOT (a < b)
op_GEQI:
        jsr     pm_pop          ; b
        sta     tmp2
        lda     scratch
        sta     tmp2+1
        jsr     pm_pop          ; a
        sec
        sbc     tmp2
        lda     scratch
        sbc     tmp2+1
        bvs     @ov
        bmi     @false5         ; a < b
        jmp     @true
@ov:    bpl     @false5
@true:  lda     #$FF
        sta     scratch
        jsr     pm_push
        jmp     prun_loop
@false5:
        lda     #0
        sta     scratch
        jsr     pm_push
        jmp     prun_loop

; OP_UJP ($50) — unconditional jump (signed word offset from current IPC)
op_UJP:
        jsr     pm_fetch_word   ; A=lo, scratch=hi (signed offset)
        clc
        adc     pm_ipc
        sta     pm_ipc
        lda     scratch
        adc     pm_ipc+1
        sta     pm_ipc+1
        jmp     prun_loop

; OP_FJP ($51) — jump if TOS is false (0)
op_FJP:
        jsr     pm_fetch_word
        pha
        lda     scratch
        pha
        jsr     pm_pop          ; condition
        ora     scratch         ; zero?
        bne     @no_jump        ; not zero = true, no jump
        pla                     ; hi offset
        sta     scratch
        pla                     ; lo offset
        clc
        adc     pm_ipc
        sta     pm_ipc
        lda     scratch
        adc     pm_ipc+1
        sta     pm_ipc+1
        jmp     prun_loop
@no_jump:
        pla
        pla
        jmp     prun_loop

; OP_TJP ($52) — jump if TOS is true (≠0)
op_TJP:
        jsr     pm_fetch_word
        pha
        lda     scratch
        pha
        jsr     pm_pop
        ora     scratch
        beq     @no_jump2
        pla
        sta     scratch
        pla
        clc
        adc     pm_ipc
        sta     pm_ipc
        lda     scratch
        adc     pm_ipc+1
        sta     pm_ipc+1
        jmp     prun_loop
@no_jump2:
        pla
        pla
        jmp     prun_loop

; OP_MRKSTK ($64) — push activation record, reserve locals
; Operand: 1 byte = size of local area in bytes
op_MRKSTK:
        jsr     pm_fetch_byte   ; local size
        sta     tmp2            ; save local size

        ; save dynamic link (current MP) into new frame at current SP
        ldy     #0
        lda     pm_mp
        sta     (pm_sp),y       ; store at current SP (dynamic link lo)
        ; ... full activation record setup deferred to Phase 1 implementation
        ; For now: advance SP by AR_LOCALS + local_size
        lda     pm_sp
        clc
        adc     #AR_LOCALS
        sta     pm_mp           ; new MP = old SP (frame base)
        lda     pm_sp+1
        adc     #0
        sta     pm_mp+1
        lda     pm_sp
        clc
        adc     tmp2
        clc
        adc     #AR_LOCALS
        sta     pm_sp
        lda     pm_sp+1
        adc     #0
        sta     pm_sp+1
        jmp     prun_loop

; OP_CALL ($60) — call procedure (signed word offset)
op_CALL:
        jsr     pm_fetch_word   ; A=lo, scratch=hi
        ; TODO: full activation record push
        ; placeholder: just jump
        clc
        adc     pm_ipc
        sta     pm_ipc
        lda     scratch
        adc     pm_ipc+1
        sta     pm_ipc+1
        jmp     prun_loop

; OP_RET ($62) — return from procedure
op_RET:
        ; TODO: restore MP and IPC from activation record
        ; placeholder: halt for now
        rts

; OP_WRITI ($80) — pop integer, print decimal
op_WRITI:
        jsr     pm_pop          ; A=lo, scratch=hi
        sta     tmp0
        lda     scratch
        sta     tmp0+1
        jsr     console_print_dec
        jmp     prun_loop

; OP_WRITC ($81) — pop char, print
op_WRITC:
        jsr     pm_pop
        jsr     console_putc
        jmp     prun_loop

; OP_WRITB ($82) — pop boolean, print TRUE/FALSE
op_WRITB:
        jsr     pm_pop
        ora     scratch
        beq     @false3
        lda     #<str_true
        sta     tmp0
        lda     #>str_true
        sta     tmp0+1
        jsr     console_print_sz
        jmp     prun_loop
@false3:
        lda     #<str_false
        sta     tmp0
        lda     #>str_false
        sta     tmp0+1
        jsr     console_print_sz
        jmp     prun_loop

; OP_WRITS ($83) — TOS = address of length-prefixed string, print it
op_WRITS:
        jsr     pm_pop
        sta     tmp0
        lda     scratch
        sta     tmp0+1
        jsr     console_print_pstr
        jmp     prun_loop

; OP_WRITLN ($84) — newline
op_WRITLN:
        jsr     console_newline
        jmp     prun_loop

; OP_READI ($85) — read integer from console, push
op_READI:
        ; TODO: implement proper integer input
        lda     #0
        sta     scratch
        jsr     pm_push
        jmp     prun_loop

; OP_READC ($86) — read char from console, push
op_READC:
        PEM PEM_CONIN
        pha
        lda     #0
        sta     scratch
        pla
        jsr     pm_push
        jmp     prun_loop

; OP_DUP ($90) — duplicate TOS
op_DUP:
        jsr     pm_peek
        jsr     pm_push
        jmp     prun_loop

; OP_POP ($91) — discard TOS
op_POP:
        jsr     pm_pop
        jmp     prun_loop

; ---------------------------------------------------------------------------
; Unimplemented opcode handler
; ---------------------------------------------------------------------------
op_UNIMP:
        lda     #<err_rt_opcode
        sta     tmp0
        lda     #>err_rt_opcode
        sta     tmp0+1
        jsr     rt_error
        ; rt_error does not return (jumps to warm boot)

; ---------------------------------------------------------------------------
; String literals for boolean output
; ---------------------------------------------------------------------------
str_true:       .byte "TRUE",0
str_false:      .byte "FALSE",0
msg_rt_banner:  .byte "PRUN v0.1 - Pascal P-Code Runtime for DOS/65",13,10,0

; ---------------------------------------------------------------------------
; Dispatch table — 256 entries of (lo, hi) pointer pairs
; ---------------------------------------------------------------------------
.align 256
dispatch_lo:
        ; $00-$07
        .byte <op_LDCI,  <op_LDCW,  <op_LDCC,  <op_LDCB
        .byte <op_UNIMP, <op_LDCN,  <op_UNIMP, <op_UNIMP
        ; $08-$0F
        .repeat 8
        .byte <op_UNIMP
        .endrepeat
        ; $10-$17
        .byte <op_LDL,   <op_STL,   <op_UNIMP, <op_UNIMP
        .repeat 4
        .byte <op_UNIMP
        .endrepeat
        ; $18-$2F
        .byte <op_LDG,   <op_STG
        .repeat 22
        .byte <op_UNIMP
        .endrepeat
        ; $30-$3F
        .byte <op_ADI,   <op_SBI,   <op_MPI,   <op_DVI
        .byte <op_MOD,   <op_NGI,   <op_UNIMP, <op_UNIMP
        .repeat 8
        .byte <op_UNIMP
        .endrepeat
        ; $40-$4F
        .byte <op_EQUI,  <op_NEQI,  <op_LESI,  <op_LEQI
        .byte <op_GTRI,  <op_GEQI,  <op_UNIMP, <op_UNIMP
        .repeat 8
        .byte <op_UNIMP
        .endrepeat
        ; $50-$5F
        .byte <op_UJP,   <op_FJP,   <op_TJP,   <op_UNIMP
        .repeat 12
        .byte <op_UNIMP
        .endrepeat
        ; $60-$6F
        .byte <op_CALL,  <op_UNIMP, <op_RET,   <op_UNIMP
        .byte <op_MRKSTK,<op_UNIMP, <op_UNIMP, <op_UNIMP
        .repeat 8
        .byte <op_UNIMP
        .endrepeat
        ; $70-$7F
        .repeat 16
        .byte <op_UNIMP
        .endrepeat
        ; $80-$8F
        .byte <op_WRITI, <op_WRITC, <op_WRITB, <op_WRITS
        .byte <op_WRITLN,<op_READI, <op_READC, <op_UNIMP
        .repeat 8
        .byte <op_UNIMP
        .endrepeat
        ; $90-$FE
        .byte <op_DUP,   <op_POP,   <op_UNIMP, <op_UNIMP
        .repeat 107
        .byte <op_UNIMP
        .endrepeat
        ; $FF
        .byte <op_HALT

dispatch_hi:
        ; $00-$07
        .byte >op_LDCI,  >op_LDCW,  >op_LDCC,  >op_LDCB
        .byte >op_UNIMP, >op_LDCN,  >op_UNIMP, >op_UNIMP
        ; $08-$0F
        .repeat 8
        .byte >op_UNIMP
        .endrepeat
        ; $10-$17
        .byte >op_LDL,   >op_STL,   >op_UNIMP, >op_UNIMP
        .repeat 4
        .byte >op_UNIMP
        .endrepeat
        ; $18-$2F
        .byte >op_LDG,   >op_STG
        .repeat 22
        .byte >op_UNIMP
        .endrepeat
        ; $30-$3F
        .byte >op_ADI,   >op_SBI,   >op_MPI,   >op_DVI
        .byte >op_MOD,   >op_NGI,   >op_UNIMP, >op_UNIMP
        .repeat 8
        .byte >op_UNIMP
        .endrepeat
        ; $40-$4F
        .byte >op_EQUI,  >op_NEQI,  >op_LESI,  >op_LEQI
        .byte >op_GTRI,  >op_GEQI,  >op_UNIMP, >op_UNIMP
        .repeat 8
        .byte >op_UNIMP
        .endrepeat
        ; $50-$5F
        .byte >op_UJP,   >op_FJP,   >op_TJP,   >op_UNIMP
        .repeat 12
        .byte >op_UNIMP
        .endrepeat
        ; $60-$6F
        .byte >op_CALL,  >op_UNIMP, >op_RET,   >op_UNIMP
        .byte >op_MRKSTK,>op_UNIMP, >op_UNIMP, >op_UNIMP
        .repeat 8
        .byte >op_UNIMP
        .endrepeat
        ; $70-$7F
        .repeat 16
        .byte >op_UNIMP
        .endrepeat
        ; $80-$8F
        .byte >op_WRITI, >op_WRITC, >op_WRITB, >op_WRITS
        .byte >op_WRITLN,>op_READI, >op_READC, >op_UNIMP
        .repeat 8
        .byte >op_UNIMP
        .endrepeat
        ; $90-$FE
        .byte >op_DUP,   >op_POP,   >op_UNIMP, >op_UNIMP
        .repeat 107
        .byte >op_UNIMP
        .endrepeat
        ; $FF
        .byte >op_HALT

; ---------------------------------------------------------------------------
; FCB for the .PCD file (placed in CODE segment, well below STACK_BASE)
; ---------------------------------------------------------------------------
pcd_fcb:        .res 36         ; FCB (36 bytes as per CP/M/DOS65)

; ---------------------------------------------------------------------------
; Include shared I/O library
; ---------------------------------------------------------------------------
.include "iolib.asm"
.include "messages.asm"
