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

        .include        "DEFINITIONS.ASM"
        .include        "ZEROPAGE.ASM"

; ---------------------------------------------------------------------------
; Runtime memory layout constants
; ---------------------------------------------------------------------------
; PCODE_BASE must lie above the end of the CODE segment so that loading
; a .PCD file does not overwrite iolib/messages. CODE has grown past $1400
; (now reaches ~$14CD), so $1800 keeps a comfortable safety margin.
PCODE_BASE      = $1800         ; where p-code is loaded
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

read_line_buf:
        .RES    32
read_len:
        .RES    1
read_neg:
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

; ---------------------------------------------------------------------------
; Unimplemented opcode handler
; ---------------------------------------------------------------------------
op_UNIMP:
        LDA     #<err_rt_opcode
        STA     tmp0
        LDA     #>err_rt_opcode
        STA     tmp0+1
        JSR     rt_error
; rt_error does not return (jumps to warm boot)

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
        .BYTE   <op_LDG,   <op_STG,   <op_LDA_G, <op_UNIMP
        .BYTE   <op_UNIMP, <op_UNIMP, <op_UNIMP, <op_UNIMP
        .BYTE   <op_LDIND, <op_STIND
        .REPEAT 14
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $30-$3F
        .BYTE   <op_ADI,   <op_SBI,   <op_MPI,   <op_DVI
        .BYTE   <op_MOD,   <op_NGI,   <op_UNIMP, <op_UNIMP
        .REPEAT 8
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
        .BYTE   <op_MRKSTK,<op_UNIMP, <op_STR,   <op_UNIMP
        .REPEAT 8
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $70-$7F
        .REPEAT 16
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $80-$8F
        .BYTE   <op_WRITI, <op_WRITC, <op_WRITB, <op_WRITS
        .BYTE   <op_WRITLN,<op_READI, <op_READC, <op_UNIMP
        .REPEAT 8
                .BYTE   <op_UNIMP
        .ENDREPEAT
; $90-$FE
        .BYTE   <op_DUP,   <op_POP,   <op_UNIMP, <op_UNIMP
        .REPEAT 107
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
        .BYTE   >op_LDG,   >op_STG,   >op_LDA_G, >op_UNIMP
        .BYTE   >op_UNIMP, >op_UNIMP, >op_UNIMP, >op_UNIMP
        .BYTE   >op_LDIND, >op_STIND
        .REPEAT 14
           .BYTE   >op_UNIMP
        .ENDREPEAT
; $30-$3F
        .BYTE   >op_ADI,   >op_SBI,   >op_MPI,   >op_DVI
        .BYTE   >op_MOD,   >op_NGI,   >op_UNIMP, >op_UNIMP
        .REPEAT 8
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
        .BYTE   >op_MRKSTK,>op_UNIMP, >op_STR,   >op_UNIMP
        .REPEAT 8
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $70-$7F
        .REPEAT 16
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $80-$8F
        .BYTE   >op_WRITI, >op_WRITC, >op_WRITB, >op_WRITS
        .BYTE   >op_WRITLN,>op_READI, >op_READC, >op_UNIMP
        .REPEAT 8
                .BYTE   >op_UNIMP
        .ENDREPEAT
; $90-$FE
        .BYTE   >op_DUP,   >op_POP,   >op_UNIMP, >op_UNIMP
        .REPEAT 107
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
