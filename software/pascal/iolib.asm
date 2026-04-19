.pc02
; iolib.asm — console and file I/O wrappers for DOS/65
;
; Uses PEM function 2 (CONOUT) for character output.
; This routes through DFT_CONSOLE and works on all platforms
; (serial, ESP video, memory-mapped video) regardless of which
; console driver is active.

.segment "CODE"

; ---------------------------------------------------------------------------
; console_putc — output character in A to console via PEM #2
; ---------------------------------------------------------------------------
console_putc:
        pha
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        pla
        rts

; ---------------------------------------------------------------------------
; console_newline — emit CR+LF
; ---------------------------------------------------------------------------
console_newline:
        lda     #$0D
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        lda     #$0A
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        rts

; ---------------------------------------------------------------------------
; console_print_sz — print null-terminated string at tmp0
; ---------------------------------------------------------------------------
console_print_sz:
        ldy     #0
@loop:  lda     (tmp0),y
        beq     @done
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        iny
        bne     @loop
@done:  rts

; ---------------------------------------------------------------------------
; console_print_pstr — print length-prefixed Pascal string at tmp0
; ---------------------------------------------------------------------------
console_print_pstr:
        ldy     #0
        lda     (tmp0),y        ; length byte
        beq     @done
        tax                     ; X = remaining chars — but X used by PEM!
        ; save length, use tmp1 as counter
        sta     tmp1
        iny
@loop:  lda     (tmp0),y
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        iny
        dec     tmp1
        bne     @loop
@done:  rts

; ---------------------------------------------------------------------------
; console_print_dec — print signed 16-bit integer in tmp0:tmp0+1
; ---------------------------------------------------------------------------
console_print_dec:
        lda     tmp0+1
        bpl     @positive
        lda     #'-'
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        lda     #0
        sec
        sbc     tmp0
        sta     tmp0
        lda     #0
        sbc     tmp0+1
        sta     tmp0+1
@positive:
        lda     #0
        pha                     ; sentinel
@div_loop:
        jsr     div16_by10      ; quotient→tmp0, remainder→scratch
        lda     scratch
        clc
        adc     #'0'
        pha
        lda     tmp0
        ora     tmp0+1
        bne     @div_loop
@print_loop:
        pla
        beq     @done2          ; sentinel
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        bra     @print_loop
@done2: rts

; ---------------------------------------------------------------------------
; div16_by10 — divide tmp0:tmp0+1 by 10
; Result: quotient in tmp0:tmp0+1, remainder in scratch
; ---------------------------------------------------------------------------
div16_by10:
        lda     #0
        sta     scratch
        ldx     #16
@bit:   asl     tmp0
        rol     tmp0+1
        rol     scratch
        lda     scratch
        cmp     #10
        bcc     @no_sub
        sbc     #10
        sta     scratch
        inc     tmp0
@no_sub:
        dex
        bne     @bit
        rts

; ---------------------------------------------------------------------------
; console_read_line — read a line into buffer at tmp0, max len in x_save
; Returns: Y = length (not including CR)
; ---------------------------------------------------------------------------
console_read_line:
        stx     x_save
        ldy     #0
@loop:  ldx     #PEM_CONIN
        jsr     PEM_ENTRY
        cmp     #$0D
        beq     @done
        cmp     #$08            ; backspace
        bne     @store
        cpy     #0
        beq     @loop
        dey
        lda     #$08
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        lda     #' '
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        lda     #$08
        ldx     #PEM_CONOUT
        jsr     PEM_ENTRY
        bra     @loop
@store: sta     (tmp0),y
        iny
        cpy     x_save
        bcc     @loop
@done:  rts
x_save: .byte   0

; ---------------------------------------------------------------------------
; file_open — open file via FCB at tmp1; returns A=0 success, $FF=not found
; PEM calling convention: X=function, A=FCB addr lo, Y=FCB addr hi
; ---------------------------------------------------------------------------
file_open:
        lda     tmp1
        ldy     tmp1+1
        ldx     #PEM_OPEN
        jsr     PEM_ENTRY
        rts

; ---------------------------------------------------------------------------
; file_make — create/truncate file via FCB at tmp1
; ---------------------------------------------------------------------------
file_make:
        lda     tmp1
        ldy     tmp1+1
        ldx     #PEM_MAKE
        jsr     PEM_ENTRY
        rts

; ---------------------------------------------------------------------------
; file_read_sector — read 128 bytes into DMA_BUF; returns A=0 ok, A=1 EOF
; ---------------------------------------------------------------------------
file_read_sector:
        lda     tmp1
        ldy     tmp1+1
        ldx     #PEM_READ
        jsr     PEM_ENTRY
        rts

; ---------------------------------------------------------------------------
; file_write_sector — write 128 bytes from DMA_BUF
; ---------------------------------------------------------------------------
file_write_sector:
        lda     tmp1
        ldy     tmp1+1
        ldx     #PEM_WRITE
        jsr     PEM_ENTRY
        rts

; ---------------------------------------------------------------------------
; file_close — close FCB at tmp1
; ---------------------------------------------------------------------------
file_close:
        lda     tmp1
        ldy     tmp1+1
        ldx     #PEM_CLOSE
        jsr     PEM_ENTRY
        rts
