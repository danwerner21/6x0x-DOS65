.pc02
; iolib.asm — console and file I/O wrappers for DOS/65
;
; Uses PEM function 2 (CONOUT) for character output.
; This routes through DFT_CONSOLE and works on all platforms
; (serial, ESP video, memory-mapped video) regardless of which
; console driver is active.

        .segment        "CODE"

; ---------------------------------------------------------------------------
; console_putc — output character in A to console via PEM #2
; ---------------------------------------------------------------------------
console_putc:
        PHA
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        PLA
        RTS

; ---------------------------------------------------------------------------
; console_newline — emit CR+LF
; ---------------------------------------------------------------------------
console_newline:
        LDA     #$0D
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        LDA     #$0A
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        RTS

; ---------------------------------------------------------------------------
; console_print_sz — print null-terminated string at tmp0
; ---------------------------------------------------------------------------
console_print_sz:
        LDY     #0
@loop:
        LDA     (tmp0),y
        BEQ     @done
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        INY
        BNE     @loop
@done:
        RTS

; ---------------------------------------------------------------------------
; console_print_pstr — print length-prefixed Pascal string at tmp0
; ---------------------------------------------------------------------------
console_print_pstr:
        LDY     #0
        LDA     (tmp0),y        ; length byte
        BEQ     @done
        TAX                     ; X = remaining chars — but X used by PEM!
; save length, use tmp1 as counter
        STA     tmp1
        INY
@loop:
        LDA     (tmp0),y
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        INY
        DEC     tmp1
        BNE     @loop
@done:
        RTS

; ---------------------------------------------------------------------------
; console_print_dec — print signed 16-bit integer in tmp0:tmp0+1
; ---------------------------------------------------------------------------
console_print_dec:
        LDA     tmp0+1
        BPL     @positive
        LDA     #'-'
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        LDA     #0
        SEC
        SBC     tmp0
        STA     tmp0
        LDA     #0
        SBC     tmp0+1
        STA     tmp0+1
@positive:
        LDA     #0
        PHA                     ; sentinel
@div_loop:
        JSR     div16_by10      ; quotient→tmp0, remainder→scratch
        LDA     scratch
        CLC
        ADC     #'0'
        PHA
        LDA     tmp0
        ORA     tmp0+1
        BNE     @div_loop
@print_loop:
        PLA
        BEQ     @done2          ; sentinel
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        BRA     @print_loop
@done2:
        RTS

; ---------------------------------------------------------------------------
; div16_by10 — divide tmp0:tmp0+1 by 10
; Result: quotient in tmp0:tmp0+1, remainder in scratch
; ---------------------------------------------------------------------------
div16_by10:
        LDA     #0
        STA     scratch
        LDX     #16
@bit:
        ASL     tmp0
        ROL     tmp0+1
        ROL     scratch
        LDA     scratch
        CMP     #10
        BCC     @no_sub
        SBC     #10
        STA     scratch
        INC     tmp0
@no_sub:
        DEX
        BNE     @bit
        RTS

; ---------------------------------------------------------------------------
; console_read_line — read a line into buffer at tmp0, max len in x_save
; Returns: Y = length (not including CR)
; ---------------------------------------------------------------------------
console_read_line:
        STX     x_save
        LDY     #0
@loop:
        LDX     #PEM_CONIN
        JSR     PEM_ENTRY
        CMP     #$0D
        BEQ     @done
        CMP     #$08            ; backspace
        BNE     @store
        CPY     #0
        BEQ     @loop
        DEY
        LDA     #$08
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        LDA     #' '
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        LDA     #$08
        LDX     #PEM_CONOUT
        JSR     PEM_ENTRY
        BRA     @loop
@store:
        STA     (tmp0),y
        INY
        CPY     x_save
        BCC     @loop
@done:
        RTS
x_save:
        .BYTE   0

; ---------------------------------------------------------------------------
; file_open — open file via FCB at tmp1; returns A=0 success, $FF=not found
; PEM calling convention: X=function, A=FCB addr lo, Y=FCB addr hi
; ---------------------------------------------------------------------------
file_open:
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_OPEN
        JSR     PEM_ENTRY
        RTS

; ---------------------------------------------------------------------------
; file_make — create/truncate file via FCB at tmp1
; ---------------------------------------------------------------------------
file_make:
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_MAKE
        JSR     PEM_ENTRY
        RTS

; ---------------------------------------------------------------------------
; file_read_sector — read 128 bytes into DMA_BUF; returns A=0 ok, A=1 EOF
; ---------------------------------------------------------------------------
file_read_sector:
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_READ
        JSR     PEM_ENTRY
        RTS

; ---------------------------------------------------------------------------
; file_write_sector — write 128 bytes from DMA_BUF
; ---------------------------------------------------------------------------
file_write_sector:
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_WRITE
        JSR     PEM_ENTRY
        RTS

; ---------------------------------------------------------------------------
; file_close — close FCB at tmp1
; ---------------------------------------------------------------------------
file_close:
        LDA     tmp1
        LDY     tmp1+1
        LDX     #PEM_CLOSE
        JSR     PEM_ENTRY
        RTS

; ---------------------------------------------------------------------------
; file_setdma — point PEM at the 128-byte buffer at (A:Y)
; A = lo, Y = hi.  Trashes X.
; ---------------------------------------------------------------------------
file_setdma:
        LDX     #PEM_SETDMA
        JMP     PEM_ENTRY

; ---------------------------------------------------------------------------
; file_setdma_default — restore PEM DMA pointer to system DMA_BUF
; ---------------------------------------------------------------------------
file_setdma_default:
        LDA     #<DMA_BUF
        LDY     #>DMA_BUF
        LDX     #PEM_SETDMA
        JMP     PEM_ENTRY
