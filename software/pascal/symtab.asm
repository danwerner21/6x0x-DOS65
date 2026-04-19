.PC02
; symtab.asm — symbol table for Pascal/65 compiler
;
; Each entry occupies 16 bytes in the SYMTAB_BASE area:
;   [0]    name length (1-15)
;   [1-15] name chars (uppercase, padded with spaces)
;   [16]   symbol kind: SYM_VAR, SYM_CONST, SYM_PROC, SYM_FUNC, SYM_TYPE, SYM_PARAM
;   [17]   data type:   TY_INT, TY_CHAR, TY_BOOL, TY_STRING, TY_ARRAY, TY_RECORD, TY_PTR
;   [18-19] value/offset (word): variable offset from BASE/MP; const value; proc address
;   [20]   scope depth (0=global)
;   [21]   parameter count (for SYM_PROC/SYM_FUNC)
;   [22-23] type info pointer (for array/record types)
;   [24-31] reserved

SYM_ENTRY_SZ    = 32

; Symbol kinds
SYM_VAR         = 1
SYM_CONST       = 2
SYM_PROC        = 3
SYM_FUNC        = 4
SYM_TYPE        = 5
SYM_PARAM       = 6

; Data type codes
TY_INT          = 1
TY_CHAR         = 2
TY_BOOL         = 3
TY_STRING       = 4
TY_ARRAY        = 5
TY_RECORD       = 6
TY_PTR          = 7
TY_NONE         = 0             ; for procedures

.segment "CODE"

; Number of entries currently in symbol table
symtab_count:   .word 0

; ---------------------------------------------------------------------------
; symtab_init — clear symbol table
; ---------------------------------------------------------------------------
symtab_init:
        lda     #0
        sta     symtab_count
        sta     symtab_count+1
        rts

; ---------------------------------------------------------------------------
; symtab_find — look up ident_buf in symbol table
; Returns: carry set = found (tmp3 = pointer to entry)
;          carry clear = not found
; Searches from newest (highest index) to oldest for proper scoping
; ---------------------------------------------------------------------------
symtab_find:
        lda     symtab_count
        ora     symtab_count+1
        bne     :+
        clc
        rts                     ; empty table
:
        ; start from last entry
        lda     symtab_count
        sta     tmp2
        lda     symtab_count+1
        sta     tmp2+1
@loop:
        lda     tmp2
        ora     tmp2+1
        beq     @not_found
        ; decrement tmp2
        lda     tmp2
        bne     :+
        dec     tmp2+1
:       dec     tmp2
        ; compute entry address: SYMTAB_BASE + tmp2 * SYM_ENTRY_SZ
        ; tmp2 * 32 = tmp2 << 5
        lda     tmp2
        asl
        asl
        asl
        asl
        asl                     ; *32
        clc
        adc     #<SYMTAB_BASE
        sta     tmp3
        lda     tmp2+1
        rol
        adc     #>SYMTAB_BASE
        sta     tmp3+1
        ; compare name length
        ldy     #0
        lda     (tmp3),y
        cmp     ident_buf
        bne     @loop
        ; compare name chars
        tax                     ; X = length
        ldy     #0
@cmp:   iny
        lda     (tmp3),y
        cmp     ident_buf,y
        bne     @loop
        dex
        bne     @cmp
        sec
        rts
@not_found:
        clc
        rts

; ---------------------------------------------------------------------------
; symtab_add — add entry to symbol table
; Inputs:
;   ident_buf = name
;   A = kind (SYM_*)
;   X = data type (TY_*)
;   tmp2 = value/offset word (lo in tmp2, hi in tmp2+1)
; Returns: tmp3 = pointer to new entry; carry set = table full
; ---------------------------------------------------------------------------
symtab_add:
        pha                     ; save kind (A) — the rest of this routine clobbers A
        txa
        pha                     ; save data type (X)
        ; check capacity
        lda     symtab_count+1
        cmp     #(SYMTAB_MAXSZ / SYM_ENTRY_SZ) >> 8
        bcc     :+
        lda     symtab_count
        cmp     #(SYMTAB_MAXSZ / SYM_ENTRY_SZ) & $FF
        bcc     :+
        pla                     ; discard saved type
        pla                     ; discard saved kind
        sec
        rts                     ; table full

:       ; compute address of new entry
        lda     symtab_count
        asl
        asl
        asl
        asl
        asl                     ; *32
        clc
        adc     #<SYMTAB_BASE
        sta     tmp3
        lda     symtab_count+1
        rol
        adc     #>SYMTAB_BASE
        sta     tmp3+1

        ; copy name
        ldy     #0
        lda     ident_buf       ; length
        sta     (tmp3),y
        tax
@name_copy:
        iny
        lda     ident_buf,y
        sta     (tmp3),y
        dex
        bne     @name_copy

        ; fill unused name bytes with space
        iny
@pad:   cpy     #16
        bcs     @done_pad
        lda     #' '
        sta     (tmp3),y
        iny
        bra     @pad
@done_pad:

        ; data type (was saved on stack first)
        pla
        ldy     #17
        sta     (tmp3),y

        ; kind (was saved on stack second-to-last → on top first)
        pla
        ldy     #16
        sta     (tmp3),y

        ; value/offset
        lda     tmp2
        ldy     #18
        sta     (tmp3),y
        lda     tmp2+1
        ldy     #19
        sta     (tmp3),y

        ; scope depth
        lda     scope_depth
        ldy     #20
        sta     (tmp3),y

        ; zero rest
        lda     #0
        ldy     #21
@zero:  sta     (tmp3),y
        iny
        cpy     #SYM_ENTRY_SZ
        bcc     @zero

        ; increment count
        inc     symtab_count
        bne     :+
        inc     symtab_count+1
:       clc
        rts

; ---------------------------------------------------------------------------
; symtab_enter_scope — increment scope depth
; ---------------------------------------------------------------------------
symtab_enter_scope:
        inc     scope_depth
        rts

; ---------------------------------------------------------------------------
; symtab_leave_scope — remove all entries at current scope, decrement depth
; ---------------------------------------------------------------------------
symtab_leave_scope:
        ; Remove all entries added at current scope depth, then decrement depth.
        ; Entries are appended in order, so trim from the end.
@trim:  lda     symtab_count
        ora     symtab_count+1
        beq     @done           ; table empty
        ; compute address of last entry: SYMTAB_BASE + (count-1)*32
        lda     symtab_count
        sec
        sbc     #1
        sta     tmp2
        lda     symtab_count+1
        sbc     #0
        sta     tmp2+1
        ; tmp2 * 32 (shift left 5) → entry address
        lda     tmp2
        asl
        asl
        asl
        asl
        asl
        clc
        adc     #<SYMTAB_BASE
        sta     tmp3
        lda     tmp2+1
        rol
        adc     #>SYMTAB_BASE
        sta     tmp3+1
        ; check scope depth field (offset 20 in entry)
        ldy     #20
        lda     (tmp3),y
        cmp     scope_depth
        bne     @done           ; entry belongs to an outer scope — stop
        ; remove this entry
        dec     symtab_count
        bne     @trim
        dec     symtab_count+1
        bra     @trim
@done:
        dec     scope_depth
        rts
