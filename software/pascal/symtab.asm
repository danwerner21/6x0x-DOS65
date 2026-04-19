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
SYM_RETVAL      = 7             ; binding for `funcname := expr` in fn body
SYM_VARREF      = 8             ; VAR (by-reference) parameter — local slot holds an address

; Data type codes
TY_INT          = 1
TY_CHAR         = 2
TY_BOOL         = 3
TY_STRING       = 4
TY_ARRAY        = 5
TY_RECORD       = 6
TY_PTR          = 7
TY_NONE         = 0             ; for procedures

        .segment        "CODE"

; Number of entries currently in symbol table
symtab_count:
        .WORD   0

; ---------------------------------------------------------------------------
; symtab_init — clear symbol table
; ---------------------------------------------------------------------------
symtab_init:
        LDA     #0
        STA     symtab_count
        STA     symtab_count+1
        RTS

; ---------------------------------------------------------------------------
; symtab_find — look up ident_buf in symbol table
; Returns: carry set = found (tmp3 = pointer to entry)
;          carry clear = not found
; Searches from newest (highest index) to oldest for proper scoping
; ---------------------------------------------------------------------------
symtab_find:
        LDA     symtab_count
        ORA     symtab_count+1
        BNE     :+
        CLC
        RTS                     ; empty table
:
; start from last entry
        LDA     symtab_count
        STA     tmp2
        LDA     symtab_count+1
        STA     tmp2+1
@loop:
        LDA     tmp2
        ORA     tmp2+1
        BEQ     @not_found
; decrement tmp2
        LDA     tmp2
        BNE     :+
        DEC     tmp2+1
:
        DEC     tmp2
; compute entry address: SYMTAB_BASE + tmp2 * SYM_ENTRY_SZ
; tmp2 * 32 = tmp2 << 5
        LDA     tmp2
        ASL
        ASL
        ASL
        ASL
        ASL                     ; *32
        CLC
        ADC     #<SYMTAB_BASE
        STA     tmp3
        LDA     tmp2+1
        ROL
        ADC     #>SYMTAB_BASE
        STA     tmp3+1
; compare name length
        LDY     #0
        LDA     (tmp3),y
        CMP     ident_buf
        BNE     @loop
; compare name chars
        TAX                     ; X = length
        LDY     #0
@cmp:
        INY
        LDA     (tmp3),y
        CMP     ident_buf,y
        BNE     @loop
        DEX
        BNE     @cmp
        SEC
        RTS
@not_found:
        CLC
        RTS

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
        PHA                     ; save kind (A) — the rest of this routine clobbers A
        TXA
        PHA                     ; save data type (X)
; check capacity
        LDA     symtab_count+1
        CMP     #(SYMTAB_MAXSZ / SYM_ENTRY_SZ) >> 8
        BCC     :+
        LDA     symtab_count
        CMP     #(SYMTAB_MAXSZ / SYM_ENTRY_SZ) & $FF
        BCC     :+
        PLA                     ; discard saved type
        PLA                     ; discard saved kind
        SEC
        RTS                     ; table full

:       ; compute address of new entry
        LDA     symtab_count
        ASL
        ASL
        ASL
        ASL
        ASL                     ; *32
        CLC
        ADC     #<SYMTAB_BASE
        STA     tmp3
        LDA     symtab_count+1
        ROL
        ADC     #>SYMTAB_BASE
        STA     tmp3+1

; copy name
        LDY     #0
        LDA     ident_buf       ; length
        STA     (tmp3),y
        TAX
@name_copy:
        INY
        LDA     ident_buf,y
        STA     (tmp3),y
        DEX
        BNE     @name_copy

; fill unused name bytes with space
        INY
@pad:
        CPY     #16
        BCS     @done_pad
        LDA     #' '
        STA     (tmp3),y
        INY
        BRA     @pad
@done_pad:

; data type (was saved on stack first)
        PLA
        LDY     #17
        STA     (tmp3),y

; kind (was saved on stack second-to-last → on top first)
        PLA
        LDY     #16
        STA     (tmp3),y

; value/offset
        LDA     tmp2
        LDY     #18
        STA     (tmp3),y
        LDA     tmp2+1
        LDY     #19
        STA     (tmp3),y

; scope depth
        LDA     scope_depth
        LDY     #20
        STA     (tmp3),y

; zero rest
        LDA     #0
        LDY     #21
@zero:
        STA     (tmp3),y
        INY
        CPY     #SYM_ENTRY_SZ
        BCC     @zero

; increment count
        INC     symtab_count
        BNE     :+
        INC     symtab_count+1
:
        CLC
        RTS

; ---------------------------------------------------------------------------
; symtab_enter_scope — increment scope depth
; ---------------------------------------------------------------------------
symtab_enter_scope:
        INC     scope_depth
        RTS

; ---------------------------------------------------------------------------
; symtab_leave_scope — remove all entries at current scope, decrement depth
; ---------------------------------------------------------------------------
symtab_leave_scope:
; Remove all entries added at current scope depth, then decrement depth.
; Entries are appended in order, so trim from the end.
@trim:
        LDA     symtab_count
        ORA     symtab_count+1
        BEQ     @done           ; table empty
; compute address of last entry: SYMTAB_BASE + (count-1)*32
        LDA     symtab_count
        SEC
        SBC     #1
        STA     tmp2
        LDA     symtab_count+1
        SBC     #0
        STA     tmp2+1
; tmp2 * 32 (shift left 5) → entry address
        LDA     tmp2
        ASL
        ASL
        ASL
        ASL
        ASL
        CLC
        ADC     #<SYMTAB_BASE
        STA     tmp3
        LDA     tmp2+1
        ROL
        ADC     #>SYMTAB_BASE
        STA     tmp3+1
; check scope depth field (offset 20 in entry)
        LDY     #20
        LDA     (tmp3),y
        CMP     scope_depth
        BNE     @done           ; entry belongs to an outer scope — stop
; remove this entry
        DEC     symtab_count
        BNE     @trim
        DEC     symtab_count+1
        BRA     @trim
@done:
        DEC     scope_depth
        RTS
