.PC02
; symtab.asm — symbol table for Pascal/65 compiler
;
; Each entry occupies 16 bytes in the SYMTAB_BASE area:
;   [0]    name length (1-15)
;   [1-15] name chars (uppercase, padded with spaces)
;   [16]   symbol kind: SYM_VAR, SYM_CONST, SYM_PROC, SYM_FUNC, SYM_TYPE, SYM_PARAM
;   [17]   data type:   TY_INT, TY_CHAR, TY_BOOL, TY_STRING, TY_ARRAY, TY_RECORD, TY_PTR, TY_TEXT, TY_SET, TY_REAL
;   [18-19] value/offset (word): variable offset from BASE/MP; const value; proc address
;   [20]   scope depth (0=global)
;   [21]   parameter count (for SYM_PROC/SYM_FUNC)
;   [22-23] type info pointer (for array/record types)
;   [24-31] formal parameter types 0..7 (for SYM_PROC/SYM_FUNC)

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
TY_TEXT         = 8             ; TEXT file variable (168-byte struct)
TY_SET          = 9             ; 16-bit set mask over element values 0..15
TY_REAL         = 10            ; signed fixed-point REAL (scale 100, 2 decimal places)
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
        STA     field_table_count
        STA     field_depth
        STA     ptr_meta_count
        RTS

; ---------------------------------------------------------------------------
; field_table_add — append a record-field entry
; Inputs:
;   ident_buf = field name (length at [0], chars at [1..])
;   A         = byte offset within the record
;   X         = field data type (TY_*)
; Returns: A = newly-assigned field index (current count - 1)
;          carry set if field table is full (entry not added)
; Clobbers: tmp2, tmp3, Y
; ---------------------------------------------------------------------------
field_table_add:
        PHA                     ; save offset
        TXA
        PHA                     ; save type
; capacity check (max 32 entries)
        LDA     field_table_count
        CMP     #32
        BCC     :+
        PLA                     ; discard type
        PLA                     ; discard offset
        SEC
        RTS
:
; compute entry address: field_table + count*16
        LDA     field_table_count
        ASL
        ASL
        ASL
        ASL                     ; *16 (count<64 fits in 10 bits, but we land in low byte; high carry becomes part of address)
        STA     tmp2
        LDA     #0
        ROL                     ; capture carry into hi
        CLC
        ADC     #>field_table
        STA     tmp2+1
        LDA     tmp2
        CLC
        ADC     #<field_table
        STA     tmp2
        BCC     :+
        INC     tmp2+1
:
; copy name length + chars
        LDY     #0
        LDA     ident_buf
        STA     (tmp2),y
        TAX                     ; X = length
        LDY     #1
@nm:
        CPY     #13
        BCS     @padn
        CPX     #0
        BEQ     @padn
        LDA     ident_buf,y
        STA     (tmp2),y
        DEX
        INY
        BRA     @nm
@padn:
        CPY     #13
        BCS     @done_nm
        LDA     #' '
        STA     (tmp2),y
        INY
        BRA     @padn
@done_nm:
; offset (was on stack first → second pull)
; type   (was on stack last  → first pull)
        PLA                     ; type
        LDY     #14
        STA     (tmp2),y
        PLA                     ; offset
        LDY     #13
        STA     (tmp2),y
        LDA     field_depth     ; nesting depth so scans can skip
        LDY     #15             ; fields owned by deeper inline records
        STA     (tmp2),y
; bump count, return new index
        LDA     field_table_count
        INC     field_table_count
        CLC
        RTS

; ---------------------------------------------------------------------------
; field_lookup_in_record — find ident_buf among a contiguous range of fields
; Inputs:
;   ident_buf = field name to find
;   A         = first field index in field_table
;   X         = number of fields to scan
; Returns: carry set = found
;            A = byte offset within record
;            X = field data type
;          carry clear = not found
; Clobbers: tmp2, tmp3, Y
; ---------------------------------------------------------------------------
field_lookup_in_record:
        STA     tmp3            ; tmp3 = current field index
        STX     tmp3+1          ; tmp3+1 = remaining count
; Read the depth of field[tmp3] — the scan's "target depth". Slots
; deeper than this belong to inline sub-records and are skipped so
; the scan stays at the record's own nesting level.
        LDA     tmp3
        ASL
        ASL
        ASL
        ASL
        STA     tmp2
        LDA     #0
        ROL
        CLC
        ADC     #>field_table
        STA     tmp2+1
        LDA     tmp2
        CLC
        ADC     #<field_table
        STA     tmp2
        BCC     :+
        INC     tmp2+1
:
        LDY     #15
        LDA     (tmp2),y
        STA     field_lookup_depth
@scan:
        LDA     tmp3+1
        BEQ     @nf
; compute entry addr: field_table + tmp3*16
        LDA     tmp3
        ASL
        ASL
        ASL
        ASL                     ; *16 (low byte; carry → hi)
        STA     tmp2
        LDA     #0
        ROL
        CLC
        ADC     #>field_table
        STA     tmp2+1
        LDA     tmp2
        CLC
        ADC     #<field_table
        STA     tmp2
        BCC     :+
        INC     tmp2+1
:
; skip slots whose depth differs from the scan's target depth
        LDY     #15
        LDA     (tmp2),y
        CMP     field_lookup_depth
        BNE     @next
; compare name length
        LDY     #0
        LDA     (tmp2),y
        CMP     ident_buf
        BNE     @next
        TAX                     ; X = length
        LDY     #1
@cmpn:
        LDA     (tmp2),y
        CMP     ident_buf,y
        BNE     @next
        INY
        DEX
        BNE     @cmpn
; match — return offset and type
        LDY     #14
        LDA     (tmp2),y        ; type
        TAX
        LDY     #13
        LDA     (tmp2),y        ; offset
        SEC
        RTS
@next:
        INC     tmp3
        DEC     tmp3+1
        BRA     @scan
@nf:
        CLC
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
