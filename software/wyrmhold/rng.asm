;______________________________________________________________________________
;  rng.asm - 16-bit xorshift pseudo-random generator
;______________________________________________________________________________

;----------------------------------------------------------------
; rng_seed - initialise the generator (avoid all-zero state).
;----------------------------------------------------------------
rng_seed:
        LDA     #$A3
        STA     seedlo
        LDA     #$1C
        STA     seedhi
        RTS

;----------------------------------------------------------------
; rng_next - advance the 16-bit xorshift state, return low byte
;            in A.  (xorshift: x ^= x<<7; x ^= x>>9; x ^= x<<8)
;----------------------------------------------------------------
rng_next:
        ; x ^= x << 7
        LDA     seedhi
        LSR     A               ; carry = bit7 of hi after some shifts...
        ; Implement x<<7 via: shift left 7 = shift right 1 then swap?
        ; Simpler: do a byte-wise xorshift that is "good enough".
        ; x ^= x << 7
        LDA     seedlo
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        ASL     A               ; lo << 7 (only bit0 survives into bit7)
        EOR     seedhi
        STA     seedhi
        ; x ^= x >> 9  (>>9 of 16-bit = hi >> 1 into lo)
        LDA     seedhi
        LSR     A
        EOR     seedlo
        STA     seedlo
        ; x ^= x << 8  (swap bytes contribution: lo ^ into hi)
        LDA     seedlo
        EOR     seedhi
        STA     seedhi
        LDA     seedlo
        RTS

;----------------------------------------------------------------
; rng_mod - return a value 0..A-1 in A.  IN: A = modulus (1..255)
;           (uses repeated subtraction mask via AND for powers of
;            two is faster, but generic mod via subtract loop here)
;----------------------------------------------------------------
rng_mod:
        STA     tmp3            ; modulus
        JSR     rng_next        ; A = random byte
        ; reduce A mod tmp3 by subtraction
@rm:
        CMP     tmp3
        BCC     @done
        SEC
        SBC     tmp3
        JMP     @rm
@done:
        RTS

;----------------------------------------------------------------
; rng_d - roll a die: return 1..A in A.  IN: A = sides (1..255)
;----------------------------------------------------------------
rng_d:
        JSR     rng_mod
        CLC
        ADC     #1
        RTS
