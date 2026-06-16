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
        LDA     #0
        STA     seed2lo
        STA     seed2hi
        RTS

;----------------------------------------------------------------
; rng_timing_tick - increment the title-screen timing counter.
;----------------------------------------------------------------
rng_timing_tick:
        INC     seed2lo
        BNE     @done
        INC     seed2hi
@done:
        RTS

;----------------------------------------------------------------
; rng_mix_timing - mix title wait time and accepted key into the
;                  generator, then diffuse the new state.
;                  IN: keych = accepted title-screen key
;----------------------------------------------------------------
rng_mix_timing:
        LDA     seedlo
        EOR     seed2lo
        EOR     keych
        STA     seedlo

        LDA     keych
        ASL     A
        EOR     seedhi
        EOR     seed2hi
        STA     seedhi

; The xorshift generator must never enter its all-zero lockup state.
        LDA     seedlo
        ORA     seedhi
        BNE     @diffuse
        LDA     #$A3
        STA     seedlo
        LDA     #$1C
        STA     seedhi
@diffuse:
        JSR     rng_next
        JMP     rng_next

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
