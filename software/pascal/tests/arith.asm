; arith.asm — hand-assembled .PCD test: arithmetic and integer output
;
; Equivalent Pascal:
;   PROGRAM arith;
;   BEGIN
;     WRITELN(10 + 20);      { should print 30 }
;     WRITELN(100 - 37);     { should print 63 }
;     WRITELN(-(5));         { should print -5  }
;   END.
;
; Build: ca65 arith.asm && ld65 arith.o -C pcd.cfg -o arith.pcd
;
; Opcodes used:
;   $00 nn   LDCI — push sign-extended byte constant
;   $01 lo hi LDCW — push 16-bit word constant
;   $30      ADI  — integer add (pop b, pop a, push a+b)
;   $31      SBI  — integer subtract
;   $35      NGI  — negate TOS
;   $80      WRITI — pop integer, print decimal
;   $84      WRITLN — write CR+LF
;   $FF      HALT

.segment "DATA"

; ---- PCD header (12 bytes) ----
.byte $50, $43
.byte $01, $00
.word (pcd_end - pcd_code)
.word $0000
.word $0000
.word $0000

; ---- P-code ----
pcd_code:
        ; WRITELN(10 + 20)  → should print 30
        .byte $00, 10        ; LDCI 10
        .byte $00, 20        ; LDCI 20
        .byte $30            ; ADI  (stack: [30])
        .byte $80            ; WRITI
        .byte $84            ; WRITLN

        ; WRITELN(100 - 37) → should print 63
        .byte $00, 100       ; LDCI 100
        .byte $00, 37        ; LDCI 37
        .byte $31            ; SBI  (stack: [63])
        .byte $80            ; WRITI
        .byte $84            ; WRITLN

        ; WRITELN(-(5))     → should print -5
        .byte $00, 5         ; LDCI 5
        .byte $35            ; NGI  (stack: [-5])
        .byte $80            ; WRITI
        .byte $84            ; WRITLN

        ; WRITELN(1000)     → tests LDCW (word constant)
        .byte $01, $E8, $03  ; LDCW 1000 (lo=$E8, hi=$03)
        .byte $80            ; WRITI
        .byte $84            ; WRITLN

        .byte $FF            ; HALT
pcd_end:
