; hello.asm — hand-assembled .PCD test: prints "HELLO" then newline
;
; Equivalent Pascal:
;   PROGRAM hello;
;   BEGIN
;     WRITE('H'); WRITE('E'); WRITE('L'); WRITE('L'); WRITE('O');
;     WRITELN;
;   END.
;
; Build: ca65 hello.asm && ld65 hello.o -C pcd.cfg -o hello.pcd
;
; Opcodes used:
;   $02 nn   LDCC — push char constant nn
;   $81      WRITC — pop char, write to console
;   $84      WRITLN — write CR+LF
;   $FF      HALT — return to DOS/65

.segment "DATA"

; ---- PCD header (12 bytes) ----
.byte $50, $43              ; magic 'PC'
.byte $01, $00              ; version 1.0
.word (pcd_end - pcd_code)  ; code size (calculated by assembler)
.word $0000                 ; globals size
.word $0000                 ; string pool size
.word $0000                 ; entry point offset into code

; ---- P-code ----
pcd_code:
        .byte $02, 'H'      ; LDCC 'H'
        .byte $81            ; WRITC
        .byte $02, 'E'      ; LDCC 'E'
        .byte $81            ; WRITC
        .byte $02, 'L'      ; LDCC 'L'
        .byte $81            ; WRITC
        .byte $02, 'L'      ; LDCC 'L'
        .byte $81            ; WRITC
        .byte $02, 'O'      ; LDCC 'O'
        .byte $81            ; WRITC
        .byte $84            ; WRITLN
        .byte $FF            ; HALT
pcd_end:
