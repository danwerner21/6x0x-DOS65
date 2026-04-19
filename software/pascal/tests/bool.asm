; bool.asm — hand-assembled .PCD test: comparisons, branches, booleans
;
; Equivalent Pascal:
;   PROGRAM bool;
;   BEGIN
;     IF 3 < 5 THEN WRITELN(1) ELSE WRITELN(0);   { should print 1 }
;     IF 7 < 2 THEN WRITELN(1) ELSE WRITELN(0);   { should print 0 }
;     WRITELN(TRUE);                                { should print TRUE  }
;     WRITELN(FALSE);                               { should print FALSE }
;   END.
;
; Build: ca65 bool.asm && ld65 bool.o -C pcd.cfg -o bool.pcd
;
; Opcodes used:
;   $00 nn   LDCI
;   $03 nn   LDCB — push boolean (0=FALSE, 1=TRUE)
;   $42      LESI — less-than comparison (push $FFFF or $0000)
;   $51 lo hi FJP  — jump if false (signed IPC-relative offset, applied AFTER opcode+operand)
;   $50 lo hi UJP  — unconditional jump
;   $80      WRITI
;   $82      WRITB — pop boolean, print TRUE or FALSE
;   $84      WRITLN
;   $FF      HALT
;
; Jump offset = target_address - (address_after_operand)
; i.e. offset is relative to the byte FOLLOWING the two operand bytes.

.segment "DATA"

; ---- PCD header ----
.byte $50, $43
.byte $01, $00
.word (pcd_end - pcd_code)
.word $0000
.word $0000
.word $0000

; ---- P-code ----
pcd_code:

; --- test 1: IF 3 < 5 THEN WRITELN(1) ELSE WRITELN(0) ---
        .byte $00, 3                    ; LDCI 3
        .byte $00, 5                    ; LDCI 5
        .byte $42                        ; LESI  (3<5 = TRUE = $FFFF)
        .byte $51                        ; FJP — jump to else if false
        .byte <(t1_else - t1_fjp_end)   ; offset lo
        .byte >(t1_else - t1_fjp_end)   ; offset hi
t1_fjp_end:
        .byte $00, 1                    ; LDCI 1  (THEN branch)
        .byte $80                        ; WRITI
        .byte $84                        ; WRITLN
        .byte $50                        ; UJP — skip else
        .byte <(t1_end - t1_ujp_end)
        .byte >(t1_end - t1_ujp_end)
t1_ujp_end:
t1_else:
        .byte $00, 0                    ; LDCI 0  (ELSE branch)
        .byte $80                        ; WRITI
        .byte $84                        ; WRITLN
t1_end:

; --- test 2: IF 7 < 2 THEN WRITELN(1) ELSE WRITELN(0) ---
        .byte $00, 7                    ; LDCI 7
        .byte $00, 2                    ; LDCI 2
        .byte $42                        ; LESI  (7<2 = FALSE = $0000)
        .byte $51
        .byte <(t2_else - t2_fjp_end)
        .byte >(t2_else - t2_fjp_end)
t2_fjp_end:
        .byte $00, 1                    ; THEN branch
        .byte $80
        .byte $84
        .byte $50
        .byte <(t2_end - t2_ujp_end)
        .byte >(t2_end - t2_ujp_end)
t2_ujp_end:
t2_else:
        .byte $00, 0                    ; ELSE branch
        .byte $80
        .byte $84
t2_end:

; --- test 3: WRITELN(TRUE) ---
        .byte $03, 1                    ; LDCB TRUE
        .byte $82                        ; WRITB
        .byte $84                        ; WRITLN

; --- test 4: WRITELN(FALSE) ---
        .byte $03, 0                    ; LDCB FALSE
        .byte $82                        ; WRITB
        .byte $84                        ; WRITLN

        .byte $FF                        ; HALT
pcd_end:
