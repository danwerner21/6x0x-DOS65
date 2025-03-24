; TITLE   "ZIP/6502-C INFOCOM, INC. --- EQUATES"
; --------------------------
; ZIP/6502 2.0 VERSION F
; Z-CODE INTERPRETER PROGRAM
; FOR DUODYNE DOS/65 65816
; --------------------------
; INFOCOM, INC.
; 55 WHEELER STREET
; CAMBRIDGE, MA 02136
; COMPANY PRIVATE -- NOT FOR DISTRIBUTION

MSTART          = $0800         ; START OF FREE PROGRAM RAM
ZEROPG          = $60           ; START OF FREE Z-PAGE RAM
ZPGTOP          = $DF           ; END OF FREE Z-PAGE RAM

;DEBUG           = 0             ; ASSEMBLY FLAG FOR DEBUGGER

; -----------
; ERROR CODES
; -----------
; 00 -- INSUFFICIENT RAM
; 01 -- ILLEGAL X-OP
; 02 -- ILLEGAL 0-OP
; 03 -- ILLEGAL 1-OP
; 04 -- ILLEGAL 2-OP
; 05 -- Z-STACK UNDERFLOW
; 06 -- Z-STACK OVERFLOW
; 07 -- ILLEGAL PROPERTY LENGTH (GETP)
; 08 -- DIVISION BY ZERO
; 09 -- ILLEGAL ARGUMENT COUNT (EQUAL?)
; 10 -- ILLEGAL PROPERTY ID (PUTP)
; 11 -- ILLEGAL PROPERTY LENGTH (PUTP)
; 12 -- DISK ADDRESS OUT OF RANGE
; 13 -- PARSER OVERFLOW
; 14 -- DRIVE ACCESS
; 15 -- Z-STACK DESTROYED

        .SEGMENT "TEA"
        .ORG    $0800
        JMP     COLD
        .INCLUDE "eq.asm"
        .SEGMENT "ZIP"
;        TITLE   "ZIP/6502-C INFOCOM, INC. --- MACHINE DEPENDENT INIT"
        .INCLUDE "hardeq.asm"
        .INCLUDE "cold.asm"

;        TITLE   "ZIP/6502-C INFOCOM, INC. --- INIT & MAINLINE"
        .INCLUDE "warm.asm"
        .INCLUDE "main.asm"
        .INCLUDE "subs.asm"
        .INCLUDE "dispatch.asm"

;        TITLE   "ZIP/6502-C INFOCOM, INC. --- OPCODE EXECUTORS"
        .INCLUDE "ops0.asm"
        .INCLUDE "ops1.asm"
        .INCLUDE "ops2.asm"
        .INCLUDE "opsx.asm"
        .INCLUDE "read.asm"

;        TITLE   "ZIP/6502-C INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT"
        .INCLUDE "paging.asm"
        .INCLUDE "zstring.asm"
        .INCLUDE "objects.asm"

;        TITLE   "ZIP/6502-C INFOCOM, INC. --- MACHINE DEPENDENT I/O"
        .INCLUDE "io.asm"
        .INCLUDE "machine.asm"
        .INCLUDE "zdos.asm"
        .INCLUDE "disk.asm"

        .IFDEF  DEBUG
        .INCLUDE "bugger.asm"
        .ENDIF

;        TITLE   "ZIP/6502-C INFOCOM, INC."
        .END
