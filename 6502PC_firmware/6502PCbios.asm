
;__6502PCbios________________________________________________________________________________________
;
;	BIOS for the 6502 PC ATX SBC
;
;	ORIGINALLY WRITTEN BY: DAN WERNER -- 1/1/2014
; 	Code cleanup: Dan Werner -- 1/22/2023
;       Port to 6502PC: Dan Werner -- 12/6/2025
;
; ** NOTE THAT THIS BIOS NEEDS PAGED MEMORY TO OPERATE

        .INCLUDE "bios_defines.asm"
        .INCLUDE "../dos65_os/dosdefn.asm"; base addresses and definitions

STARTOS         = $B800

        .SEGMENT "TROM"
        .ORG    $F000


;__COLD_START___________________________________________________
;
; PERFORM SYSTEM COLD INIT
;
;_______________________________________________________________
COLD_START:
        SEI                     ; DISABLE INTERRUPTS
        CLD                     ;  VERIFY DECIMAL MODE IS OFF
        LDX     #$FF            ;
        TXS                     ; CLEAR STACK
        TXA
;;;

        LDA     #<IRQROUTINE
        STA     IRQVECTOR
        STA     NMIVECTOR
        LDA     #>IRQROUTINE
        STA     IRQVECTOR+1
        STA     NMIVECTOR+1
;
;       INIT PAGING
        JSR     INITPAGES       ;

;	INIT HARDWARE
        JSR     SERIALINIT

        JSR     PAGE_ENTER
        LDA     #<STARTUP       ; OUTPUT STARTUP STRING
        STA     STRPTR          ;
        LDA     #>STARTUP       ;
        STA     STRPTR+1        ;
        JSR     OUTSTR          ;
        JSR     PAGE_EXIT


        LDA     #$00            ;
        STA     INBUFFER        ; MAKE SURE INPUT BUFFER IS EMPTY

        BRK                     ; PERFORM BRK (START MONITOR)

;__IRQROUTINE___________________________________________________
;
; HANDLE INTERRUPT PROCESING
;
;_______________________________________________________________
IRQROUTINE:
        CLI                     ; ENABLE INTERRUPTS AGAIN
        RTI

;__INTERRUPT____________________________________________________
;
; HANDLE IRQ INTERRUPT AND DETERMINE IF IT IS A BRK OR AN IRQ
;
;_______________________________________________________________
INTERRUPT:
        SEI                     ; DISABLE INTERRUPTS
        STA     TEMPWORD
        PLA                     ; GET STATUS REGISTER
        PHA                     ; SAVE STATUS REGISTER
        AND     #$10            ; MASK BRK
        BNE     BRKCMD          ; BRK CMD
        JMP     (IRQVECTOR)     ; LET USER ROUTINE HAVE IT (USER DEFINED IRQ)
BRKCMD:
        LDA     TEMPWORD
        PHA
        TXA
        PHA
        TYA
        PHA
        JMP     BRKROUTINE      ; MONITOR BRK ROUTINE

NINTERRUPT:
        JMP     (NMIVECTOR)     ; LET USER ROUTINE HAVE IT (USER DEFINED NMI)

        .INCLUDE "../supermon/supermon.asm"
        .INCLUDE "bios_serial.asm"
        .INCLUDE "bios_pager.ASM"


;__OUTSTR______________________________________________________
;
; OUTPUT THE STRING POINTED TO BY OUTSTR TO THE SCREEN
;
;______________________________________________________________
OUTSTR:
        LDY     #$00            ; LOAD $00 INTO Y
OUTSTRLP:
        LDA     (STRPTR),Y      ; LOAD NEXT CHAR FROM STRING INTO ACC
        CMP     #$00            ; IS NULL?
        BEQ     ENDOUTSTR       ; YES, END PRINT OUT
        JSR     IOF_OUTCH       ; PRINT CHAR IN ACC
        INC     STRPTR
        BNE     OUTSTRLP
        INC     STRPTR+1
        JMP     OUTSTRLP        ; DO NEXT CHAR
ENDOUTSTR:
        RTS                     ; RETURN

DSKYMSG:
        .BYTE   $7C, $6, $3F, $6D, $0, $3E, $73, $0
STARTUP:
        .BYTE   $0D,$0A
        .BYTE   "  __  ____   ___ ____    ___  ___",$0D,$0A
        .BYTE   " / /_| ___| / _ \___ \  / _ \/ __\",$0D,$0A
        .BYTE   "| '_ \___ \| | | |__) |/ /_)/ /",$0D,$0A
        .BYTE   "| (_) |__) | |_| / __// ___/ /___",$0D,$0A
        .BYTE   " \___/____/ \___/_____\/   \____/",$0D,$0A,$0D,$0A

        .SEGMENT "IVECTOR"
        .ORG    $FFF0
        JMP     DO_FARCALL_ACTUAL
        JMP     LOADS19

        .SEGMENT "VECTORS"
        .ORG    $FFFA
NNTVECTOR:
        .WORD   NINTERRUPT      ;
RSTVECTOR:
        .WORD   COLD_START      ;
INTVECTOR:
        .WORD   INTERRUPT       ; ROM VECTOR FOR IRQ

        .END
