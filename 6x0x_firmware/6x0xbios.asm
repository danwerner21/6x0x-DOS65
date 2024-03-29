
;__6x0xbios__________________________________________________________________________________________
;
;	BIOS for the 6502 6x0x Retrobrew Computers SBC
;
;	ORIGINALLY WRITTEN BY: DAN WERNER -- 1/1/2014
; 	Code cleanup: Dan Werner -- 1/22/2023
;
; ** NOTE THAT THIS BIOS NEEDS PAGED MEMORY TO OPERATE
; ** K17 MUST BE OPEN FOR PAGED MEMORY TO OPERATE ON THE 6502 CPU

        .INCLUDE "bios_defines.asm"
        .INCLUDE "../dos65_os/dosdefn.asm"; base addresses and definitions

STARTOS         = $B800


        .ORG    $C000
        .SEGMENT "DRIVERS"
        .INCLUDE "macro.asm"
        .INCLUDE "bios_options.asm"
        .INCLUDE "bios_dispatch.asm"
        .INCLUDE "bios_ppp_common.asm"
        .INCLUDE "bios_serial.asm"
        .INCLUDE "bios_ppp_console.asm"
        .INCLUDE "bios_ppp_hd.asm"
        .INCLUDE "bios_diov3_flp.asm"
        .INCLUDE "bios_diov3_ide.asm"
        .INCLUDE "bios_console.asm"
        .INCLUDE "bios_rtc.asm"
        .INCLUDE "bios_dskyng.asm"



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

;;; TODO  ADD AUTODETECT/OPTION AT SOME POINT
        LDA     #09             ; SET CONSOLE (09=PPP, 04= SERIAL)
        STA     CONSOLE
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
        LDA     #08             ; SERIAL INITIALIZE
        STA     farfunct
        JSR     DO_FARCALL
;
        LDA     #13             ; PROP VIDEO INITIALIZE
        STA     farfunct
        JSR     DO_FARCALL

        LDA     #52             ; RTC_RESET
        STA     farfunct
        JSR     DO_FARCALL

        JSR     Init_NVRAM      ; get NVRAM Settings, init if invalid


        JSR     PAGE_ENTER
        LDA     #<STARTUP       ; OUTPUT STARTUP STRING
        STA     STRPTR          ;
        LDA     #>STARTUP       ;
        STA     STRPTR+1        ;
        JSR     OUTSTR          ;
        JSR     PAGE_EXIT

        LDA     #$00            ;
        STA     INBUFFER        ; MAKE SURE INPUT BUFFER IS EMPTY
;
        LDA     #40             ; DSKY INITIALIZE
        STA     farfunct
        JSR     DO_FARCALL

        LDA     #60             ; IDE INITIALIZE
        STA     farfunct
        JSR     DO_FARCALL

        LDA     #63             ; SD INITIALIZE
        STA     farfunct
        JSR     DO_FARCALL

        LDA     #66             ; FLOPPY INITIALIZE
        STA     farfunct
        JSR     DO_FARCALL

        LDX     #$00            ; SHOW A STARTUP MESSAGE ON DSKY
:
        LDA     DSKYMSG,x
        STA     DSKY_BUF,x
        INX
        CPX     #8
        BNE     :-
        LDA     #41             ; DSKY_SHOW
        STA     farfunct
        JSR     DO_FARCALL

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
        .INCLUDE "bios_pager.ASM"


;__IOF_CONINW____________________________________________________________________________________________
;
; PERFORM BLOCKING CONSOLE READ
;________________________________________________________________________________________________________
IOF_CONINW:
        LDA     #02
        STA     farfunct
        JMP     DO_FARCALL

;__IOF_CONIN_____________________________________________________________________________________________
;
; PERFORM NON-BLOCKING CONSOLE READ
;________________________________________________________________________________________________________
IOF_CONIN:
        LDA     #01
        STA     farfunct
        JMP     DO_FARCALL

;__OUTCH_________________________________________________________________________________________________
;
; PERFORM CONSOLE WRITE
;________________________________________________________________________________________________________
IOF_OUTCH:
        PHA
        PHA
        LDA     #00
        STA     farfunct
        PLA
        JSR     DO_FARCALL
        PLA
        RTS

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

;__Init_NVRAM__________________________________________________
;
; get NVRAM Settings, init if invalid
;
;______________________________________________________________
Init_NVRAM:
        LDX     #$20            ; get signature byte
        LDA     #51             ; Read RTC
        STA     farfunct
        JSR     DO_FARCALL
        TYA
        CMP     #$a5            ; is valid
        BEQ     :+
        LDX     #$20            ; set signature byte
        LDY     #$a5
        LDA     #50             ; Write RTC
        STA     farfunct
        JSR     DO_FARCALL
        LDX     #$21            ; set console byte
        LDY     CONSOLE
        LDA     #50             ; Write RTC
        STA     farfunct
        JSR     DO_FARCALL
        LDX     #$22            ; set dsky byte
        LDY     #$00
        LDA     #50             ; Write RTC
        STA     farfunct
        JSR     DO_FARCALL
        RTS
:
        LDX     #$21            ; get default console
        LDA     #51             ; Read RTC
        STA     farfunct
        JSR     DO_FARCALL
        STY     CONSOLE
        LDX     #$22            ; get dsky mode
        LDA     #51             ; Read RTC
        STA     farfunct
        JSR     DO_FARCALL
        STY     DSKYMODE
        RTS






DSKYMSG:
        .BYTE   $7C, $6, $3F, $6D, $0, $3E, $73, $0
STARTUP:
        .BYTE   $0D,$0A

        .BYTE   "  RetroBrew Computers 6x0x",$0D,$0A,$0D,$0A
        .BYTE   " .d8888b.            .d8888b. ",$0D,$0A
        .BYTE   "d88P  Y88b          d88P  Y88b ",$0D,$0A
        .BYTE   "888                 888    888 ",$0D,$0A
        .BYTE   "888d888b.  888  888 888    888 888  888 ",$0D,$0A
        .BYTE   "888P  Y88b `Y8bd8P' 888    888 `Y8bd8P' ",$0D,$0A
        .BYTE   "888    888   X88K   888    888   X88K ",$0D,$0A
        .BYTE   "Y88b  d88P .d8  8b. Y88b  d88P .d8  8b. ",$0D,$0A
        .BYTE   "  Y8888P   888  888   Y8888P   888  888 ",$0D,$0A,$0D,$0A

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
