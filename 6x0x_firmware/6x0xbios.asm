
;__6x0xbios__________________________________________________________________________________________
;
;	BIOS for the 6502 6x0x Retrobrew Computers SBC
;
;	ORIGINALLY WRITTEN BY: DAN WERNER -- 1/1/2014
; 	Code cleanup: Dan Werner -- 1/22/2023
;
; ** NOTE THAT THIS BIOS NEEDS PAGED MEMORY TO OPERATE
; ** K17 MUST BE OPEN FOR PAGED MEMORY TO OPERATE ON THE 6502 CPU
;__________________________________________________________________________________________________
;
; CONFIGURATION
;__________________________________________________________________________________________________
;
M6X0X_IOSPACE   = $E000
M6X0X_SHADOW_ROM = $F000

; PAGER
M6X0X_ACT_TASK  = M6X0X_IOSPACE+$A00
M6X0X_MAP_SETUP = M6X0X_IOSPACE+$A10
M6X0X_MAP_SPACE = M6X0X_IOSPACE+$A20
M6X0X_MMU_ENA   = M6X0X_IOSPACE+$A30

;
;
;__________________________________________________________________________________________________
;
; DATA CONSTANTS
;__________________________________________________________________________________________________
;ZERO PAGE	ADDRESS			; FUNCTION
farfunct        = $32           ; function to call in driver area
farpointer      = $33           ;
IRQVECTOR       = $35           ; VECTOR FOR USER IRQ RTN
NMIVECTOR       = $37           ; VECTOR FOR USER NMI RTN
CONSOLE         = $3A           ; CURRENT CONSOLE
TEMPWORD        = $3B           ;
TEMPWORD1       = $3D           ;
TEMPWORD2       = $3F           ;
STRPTR          = $41
hstbuf          = $0200         ; 0200-03ff host buffer

;
; DRIVER WORKING STORAGE
;
INBUFFER        = $0400         ;

DSKY_BUF        = $0500         ; Eight Bytes DSKY display buffer
DSKY_BUFLEN     = 8             ;
DSKY_HEXBUF     = $0508         ; Four Bytes DSKY hex buffer
DSKY_HEXBUFLEN  = 4             ;
sektrk          = $050C         ; seek track number
seksec          = $050E         ; seek sector number
debcyll         = $0510         ; DEBLOCKED CYLINDER LSB
debcylm         = $0511         ; DEBLOCKED CYLINDER MSB
debsehd         = $0512         ; DEBLOCKED SECTOR AND HEAD (HS)
Cdebcyll        = $0513         ; DEBLOCKED CYLINDER LSB (IN CACHE)
Cdebcylm        = $0514         ; DEBLOCKED CYLINDER MSB (IN CACHE)
Cdebsehd        = $0515         ; DEBLOCKED SECTOR AND HEAD (HS)  (IN CACHE)
sekdsk          = $0516         ; seek disk number
dskcfg          = $0517         ; 16 bytes disk configuration table
DSKUNIT         = $0528         ; seek disk number
ST0             = $0529         ;
FLERR           = $052A         ;
FCMD            = $052B         ;
FLRETRY         = $052C         ;
FLRETRY1        = $052D         ;
FLATCH_STORE    = $052E         ;
PPIDETIMEOUT    = $052F         ; (word)
slicetmp        = $0531         ; (word)
PPIDEINDEX      = $0533
CURRENT_IDE_DRIVE = $0534
DSKY_X_STORAGE  = $0535
DSKY_Y_STORAGE  = $0536
DSKY_TEMP_VAL   = $0537
DSKY_PPIX_VAL   = $0538
DSKY_PRESENT    = $0539
FLOPPY_DETCT    = $053A

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
        .IF     DSKY_OPTION=1
            .INCLUDE "bios_dsky.asm"
        .ENDIF

        .IF     DSKY_OPTION=2
            .INCLUDE "bios_dskyng.asm"
        .ENDIF

        .IF     DSKY_OPTION=0
DSKY_INIT:
DSKY_RESET:
DSKY_SHOW:
DSKY_BIN2SEG:
DSKY_STAT:
DSKY_GETKEY:
DSKY_BEEP:
DSKY_PUTLED:
DSKY_BLANK:
DSKY_DSPL:
            RTS
        .ENDIF


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

        LDA     #52             ; RTC_RESET
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

        .INCLUDE "supermon.asm"
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
        JMP     DO_FARCALL
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
