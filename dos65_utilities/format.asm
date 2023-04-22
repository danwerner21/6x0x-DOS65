;__FLOPPY FORMAT_________________________________________________________________________________________________________________
;
; 	DOS/65 floppy FORMATTER for Nhyodyne FDC card
;
;________________________________________________________________________________________________________________________________
;
        .INCLUDE "../6X0X_FIRMWARE/BIOS_DEFINES.ASM"

;
DFLFCB          = $107          ;DEFAULT FCB
PEM             = $103          ;PEM ENTRY
BOOT            = $100          ;WARM BOOT
TEA             = $800          ;EXECUTION ORG
CCMLNG          = 2048          ;CCM LENGTH
CRSYM           = 32            ;CR SYMBOL
LASTROW         = 20
MAXCOL          = 80

USEDSKYNG       = 0
USEDSKY         = 0

OUTMSG_W        = $F0
STACKA          = $3A
DENS            = 2             ; DENSITY
EOTSEC          = 09            ; LAST SECTOR OF TRACK

DRIVERS         = 0

        .MACRO  PRTS      message
        .LOCAL  p1
        .LOCAL  p2
        .LOCAL  p3
        .LOCAL  p4
        .LOCAL  p5

        .IF     .paramcount <> 1
            .ERROR  "Too few parameters for macro PRTS"
        .ENDIF
        PHA
        TXA
        PHA
        TYA
        PHA
        LDX     #$00
p1:
        LDA     p4,x
        INX
        CMP     #'$'
        BEQ     p2
        JSR     OUT
        JMP     p1
p2:
        PLA
        TAY
        PLA
        TAX
        PLA
        JMP     p5
p4:
        .BYTE   message
p5:
        .ENDMACRO


;________________________________________________________________________________________________________________________________
;________________________________________________________________________________________________________________________________


;MAIN PROGRAM
        .SEGMENT "TEA"
        .ORG    $0800



        LDA     #STR_BANNER &$FF
        LDY     #STR_BANNER>>8 & $FF
        JSR     WRITESTR

INLOOP:
        JSR     CIN
        CMP     #'1'
        BEQ     TRACK40
        CMP     #'2'
        BEQ     TRACK80
        JMP     INLOOP

TRACK40:
        LDA     #40
        STA     MAXTRACK
        LDA     #$50
        STA     FMTGAP          ; GAP FOR FORMAT 5.25=$50, 3.5=$54
        JMP     FMTCONT

TRACK80:
        LDA     #80
        STA     MAXTRACK
        LDA     #$54
        STA     FMTGAP          ; GAP FOR FORMAT 5.25=$50, 3.5=$54
        JMP     FMTCONT


FMTCONT:
        LDA     #STR_DRIVE &$FF
        LDY     #STR_DRIVE>>8 & $FF
        JSR     WRITESTR

INLOOP1:
        JSR     CIN
        CMP     #'1'
        BEQ     FMTCONT1
        CMP     #'2'
        BEQ     FMTCONT2
        JMP     INLOOP1

FMTCONT1:
        LDA     #$00
        STA     sekdsk
        LDA     #%00010001
        STA     DSKUNIT
        JMP     FMTCONT3

FMTCONT2:
        LDA     #$01
        STA     sekdsk
        LDA     #%00100001
        STA     DSKUNIT
        JMP     FMTCONT3


FMTCONT3:
        LDA     #STR_INTRO &$FF
        LDY     #STR_INTRO>>8 & $FF
        JSR     WRITESTR

        LDA     #STR_CONFIRM &$FF
        LDY     #STR_CONFIRM>>8 & $FF
        JSR     WRITESTR

        JSR     CONF

        JSR     PC_CR
        JSR     PC_LF
        LDA     #$00
        STA     debcyll

FMTLOOP:
        LDA     #$00
        STA     debcylm
        JSR     FMTCYL

        LDA     #$01
        STA     debcylm
        JSR     FMTCYL

        INC     debcyll
        LDA     debcyll
        CMP     MAXTRACK
        BNE     FMTLOOP

DONE:
        JSR     PC_CR
        JSR     PC_LF
        LDX     #$00
        JMP     PEM


;__FMTCYL________________________________________________________________________________________________________________________
;
; 	FORMAT A FLOPPY TRACK
;________________________________________________________________________________________________________________________________
;
FMTCYL:
        JSR     SETTRACK        ; PERFORM SEEK TO TRACK
        SEI
        JSR     CHECKINT        ; CHECK INTERRUPT STATUS, MAKE SURE IT IS CLEAR
        CMP     #$FF            ; DID IT RETURN WITH ERROR CODE?
        BNE     FMTGO           ; IF YES, EXIT WITH ERROR CODE
        LDA     #STR_ERR1 &$FF
        LDY     #STR_ERR1>>8 & $FF
        JSR     WRITESTR
        JMP     DSKEXIT
FMTGO:  ; 					;
FMT:    ; FORMAT TRACK COMMAND
;
        CLC
        LDA     sekdsk          ; GET DISK UNIT NUMBER
        AND     #$01            ; MASK FOR TWO DRIVES.
        STA     slicetmp        ; PARK IT IN TEMP
        LDA     debcylm         ; GET HEAD SELECTION
        AND     #$01            ; INSURE SINGLE BIT
        ASL     A               ;
        ASL     A               ; MOVE HEAD TO BIT 2 POSITION
        ORA     slicetmp        ; OR HEAD TO UNIT BYTE IN COMMAND BLOCK
        STA     slicetmp        ; STORE IN UNIT
;
        LDA     #$4D            ; BIT 6 SETS MFM, 0DH IS FORMAT COMMAND
        JSR     PFDATA          ; PUSH FORMAT COMMAND TO I8272
        LDA     slicetmp        ;
        JSR     PFDATA          ; WHICH DRIVE UNIT TO FORMAT
        LDA     #DENS           ;
        JSR     PFDATA          ; WHAT DENSITY
        LDA     #EOTSEC         ;
        JSR     PFDATA          ; SECTOR COUNT
        LDA     FMTGAP          ;
        JSR     PFDATA          ; WHAT GAP IS NEEDED
        LDA     #$E5            ;
        JSR     PFDATAS         ; FILLER BYTE FOR SECTORS
        LDY     #$01            ; SET SECTOR#

FMT1:
        LDA     FMSR            ; GET STATUS
        BPL     FMT1
        STA     temp            ;
        AND     #%00100000      ; EXECUTION MODE?
        BEQ     DSKFMEND1       ; NO, ERROR

        LDA     debcyll         ; UPDATE I8272 DURING FORMAT
        STA     FDATA           ; SEND CYLINDER NUMBER
;
;
FMT1A:
        LDA     FMSR            ; GET STATUS
        BPL     FMT1A
        STA     temp            ;
        AND     #%00100000      ; EXECUTION MODE?
        BEQ     DSKFMEND1A      ; NO, ERROR
        LDA     debcylm         ; UPDATE I8272 DURING FORMAT
        STA     FDATA           ; SEND HEAD NUMBER

FMT1B:
        LDA     FMSR            ; GET STATUS
        BPL     FMT1B
        STA     temp            ;
        AND     #%00100000      ; EXECUTION MODE?
        BEQ     DSKFMEND1B      ; NO, ERROR
;
        TYA
        STA     FDATA           ; WHAT SECTOR NUMBER


FMT1C:
        LDA     FMSR            ; GET STATUS
        BPL     FMT1C
        STA     temp            ;
        AND     #%00100000      ; EXECUTION MODE?
        BEQ     DSKFMEND1C      ; NO, ERROR
;
        LDA     #DENS           ;
        STA     FDATA           ; NUMBER OF BYTES PER SECTOR (N2)

FMT1D:
        LDA     FMSR            ; GET STATUS
        BPL     FMT1D           ;
;
        INY                     ; INCREASE SECTOR COUNT
;
        CPY     #$0A            ; IS THIS PAST THE LAST SECTOR OF TRACK?
        BNE     FMT1            ; IF NO, SEND ANOTHER SECTOR
        JSR     PC_PERIOD
        JMP     FMTEND



DSKFMEND1:
        LDA     #STR_ERR2 &$FF
        LDY     #STR_ERR2>>8 & $FF
        JSR     WRITESTR
        JSR     GETERR
        JMP     DONE
DSKFMEND1A:
        LDA     #STR_ERR3 &$FF
        LDY     #STR_ERR3>>8 & $FF
        JSR     WRITESTR
        JSR     GETERR
        JMP     DONE
DSKFMEND1B:
        LDA     #STR_ERR4 &$FF
        LDY     #STR_ERR4>>8 & $FF
        JSR     WRITESTR
        JSR     GETERR
        JMP     DONE
DSKFMEND1C:
        LDA     #STR_ERR5 &$FF
        LDY     #STR_ERR5>>8 & $FF
        JSR     WRITESTR
        JSR     GETERR
        JMP     DONE

FMTEND:
;* CLEAR OUT ANY REMAINING DATA
        LDY     #$10
FMTEND_1:
        JSR     GFDATA          ;GET ERROR TYPE
        DEY
        BNE     FMTEND_1
        RTS

GETERR:
        LDA     temp            ;GET ERROR TYPE
        JSR     PRINT_BYTE
        JSR     PC_SPACE
        JSR     GFDATA          ;GET ERROR TYPE
        JSR     PRINT_BYTE
        JSR     PC_SPACE
        JSR     GFDATA          ;GET ERROR TYPE
        JSR     PRINT_BYTE
        JSR     PC_SPACE
        JSR     GFDATA          ;GET ERROR TYPE
        JSR     PRINT_BYTE
        JSR     PC_SPACE
        JSR     GFDATA          ;GET ERROR TYPE
        JSR     PRINT_BYTE
        JSR     PC_SPACE
        JSR     GFDATA          ;GET ERROR TYPE
        JSR     PRINT_BYTE
        JSR     PC_SPACE
        JSR     GFDATA          ;GET ERROR TYPE
        JSR     PRINT_BYTE
        JSR     PC_SPACE
        JSR     GFDATA          ;GET ERROR TYPE
        JSR     PRINT_BYTE
        RTS

        .INCLUDE "../6x0x_firmware/bios_diov3_flp.asm"




;
;==================================================================================================
; UTILITY FUNCTIONS
;==================================================================================================
;
;
CHR_CR          = $0D
CHR_LF          = $0A
CHR_BS          = $08
CHR_ESC         = $1B
;
;__________________________________________________________________________________________________
;
; UTILITY PROCS TO PRINT SINGLE CHARACTERS WITHOUT TRASHING ANY REGISTERS
;

PC_SPACE:
        PHA
        LDA     #' '
        JSR     OUT             ; PRINT CHAR IN ACC
        PLA
        RTS

PC_PERIOD:
        PHA
        LDA     #'.'
        JSR     OUT             ; PRINT CHAR IN ACC
        PLA
        RTS


PC_CR:
        PHA
        LDA     #CHR_CR
        JSR     OUT             ; PRINT CHAR IN ACC
        PLA
        RTS

PC_LF:
        PHA
        LDA     #CHR_LF
        JSR     OUT             ; PRINT CHAR IN ACC
        PLA
        RTS

OUT:
conwrt:
        PHX
        PHY
        LDX     #2              ;
        JSR     PEM             ;
        PLY
        PLX
        RTS

BLKSECR512:
DEBSECR512:
        RTS


;__PRINT_BYTE__________________________________________________
;
; PRINT OUT ACCUMULATOR AS HEX NUMBER
;
;______________________________________________________________
PRINT_BYTE:
        TAX                     ; SAVE A REGISTER
        LSR     A               ; SHIFT HIGH NIBBLE TO LOW NIBBLE
        LSR     A               ;
        LSR     A               ;
        LSR     A               ;
        CLC                     ; CLEAR CARRY
        JSR     PRINT_DIGIT     ; PRINT LOW NIBBLE
        TXA                     ; RESTORE ACCUMULATOR
        JMP     PRINT_DIGIT     ; PRINT LOW NIBBLE
;__PRINT_DIGIT_________________________________________________
;
; PRINT OUT LOW NIBBLE OF ACCUMULATOR IN HEX
;
;______________________________________________________________
PRINT_DIGIT:
        AND     #$0F            ; STRIP OFF HIGH NIBBLE
        ORA     #$30            ; ADD $30 TO PRODUCE ASCII
        CMP     #$3A            ; IS GREATER THAN 9
        BMI     PRINT_DIGIT_OUT ; NO, SKIP ADD
        CLC                     ; CLEAR CARRY
        ADC     #$07            ; ADD ON FOR LETTER VALUES
PRINT_DIGIT_OUT:                ;
        JMP     OUT             ; PRINT OUT CHAR



;
; OUTPUT A '$' TERMINATED STRING
;
WRITESTR:
        STA     OUTMSG_W
        STY     OUTMSG_W+1
        LDY     #$00
WRITESTR1:
        LDA     (OUTMSG_W),Y    ; LOAD NEXT CHAR FROM STRING INTO ACC
        CMP     #'$'            ; IS END?
        BEQ     ENDOUTSTR       ; YES, END PRINT OUT
        JSR     OUT             ; PRINT CHAR IN ACC
        INY                     ; Y=Y+1 (BUMP INDEX)
        JMP     WRITESTR1       ; DO NEXT CHAR
ENDOUTSTR:
        RTS                     ; RETURN





CIN:
        PHX
        PHY
        LDX     #1              ;
        JSR     PEM             ;
        PLY
        PLX
        RTS


CONF:
        JSR     CIN
        CMP     #$1B
        BEQ     EXIT
        CMP     #$0D
        BNE     CONF

        RTS


NEWLINE:
        LDA     #$0d
        JSR     OUT
        LDA     #$0a
        JMP     OUT

EXIT:
; CLEAN UP AND RETURN TO OS
        JSR     NEWLINE
        JSR     NEWLINE
        JMP     $0100


FMTGAP:
        .BYTE   0               ; GAP FOR FORMAT 5.25=$50, 3.5=$54
CURSEC:
        .BYTE   0

temp:
        .BYTE   0
MAXTRACK:
        .BYTE   40

STR_BANNER:
        .BYTE   $0D,$0A,"Retrobrew 6x0x Floppy Disk Format v1.0"
        .BYTE   $0D,$0A,"Choose Floppy type:",$0D,$0A
        .BYTE   "1> 5.25 inch floppy  360K 40 tracks",$0D,$0A
        .BYTE   "2> 3.5 inch floppy   720K 80 tracks",$0D,$0A,"$"
STR_DRIVE:
        .BYTE   $0D,$0A,"Choose Floppy drive:",$0D,$0A
        .BYTE   "1> DRIVE 0",$0D,$0A
        .BYTE   "2> DRIVE 1",$0D,$0A,"$"
STR_INTRO:
        .BYTE   $0D,$0A,$0D,$0A,"Insert FLOPPY, NOTE EXISTING DATA WILL BE DESTROYED!!!",$0D,$0A,"$"
STR_CONFIRM:
        .BYTE   "Press <Enter> to continue, <Esc> to abort",$0D,$0A,"$"





STR_ERR1:
        .BYTE   $0D,$0A,"DISK ERROR 1",$0D,$0A,"$"
STR_ERR2:
        .BYTE   $0D,$0A,"DISK ERROR 2",$0D,$0A,"$"
STR_ERR3:
        .BYTE   $0D,$0A,"DISK ERROR 3",$0D,$0A,"$"
STR_ERR4:
        .BYTE   $0D,$0A,"DISK ERROR 4",$0D,$0A,"$"
STR_ERR5:
        .BYTE   $0D,$0A,"DISK ERROR 5",$0D,$0A,"$"





        .END
