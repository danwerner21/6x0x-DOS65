
;__CLRDIR__________________________________________________________________________________________
;
;	CLEAR TRACKS ON SELECTED DRIVE
;
;	WRITTEN BY: DAN WERNER -- 4/1/2023
;
;__________________________________________________________________________________________________
;
; BIOS JUMP TABLE

farfunct        = $32           ; function to call in driver area
farpointer      = $33           ;
STRPTR          = $48           ;


TEMPWORD        = $0640         ;
TEMPWORD1       = $0642         ;
STARTTRACK      = $0644         ;
COUNTER         = $0646         ; COUNTER
FUNCTREF        = $0648         ;

DSKY_HEXBUF     = $0508         ; Four Bytes DSKY hex buffer
sektrk          = $050C         ; seek track number
seksec          = $050E         ; seek sector number
debcyll         = $0510         ; DEBLOCKED CYLINDER LSB
debcylm         = $0511         ; DEBLOCKED CYLINDER MSB
debsehd         = $0512         ; DEBLOCKED SECTOR AND HEAD (HS)
sekdsk          = $0516         ; seek disk number
dskcfg          = $0517         ; 16 bytes disk configuration table
DSKUNIT         = $0528         ; seek disk number
slicetmp        = $0531         ; (word)
CURRENT_IDE_DRIVE = $0534


INBUFFER        = $0200         ; DISK BUFFER
BUFFER          = $0400         ; DISK BUFFER
DO_FARCALL      = $FFF0


        .SEGMENT "TEA"
        .ORG    $0800

        LDA     #<MSG
        STA     STRPTR
        LDA     #>MSG
        STA     STRPTR+1
        JSR     OUTSTR

        LDA     #02
        STA     farfunct
        JSR     DO_FARCALL

        CMP     #'I'
        BEQ     DO_IDE

        CMP     #'J'
        BEQ     DO_2IDE

        CMP     #'S'
        BEQ     DO_SD

; ABORT
        LDA     #<ABORTMSG
        STA     STRPTR
        LDA     #>ABORTMSG
        STA     STRPTR+1
        JSR     OUTSTR
        BRK

DO_SD:
        LDA     #<SDMSG
        STA     STRPTR
        LDA     #>SDMSG
        STA     STRPTR+1
        JSR     OUTSTR
        LDA     #$02
        STA     CURRENT_IDE_DRIVE
        LDA     #65
        STA     FUNCTREF
        JMP     :++
DO_2IDE:
        LDA     #$01
        STA     CURRENT_IDE_DRIVE
        LDA     #62
        STA     FUNCTREF
        JMP     :+

DO_IDE:
        LDA     #$00
        STA     CURRENT_IDE_DRIVE
        LDA     #62
        STA     FUNCTREF
:
        LDA     #<IDEMSG
        STA     STRPTR
        LDA     #>IDEMSG
        STA     STRPTR+1
        JSR     OUTSTR
:

;   get SLICE
        LDA     #<INPUTNUMBER
        STA     STRPTR
        LDA     #>INPUTNUMBER
        STA     STRPTR+1
        JSR     OUTSTR
        JSR     GETSTR
        JSR     GETDECIMAL
        JSR     TRANSLATESLICE

;   FILL BUFFER WITH $E5
        LDA     #$E5
        LDX     #$00
:
        STA     INBUFFER,X
        INX
        CPX     #$00
        BNE     :-

        LDA     #00
        STA     farfunct
        LDA     #$0D
        JSR     DO_FARCALL
        LDA     #$0A
        JSR     DO_FARCALL

        LDX     #$00
        STX     COUNTER
LOOP:
        LDA     #<RUNMSG
        STA     STRPTR
        LDA     #>RUNMSG
        STA     STRPTR+1
        JSR     OUTSTR
        LDA     debcylm
        JSR     PRINT_BYTE
        LDA     debcyll
        JSR     PRINT_BYTE
        LDA     #00
        STA     farfunct
        LDA     #$0D
        JSR     DO_FARCALL
:
        STX     debsehd
        INX
        LDA     FUNCTREF
        STA     farfunct
        JSR     DO_FARCALL

        CPX     #$00
        BNE     :-

        INC     debcyll
        BNE     :+
        INC     debcylm
:
        INC     COUNTER
        LDA     COUNTER
        CMP     #$1F
        BNE     LOOP

        LDA     #<ENDMSG
        STA     STRPTR
        LDA     #>ENDMSG
        STA     STRPTR+1
        JSR     OUTSTR

        BRK

;__GETSTR______________________________________________________
;
; GET A STRING INTO BUFFER
;
;______________________________________________________________
GETSTR:
        LDX     #$00
GETSTR_LOOP:
        LDA     #02
        STA     farfunct
        JSR     DO_FARCALL

        CMP     #$0D
        BEQ     GETSTR_DONE
        CMP     #$08
        BEQ     GETSTR_DEL
        CMP     #$30
        BCC     GETSTR_LOOP
        CMP     #$40
        BCS     GETSTR_LOOP

        PHA
        LDA     #00
        STA     farfunct
        PLA
        JSR     DO_FARCALL

        AND     #$0F
        STA     BUFFER,X
        INX
        JMP     GETSTR_LOOP
GETSTR_DONE:
        LDA     #$FF
        STA     BUFFER,X
        RTS
GETSTR_DEL:
        CPX     #$00
        BEQ     GETSTR_LOOP

        PHA
        LDA     #00
        STA     farfunct
        PLA
        JSR     DO_FARCALL

        DEX
        JMP     GETSTR_LOOP



;__GETDECIMAL__________________________________________________
;
; CONVERT A STRING (DECIMAL) TO A 8 BIT NUMBER IN TEMPWORD
;
;______________________________________________________________
GETDECIMAL:
        LDX     #$00
        STX     TEMPWORD

GETDECIMAL_LOOP:
        LDA     BUFFER,X
        CMP     #$FF
        BEQ     GETDECIMAL_DONE

; MULTIPLY TEMPWORD*10
        LDA     TEMPWORD
        STA     TEMPWORD1

        LDA     TEMPWORD
        ASL     A               ;*2
        ASL     A               ;*2
        ASL     A               ;*2

        CLC
        ADC     TEMPWORD1
        ADC     TEMPWORD1
        STA     TEMPWORD        ; store sum
        LDA     BUFFER,X        ; add new decimal digit
        INX
        CLC
        ADC     TEMPWORD
        STA     TEMPWORD        ; store sum
        JMP     GETDECIMAL_LOOP
GETDECIMAL_DONE:
        RTS



;__OUTSTR______________________________________________________
;
; OUTPUT THE STRING POINTED TO BY OUTSTR TO THE SCREEN
;
;______________________________________________________________
OUTSTR:
        LDA     #00
        STA     farfunct
        LDY     #$00            ; LOAD $00 INTO Y
OUTSTRLP:
        LDA     (STRPTR),Y      ; LOAD NEXT CHAR FROM STRING INTO ACC
        CMP     #$00            ; IS NULL?
        BEQ     ENDOUTSTR       ; YES, END PRINT OUT
        JSR     DO_FARCALL

        INC     STRPTR
        BNE     OUTSTRLP
        INC     STRPTR+1
        JMP     OUTSTRLP        ; DO NEXT CHAR
ENDOUTSTR:
        RTS                     ; RETURN


TRANSLATESLICE:
        LDA     #$00            ; LOAD TRACK # (LOW BYTE)
        STA     debsehd         ; STORE IN SECTOR/HEAD

        LDA     TEMPWORD        ; GET SLICE#
        STA     debcylm         ; SLICE OFFSET MSB
        LDA     #0              ; GET SLICE#
        STA     debcyll         ; SLICE OFFSET LSB
        CLC                     ; VOODOO MATH TO TAKE SLICE*$4000
        ROR     debcylm
        ROR     debcyll
        ROR     debcylm
        ROR     debcyll

        LDA     TEMPWORD        ; GET SLICE#
        CLC
        ADC     debcyll
        STA     debcyll
        LDA     #$00
        ADC     debcylm
        STA     debcylm
        RTS

;__PRINT_BYTE__________________________________________________
;
; PRINT OUT ACCUMULATOR AS HEX NUMBER
;
;______________________________________________________________
PRINT_BYTE:
        PHA                     ; SAVE A REGISTER
        LSR     A               ; SHIFT HIGH NIBBLE TO LOW NIBBLE
        LSR     A               ;
        LSR     A               ;
        LSR     A               ;
        CLC                     ; CLEAR CARRY
        JSR     PRINT_DIGIT     ; PRINT LOW NIBBLE
        PLA                     ; RESTORE ACCUMULATOR
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
        BMI     :+              ; NO, SKIP ADD
        CLC                     ; CLEAR CARRY
        ADC     #$07            ; ADD ON FOR LETTER VALUES
:       ;
        PHA
        LDA     #00
        STA     farfunct
        PLA
        JMP     DO_FARCALL

MSG:
        .BYTE   $0D,$0A,"CLEARDIR - CLEAR TRACKS ON SELECTED DEVICE",$0D,$0A,$0D,$0A
        .BYTE   "SELECT DRIVE:",$0D,$0A
        .BYTE   "(I) IDE PRIMARY",$0D,$0A
        .BYTE   "(J) IDE SECONDARY",$0D,$0A
        .BYTE   "(S) SD CARD",$0D,$0A
        .BYTE   0

IDEMSG:
        .BYTE   $0D,$0A,"CLEARING TRACKS ON IDE."
        .BYTE   0


SDMSG:
        .BYTE   $0D,$0A,"CLEARING TRACKS ON SD."
        .BYTE   0


INPUTNUMBER:
        .BYTE   $0D,$0A,"WHICH SLICE TO CLEAR? (0-255):"
        .BYTE   0


ABORTMSG:
        .BYTE   $0D,$0A,"ABORTED.",$0D,$0A,$0D,$0A
        .BYTE   0

RUNMSG:
        .BYTE   "CLEARING TRACK $"
        .BYTE   0

ENDMSG:
        .BYTE   $0D,$0A,"CLEAR COMPLETE.",$0D,$0A
        .BYTE   0




.END
