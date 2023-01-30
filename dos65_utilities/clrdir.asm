
; BIOS JUMP TABLE
IOF_CONIN       = $FD00         ; read a byte from CONSOLE ('A' POINTS TO BYTE)
IOF_CONINW      = $FD03         ; read a byte from CONSOLE ('A' POINTS TO BYTE, WAIT FOR BYTE)
IOF_OUTCH       = $FD06         ; write a byte from CONSOLE  ('A' POINTS TO BYTE)
IOF_CONSTATUS   = $FD09         ; RETURN CONSOLE STATUS
SERIALINIT      = $FD0C         ; called during OS init
RDSER1          = $FD0F         ; read a byte from serial port ('A' POINTS TO BYTE)
WRSER1          = $FD12         ; write a byte from serial port  ('A' POINTS TO BYTE)
RDSER1W         = $FD15         ; read a byte from serial port ('A' POINTS TO BYTE, WAIT FOR INPUT)
SERIALSTATUS    = $FD18         ; GET UART STATUS
SETUPDRIVE      = $FD1B         ; init floppy drive
READFL          = $FD1E         ; read sector from floppy
WRITEFL         = $FD21         ; write sector to floppy
PPP_SOFT_RESET  = $FD24         ; reset ppp sd drive
PPP_READ_SECTOR = $FD27         ; read ppp sd drive sector
PPP_WRITE_SECTOR = $FD2A        ; write ppp sd drive sector
IDE_SOFT_RESET  = $FD2D         ; reset ide drive
IDE_READ_SECTOR = $FD30         ; ide read sector
IDE_WRITE_SECTOR = $FD33        ; ide write sector
LOADS19         = $FD33         ; load s19 from serial port into ram

debcyll         = $0510         ; DEBLOCKED CYLINDER LSB
debcylm         = $0511         ; DEBLOCKED CYLINDER MSB
debsehd         = $0512         ; DEBLOCKED SECTOR AND HEAD (HS)

OUTMSG_W    = $25

        .SEGMENT "TEA"
        .ORG    $0800


        LDA     #<opnmsg        ;point to message
        LDY     #>opnmsg
        JSR     outmsg          ;send it

    jsr     PPP_SOFT_RESET
    lda     #$02
    sta     $11
    lda     #$00
    sta     $10
    lda     #$e5
    ldy     #$00
:
    sta     ($10),y
    INy
    cpy     #00
    bne     :-
    inc     $11
:
    sta     ($10),y
    INy
    cpy     #00
    bne     :-

    LDA     #$00
    sta     debcylm
    sta     debcyll
    sta     debsehd
:
    jsr     PPP_WRITE_SECTOR
    inc     debsehd
    lda     debsehd
    cmp     #$00
    bne     :-
    inc     debcyll
    lda     debcyll
    JSR     PRINT_BYTE
    LDA     #'.'
    JSR     IOF_OUTCH
    lda     debcyll
    cmp     #$00
    bne     :-
    brk



;__OUTMSG________________________________________________________________________________________________
;
; 	WRITE A NULL TERMINATED STRING TO THE CONSOLE
;
;	A=POINTER LOW BYTE
;	Y=POINTER HIGH BYTE
;________________________________________________________________________________________________________
outmsg: ;output message
        STA     OUTMSG_W
        STY     OUTMSG_W+1
        LDY     #$00
OUTSTRLP:
        LDA     (OUTMSG_W),Y    ; LOAD NEXT CHAR FROM STRING INTO ACC
        CMP     #$00            ; IS NULL?
        BEQ     ENDOUTSTR       ; YES, END PRINT OUT
        JSR     IOF_OUTCH       ; PRINT CHAR IN ACC
        INY                     ; Y=Y+1 (BUMP INDEX)
        JMP     OUTSTRLP        ; DO NEXT CHAR
ENDOUTSTR:
        RTS                     ; RETURN


opnmsg:
        .BYTE   $0d,$0a,"Clear SD drive for DOS/65 ON THE RBC",$0d,$0a,0

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
PRINT_DIGIT:
          AND     #$0F            ; STRIP OFF HIGH NIBBLE
           ORA     #$30            ; ADD $30 TO PRODUCE ASCII
         CMP     #$3A            ; IS GREATER THAN 9
                BMI     PRINT_DIGIT_OUT ; NO, SKIP ADD
            CLC                     ; CLEAR CARRY
         ADC     #$07            ; ADD ON FOR LETTER VALUES
PRINT_DIGIT_OUT:                ;
      JMP     IOF_OUTCH       ; PRINT OUT CHAR
