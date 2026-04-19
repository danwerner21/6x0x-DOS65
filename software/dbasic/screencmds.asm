
        .IFDEF  MEMORYMAPPEDSCREEN

;* MEMORY MAP
;  VIDEO CARD IS A 32K AREA (MAPPED IN BANKS TO $AXXX)
;  $0x00 0x01=soft_scanline_emulation, 0x02 NO soft_scanline_emulation
;  $0x01 0x01=page1, 0x02 page2
;  $0x02  character generator write offset (data << 3)
;  $0x03 character generator write DATA
;  $0x04 EXECUTE device command
;        0x00- reset to the default configuration
;        0x01- reset to the saved configuration
;	0x02- save the current configuration
;
;  $0x05  0x01=soft_text=true, 0x02=soft_text=false
;  $0x06 Lores Mode=0X01,NO lores Mode=0x02
;  $0x07 Double Lores Mode=0X01,NO Double lores Mode=0x02
;  $0x08 Hires Mode=0X01,NO Hires Mode=0x02
;  $0x09 Double Hires Mode=0X01,NO Double Hires lores Mode=0x02
;  $0x0A 80COL Mode=0X01,NO 80COL Mode=0x02
;  $0x0B MIXED Mode=0X01,NO MIXED Mode=0x02
;  $0x0C QUAD Hires Mode=0X01,NO QUAD Hires lores Mode=0x02
;  $0x0D MONO Hires Mode=0X01,NO MONO Hires lores Mode=0x02
;
; 	VRAM Memory Map
;	$1000-$177F	40/80 Text Page 1
;	$1800-$1F7F	40/80 Color Page 1
;	$2000-$277F	40/80 Text Page 2
;	$2800-$2F7F	40/80 Color Page 2
;	$2000-$5FFF	HIRES PAGE 1
;	$6000-$8FFF	HIRES PAGE 2
;	$2000-$BFFF	DOUBLE HIRES

VIDEOBANK       = $F8
PC6502_IOSPACE  = $EF00
; PAGER
PC6502_ACT_TASK = PC6502_IOSPACE+$E0
PC6502_MAP_SETUP = PC6502_IOSPACE+$E1
PC6502_MMU_ENA  = PC6502_IOSPACE+$E2
PC6502_MAP_SPACE = PC6502_IOSPACE+$D0

VideoDisplayPage = $A001
VideoCharGenOffset = $A002
VideoCharGenData = $A003
VideoTextMode   = $A005
VideoLoresMode  = $A006
VideoDoubleLores = $A007
VideoHiresMode  = $A008
VideoDoubleHires = $A009
Video80col      = $A00A
VideoMixedMode  = $A00b
VideoQuadHires  = $A00c
VideoMonoHires  = $A00d

CURX            = $0647         ; CURRENT CURSOR POSITION (MEMORY MAPPED)
CURY            = $0648
SHOWCRSR        = $0649         ; SHOW CURSOR (1-YES, 0-NO) (MEMORY MAPPED)
CURCOLOR        = $064A         ; CURRENT PRINT COLOR (MEMORY MAPPED)
CSRCOLOR        = $064B         ; CURRENT CURSOR COLOR (MEMORY MAPPED)
BVIDEOMODE      = $064C         ; CURRENT VIDEO MODE (MEMORY MAPPED) 00=40 COL, 01=80 COL


VIDEOMODE:
        .BYTE   $00
CLRTMP:
        .BYTE   $00
VIDEOWIDTH:
        .BYTE   80
PLOTX:
        .BYTE   $00,$00
PLOTY:
        .BYTE   $00,$00
PLOTCOLOR:
        .BYTE   $00
LINECOLOR:
        .BYTE   $00
LINEX1:
        .BYTE   $00,$00
LINEY1:
        .BYTE   $00,$00
LINEX2:
        .BYTE   $00,$00
LINEY2:
        .BYTE   $00,$00
DELTAX:
        .BYTE   $00,$00
DELTAY:
        .BYTE   $00,$00
PTEMPW:
        .BYTE   $00,$00
PTEMPW1:
        .BYTE   $00,$00
PTEMP:
        .BYTE   $00
LINEERR:
        .BYTE   $00,$00


;___V_SCRCLR________________________________________________
;
; CLEAR SCREEN
;
;__________________________________________________________
LAB_SCRCLR:
V_SCNCLR:
        STA     DBGA
        STY     DBGY
        STX     DBGX
        LDA     #38
        STA     farfunct
        JSR     DO_FARCALL
        LDA     #59
        STA     farfunct
        JSR     DO_FARCALL
        LDA     DBGA
        LDY     DBGY
        LDX     DBGX
        RTS



;___V_LOCATE________________________________________________
;
; SET CURSOR LOCATION
;
;       LOCATE X,Y
;
;__________________________________________________________
V_LOCATE:
LAB_LOCATE:
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X (MODE)
        STX     DBGX
        JSR     LAB_1C01        ; GET THE SECOND PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X
        TXA
        TAY
        LDX     DBGX
        LDA     #37
        STA     farfunct
        JSR     DO_FARCALL
        RTS

;___V_COLOR_________________________________________________
;
; SET COLOR FG,BG,CFG,CBG
;
;__________________________________________________________
LAB_COLOR:
V_COLOR:
        LDA     #$00            ; CLEAR TEMP SPACE
        STA     PTEMPW
        STA     PTEMPW+1
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X (MODE)
        TXA
        AND     #$0F
        STA     PTEMPW
        JSR     LAB_1C01        ; GET THE SECOND PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X
        TXA
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        ORA     PTEMPW
        STA     PTEMPW
        JSR     LAB_1C01        ; GET THE THIRD PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE THIRD PARAMETER, RETURN IN X
        TXA
        AND     #$0F
        STA     PTEMPW+1
        JSR     LAB_1C01        ; GET THE FOURTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE FOURTH PARAMETER, RETURN IN X
        TXA
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        ORA     PTEMPW+1
        STA     PTEMPW+1
        LDX     PTEMPW
        LDY     PTEMPW+1
        LDA     #39
        STA     farfunct
        JSR     DO_FARCALL
        RTS



;___V_SPEEK()______________________________________________
;
; GET VALUE FROM SCREEN MEMORY
;
;  TAKES ONE PARAMETER (ADDRESS), RETURNS VALUE
;
;__________________________________________________________
LAB_SPEEK:
V_SPEEK:
        JSR     LAB_F2FX        ; save integer part of FAC1 in temporary integer
        LDA     Itemph
        TAY
        AND     #$0F
        ORA     #$A0
        STA     Itemph
        TYA
        AND     #$F0
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #$07
        CLC
        ADC     #VIDEOBANK
        TAY
        LDA     #$01            ; MODIFY TASK 01 (DRIVER TASK)
        LDX     #$0A            ; MAP $AXXX
        JSR     SETPAGE         ; CALL SETPAGE
        LDA     #$01
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 01
        LDY     #0
        LDA     (Itempl),Y
        TAY
        LDA     #$00
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 00
        JMP     LAB_1FD0        ; convert Y to byte in FAC1 and return



;___V_SPOKE_________________________________________________
;
; PUT VALUE IN SCREEN MEMORY
;
;  TAKES TWO PARAMETERS ADDRESS,VALUE
;
;__________________________________________________________
LAB_SPOKE:
V_SPOKE:
        JSR     LAB_GADB        ; get two parameters for POKE or WAIT
        TXA                     ; BYTE ARGUMENT IS IN X
        PHA                     ; PUSH BYTE ARGUMENT TO STACK
        LDA     Itemph
        TAY
        AND     #$0F
        ORA     #$A0
        STA     Itemph
        TYA
        AND     #$F0
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #$07
        CLC
        ADC     #VIDEOBANK
        TAY
        LDA     #$01            ; MODIFY TASK 01 (DRIVER TASK)
        LDX     #$0A            ; MAP $AXXX
        JSR     SETPAGE         ; CALL SETPAGE
        LDA     #$01
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 01
        LDY     #0
        PLA                     ; PULL BYTE ARGUMENT
        STA     (Itempl),Y
        LDA     #$00
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 00
        RTS

;___V_SCREEN_________________________________________________
;
;  SET SCREEN MODE
;
;  TAKES UP TO THREE PARAMETERS
;  FIRST PARAMETER SCREEN MODE
;  0=TEXT MODE
;  1=LORES MODE
;  2=HIRES MODE
;
;  TEXT MODE PARAMETERS
;  0=40 COLUMNS
;  1=80 COLUMNS
;
;  LORES MODE SECOND PARAMETER
;  0=SINGLE LORES
;  1=DOUBLE LORES
;  LORES MODE THIRD PARAMETER
;  0=MIXED MODE
;  1=FULL SCREEN MODE
;
;  HIRES MODE SECOND PARAMETER
;  0=SINGLE HIRES
;  1=DOUBLE HIRES
;  2=QUAD HIRES
;  3=MONO HIRES
;  HIRES MODE THIRD PARAMETER
;  1=MIXED MODE
;  0=FULL SCREEN MODE
;
;__________________________________________________________
LAB_SCREEN:
V_SCREEN:
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X (MODE)
V_SCREEN1:
        STX     VIDEOMODE
        CPX     #00
        BNE     *+5
        JMP     SETUPMODE0
        CPX     #01
        BNE     *+5
        JMP     SETUPMODE1
        CPX     #02
        BNE     *+5
        JMP     SETUPMODE2

        LDX     #$02            ; SYNTAX ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
        RTS

SETUPMODE0:                     ; TEXT MODE
        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$01
        STA     VideoTextMode
        LDA     #$02
        STA     VideoLoresMode
        STA     VideoHiresMode
        JSR     PAGE_EXIT

        JSR     LAB_1C01        ; GET THE SECOND PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X
        CPX     #$00
        BNE     SETUPMODE0_80

        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$02
        STA     Video80col
        JSR     PAGE_EXIT
        LDA     #40
        STA     VIDEOWIDTH
        LDA     #0
        STA     BVIDEOMODE
        JMP     SETUPMODE0_CLEAR
SETUPMODE0_80:
        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$01
        STA     Video80col
        JSR     PAGE_EXIT
        LDA     #80
        STA     VIDEOWIDTH
        LDA     #1
        STA     BVIDEOMODE
SETUPMODE0_CLEAR:
        JMP     V_SCNCLR
        RTS

SETUPMODE1:                     ; LORES MODE
        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$01
        STA     VideoLoresMode
        LDA     #$02
        STA     VideoTextMode
        STA     VideoHiresMode
        JSR     PAGE_EXIT
        JSR     LAB_1C01        ; GET THE SECOND PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X

        CPX     #$00
        BNE     SETUPMODE1_DOUBLE

        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$02
        STA     VideoDoubleLores
        JSR     PAGE_EXIT
        JMP     SETUPMODE1_CLEAR
SETUPMODE1_DOUBLE:
        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$01
        STA     VideoDoubleLores
        JSR     PAGE_EXIT
        LDA     #$11
        STA     VIDEOMODE
SETUPMODE1_CLEAR:               ;    ($2000-$2800)
        LDY     #$02+VIDEOBANK  ; AREA $2000-$2FFF
        JSR     PAGE_ENTER
        LDA     #$00
        STA     TEMPW
        LDA     #$A0
        STA     TEMPW+1
        LDY     #$00
        LDA     #$00
:
        STA     (TEMPW),Y
        INC     TEMPW
        BNE     :-
        INC     TEMPW+1

        LDX     TEMPW+1
        CPX     #$A8
        BNE     :-
        JSR     PAGE_EXIT
        JSR     LAB_1C01        ; GET THE THIRD PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE THIRD PARAMETER, RETURN IN X
        CPX     #$00
        BNE     SETUPMODE1_MIXED

        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$02
        STA     VideoMixedMode
        JSR     PAGE_EXIT
        RTS
SETUPMODE1_MIXED:
        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$01
        STA     VideoMixedMode
        JSR     PAGE_EXIT
        LDA     VIDEOMODE
        ORA     #$80
        STA     VIDEOMODE
        RTS


SETUPMODE2:                     ; HIRES MODE
        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$01
        STA     VideoHiresMode
        LDA     #$02
        STA     VideoTextMode
        STA     VideoLoresMode
        JSR     PAGE_EXIT
        JSR     LAB_1C01        ; GET THE SECOND PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X

        CPX     #$00
        BNE     SETUPMODE2_DOUBLE

        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$02
        STA     VideoDoubleHires
        STA     VideoQuadHires
        STA     VideoMonoHires
        JSR     PAGE_EXIT
        JMP     SETUPMODE2_CLEAR
SETUPMODE2_DOUBLE:
        CPX     #$01
        BNE     SETUPMODE2_QUAD

        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$01
        STA     VideoDoubleHires
        LDA     #$02
        STA     VideoQuadHires
        STA     VideoMonoHires
        JSR     PAGE_EXIT
        LDA     #$12
        STA     VIDEOMODE
        JMP     SETUPMODE2_CLEAR
SETUPMODE2_QUAD:
        CPX     #$02
        BNE     SETUPMODE2_MONO

        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$01
        STA     VideoQuadHires
        LDA     #$02
        STA     VideoDoubleHires
        STA     VideoMonoHires
        JSR     PAGE_EXIT
        LDA     #$22
        STA     VIDEOMODE
        JMP     SETUPMODE2_CLEAR
SETUPMODE2_MONO:
        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$01
        STA     VideoMonoHires
        LDA     #$02
        STA     VideoDoubleHires
        STA     VideoQuadHires
        JSR     PAGE_EXIT
        LDA     #$32
        STA     VIDEOMODE

SETUPMODE2_CLEAR:               ; ($2000-$7FFF)
        LDY     #$02+VIDEOBANK
        STY     CLRTMP

:
        LDY     CLRTMP
        JSR     PAGE_ENTER

        LDA     #$00
        TAY
        STA     TEMPW
        LDA     #$A0
        STA     TEMPW+1
        LDA     #$00
:
        STA     (TEMPW),Y
        INC     TEMPW
        BNE     :-
        INC     TEMPW+1
        LDX     TEMPW+1
        CPX     #$B0
        BNE     :-
        LDX     #$A0
        INC     CLRTMP
        LDX     CLRTMP
        CPX     #$00
        BNE     :--
        JSR     PAGE_EXIT
        JSR     LAB_1C01        ; GET THE THIRD PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE THIRD PARAMETER, RETURN IN X
        CPX     #$00
        BNE     SETUPMODE2_MIXED
        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$02
        STA     VideoMixedMode
        JSR     PAGE_EXIT
        RTS
SETUPMODE2_MIXED:
        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$01
        STA     VideoMixedMode
        JSR     PAGE_EXIT
        LDA     VIDEOMODE
        ORA     #$80
        STA     VIDEOMODE
        RTS



PAGE_EXIT:
        LDA     #$00
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 00
        RTS

PAGE_ENTER:
; Set "Y" register to video page ($YXXX)
        LDA     #$01            ; MODIFY TASK 01 (DRIVER TASK)
        LDX     #$0A            ; MAP $AXXX
        JSR     SETPAGE         ; CALL SETPAGE
        LDA     #$01
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 01
        RTS

;___V_PLOT__________________________________________________
;
;  PLOT ON SCREEN
;         TAKES THREE PARAMETERS,  X,Y,COLOR
;
;__________________________________________________________
LAB_PLOT:
V_PLOT:
        JSR     LAB_EVNM        ; evaluate expression and check is numeric,
                                ; else do type mismatch
        JSR     LAB_F2FX        ; save integer part of FAC1 in temporary integer
        LDA     Itemph
        STA     PLOTX+1
        LDA     Itempl
        STA     PLOTX
        JSR     LAB_1C01        ; GET THE SECOND PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X
        STX     PLOTY           ; STORE IN PLOTY
        LDA     #$00
        STA     PLOTY+1
        JSR     LAB_1C01        ; GET THE THIRD PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; RETURN IN X
        STX     PLOTCOLOR
DOPLOTXY:
                                ; PARAMETERS PARSED, GO TO PROPER MODE CODE
        LDA     VIDEOMODE
        AND     #$0F
        CMP     #$01
        BEQ     V_PLOT_LORES
        LDA     VIDEOMODE
        AND     #$2F
        CMP     #$02
        BNE     :+
        JMP     V_PLOT_HIRES_COLOR
:
        CMP     #$22
        BNE     :+
        JMP     V_PLOT_HIRES_MONO
:
        RTS

V_PLOT_LORES:
        LDA     PLOTY
        PHA
        LSR     A               ; 2 LINES PER BYTE
        STA     PLOTY
                                ; MULTIPLY Y (PTEMPW) COORD BY 40 OR 80 (SINGLE OR DOUBLE LORES)
        CLC
        ASL     PLOTY
        ROL     PLOTY+1         ; *2
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*4)
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*8)
        LDA     PLOTY
        STA     PTEMPW
        LDA     PLOTY+1
        STA     PTEMPW+1        ; STORE Y*8 INTO PTEMPW
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*16)
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*32)
; NOW TAKE Y*8(PTEMPW) + Y*32(PLOTY) == Y*40 STORE IN PLOTY
        CLC                     ; Clear the Carry flag before the first addition

        LDA     PLOTY           ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW          ; Add the low byte of the second number (plus carry)
        STA     PLOTY           ; Store the low byte of the result

        LDA     PLOTY+1         ; Load the high byte of the first number
        ADC     PTEMPW+1        ; Add the high byte of the second number (plus carry from previous op)
        STA     PLOTY+1         ; Store the high byte of the result

; if double lores columns double it.
        LDA     VIDEOMODE
        AND     #$10
        CMP     #00
        BEQ     :+
        ASL     PLOTY
        ROL     PLOTY+1         ; *2
:
; OK, Y OFFSET IS IN PTEMPW1, X IS IN PTEMPW
        LDA     #$A0            ; ADD THE MEMORY OFFSET TO PTEMPW (BECAUSE IT IS EASY)
        STA     PLOTX+1         ; THEN ADD PTEMPW1 TO PTEMPW THAT SHOULD BE THE MEMORY ADDRESS TO UPDATE
                                ;
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PLOTX           ; Load the low byte of the first number into the accumulator
        ADC     PLOTY           ; Add the low byte of the second number (plus carry)
        STA     TEMPW           ; Store the low byte of the result

        LDA     PLOTX+1         ; Load the high byte of the first number
        ADC     PLOTY+1         ; Add the high byte of the second number (plus carry from previous op)
        STA     TEMPW+1         ; Store the high byte of the result
                                ; PTEMPW IS THE MEMORY OFFSET TO UPDATE
        PLA
        LSR     A               ; TOP OR BOTTOM PIXEL?
        BCC     :+
                                ; TOP PIXEL
        LDY     #$02+VIDEOBANK  ; AREA $2000-$2FFF
        JSR     PAGE_ENTER
        LDY     #$00
        LDA     (TEMPW),Y       ; GET EXISTING DOUBLE PIXEL
        AND     #$0F
        PHA
        LDA     PLOTCOLOR
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        STA     PLOTCOLOR
        PLA
        ORA     PLOTCOLOR
        STA     (TEMPW),Y       ; STORE  DOUBLE PIXEL
        JSR     PAGE_EXIT
        RTS
:
                                ; BOTTOM PIXEL
        LDY     #$02+VIDEOBANK  ; AREA $2000-$2FFF
        JSR     PAGE_ENTER
        LDY     #$00
        LDA     (TEMPW),Y       ; GET EXISTING DOUBLE PIXEL
        AND     #$F0
        ORA     PLOTCOLOR
        STA     (TEMPW),Y       ; STORE  DOUBLE PIXEL
        JSR     PAGE_EXIT
        RTS

V_PLOT_HIRES_COLOR:
        LDA     PLOTX
        PHA                     ; PUSH LOW BYTE OF X (FOR EVEN/ODD LATER)
        LSR     PLOTX+1
        ROR     PLOTX           ; 2 PIXEL PER BYTE
        LDA     PLOTX           ;
                                ; FIGURE THE BUFFER OFFSET
        CLC                     ; MULTIPLY Y COORD BY 70 OR 140 (SINGLE OR DOUBLE HIRES)
        ASL     PLOTY
        ROL     PLOTY+1         ; *2
        LDA     PLOTY
        STA     PTEMPW
        LDA     PLOTY+1
        STA     PTEMPW+1        ; STORE Y*2 INTO PTEMPW
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*4)
        LDA     PLOTY
        STA     PTEMPW1
        LDA     PLOTY+1
        STA     PTEMPW1+1       ; STORE Y*4 INTO PTEMPW1
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*8)
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*16)
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*32)
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*64)
                                ; RESULT OFFSET PLOTY = PLOTY(Y*64)+PTEMPW(Y*2)+PTEMPW1(Y*4)
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PLOTY           ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW          ; Add the low byte of the second number (plus carry)
        STA     PLOTY           ; Store the low byte of the result
        LDA     PLOTY+1         ; Load the high byte of the first number
        ADC     PTEMPW+1        ; Add the high byte of the second number (plus carry from previous op)
        STA     PLOTY+1         ; Store the high byte of the result

        CLC                     ; Clear the Carry flag before the first addition
        LDA     PLOTY           ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW1         ; Add the low byte of the second number (plus carry)
        STA     PLOTY           ; Store the low byte of the result
        LDA     PLOTY+1         ; Load the high byte of the first number
        ADC     PTEMPW1+1       ; Add the high byte of the second number (plus carry from previous op)
        STA     PLOTY+1         ; Store the high byte of the result

; if double hires double it.
        LDA     VIDEOMODE
        AND     #$10
        CMP     #00
        BEQ     :+
        ASL     PLOTY
        ROL     PLOTY+1         ; *2
:
; PLOTY IS NOW Y OFFSET, ADD X OFFSET FOR MEMORY ADDRESS INTO TEMPW
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PLOTY           ; Load the low byte of the first number into the accumulator
        ADC     PLOTX           ; Add the low byte of the second number (plus carry)
        STA     TEMPW           ; Store the low byte of the result
        LDA     PLOTY+1         ; Load the high byte of the first number
        ADC     PLOTX+1         ; Add the high byte of the second number (plus carry from previous op)
        STA     TEMPW+1         ; Store the high byte of the result

        LDA     TEMPW+1         ; OK, LET'S CALCULATE THE BANK
        AND     #$F0
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        CLC
        ADC     #$02+VIDEOBANK  ; AREA $2000-?
        CMP     #$F8
        BCC     V_PLOT_HIRES_COLOR_RANGE
        TAY                     ; VIDEO BANK SHOULD BE IN Y
        JSR     PAGE_ENTER

        LDA     TEMPW+1
        AND     #$0F
        ORA     #$A0
        STA     TEMPW+1         ; TEMPW NOW REFLECTS MAPPED ADDRESSS
        PLA
        LSR     A               ; LEFT OR RIGHT PIXEL?
        BCC     :+
                                ; LEFT PIXEL
        LDY     #$00
        LDA     (TEMPW),Y       ; GET EXISTING DOUBLE PIXEL
        AND     #$0F
        PHA
        LDA     PLOTCOLOR
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        STA     PLOTCOLOR
        PLA
        ORA     PLOTCOLOR
        STA     (TEMPW),Y       ; WRITE THE PIXEL BACK OUT
        JSR     PAGE_EXIT
        RTS
:
; RIGHT PIXEL
        LDY     #$00
        LDA     (TEMPW),Y       ; GET EXISTING DOUBLE PIXEL
        AND     #$F0
        ORA     PLOTCOLOR
        STA     (TEMPW),Y       ; WRITE THE PIXEL BACK OUT
        JSR     PAGE_EXIT
        RTS

V_PLOT_HIRES_COLOR_RANGE:
        JMP     LAB_2564

V_PLOT_HIRES_MONO:
        LDA     PLOTX
        PHA
        LSR     PLOTX+1
        ROR     PLOTX
        LSR     PLOTX+1
        ROR     PLOTX
        LSR     PLOTX+1
        ROR     PLOTX           ; 8 PIXEL PER BYTE
        LDA     #00
        STA     PLOTX+1
        STA     PTEMPW1+1
        LDA     PLOTY
        STA     PTEMPW1
                                ; MULTIPLY Y COORD BY 35 OR 70 (MONO OR QUAD HIRES)
        ASL     PLOTY
        ROL     PLOTY+1         ; *2
        LDA     PLOTY
        STA     PTEMPW
        LDA     PLOTY+1
        STA     PTEMPW+1        ; STORE *2 INTO PTEMPW
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*4)
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*8)
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*16)
        ASL     PLOTY
        ROL     PLOTY+1         ; *2 (*32)
;
; PLOTY(Y*35) = PLOTY(Y*32)+PTEMPW(Y*2)+PTEMPW1(Y)
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PLOTY           ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW          ; Add the low byte of the second number (plus carry)
        STA     PLOTY           ; Store the low byte of the result

        LDA     PLOTY+1         ; Load the high byte of the first number
        ADC     PTEMPW+1        ; Add the high byte of the second number (plus carry from previous op)
        STA     PLOTY+1         ; Store the high byte of the result
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PLOTY           ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW1         ; Add the low byte of the second number (plus carry)
        STA     PLOTY           ; Store the low byte of the result

        LDA     PLOTY+1         ; Load the high byte of the first number
        ADC     PTEMPW1+1       ; Add the high byte of the second number (plus carry from previous op)
        STA     PLOTY+1         ; Store the high byte of the result
;
; PLOTY IS NOW THE Y OFFSET
; if quad hires double it.
        LDA     VIDEOMODE
        AND     #$10
        CMP     #00
        BNE     :+
        ASL     PLOTY
        ROL     PLOTY+1         ; *2
:
; ADD THE X OFFSET
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PLOTY           ; Load the low byte of the first number into the accumulator
        ADC     PLOTX           ; Add the low byte of the second number (plus carry)
        STA     TEMPW           ; Store the low byte of the result

        LDA     PLOTY+1         ; Load the high byte of the first number
        ADC     PLOTX+1         ; Add the high byte of the second number (plus carry from previous op)
        STA     TEMPW+1         ; Store the high byte of the result
                                ; TEMPW IS NOW THE MEMORY ADDRESS

        LDA     TEMPW+1         ; OK, LET'S CALCULATE THE BANK
        AND     #$F0
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        CLC
        ADC     #$02+VIDEOBANK  ; AREA $2000-?
        CMP     #$F8
        BCC     V_PLOT_HIRES_COLOR_RANGE1
        TAY                     ; VIDEO BANK SHOULD BE IN Y
        JSR     PAGE_ENTER
        LDA     TEMPW+1
        AND     #$0F
        ORA     #$A0
        STA     TEMPW+1         ; TEMPW NOW REFLECTS MAPPED ADDRESSS
        PLA
        AND     #$07            ; WHICH BIT?
        TAX
        LDA     PLOTCOLOR
        CMP     #$01
        BNE     :+
        LDA     HIRES_BIT_LOOKUP_SET,X
        LDY     #$00
        ORA     (TEMPW),Y
        STA     (TEMPW),Y
        JSR     PAGE_EXIT
        RTS
:
        LDA     HIRES_BIT_LOOKUP_RESET,X
        PHA
        LDY     #$00
        AND     (TEMPW),Y
        STA     (TEMPW),Y
        JSR     PAGE_EXIT
        RTS
V_PLOT_HIRES_COLOR_RANGE1:
        JMP     LAB_2564
HIRES_BIT_LOOKUP_SET:
        .BYTE   %10000000,%01000000,%00100000,%00010000,%00001000,%00000100,%00000010,%00000001
HIRES_BIT_LOOKUP_RESET:
        .BYTE   %01111111,%10111111,%11011111,%11101111,%11110111,%11111011,%11111101,%11111110

;___V_PATTERN________________________________________________
;
;  DEFINE GRAPHICS PATTERN
;
;  TAKES 10 PARAMETERS
;       PATTERN NUM (0-255)
;       PATTERN DATA (8 BYTES)
;__________________________________________________________
LAB_PATTERN
V_PATTERN:
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X
        TXA
        PHA
        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        PLA
        STA     VideoCharGenOffset
        LDY     #8
:
        TYA
        PHA
        JSR     LAB_1C01        ; GET THE NEXT PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE NEXT PARAMETER, RETURN IN X
        PLA
        TAY
        TXA
        STA     VideoCharGenData
        DEY
        CPY     #$00
        BNE     :-
        JSR     PAGE_EXIT
        RTS

;___LAB_LINE_______________________________________________
;
;  DRAW LINE USING THE BRESENHAM LINE DRAWING ALGORITHM
;
;  TAKES 5 PARAMETERS
;       START X,Y
;       DESTINATION X,Y
;       COLOR
;__________________________________________________________
LAB_LINE:
; GET X1
        JSR     LAB_EVNM        ; evaluate expression and check is numeric,
                                ; else do type mismatch
        JSR     LAB_F2FX        ; save integer part of FAC1 in temporary integer
        LDA     Itemph
        STA     LINEX1+1
        LDA     Itempl
        STA     LINEX1
                                ; GET Y1
        JSR     LAB_1C01        ; GET THE SECOND PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X
        STX     LINEY1          ; STORE IN LINEY1

; GET X2
        JSR     LAB_1C01        ; GET THE THIRD PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_EVNM        ; evaluate expression and check is numeric,
                                ; else do type mismatch
        JSR     LAB_F2FX        ; save integer part of FAC1 in temporary integer
        LDA     Itemph
        STA     LINEX2+1
        LDA     Itempl
        STA     LINEX2
                                ; GET Y2
        JSR     LAB_1C01        ; GET THE FOURTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; RETURN IN X
        STX     LINEY2          ; STORE IN LINEY2
                                ; GET COLOR
        JSR     LAB_1C01        ; GET THE FIFTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; RETURN IN X
        STX     LINECOLOR       ; STORE IN COLOR
        LDA     #$00
        STA     LINEY1+1
        STA     LINEY2+1
                                ; LINE COORDS AND COLOR STORED IN PROPER PLACE
                                ; CALCULATE DELTAX
        SEC                     ; Set Carry flag (represents "no borrow" for 1st byte)
        LDA     LINEX1          ; Load low byte of number 1
        SBC     LINEX2          ; Subtract low byte of number 2 with carry
        STA     DELTAX          ; Store result low byte
        LDA     LINEX1+1        ; Load high byte of number 1
        SBC     LINEX2+1        ; Subtract high byte of number 2 with borrow
        STA     DELTAX+1        ; Store result high byte
        BCS     :+              ; BRANCH IF X1>=X2
                                ; X1 < X2
        LDA     #0
        STA     PTEMP           ; IF X2>X1 (PTEMP=x0)
        LDA     #$FF            ; TAKE ABSOLUTE VALUE OF DELTAX
        EOR     DELTAX+1        ; Invert high byte (one's complement)
        STA     DELTAX+1
        LDA     #$FF            ;
        EOR     DELTAX          ; Invert low byte
        STA     DELTAX          ;
        INC     DELTAX          ; Add 1 to the low byte
        BNE     :++             ; If no carry from low byte INC, we are done
        INC     DELTAX+1        ; Increment high byte if carry occurred
        JMP     :++
:
        LDA     #1
        STA     PTEMP           ; IF X1>X2 (PTEMP=x1)
:
                                ; CALCULATE DELTAY
        LDA     #0
        STA     DELTAY+1
        SEC                     ; Set Carry flag
        LDA     LINEY1          ; Load low byte of number 1
        SBC     LINEY2          ; Subtract low byte of number 2 with carry
        STA     DELTAY          ; Store result low byte
        BCS     :+              ; BRANCH IF Y1>=Y2
                                ; Y1 < Y2
        LDA     PTEMP
        AND     #1
        STA     PTEMP           ; IF Y2>Y1 (PTEMP=0x)
                                ; TAKE ABSOLUTE VALUE OF DELTAY
        LDA     #$FF
        EOR     DELTAY          ; Flip all bits (one's complement)
        STA     DELTAY
        INC     DELTAY          ; Add 1 (two's complement)
        JMP     :++
:
        LDA     PTEMP
        ORA     #$10
        STA     PTEMP           ; IF Y1>Y2 (PTEMP=1x)
:
                                ; SLOPE DETERMINED, AND DELTAS CALCULATED
                                ; OK, LET'S FIND OUT IF THIS IS A HORIZONTAL-ISH
                                ; OR VERTICAL-ISH LINE
        SEC                     ; Set Carry Flag (for subtraction)
        LDA     DELTAX          ; Load low byte of DELTAX
        SBC     DELTAY          ; Subtract low byte of DELTAY
        LDA     DELTAX+1        ; Load high byte of DELTAX
        SBC     DELTAY+1        ; Subtract high byte of DELTAY (with borrow)
                                ; Flags now reflect the 16-bit comparison
        BCC     LINEVERT        ; If Carry clear, LINE IS VERTICAL-ISH
        JMP     LINEHORZ
LINEVERT:
                                ; FOR A VERTICAL-ISH LINE WE STEP ALONG THE
                                ; Y AXIS (STEEP GRADIENT)
                                ; FIND LARGER Y . . .
        LDA     LINEY1
        CMP     LINEY2
        BCS     :+
        JSR     LINESWAPXY
:
                                ; Initialize error = DELTAY / 2
        LDA     DELTAY
        STA     LINEERR
        LDA     DELTAY+1
        STA     LINEERR+1
        LSR     LINEERR+1
        ROR     LINEERR
LINEVERTLOOP:
        LDA     LINEY1          ; OK LET'S PLOT A POINT
        STA     PLOTY
        LDA     LINEY1+1
        STA     PLOTY+1
        LDA     LINEX1
        STA     PLOTX
        LDA     LINEX1+1
        STA     PLOTX+1
        LDA     LINECOLOR
        STA     PLOTCOLOR
        JSR     DOPLOTXY
                                ; NOW MOVE LINE1 CLOSER TO LINE2
        DEC     LINEY1          ; BY STEPPING ALONG Y AXIS

; BRESENHAM: err -= DELTAX
        SEC
        LDA     LINEERR
        SBC     DELTAX
        STA     LINEERR
        LDA     LINEERR+1
        SBC     DELTAX+1
        STA     LINEERR+1
        BPL     LINEVERT_NOSTEP ; IF ERR >= 0, NO X STEP NEEDED
                                ; ERR += DELTAY (RESTORE ERROR)
        CLC
        LDA     LINEERR
        ADC     DELTAY
        STA     LINEERR
        LDA     LINEERR+1
        ADC     DELTAY+1
        STA     LINEERR+1
                                ; STEP X TOWARD X2 (16-BIT)
        LDA     LINEX2
        CMP     LINEX1
        LDA     LINEX2+1
        SBC     LINEX1+1
        BCC     LINEVERT_DECX   ; X2 < X1, DECREMENT X1
        INC     LINEX1          ; X2 >= X1, INCREMENT X1
        BNE     LINEVERT_NOSTEP
        INC     LINEX1+1
        JMP     LINEVERT_NOSTEP
LINEVERT_DECX:
        LDA     LINEX1
        BNE     :+
        DEC     LINEX1+1
:
        DEC     LINEX1
LINEVERT_NOSTEP:

        LDA     LINEY1
        CMP     LINEY2
        BNE     LINEVERTLOOP
        RTS                     ; DONE

LINEHORZ:
; FOR A HORIZONTAL-ISH LINE WE STEP ALONG THE
; X AXIS (SHALLOW GRADIENT)
; FIND LARGER X . . .
        SEC                     ; Set carry flag for subtraction
        LDA     LINEX2          ; Load low byte of first number
        SBC     LINEX1          ; Subtract low byte of second number
        LDA     LINEX2+1        ; Load high byte of first number
        SBC     LINEX1+1        ; Subtract high byte of second number
                                ; Result flags (N, Z, C) now reflect: Val1 - Val2
        BCS     :+              ; If carry is set, LINEX2 >= LINEX1
        JSR     LINESWAPXY
:
                                ; Initialize error = DELTAX / 2
        LDA     DELTAX
        STA     LINEERR
        LDA     DELTAX+1
        STA     LINEERR+1
        LSR     LINEERR+1
        ROR     LINEERR
LINEHORZLOOP:
        LDA     LINEY1          ; OK LET'S PLOT A POINT
        STA     PLOTY
        LDA     LINEY1+1
        STA     PLOTY+1
        LDA     LINEX1
        STA     PLOTX
        LDA     LINEX1+1
        STA     PLOTX+1
        LDA     LINECOLOR
        STA     PLOTCOLOR
        JSR     DOPLOTXY
                                ; NOW MOVE LINE1 CLOSER TO LINE2
        INC     LINEX1          ; BY STEPPING ALONG X AXIS
        BNE     :+
        INC     LINEX1+1
:
                                ; BRESENHAM: err -= DELTAY
        SEC
        LDA     LINEERR
        SBC     DELTAY
        STA     LINEERR
        LDA     LINEERR+1
        SBC     DELTAY+1
        STA     LINEERR+1
        BPL     LINEHORZ_NOSTEP ; IF ERR >= 0, NO Y STEP NEEDED
                                ; ERR += DELTAX (RESTORE ERROR)
        CLC
        LDA     LINEERR
        ADC     DELTAX
        STA     LINEERR
        LDA     LINEERR+1
        ADC     DELTAX+1
        STA     LINEERR+1
                                ; STEP Y TOWARD Y2
        LDA     LINEY2
        CMP     LINEY1
        BCC     LINEHORZ_DECY   ; Y2 < Y1, DECREMENT
        INC     LINEY1          ; Y2 >= Y1, INCREMENT
        JMP     LINEHORZ_NOSTEP
LINEHORZ_DECY:
        DEC     LINEY1
LINEHORZ_NOSTEP:

        LDA     LINEX1
        CMP     LINEX2
        BNE     LINEHORZLOOP
        LDA     LINEX1+1
        CMP     LINEX2+1
        BNE     LINEHORZLOOP
        RTS                     ; DONE

LINESWAPXY:
        LDA     LINEX1
        LDY     LINEX2
        STY     LINEX1
        STA     LINEX2
        LDA     LINEX1+1
        LDY     LINEX2+1
        STY     LINEX1+1
        STA     LINEX2+1
;
        LDA     LINEY1
        LDY     LINEY2
        STY     LINEY1
        STA     LINEY2
        LDA     LINEY1+1
        LDY     LINEY2+1
        STY     LINEY1+1
        STA     LINEY2+1
        RTS
        .ELSE


; ENSURE ALL OF THESE SPECIAL COMMANDS GIVE ERRORS IN NORMAL BASIC
LAB_SPEEK:
LAB_SPOKE:
LAB_SCRCLR:
LAB_SCREEN:
LAB_PATTERN:
LAB_PLOT:
LAB_LOCATE:
LAB_COLOR:
LAB_LINE:
        LDX     #$02            ; error code $02 ("SYNTAX" error)
        JMP     LAB_XERR        ; do error #X, then warm start

        .ENDIF
