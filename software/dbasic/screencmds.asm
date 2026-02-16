HIRES PLOT DOES NOT WORK
NEED TO TEST HIRES MONO PLOT
NEED TO TEST PATTERN

THEN OTHER SCREEN COMMANDS (PRINT, COLOR, CLEAR, LOCATE, ETC . . . )
SCREEN EDITOR
CIRCLE?
BOX?
LINE?

SID COMMANDS
RTC COMMANDS

DOS COMMANDS
FILE IO . . .
DIRECTORY . . .




        .IFDEF  MEMORYMAPPEDSCREEN

;* MEMORY MAP
;  VIDEO CARD IS A 32K AREA
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

VideoDisplayPage = $B001
VideoCharGenOffset = $B002
VideoCharGenData = $B003
VideoTextMode   = $B005
VideoLoresMode  = $B006
VideoDoubleLores = $B007
VideoHiresMode  = $B008
VideoDoubleHires = $B009
Video80col      = $B00A
VideoMixedMode  = $B00b
VideoQuadHires  = $B00c
VideoMonoHires  = $B00d


VIDEOMODE:
        .BYTE   $00
CLRTMP:
        .BYTE   $00
VIDEOWIDTH:
        .BYTE   $00

PTEMPW:
        .BYTE   $00,$00
PTEMPW1:
        .BYTE   $00,$00
PTEMPW2:
        .BYTE   $00,$00
PTEMPW3:
        .BYTE   $00,$00
PTEMP:
        .BYTE   $00

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
        JSR     PRINT_BYTE
        TAY
        AND     #$0F
        ORA     #$B0
        STA     Itemph
        JSR     PRINT_BYTE
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
        JSR     PRINT_BYTE
        LDA     #$01            ; MODIFY TASK 01 (DRIVER TASK)
        LDX     #$0B            ; MAP $BXXX
        JSR     $FFF6           ; CALL SETPAGE
        LDA     #$01
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 01
        LDY     #0
        LDA     (Itempl),Y
        JSR     PRINT_BYTE
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
        ORA     #$B0
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
        JMP     SETUPMODE0_CLEAR
SETUPMODE0_80:
        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        LDA     #$01
        STA     Video80col
        JSR     PAGE_EXIT
        LDA     #80
        STA     VIDEOWIDTH
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
        LDA     #$B0
        STA     TEMPW+1
        LDY     #$00
        LDA     #$00
:
        STA     (TEMPW),Y
        INC     TEMPW
        BNE     :-
        INC     TEMPW+1

        LDX     TEMPW+1
        CPX     #$B8
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
        LDA     #$B0
        STA     TEMPW+1
        LDA     #$00
:
        STA     (TEMPW),Y
        INC     TEMPW
        BNE     :-
        INC     TEMPW+1
        LDX     TEMPW+1
        CPX     #$C0
        BNE     :-
        LDX     #$B0
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

LAB_SCRCLR:
V_SCNCLR:
        RTS

PAGE_EXIT:
        LDA     #$00
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 00
        RTS

PAGE_ENTER:
; Set "Y" register to video page ($YXXX)
        LDA     #$01            ; MODIFY TASK 01 (DRIVER TASK)
        LDX     #$0B            ; MAP $BXXX
        JSR     $FFF6           ; CALL SETPAGE
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
        LDA     VIDEOMODE
        AND     #$0F
        CMP     #$01
        BEQ     V_PLOT_LORES
        LDA     VIDEOMODE
        AND     #$2F
        CMP     #$02
        BNE     :+
        JMP    V_PLOT_HIRES_COLOR
:
        CMP     #$22
        BNE     :+
        JMP     V_PLOT_HIRES_MONO
:
        RTS

V_PLOT_LORES:
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X
        STX     PTEMPW          ; STORE X COORD IN OFFSET ADDRESS
        LDA     #00
        STA     PTEMPW+1
        STA     PTEMPW1+1
        JSR     LAB_1C01        ; GET THE SECOND PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X
                                ; FIGURE THE BUFFER OFFSET
        TXA                     ; Y COORD IN X
        PHA
        LSR     A               ; 2 LINES PER BYTE
        STA     PTEMPW1
                                ; MULTIPLY Y (PTEMPW) COORD BY 40 OR 80 (SINGLE OR DOUBLE LORES)
        CLC
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*4)
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*8)
        LDA     PTEMPW1
        STA     PTEMPW2
        LDA     PTEMPW1+1
        STA     PTEMPW2+1       ; STORE Y*8 INTO PTEMPW2
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*16)
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*32)
                                ; NOW TAKE Y*8(PTEMPW2) + Y*32(PTEMPW1) == Y*40 STORE IN PTEMPW1
        CLC                     ; Clear the Carry flag before the first addition

        LDA     PTEMPW1         ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW2         ; Add the low byte of the second number (plus carry)
        STA     PTEMPW1         ; Store the low byte of the result

        LDA     PTEMPW1+1       ; Load the high byte of the first number
        ADC     PTEMPW2+1       ; Add the high byte of the second number (plus carry from previous op)
        STA     PTEMPW1+1       ; Store the high byte of the result

; if double lores columns double it.
        LDA     VIDEOMODE
        AND     #$10
        CMP     #00
        BEQ     :+
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2
:
                                ; OK, Y OFFSET IS IN PTEMPW1, X IS IN PTEMPW
        LDA     #$B0            ; ADD THE MEMORY OFFSET TO PTEMPW (BECAUSE IT IS EASY)
        STA     PTEMPW+1        ; THEN ADD PTEMPW1 TO PTEMPW THAT SHOULD BE THE MEMORY ADDRESS TO UPDATE
                                ;
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PTEMPW          ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW1         ; Add the low byte of the second number (plus carry)
        STA     TEMPW           ; Store the low byte of the result

        LDA     PTEMPW+1        ; Load the high byte of the first number
        ADC     PTEMPW1+1       ; Add the high byte of the second number (plus carry from previous op)
        STA     TEMPW+1         ; Store the high byte of the result
                                ; PTEMPW IS THE MEMORY OFFSET TO UPDATE
        JSR     LAB_1C01        ; GET THE THIRD PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE THIRD PARAMETER, RETURN IN X (PATTERN)
        TXA
        AND     #$0F
        STA     PTEMP           ; SAVE COLOR IN PTEMP
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
        LDA     PTEMP
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        STA     PTEMP
        PLA
        ORA     PTEMP
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
        ORA     PTEMP
        STA     (TEMPW),Y       ; STORE  DOUBLE PIXEL
        JSR     PAGE_EXIT
        RTS

V_PLOT_HIRES_COLOR:
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X
        TXA
        PHA
        LSR     A               ; 2 PIXEL PER BYTE
        STA     PTEMPW          ; STORE X COORD IN OFFSET ADDRESS
        LDA     #$00
        STA     PTEMPW+1
        STA     PTEMPW1+1
        JSR     LAB_1C01        ; GET THE SECOND PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X
                                ; FIGURE THE BUFFER OFFSET
        LDA     #$00

        TXA                     ; GET Y COORD
        STX     PTEMPW1
        CLC                     ; MULTIPLY Y COORD BY 70 OR 140 (SINGLE OR DOUBLE HIRES)
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2
        LDA     PTEMPW1
        STA     PTEMPW2
        LDA     PTEMPW1+1
        STA     PTEMPW2+1       ; STORE Y*2 INTO PTEMPW2
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*4)
        LDA     PTEMPW1
        STA     PTEMPW3
        LDA     PTEMPW1+1
        STA     PTEMPW3+1       ; STORE Y*4 INTO PTEMPW3
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*8)
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*16)
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*32)
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*64)
                                ; RESULT OFFSET PTEMPW1 = PTEMPW1(Y*64)+PTEMPW2(Y*2)+PTEMPW3(Y*4)
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PTEMPW1         ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW2         ; Add the low byte of the second number (plus carry)
        STA     PTEMPW1         ; Store the low byte of the result

        LDA     PTEMPW1+1       ; Load the high byte of the first number
        ADC     PTEMPW2+1       ; Add the high byte of the second number (plus carry from previous op)
        STA     PTEMPW1+1       ; Store the high byte of the result
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PTEMPW1         ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW3         ; Add the low byte of the second number (plus carry)
        STA     PTEMPW1         ; Store the low byte of the result

        LDA     PTEMPW1+1       ; Load the high byte of the first number
        ADC     PTEMPW3+1       ; Add the high byte of the second number (plus carry from previous op)
        STA     PTEMPW1+1       ; Store the high byte of the result

; if double hires double it.
        LDA     VIDEOMODE
        AND     #$10
        CMP     #00
        BEQ     :+
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*4)
:
                                ; PTEMPW1 IS NOW Y OFFSET, ADD X OFFSET FOR MEMORY ADDRESS INTO TEMPW
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PTEMPW1         ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW          ; Add the low byte of the second number (plus carry)
        STA     TEMPW           ; Store the low byte of the result

        LDA     PTEMPW1+1        ; Load the high byte of the first number
        ADC     PTEMPW+1        ; Add the high byte of the second number (plus carry from previous op)
        STA     TEMPW+1         ; Store the high byte of the result

        JSR     LAB_1C01        ; GET THE THIRD PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE THIRD PARAMETER, RETURN IN X (PATTERN)
        TXA
        AND     #$0F
        STA     PTEMP          ; SAVE COLOR IN TEMP
        LDA     TEMPW           ; OK, LET'S CALCULATE THE BANK
        AND     #$F0
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        CLC
        ADC     #$02+VIDEOBANK  ; AREA $2000-?
        TAY                     ; VIDEO BANK SHOULD BE IN Y
        JSR     PAGE_ENTER
        LDA     TEMPW+1
        AND     #$0F
        ORA     #$B0
        STA     TEMPW+1         ; TEMPW NOW REFLECTS MAPPED ADDRESSS
        PLA
        LSR     A               ; LEFT OR RIGHT PIXEL?
        BCC     :+
                                ; LEFT PIXEL
        LDY     #$00
        LDA     (TEMPW),Y       ; GET EXISTING DOUBLE PIXEL
        AND     #$0F
        PHA
        LDA     PTEMP
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        STA     PTEMP
        PLA
        ORA     PTEMP
        STA     (TEMPW),Y       ; WRITE THE PIXEL BACK OUT
        JSR     PAGE_EXIT
        RTS
:
; RIGHT PIXEL
        LDY     #$00
        LDA     (TEMPW),Y       ; GET EXISTING DOUBLE PIXEL
        AND     #$F0
        ORA     PTEMP
        STA     (TEMPW),Y       ; WRITE THE PIXEL BACK OUT
        JSR     PAGE_EXIT
        RTS

V_PLOT_HIRES_MONO:
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X
        TXA
        PHA
        LSR     A               ; 8 PIXEL PER BYTE
        LSR     A
        LSR     A

        STA     PTEMPW         ; STORE X COORD IN OFFSET ADDRESS
        LDA     #00
        STA     PTEMPW+1
        STA     PTEMPW1+1
        STA     PTEMPW3+1
        JSR     LAB_1C01        ; GET THE SECOND PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X
                                ; FIGURE THE BUFFER OFFSET
        TXA                     ; GET Y COORD
        STX     PTEMPW1
        STX     PTEMPW3
                                ; MULTIPLY Y COORD BY 35 OR 70 (MONO OR QUAD HIRES)
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2
        LDA     PTEMPW1
        STA     PTEMPW2
        LDA     PTEMPW1+1
        STA     PTEMPW2+1       ; STORE *2 INTO PTEMPW2
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*4)
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*8)
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*16)
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2 (*32)
                                ;
                                ; PTEMPW1(Y*35) = PTEMPW1(Y*32)+PTEMPW2(Y*2)+PTEMPW3(Y)
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PTEMPW1         ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW2         ; Add the low byte of the second number (plus carry)
        STA     PTEMPW1         ; Store the low byte of the result

        LDA     PTEMPW1+1       ; Load the high byte of the first number
        ADC     PTEMPW2+1       ; Add the high byte of the second number (plus carry from previous op)
        STA     PTEMPW1+1       ; Store the high byte of the result
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PTEMPW1         ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW3         ; Add the low byte of the second number (plus carry)
        STA     PTEMPW1         ; Store the low byte of the result

        LDA     PTEMPW1+1       ; Load the high byte of the first number
        ADC     PTEMPW3+1       ; Add the high byte of the second number (plus carry from previous op)
        STA     PTEMPW1+1       ; Store the high byte of the result
                                ;
                                ; PTEMPW1 IS NOW THE Y OFFSET
                                ; if quad hires double it.
        LDA     VIDEOMODE
        AND     #$10
        CMP     #00
        BNE     :+
        ASL     PTEMPW1
        ROL     PTEMPW1+1       ; *2
:
                                ; ADD THE X OFFSET
        CLC                     ; Clear the Carry flag before the first addition
        LDA     PTEMPW1         ; Load the low byte of the first number into the accumulator
        ADC     PTEMPW          ; Add the low byte of the second number (plus carry)
        STA     TEMPW           ; Store the low byte of the result

        LDA     PTEMPW1+1       ; Load the high byte of the first number
        ADC     PTEMPW+1        ; Add the high byte of the second number (plus carry from previous op)
        STA     TEMPW+1         ; Store the high byte of the result
                                ; TEMPW IS NOW THE MEMORY ADDRESS
        JSR     LAB_1C01        ; GET THE THIRD PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE THIRD PARAMETER, RETURN IN X (PATTERN)
        LDA     TEMPW           ; OK, LET'S CALCULATE THE BANK
        AND     #$F0
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        CLC
        ADC     #$02+VIDEOBANK  ; AREA $2000-?
        TAY                     ; VIDEO BANK SHOULD BE IN Y
        JSR     PAGE_ENTER
        LDA     TEMPW+1
        AND     #$0F
        ORA     #$B0
        STA     TEMPW+1         ; TEMPW NOW REFLECTS MAPPED ADDRESSS
        TXA
        AND     #$01
        STA     PTEMP          ; SAVE COLOR IN TEMP
        PLA
        AND     #$07            ; WHICH BIT?
        TAX
        LDA     PTEMP
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
        LDY     #$00+VIDEOBANK  ; AREA $0000-$0FFF
        JSR     PAGE_ENTER
        TXA
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



        .ELSE


; ENSURE ALL OF THESE SPECIAL COMMANDS GIVE ERRORS IN NORMAL BASIC
LAB_SPEEK:
LAB_SPOKE:
LAB_SCRCLR:
LAB_SCREEN:
LAB_PATTERN:
LAB_PLOT:
        LDX     #$02            ; error code $02 ("SYNTAX" error)
        JMP     LAB_XERR        ; do error #X, then warm start

        .ENDIF
