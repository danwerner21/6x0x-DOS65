;__VIDEO DRIVERS________________________________________________________________________________________________________________
;
; 	Video drivers for the memory mapped video card
;
;	Entry points:
;		VIDEOINIT   - called during OS init
;		WRVID	    - write a byte to video port  ('A' POINTS TO BYTE)
;		SETXY	    - Set the xy position of the cursor (X=X,Y=Y)
;		CLEARSCREEN - Set the xy position of the cursor (X=X,Y=Y)
;		SETCOLOR    - Set the xy position of the cursor (X=X,Y=Y)
;________________________________________________________________________________________________________________________________
;

        .SEGMENT "TEA"
        .ORG    $1000

        VIDEOBANK = $F8
        TEMPWORD = $35
        PC6502_ACT_TASK  = $EFE0
; DATA STORAGE
CURX:
        .BYTE   00
CURY:
        .BYTE   00
SHOWCRSR:
        .BYTE   01

CURCOLOR:
        .BYTE   $1E
CSRCOLOR:
        .BYTE   $E1

VIDEOMODE:
        .BYTE   01              ; 00=40 COL, 01=80 COL

VIDEOWORK:
        .BYTE   00,00,00,00,00,00
;*
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
;*

START:
        LDA     #$01
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 01

        JSR     VIDEOINIT

:
        LDX     TMP
        LDA     BANNER,X
        CMP     #$00
        BEQ     :+
        JSR     WRVID
        INC     TMP
        JMP     :-
:

        JSR     RDSER1W
        CMP     #27
        beq     :+
        JSR     WRVID
        JMP     :-
:
        BRK


BANNER:
        .BYTE   "  __  ____   ___ ____    ___  ___",$0D,$0A
        .BYTE   " / /_| ___| / _ \___ \  / _ \/ __\",$0D,$0A
        .BYTE   "| '_ \___ \| | | |__) |/ /_)/ /",$0D,$0A
        .BYTE   "| (_) |__) | |_| / __// ___/ /___",$0D,$0A
        .BYTE   " \___/____/ \___/_____\/   \____/",$0D,$0A,$0D,$0A
        .BYTE   "THIS",08,08,08,08,"THAT WAS A TEST XX",08,08,00
TMP:
        .BYTE   $00
;__VIDEOINIT____________________________________________________________________________________________________________________
;
;	INITIALIZE VIDEO CARD
;________________________________________________________________________________________________________________________________
;
VIDEOINIT:
        LDA     #$01            ; MODIFY TASK 01 (DRIVER TASK)
        LDX     #$0B            ; MAP $BXXX
        LDY     #VIDEOBANK      ; TO $F8XXX
        JSR     $FFF6           ; CALL SETPAGE

        LDA     #$02
        STA     $B006           ; CLEAR LORES MODE
        STA     $B007           ; CLEAR DOUBLE LORES MODE
        STA     $B008           ; CLEAR HIRES MODE
        STA     $B009           ; CLEAR DOUBLE HIRES MODE
        STA     $B00B           ; CLEAR MIXED MODE
        STA     $B00C           ; CLEAR QUAD HIRES MODE
        STA     $B00D           ; CLEAR MONO MODE

        LDA     #$01
        STA     $B001           ; SET TEXT MODE PAGE 1
        STA     $B005           ; SET TEXT MODE
        STA     $B00A           ; SET 80COL MODE

        JSR     CLEARSCREEN
        RTS

;__CLEARSCREEN___________________________________________________________________________________________________________________
;
;	CLEAR VIDEO CARD SCREEN
;________________________________________________________________________________________________________________________________
;
CLEARSCREEN:
        LDA     #$01            ; MODIFY TASK 01 (DRIVER TASK)
        LDX     #$0B            ; MAP $BXXX
        LDY     #VIDEOBANK+1    ; TO $F8XXX
        JSR     $FFF6           ; CALL SETPAGE

        LDA     #$00
        STA     TEMPWORD
        LDA     #$B0
        STA     TEMPWORD+1
        LDY     #$00
; CLEAR CHAR RAM
:
        LDA     #32
        STA     (TEMPWORD),Y
        INC     TEMPWORD
        BNE     :-
        INC     TEMPWORD+1
        LDA     TEMPWORD+1
        CMP     #$B8
        BNE     :-
; CLEAR COLOR   RAM
:
        LDA     CURCOLOR
        STA     (TEMPWORD),Y
        INC     TEMPWORD
        BNE     :-
        INC     TEMPWORD+1
        LDA     TEMPWORD+1
        CMP     #$C0
        BNE     :-

        LDA     #$00
        STA     CURX
        STA     CURY

        LDA     CSRCOLOR
        STA     $B800
        RTS

;__SETCOLOR______________________________________________________________________________________________________________________
;
;	SET COLOR AND CURSOR COLOR
;       X= COLOR
;       Y= CURSOR COLOR
;________________________________________________________________________________________________________________________________
;
SETCOLOR:
        STX     CURCOLOR
        STY     CSRCOLOR
        RTS

;__SETXY_________________________________________________________________________________________________________________________
;
;	SET CURSOR POSITION
;       X= X POSITION
;       Y= Y POSITION
;________________________________________________________________________________________________________________________________
;
SETXY:
        STX     CURX
        STY     CURY
        RTS

;__WRVID_________________________________________________________________________________________________________________________
;
;	WRITE CHARACTER(A) TO VIDEO AT CURRENT X AND Y
;________________________________________________________________________________________________________________________________
;
WRVID:
; NEED TO ADD A CHECK FOR SCROLL
        CMP     #$0D
        BNE     :+
        JSR     UNPAINTCURSOR
        LDA     #$00
        STA     CURX
        JMP     PAINTCURSOR
:
        CMP     #$0A
        BNE     :+
        JSR     UNPAINTCURSOR
        INC     CURY
        JMP     PAINTCURSOR
:
        CMP     #$08
        BNE     WRVIDGO
        JSR     UNPAINTCURSOR
        LDA     CURX
        CMP     #$00
        BNE     WRVID2
        LDA     CURY
        CMP     #$00
        BEQ     WRVIDX
        DEC     CURY
        LDA     VIDEOMODE
        CMP     #01
        BNE     :+
        LDA     #80
        STA     CURX
        JMP     WRVID2
:
        LDA     #40
        STA     CURX
WRVID2:
        DEC     CURX
WRVIDX:
        JSR     GETVIDEOADDRESS
        LDA     VIDEOWORK
        STA     TEMPWORD
        CLC
        LDA     #$B0
        ADC     VIDEOWORK+1
        STA     TEMPWORD+1
        LDY     #$00
        LDA     #32
        STA     (TEMPWORD),Y
        JMP     PAINTCURSOR



WRVIDGO:
        PHA
        JSR     UNPAINTCURSOR
        LDA     VIDEOWORK
        STA     TEMPWORD
        CLC
        LDA     #$B0
        ADC     VIDEOWORK+1
        STA     TEMPWORD+1
        PLA
        LDY     #$00
        STA     (TEMPWORD),Y

        LDX     VIDEOMODE
        CMP     #$00
        BNE     :+
        LDX     CURX
        CPX     #40
        BNE     :++
        INC     CURY
        LDX     #$FF
        JMP     :++
:
        LDX     CURX
        CPX     #80
        BNE     :+
        INC     CURY
        LDX     #$FF
:
        INX
        STX     CURX
        JSR     PAINTCURSOR
        RTS


PAINTCURSOR:
        JSR     GETVIDEOADDRESS
        LDA     VIDEOWORK
        STA     TEMPWORD
        CLC
        LDA     #$B8
        ADC     VIDEOWORK+1
        STA     TEMPWORD+1
        LDA     CSRCOLOR
        LDY     #$00
        STA     (TEMPWORD),Y
        RTS

UNPAINTCURSOR:
        JSR     GETVIDEOADDRESS
        LDA     VIDEOWORK
        STA     TEMPWORD
        CLC
        LDA     #$B8
        ADC     VIDEOWORK+1
        STA     TEMPWORD+1
        LDA     CURCOLOR
        LDY     #$00
        STA     (TEMPWORD),Y
        RTS



GETVIDEOADDRESS:
        LDA     CURY
        STA     VIDEOWORK
        LDA     #$00
        STA     VIDEOWORK+1

        ASL     VIDEOWORK       ; TIMES 2
        ROL     VIDEOWORK+1     ; TIMES 2
        ASL     VIDEOWORK       ; TIMES 4
        ROL     VIDEOWORK+1     ; TIMES 4
        ASL     VIDEOWORK       ; TIMES 8
        ROL     VIDEOWORK+1     ; TIMES 8

        LDA     VIDEOWORK       ; PARK THIS NUMBER FOR THE FUTURE
        STA     VIDEOWORK+2
        LDA     VIDEOWORK+1
        STA     VIDEOWORK+3

        ASL     VIDEOWORK       ; TIMES 16
        ROL     VIDEOWORK+1     ; TIMES 16
        ASL     VIDEOWORK       ; TIMES 32
        ROL     VIDEOWORK+1     ; TIMES 32

        CLC                     ; ADD THE *32 VALUE WITH THE *8 VALUE TO GET *40 :)
        LDA     VIDEOWORK
        ADC     VIDEOWORK+2
        STA     VIDEOWORK+4

        LDA     VIDEOWORK+1
        ADC     VIDEOWORK+3
        STA     VIDEOWORK+5

        LDA     VIDEOWORK+4     ; PLACE BACK IN VIDEOWORK(+1)
        STA     VIDEOWORK
        LDA     VIDEOWORK+5
        STA     VIDEOWORK+1

        LDA     VIDEOMODE       ; IF 40 COLUMN, WE ARE DONE, OTHERWISE DOUBLE AGAIN
        CMP     #$00
        BEQ     :+
        ASL     VIDEOWORK       ; TIMES 2
        ROL     VIDEOWORK+1     ; TIMES 2
:
        CLC                     ; NOW ADD IN X COORD
        LDA     CURX
        ADC     VIDEOWORK
        STA     VIDEOWORK
        LDA     #$00
        ADC     VIDEOWORK+1
        STA     VIDEOWORK+1
        RTS


;--------------------------------------------------

UART1DATA       = $EF84; SERIAL PORT 1 (I/O Card)
UART1STATUS     = $EF85; SERIAL PORT 1 (I/O Card)
UART1COMMAND    = $EF86; SERIAL PORT 1 (I/O Card)
UART1CONTROL    = $EF87; SERIAL PORT 1 (I/O Card)

;__RDSER1________________________________________________________________________________________________________________________
;
;	READ CHARACTER FROM UART TO (A)
;________________________________________________________________________________________________________________________________
;
RDSER1:
        LDA     UART1STATUS     ; GET STATUS REGISTER
        AND     #%00001000      ; IS RX READY
        BEQ     RDSER1N         ; NO, INDICATE NO CHAR
        LDA     UART1DATA       ; GET DATA CHAR
        RTS
RDSER1N:
        LDA     #$00            ;
        RTS                     ;

;__RDSER1W_______________________________________________________________________________________________________________________
;
;	READ CHARACTER FROM UART TO (A) - WAIT FOR CHAR
;________________________________________________________________________________________________________________________________
;
RDSER1W:
        JSR     RDSER1
        CMP     #$00
        BEQ     RDSER1W
        AND     #$7F
        RTS
