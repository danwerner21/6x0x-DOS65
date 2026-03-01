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
;               SCROLLUP    - Scroll the screen up one line
;               SETMODE     - Set 40/80 mode
;________________________________________________________________________________________________________________________________
;

VIDEOBANK       = $F8

; DATA STORAGE
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

        LDA     #$00
        STA     $B006
        LDA     $B006
        CMP     #$00
        BNE     VIDEOINIT_FAIL
        LDA     #$FF
        STA     $B006
        LDA     $B006
        CMP     #$FF
        BNE     VIDEOINIT_FAIL

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
        LDA     #$01
        STA     $B00A           ; SET 80COL MODE

        LDA     #$01
        STA     SHOWCRSR        ; SHOW CURSOR (1-YES, 0-NO)
        LDA     #$1E
        STA     CURCOLOR        ; CURRENT PRINT COLOR
        LDA     #$E1
        STA     CSRCOLOR        ; CURRENT CURSOR COLOR
        LDA     #$01
        STA     VIDEOMODE       ; 00=40 COL, 01=80 COL

        JSR     CLEARSCREEN

        JSR     LFCR            ; CRLF
        LDA     #<VIDEOMESSAGE1 ;
        STA     STRPTR          ;
        LDA     #>VIDEOMESSAGE1 ;
        STA     STRPTR+1        ;
        JSR     WRSTR

        LDA     #<VIDEOMESSAGE3 ;
        STA     STRPTR          ;
        LDA     #>VIDEOMESSAGE3 ;
        STA     STRPTR+1        ;
        JSR     WRSTR
        JSR     LFCR            ; AND CRLF

        RTS

VIDEOINIT_FAIL:
        JSR     LFCR            ; CRLF
        LDA     #<VIDEOMESSAGE1 ;
        STA     STRPTR          ;
        LDA     #>VIDEOMESSAGE1 ;
        STA     STRPTR+1        ;
        JSR     WRSTR

        LDA     #<VIDEOMESSAGE3 ;
        STA     STRPTR          ;
        LDA     #>VIDEOMESSAGE3 ;
        STA     STRPTR+1        ;
        JSR     WRSTR
        JSR     LFCR            ; AND CRLF
        RTS


;__SETMODE_______________________________________________________________________________________________________________________
;
;	SET 40/80 VIDEO MODE  (0=40/1=80)
;________________________________________________________________________________________________________________________________
;
SETMODE:
        STA     VIDEOMODE
        LDA     #$01            ; MODIFY TASK 01 (DRIVER TASK)
        LDX     #$0B            ; MAP $BXXX
        LDY     #VIDEOBANK      ; TO $F8XXX
        JSR     $FFF6           ; CALL SETPAGE
        LDA     #$01
        STA     $B001           ; SET TEXT MODE PAGE 1
        STA     $B005           ; SET TEXT MODE
        LDA     VIDEOMODE
        CMP     #$00
        BEQ     :+
        LDA     #$01
        STA     $B00A           ; SET 80COL MODE
        JMP     CLEARSCREEN
:
        LDA     #$02
        STA     $B00A           ; SET 40COL MODE
        JMP     CLEARSCREEN


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


WRVIDTMP:
        .BYTE    $00

;__WRVID_________________________________________________________________________________________________________________________
;
;	WRITE CHARACTER(A) TO VIDEO AT CURRENT X AND Y
;________________________________________________________________________________________________________________________________
;
WRVID:
        STA    WRVIDTMP
        PHA
        TXA
        PHA
        TYA
        PHA
        LDA     WRVIDTMP
        CMP     #$0D
        BNE     :+
        JSR     UNPAINTCURSOR
        LDA     #$00
        STA     CURX
        JSR     PAINTCURSOR
        PLA
        TAY
        PLA
        TAX
        PLA
        RTS
:
        CMP     #$0A
        BNE     :+
        JSR     UNPAINTCURSOR
        INC     CURY
        LDA     CURY
        CMP     #24
        BEQ     SCROLLV
        JSR     PAINTCURSOR
        PLA
        TAY
        PLA
        TAX
        PLA
        RTS
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
        LDA     VIDEOWORK
        STA     TEMPWORD
        CLC
        LDA     #$B8
        ADC     VIDEOWORK+1
        STA     TEMPWORD+1
        LDA     CURCOLOR
        STA     (TEMPWORD),Y
        JSR     PAINTCURSOR
        PLA
        TAY
        PLA
        TAX
        PLA
        RTS


SCROLLV:
        JSR     SCROLLUP
        LDA     #23
        STA     CURY
        LDA     #0
        STA     CURX
        JSR     PAINTCURSOR
        PLA
        TAY
        PLA
        TAX
        PLA
        RTS



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
        LDA     VIDEOWORK
        STA     TEMPWORD
        CLC
        LDA     #$B8
        ADC     VIDEOWORK+1
        STA     TEMPWORD+1
        LDA     CURCOLOR
        STA     (TEMPWORD),Y

        LDX     CURX
        LDA     VIDEOMODE
        CMP     #$00
        BNE     :+
        CPX     #39
        BNE     :++
        INC     CURY
        LDX     #$FF
        JMP     :++
:
        CPX     #79
        BNE     :+
        INC     CURY
        LDX     #$FF
:
        INX
        STX     CURX
        LDA     CURY
        CMP     #24
        BEQ     SCROLLV
        JSR     PAINTCURSOR
        PLA
        TAY
        PLA
        TAX
        PLA
        RTS



PAINTCURSOR:
        LDA     SHOWCRSR
        BEQ     :+
FPAINTCURSOR:
        JSR     GETVIDEOADDRESS
        LDA     VIDEOWORK
        STA     TEMPWORD
        CLC
        LDA     #$B8
        ADC     VIDEOWORK+1
        STA     TEMPWORD+1

        LDY     #$00
        LDA     (TEMPWORD),Y
        STA     UNDERCRSR
        LDA     CSRCOLOR
        STA     (TEMPWORD),Y
        RTS
:
        JSR     GETVIDEOADDRESS
        LDA     VIDEOWORK
        STA     TEMPWORD
        CLC
        LDA     #$B8
        ADC     VIDEOWORK+1
        STA     TEMPWORD+1
        LDY     #$00
        LDA     (TEMPWORD),Y
        STA     UNDERCRSR
        RTS

UNPAINTCURSOR:
        JSR     GETVIDEOADDRESS
        LDA     VIDEOWORK
        STA     TEMPWORD
        CLC
        LDA     #$B8
        ADC     VIDEOWORK+1
        STA     TEMPWORD+1
        LDA     UNDERCRSR
        LDY     #$00
        STA     (TEMPWORD),Y
        RTS


;__SCROLLUP______________________________________________________________________________________________________________________
;
;	SCROLL THE SCREEN UP
;
;
;________________________________________________________________________________________________________________________________
;
SCROLLUP:
        LDA     #$B0                    ; SCREEN MEMORY STARTS AT $B000
        STA     TEMPWORD+1              ;
        STA     TEMPWORD1+1             ;
        LDA     #$B8                    ; COLOR MEMORT STARTS AT $B800
        STA     TEMPWORD2+1             ;
        STA     TEMPWORD3+1             ;
        LDA     #$00                    ;
        STA     TEMPWORD                ;
        STA     TEMPWORD2               ;

        LDA     VIDEOMODE               ; 1=80 COL, 0=40 COL
        CMP     #01
        BNE     :+
        LDA     #80
        STA     TEMPWORD1
        STA     TEMPWORD3
        JMP     SCROLLUP_G
:
        LDA     #40
        STA     TEMPWORD1
        STA     TEMPWORD3


SCROLLUP_G:
        LDY     #$00                    ; INDEX ALWAYS =0
; SCROLL UP 40/80 CHARACTERS AND COLOR
SCROLLUP_G1:
        LDA     (TEMPWORD1),Y           ; READ FROM SCREEN+X
        STA     (TEMPWORD),Y            ; WRITE TO SCREEN
        LDA     (TEMPWORD3),Y           ; READ FROM COLOR+X
        STA     (TEMPWORD2),Y           ; WRITE TO COLOR

        INC     TEMPWORD2
        BNE     :+
        INC     TEMPWORD2+1
:
        INC     TEMPWORD
        BNE     :+
        INC     TEMPWORD+1
:
        INC     TEMPWORD3
        BNE     :+
        INC     TEMPWORD3+1
:
        INC     TEMPWORD1
        BNE     :+
        INC     TEMPWORD1+1
:
        LDA     TEMPWORD1+1
        CMP     #$B8
        BNE     SCROLLUP_G1


; CLEAR BOTTOM LINE.
        LDA     VIDEOMODE
        CMP     #01
        BEQ     :+
        LDA     #$98
        STA     TEMPWORD
        STA     TEMPWORD1
        LDA     #$B3
        STA     TEMPWORD+1
        LDA     #$BB
        STA     TEMPWORD1+1

        JMP     SCROLLUP_C
:
        LDA     #$30
        STA     TEMPWORD
        STA     TEMPWORD1
        LDA     #$B7
        STA     TEMPWORD+1
        LDA     #$BF
        STA     TEMPWORD1+1

SCROLLUP_C:
        LDA     #32
        STA     (TEMPWORD),Y
        LDA     CURCOLOR
        STA     (TEMPWORD1),Y
        INY
        CPY     #80
        BNE     SCROLLUP_C
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

        .IFNDEF PC6502BIOS
;
; DRIVER DATA
;__________________________________________________________________________________________________
; MESSAGES
;__________________________________________________________________________________________________
VIDEOMESSAGE1:
        .BYTE   "MEMORY MAPPED VIDEO:",$0D,$0A
        .BYTE   " BANK=0xF8 "
        .BYTE   00
VIDEOMESSAGE2:
        .BYTE   "NOT "
VIDEOMESSAGE3:
        .BYTE   "FOUND."
        .BYTE   00
        .ENDIF