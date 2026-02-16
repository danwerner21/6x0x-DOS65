;
;
;

;The initialization routine sets up the
;memory map, clears out certain flags,
;and enables the raster interrupt

INIT:
	; set 80 col text mode here
        ldx 	#80
        STX     COLUMNS
	ldy 	#25
        STY     ROWS
	; set colors here
	; clear screen here

        LDA     #TOPFGCOLOR
        STA     INSMODE
        STZ     texstart
        STZ     texend
        STZ     texbuf
        STZ     bufend
        STZ     huntlen
        STZ     replen
        LDA     #>END           ;
        INC
        STA     texstart+1
        LDA     #>TEXEND_INIT
        STA     texend+1
        LDA     #<TEXBUF_INIT
        STA     texbuf+1
        LDA     #<BUFEND_INIT
        STA     bufend+1
        STA     fpos+1
	RTS
INIT2:
        JSR     killbuff
; moved forward to match binary
;       TODO: confirm or convert 650/$9D settings
INIT3:
        LDA     #128
        COPY16  texstart,curr
; display program title
        JSR     sysmsg
; display author name
        PRINTMESSAGE MSG2
        INC     msgflg
        RTS

;sysmsg displays "SpeedScript" and the version.
sysmsg:
        LDA     INSMODE
        STA     windcolr
        JSR     topclr
        PRINTMESSAGE MSG1
        STZ     msgflg
        RTS

;topclr keeps the command line clean.
;It is called before most messages.
;It's like a one-line clear-screen.
topclr:
;        LDX     COLUMNS         ; columns
;        LDA     #V_H_INC1       ; write both text & color
;        STA     V_H
;        STZ     V_M
;        STZ     V_L
;        LDA     #space
;        LDY     windcolr
;        STY     646
;toploop:
;        STA     V_1
;        STY     V_1
;        DEX
;        BPL     toploop
;        LDA     #19             ;HOME
;        JMP     chrout
;Converts PETSCII to screen codes.
;astoin:
;        PHA
;        AND     #128
;        LSR
;        STA     temp
;        PLA
;        AND     #63
;        ORA     temp
;        RTS


chrin:
        stx     chrouttmpx
        LDX     #6              ;
        JSR     PEM             ;
        ldx     chrouttmpx
        rts

chrout:
        stx     chrouttmpx
        LDX     #2              ;  OUTPUT THE CHAR TO THE CONSOLE
        JSR     PEM             ;
        ldx     chrouttmpx
        rts
chrouttmpx:
        .byte 00


;refresh copies a screenful of text
;from the area of memory pointed to by
;toplin. It works like a printer routine,
;fitting a line of text between the screen
;margins, wrapping words, and restarts
;at the left margin after printing a car-
;riage return. SpeedScript constantly calls
;this routine while the cursor is blink-
;ing, so it has to be very fast. To elimi-
;nate flicker, it also clears out the end of
;each line instead of first clearing the
;screen. It stores the length of the first
;screen line for the sake of the check
;routine (which scrolls up by adding
;that length to toplin), the last text
;location referenced (so check can see
;if the cursor has moved off the visible
;screen).


refresh:
; set topline and border color elsewhere

; set pointer tex to top of document
        COPY16  toplin,tex
; set VERA to skip color memory
;        LDA     #$20
;        STA     V_H
;; set VERA pointer to beginning of 2nd line
;        LDX     #1
;        STX     V_M
;        STZ     V_L
;PPAGE:
;        LDY     #0
;PLINE:
;        LDA     (tex),Y
;        STA     lbuff,Y
;        INY
;        AND     #127
;        CMP     #retchar
;        BEQ     BREAK
;        CPY     COLUMNS         ;COLUMNS
;        BNE     PLINE
; hit column 39 without end-of-paragraph
; backspace until it hits a space
;        DEY
;SLOOP:
;        LDA     (tex),Y
;        AND     #127
;NXCUR:
;        CMP     #space
;        BEQ     SBRK            ; wrap at this character
;        DEY
;        BNE     SLOOP
;        LDY     COLUMNS         ; columns
;        DEY
;; copy line onto screen
;SBRK:
;        INY
;BREAK:
;        STY     temp
;        LDY     #0
;COPY:
;        LDA     lbuff,Y
;        STA     V_1
;        INY
;        CPY     temp
;        BMI     COPY
;        LDY     temp
;        CLC
;        TYA
;        ADC     tex
;        STA     tex
;        LDA     tex+1
;        ADC     #0
;        STA     tex+1
;        CPX     #1
;        BNE     CLRLN
;        STY     LENTABLE
;; fill rest of line with spaces
;CLRLN:
;        CPY     COLUMNS         ; columns
;        BEQ     CLEARED
;        LDA     #32
;        STA     V_1
;        INY
;        BRA     CLRLN
;CLEARED:
;; move screen pointer to next row
;        STZ     V_L
;        INC     V_M
;        INX
;        CPX     ROWS            ; rows
;        BEQ     pdone
;        BRA     PPAGE
;pdone:
;        COPY16  tex,BOTSCR
        RTS

BORDER:
;        LDA     TEXCOLR
;        CLC
;        ADC     #$10
;        STA     TEXCOLR
COLORALL:
;        LDA     #$20
;        STA     V_H
;        LDX     #1
;        LDA     TEXCOLR
;colorrow:
;        STX     V_M
;        LDY     #1
;        STY     V_L
;        LDY     COLUMNS         ;Columns
;colorcol:
;        STA     V_1
;        DEY
;        BNE     colorcol
;        INX
;        CPX     ROWS            ;ROWS
;        BNE     colorrow
        RTS

scrcol:
;        .BYTE   12              ; gray
;;TEXCOLR (text color) is used in the refresh routine
;;and stored into color memory. Both SCRCOL and TEXCOLR
;;are stored within the SpeedScript code so that after
;;they're changed, you can resave SpeedScript and it
;;will come up with your color choice in the future.
LETTERS:
;        LDA     TEXCOLR
;        INA
;        AND     #$0F
;        TAX
;        LDA     TEXCOLR
;        AND     #$F0
;        STX     TEXCOLR
;        ORA     TEXCOLR
;        STA     TEXCOLR
;        BRA     COLORALL
;TEXCOLR:
;        .BYTE   $cb             ;dark gray on light gray


        rts