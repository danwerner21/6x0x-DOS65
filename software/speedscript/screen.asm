;
;
;

;The initialization routine sets up the
;memory map, clears out certain flags,
;and enables the raster interrupt

INIT:
; set 80 col text mode here
        LDX     #80
        STX     COLUMNS
; Use 23 rows so refresh paints rows 1-22 only.
; Row 0 = banner, row 23 = unused. Screen has 24 rows (0-23).
; Painting row 23 wraps cursor to row 24 which auto-scrolls.
        LDY     #23
        STY     ROWS
; ensure 80 col
        LDA     #FC_SETMODE
        STA     farfunct
        LDA     #1
        JSR     DO_FARCALL
; set colors
        LDA     #FC_COLOR
        STA     farfunct
        LDX     #SCNCOLOR
        LDY     #CRCOLOR
        JSR     DO_FARCALL
; clear screen
        LDA     #FC_SCNCLR
        STA     farfunct
        JSR     DO_FARCALL
; init default cursor tracking
        LDA     #1
        STA     csrrow
        STZ     csrcol

        STZ     INSMODE
        STZ     texstart
        STZ     texend
        STZ     texbuf
        STZ     bufend
        STZ     huntlen
        STZ     replen
        STZ     SHOWCRSR        ; TURN OFF CURSOR

        LDA     #>END           ;
        INC
        STA     texstart+1
        LDA     #>TEXEND_INIT
        STA     texend+1
        LDA     #>TEXBUF_INIT
        STA     texbuf+1
        LDA     #>BUFEND_INIT
        STA     bufend+1
        STA     fpos+1
        RTS
INIT2:
        JSR     killbuff
; moved forward to match binary
;       TODO: confirm or convert 650/$9D settings
INIT3:
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
        CMP     #1
        BNE     :+
        LDA     #FC_COLOR
        STA     farfunct
        LDX     #TOPCOLOR
        LDY     #CRCOLOR
        JSR     DO_FARCALL
        JMP     :++
:
        LDA     #FC_COLOR
        STA     farfunct
        LDX     #INSCOLOR
        LDY     #CRCOLOR
        JSR     DO_FARCALL
:
        JSR     topclr
        PRINTMESSAGE MSG1
        STZ     msgflg
        RTS

;topclr keeps the command line clean.
;It is called before most messages.
;It's like a one-line clear-screen.
topclr:
; map video text page into $A000-$AFFF
        LDY     #VIDTEXT_PAGE
        JSR     vid_enter
; fill row 0 text ($A000-$A04F) with spaces
        LDY     #79
        LDA     #space
@tcloop:
        STA     $A000,Y
        DEY
        BPL     @tcloop
; fill row 0 color ($A800-$A84F) with current color
        LDY     #79
        LDA     #TOPCOLOR
@ccloop:
        STA     $A800,Y
        DEY
        BPL     @ccloop
; unmap video
        JSR     vid_exit
; position firmware cursor at row 0, col 0 for PRINTMESSAGE
        LDX     #0
        LDY     #0
        LDA     #FC_LOCATE
        STA     farfunct
        JSR     DO_FARCALL
        RTS


chrin:
        PHX
        PHY
        LDA     #FC_CHRIN       ; FARCALL #20 - read keyboard (non-blocking)
        STA     farfunct
        JSR     DO_FARCALL      ; returns A=key or A=$FF if no key
        PLY
        PLX
        CMP     #$FF
        BNE     @gotkey
        LDA     #0              ; return 0 for "no key"
@gotkey:
        RTS

chrout:
        PHX
        PHY
        PHA                     ; save character
        LDA     #FC_CHROUT      ; FARCALL #19 - output char
        STA     farfunct
        PLA                     ; restore character to A
        JSR     DO_FARCALL
        PLY
        PLX
        RTS

; drain all pending keystrokes from the keyboard buffer
drainkeys:
        LDX     #11             ; keyboard status
        JSR     PEM
        CMP     #0
        BEQ     @drained
        LDX     #6              ; consume the key
        JSR     PEM
        JMP     drainkeys
@drained:
        RTS

;refresh copies a screenful of text
;from the area of memory pointed to by
;toplin. It works like a printer routine,
;fitting a line of text between the screen
;margins, wrapping words, and restarts
;at the left margin after printing a car-
;riage return.
;
;It also tracks the screen position of
;the cursor (curr) so the hardware cursor
;can be painted there afterward.
;
;Uses direct video memory writes via MMU
;paging for performance (~18x faster than
;per-character FARCALL calls).

refresh:
; force screen dimensions (protect against corruption)
        LDA     #23
        STA     ROWS
        LDA     #80
        STA     COLUMNS
; set pointer tex to top of visible text
        COPY16  toplin,tex
        LDA     #1
        STA     scrrow          ; start at screen row 1 (row 0 = command line)
; default cursor position (in case curr is off-screen)
        LDA     #1
        STA     csrrow
        STZ     csrcol

; map video text page into $A000-$AFFF
        LDY     #VIDTEXT_PAGE
        JSR     vid_enter
; initialize video pointer to row 1 ($A000 + 1*80 = $A050)
        LDA     #$50
        STA     indir
        LDA     #$A0
        STA     indir+1

; main page loop: process one screen row per iteration
RPPAGE:
        LDY     #0
; scan text for line break point
RPLINE:
        LDA     (tex),Y
        INY
        AND     #$7F            ; strip high bit for comparison
        CMP     #retchar        ; end of paragraph?
        BEQ     RBREAK
        CPY     COLUMNS
        BNE     RPLINE

; hit screen width without a paragraph break - word wrap
; scan backward for a space to break at
        DEY
RSLOOP:
        LDA     (tex),Y
        AND     #$7F
        CMP     #space
        BEQ     RSBRK           ; found a space - wrap here
        DEY
        BNE     RSLOOP
; no space found in entire line - force break at column width
        LDY     COLUMNS
        DEY
RSBRK:
        INY                     ; wrap point (char after the space)
RBREAK:
        STY     temp            ; temp = number of chars in this line

; copy line to video memory, tracking cursor position
        LDY     #0
RCOPY:
; check if this text position is the cursor (tex + Y == curr?)
        TYA
        CLC
        ADC     tex             ; A = low byte of tex+Y
        PHA                     ; save (preserves carry for high byte calc)
        LDA     tex+1
        ADC     #0              ; A = high byte of tex+Y
        CMP     curr+1          ; compare high bytes
        BNE     @nocsr
        PLA                     ; A = low byte of tex+Y
        CMP     curr            ; compare low bytes
        BNE     @nocsr2
; found the cursor position on screen
        STY     csrcol
        LDA     scrrow
        STA     csrrow
        BRA     @dochr
@nocsr:
        PLA                     ; clean up stack
@nocsr2:
@dochr:
; read character from text memory and write to video RAM
        LDA     (tex),Y         ; read from text buffer (below $A000)
        AND     #$7F            ; strip high bit
        CMP     #space
        BCS     @rok            ; >= 32 is printable
        LDA     #space          ; replace control chars with space
@rok:
        STA     (indir),Y       ; write directly to video RAM
        INY
        CPY     temp
        BNE     RCOPY

; store length of first screen line in LENTABLE
        LDA     scrrow
        CMP     #1
        BNE     RCLRLN
        LDA     temp
        STA     LENTABLE

; pad remainder of line with spaces
RCLRLN:
        LDY     temp
        LDA     #space
@rpad:
        CPY     COLUMNS
        BEQ     RCLEARED
        STA     (indir),Y       ; write space to video RAM
        INY
        BRA     @rpad

RCLEARED:
; advance tex past the characters consumed
        CLC
        LDA     temp
        ADC     tex
        STA     tex
        LDA     tex+1
        ADC     #0
        STA     tex+1

; advance video pointer to next row (+80 bytes)
        CLC
        LDA     indir
        ADC     #80
        STA     indir
        LDA     indir+1
        ADC     #0
        STA     indir+1

; next screen row
        INC     scrrow
        LDA     scrrow
        CMP     ROWS
        BEQ     @refdone
        JMP     RPPAGE
@refdone:

; unmap video memory
        JSR     vid_exit
; record bottom-of-screen text address
        COPY16  tex,BOTSCR
        RTS

; --- video memory paging helpers ---
; Following the pattern from dbasic/screencmds.asm

; Map a video sub-page into $A000-$AFFF via Task 01.
; Y = video sub-page (e.g. VIDTEXT_PAGE for text chars).
; Trashes A, X. Preserves Y.
vid_enter:
        LDA     #$01            ; configure Task 01
        LDX     #$0A            ; bank $A ($A000-$AFFF)
        JSR     SETPAGE         ; Y = video sub-page (set by caller)
        LDA     #$01
        STA     PC6502_ACT_TASK ; switch to Task 01
        RTS

; Switch back to Task 00 (normal memory at $A000-$AFFF).
vid_exit:
        LDA     #$00
        STA     PC6502_ACT_TASK ; switch to Task 00
        RTS

; --- cursor helpers using FARCALL #58/#59 ---

; Paint the hardware cursor at the position found during refresh.
; Call this AFTER refresh returns.
showcursor:
        LDX     csrcol
        LDY     csrrow
        LDA     #FC_LOCATE
        STA     farfunct
        JSR     DO_FARCALL
        LDA     #FC_PAINTCSR
        STA     farfunct
        JSR     DO_FARCALL
        RTS

; Remove the hardware cursor from the screen.
; Call this BEFORE refresh or when processing a keypress.
hidecursor:
        LDA     #FC_UNPAINTCSR
        STA     farfunct
        JSR     DO_FARCALL
        RTS

BORDER:
        RTS

scrcol:
LETTERS:
        RTS
