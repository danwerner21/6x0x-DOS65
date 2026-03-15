;__SpeedScript_________________________________________________________________
;
; SpeedScript 3.1 by Charles Brannon
; published in Compute Magazine for various 8-bit computers.
; This project is a port of the Commodore 64 version of SpeedScript
; to the 6502PC running DOS/65 with the memory mapped video card.
;
;______________________________________________________________________________
        .PSC02

        .SEGMENT "TEA"
        .ORG    $0800

        .INCLUDE "macro.asm"
        .INCLUDE "defines.asm"

BEGIN:
        JSR     INIT
        JSR     erase
        JSR     INIT2
        JMP     main

;UMOVE is a high-speed memory move
;routine. It gets its speed from self-
;modifying code (the $0000's at
;MOVLOOP are replaced by actual ad-
;dresses when UMOVE is called). Some
;assemblers may assemble this as a
;zero-page mode, so you may want to
;change the $0000's to $FFFF's. UMOVE
;is used to move an overlapping range
;of memory upward, so it is used to de-
;lete. Set froml/fromh to point to
;the source area of memory,
;destl/desth to point to the destina-
;tion, and llen/hlen to hold the
;length of the area being moved.

umove:
        LDA     froml
        STA     MOVLOOP+1
        LDA     fromh
        STA     MOVLOOP+2
        LDA     destl
        STA     MOVLOOP+4
        LDA     desth
        STA     MOVLOOP+5
        LDX     hlen
        BEQ     SKIPMOV
MOV1:
        LDA     #0
MOV2:
        STA     endpos
        LDY     #0
MOVLOOP:
        LDA     $0000,Y
        STA     $0000,Y
        INY
        CPY     endpos
        BNE     MOVLOOP
        INC     MOVLOOP+2
        INC     MOVLOOP+5
        CPX     #0
        BEQ     OUT
        DEX
        BNE     MOV1
SKIPMOV:
        LDA     llen
        BNE     MOV2
OUT:
        RTS
;DMOVE uses the same variables as UMOVE, but is used to move an
;overlapping block of memory down ward, so it is used to insert. If the block
;of memory to be moved does not overlap the destination area, then either
;routine can be used.
dmove:
        LDA     hlen
        TAX
        ORA     llen
        BNE     NOTNULL
        RTS
NOTNULL:
        CLC
        TXA
        ADC     fromh
        STA     DMOVLOOP+2
        LDA     froml
        STA     DMOVLOOP+1
        CLC
        TXA
        ADC     desth
        STA     DMOVLOOP+5
        LDA     destl
        STA     DMOVLOOP+4
        INX
        LDY     llen
        BNE     DMOVLOOP
        BEQ     SKIPDMOV
DMOV1:
        LDY     #255
DMOVLOOP:
        LDA     $0000,Y
        STA     $0000,Y
        DEY
        CPY     #255
        BNE     DMOVLOOP
SKIPDMOV:
        DEC     DMOVLOOP+2
        DEC     DMOVLOOP+5
        DEX
        BNE     DMOV1
        RTS

; displaynum replaces the call to BASIC $BCDC
; from the original one.
nums:
        .WORD   1,10,100,1000,10000
numarg:
        .WORD   0
numspace:
        .BYTE   $20
displaynum:
        STX     numarg
        STA     numarg+1
;lda #space
;jsr chrout ; leading space just like real one
basicdisplaynum:
        LDY     #8
dnzz:
        LDA     numarg+1
        CMP     nums+1,y
        BCC     dn2
        BNE     startout
        LDA     numarg
        CMP     nums,y
        BCS     startout
dn2:
        DEY
        DEY
        BNE     dnzz
startout:
        LDA     #$30
        STA     numspace
diglp:
        LDA     numarg+1
        CMP     nums+1,y
        BCC     digitout
        BNE     dontcmp
        LDA     numarg
        SBC     nums,y          ;carry already set
        BCC     digitout
        BCS     digsub
dontcmp:
        LDA     numarg
        SBC     nums,y          ;carry already set
digsub: ; subtract value of digit
        INC     numspace
        STA     numarg          ;already subtracted
        LDA     numarg+1
        SBC     nums+1,y
        STA     numarg+1
        JMP     diglp
digitout:
        LDA     numspace
        JSR     chrout
        DEY
        DEY
        BPL     startout
        RTS


; comment from left column of page 99

erase:
        LDA     texstart
        STA     tex
        STA     toplin
        STA     lastline
        STA     curr
        LDA     texstart+1
        STA     tex+1
        STA     toplin+1
        STA     lastline+1
        STA     curr+1
        SEC
        LDA     texend+1
        SBC     texstart+1
        TAX
        LDA     #space
CLRLOOP:
        LDY     #255
        DEC     tex+1
        STA     (tex),Y
        INY
        INC     tex+1
CLR2:
        STA     (tex),Y
        INY
        BNE     CLR2
        INC     tex+1
        DEX
        BNE     CLR2
        STA     (tex),Y
        RTS

; PRMSG is used anytime we need to
; print something at the top of the screen
; (the command line). Pass it the address
; of the message to be printed by storing
; the low .byte of the address in the accu
; mulator, and the high .byte in the
; register. The message in memory must
; end with a zero .byte. The routine does
; not add a carriage return.

PRMSG:
        STA     temp
        STY     temp+1
        LDY     #0
PRLOOP:
        LDA     (temp),Y
        BEQ     PREXIT
        JSR     chrout
        INY
        BNE     PRLOOP
PREXIT:
        RTS
getakey:
        JSR     chrin
        BEQ     getakey
        RTS



;The MAIN loop blinks the cursor,
;checks for keystrokes, converts
;them from ASCII to screen codes,
;puts them in text at the CURRent position, and increments
;the CURRent position and lastline. It also checks for special
;cases like the back-arrow and the return key and passes control
;characters to the CONTROL routine. SHIFTed spaces are turned into
;unSHIFTed ones. The INSMODE flag is checked to see if we should
;insert a space before a character.
main:
        JSR     hidecursor
        JSR     refresh
        JSR     showcursor
WAIT:
        JSR     chrin
        BEQ     WAIT
KEYPRESS:
        PHA                     ; save key code (hidecursor trashes A)
        JSR     hidecursor
        PLA                     ; restore key code
        TAX
        CPX     #8              ; ASCII backspace
        BEQ     @dobackspace
        CPX     #127            ; ASCII DEL
        BNE     NOTBKS
@dobackspace:
        JSR     left
        LDA     #space
        STA     (curr)
        JMP     main
NOTBKS:
        LDA     msgflg
        BEQ     nomsg
        PHX
        JSR     sysmsg
        PLX
nomsg:
        TXA
        CMP     #13             ; ASCII carriage return?
        BNE     notcr
        LDX     #retchar        ; store paragraph marker directly
        BRA     NESHIFT         ; skip control-char check (retchar < 32)
notcr:
        TXA
        AND     #127
        CMP     #32
        BCC     CONTROL
        TXA
        CMP     #$E0
        BCS     CONTROL
NESHIFT:
        TXA
        PHA
        LDY     #0
        LDA     (curr),Y
        CMP     #retchar
        BEQ     DOINS
        LDA     INSMODE
        CMP     #$00
        BEQ     NOTINST
DOINS:
        JSR     inschar
NOTINST:
        PLA
PUTCHR:
        STA     (curr)
        JSR     refresh
        SEC
        LDA     curr
        SBC     lastline
        STA     temp
        LDA     curr+1
        SBC     lastline+1
        ORA     temp
        BCC     INKURR
        LDA     curr
        ADC     #0
        STA     lastline
        LDA     curr+1
        ADC     #0
        STA     lastline+1
INKURR:
        INC     curr
        BNE     NOINC2
        INC     curr+1
NOINC2:
        JSR     check
        JMP     main
;CONTROl looks up a keyboard command in the list
;of control codes at CTBL. The first .byte of
;CTBL is the actual number of commands. Once the
;position is found, this position is doubled as
;an index to the two-byte address table at VECT.
;The address of MAIN-1 is put on the stack,
;simulating the return address; then the address
;of the command routine taken from VECT is pushed.
;We then perform an RTS. RTS pulls the .bytes off
;the stack as if they were put there by a JSR.
;This powerful technique is used to simulate
;ON+GOTO in machine language.
CONTROL:
        TXA
        LDX     CTBL
SRCH:
        CMP     CTBL,X
        BEQ     FOUND
        DEX
        BNE     SRCH
        JMP     main
FOUND:
        DEX
        TXA
        ASL
        TAX
        LDA     #>(main-1)
        PHA
        LDA     #<(main-1)
        PHA
        LDA     VECT+1,X
        PHA
        LDA     VECT,X
        PHA
        RTS
; Command key table - remapped for ASCII (Ctrl+letter combos)
; CTBL[1..N] are key codes, VECT[0..N-1] are matching function addresses
CTBL:
        .BYTE   30              ; 30 commands
        .BYTE   $F9             ; <RIGHT>:cursor right
        .BYTE   $F8             ; <LEFT>: cursor left
        .BYTE   $F1             ; <DEL>:  delete character at cursor
        .BYTE   4               ; Ctrl+D: delete (S,W,P)
        .BYTE   19              ; Ctrl+S: home (top of text)
        .BYTE   $F0             ; <INS>:  insert mode toggle
        .BYTE   17              ; Ctrl+Q: erase all text (with confirm)
        .BYTE   5               ; Ctrl+E: erase (S,W,P)
        .BYTE   12              ; Ctrl+L: load file
        .BYTE   22              ; Ctrl+V: save file
        .BYTE   18              ; Ctrl+R: insert buffer
        .BYTE   24              ; Ctrl+X: switch (cut to buffer)
        .BYTE   26              ; Ctrl+Z: go to end of text
        .BYTE   16              ; Ctrl+P: print
        .BYTE   28              ; Ctrl+\: format code
        .BYTE   25              ; Ctrl+Y: delete line
        .BYTE   1               ; Ctrl+A: alpha (go to top of screen)
        .BYTE   11              ; Ctrl+K: kill buffer
        .BYTE   8               ; Ctrl+H: hunt/search (backspace handled before here)
        .BYTE   31              ; Ctrl+_: show free memory
        .BYTE   3               ; Ctrl+C: tab
        .BYTE   10              ; Ctrl+J: replace start
        .BYTE   7               ; Ctrl+G: search and replace
        .BYTE   $F7             ; <DOWN>: paragraph right
        .BYTE   $F6             ; <UP>:   paragraph left
        .BYTE   23              ; Ctrl+W: word left
        .BYTE   21              ; Ctrl+U: word right
        .BYTE   $E2             ; <F3>:   return to os
        .BYTE   $E0             ; <F1>:   disk catalog
        .BYTE   $E8             ; <F9>:   print
VECT:
        .WORD   right-1         ; <RIGHT>
        .WORD   left-1          ; <LEFT>
        .WORD   DELCHAR-1       ; <DEL>
        .WORD   DELETE-1        ; Ctrl+D
        .WORD   HOME-1          ; Ctrl+S
        .WORD   instgl-1        ; <INSERT>
        .WORD   CLEAR-1         ; Ctrl+Q
        .WORD   ERAS-1          ; Ctrl+E
        .WORD   TLOAD-1         ; Ctrl+L
        .WORD   TSAVE-1         ; Ctrl+V
        .WORD   insbuffer-1     ; Ctrl+R
        .WORD   switch-1        ; Ctrl+X
        .WORD   endtex-1        ; Ctrl+Z
        .WORD   print-1         ; Ctrl+P
        .WORD   FORMAT-1        ; Ctrl+\
        .WORD   DELIN-1         ; Ctrl+Y
        .WORD   alpha-1         ; Ctrl+A
        .WORD   killbuff-1      ; Ctrl+K
        .WORD   HUNT-1          ; Ctrl+H
        .WORD   FREEMEM-1       ; Ctrl+_
        .WORD   tab-1           ; Ctrl+C
        .WORD   repstart-1      ; Ctrl+J
        .WORD   SANDR-1         ; Ctrl+G
        .WORD   paright-1       ; <DOWN>
        .WORD   parleft-1       ; <UP>
        .WORD   wleft-1         ; Ctrl+W
        .WORD   wright-1        ; Ctrl+U
        .WORD   quit-1          ; <F3>
        .WORD   catalog-1       ; <F1>
        .WORD   print-1         ; <F9>
;The check routine first prevents the cursor from
;disappearing past the beginning or end-of-text memory,
;and prevents us from cursoring past the end-of-text pointer.
;It also checks to see if the cursor has left the visible
;screen, scrolling with REFRESH to make the cursor visible.


check:
        JSR     check2
        COMPC16 curr,toplin
        BCS     OK1
        COMP16  toplin,texstart
        BEQ     OK1
        COPY16  curr,toplin
        JSR     refresh
OK1:
        SEC
        LDA     BOTSCR
        SBC     curr
        STA     tex
        LDA     BOTSCR+1
        SBC     curr+1
        STA     tex+1
        ORA     tex
        BEQ     EQA
        BCS     OK2
EQA:
        CLC
        LDA     toplin
        ADC     LENTABLE
        STA     toplin
        LDA     toplin+1
        ADC     #0
        STA     toplin+1
REF:
        JSR     refresh
        JMP     OK1
OK2:
        RTS

check2:
        SEC
        LDA     lastline
        SBC     texend
        STA     temp
        LDA     lastline+1
        SBC     texend+1
        ORA     temp
        BCC     CK3
        LDA     texend
        STA     lastline
        LDA     texend+1
        STA     lastline+1
CK3:
        SEC
        LDA     curr
        SBC     texstart
        STA     temp
        LDA     curr+1
        SBC     texstart+1
        ORA     temp
        BCS     INRANGE
        LDA     texstart
        STA     curr
        LDA     texstart+1
        STA     curr+1
        RTS
INRANGE:
        SEC
        LDA     curr
        SBC     lastline
        STA     temp
        LDA     curr+1
        SBC     lastline+1
        ORA     temp
        BCS     OUTRANGE
        RTS
OUTRANGE:
        COPY16  lastline,curr
        RTS
; move cursor right.
right:
        INC     curr
        BNE     NOINCR
        INC     curr+1
NOINCR:
        JMP     check

; Cursor left.
left:
        LDA     curr
        BNE     NODEC
        DEC     curr+1
NODEC:
        DEC     curr
        JMP     check
; Word left. We look backward for a space.
wleft:
        COPY16  curr,tex
        DEC     tex+1
        LDY     #$FF
STRIP:
        LDA     (tex),Y
        CMP     #space
        BEQ     STRLOOP
        CMP     #retchar
        BNE     WLOOP
STRLOOP:
        DEY
        BNE     STRIP
WLOOP:
        LDA     (tex),Y
        CMP     #space
        BEQ     wrout
        CMP     #retchar
        BEQ     wrout
        DEY
        BNE     WLOOP
        RTS
wrout:
        SEC
        TYA
        ADC     tex
        STA     curr
        LDA     tex+1
        ADC     #0
        STA     curr+1
        JMP     check
;Word right. We scan forward
;for a space. OIDS is not a meaningful label.
wright:
        LDY     #0
RLOOP:
        LDA     (curr),Y
        CMP     #space
        BEQ     ROUT
        CMP     #retchar
        BEQ     ROUT
        INY
        BNE     RLOOP
        RTS
ROUT:
        INY
        BNE     OIDS
        INC     curr+1
        LDA     curr+1
        CMP     lastline+1
        BCC     OIDS
        BNE     lastword
OIDS:
        LDA     (curr),Y
        CMP     #space
        BEQ     ROUT
        CMP     #retchar
        BEQ     ROUT
; add Y to curr to move the cursor
; check prevents illegal cursor movement.
; LASTWORD is called if the end
; of the word cannot be found within 255 characters.
adycurr:
        CLC
        TYA
        ADC     curr
        STA     curr
        LDA     curr+1
        ADC     #0
        STA     curr+1
wrtn:
        JMP     check

lastword:
        COPY16  lastline,curr
        JMP     check

;endtex is tricky,  (p103)

endtex:
        STZ     toplin
        LDA     lastline+1
        SEC
        SBC     #4
        CMP     texstart+1
        BCS     SAFE
        LDA     texstart+1
SAFE:
        STA     toplin+1
        JSR     refresh
        JMP     lastword



;Sentence left. We look backward for ending punctuation
;or a return mark, then go forward until we run out of spaces.


sleft:
        COPY16  curr,tex
        DEC     tex+1
        LDY     #$FF
PMANY:
        LDA     (tex),Y
        B_IF_PUNCT PSRCH
        CMP     #retchar
        BNE     psloop
PSRCH:
        DEY
        BNE     PMANY
        RTS
psloop:
        LDA     (tex),Y
        B_IF_PUNCT PUNCT
        CMP     #retchar
        BEQ     PUNCT
        DEY
        BNE     psloop
        DEC     tex+1
        LDA     tex+1
        CMP     texstart
        BCS     psloop
        JMP     firstword
PUNCT:
        STY     temp
        DEC     temp
SKIPSPC:
        INY
        BEQ     REPEAT
        LDA     (tex),y
        CMP     #space
        BEQ     SKIPSPC
        DEY
        JMP     wrout
REPEAT:
        LDY     temp
        JMP     psloop
firstword:
        COPY16  texstart,curr
        JMP     check
; Sentence right. We look for ending punctuation,
; then skip forward until we run out of spaces.
sright:
        LDY     #0
srlp:
        LDA     (curr),Y
        B_IF_PUNCT punct2
        CMP     #retchar
        BEQ     punct2
        INY
        BNE     srlp
        INC     curr+1
        LDA     curr+1
        CMP     lastline+1
        BEQ     srlp
        BCC     srlp
srexit:
        JMP     lastword
punct2:
        INY
        BNE     nofixcurr
        INC     curr+1
        LDA     curr+1
        CMP     lastline+1
        BCC     nofixcurr
        BEQ     nofixcurr
        JMP     lastword
nofixcurr:
        LDA     (curr),y
        CMP     #space
        BEQ     punct2
        B_IF_PUNCT punct2
        CMP     #retchar
        BEQ     punct2
        JMP     adycurr
; The text buffer starts at a fixed
; location, but the end of the buffer
; is changed as text is added to it.
; To clear the buffer, we just set
; the end of the buffer to the
; start of the buffer. No text is
; actually erased.
killbuff:
        COPY16  texbuf,tptr
        JSR     topclr
        PRINTMESSAGE killmsg
        LDA     #1
        STA     msgflg
        RTS

; This is the second level of the
; general-purpose delete routines. (p.105)


del1:
        COMP16  curr,texstart
        BNE     DEL1A
DELABORT:
        PLA
        PLA
        RTS
DEL1A:
        COPY16  curr,froml
        RTS
del2:
        SEC
        LDA     curr
        STA     destl
        EOR     #$ff
        ADC     froml
        STA     goblen
        LDA     curr+1
        STA     desth
        EOR     #$ff
        ADC     fromh
        STA     goblen+1
delc:
        COPY16  froml,fromsav
        LDA     destl
        STA     destsav
        STA     froml
        LDA     desth
        STA     destsav+1
        STA     fromh
        SEC
        LDA     goblen+1
        ADC     tptr+1
        CMP     bufend+1
        BCC     gosav
        TOPPRINTMESSAGE buferr
        LDA     #1
        STA     msgflg
        JSR     drainkeys       ; drain any pending keystrokes
        RTS

gosav:
        COPY16  tptr,destl
        LDA     goblen
        STA     llen
        CLC
        ADC     tptr
        STA     tptr
        LDA     goblen+1
        STA     hlen
        ADC     tptr+1
        STA     tptr+1
;lda #0
;sta $D01A ;TODO: label
;lda #52
;sta map
        JSR     umove
;lda #54
;sta map
;lda #1
;sta $D01A

        COPY16  fromsav,froml
        COPY16  destsav,destl
        SUB162  lastline,destl,llen
        JSR     umove
        SUB16   lastline,goblen
        RTS
;Most delete commands end up calling
;the above routines. (p106)
DELCHAR:
        JSR     del1
        JSR     left
        JSR     del2
fixtp:
        SUB8    tptr,#1
        RTS
;this is called from CTRL-back arrow.
;We first check to see if SHIFT is also
;held down. If so, we go to another routine
;that "eats" spaces.
DELIN:
        B_UNLESS_SHIFT_CTRL DODELIN
        JMP     EATSPACE
DODELIN:
        JSR     right
        JSR     del1
        JSR     left
        JSR     del2
        JMP     fixtp

;Called by CTRL-D.  (etc)
DELETE:
        JSR     killbuff
        JSR     topclr
        PRINTMESSAGE delmsg
        JSR     getakey
        PHA
        JSR     sysmsg
        PLA
        AND     #191
        CMP     #23             ; "W"
        BNE     NOTWORD
DELWORD:
        JSR     del1
        JSR     wleft
        JMP     del2
NOTWORD:
        CMP     #19             ; "S"
        BNE     NOTSENT
DELSENT:
        JSR     del1
        JSR     sleft
        JMP     del2
NOTSENT:
        CMP     #16             ; "P"
        BNE     NOTPAR
        JSR     del1
        JSR     parleft
        JMP     del2
NOTPAR:
        RTS


;Home the cursor. if the cursor
;is already home, move the cursor
;to the top of text.
HOME:
        COMP16  curr,toplin
        BEQ     tophome
        COPY16  toplin,curr
        RTS
tophome:
        COPY16  texstart,curr
        JMP     check

; This deletes all spaces between the
; cursor and following nonspace text.
; Sometimes inventing labels can be fun.
EATSPACE:
        COPY162 curr,tex,destl
        LDY     #0
spcsrch:
        LDA     (tex),y
        CMP     #space
        BNE     outspace
        INY
        BNE     spcsrch
        LDA     tex+1
        CMP     lastline+1
        BCC     goinc
        COPY16  lastline,tex
        LDY     #0
        JMP     outspace
goinc:
        INC     tex+1
        JMP     spcsrch
outspace:
        CLC
        TYA
        ADC     tex
        STA     froml
        LDA     #0
        ADC     tex+1
        STA     fromh

        SUB162  lastline,destl,llen
        SUB162  froml,destl,goblen
        JSR     umove
        SUB162  lastline,goblen,lastline
        RTS

;Inserts 255 spaces. Notice how it and other
;insert routines use TAB2.
lottaspaces:
        LDA     #255
        STA     inslen
        JMP     tab2

tab:
        LDA     #5
        STA     inslen
        JSR     tab2
        LDA     (curr),y
        CMP     #space
        BNE     noincy
        INY
noincy:
        JMP     adycurr
tab2:
        LDA     #0
        STA     inslen+1
        JSR     insblock
        LDA     #space
        LDX     inslen
        LDY     #0
fillsp:
        STA     (curr),y
        INY
        DEX
        BNE     fillsp
        RTS
;SHIFT-RETURN calls this. It inserts
;two spaces, fills them with return marks,
;then calls tAB for a margin indent. Not
;much code for a useful routine.
endpar:
        JSR     inschar
        JSR     inschar
        LDA     #retchar
        LDY     #0
        STA     (curr),Y
        INY
        STA     (curr),Y
        JSR     refresh
        JSR     right
        JSR     right
        JMP     tab
;insert a single space:
inschar:
        LDA     #1
        STA     inslen
        STZ     inslen+1
        JSR     insblock
        LDA     #space
        STA     (curr)
        JMP     check
;A general routine to insert as many
;spaces as are specified by inslen.
insblock:
        CLC
        LDA     lastline
        ADC     inslen          ; discarded?
        LDA     lastline+1
        ADC     inslen+1
        CMP     texend+1
        BCC     okins
        PLA
        PLA
        JMP     inout
okins:
        CLC
        LDA     curr
        STA     froml
        ADC     inslen
        STA     destl
        LDA     curr+1
        STA     froml+1
        ADC     inslen+1
        STA     destl+1
        SUB162  lastline,froml,llen
        JSR     dmove
        ADD16   lastline,inslen
inout:
        RTS
;toggle insert mode. The INSMODE
;flag doubles as the color of the
;command line.
instgl:
        LDA     INSMODE
        EOR     #1
        STA     INSMODE
        JMP     sysmsg
;Another example of modular code.
YORN:
        PRINTMESSAGE ynmsg
YORNKEY:                        ;JSR scnkey
        JSR     chrin
        BEQ     YORNKEY
        AND     #127
        CMP     #'y'
        RTS
;Erase all text. (p108)
CLEAR:
        JSR     topclr
        PRINTMESSAGE clrmsg
        JSR     YORN
        BEQ     DOIT
        JMP     sysmsg
DOIT:
        LDX     #$FA
        TXS
        JSR     erase
        JSR     INIT3
        JMP     main
;Paragraph right.
paright:
        LDY     #0
parlp:
        LDA     (curr),Y
        CMP     #retchar
        BEQ     retfound
        INY
        BNE     parlp
        INC     curr+1
        LDA     curr+1
        CMP     lastline+1
        BCC     parlp
        BEQ     parlp
        JMP     lastword
retfound:
        INY
        BNE     goady
        INC     curr+1
goady:
        JMP     adycurr
;Paragraph left.
parleft:
        COPY16  curr,tex
        DEC     tex+1
        LDY     #$ff
parloop:
        LDA     (tex),y
        CMP     #retchar
        BEQ     retf2
parcont:
        DEY
        CPY     #255
        BNE     parloop
        DEC     tex+1
        LDA     tex+1
        CMP     texstart+1
        BCS     parloop
        JMP     firstword
retf2:
        SEC
        TYA
        ADC     tex
        STA     tex
        LDA     #0
        ADC     tex+1
        STA     tex+1
        COMP16  tex,curr
        BNE     textocurr
        STY     temp
        CLC
        LDA     tex
        SBC     temp
        STA     tex
        LDA     tex+1
        SBC     #0
        STA     tex+1
        JMP     parcont
textocurr:
        COPY16  tex,curr
        JMP     check

;ERAS is called by CTRL-E. It works
;much like CTRL-D. Notice that the
;ORA #64 allows ....
ERAS:
        B_UNLESS_SHIFT ERAS1
        JSR     killbuff
ERAS1:
        JSR     topclr
        PRINTMESSAGE erasmsg
erasagain:
        LDA     (curr)
        EOR     #$80
        STA     (curr)
        JSR     refresh
        LDA     (curr)
        EOR     #$80
        STA     (curr)
        JSR     getakey
        ORA     #64
        CMP     #'w'
        BNE     noword
erasword:
        JSR     era1
        JSR     wright
        JMP     era2
noword:
        CMP     #'s'
        BNE     unsent
erasent:
        JSR     era1
        JSR     sright
        JMP     era2
unsent:
        CMP     #'p'
        BNE     nopar
        JSR     era1
        JSR     paright
        JMP     era2
nopar:
        JSR     check
        JMP     sysmsg
era1:
        COPY162 curr,destl,savcurr
        RTS
era2:
        SEC
        LDA     curr
        STA     froml
        SBC     savcurr
        STA     goblen
        LDA     curr+1
        STA     froml+1
        SBC     savcurr+1
        STA     goblen+1
        JSR     delc
        COPY16  savcurr,curr
        JSR     refresh
        JMP     erasagain
;the INPUT routine is used to get responses
;from the command line.
;input: get a line of text from the command line.
;Returns length in A, text in inbuff (null-terminated).
input:
        LDA     #39             ; max input length (inbuff is 40 bytes)
        STA     limit
inp1:
        LDY     #0
cursin:
        STY     inlen
        JSR     getakey         ; blocking wait for key
        LDY     inlen
        CMP     #13             ; CR = done
        BEQ     inexit
        CMP     #8              ; ASCII backspace
        BEQ     @inback
        CMP     #127            ; ASCII DEL
        BEQ     @inback
        AND     #127
        CMP     #space
        BCC     cursin          ; ignore control chars
        CPY     limit
        BEQ     cursin          ; buffer full
        STA     inbuff,Y
        JSR     chrout          ; echo the character
        INY
        JMP     cursin
@inback:
        CPY     #0
        BEQ     cursin          ; nothing to delete
        DEY
        LDA     #8              ; backspace (cursor left)
        JSR     chrout
        LDA     #space          ; overwrite with space
        JSR     chrout
        LDA     #8              ; backspace again
        JSR     chrout
        JMP     cursin
inexit:
        LDA     #0
        STA     inbuff,Y
        TYA
        RTS

; called by CTRL-\ to enter a format code.
; It checks insert mode and inserts if necessary.
FORMAT:
        JSR     topclr
        PRINTMESSAGE formsg
        JSR     getakey
        ORA     #$80
        PHA
        LDA     INSMODE
        CMP     #$00
        BEQ     NOINS
        JSR     inschar
NOINS:
        JSR     sysmsg
        PLA
        JMP     PUTCHR
; oh boy
aschex:
        STZ     bcd
        STZ     bcd+1
        STZ     hex
        STZ     hex+1
digit:
        SEC
        LDA     (tex),y
        SBC     #'0'
        BCC     nonum
        CMP     #10             ;radix
        BCS     nonum
        ASL     bcd
        ROL     bcd+1
        ASL     bcd
        ROL     bcd+1
        ASL     bcd
        ROL     bcd+1
        ASL     bcd
        ROL     bcd+1
        ORA     bcd
        STA     bcd
        INY
        BNE     digit
        INC     tex+1
        JMP     digit
nonum:
        SED
dechex:
        LDA     bcd
        ORA     bcd+1
        BEQ     donenum
        SUB8    bcd,#1
        INC     hex
        BNE     nohexinc
        INC     hex+1
nohexinc:
        JMP     dechex
donenum:
        LDA     hex
        CLD
        RTS

;p113

insbuffer:
        SUB162  tptr,texbuf,buflen
        ORA     buflen
        BNE     okbuff
        JSR     topclr
        PRINTMESSAGE insmsg
        LDA     #1
        STA     msgflg
        RTS

okbuff:
        CLC
        LDA     curr
        STA     froml
        ADC     buflen
        STA     destl
        LDA     curr+1
        STA     froml+1
        ADC     buflen+1
        STA     destl+1
        SUB162  lastline,froml, llen
        CLC
        ADC     desth
        CMP     texend+1
        BCC     okmov
        JSR     topclr
        PRINTMESSAGE inserr
        LDA     #1
        STA     msgflg
        RTS

okmov:
        JSR     dmove
        CLC
        LDA     buflen
        STA     llen
        ADC     lastline
        STA     lastline
        LDA     buflen+1
        STA     llen+1
        ADC     lastline+1
        STA     lastline+1
        COPY16  curr,destl
        COPY16  texbuf,froml
;lda #0
;sta $d01a
;;;;; TODO         LDA     #52
;;;;; TODO         STA     map
;;;;; TODO         JSR     umove
;;;;; TODO         LDA     #54
;;;;; TODO         STA     map
;lda #1
;sta $d01a
;;;;; TODO         JMP     check

switch:
        LDY     #1
        LDA     (curr)
        TAX
        LDA     (curr),y
        STA     (curr)
        TXA
        STA     (curr),y
        RTS

alpha:
        LDA     (curr)
        AND     #63
        BEQ     notalpha
        CMP     #27
        BCS     notalpha
        LDA     (curr)
        EOR     #64
        STA     (curr)
notalpha:
        JMP     right

intoas:
        STA     temp
        AND     #$3f
        ASL     temp
        BIT     temp
        BPL     isk1
        ORA     #$80
isk1:
        BVS     isk2
        ORA     #$40
isk2:
        STA     temp
        RTS


;Global search and replace. This just
;links together the search-specify routine,
;the replace-specify routine,
;then repeatedly calls Hunt and Replace,
;until Hunt returns "Not Found." (fpos+1
;is $FF after a search failure.)
SANDR:
        JSR     reset
        LDA     huntlen
        BEQ     NOSR
        JSR     askrep
SNR:
        JSR     CONTSRCH
        LDA     fpos+1
        CMP     #$FF
        BEQ     NOSR
        JSR     repl
        JSR     refresh
        JMP     SNR
NOSR:
        JMP     sysmsg
;if SHIFT is held down, we ask for and store
;the hunt phrase. If SHIFT is not down, we
;perform the actual hunt. The line in the inbuff is compared with
;characters in text. (p121)
HUNT:
        B_UNLESS_SHIFT_CTRL CONTSRCH
reset:
        TOPPRINTMESSAGE srchmsg
        JSR     input
        STA     huntlen
        BNE     oksrch
        JMP     sysmsg
oksrch:
        LDY     #0
tobuff:
        LDA     inbuff,Y
        STA     huntbuff,Y
        INY
        CPY     inlen
        BNE     tobuff
        JMP     sysmsg
CONTSRCH:
        COPY16  curr,tex
        LDA     #$FF
        STA     fpos+1
        LDY     #1
        LDX     #0
        LDA     huntlen
        BEQ     notfound
SRCH1:
        LDA     huntbuff,X
        CMP     (tex),Y
        BEQ     CY
        LDX     #$FF
CY:
        INY
        BNE     novfl
        INC     tex+1
        LDA     tex+1
        CMP     lastline+1
        BEQ     novfl
        BCS     notfound
novfl:
        INX
        CPX     huntlen
        BNE     SRCH1
        CLC
        TYA
        ADC     tex
        STA     temp
        LDA     tex+1
        ADC     #0
        STA     temp+1
        LDA     lastline
        CMP     temp
        LDA     lastline+1
        SBC     temp+1
        BCC     notfound
        SEC
        LDA     temp
        SBC     huntlen
        STA     curr
        STA     fpos
        LDA     temp+1
        SBC     #0
        STA     curr+1
        STA     fpos+1
        JSR     check
        RTS


notfound:
        TOPPRINTMESSAGE nfmsg
        LDA     #1
        STA     msgflg
        RTS

;replace
repstart:
        B_UNLESS_SHIFT_CTRL repl
askrep:
        TOPPRINTMESSAGE repmsg
        JSR     input
        STA     replen
        BEQ     norep
        LDY     #0
repmov:
        LDA     inbuff,y
        STA     repbuff,y
        INY
        CPY     inlen
        BNE     repmov
norep:
        JMP     sysmsg
repl:
        SEC
        LDA     curr
        STA     destl
        SBC     fpos
        STA     temp
        LDA     curr+1
        STA     destl+1
        SBC     fpos+1
        ORA     temp
        BNE     norepl
        LDA     #$ff
        STA     fpos+1
        CLC
        LDA     huntlen
        ADC     curr
        STA     froml
        LDA     #0
        ADC     curr+1
        STA     fromh
        SUB162  lastline,destl, llen
        JSR     umove
        SUB8    lastline,huntlen
        LDA     replen
        BEQ     norepl
        STA     inslen
        STZ     inslen+1
        JSR     insblock
        LDY     #0
reploop:
        LDA     repbuff,y
        STA     (curr),y
        INY
        CPY     replen
        BNE     reploop
        CLC
        LDA     curr
        ADC     replen
        STA     curr
        LDA     curr+1
        ADC     #0
        STA     curr+1
norepl:
        JMP     check

;display free memory
FREEMEM:
        JSR     topclr
        SEC
        LDA     texend
        SBC     lastline
        TAX
        LDA     texend+1
        SBC     lastline+1
        DISPLAY_NUMBER
        LDA     #1
        STA     msgflg
        RTS


quit:
        JSR     topclr
        PRINTMESSAGE quitmsg
        JSR     YORN
        BEQ     :+
        JMP     sysmsg
:
        LDA     #FC_COLOR
        STA     farfunct
        LDX     #SCNCOLOR
        LDY     #CRCOLOR
        JSR     DO_FARCALL
        LDA     #FC_SCNCLR      ; CLEAR SCREEN
        STA     farfunct
        JSR     DO_FARCALL
        LDA     #1
        STA     SHOWCRSR        ; TURN ON CURSOR
        JMP     $0100


        .INCLUDE "screen.asm"
        .INCLUDE "io.asm"

; speedscript 3.1 by charles brannon
MSG1:
        .ASCIIZ "SpeedScript 3.1"
MSG2:
        .ASCIIZ " by Charles Brannon"
killmsg:
        .ASCIIZ "Buffer Cleared"
buferr:
        .ASCIIZ "Buffer Full"
delmsg:
        .ASCIIZ "Delete (S,W,P)"
ynmsg:
        .ASCIIZ ": Are you sure? (Y/N):"
clrmsg:
        .ASCIIZ "ERASE ALL TEXT"
quitmsg:
        .ASCIIZ "Leave SpeedScript"
erasmsg:
        .ASCIIZ "Erase (S,W,P): RETURN to exit"
formsg:
        .ASCIIZ "Press format key:"
savmsg:
        .ASCIIZ "Save:"
fnfmsg:
        .ASCIIZ "File not found"
ioerrmsg:
        .ASCIIZ "I/O error"
okmsg:
        .ASCIIZ "No errors"
loadmsg:
        .ASCIIZ "Load:"
inserr:
        .ASCIIZ "No Room"
insmsg:
        .ASCIIZ "No text in buffer."
choosemsg:
        .ASCIIZ "Print to: [S]creen,[D]isk,[P]rinter?"
fnmsg:
        .ASCIIZ "Print to filename:"
prinmsg:
        .ASCIIZ "Printing..."
waitmsg:
        .ASCIIZ "Insert next sheet, press RETURN"
srchmsg:
        .ASCIIZ "Hunt for:"
nfmsg:
        .ASCIIZ "Not Found"
repmsg:
        .ASCIIZ "Replace with:"
xitmsg:
;EXIT SpeedScript
        .ASCIIZ "EXIT SpeedScript"

texstart:
        .ORG    *+2
texend:
        .ORG    *+2
texbuf:
        .ORG    *+2
bufend:
        .ORG    *+2
LENTABLE:
        .ORG    *+1
toplin:
        .ORG    *+2
msgflg:
        .ORG    *+1
INSMODE:
        .ORG    *+1
endpos:
        .ORG    *+1
finpos:
        .ORG    *+1
lastline:
        .ORG    *+2
limit:
        .ORG    *+1
inlen:
        .ORG    *+1
BOTSCR:
        .ORG    *+2
lbuff:
        .ORG    *+80
inbuff:
        .ORG    *+40
filename:
        .ORG    *+24
fnlen:
        .ORG    *+1
savcurr:
        .ORG    *+2
bcd:
        .ORG    *+2
hex:
        .ORG    *+2
tptr:
        .ORG    *+2
buflen:
        .ORG    *+2
goblen:
        .ORG    *+2
fromsav:
        .ORG    *+2
destsav:
        .ORG    *+2
hdlen:
        .ORG    *+1
ftlen:
        .ORG    *+1
lmargin:
        .ORG    *+1
rmargin:
        .ORG    *+1
pagelength:
        .ORG    *+1
topmarg:
        .ORG    *+1
botmarg:
        .ORG    *+1
spacing:
        .ORG    *+1
continuous:
        .ORG    *+1
pagenum:
        .ORG    *+2
startnum:
        .ORG    *+2
pagewidth:
        .ORG    *+1
nomarg:
        .ORG    *+1
pos:
        .ORG    *+1
line:
        .ORG    *+1
ysave:
        .ORG    *+1
savchar:
        .ORG    *+1
inslen:
        .ORG    *+1
devno:
        .ORG    *+1
needasc:
        .ORG    *+1
underline:
        .ORG    *+1
fpos:
        .ORG    *+2
pcr:
        .ORG    *+1
huntlen:
        .ORG    *+1
huntbuff:
        .ORG    *+30
replen:
        .ORG    *+1
repbuff:
        .ORG    *+30
codebuffer:
        .ORG    *+128
prbuff:
        .ORG    *+256
hdbuff:
        .ORG    *+256
firstrun:
        .ORG    *+1
ftbuff:
        .ORG    *+256
savcol:
        .ORG    *+1
linefeed:
        .ORG    *+1
blinkflag:
        .ORG    *+1
blinktimer:
        .ORG    *+1
fcb:
        .ORG    *+33
END:
