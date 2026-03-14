
;
;
;
;oh boy printer stuff p114

deftab:
        .BYTE   5,75,66,5,58,1,1,1,0,1,0,80

prcodes:
        .BYTE   27,14,15,18

pchrout:
        STA     pcr
        PHX
        PHY
        COMPC16 pagenum,startnum
        BCC     skipout
        LDA     pcr
        JSR     chrout
shiftfreeze:
        B_UNLESS_SHIFT shiftfreeze
        LDA     $91
        CMP     #$7f
        BNE     skipout
        INC     53280
        JSR     cr
        JMP     pexit
skipout:
        PLY
        PLX
        LDA     pcr
        RTS


;display "Printing..."
prin:
        JSR     topclr
;PrintMessage prinmsg
        LDA     #<prinmsg
        LDY     #>prinmsg
        JMP     PRMSG
pbort:
        JMP     pexit
;ctrlp
print:
        LDA     scrcol
        STA     savcol
        LDA     #0
        STA     windcolr
        STA     scrcol
        JSR     setnam
        LDA     #4
        STA     devno
        LDY     #7
        B_UNLESS_SHIFT askques
        JMP     overques
askques:
        TOPPRINTMESSAGE choosemsg
        JSR     getakey
        AND     #127
        LDX     #3
        STX     devno
        CMP     #'s'
        BEQ     prcont
notscreen:
        LDX     #8
        STX     devno
        CMP     #'d'
        BEQ     dofn
        CMP     #'p'
        BNE     pbort
        TOPPRINTMESSAGE devmsg
        JSR     getakey
        SEC
        SBC     #48
        CMP     #4
        BCC     pbort
        CMP     #80
        BCS     pbort
        STA     devno
        JMP     prcont

dofn:
        TOPPRINTMESSAGE fnmsg
        JSR     input
        BEQ     pbort
        LDY     inlen
        LDA     #','
        STA     inbuff,y
        INY
        LDA     #'w'
        STA     inbuff,y
        INY
        STY     inlen
        LDA     inlen
        LDX     #<inbuff
        LDY     #>inbuff
        JSR     setnam
prcont:
        LDA     devno
        TAY
        CMP     #4
        BCC     overques
        CMP     #8
        BCS     overques
notd2:
        TOPPRINTMESSAGE sadrmsg
        JSR     getakey
        SEC
        SBC     #'0'
        TAY
        BPL     overques
        JMP     pbort
overques:
;;;;; TODO        LDA     #1
;;;;; TODO        LDX     devno
;;;;; TODO        JSR     setlfs
;;;;; TODO        JSR     prin
;;;;; TODO        LDA     #1
;;;;; TODO        JSR     close
;;;;; TODO        JSR     open
;;;;; TODO        LDX     #1
;;;;; TODO        JSR     chkout
;;;;; TODO        BCC     prok
        JMP     pexit
;reset flags

prok:
        LDX     #0
        STX     ftlen
        STX     hdlen
        STX     needasc
        STX     underline
        STX     linefeed

;copy definition
copydef:
        LDA     deftab,x
        STA     lmargin,x
        INX
        CPX     #12
        BNE     copydef
        LDA     #$ff
        STA     line
        STA     nomarg
        LDX     #4
copydefs:
        LDA     prcodes-1,x
        STA     codebuffer+48,x
        DEX
        BNE     copydefs

retex:
        COPY16  texstart,tex
;main printing loop
ploop:
        LDY     #0
        STY     pos
        CPY     nomarg
        BEQ     ploop1
        LDA     lmargin
        STA     pos
ploop1:
        LDA     (tex),y
        BPL     notsp
        JMP     special
notsp:
        CMP     #retchar
        BEQ     foundspace
notret:
        STA     prbuff,y
        INY
        INC     pos
        LDA     pos
        CMP     rmargin
        BCC     ploop1
        STY     finpos
findspace:
        LDA     (tex),y
        CMP     #space
        BEQ     foundspace
        DEC     pos
        DEY
        BNE     findspace
        LDY     finpos
        JMP     overstor
fspace:
        INY
        LDA     (tex),y
        CMP     #space
        BEQ     foundspace
        DEY
foundspace:
        STY     finpos
overstor:
        TYA
        SEC
        ADC     tex
        STA     tex
        LDA     tex+1
        ADC     #0
        STA     tex+1
        LDY     #0
dobuff:
        LDA     line
        CMP     #$ff
        BNE     @dobuf2
        JSR     top
@dobuf2:
        LDA     nomarg
        BEQ     @over
        JSR     lmarg
@over:
        SEC
        ROL     nomarg
        LDA     finpos
        STA     endpos
        LDA     #<prbuff
        STA     indir
        LDA     #>prbuff
        STA     indir+1
        JSR     bufprt

zbuff:
        JSR     crlf
        LDA     line
        CMP     botmarg
        BCC     notpage
        JSR     page
;Have we reached end of text?

notpage:
        COMP16  tex,lastline
        BEQ     dorpt
        BCC     dorpt
;check for footer
        LDA     ftlen
        BEQ     pxit
        LDA     #0
        STA     hdlen
        STA     topmarg
        JSR     page

pxit:
        LDA     devno
        CMP     #3
        BNE     pexit
        JSR     getakey
pexit:
;        JSR     stop			; ///TODO check the stop key and exit if it is pressed
;        BEQ     pexit
        LDA     #1
        JSR     close
        JSR     clall
        LDA     savcol
        STA     scrcol
        LDX     #$fa
        TXS
        JSR     sysmsg
        JMP     main
dorpt:
        JMP     ploop
page:
        SEC
        LDA     pagelength
        SBC     line
        TAY
        DEY
        DEY
        BEQ     nosk
        BMI     nosk

nexpage:
        JSR     cr
        DEY
        BNE     nexpage

nosk:
        LDA     ftlen
        BEQ     skipft
        STA     endpos
        LDA     #<ftbuff
        STA     indir
        LDA     #>ftbuff
        STA     indir+1

        JSR     lmarg
        JSR     bufprt
skipft:
        JSR     cr
        JSR     cr
        JSR     cr

;increment the page number
        INC     pagenum
        BNE     noipn
        INC     pagenum+1
noipn:
        LDA     continuous
        BNE     top
        LDA     devno
        CMP     #3
        BEQ     top
        CMP     #8
        BEQ     top
        COMPC16 pagenum,startnum
        BCC     top
        JSR     clrchn
        TOPPRINTMESSAGE waitmsg
        JSR     getakey
        JSR     prin
        LDX     #1
;;;;; TODO        JSR     chkout
;print header
top:
        LDA     hdlen
        BEQ     noheader
        STA     endpos
        LDA     #<hdbuff
        STA     indir
        LDA     #>hdbuff
        STA     indir+1
        JSR     lmarg
        JSR     bufprt
noheader:
        LDY     topmarg
        STY     line
        DEY
        BEQ     skiptop
        BMI     skiptop
toplp:
        JSR     cr
        DEY
        BNE     toplp
skiptop:
        RTS
;left margin routine
lmarg:
        LDA     #space
        LDY     lmargin
        STY     pos
        BEQ     lmexit
lmloop:
        JSR     pchrout
        DEY
        BNE     lmloop
lmexit:
        RTS

crlf:
        LDY     spacing
        CLC
        TYA
        ADC     line
        STA     line
crloop:
        JSR     cr
        DEY
        BNE     crloop
        RTS

cr:
        LDA     #13
        JSR     pchrout
        LDA     linefeed
        BEQ     nolf
        JSR     pchrout
nolf:
        RTS

special:
        STA     savchar
        AND     #127
        JSR     intoas
        LDX     sptab
srchsp:
        CMP     sptab,x
        BEQ     fsp
        DEX
        BNE     srchsp
        DEC     pos
        JMP     define
fsp:
        DEX
        TXA
        ASL
        TAX
        STY     ysave
        LDA     #>(spcont-1)
        PHA
        LDA     #<(spcont-1)
        PHA
        LDA     spvect+1,x
        PHA
        LDA     spvect,x
        PHA
        RTS
spcont:
        SEC
        LDA     ysave
        ADC     tex
        STA     tex
        LDA     tex+1
        ADC     #0
        STA     tex+1
        JMP     ploop
spcexit:
        LDA     (tex),y
        CMP     #retchar
        BEQ     noad
        DEY
noad:
        STY     ysave
        RTS
sptab:
        .BYTE   18
        .BYTE   "walrtbsnhf@p?xmigj"

spvect:
        .WORD   pw-1,as-1,lm-1,rm-1,tp-1
        .WORD   bt-1,sp-1,nx-1,hd-1,ft-1
        .WORD   pn-1,pl-1,spage-1,across-1
        .WORD   mrelease-1,comment-1,link-1
        .WORD   lfset-1

;m Margin release.
; INY is used to skip over the format character.
mrelease:
        INY
        STZ     nomarg
        JMP     spcexit
;x Columns across
across:
        INY
        JSR     aschex
        STA     pagewidth
        JMP     spcexit

;? Print starting at specified page
spage:
        INY
        PRCODE16 startnum
        JMP     spcexit
;@ set starting page default number
pn:
        INY
        PRCODE16 pagenum
        JMP     spcexit

;p page length
pl:
        INY
        PRCODE  pagelength
        JMP     spcexit

;w set page wait mode
pw:
        STZ     continuous
        INY
        JMP     spcexit

lfset:
        LDA     #10
        STA     linefeed
        INY
        JMP     spcexit
;a set true ASCII mode
as:
        INY
        LDA     #1
        STA     needasc
        JMP     spcexit
lm:
        INY
        PRCODE  lmargin
        JMP     spcexit
rm:
        INY
        PRCODE  rmargin
        JMP     spcexit
tp:
        INY
        PRCODE  topmarg
        JMP     spcexit
bt:
        INY
        PRCODE  botmarg
        JMP     spcexit
sp:
        INY
        PRCODE  spacing
        JMP     spcexit
;n Jump to next page
nx:
        LDY     ysave
        INY
        PHY
        JSR     page
        PLY
        STY     ysave
        RTS
;h define header
hd:
        JSR     PASTRET
        DEY
        STY     hdlen
        LDY     #1
hdcopy:
        LDA     (tex),Y
        STA     hdbuff-1,Y
        INY
        CPY     hdlen
        BCC     hdcopy
        BEQ     hdcopy
        INY
        JMP     spcexit
;Skip just past the return mark
PASTRET:
        INY
        LDA     (tex),Y
        CMP     #retchar
        BNE     PASTRET
        RTS
;f define header
ft:
        JSR     PASTRET
        DEY
        STY     ftlen
        LDY     #1
FTCOPY:
        LDA     (tex),Y
        STA     ftbuff-1,Y
        INY
        CPY     ftlen
        BCC     FTCOPY
        BEQ     FTCOPY
        JMP     spcexit

;i ignore a line
comment:
        JSR     PASTRET
        JMP     spcexit

; Define programmable printeys?

define:
        INY
        LDA     (tex),Y
        CMP     #'='
        BEQ     DODEFINE
        DEY
        LDA     savchar
        JMP     notret
DODEFINE:
        INY
        JSR     aschex
        PHA
        LDA     savchar
        AND     #127
        TAX
        PLA
        STA     codebuffer,X
        JSR     spcexit
        JMP     spcont
;Link to next file
link:
;;;;; TODO        INY
;;;;; TODO        LDX     #8
;;;;; TODO        LDA     (tex),Y
;;;;; TODO        AND     #63
;;;;; TODO        CMP     #4
;;;;; TODO        BEQ     link2
;;;;; TODO        LDX     #1
;;;;; TODO        CMP     #20             ;t
;;;;; TODO        BEQ     link2
;;;;; TODO        JMP     pbort
;;;;; TODO link2:
;;;;; TODO        STX     dvn
;;;;; TODO        INY
;;;;; TODO        LDA     (tex),y
;;;;; TODO         CMP     #':'
;;;;; TODO         BEQ     linkloop
;;;;; TODO         JMP     pbort
;;;;; TODO linkloop:
;;;;; TODO         INY
;;;;; TODO         LDA     (tex),y
;;;;; TODO         CMP     #retchar
;;;;; TODO         BEQ     outnam
;;;;; TODO         JSR     intoas
;;;;; TODO         STA     filename-3,y
;;;;; TODO         JMP     linkloop
outnam:
;;;;; TODO         TYA
;;;;; TODO         SEC
;;;;; TODO         SBC     #3
;;;;; TODO         LDX     #<filename
;;;;; TODO         LDY     #>filename
;;;;; TODO         JSR     setnam
;;;;; TODO         JSR     clrchn
;;;;; TODO         LDA     #2
;;;;; TODO         JSR     close
;;;;; TODO         LDA     #2
;;;;; TODO         LDX     dvn
;;;;; TODO         LDY     #0
;;;;; TODO         JSR     setlfs
;;;;; TODO         JSR     erase
;;;;; TODO         LDA     #0
;;;;; TODO         LDX     curr
;;;;; TODO         LDY     curr+1
;;;;; TODO         JSR     load
;;;;; TODO         BCC     oklod
;;;;; TODO         JMP     pbort
;;;;; TODO oklod:
;;;;; TODO         STX     lastline
;;;;; TODO         STY     lastline+1
;;;;; TODO         PLA
;;;;; TODO         PLA
;;;;; TODO         LDX     #1
;;;;; TODO         JSR     chkout
;;;;; TODO         JMP     retex
;;;;; TODO dcmnd:
;;;;; TODO         JSR     clall
;;;;; TODO         LDA     #0
;;;;; TODO         JSR     setnam
;;;;; TODO         LDA     #15
;;;;; TODO         LDX     #DEFAULT_DEVICE
;;;;; TODO         LDY     #15
;;;;; TODO         JSR     setlfs
;;;;; TODO         JSR     open
;;;;; TODO         BCC     okd
;;;;; TODO dcout:
;;;;; TODO         LDA     #15
;;;;; TODO         JSR     close
;;;;; TODO         JSR     clall
;;;;; TODO         JMP     sysmsg
;;;;; TODO okd:
;;;;; TODO         TOPPRINTMESSAGE dcmsg
;;;;; TODO         JSR     input
;;;;; TODO         BEQ     readerr
;;;;; TODO         LDX     #15
;;;;; TODO         JSR     chkout
;;;;; TODO         BCS     dcout
;;;;; TODO         PRINTMESSAGE inbuff
;;;;; TODO         LDA     #13             ;cr
;;;;; TODO         JSR     chrout
;;;;; TODO         JSR     clrchn
;;;;; TODO
;;;;; TODO readerr:
;;;;; TODO         JSR     clall
;;;;; TODO         LDA     #0
;;;;; TODO         JSR     setnam
;;;;; TODO         LDA     #15
;;;;; TODO         LDX     #DEFAULT_DEVICE
;;;;; TODO         LDY     #15
;;;;; TODO         JSR     setlfs
;;;;; TODO         JSR     open
;;;;; TODO         BCS     dcout
;;;;; TODO         JSR     topclr
;;;;; TODO         LDX     #15
;;;;; TODO         JSR     chkin
;;;;; TODO         JSR     input
;;;;; TODO         JSR     clrchn
;;;;; TODO         LDA     #15
;;;;; TODO         JSR     close
;;;;; TODO         JSR     clall
;;;;; TODO         LDA     #1
;;;;; TODO         STA     msgflg
;;;;; TODO         RTS
bufprt:
        LDY     #0
buflp:
        CPY     endpos
        BEQ     endbuff
        LDA     (indir),y
        BMI     spec2
        JSR     intoas
        JSR     convasc
        JSR     pchrout
;underline mode
        LDA     underline
        BEQ     nobrk
        LDA     #8              ;backspace
        JSR     pchrout
        LDA     #95             ;underscore
        JSR     pchrout
nobrk:
        INY
        JMP     buflp
endbuff:
        RTS
;stage 2 format commands
spec2:
        STY     ysave
        AND     #127
        STA     savchar
        JSR     intoas

other:
        CMP     #'c'
        BNE     notcenter
        SEC
        LDA     pagewidth
        SBC     endpos
        LSR
        SEC
        SBC     lmargin
        TAY
        LDA     #space
cloop:
        JSR     pchrout
        DEY
        BNE     cloop
        LDY     ysave
        JMP     nobrk
;edge right
notcenter:
        CMP     #'e'
        BNE     notedge
edge:
        SEC
        LDA     rmargin
        SBC     endpos
        SEC
        SBC     lmargin
        TAY
        LDA     #space
        JMP     cloop
notedge:
        CMP     #'u'
        BNE     notog
        LDA     underline
        EOR     #1
        STA     underline
notog:
        CMP     #'#'
        BNE     docodes
dopgn:
        STY     ysave
        LDX     pagenum
        LDA     pagenum+1
        DISPLAY_NUMBER
        LDY     ysave
        JMP     nobrk
docodes:
        LDX     savchar
        LDA     codebuffer,x
        JSR     pchrout
        JMP     nobrk
convasc:
        LDX     needasc
        BEQ     skipasc
        STA     temp
        AND     #127
        CMP     #'a'
        BCC     skipasc
        CMP     #'['
        BCS     skipasc
        TAX
        LDA     temp
        AND     #128
        EOR     #128
        LSR
        LSR
        STA     temp
        TXA
        ORA     temp
skipasc:
        RTS
