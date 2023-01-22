;*      
;* SuperMON 64
;*
;* Original source code courtesy Jim Butterfield
;*
;* Merlinized and adapted for the SuperCPU by
;* Stephen L. Judd
;* 3/14/97
;*

;*rem"s= dsave"@super64.src2"
;*rem;open 8,8,8,"1:listing,s,w"
;*sys7*4096
                          ;.opt p8
                          ; supermon 64 sept 9/83
                          ;*=  $9824
         .ORG $9824

savx     = $1c
tmpc     = $1d
nemo     = $1e
length   = $1f
psav     = $20
wrap     = $26
aflg     = $28
acmd     = $2a
satus    = $90
lvflag   = $93
indev    = $99
outdev   = $9a
eal      = $ae
eah      = $af
fnlen    = $b7
wtype    = $b9
fa       = $ba
fnadr    = $bb
tmp0     = $c1
tmp2     = $c3
inbuf    = $0100
stage    = $0210
addrs    = $0240
pch      = $0239
pcl      = $023a
flgs     = $023b
acc      = $023c
xr       = $023d
yr       = $023e
sp       = $023f
bkvec    = $0316
ready    = $f679
ldvec    = $0330
svvec    = $0332
setmsg   = $ff90
rdt      = $f137
wrt      = $f123
stop1    = $ffe1
                          ; ** initialization **
super    lda linkad
         sta bkvec
         lda linkad+1
         sta bkvec+1
         lda #$80
       ;  jsr setmsg
         brk
                          ; ** break entry **
break    cld
         cli
         pla
         sta yr
         pla
         sta xr
         pla
         sta acc
         pla
         sta flgs
         pla
         tax
         pla
         tay
         sec
         txa
         sbc #$02
         sta pcl
         tya
         sbc #0
         sta pch
         tsx
         stx sp
         jsr crlf
         ldx #$42
         lda #$2a
         jsr wrtwo
         lda #$52
         bne s0
                          ; ** increment temp pointer **
inctmp   inc tmp0
         bne setwr
         inc tmp0+1
         bne setwr
         inc wrap
setwr    rts
                          ; ** read character **
rdoc     jsr rdt
         cmp #$0d
         bne setwr
         pla
         pla
                          ; ** prompt for command  **
strt     lda #0
         sta wrap
         ldx #$0d
         lda #$2e
         jsr wrtwo
                          ; ** check input command **
st1      jsr rdoc
         cmp #$2e
         beq st1
         cmp #$20
         beq st1
s0       ldx #$0e
s1       cmp keyw,x
         bne s2
         txa
         asl a
         tax
         lda kaddr+1,x
         pha
         lda kaddr,x
         pha
         rts
s2       dex
         bpl s1
         jmp error
                          ; ** set pc addrress **
putp     lda tmp0
         sta pcl
         lda tmp0+1
         sta pch
         rts
                          ; ** memory display **
dm       lda #$08
dm0      sta tmpc
         ldy #0
dm1      jsr space
         lda (tmp0),y
         jsr wrob
         jsr inctmp
         dec tmpc
         bne dm1
         rts
                          ; ** change memory **
byte     jsr rdob
         bcc by3
         ldx #0
         sta (tmp0,x)
         cmp (tmp0,x)
         beq by3
         jmp error
by3      jsr inctmp
         dec tmpc
         rts
                          ; ** set pc reg addrs **
setr     lda #<pch+2
         sta tmp0
         lda #>pch+2
         sta tmp0+1
         lda #5
         rts
                          ; ** write display prompt **
altrit   tya 
         pha
         jsr crlf
         pla
         ldx #$2e
         jmp wrtwo
                          ; **display registers **
dsplyr   ldx #0
d2       lda regk,x
         jsr wrt
         inx
         cpx #$16
         bne d2
         ldy #$3b
         jsr altrit
         lda pch
         jsr wrob
         lda pcl
         jsr wrob
         jsr setr
         jsr dm0
         beq a9
                          ; ** display memory **
dsplym   jsr rdoc
         jsr rdoa
         bcc errs1
         lda tmp0
         sta tmp2
         lda tmp0+1
         sta tmp2+1
         jsr rdt
         cmp #$0d
         beq dsp1
         jsr rdoa
         bcc errs1
         jsr t2t2
dsp1     ;jsr stop1
         ;beq a9
         ldx wrap
         bne a9
         jsr diffp
         bcc a9
         ldy #$3a
         jsr altrit
         jsr wroa
         jsr dm
         beq dsp1
errs1    jmp error
                          ; ** alter memory **
altr     jsr rdoa
         bcc al2
         jsr putp
al2      jsr setr
         bne a0
altm     jsr rdoa
         bcc errs1
a4       lda #$08
a0       sta tmpc
a5       jsr rdoc
         jsr byte
         bne a5
a9       jmp strt
                          ; ** .go **
go       jsr rdt
         cmp #$0d
         beq g1
         cmp #$20
         bne errs1
         jsr rdoa
         bcc g1
         jsr putp
g1       ldx sp
         txs
         sei
         lda pch
         pha
         lda pcl
         pha
         lda flgs
         pha
         lda acc
         ldx xr
         ldy yr
         rti
                          ; ** back to basic **
exit     ldx sp
         txs
         jmp ready
                          ; ** load/save **
ld       ldy #1
         sty fa
         sty wtype
         dey
         sty fnlen
         sty satus
         sty lvflag
         lda #<addrs
         sta fnadr
         lda #>addrs
         sta fnadr+1
l1       jsr rdt
         cmp #$20
         beq l1
         cmp #$0d
         beq l5
         cmp #$22
l2       bne errl
l3       jsr rdt
         cmp #$22
         beq l8
         cmp #$0d
         beq l5
         sta (fnadr),y
         inc fnlen
         iny
         cpy #$10
l4       bne l3
errl     jmp error
l8       jsr rdt
         cmp #$0d
         beq l5
         cmp #$2c
         bne l2
         jsr rdob
         and #$0f
         beq l4
         cmp #$03
         beq l4
         sta fa
         jsr rdt
         cmp #$0d
l5       rts
ld15     jmp (ldvec)
sv15     jmp (svvec)
load     jsr ld
         bne errl
         lda #$00
         jsr ld15
         lda satus
         and #$10
         bne errl
         jmp strt
save     jsr ld
         cmp #$2c
         bne errl
         jsr rdoa
         jsr t2t2
         jsr rdt
         cmp #$2c
         bne errl
         jsr rdoa
         lda tmp0
         sta eal
         lda tmp0+1
         sta eah
         jsr t2t2
         jsr rdt
         cmp #$0d
         bne errl
         jsr sv15
         jmp strt
                          ; ** print hex byte **
wroa     lda tmp0+1
         jsr wrob
         lda tmp0
wrob     pha 
         lsr a
         lsr a
         lsr a
         lsr a
         jsr ascii
         tax
         pla
         and #$0f
         jsr ascii
                          ; ** print two chars **
wrtwo    pha 
         txa
         jsr wrt
         pla
         jmp wrt
                          ; ** convert to ascii **
ascii    ora #$30
         cmp #$3a
         bcc asc1
         adc #$06
asc1     rts 
                          ; ** swap tmp0, tmp2 **
t2t2     ldx #$02
t2t21    lda tmp0-1,x
         pha
         lda tmp2-1,x
         sta tmp0-1,x
         pla
         sta tmp2-1,x
         dex
         bne t2t21
         rts
                          ; ** read hex address **
rdoa     jsr rdob
rdoa1    bcc rdoa2
         sta tmp0+1
rdoa2    jsr rdob
         bcc rdexit
         sta tmp0
rdexit   rts
rdob     lda #0
                          ; ** scan for hex byte **
         sta acmd
         jsr rdoc
rdob1    cmp #$20
         bne rdob2
         jsr rdoc
         cmp #$20
         bne rdob3
         clc
         rts
                          ; ** read hex byte **
rdob2    jsr hexit
         asl a
         asl a
         asl a
         asl a
         sta acmd
         jsr rdoc
rdob3    jsr hexit
         ora acmd
         sec
         rts
                          ; ** convert from hex **
hexit    cmp #$3a
         bcc hex08
         adc #$08
hex08    and #$0f
         rts
                          ; ** decrement t0,t2 **
spread   = tmp2-tmp0
dect2    ldx #<spread
         .db $2c
dect0    ldx #0
         ldy tmp0,x
         bne dec20
         ldy tmp0+1,x
         bne dec10
         inc wrap
dec10    dec tmp0+1,x
dec20    dec tmp0,x
ret1     rts 
                          ; ** get a non space **
gtchr    jsr rdoc
         cmp #$20
         beq gtchr
         rts
                          ; ** get address **
input    lda #0
         sta inbuf
         jsr gtchr
         jsr rdob1
         jsr rdoa1
         bcc error
         rts
                          ; ** get address **
get2b    jsr rdoc
         jsr rdoa
         bcs ret1
                          ; ** error exit **
error    ldx sp
         txs
         lda #$3f
         jsr wrt
         jmp strt
                          ; ** print spaces **
spacd    jsr space
         dex
         bne spacd
         rts
                          ; ** inc tmp 2 **
ptrinc   inc tmp2
         bne p1ov
         inc tmp2+1
p1ov     rts 
                          ; ** swap aflg, tmp0 **
swap     ldx #$02
swp1     lda tmp0-1,x
         pha
         lda aflg-1,x
         sta tmp0-1,x
         pla
         sta aflg-1,x
         dex
         bne swp1
         rts
                          ; ** calc tmp2-tmp0-2 **
diffb    lda tmp2
         ldy tmp2+1
         sec
         sbc #2
         bcs deck
         dey
         bcc deck
                          ; ** calc aflg-tmp0 **
diffa    lda aflg
         ldy aflg+1
         jmp deck
                          ; ** calc tmp2-tmp0 **
diffp    lda tmp2
         ldy tmp2+1
deck     sec 
         sbc tmp0
         sta nemo
         tya
         sbc tmp0+1
         tay
         ora nemo
         rts
                          ; ** .transfer **
trans    jsr input
         jsr t2t2
         jsr get2b
         jsr swap
         jsr get2b
         jsr diffp
         jsr t2t2
         bcc mov2
                          ; ** move down **
mllp1    ldx wrap
         bne strt1
         jsr diffa
         bcc strt1
         lda (tmp0,x)
         sta (tmp2,x)
         jsr ptrinc
         jsr inctmp
         bne mllp1
                          ; ** move up **
mov2     jsr diffa
         clc
         lda nemo
         adc tmp2
         sta tmp2
         tya
         adc tmp2+1
         sta tmp2+1
         jsr swap
mllp2    ldx wrap
         bne strt1
         lda (tmp0,x)
         sta (tmp2,x)
         jsr diffa
         bcs strt1
         jsr dect2
         jsr dect0
         jmp mllp2
                          ; ** .fill **
fill     jsr input
         jsr t2t2
         jsr get2b
         jsr t2t2
         jsr rdoc
         jsr rdob
         bcc error2
         sta tmpc
flup1    ldx wrap
         bne strt1
         jsr diffp
         bcc strt1
         lda tmpc
         sta (tmp0,x)
         jsr inctmp
         bne flup1
error2   jmp error
strt1    jmp strt
                          ; ** .hunt **
hunt     jsr input
         jsr t2t2
         jsr get2b
         jsr t2t2
         jsr rdoc
         ldx #0
         jsr rdoc
         cmp #$27
         bne nostrh
         jsr rdoc
hpar     sta stage,x
         inx
         jsr rdt
         cmp #$0d
         beq htgo
         cpx #$20
         bne hpar
         beq htgo
nostrh   stx inbuf
         jsr rdob1
         bcc error2
hlp      sta stage,x
         inx
         jsr rdt
         cmp #$0d
         beq htgo
         jsr rdob
         bcc error2
         cpx #$20
         bne hlp
htgo     stx savx
         jsr crlf
hscan    ldx #0
         ldy #0
hlp3     lda (tmp0),y
         cmp stage,x
         bne hnmtch
         iny
         inx
         cpx savx
         bne hlp3
                          ; ** match found **
         jsr wroa
         jsr space
hnmtch   jsr inctmp
         ldx wrap
         bne strt1
         jsr diffp
         bcs hscan
         jmp strt
                          ; ** .disassemble **
disas    jsr input
         sta psav
         lda tmp0+1
         sta psav+1
         ldx #0
         stx aflg
dpag     lda #$93         ;clr
         jsr wrt
         lda #$16
         sta tmpc
dislp    jsr diss1
         jsr pcadj
         sta tmp0
         sty tmp0+1
         dec tmpc
         bne dislp
         lda #$91         ;up
         jsr wrt
         jmp strt
                          ; ** diss 1 line **
diss1    ldy #','
         jsr altrit
         jsr space
diss1a   jsr wroa
         jsr space
         ldx #0
         lda (tmp0,x)
         jsr instxx
         pha 
         jsr disvv
         pla 
         jsr propxx
         ldx #$06
pradr1   cpx #$03
         bne pradr3
         ldy length
         beq pradr3
pradr2   lda acmd
         cmp #$e8
         lda (tmp0),y
         bcs reladr
         jsr prbyte
         dey 
         bne pradr2
pradr3   asl acmd
         bcc pradr4
         lda char1-1,x
         jsr chrout
         lda char2-1,x
         beq pradr4
         jsr chrout
pradr4   dex 
         bne pradr1
         rts 
                          ; ** print rel address **
reladr   jsr pcadj3
         tax 
         inx 
         bne prntyx
         iny 
prntyx   tya 
         jsr prbyte
         txa 
prbyte   stx savx
         jsr wrob
         ldx savx
         rts 
                          ; ** add length+1 to pc **
pcadj    lda length
         sec 
pcadj3   ldy tmp0+1
         tax 
         bpl pcadj4
         dey 
pcadj4   adc tmp0
         bcc rts1
         iny 
rts1     rts 
                          ; ** check inst valid, len **
instxx   tay 
         lsr a
         bcc ieven
         lsr a
         bcs err
         cmp #$22
         beq err
         and #$07
         ora #$80
ieven    lsr a
         tax 
         lda mode,x
         bcs rtmode
         lsr a
         lsr a
         lsr a
         lsr a
rtmode   and #$0f
         bne getfmt
err      ldy #$80
         lda #0
                          ; ** get addr mode, length **
getfmt   tax 
         lda mode2,x
         sta acmd
         and #$03
         sta length
                          ; ** extract intructn **
         tya 
         and #$8f
         tax 
         tya 
         ldy #$03
         cpx #$8a
         beq mnndx3
mnndx1   lsr a
         bcc mnndx3
         lsr a
mnndx2   lsr a
         ora #$20
         dey 
         bne mnndx2
         iny 
mnndx3   dey 
         bne mnndx1
         rts 
                          ; print bytes
disvv    lda (tmp0),y
         jsr prbyte
         ldx #1
disvl    jsr spacd
         cpy length
         iny 
         bcc disvv
         ldx #$03
         cpy #4
         bcc disvl
         rts 
                          ; ** print mnemonic **
propxx   tay 
         lda mneml,y
         sta aflg
         lda mnemr,y
         sta aflg+1
prmn1    lda #0
         ldy #$05
prmn2    asl aflg+1
         rol aflg
         rol a
         dey 
         bne prmn2
         adc #$3f
         jsr wrt
         dex 
         bne prmn1
                          ; ** print space **
space    lda #$20
         bne flip
                          ; ** print cr, maybe lf **
crlf     lda #$0d
         bit $13
         bpl flip
         jsr wrt
         lda #$0a
flip     jmp wrt
                          ; **.p disassemble **
prin     jsr input
         jsr t2t2
         jsr get2b
         jsr t2t2
         ldx #0
         stx aflg
ploop    jsr crlf
         jsr diss1a
         jsr pcadj
         sta tmp0
         sty tmp0+1
         ;jsr stop1
         ;beq strtx
         jsr diffp
         bcs ploop
strtx    jmp strt
                          ; ** re-disassemble **
redis    jsr input
         lda #$03
         sta tmpc
rlp      jsr rdoc
         jsr byte
         bne rlp
         lda psav
         sta tmp0
         lda psav+1
         sta tmp0+1
         jmp dpag
chrout   cmp aflg
         beq caltrit
         jsr wrt
caltrit  rts 
                          ; ** assemble **
assem    jsr input
assrp    jsr t2t2
         stx stage+1
         ldx #$03
apush    jsr gtchr
         pha 
         dex 
         bne apush
         ldx #$03
apull    pla 
         sec 
         sbc #$3f
         ldy #$05
acrun    lsr a
         ror stage+1
         ror stage
         dey 
         bne acrun
         dex 
         bne apull
         ldx #$02
ainp     jsr rdt
         cmp #$0d
         beq aret
         cmp #$20
         beq ainp
         jsr ahex
         bcs stone
         jsr rdob2
         ldy tmp0
         sty tmp0+1
         sta tmp0
         lda #$30
         sta stage,x
         inx 
stone    sta stage,x
         inx 
         bne ainp
aret     stx aflg
         ldx #0
         stx wrap
         beq atry
abump    inc wrap
         beq aerr
atry     ldx #0
         stx tmpc
         lda wrap
         jsr instxx
         ldx acmd
         stx aflg+1
         tax 
         ldy mneml,x
         lda mnemr,x
         jsr achek2
         bne abump
         ldx #$06
aoprnd   cpx #$03
         bne ashf
         ldy length
         beq ashf
arsc     lda acmd
         cmp #$e8
         lda #$30
         bcs arel1
         jsr acheck
         bne abump
         jsr achick
         bne abump
         dey 
         bne arsc
ashf     asl acmd
         bcc adex
         ldy char2-1,x
         lda char1-1,x
         jsr achek2
         bne abump
adex     dex 
         bne aoprnd
         beq ald
arel1    jsr acdb1
         bne abump
         jsr acdb1
         bne abump
ald      lda aflg
         cmp tmpc
         bne abump
         jsr t2t2
         ldy length
         beq aopset
         lda aflg+1
         cmp #$9d
         bne aopnd
         jsr diffb
         bcc abdown
         tya 
         bne aerr
         lda nemo
         bpl abran
aerr     jmp error
abdown   iny 
         bne aerr
         lda nemo
         bpl aerr
abran    ldy length
         bne abrel
aopnd    lda tmp2-1,y
abrel    sta (tmp0),y
         dey 
         bne aopnd
aopset   lda wrap
         sta (tmp0),y
         jsr pcadj
         sta tmp0
         sty tmp0+1
         ldy #$41
         jsr altrit
         jsr space
         jsr wroa
         jsr space
         jmp assrp
acdb1    tay 
achek2   jsr acheck
         bne acex
         tya 
acheck   beq acex
achick   stx savx
         ldx tmpc
         cmp stage,x
         php 
         inx
         stx tmpc
         ldx savx
         plp 
acex     rts 
ahex     cmp #$30
         bcc asx
         cmp #$47
         rts 
asx      sec 
         rts 
                          ; mode table... nybble organized
                          ; 0= err  4= implied  8= zer,x   c= zer,y
                          ; 1= imm  5= acc      9= abs,x   d= rel
                          ; 2= zer  6= (ind,x)  a= abs,y
                          ; 3= abs  7= (ind),y  b= (ind)
mode     .db $40,$02,$45,$03
         .db $d0,$08,$40,$09
         .db $30,$22,$45,$33
         .db $d0,$08,$40,$09
         .db $40,$02,$45,$33
         .db $d0,$08,$40,$09
         .db $40,$02,$45,$b3
         .db $d0,$08,$40,$09
         .db $00,$22,$44,$33
         .db $d0,$8c,$44,$00
         .db $11,$22,$44,$33
         .db $d0,$8c,$44,$9a
         .db $10,$22,$44,$33
         .db $d0,$08,$40,$09
         .db $10,$22,$44,$33
         .db $d0,$08,$40,$09
         .db $62,$13,$78,$a9
                          ;master modes
                          ;six hi-order bits  mode options
                          ;two lo-order bits  operand lengthready.
mode2    .db $00,$21,$81,$82
         .db $00,$00,$59,$4d
         .db $91,$92,$86,$4a,$85,$9d
char1    .db $2c,$29,$2c,$23,$28,$24
char2    .db $59,$00,$58,$24,$24,$00
                          ; packed mnemonics
mneml    .db $1c,$8a,$1c,$23
         .db $5d,$8b,$1b,$a1
         .db $9d,$8a,$1d,$23
         .db $9d,$8b,$1d,$a1
         .db $00,$29,$19,$ae
         .db $69,$a8,$19,$23
         .db $24,$53,$1b,$23
         .db $24,$53,$19,$a1
         .db $00,$1a,$5b,$5b
         .db $a5,$69,$24,$24
         .db $ae,$ae,$a8,$ad
         .db $29,$00,$7c,$00
         .db $15,$9c,$6d,$9c
         .db $a5,$69,$29,$53
         .db $84,$13,$34,$11
         .db $a5,$69,$23,$a0

mnemr    .db $d8,$62,$5a,$48
         .db $26,$62,$94,$88
         .db $54,$44,$c8,$54
         .db $68,$44,$e8,$94
         .db $00,$b4,$08,$84
         .db $74,$b4,$28,$6e
         .db $74,$f4,$cc,$4a
         .db $72,$f2,$a4,$8a
         .db $00,$aa,$a2,$a2
         .db $74,$74,$74,$72
         .db $44,$68,$b2,$32
         .db $b2,$00,$22,$00
         .db $1a,$1a,$26,$26
         .db $72,$72,$88,$c8
         .db $c4,$ca,$26,$48
         .db $44,$44,$a2,$c8
keyw     .text ':;rmgxls'
         .text 'tfhdp,a'
kaddr    .dw altm-1
         .dw altr-1
         .dw dsplyr-1
         .dw dsplym-1
         .dw go-1
         .dw exit-1
         .dw load-1
         .dw save-1
         .dw trans-1
         .dw fill-1
         .dw hunt-1
         .dw disas-1
         .dw prin-1
         .dw redis-1
         .dw assem-1
         .dw strt-1        ;n
linkad   .dw break
sadr     .dw super
regk     .db $0d
         .text '   pc  sr'
         .text ' ac xr yr sp'
         
 .END
 