;
; File generated by cc65 v 2.18 - Ubuntu 2.19-1
;
	.fopt		compiler,"cc65 v 2.18 - Ubuntu 2.19-1"
	.setcpu		"6502"
	.smart		on
	.autoimport	on
	.case		on
	.debuginfo	off
	.importzp	sp, sreg, regsave, regbank
	.importzp	tmp1, tmp2, tmp3, tmp4, ptr1, ptr2, ptr3, ptr4
	.macpack	longbranch
	.forceimport	__STARTUP__
	.import		_cputs
	.import		_cprintf
	.import		_strncmp
	.import		_strncpy
	.import		_strtok
	.import		_strtoul
	.export		_floppy35720dcb
	.export		_floppy35144dcb
	.export		_floppy525360dcb
	.export		_floppy52512dcb
	.export		_hdddcb
	.export		_prtusage
	.export		_prtdevice
	.export		_prttable
	.export		_parsecmd
	.export		_mapdrive
	.export		_updatedosmap
	.export		_toupper
	.export		_main

.segment	"DATA"

_floppy35720dcb:
	.byte	$5E
	.byte	$01
	.byte	$24
	.byte	$00
	.byte	$04
	.byte	$00
	.byte	$01
	.byte	$7F
	.byte	$00
_floppy35144dcb:
	.byte	$5E
	.byte	$01
	.byte	$24
	.byte	$00
	.byte	$04
	.byte	$00
	.byte	$01
	.byte	$7F
	.byte	$00
_floppy525360dcb:
	.byte	$5E
	.byte	$01
	.byte	$24
	.byte	$00
	.byte	$04
	.byte	$00
	.byte	$01
	.byte	$7F
	.byte	$00
_floppy52512dcb:
	.byte	$5E
	.byte	$01
	.byte	$24
	.byte	$00
	.byte	$04
	.byte	$00
	.byte	$01
	.byte	$7F
	.byte	$00
_hdddcb:
	.byte	$FF
	.byte	$07
	.byte	$40
	.byte	$00
	.byte	$10
	.byte	$00
	.byte	$02
	.byte	$FF
	.byte	$01

.segment	"RODATA"

L00DD:
	.byte	$20,$20,$20,$20,$41,$53,$53,$49,$47,$4E,$20,$44,$3A,$3D,$5B,$7B
	.byte	$44,$3A,$7C,$3C,$64,$65,$76,$69,$63,$65,$3E,$5B,$3C,$75,$6E,$69
	.byte	$74,$6E,$75,$6D,$3E,$5D,$3A,$5B,$3C,$73,$6C,$69,$63,$65,$6E,$75
	.byte	$6D,$3E,$5D,$7D,$5D,$20,$7B,$2F,$66,$6C,$61,$67,$73,$7D,$20,$0A
	.byte	$0D,$00
L00E9:
	.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$41,$53,$53,$49,$47,$4E
	.byte	$20,$43,$3A,$3D,$49,$44,$45,$30,$3A,$31,$09,$28,$61,$73,$73,$69
	.byte	$67,$6E,$20,$43,$3A,$20,$74,$6F,$20,$49,$44,$45,$20,$75,$6E,$69
	.byte	$74,$30,$2C,$20,$73,$6C,$69,$63,$65,$20,$31,$29,$20,$0A,$0D,$00
L00E0:
	.byte	$20,$20,$20,$20,$20,$20,$65,$78,$3A,$20,$41,$53,$53,$49,$47,$4E
	.byte	$09,$09,$28,$64,$69,$73,$70,$6C,$61,$79,$20,$61,$6C,$6C,$20,$61
	.byte	$63,$74,$69,$76,$65,$20,$64,$72,$69,$76,$65,$20,$61,$73,$73,$69
	.byte	$67,$6E,$6D,$65,$6E,$74,$73,$29,$20,$0A,$0D,$00
L00E6:
	.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$41,$53,$53,$49,$47,$4E
	.byte	$20,$43,$3A,$3D,$46,$44,$30,$3A,$09,$28,$61,$73,$73,$69,$67,$6E
	.byte	$20,$43,$3A,$20,$74,$6F,$20,$66,$6C,$6F,$70,$70,$79,$20,$75,$6E
	.byte	$69,$74,$20,$30,$29,$20,$0A,$0D,$00
L00E3:
	.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$41,$53,$53,$49,$47,$4E
	.byte	$20,$2F,$3F,$09,$09,$28,$64,$69,$73,$70,$6C,$61,$79,$20,$76,$65
	.byte	$72,$73,$69,$6F,$6E,$20,$61,$6E,$64,$20,$75,$73,$61,$67,$65,$29
	.byte	$20,$0A,$0D,$00
L00FB:
	.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$50,$50,$49,$44,$45,$31
	.byte	$3A,$20,$53,$45,$43,$4F,$4E,$44,$41,$52,$59,$20,$50,$50,$49,$44
	.byte	$45,$20,$46,$49,$58,$45,$44,$20,$44,$49,$53,$4B,$0A,$0D,$00
L0120:
	.byte	$41,$73,$73,$69,$67,$6E,$65,$64,$20,$64,$72,$69,$76,$65,$20,$6D
	.byte	$75,$73,$74,$20,$62,$65,$20,$69,$6E,$20,$74,$68,$65,$20,$72,$61
	.byte	$6E,$67,$65,$20,$6F,$66,$20,$41,$2D,$48,$2E,$0A,$0D,$00
L0101:
	.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2F,$33,$35,$20,$20,$20
	.byte	$20,$20,$33,$2E,$35,$20,$49,$4E,$43,$48,$20,$46,$4C,$4F,$50,$50
	.byte	$59,$20,$28,$44,$45,$46,$41,$55,$4C,$54,$29,$0A,$0D,$00
L00F8:
	.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$50,$50,$49,$44,$45,$30
	.byte	$3A,$20,$50,$52,$49,$4D,$41,$52,$59,$20,$50,$50,$49,$44,$45,$20
	.byte	$46,$49,$58,$45,$44,$20,$44,$49,$53,$4B,$0A,$0D,$00
L00F5:
	.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$46,$44,$31,$3A,$20,$20
	.byte	$20,$20,$46,$4C,$4F,$50,$50,$59,$20,$44,$49,$53,$4B,$20,$55,$4E
	.byte	$49,$54,$20,$31,$0A,$0D,$00
L00F2:
	.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$46,$44,$30,$3A,$20,$20
	.byte	$20,$20,$46,$4C,$4F,$50,$50,$59,$20,$44,$49,$53,$4B,$20,$55,$4E
	.byte	$49,$54,$20,$30,$0A,$0D,$00
L0104:
	.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2F,$35,$32,$35,$20,$20
	.byte	$20,$20,$35,$2E,$32,$35,$20,$49,$4E,$43,$48,$20,$46,$4C,$4F,$50
	.byte	$50,$59,$0A,$0D,$00
L00A1:
	.byte	$0A,$0D,$20,$44,$4F,$53,$2F,$36,$35,$20,$44,$72,$69,$76,$65,$20
	.byte	$61,$73,$73,$69,$67,$6E,$6D,$65,$6E,$74,$3A,$0A,$0D,$00
L017E:
	.byte	$55,$6E,$6B,$6F,$77,$6E,$20,$64,$65,$76,$69,$63,$65,$20,$61,$73
	.byte	$73,$69,$67,$6E,$6D,$65,$6E,$74,$2E,$20,$0A,$0D,$00
L00EF:
	.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$53,$44,$30,$3A,$20,$20
	.byte	$20,$20,$53,$44,$20,$44,$49,$53,$4B,$0A,$0D,$00
L00EC:
	.byte	$0A,$0D,$20,$50,$4F,$53,$53,$49,$42,$4C,$45,$20,$44,$45,$56,$49
	.byte	$43,$45,$53,$3A,$0A,$0D,$00
L00FE:
	.byte	$0A,$0D,$20,$50,$4F,$53,$53,$49,$42,$4C,$45,$20,$46,$4C,$41,$47
	.byte	$53,$3A,$0A,$0D,$00
L019B:
	.byte	$43,$68,$61,$6E,$67,$65,$64,$20,$74,$6F,$3A,$20,$20,$25,$63,$3A
	.byte	$3D,$00
L0123:
	.byte	$43,$75,$72,$72,$65,$6E,$74,$6C,$79,$3A,$20,$20,$20,$25,$63,$3A
	.byte	$3D,$00
L00DA:
	.byte	$20,$55,$73,$61,$67,$65,$3A,$20,$0A,$0D,$00
L00D2:
	.byte	$55,$4E,$4B,$4E,$4F,$57,$4E,$00
L0173:
	.byte	$50,$50,$49,$44,$45,$31,$3A,$00
L0168:
	.byte	$50,$50,$49,$44,$45,$30,$3A,$00
L00C4	:=	L00D2+0
L01AF:
	.byte	$28,$33,$36,$30,$4B,$29,$00
L012B:
	.byte	$3A,$25,$75,$20,$0A,$0D,$00
L00AD	:=	L019B+11
L00B4:
	.byte	$3A,$25,$69,$0A,$0D,$00
L00CE:
	.byte	$50,$50,$49,$44,$45,$00
L015F:
	.byte	$2F,$35,$32,$35,$00
L01AB	:=	L015F+0
L0135:
	.byte	$53,$44,$30,$3A,$00
L0140:
	.byte	$46,$44,$30,$3A,$00
L014B	:=	L015F+0
L0154:
	.byte	$46,$44,$31,$3A,$00
L01A3:
	.byte	$3A,$25,$75,$20,$00
L00BF:
	.byte	$53,$44,$00
L00C9:
	.byte	$46,$44,$00
L01B2	:=	L00DD+63
L00D5:
	.byte	$25,$69,$00
L0188	:=	L0173+6
L0093	:=	L019B+16
L0085	:=	L019B+16
L0076	:=	L01A3+3
L0071	:=	L01A3+3
L006C	:=	L01A3+3
L0183	:=	L0173+6

; ---------------------------------------------------------------
; void __near__ prtusage (void)
; ---------------------------------------------------------------

.segment	"CODE"

.proc	_prtusage: near

.segment	"CODE"

	lda     #<(L00DA)
	ldx     #>(L00DA)
	jsr     _cputs
	lda     #<(L00DD)
	ldx     #>(L00DD)
	jsr     _cputs
	lda     #<(L00E0)
	ldx     #>(L00E0)
	jsr     _cputs
	lda     #<(L00E3)
	ldx     #>(L00E3)
	jsr     _cputs
	lda     #<(L00E6)
	ldx     #>(L00E6)
	jsr     _cputs
	lda     #<(L00E9)
	ldx     #>(L00E9)
	jsr     _cputs
	lda     #<(L00EC)
	ldx     #>(L00EC)
	jsr     _cputs
	lda     #<(L00EF)
	ldx     #>(L00EF)
	jsr     _cputs
	lda     #<(L00F2)
	ldx     #>(L00F2)
	jsr     _cputs
	lda     #<(L00F5)
	ldx     #>(L00F5)
	jsr     _cputs
	lda     #<(L00F8)
	ldx     #>(L00F8)
	jsr     _cputs
	lda     #<(L00FB)
	ldx     #>(L00FB)
	jsr     _cputs
	lda     #<(L00FE)
	ldx     #>(L00FE)
	jsr     _cputs
	lda     #<(L0101)
	ldx     #>(L0101)
	jsr     _cputs
	lda     #<(L0104)
	ldx     #>(L0104)
	jmp     _cputs

.endproc

; ---------------------------------------------------------------
; void __near__ prtdevice (unsigned char)
; ---------------------------------------------------------------

.segment	"CODE"

.proc	_prtdevice: near

.segment	"CODE"

	jsr     pusha
	ldy     #$00
	lda     (sp),y
	and     #$F0
	beq     L00BD
	cmp     #$10
	beq     L00C2
	cmp     #$20
	beq     L00C7
	cmp     #$30
	beq     L00CC
	jmp     L00D0
L00BD:	lda     #<(L00BF)
	ldx     #>(L00BF)
	jsr     _cputs
	jmp     L00BB
L00C2:	lda     #<(L00C4)
	ldx     #>(L00C4)
	jsr     _cputs
	jmp     incsp1
L00C7:	lda     #<(L00C9)
	ldx     #>(L00C9)
	jsr     _cputs
	jmp     L00BB
L00CC:	lda     #<(L00CE)
	ldx     #>(L00CE)
	jsr     _cputs
	jmp     L00BB
L00D0:	lda     #<(L00D2)
	ldx     #>(L00D2)
	jsr     _cputs
	jmp     incsp1
L00BB:	lda     #<(L00D5)
	ldx     #>(L00D5)
	jsr     pushax
	ldy     #$02
	lda     (sp),y
	and     #$0F
	jsr     pusha0
	ldy     #$04
	jsr     _cprintf
	jmp     incsp1

.endproc

; ---------------------------------------------------------------
; void __near__ prttable (__near__ unsigned char *)
; ---------------------------------------------------------------

.segment	"CODE"

.proc	_prttable: near

.segment	"CODE"

	jsr     pushax
	jsr     decsp2
	lda     #<(L00A1)
	ldx     #>(L00A1)
	jsr     _cputs
	ldx     #$00
	txa
L01C7:	jsr     stax0sp
	cmp     #$10
	txa
	sbc     #$00
	bvc     L00AA
	eor     #$80
L00AA:	bpl     L00A4
	lda     #<(L00AD)
	ldx     #>(L00AD)
	jsr     pushax
	ldy     #$03
	jsr     ldaxysp
	jsr     asrax1
	ldy     #$41
	jsr     incaxy
	jsr     pushax
	ldy     #$04
	jsr     _cprintf
	ldy     #$05
	jsr     pushwysp
	ldy     #$03
	jsr     ldaxysp
	sta     regsave
	stx     regsave+1
	jsr     incax1
	ldy     #$02
	jsr     staxysp
	lda     regsave
	ldx     regsave+1
	jsr     tosaddax
	sta     ptr1
	stx     ptr1+1
	ldy     #$00
	lda     (ptr1),y
	jsr     _prtdevice
	lda     #<(L00B4)
	ldx     #>(L00B4)
	jsr     pushax
	ldy     #$03
	jsr     ldaxysp
	clc
	ldy     #$04
	adc     (sp),y
	sta     ptr1
	txa
	iny
	adc     (sp),y
	sta     ptr1+1
	ldx     #$00
	lda     (ptr1,x)
	jsr     pusha0
	ldy     #$04
	jsr     _cprintf
	jsr     ldax0sp
	jsr     incax1
	jmp     L01C7
L00A4:	jmp     incsp4

.endproc

; ---------------------------------------------------------------
; int __near__ parsecmd (__near__ unsigned char *, __near__ unsigned char *, __near__ unsigned char *, __near__ unsigned char *)
; ---------------------------------------------------------------

.segment	"CODE"

.proc	_parsecmd: near

.segment	"CODE"

	jsr     pushax
	jsr     push0
	ldy     #$09
	jsr     ldaxysp
	jsr     decax1
	sta     ptr1
	stx     ptr1+1
	ldy     #$00
	lda     (ptr1),y
	jsr     pusha
	jsr     decsp4
	ldy     #$0C
	jsr     ldaxysp
	sta     ptr1
	stx     ptr1+1
	lda     #$00
	tay
	sta     (ptr1),y
	ldy     #$0A
	jsr     ldaxysp
	sta     ptr1
	stx     ptr1+1
	lda     #$00
	tay
	sta     (ptr1),y
	ldy     #$08
	jsr     ldaxysp
	sta     ptr1
	stx     ptr1+1
	lda     #$00
	tay
	sta     (ptr1),y
	ldy     #$04
	lda     (sp),y
	cmp     #$80
	bcc     L0062
	lda     #$7F
	sta     (sp),y
L0062:	lda     (sp),y
	clc
	ldy     #$0D
	adc     (sp),y
	sta     ptr1
	lda     #$00
	iny
	adc     (sp),y
	sta     ptr1+1
	lda     #$00
	tay
	sta     (ptr1),y
	ldy     #$10
	jsr     pushwysp
	lda     #<(L006C)
	ldx     #>(L006C)
	jsr     _strtok
	ldy     #$02
	jsr     staxysp
	jsr     push0
	lda     #<(L0071)
	ldx     #>(L0071)
	jsr     _strtok
	ldy     #$02
	jsr     staxysp
	jsr     push0
	lda     #<(L0076)
	ldx     #>(L0076)
	jsr     _strtok
	jsr     stax0sp
	cpx     #$00
	bne     L01C8
	cmp     #$00
	beq     L0078
L01C8:	ldy     #$0A
	jsr     pushwysp
	ldy     #$05
	jsr     pushwysp
	ldx     #$00
	lda     #$05
	jsr     _strncpy
L0078:	ldy     #$02
	lda     (sp),y
	iny
	ora     (sp),y
	beq     L0095
	ldy     #$05
	jsr     pushwysp
	lda     #<(L0085)
	ldx     #>(L0085)
	jsr     _strtok
	ldy     #$02
	jsr     staxysp
	cpx     #$00
	bne     L01C9
	cmp     #$00
	beq     L0095
L01C9:	ldy     #$0E
	jsr     pushwysp
	ldy     #$07
	jsr     pushwysp
	ldx     #$00
	lda     #$1D
	jsr     _strncpy
	ldx     #$00
	lda     #$01
	ldy     #$05
	jsr     staxysp
	jsr     push0
	lda     #<(L0093)
	ldx     #>(L0093)
	jsr     _strtok
	ldy     #$02
	jsr     staxysp
	cpx     #$00
	bne     L01CA
	cmp     #$00
	beq     L0095
L01CA:	ldy     #$0C
	jsr     pushwysp
	ldy     #$07
	jsr     pushwysp
	ldx     #$00
	lda     #$1D
	jsr     _strncpy
	ldx     #$00
	lda     #$02
	ldy     #$05
	jsr     staxysp
L0095:	ldy     #$06
	jsr     ldaxysp
	ldy     #$0F
	jmp     addysp

.endproc

; ---------------------------------------------------------------
; void __near__ mapdrive (__near__ unsigned char *, __near__ unsigned char *, __near__ unsigned char *, __near__ unsigned char *)
; ---------------------------------------------------------------

.segment	"CODE"

.proc	_mapdrive: near

.segment	"CODE"

	jsr     pushax
	ldy     #$05
	jsr     ldaxysp
	sta     ptr1
	stx     ptr1+1
	ldy     #$00
	lda     (ptr1),y
	ldx     #$00
	and     #$5F
	ldy     #$41
	jsr     decaxy
	jsr     pusha
	lda     #$FF
	jsr     pusha
	jsr     decsp4
	lda     #$00
	jsr     pusha
	ldy     #$06
	lda     (sp),y
	cmp     #$08
	bcc     L011A
	lda     #<(L0120)
	ldx     #>(L0120)
	jmp     L01D1
L011A:	lda     #<(L0123)
	ldx     #>(L0123)
	jsr     pushax
	ldy     #$08
	ldx     #$00
	lda     (sp),y
	ldy     #$41
	jsr     incaxy
	jsr     pushax
	ldy     #$04
	jsr     _cprintf
	ldx     #$00
	ldy     #$06
	lda     (sp),y
	asl     a
	bcc     L01D2
	inx
	clc
L01D2:	ldy     #$0D
	adc     (sp),y
	sta     ptr1
	txa
	iny
	adc     (sp),y
	sta     ptr1+1
	ldy     #$00
	lda     (ptr1),y
	jsr     _prtdevice
	lda     #<(L012B)
	ldx     #>(L012B)
	jsr     pushax
	ldx     #$00
	ldy     #$08
	lda     (sp),y
	asl     a
	bcc     L01D3
	inx
	clc
L01D3:	ldy     #$0F
	adc     (sp),y
	pha
	txa
	iny
	adc     (sp),y
	tax
	pla
	jsr     incax1
	sta     ptr1
	stx     ptr1+1
	ldx     #$00
	lda     (ptr1,x)
	jsr     pusha0
	ldy     #$04
	jsr     _cprintf
	ldy     #$0A
	jsr     ldaxysp
	jsr     _toupper
	ldy     #$0C
	jsr     pushwysp
	lda     #<(L0135)
	ldx     #>(L0135)
	jsr     pushax
	ldx     #$00
	lda     #$04
	jsr     _strncmp
	stx     tmp1
	ora     tmp1
	bne     L0132
	ldy     #$05
	sta     (sp),y
	iny
	lda     (sp),y
	jsr     pusha
	lda     #<(_hdddcb)
	ldx     #>(_hdddcb)
	jsr     _updatedosmap
L0132:	ldy     #$0C
	jsr     pushwysp
	lda     #<(L0140)
	ldx     #>(L0140)
	jsr     pushax
	ldx     #$00
	lda     #$04
	jsr     _strncmp
	stx     tmp1
	ora     tmp1
	bne     L0148
	lda     #$20
	ldy     #$05
	sta     (sp),y
	iny
	lda     (sp),y
	jsr     pusha
	lda     #<(_floppy35720dcb)
	ldx     #>(_floppy35720dcb)
	jsr     _updatedosmap
	ldy     #$0A
	jsr     pushwysp
	lda     #<(L014B)
	ldx     #>(L014B)
	jsr     pushax
	ldx     #$00
	lda     #$04
	jsr     _strncmp
	stx     tmp1
	ora     tmp1
	bne     L0148
	ldy     #$06
	lda     (sp),y
	jsr     pusha
	lda     #<(_floppy525360dcb)
	ldx     #>(_floppy525360dcb)
	jsr     _updatedosmap
L0148:	ldy     #$0C
	jsr     pushwysp
	lda     #<(L0154)
	ldx     #>(L0154)
	jsr     pushax
	ldx     #$00
	lda     #$04
	jsr     _strncmp
	stx     tmp1
	ora     tmp1
	bne     L015C
	lda     #$21
	ldy     #$05
	sta     (sp),y
	iny
	lda     (sp),y
	jsr     pusha
	lda     #<(_floppy35720dcb)
	ldx     #>(_floppy35720dcb)
	jsr     _updatedosmap
	ldy     #$0A
	jsr     pushwysp
	lda     #<(L015F)
	ldx     #>(L015F)
	jsr     pushax
	ldx     #$00
	lda     #$04
	jsr     _strncmp
	stx     tmp1
	ora     tmp1
	bne     L015C
	ldy     #$06
	lda     (sp),y
	jsr     pusha
	lda     #<(_floppy525360dcb)
	ldx     #>(_floppy525360dcb)
	jsr     _updatedosmap
L015C:	ldy     #$0C
	jsr     pushwysp
	lda     #<(L0168)
	ldx     #>(L0168)
	jsr     pushax
	ldx     #$00
	lda     #$07
	jsr     _strncmp
	stx     tmp1
	ora     tmp1
	bne     L0165
	lda     #$30
	ldy     #$05
	sta     (sp),y
	iny
	lda     (sp),y
	jsr     pusha
	lda     #<(_hdddcb)
	ldx     #>(_hdddcb)
	jsr     _updatedosmap
L0165:	ldy     #$0C
	jsr     pushwysp
	lda     #<(L0173)
	ldx     #>(L0173)
	jsr     pushax
	ldx     #$00
	lda     #$07
	jsr     _strncmp
	stx     tmp1
	ora     tmp1
	bne     L0170
	lda     #$31
	ldy     #$05
	sta     (sp),y
	iny
	lda     (sp),y
	jsr     pusha
	lda     #<(_hdddcb)
	ldx     #>(_hdddcb)
	jsr     _updatedosmap
L0170:	ldy     #$05
	lda     (sp),y
	cmp     #$FF
	bne     L017B
	lda     #<(L017E)
	ldx     #>(L017E)
	jsr     pushax
	ldy     #$02
	jsr     _cprintf
	jmp     L0114
L017B:	ldy     #$0C
	jsr     pushwysp
	lda     #<(L0183)
	ldx     #>(L0183)
	jsr     _strtok
	ldy     #$03
	jsr     staxysp
	jsr     push0
	lda     #<(L0188)
	ldx     #>(L0188)
	jsr     _strtok
	ldy     #$03
	jsr     staxysp
	cpx     #$00
	bne     L01D9
	cmp     #$00
	beq     L01DA
L01D9:	ldy     #$06
	jsr     pushwysp
	lda     #$03
	jsr     leaa0sp
	jsr     pushax
	ldx     #$00
	lda     #$0A
	jsr     _strtoul
	ldy     #$00
	sta     (sp),y
	ldx     #$00
L01DA:	ldy     #$06
	lda     (sp),y
	asl     a
	bcc     L01D4
	inx
	clc
L01D4:	ldy     #$0D
	adc     (sp),y
	sta     ptr1
	txa
	iny
	adc     (sp),y
	sta     ptr1+1
	ldy     #$05
	lda     (sp),y
	ldy     #$00
	sta     (ptr1),y
	ldx     #$00
	ldy     #$06
	lda     (sp),y
	asl     a
	bcc     L01D5
	inx
	clc
L01D5:	ldy     #$0D
	adc     (sp),y
	pha
	txa
	iny
	adc     (sp),y
	tax
	pla
	jsr     incax1
	sta     ptr1
	stx     ptr1+1
	ldy     #$00
	lda     (sp),y
	sta     (ptr1),y
	lda     #<(L019B)
	ldx     #>(L019B)
	jsr     pushax
	ldy     #$08
	ldx     #$00
	lda     (sp),y
	ldy     #$41
	jsr     incaxy
	jsr     pushax
	ldy     #$04
	jsr     _cprintf
	ldx     #$00
	ldy     #$06
	lda     (sp),y
	asl     a
	bcc     L01D6
	inx
	clc
L01D6:	ldy     #$0D
	adc     (sp),y
	sta     ptr1
	txa
	iny
	adc     (sp),y
	sta     ptr1+1
	ldy     #$00
	lda     (ptr1),y
	jsr     _prtdevice
	lda     #<(L01A3)
	ldx     #>(L01A3)
	jsr     pushax
	ldx     #$00
	ldy     #$08
	lda     (sp),y
	asl     a
	bcc     L01D7
	inx
	clc
L01D7:	ldy     #$0F
	adc     (sp),y
	pha
	txa
	iny
	adc     (sp),y
	tax
	pla
	jsr     incax1
	sta     ptr1
	stx     ptr1+1
	ldx     #$00
	lda     (ptr1,x)
	jsr     pusha0
	ldy     #$04
	jsr     _cprintf
	ldy     #$0A
	jsr     pushwysp
	lda     #<(L01AB)
	ldx     #>(L01AB)
	jsr     pushax
	ldx     #$00
	lda     #$04
	jsr     _strncmp
	stx     tmp1
	ora     tmp1
	bne     L01A8
	lda     #<(L01AF)
	ldx     #>(L01AF)
	jsr     _cputs
L01A8:	lda     #<(L01B2)
	ldx     #>(L01B2)
L01D1:	jsr     _cputs
L0114:	ldy     #$0F
	jmp     addysp

.endproc

; ---------------------------------------------------------------
; void __near__ updatedosmap (unsigned char, unsigned char *)
; ---------------------------------------------------------------

.segment	"CODE"

.proc	_updatedosmap: near

.segment	"CODE"

	jsr     pushax
	lda     $002C
	ldx     $002C+1
	jsr     pushax
	jsr     pushw0sp
	ldy     #$06
	lda     (sp),y
	jsr     pusha0
	lda     #$0E
	jsr     tosumula0
	jsr     tosaddax
	jsr     pushax
	jsr     decsp2
	ldx     #$00
	txa
L01DC:	jsr     stax0sp
	cmp     #$09
	txa
	sbc     #$00
	bvc     L01C0
	eor     #$80
L01C0:	bpl     L01BA
	jsr     ldax0sp
	clc
	ldy     #$02
	adc     (sp),y
	sta     sreg
	txa
	iny
	adc     (sp),y
	sta     sreg+1
	jsr     ldax0sp
	clc
	ldy     #$06
	adc     (sp),y
	sta     ptr1
	txa
	iny
	adc     (sp),y
	sta     ptr1+1
	ldy     #$00
	lda     (ptr1),y
	sta     (sreg),y
	jsr     ldax0sp
	jsr     incax1
	jmp     L01DC
L01BA:	ldy     #$09
	jmp     addysp

.endproc

; ---------------------------------------------------------------
; void __near__ toupper (__near__ unsigned char *)
; ---------------------------------------------------------------

.segment	"CODE"

.proc	_toupper: near

.segment	"CODE"

	jsr     pushax
	jmp     L0109
L0107:	jsr     ldax0sp
	sta     ptr1
	stx     ptr1+1
	ldy     #$00
	lda     (ptr1),y
	cmp     #$61
	bcc     L010B
	jsr     ldax0sp
	sta     ptr1
	stx     ptr1+1
	ldy     #$00
	lda     (ptr1),y
	cmp     #$7B
	bcs     L010B
	jsr     ldax0sp
	sta     sreg
	stx     sreg+1
	jsr     ldax0sp
	sta     ptr1
	stx     ptr1+1
	ldy     #$00
	lda     (ptr1),y
	and     #$5F
	sta     (sreg),y
L010B:	jsr     ldax0sp
	jsr     incax1
	jsr     stax0sp
L0109:	jsr     ldax0sp
	sta     ptr1
	stx     ptr1+1
	ldy     #$00
	lda     (ptr1),y
	bne     L0107
	jmp     incsp2

.endproc

; ---------------------------------------------------------------
; int __near__ main (void)
; ---------------------------------------------------------------

.segment	"CODE"

.proc	_main: near

.segment	"CODE"

	lda     $002E
	ldx     $002E+1
	jsr     pushax
	lda     $0030
	ldx     $0030+1
	jsr     pushax
	ldy     #$5C
	jsr     subysp
	lda     $0030
	ldx     $0030+1
	jsr     pushax
	lda     #$3E
	jsr     leaa0sp
	jsr     pushax
	lda     #$22
	jsr     leaa0sp
	jsr     pushax
	lda     #$06
	jsr     leaa0sp
	jsr     _parsecmd
	ldy     #$5A
	jsr     staxysp
	cpx     #$00
	bne     L0044
	cmp     #$00
	beq     L0046
	cmp     #$01
	beq     L004A
	cmp     #$02
	beq     L004D
	jmp     L01E0
L0046:	ldy     #$5F
	jsr     ldaxysp
	jsr     _prttable
	jmp     L0044
L004A:	jsr     _prtusage
	jmp     L0044
L004D:	ldy     #$61
	jsr     pushwysp
	lda     #$3E
	jsr     leaa0sp
	jsr     pushax
	lda     #$22
	jsr     leaa0sp
	jsr     pushax
	lda     #$06
	jsr     leaa0sp
	jsr     _mapdrive
L0044:	ldx     #$00
L01E0:	txa
	ldy     #$60
	jmp     addysp

.endproc

