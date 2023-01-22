; ---------------------------------------------------------------------------
; DOS65.s
; ---------------------------------------------------------------------------
;
; INTERFACE WITH DOS/65

.export         _DOS65_warmBoot
.export         _DOS65_ConsoleOut
.export         _DOS65_ConsoleIn


.define		PEM		$103		;PEM ENTRY

.segment  "CODE"

.proc _DOS65_warmBoot: near

	ldx	#0
	jmp	PEM

.endproc

.proc _DOS65_ConsoleOut: near
	pha
	txa
	tay
	pla	
	ldx	#9
	jmp	PEM

.endproc

.proc _DOS65_ConsoleIn: near
	pha
	txa
	tay
	pla	
	ldx	#10
	jmp	PEM
.endproc	
	


