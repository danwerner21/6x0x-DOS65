PEM	        =	$103		; PEM ENTRY
DO_FARCALL      =       $FFF0
farfunct        =       $32             ; function to call in driver area

; void cputc (char c);
;

        .export         _cputc
        .export         _cgetc
        .export         gotoxy
        .export         _readrtc
        .export         _writertc
        .export         _cputserial
        .export         _cgetserial
        .import         popa

tmpsave:         .res    1

; Plot a character -- also used as an internal function
_cputc:
        	LDX	#2		        ; print char
	        JSR	PEM
                rts

_cgetc:
        	LDX	#6		        ; get char
	        JSR	PEM
                beq     _cgetc
                rts

_cputserial:
        	tax                     ; data in x
                LDA     #4             ; put serial port
                STA     farfunct
                txa
                JSR     DO_FARCALL
                rts

_cgetserial:
                LDA     #5             ; get serial port
                STA     farfunct
                JSR     DO_FARCALL
                rts

gotoxy:
                rts

; read rtc value.  unsigned char __fastcall__ readrtc (unsigned char address);
;
_readrtc:
                tax                     ; address in x
                LDA     #51             ; Read RTC
                STA     farfunct
                JSR     DO_FARCALL
                TYA                     ; return value in a
                rts

; write rtc value.  void __fastcall__ writertc (unsigned char address,unsigned char value);
;
_writertc:
                sta     tmpsave
                jsr     popa            ; Get address
                tax                     ; address in x
                ldy     tmpsave         ; value in y
                LDA     #50             ; write RTC
                STA     farfunct
                JSR     DO_FARCALL
                rts
