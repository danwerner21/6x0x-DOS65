PEM	        =	$103		; PEM ENTRY
DO_FARCALL      =       $FFF0
farfunct        =       $32             ; function to call in driver area
M6X0X_IOSPACE   = $E000
UART1DATA       = M6X0X_IOSPACE+$FF4; SERIAL PORT 1 (I/O Card)
UART1STATUS     = M6X0X_IOSPACE+$FF5; SERIAL PORT 1 (I/O Card)
UART1COMMAND    = M6X0X_IOSPACE+$FF6; SERIAL PORT 1 (I/O Card)
UART1CONTROL    = M6X0X_IOSPACE+$FF7; SERIAL PORT 1 (I/O Card)

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
                sta     tmpsave
                LDA     #04
                STA     farfunct
                LDA     tmpsave
                JMP     DO_FARCALL


_cgetserial:
                LDA     #07             ; get status
                STA     farfunct
                JSR     DO_FARCALL
                CMP     #$FF
                BNE     :+              ; NO, INDICATE NO CHAR

                LDA     #05
                STA     farfunct
                JSR     DO_FARCALL
                LDX     #$00            ; set high bit to 0
                RTS
:
                LDA     #$00            ;
                LDX     #$01            ; set high bit to 1
                RTS                     ;


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
