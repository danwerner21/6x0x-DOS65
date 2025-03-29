PEM	        =	$103		; PEM ENTRY
IO              = $000300
UART0           = IO+$58        ;   DATA IN/OUT
UART1           = IO+$59        ;   CHECK RX
UART2           = IO+$5A        ;   INTERRUPTS
UART3           = IO+$5B        ;   LINE CONTROL
UART4           = IO+$5C        ;   MODEM CONTROL
UART5           = IO+$5D        ;   LINE STATUS
UART6           = IO+$5E        ;   MODEM STATUS
UART7           = IO+$5F        ;   SCRATCH REG.

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
WRSER1a:
                ;LDA     f:UART5         ; READ LINE STATUS REGISTER
                .byte    $AF,$5D,$03,$00
                AND     #$20            ; TEST IF UART IS READY TO SEND (BIT 5)
                CMP     #$00
                BEQ     WRSER1a         ; NO, WAIT FOR IT
                LDA     tmpsave
                ;STA     f:UART0         ; THEN WRITE THE CHAR TO UART
                .byte    $8F,$58,$03,$00
                RTS


_cgetserial:
                ;LDA     f:UART5         ; READ LINE STATUS REGISTER
                .byte    $AF,$5D,$03,$00
                AND     #$01            ; TEST IF DATA IN RECEIVE BUFFER
                CMP     #$00
                BEQ     :+              ; NO, INDICATE NO CHAR
                ;LDA     f:UART0         ; THEN READ THE CHAR FROM THE UART
                .byte    $AF,$58,$03,$00
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
                rts

; write rtc value.  void __fastcall__ writertc (unsigned char address,unsigned char value);
;
_writertc:
                rts
