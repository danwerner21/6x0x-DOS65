; ---------------------------------------------------------------------------
; rs232_tx.s
; ---------------------------------------------------------------------------
;
; Write a string to the transmit UART FIFO

.export         _rs232_tx
.exportzp       _rs232_data: near

.define		UART1DATA	$F004		; SERIAL PORT 1 (I/O Card)
.define		UART1STATUS	$F005		; SERIAL PORT 1 (I/O Card)
.define		UART1COMMAND	$F006		; SERIAL PORT 1 (I/O Card)
.define		UART1CONTROL	$F007		; SERIAL PORT 1 (I/O Card)

.zeropage

_rs232_data:    .res 2, $00      ;  Reserve a local zero page pointer

.segment  "CODE"

.proc _rs232_tx: near



	

	
; ---------------------------------------------------------------------------
; Store pointer to zero page memory and load first character

        sta     _rs232_data      ;  Set zero page pointer to string address
        stx     _rs232_data+1    ;  (pointer passed in via the A/X registers)
        ldy     #00              ;  Initialize Y to 0
        lda     (_rs232_data),Y  ;  Load first character

; ---------------------------------------------------------------------------
; Main loop:  read data and store to FIFO until \0 is encountered

loop: 
	PHA			;
WRSER1a:	
	LDA	UART1STATUS	; GET STATUS
	AND	#%00010000	; IS TX READY
	BEQ	WRSER1a		; NO, WAIT FOR IT
	PLA			;
	STA	UART1DATA	; WRITE DATA
        iny                     ;         Increment Y index
        lda     (_rs232_data),y ;         Get next character
        bne     loop            ;         If character == 0, exit loop

; ---------------------------------------------------------------------------
; Append CR/LF to output stream and return

        rts                      ;  Return

.endproc

