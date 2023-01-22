; mainargs.s
;
; Dan Werner 5/30/2011
;
; Scan a group of arguments that are in DOS/65's input-buffer.
; Build an array that points to the beginning of each argument.
; Send, to main(), that array and the count of the arguments.


        .constructor	initmainargs, 24
        .import		__argc, __argv, __dos_type

tmp1:	.byte    0


; Maximum number of arguments allowed in the argument table.
; (An argument contains a comma, at least.)

MAXARGS = 10


BUF_LEN = 122

BASIC_BUF = $128

; Get possible command-line arguments. Goes into the special INIT segment,
; which may be reused after the startup code is run.

        .segment        "INIT"

initmainargs:


; Start processing the arguments.

	ldx	#$00
	ldy	#$00		; Start with argv[1]

; Find the next argument. 

next:   lda	BASIC_BUF,x
        beq	done
        bmi	done
        inx
        cmp	#' '		; Skip leading spaces
        beq	next

; Found start of next argument. We've incremented the pointer in X already, so
; it points to the second character of the argument. This is useful since we
; will check now for a quoted argument, in which case we will have to skip this
; first character.

        cmp	#'"'		; Is the argument quoted?
        beq	:+		; Jump if so
        dex			; Reset pointer to first argument character
        lda	#' '		; A space ends the argument
:       sta     tmp1		; Set end of argument marker

; Now store a pointer to the argument into the next slot.

        txa			; Get low byte
        clc
        adc	#<BASIC_BUF
        sta	argv,y		; argv[y] = &arg
        iny
        lda	#$00
        adc	#>BASIC_BUF
        sta	argv,y
        iny
        inc	__argc		; Found another arg

; Search for the end of the argument.

:       lda     BASIC_BUF,x
        beq	done
        inx
        cmp	tmp1
        bne	:-

; We've found the end of the argument. X points one character behind it, and
; A contains the terminating character. To make the argument a valid C string,
; replace the terminating character by a zero.

        lda	#$00
        sta	BASIC_BUF-1,x

; Check if the maximum number of command-line arguments is reached. If not,
; parse the next one.

        lda	__argc		; Get low byte of argument count
        cmp	#MAXARGS	; Maximum number of arguments reached?
        bcc	next		; Parse next one if not

; (The last vector in argv[] already is NULL.)

done:   lda	#<argv
        ldx	#>argv
        sta	__argv
        stx	__argv+1
        rts

; This array is zeroed before initmainargs is called.
; char* argv[MAXARGS+1] = {FNAM};

        .data
argv:   .res	MAXARGS * 2

	.bss
