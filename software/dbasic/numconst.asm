

; numeric constants and series

; constants and series for LOG(n)
LAB_25A0:
        .BYTE   $02             ; counter
        .BYTE   $80,$19,$56,$62 ; 0.59898
        .BYTE   $80,$76,$22,$F3 ; 0.96147
;##	.byte	$80,$76,$22,$F1	; 0.96147
        .BYTE   $82,$38,$AA,$40 ; 2.88539
;##	.byte	$82,$38,$AA,$45	; 2.88539

LAB_25AD:
        .BYTE   $80,$35,$04,$F3 ; 0.70711	1/root 2
LAB_25B1:
        .BYTE   $81,$35,$04,$F3 ; 1.41421	root 2
LAB_25B5:
        .BYTE   $80,$80,$00,$00 ; -0.5
LAB_25B9:
        .BYTE   $80,$31,$72,$18 ; 0.69315	LOG(2)

; numeric PRINT constants
LAB_2947:
        .BYTE   $91,$43,$4F,$F8 ; 99999.9375 (max value with at least one decimal)
LAB_294B:
        .BYTE   $94,$74,$23,$F7 ; 999999.4375 (max value before scientific notation)
LAB_294F:
        .BYTE   $94,$74,$24,$00 ; 1000000

; EXP(n) constants and series
LAB_2AFA:
        .BYTE   $81,$38,$AA,$3B ; 1.4427	(1/LOG base 2 e)
LAB_2AFE:
        .BYTE   $06             ; counter
        .BYTE   $74,$63,$90,$8C ; 2.17023e-4
        .BYTE   $77,$23,$0C,$AB ; 0.00124
        .BYTE   $7A,$1E,$94,$00 ; 0.00968
        .BYTE   $7C,$63,$42,$80 ; 0.05548
        .BYTE   $7E,$75,$FE,$D0 ; 0.24023
        .BYTE   $80,$31,$72,$15 ; 0.69315
        .BYTE   $81,$00,$00,$00 ; 1.00000

;##	.byte	$07			; counter
;##	.byte	$74,$94,$2E,$40	; -1/7! (-1/5040)
;##	.byte	$77,$2E,$4F,$70	;  1/6! ( 1/720)
;##	.byte	$7A,$88,$02,$6E	; -1/5! (-1/120)
;##	.byte	$7C,$2A,$A0,$E6	;  1/4! ( 1/24)
;##	.byte	$7E,$AA,$AA,$50	; -1/3! (-1/6)
;##	.byte	$7F,$7F,$FF,$FF	;  1/2! ( 1/2)
;##	.byte	$81,$80,$00,$00	; -1/1! (-1/1)
;##	.byte	$81,$00,$00,$00	;  1/0! ( 1/1)

; trigonometric constants and series
LAB_2C78:
        .BYTE   $81,$49,$0F,$DB ; 1.570796371 (pi/2) as floating #
LAB_2C84:
        .BYTE   $04             ; counter
        .BYTE   $86,$1E,$D7,$FB ; 39.7109
;##	.byte	$86,$1E,$D7,$BA	; 39.7109
        .BYTE   $87,$99,$26,$65 ;-76.575
;##	.byte	$87,$99,$26,$64	;-76.575
        .BYTE   $87,$23,$34,$58 ; 81.6022
        .BYTE   $86,$A5,$5D,$E1 ;-41.3417
;##	.byte	$86,$A5,$5D,$E0	;-41.3417
LAB_2C7C:
        .BYTE   $83,$49,$0F,$DB ; 6.28319 (2*pi) as floating #
;##	.byte	$83,$49,$0F,$DA	; 6.28319 (2*pi) as floating #

LAB_2CC9:
        .BYTE   $08             ; counter
        .BYTE   $78,$3A,$C5,$37 ; 0.00285
        .BYTE   $7B,$83,$A2,$5C ;-0.0160686
        .BYTE   $7C,$2E,$DD,$4D ; 0.0426915
        .BYTE   $7D,$99,$B0,$1E ;-0.0750429
        .BYTE   $7D,$59,$ED,$24 ; 0.106409
        .BYTE   $7E,$91,$72,$00 ;-0.142036
        .BYTE   $7E,$4C,$B9,$73 ; 0.199926
        .BYTE   $7F,$AA,$AA,$53 ;-0.333331

;##	.byte	$08			; counter
;##	.byte	$78,$3B,$D7,$4A	; 1/17
;##	.byte	$7B,$84,$6E,$02	;-1/15
;##	.byte	$7C,$2F,$C1,$FE	; 1/13
;##	.byte	$7D,$9A,$31,$74	;-1/11
;##	.byte	$7D,$5A,$3D,$84	; 1/9
;##	.byte	$7E,$91,$7F,$C8	;-1/7
;##	.byte	$7E,$4C,$BB,$E4	; 1/5
;##	.byte	$7F,$AA,$AA,$6C	;-1/3

LAB_1D96        = *+1           ; $00,$00 used for undefined variables
LAB_259C:
        .BYTE   $81,$00,$00,$00 ; 1.000000, used for INC
LAB_2AFD:
        .BYTE   $81,$80,$00,$00 ; -1.00000, used for DEC. must be on the same page as +1.00

; misc constants
LAB_1DF7:
        .BYTE   $90             ;-32768 (uses first three bytes from 0.5)
LAB_2A96:
        .BYTE   $80,$00,$00,$00 ; 0.5
LAB_2C80:
        .BYTE   $7F,$00,$00,$00 ; 0.25
LAB_26B5:
        .BYTE   $84,$20,$00,$00 ; 10.0000 divide by 10 constant

; This table is used in converting numbers to ASCII.

LAB_2A9A:
LAB_2A9B        = LAB_2A9A+1
LAB_2A9C        = LAB_2A9B+1
        .BYTE   $FE,$79,$60     ; -100000
        .BYTE   $00,$27,$10     ; 10000
        .BYTE   $FF,$FC,$18     ; -1000
        .BYTE   $00,$00,$64     ; 100
        .BYTE   $FF,$FF,$F6     ; -10
        .BYTE   $00,$00,$01     ; 1
