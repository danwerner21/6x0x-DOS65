
        .IFNDEF  DUODYNE
.pc02

PPI_BASE        = $E230

ppiPortA        = PPI_BASE+0         ; Register
ppiPortB        = PPI_BASE+1         ; Register
ppiPortC        = PPI_BASE+2         ; Register
ppiControl      = PPI_BASE+3         ; Register

;___SOUND__________________________________________________
;
; PLAY SOUND
;
;  TAKES TWO PARAMETERS CHANNEL,FREQUENCY
;
;__________________________________________________________
LAB_SOUND:
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X (CHANNEL)
        PHX
        JSR     LAB_1C01        ; (AFTER ',') OR SYN ERR
        JSR     LAB_EVNM        ; evaluate expression and check is numeric,
; else do type mismatch
        JSR     LAB_F2FX        ; save integer part of FAC1 in temporary integer

        PLA                     ; LIMIT THE CHANNELS TO <3
        AND     #$03
        CLC
        ASL                     ; = *2
        PHA
        LDY     <Itempl
        JSR     psgwr           ; SET LOW BYTE
        PLA
        INC     A
        LDY     <Itemph
        JSR     psgwr           ; SET HIGH BYTE
        RTS

;___VOLUME__________________________________________________
;
; SET VOLUME
;
;  TAKES TWO PARAMETERS CHANNEL,VOLUME
;
;__________________________________________________________
LAB_VOLUME:
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X (CHANNEL)
        PHX
        JSR     LAB_1C01        ; (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X (VOLUME)
        TXA
        TAY
        PLA                     ; LIMIT THE CHANNELS TO <3
        AND     #$03

        CLC
        ADC     #08
        JSR     psgwr
        RTS


;___VOICE__________________________________________________
;
; SET VOICE
;
;  TAKES TWO PARAMETERS VOICE, ENVELOPE
;
;__________________________________________________________
LAB_VOICE:
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X (CHANNEL)
        PHX
        JSR     LAB_1C01        ; (AFTER ',') OR SYN ERR
        JSR     LAB_EVNM        ; evaluate expression and check is numeric,
; else do type mismatch
        JSR     LAB_F2FX        ; save integer part of FAC1 in temporary integer
        PLY
        LDA     #13
        JSR     psgwr

        LDA     #11
        LDY     <Itempl
        JSR     psgwr           ; SET LOW BYTE
        LDA     #12
        LDY     <Itemph         ; SET HIGH BYTE
        JSR     psgwr

        RTS





;___NOISE__________________________________________________
;
; SELECT NOISE CHANNEL
;
;  TAKES TWO PARAMETERS CHANNEL,FREQUENCY
;
;__________________________________________________________
LAB_NOISE:
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X (CHANNEL)
        PHX
        JSR     LAB_1C01        ; (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X (FREQ)
        PLA                     ; LIMIT THE CHANNELS TO <3
        PHX
        TAX

        AND     #%00011111
        TAY
        LDA     #$06
        JSR     psgwr           ; SET NOISE FREQ

        LDA     #$07
        JSR     psgrd           ; GET CONFIG
        PLA
        PHY
        AND     #$03
        TAX                     ; A=CONFIG, X=CHANNEL
        PLA
        CPX     #$00
        BNE     NOISE_1
        AND     #%00110111
        ORA     #%00000001
        BRA     NOISE_3
NOISE_1:
        CPX     #$01
        BNE     NOISE_2
        AND     #%00101111
        ORA     #%00000010
        BRA     NOISE_3
NOISE_2:
        AND     #%00011111
        ORA     #%00000100
NOISE_3:
        TAY
        LDA     #$07
        JSR     psgwr           ; SET LOW BYTE
        RTS

;___TONE___________________________________________________
;
; SELECT TONE CHANNEL
;
;  TAKES ONE PARAMETER CHANNEL
;
;__________________________________________________________
LAB_TONE:
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X (CHANNEL)
        PHX

        LDA     #$07
        JSR     psgrd           ; GET CONFIG
        PLA
        PHY
        AND     #$03
        TAX                     ; A=CONFIG, X=CHANNEL
        PLA
        CPX     #$00
        BNE     TONE_1
        AND     #%00111110
        ORA     #%00001000
        BRA     TONE_3
TONE_1:
        CPX     #$01
        BNE     TONE_2
        AND     #%00111101
        ORA     #%00010000
        BRA     TONE_3
TONE_2:
        AND     #%00111011
        ORA     #%00100000
TONE_3:
        TAY
        LDA     #$07
        JSR     psgwr           ; SET LOW BYTE
        RTS


;___CONTROLLER_______________________________________________
;
; GET JOYTICK STATUS
;
;  TAKES ONE PARAMETERS JOYSTICK#, RETURNS STATUS
;
;__________________________________________________________
LAB_CON:
        JSR     LAB_F2FX        ; save integer part of FAC1 in temporary integer
        LDA     <Itempl
        AND     #$01
        CLC
        ADC     #14
        JSR     psgrd           ; return value in y
        JMP     LAB_1FD0        ; convert Y to byte in FAC1 and return




;___utility functions____________________________________________
psginit:
        LDA     #%10000000
        STA     ppiControl
        LDA     #%00010000
        STA     ppiPortC
        LDA     #$00
        STA     ppiPortB
        RTS
        JSR     clrpsg
        LDA     #7
        LDY     #$3F
        JSR     psgwr
        RTS

psgrd:
        STA     ppiPortB      ; select register
        LDA     #%00011100      ; latch address
        STA     ppiPortC
        STA     ppiPortC
        STA     ppiPortC

        LDA     #%00010000      ; inact
        STA     ppiPortC
        STA     ppiPortC

        LDA     #%10000010
        STA     ppiControl
        LDA     #%00011000      ; latch data
        STA     ppiPortC
        STA     ppiPortC
        STA     ppiPortC

        LDA     ppiPortB      ; get data
        TAY
        LDA     #%10000000
        STA     ppiControl
        LDA     #%00010000      ; inact
        STA     ppiPortC
        RTS


psgwr:
        STA     ppiPortB      ; select register
        LDA     #%00011100      ; latch address
        STA     ppiPortC
        STA     ppiPortC
        STA     ppiPortC

        LDA     #%00010000      ; inact
        STA     ppiPortC
        STA     ppiPortC
        STA     ppiPortC
        TYA
        STA     ppiPortB      ; store data
        STA     ppiPortB      ; store data
        STA     ppiPortB      ; store data

        LDA     #%00010100      ; latch data
        STA     ppiPortC
        STA     ppiPortC
        STA     ppiPortC

        LDA     #%00010000      ; inact
        STA     ppiPortC
        RTS

;
; Clear PSG registers to default
;
clrpsg:
        LDX     #00
        LDY     #00
clrpsg1:
        TXA
        JSR     psgwr           ; set register X to 0
        INX
        CPX     #16
        BNE     clrpsg1
        LDA     #07
        LDY     #%00111000
        JSR     psgwr
        RTS

        .ELSE


; ENSURE ALL OF THESE SPECIAL COMMANDS GIVE ERRORS IN NORMAL BASIC
LAB_SOUND:
LAB_VOLUME:
LAB_VOICE:
LAB_NOISE:
LAB_TONE:
LAB_CON:
        LDX     #$02            ; error code $02 ("SYNTAX" error)
        JMP     LAB_XERR        ; do error #X, then warm start

        .ENDIF