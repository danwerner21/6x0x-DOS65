

        .IFDEF  MEMORYMAPPEDSCREEN

PC6502_IO       = $E000
SID_BASE        = PC6502_IO+$100

;___SOUND__________________________________________________
;
; PLAY SOUND
;
;  SOUND VOICE, FREQ, DUTY, WAVEFORM, ATTACK, SUSTAIN, DECAY, RELEASE
;
;
;__________________________________________________________
LAB_SOUND:
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X (VOICE)
        TXA
        AND     #3
        ASL     A
        TAX
        LDA     SIDVOICELOOKUP,X
        STA     TEMPW
        LDA     SIDVOICELOOKUP+1,X
        STA     TEMPW+1
                                ; OK, TEMPW POINTS TO SID REGISTER TO MODIFY
        JSR     LAB_1C01        ; GET THE SECOND PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_EVNM        ; evaluate expression and check is numeric,
                                ; else do type mismatch
        JSR     LAB_F2FX        ; save integer part of FAC1 in temporary integer
        LDY     #0              ; SET FREQUENCY
        LDA     Itempl
        STA     (TEMPW),Y
        LDA     Itemph
        INX
        STA     (TEMPW),Y
        JSR     LAB_1C01        ; GET THE THIRD PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_EVNM        ; evaluate expression and check is numeric,
                                ; else do type mismatch
        JSR     LAB_F2FX        ; save integer part of FAC1 in temporary integer
        LDY     #2              ; SET DUTY CYCLE
        LDA     Itempl
        STA     (TEMPW),Y
        LDA     Itemph
        INY
        STA     (TEMPW),Y
        JSR     LAB_1C01        ; GET THE FOURTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE PARAMETER, RETURN IN X (WAVEFORM)
        TXA
        LDY     #4              ; SET WAVEFORM CYCLE
        STA     (TEMPW),Y
        JSR     LAB_1C01        ; GET THE FIFTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE PARAMETER, RETURN IN X (ATTACK)
        TXA
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        STA     PTEMPW
        JSR     LAB_1C01        ; GET THE FIFTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE PARAMETER, RETURN IN X (DECAY)
        TXA
        AND     #$0F
        ORA     PTEMPW
        LDY     #5              ; SET ATTACK/DECAY
        STA     (TEMPW),Y
        JSR     LAB_1C01        ; GET THE FIFTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE PARAMETER, RETURN IN X (SUSTAIN)
        TXA
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        STA     PTEMPW
        JSR     LAB_1C01        ; GET THE FIFTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE PARAMETER, RETURN IN X (RELEASE)
        TXA
        AND     #$0F
        ORA     PTEMPW
        LDY     #6              ; SET SUSTAIN/RELEASE
        STA     (TEMPW),Y
        RTS
SIDVOICELOOKUP:
        .WORD   SID_BASE,SID_BASE+7,SID_BASE+$12

;___FILTER_________________________________________________
;
; SET SID FILTERS
;
;  FILTER CUTOFF, RESONANCE, VOICE 1 ENABLE, VOICE 2 ENABLE, VOICE 3 ENABLE, EXTERNAL ENABLE, FILTER MODE, VOLUME
;
;
;__________________________________________________________
LAB_FILTER:
        JSR     LAB_EVNM        ; evaluate expression and check is numeric,
                                ; else do type mismatch
        JSR     LAB_F2FX        ; save integer part of FAC1 in temporary integer
                                ; SET CUTOFF
        LDA     Itempl
        STA     SID_BASE+$15
        LDA     Itemph
        STA     SID_BASE+$16
        JSR     LAB_1C01        ; GET THE SECOND PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE PARAMETER, RETURN IN X (RESONANCE)
        TXA
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        STA     PTEMPW
        JSR     LAB_1C01        ; GET THE THIRD PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE PARAMETER, RETURN IN X (VOICE 1 ENABLE)
        TXA
        AND     #1
        ORA     PTEMPW
        STA     PTEMPW
        JSR     LAB_1C01        ; GET THE FOURTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE PARAMETER, RETURN IN X (VOICE 2 ENABLE)
        TXA
        AND     #1
        LSR     A
        ORA     PTEMPW
        STA     PTEMPW
        JSR     LAB_1C01        ; GET THE FIFTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE PARAMETER, RETURN IN X (VOICE 3 ENABLE)
        TXA
        AND     #1
        LSR     A
        LSR     A
        ORA     PTEMPW
        STA     PTEMPW
        JSR     LAB_1C01        ; GET THE SIXTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE PARAMETER, RETURN IN X (EXTERNAL AUDIO ENABLE)
        TXA
        AND     #1
        LSR     A
        LSR     A
        LSR     A
        ORA     PTEMPW
        STA     SID_BASE+$17
        JSR     LAB_1C01        ; GET THE SEVENTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE PARAMETER, RETURN IN X (FILTER MODE)
        TXA
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        STA     PTEMPW
        JSR     LAB_1C01        ; GET THE EIGHTH PARAMETER (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE PARAMETER, RETURN IN X (VOLUME)
        TXA
        AND     #$0F
        ORA     PTEMPW
        STA     SID_BASE+$18
        RTS

        .ELSE


; ENSURE ALL OF THESE SPECIAL COMMANDS GIVE ERRORS IN NORMAL BASIC
LAB_SOUND:
LAB_FILTER:
        LDX     #$02            ; error code $02 ("SYNTAX" error)
        JMP     LAB_XERR        ; do error #X, then warm start

        .ENDIF
