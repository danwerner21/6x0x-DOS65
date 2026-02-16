 .IFDEF  DUODYNE
.P816

        .MACRO  INDEX16         ; Set 16bit Index Registers
        REP     #$10            ; 16 bit Index registers
        .I16
        .ENDMACRO
        .MACRO  INDEX8          ; Set 8bit Index Registers
        SEP     #$10            ; 8 bit Index registers
        .I8
        .ENDMACRO

        .MACRO  ACCUMULATOR16   ; Set 16bit Index Registers
        REP     #$20            ; 16 bit Index registers
        .A16
        .ENDMACRO

        .MACRO  ACCUMULATOR8    ; Set 8bit Index Registers
        SEP     #$20            ; 8 bit Index registers
        .A8
        .ENDMACRO

        .MACRO  ACCUMULATORINDEX16; Set 16bit Index Registers
        REP     #$30            ; 16 bit Index registers
        .A16
        .I16
        .ENDMACRO

        .MACRO  ACCUMULATORINDEX8; Set 8bit Index Registers
        SEP     #$30            ; 8 bit Index registers
        .A8
        .I8
        .ENDMACRO


        .MACRO  LDAINDIRECTY PARM1
        PHB
        PHX
        LDX     #$01
        LDA     <PARM1,X
        CMP     #$00
        BNE     *+6
        LDX     #00
        PHX
        PLB
        PLX
        LDA     (<PARM1),Y      ;
        STA     <TMPFLG
        PLB
        LDA     <TMPFLG
        .ENDMACRO

        .MACRO  STAINDIRECTY PARM1
        PHB
        PHX
        PHA
        LDX     #$01
        LDA     <PARM1,X
        CMP     #$00
        BNE     *+6
        LDX     #00
        PHX
        PLB
        PLA
        PLX
        STA     (<PARM1),Y      ;
        PLB
        STA     <TMPFLG
        .ENDMACRO

        .MACRO  FETCHINDIRECTY PARM1
        PHB
        PHA
        PHX
        LDX     #$01
        LDA     <PARM1,X
        CMP     #$00
        BNE     *+6
        LDX     #00
        PHX
        PLB
        PLX
        LDA     (<PARM1),Y      ;
        STA     <TMPFLG
        PLA
        PLB
        .ENDMACRO

        .MACRO  CMPINDIRECTY  PARM1
        PHB
        PHA
        PHX
        LDX     #$01
        LDA     <PARM1,X
        CMP     #$00
        BNE     *+6
        LDX     #00
        PHX
        PLB
        PLX
        LDA     (<PARM1),Y      ;
        STA     <TMPFLG
        PLA
        PLB
        CMP     <TMPFLG         ;
        .ENDMACRO

        .MACRO  ADCINDIRECTY  PARM1
        PHB
        PHA
        PHX
        LDX     #$01
        LDA     <PARM1,X
        CMP     #$00
        BNE     *+6
        LDX     #00
        PHX
        PLB
        PLX
        LDA     (<PARM1),Y      ;
        STA     <TMPFLG
        PLA
        PLB
        CLC
        ADC     <TMPFLG         ;
        .ENDMACRO

        .MACRO  ORAINDIRECTY  PARM1
        PHB
        PHA
        PHX
        LDX     #$01
        LDA     <PARM1,X
        CMP     #$00
        BNE     *+6
        LDX     #00
        PHX
        PLB
        PLX
        LDA     (<PARM1),Y      ;
        STA     <TMPFLG
        PLA
        PLB
        CLC
        ORA     <TMPFLG         ;
        .ENDMACRO

        .MACRO  LBEQ  PARM1
        BNE     *+5
        JMP     PARM1
        .ENDMACRO

        .MACRO  LBNE PARM1
        BEQ     *+5
        JMP     PARM1
        .ENDMACRO

        .MACRO  LBCC  PARM1
        BCC     *+4
        BRA     *+5
        JMP     PARM1
        .ENDMACRO

        .MACRO  LBCS PARM1
        BCS     *+4
        BRA     *+5
        JMP     PARM1
        .ENDMACRO

        .ELSE

        .MACRO  LDAINDIRECTY PARM1
        LDA     (PARM1),Y       ;
        .ENDMACRO

        .MACRO  STAINDIRECTY PARM1
        STA     (PARM1),Y       ;
        .ENDMACRO

        .MACRO  FETCHINDIRECTY PARM1
        LDA     (PARM1),Y       ;
        .ENDMACRO

        .MACRO  CMPINDIRECTY  PARM1
        CMP     (<PARM1),Y      ;
        .ENDMACRO

        .MACRO  ADCINDIRECTY  PARM1
        ADC     (PARM1),Y       ;
        .ENDMACRO

        .MACRO  ORAINDIRECTY  PARM1
        ORA     (PARM1),Y       ;
        .ENDMACRO

        .MACRO  LBEQ  PARM1
        BEQ     PARM1
        .ENDMACRO

        .MACRO  LBNE  PARM1
        BNE     PARM1
        .ENDMACRO

        .MACRO  LBCC  PARM1
        BCC     PARM1
        .ENDMACRO

        .MACRO  LBCS PARM1
        BCS     PARM1
        .ENDMACRO

        .ENDIF