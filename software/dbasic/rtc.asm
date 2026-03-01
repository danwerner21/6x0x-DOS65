

RTCSECONDS      = 0
RTCMINUTES      = 1
RTCHOUR         = 2
RTCDATE         = 3
RTCMONTH        = 4
RTCYEAR         = 6

;___LAB_SECOND______________________________________________
;
; RETURN SYSTEM SECONDS
;
;__________________________________________________________
LAB_SECOND:
        LDX     #RTCSECONDS
        LDA     #51
        STA     farfunct
        JSR     DO_FARCALL
        TYA

        JSR     BCD_TO_HEX
        TAY
        LDA     #0              ; Get high byte
        JSR     LAB_AYFC
        RTS
LAB_PSECOND:
        LSR     <Dtypef         ; clear data type flag, $FF=string, $00=numeric
        JSR     LAB_IGBY        ; increment and scan memory then do function
        RTS
;___LAB_MINUTE______________________________________________
;
; RETURN SYSTEM MINUTE
;
;__________________________________________________________
LAB_MINUTE:
        LDX     #RTCMINUTES
        LDA     #51
        STA     farfunct
        JSR     DO_FARCALL
        TYA
        JSR     BCD_TO_HEX
        TAY
        LDA     #0              ; Get high byte
        JSR     LAB_AYFC
        RTS
LAB_PMINUTE:
        LSR     <Dtypef         ; clear data type flag, $FF=string, $00=numeric
        JSR     LAB_IGBY        ; increment and scan memory then do function
        RTS
;___LAB_HOUR_________________________________________________
;
; RETURN SYSTEM HOUR
;
;__________________________________________________________
LAB_HOUR:
        LDX     #RTCHOUR
        LDA     #51
        STA     farfunct
        JSR     DO_FARCALL
        TYA
        JSR     BCD_TO_HEX
        TAY
        LDA     #0              ; Get high byte
        JSR     LAB_AYFC
        RTS
LAB_PHOUR:
        LSR     <Dtypef         ; clear data type flag, $FF=string, $00=numeric
        JSR     LAB_IGBY        ; increment and scan memory then do function
        RTS

;___LAB_DAY______________________________________________
;
; RETURN SYSTEM DAY OF MONTH
;
;__________________________________________________________
LAB_DAY:
        LDX     #RTCDATE
        LDA     #51
        STA     farfunct
        JSR     DO_FARCALL
        TYA
        JSR     BCD_TO_HEX
        TAY
        LDA     #0              ; Get high byte
        JSR     LAB_AYFC
        RTS
LAB_PDAY:
        LSR     <Dtypef         ; clear data type flag, $FF=string, $00=numeric
        JSR     LAB_IGBY        ; increment and scan memory then do function
        RTS

;___LAB_MONTH______________________________________________
;
; RETURN SYSTEM MONTH
;
;__________________________________________________________
LAB_MONTH:
        LDX     #RTCMONTH
        LDA     #51
        STA     farfunct
        JSR     DO_FARCALL
        TYA
        JSR     BCD_TO_HEX
        TAY
        LDA     #0              ; Get high byte
        JSR     LAB_AYFC
        RTS
LAB_PMONTH:
        LSR     <Dtypef         ; clear data type flag, $FF=string, $00=numeric
        JSR     LAB_IGBY        ; increment and scan memory then do function
        RTS

;___LAB_YEAR________________________________________________
;
; RETURN SYSTEM YEAR
;
;__________________________________________________________
LAB_YEAR:
        LDX     #RTCYEAR
        LDA     #51
        STA     farfunct
        JSR     DO_FARCALL
        TYA
        JSR     BCD_TO_HEX
        LDA     #0              ; Get high byte
        JSR     LAB_AYFC
        RTS
LAB_PYEAR:
        LSR     <Dtypef         ; clear data type flag, $FF=string, $00=numeric
        JSR     LAB_IGBY        ; increment and scan memory then do function
        RTS


BCD_TO_HEX:
        STA     nums_2          ; Store in TEMP2
        AND     #$F0
        LSR     A               ; (shift 1 times to /2)
        STA     nums_1          ; Store the /2 into TEMP
        LSR     A               ;
        LSR     A               ; Shift two more times to /8
        CLC
        ADC     nums_1          ; add /8 + /2 to get /10
        STA     nums_1          ; Store tens digit into TEMP
        LDA     nums_2          ; Get Ones
        AND     #$0F
        CLC
        ADC     nums_1
        RTS                     ; Return from subroutine
