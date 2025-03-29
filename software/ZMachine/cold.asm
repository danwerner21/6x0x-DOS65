;        PAGE
;        SBTTL   "--- MACHINE COLDSTART: DOS/65 ---"


; ---------
; COLDSTART
; ---------
        .ORG    ZIP


COLD:
        LDA     #0
        STA     SFLAG           ; NO PREVIOUS SCRIPTING (BM 5/14/85)
        JMP     WARM1

; ---------------
; WARMSTART ENTRY
; ---------------

SLOAD:
        .BYTE   "Infocom ZMachine Intrepreter for DOS/65"
        .BYTE   EOL
        .BYTE   EOL
        .BYTE   "                 The story is loading ..."
        .BYTE   EOL
SLOADL          = *-SLOAD

WARM1:
        CLD
        .IFDEF  DUODYNE
        .ELSE
        LDX     #$FF            ; (Dont do this for Duodyne . . . . .)
        TXS                     ; RESET MACHINE STACK
        .ENDIF



        JSR     CLS             ; CLEAR SCREEN, ETC.

        LDY     #8              ; POSITION "STORY LOADING" MESSAGE
        LDX     #11             ; AT (8,11)
        CLC
        JSR     PLOT

        LDX     #<SLOAD
        LDA     #>SLOAD
        LDY     #SLOADL
        JSR     DLINE           ; "THE STORY IS LOADING ..."
        JSR     DOPEN           ; AND OPEN THE STORY


; FALL THROUGH TO ZIP WARMSTART AT "WARM2"
