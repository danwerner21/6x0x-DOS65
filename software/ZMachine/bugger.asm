;        PAGE
;        SBTTL   "--- DEBUGGER: DOS/65 ---"

; --------------
; DOS/65 DEBUGGER
; --------------

; ENTRY: BREAKPOINT ID IN [A]

BLINE:
        .BYTE   "B:    OP:   PC:       B:   S:   V:"
BLINL           = *-BLINE

BUGLIN          = SCREEN+960

DOBUG:
        PHA                     ; SAVE BREAKPOINT ID
        LDA     SHFLAG
        AND     #%00000100      ; CTRL KEY PRESSED?
        BEQ     BUGIT           ; CONTINUE IF NOT
        PLA                     ; ELSE RESET STACK
        RTS                     ; AND EXIT

BUGIT:
        LDX     #39
DBG0:
        LDA     #SPACE
        STA     BUGLIN,X        ; CLEAR SCREEN LINE
        LDA     #0
        STA     COLRAM+960,X    ; BLACKEN COLOR RAM
        DEX
        BPL     DBG0

        LDX     #0
DBG1:
        LDA     BLINE,X         ; PRINT DEBUGGER TEXT
        STA     BUGLIN,X
        INX
        CPX     #BLINL
        BCC     DBG1

        LDX     #2              ; INIT "CURSOR"
        PLA
        JSR     HEX             ; SHOW BREAKPOINT

        LDA     OPCODE
        BMI     ITQ0
        LDA     #'2'
        BNE     SHOWOP

ITQ0:
        CMP     #$B0
        BCS     ITQ1
        LDA     #'1'
        BNE     SHOWOP

ITQ1:
        CMP     #$C0
        BCS     ITQ2
        LDA     #'0'
        BNE     SHOWOP

ITQ2:
        CMP     #$E0
        BCS     ITQ3
        LDA     #'E'
        BNE     SHOWOP

ITQ3:
        LDA     #'X'

SHOWOP:
        LDX     #5              ; SET CURSOR
        STA     BUGLIN,X

        LDX     #9              ; CURSOR FOR OP ID
        LDA     OPCODE
        JSR     HEX

        LDX     #15             ; CURSOR FOR PC
        LDA     ZPCH
        JSR     HEX
        LDA     ZPCM
        JSR     HEX
        LDA     ZPCL
        JSR     HEX

        LDX     #24             ; CURSOR FOR BYTE
        LDA     MBYTE
        JSR     HEX

        LDX     #29             ; CURSOR FOR [ZSP]
        LDA     BUFSIZ
        JSR     HEX

        LDX     #34             ; CURSOR FOR [MPC]
        LDA     MPCH
        JSR     HEX
        LDA     MPCM
        JSR     HEX
        LDA     MPCL
        JSR     HEX

WAITT:
        LDA     SHFLAG          ; LOGO KEY PRESSED?
        AND     #%00000010
        BEQ     WAITT
LETGO:
        LDA     SHFLAG          ; WAIT FOR RELEASE
        AND     #%00000010
        BNE     LETGO
        RTS

; CONVERT [A] TO HEX & PRINT

HEX:
        PHA
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        JSR     NIB
        PLA

NIB:
        AND     #%00001111
        TAY
        LDA     HCHARS,Y
        STA     BUGLIN,X
        INX
        RTS

HCHARS:
        .BYTE   "0123456789ABCDEF"

MBYTE:
        .BYTE   0
