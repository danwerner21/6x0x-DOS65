;FIXED PARAMETERS
DFLFCB          = $107          ;DEFAULT FCB
PEM             = $103          ;PEM ENTRY
BOOT            = $100          ;WARM BOOT
TEA             = $800          ;EXECUTION ORG
CCMLNG          = 2048          ;CCM LENGTH
;MAIN PROGRAM
        .SEGMENT "TEA"
        .ORG    $0800

        LDY     #$00
LOOP:
        LDA     START,Y
        STA     $0400,Y
        INY
        CPY     #$10
        BNE     LOOP
        JMP     $0400
START:
        JSR     $FD36           ; CALL S19 LOADER FROM BIOS
        JMP     BOOT
        .END
