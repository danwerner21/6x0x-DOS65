.PC02
;________________________________________________________________________________________________________________________________
;
;	DOS/65 for the Duodyne 6502
;
;  DWERNER 5/03/2025 	Initial
;________________________________________________________________________________________________________________________________
;       It is assumed that Bank 0 is not usable due to IO and ROM starting at DF00-FFFF
;       OS will run in Bank $82-$85
;       Drivers will run in Bank $86-$87
;________________________________________________________________________________________________________________________________

DOSSIZE         = OSEND-DOSBEGIN
DRIVERSIZE      = DRIVEREND-DRIVERBEGIN
LOADERSIZE      = LOADEREND-LOADERBEGIN

OSSTART         = LOADEREND
DRIVERSTART     = LOADEREND+DOSSIZE

        .INCLUDE "DOSDEFN.ASM"  ; base addresses and definitions
        .SEGMENT "LOWCODE"

LOADERBEGIN:
        CLD                     ; VERIFY DECIMAL MODE IS OFF
        CLC                     ;

; RELOCATE FROM CPM RAM
        LDA     #$00
        STA     TEMPWORD
        LDA     #$84
        STA     TEMPWORD+1

        LDA     #00
        STA     TEMPWORD1
        LDA     #$04
        STA     TEMPWORD1+1
        LDY     #$00
LOOP:
        LDA     (TEMPWORD),Y    ;MOVE IT
        STA     (TEMPWORD1),Y   ;MOVE IT
        INY
        BNE     LOOP

        INC     TEMPWORD+1
        INC     TEMPWORD1+1
        LDA     TEMPWORD1+1
        CMP     #$7F
        BNE     LOOP

        JMP     RELOC1
RELOC1:

;* Setup Memory Banks  (page out ROM)
        LDA     #$83
        STA     BANKC0

; begin by copying DOS/65 to Code Bank
        LDA     #<OSSTART
        STA     TEMPWORD
        LDA     #>OSSTART
        STA     TEMPWORD+1

        LDA     #00
        STA     TEMPWORD1
        LDA     #$D0
        STA     TEMPWORD1+1
        LDY     #$00

LOOP1:
        LDA     (TEMPWORD),Y    ;MOVE IT
        STA     (TEMPWORD1),Y   ;MOVE IT
        INY
        BNE     LOOP1

        INC     TEMPWORD+1
        INC     TEMPWORD1+1
        LDA     TEMPWORD1+1
        CMP     #$FF
        BNE     LOOP1

; copy Drivers to proper bank
        LDA     #$85
        STA     BANK80
        LDA     #$86
        STA     BANKC0


        LDA     #<DRIVERSTART
        STA     TEMPWORD
        LDA     #>DRIVERSTART
        STA     TEMPWORD+1

        LDA     #00
        STA     TEMPWORD1
        LDA     #$88
        STA     TEMPWORD1+1

        LDY     #$00
LOOP2:
        LDA     (TEMPWORD),Y    ;MOVE IT
        STA     (TEMPWORD1),Y   ;MOVE IT
        INY
        BNE     LOOP2

        INC     TEMPWORD+1
        INC     TEMPWORD1+1
        LDA     TEMPWORD+1
        CMP     #$80
        BNE     LOOP2



;* Setup Memory Banks  (page out ROM)
        LDA     #$80
        STA     BANK00
        LDA     #$81
        STA     BANK40
        LDA     #$82
        STA     BANK80
        LDA     #$83
        STA     BANKC0
; Boot
        LDA     #'*'
        STA     UART0
        LDA     #04
        STA     CONSOLE

        JSR     PAGER_INIT
        JMP     $D000

        .INCLUDE "dospager.asm"

;       Page Align
        .ALIGN  256
LOADEREND:


OSBEGIN:
        .SEGMENT "OS"
        .INCLUDE "../dos65_os/ccm215.asm"
        .INCLUDE "../dos65_os/pemrbc.asm"
        .INCLUDE "../dos65_os/simrbc.asm"

;       Page Align
        .ALIGN  256
OSEND:


DRIVERBEGIN:
        .INCLUDE "dos65drv.asm"
DRIVEREND:
        .END
