.P816
;________________________________________________________________________________________________________________________________
;
;	DOS/65 for the Duodyne 65816
;
;  DWERNER 12/17/2023 	Initial
;________________________________________________________________________________________________________________________________
;       It is assumed that Bank 0 is not usable due to IO and ROM starting at DF00-FFFF
;       OS will run in Bank 1, in emulation mode
;       Drivers will run in Bank 2, in native mode
;________________________________________________________________________________________________________________________________
DOS65BANK       = $1D           ; Bank for DOS65
DOS65DRIVERSBNK = $1E           ; Bank for DOS65 Drivers
DOSSIZE         = OSEND-DOSBEGIN
        .INCLUDE "MACROS.ASM"
        .INCLUDE "DOSDEFN.ASM"  ; base addresses and definitions

        .SEGMENT "LOWCODE"
        .ORG    $0800
        CLD                     ; VERIFY DECIMAL MODE IS OFF
        CLC                     ;
        XCE                     ; SET NATIVE MODE



; begin by copying DOS/65 to Bank 1
        ACCUMULATORINDEX16
        LDX     #OSBEGIN        ; MVP starts with the last byte to copy
        LDY     #DOSBEGIN
        ;LDA     #DOSSIZE-1
       ; MVN     #00, #DOS65BANK

; use this until new boards are here
        stx     TEMPWORD
        sty     TEMPWORD1
        ldy     #0000
clp:
        lda     (TEMPWORD),Y
        tax
        lda     #DOS65BANK
        pha
        plb
        txa
        sta     (TEMPWORD1),Y
        lda     #$00
        pha
        plb
        iny
        cpy     #DOSSIZE
        bne     clp


        BRK
OSBEGIN:
        .ORG    DOSBEGIN
        ACCUMULATORINDEX8
        .INCLUDE "../dos65_os/ccm215.asm"
        .INCLUDE "../dos65_os/pemrbc.asm"
        .INCLUDE "../dos65_os/simrbc.asm"
OSEND:
        .END
