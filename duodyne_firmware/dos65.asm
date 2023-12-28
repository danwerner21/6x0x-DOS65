.P816
;________________________________________________________________________________________________________________________________
;
;	DOS/65 for the Duodyne 65816
;
;  DWERNER 12/17/2023 	Initial
;________________________________________________________________________________________________________________________________
;       It is assumed that Bank 0 is not usable due to IO and ROM starting at DF00-FFFF
;       OS will run in Bank $1D
;       Drivers will run in Bank $1E
;________________________________________________________________________________________________________________________________
DOS65BANK       = $1D           ; Bank for DOS65
DOS65DRIVERSBNK = $1E           ; Bank for DOS65 Drivers
DOSSIZE         = OSEND-DOSBEGIN
DRIVERSIZE      = DRIVEREND-DRIVERBEGIN
        .INCLUDE "MACROS.ASM"
        .INCLUDE "DOSDEFN.ASM"  ; base addresses and definitions

        .SEGMENT "LOWCODE"
        .ORG    $0800
        CLD                     ; VERIFY DECIMAL MODE IS OFF
        CLC                     ;
        XCE                     ; SET NATIVE MODE

; begin by copying DOS/65 to Code Bank
        ACCUMULATORINDEX16
        LDX     #OSBEGIN
        LDY     #DOSBEGIN
        LDA     #DOSSIZE-1
        MVN     #00, #DOS65BANK

; Then copy Drivers into Driver Bank
        LDX     #OSBEGIN+DOSSIZE
        LDY     #DRIVERBEGIN
        LDA     #DRIVERSIZE-1
        MVN     #00, #DOS65DRIVERSBNK


; Set Default Console
        ACCUMULATORINDEX8
        LDA     #$04
        STA     CONSOLE
; Set Data Bank
        LDA     #DOS65BANK
        PHA
        PLB
        JML     $1DD002

OSBEGIN:
        .ORG    DOSBEGIN
        ACCUMULATORINDEX8
        .INCLUDE "../dos65_os/ccm215.asm"
        .INCLUDE "../dos65_os/pemrbc.asm"
        .INCLUDE "../dos65_os/simrbc.asm"

OSVECTAG:
        .RES    $F200-OSVECTAG
        JML     $1E8000
        RTS
OSEND:

        .ORG    $8000
DRIVERBEGIN:
        .INCLUDE "dos65drv.asm"
DRIVEREND:
        .END
