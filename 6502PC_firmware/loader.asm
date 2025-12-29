;________________________________________________________________________________________________________________________________
;
;	PC6502 DOS/65 loader program
;
;  DWERNER 12/20/2025 	Initial
;________________________________________________________________________________________________________________________________

;  setup banking
;  copy dos65 from $1000 to 00:$B800
;  copy drivers from $4000 to 01:$C000
;
;  Execute DOS
;
;
PC6502_ACT_TASK = $EFE0
PC6502_MMU_ENA  = $EFE2
POINTER         = $20
POINTER1        = $22
;
;
        .SEGMENT "TEA"
        .ORG    $0800
;
;
;
        LDA     #$00
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 00
        LDA     #$01
        STA     PC6502_MMU_ENA  ; ENABLE MMU --- FEEEEEL THE POOOOWERRRR
;
;       copy DOS/65 to $B800, task 00
;
        LDY     #$00
        STY     POINTER
        STY     POINTER1
        LDA     #$10
        STA     POINTER+1
        LDA     #$B8
        STA     POINTER1+1
LOOP1:
        LDA     (POINTER),Y
        STA     (POINTER1),Y
        INC     POINTER
        BNE     :+
        INC     POINTER+1
:
        INC     POINTER1
        BNE     :+
        INC     POINTER1+1
:
        LDA     POINTER1+1
        CMP     #$E0
        BNE     LOOP1
;       set task to 01
;
        LDY     #$00
        STY     POINTER
        STY     POINTER1
        LDA     #$40
        STA     POINTER+1
        LDA     #$C0
        STA     POINTER1+1
        LDA     #$01
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 01
;
;       copy DRIVERS to $C000, task 01
;
LOOP2:
        LDA     (POINTER),Y
        STA     (POINTER1),Y
        INC     POINTER
        BNE     :+
        INC     POINTER+1
:
        INC     POINTER1
        BNE     :+
        INC     POINTER1+1
:
        LDA     POINTER1+1
        CMP     #$E0
        BNE     LOOP2

        LDA     #$00
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 00

        JMP     $B800           ; BOOT IT UP
