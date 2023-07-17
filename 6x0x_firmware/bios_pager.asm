;__BIOS PAGERS__________________________________________________________________________________________________________________
;
; 	Memory Page Management Functions
;
;________________________________________________________________________________________________________________________________
;

;__INITPAGES____________________________________________________
;
; SETUP MMU FOR BIOS PAGED MEMORY OPERATION
;
; SETUP:
; 	TASK 0, NORMAL OPERATION
;   TASK 1, ADDITIONAL ROM DRIVERS PAGED INTO C000-D000
;	TASKS 2-15 -- OPEN FOR OS/USER USE
;_______________________________________________________________
INITPAGES:
        LDA     #$00            ; ENSURE MMU IS DISABLED (SHOULD BE ALREADY, BUT . . . )
        STA     M6X0X_MMU_ENA
        LDA     #$01
        STA     M6X0X_MAP_SETUP ; Fill TASK 1
        JSR     INITPAGE        ; FILL TASK 1 WITH A 1:1 MAP
        LDA     #$8C            ; BUT, MAP Cxxx AND Dxxx TO ROM Cxxx AND Dxxx RATHER THAN RAM
        STA     M6X0X_MAP_SPACE+$0C
        LDA     #$8D
        STA     M6X0X_MAP_SPACE+$0D
        LDA     #$00
        STA     M6X0X_MAP_SETUP ; Then do task 0
        JSR     INITPAGE        ; FILL TASK 0 WITH A 1:1 MAP
;  Why task 1 before task 0??   On some boards every write to the register also writes to task 0
;  so it is important to write task 0 last (or not use it)
        LDA     #$00
        STA     M6X0X_ACT_TASK  ; SET ACTIVE TASK TO 00
        LDA     #$01
        STA     M6X0X_MMU_ENA   ; ENABLE MMU --- FEEEEEL THE POOOOWERRRR
        RTS

INITPAGE:
        LDX     #$00
:
        TXA
        STA     M6X0X_MAP_SPACE,X; CREATE A 1:1 MAP OF BANK
        INX
        CPX     #$10
        BNE     :-
        RTS
PAGE_EXIT:
        PHA
        LDA     #$00
        STA     M6X0X_ACT_TASK  ; SET ACTIVE TASK TO 00
        PLA
        RTS
PAGE_ENTER:
        PHA
        SEI
        LDA     #$01
        STA     M6X0X_ACT_TASK  ; SET ACTIVE TASK TO 00
        LDA     #$01
        STA     M6X0X_MMU_ENA   ; ENSURE MMU IS ENABLED --- FEEEEEL THE POOOOWERRRR
        PLA
        RTS


DO_FARCALL_ACTUAL:
        JSR     PAGE_ENTER
        JSR     FUNCTION_DISPATCHER
        JMP     PAGE_EXIT
