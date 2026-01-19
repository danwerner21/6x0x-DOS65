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
;       TASK 1, ADDITIONAL RAM PAGED INTO C000-D000 (10,11)
;	TASKS 2-15 -- OPEN FOR OS/USER USE
;_______________________________________________________________
;  The 6502PC has a flexible hardware MMU. There are 64 programmable task contexts, each with 16 4K banks ($BXXX).
;  The MMU has several Registers.
;     $xFE0- Write only-Task Register, Sets which context is active, when the mmu is enabled (0-63)
;     $xFE1- Write only-Setup Register, Sets which context is being setup (exposed in the edit window) (0-63)
;     $xFE2- Write only-Enable Register 0=MMU Disabled, 1=MMU Enabled
;     $xFE4- Read Only- Active Task Register (only lower 6 bits)
;     $xFE6- Read Only- hit ISA TC Bit
;     $xFE7- Read Only- Current IO Page (only lower 4 bits)
;
;     $xfDx- read or write task edit window
;
;_______________________________________________________________
INITPAGES:
        LDA     #$00            ; ENSURE MMU IS DISABLED (SHOULD BE ALREADY, BUT . . . )
        STA     PC6502_MMU_ENA
        LDA     #$01
        STA     PC6502_MAP_SETUP; Fill TASK 1
        JSR     INITPAGE        ; FILL TASK 1 WITH A 1:1 MAP
        LDA     #$10            ; BUT, MAP Cxxx AND Dxxx TO RAM 10xxx AND 11xxx RATHER THAN RAM
        STA     PC6502_MAP_SPACE+$0C
        LDA     #$11
        STA     PC6502_MAP_SPACE+$0D
        LDA     #$00
        STA     PC6502_MAP_SETUP; Then do task 0
        JSR     INITPAGE        ; FILL TASK 0 WITH A 1:1 MAP
;  Why task 1 before task 0??   On some boards every write to the register also writes to task 0
;  so it is important to write task 0 last (or not use it)
        LDA     #$00
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 00
        LDA     #$01
        STA     PC6502_MMU_ENA  ; ENABLE MMU --- FEEEEEL THE POOOOWERRRR
        RTS

INITPAGE:
        LDX     #$00
:
        TXA
        STA     PC6502_MAP_SPACE,X; CREATE A 1:1 MAP OF BANK
        INX
        CPX     #$10
        BNE     :-
        RTS
PAGE_EXIT:
        PHA
        LDA     #$00
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 00
        PLA
        RTS
PAGE_ENTER:
        PHA
        LDA     #$01
        STA     PC6502_ACT_TASK ; SET ACTIVE TASK TO 01
        PLA
        RTS



;__SETPAGE________________________________________________________
;
; SETUP MMU FOR A SPECIFIC MAPPED PAGE
;
; A=BANK TO SWAP
; X=MAP PAGE (SPOT IN MEMORY MAP)
; Y=DESTINATION PAGE (SPOT IN PHYSICAL MEMORY)
;_______________________________________________________________
SETPAGE:
        STA     TEMPWORD
        LDA     #$00            ; DISABLE MMU
        STA     PC6502_MMU_ENA
        LDA     TEMPWORD
        STA     PC6502_MAP_SETUP; Fill TASK AREA
        TYA
        STA     PC6502_MAP_SPACE,X
        LDA     #$00            ; RE-SETUP TASK AREA 0, JUST IN CASE THE ABOVE WRITE CORRUPTED IT
        STA     PC6502_MAP_SETUP; Fill TASK AREA
        TXA
        STA     PC6502_MAP_SPACE,X
        LDA     #$01
        STA     PC6502_MMU_ENA  ; ENABLE MMU --- FEEEEEL THE POOOOWERRRR
        RTS




DO_FARCALL_ACTUAL:
        JSR     PAGE_ENTER
        JSR     FUNCTION_DISPATCHER
        JMP     PAGE_EXIT
