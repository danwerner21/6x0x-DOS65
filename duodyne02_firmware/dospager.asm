;__pager_________________________________________________________________________________________________________________________
;
; 	Nhyodyne Memory page management code
;
;	Entry points:
;		PAGER_INIT          - called during OS init
;________________________________________________________________________________________________________________________________
;

;__PAGER_INIT___________________________________________________________________________________________
;
;  INIT -- Copy code into $0200-$02FF for controling banking
;____________________________________________________________________________________________________
PAGER_INIT:
        LDX     #$00
:
        LDA     md_pagecode,X
        STA     MD_PAGERA,X
        INX
        CPX     #$00
        BNE     :-
        RTS

md_pagecode:
        PHA
        LDA     #$85
        STA     BANK80
        LDA     #$86
        STA     BANKC0
        NOP
        NOP
        PLA
        JSR     BANKED_DRIVER_DISPATCHER
        PHA
        LDA     #$82
        STA     BANK80
        LDA     #$83
        STA     BANKC0

        PLA
        RTS
