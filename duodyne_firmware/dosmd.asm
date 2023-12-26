;__MD DRIVERS____________________________________________________________________________________________________________________
;
; 	Duodyne Memory disk drivers
;
;	Entry points:
;		MD_SHOW         - called during OS init
;		MD_READ_SECTOR  - read a sector from drive
;		MD_WRITE_SECTOR - write a sector to drive
;________________________________________________________________________________________________________________________________
;
; RAM BANK $1E is RAM area for Drivers
; RAM BANK $1D is operating bank for DOS/65 $8000-$FFFF
;
; ROM BANKS $00 and $0C-$0F are reserved for ROMWBW code
; ROM Drive starts in bank $22
; RAM Drive starts in bank $02
;
; "Magic" number at $01FFFF.  If $E5 do not format RAM Drive.

MD_MAGIC        = $01FFFF
MD_RAM_START    = $020000

;__MD_SHOW___________________________________________________________________________________________
;
;  Display info on MD devices
;____________________________________________________________________________________________________
;
MD_SHOW:
        PRTDBG  "MD INIT:$"
        PRTS    "MD: UNITS=2 RAMDISK=256KB ROMDISK=384KB$"
        JSR     NEWLINE
        LDA     f:MD_MAGIC
        CMP     #$E5
        BEQ     :++

        PRTS    "MD: FORMATTING RAM DRIVE$"
        ACCUMULATOR8
        INDEX16
        LDX     #$0000
        LDA     #$E5
:
        STA     f:MD_MAGIC,x
        INX
        CPX     #0
        BNE     :-
        ACCUMULATORINDEX8
:
        RTS

;*__MD_READ_SECTOR____________________________________________________________________________________
;*
;*  READ MD SECTOR INTO BUFFER
;*
;*____________________________________________________________________________________________________
MD_READ_SECTOR:
        PRTDBG  "MD Read Sector$"
        LDA     f:LDSKUNIT
        AND     #$01            ; only want drive cfg
        ASL     a               ; SHIFT 5
        ASL     a               ;
        ASL     a               ;
        ASL     a               ;
        ASL     a               ;
        TAX                     ; STASH BANK
        JSR     MD_CONVERT_SECTOR

        LDA     debsehd
        STA     TEMPWORD
        LDA     debsehd+1
        STA     TEMPWORD+1
        LDA     debsehd+2
        STA     TEMPWORD+2

; Then copy sector to buffer
        ACCUMULATOR8
        INDEX16
        LDX     #$0000
        LDY     #$0000
:
        LDA     [TEMPWORD],y
        STA     f:LHSTBUF,x
        INX
        INY
        CPX     #512
        BNE     :-
        ACCUMULATORINDEX8
        LDA     #$00
        RTS


;*__MD_WRITE_SECTOR___________________________________________________________________________________
;*
;*  WRITE MD SECTOR FROM BUFFER
;*
;*____________________________________________________________________________________________________
MD_WRITE_SECTOR:
        PRTDBG  "MD Write Sector$"
        LDA     f:LDSKUNIT
        AND     #$01            ; only want drive cfg
        CMP     #$00            ; NO WRITE FOR ROM
        BEQ     MD_WRITE_SECTOR_RAM
        LDA     #$FF
        RTS
MD_WRITE_SECTOR_RAM:
        LDX     #$00
        JSR     MD_CONVERT_SECTOR

        LDA     debsehd
        STA     TEMPWORD
        LDA     debsehd+1
        STA     TEMPWORD+1
        LDA     debsehd+2
        STA     TEMPWORD+2

; Then copy sector to buffer
        ACCUMULATOR8
        INDEX16
        LDX     #$0000
        LDY     #$0000
:
        LDA     f:LHSTBUF,x
        STA     [TEMPWORD],y
        INX
        INY
        CPX     #512
        BNE     :-
        ACCUMULATORINDEX8
        LDA     #$00
        RTS

;___MD_CONVERT_SECTOR___________________________________________________________________________________
;
; 	TRANSLATE SECTORS INTO MD FORMAT
;________________________________________________________________________________________________________
MD_CONVERT_SECTOR:
        PRTDBG  "CONVERT SECTOR$"
        PHA
        PHX
        LDA     #$00
        STA     debsehd
        LDA     seksec          ; LOAD SECTOR # (LOW BYTE)
        LSR     A               ; DIVIDE BY 4 (FOR BLOCKING, 512 byte "Sectors")
        LSR     A               ;
        AND     #$1F            ; Ensure we have no junk in number
        STA     debcyll         ; STORE IN SECTOR/HEAD
        LDA     sektrk          ; LOAD TRACK # (LOW BYTE)
        AND     #$03            ; BOTTOM 2 BITS ARE PART OF SECTOR
        ASL     a               ; MOVE TO HIGH BITS
        ASL     a
        ASL     a
        ASL     a
        ASL     a
        ORA     debcyll
        STA     debcyll         ; STORE IN SECTOR/HEAD
        LDA     sektrk          ; LOAD TRACK # (LOW BYTE)
        LSR     A               ; DIVIDE BY 4 TO GET BANK
        LSR     A
        AND     #$3F            ; Ensure we have no junk in number
        STA     debcylm         ;
        TXA                     ; Adjust for ROM/RAM DISK
        ORA     debcylm         ;
        INC     A
        INC     A
        STA     debcylm         ; THIS SHOULD BE BANK#
        PLX
        PLA
        RTS
