;__CLRDIR_______________________________________________________
;
; Clear Device Tracks
; USAGE:
;
; CLRDIR  D TTTTTT NN
; D     = Device (I)DE Primary (J)IDE Secondary or (S)D
; TTTTTT= Starting Track
; NN    = Number of Tracks
;______________________________________________________________
CLRDIR:

        LDA     #<INBUFFER      ; SETUP WORK BUFFER
        STA     WORKPTR         ;
        LDA     #>INBUFFER      ;
        STA     WORKPTR +1      ;

        LDA     #6
        JSR     INCWORKPTRX     ; JUMP OVER "CLRDIR"

        JSR     EATWHITESPACE   ; SKIP OVER THE WHITESPACE
        LDX     #$00            ;

        LDA     (WORKPTR,X)     ; GET NEXT BYTE FROM BUFFER
        CMP     #'I'            ; IDE Selected
        BNE     :+
        LDA     #$00
        STA     sekdsk
        STA     CURRENT_IDE_DRIVE
        JMP     clrdir_gettrk
:
        CMP     #'J'            ; IDE SECONDARY Selected
        BNE     :+
        LDA     #$00
        STA     sekdsk
        LDA     #$01
        STA     CURRENT_IDE_DRIVE
        JMP     clrdir_gettrk
:
        CMP     #'S'            ; SD  Selected
        BNE     :+
        LDA     #$01
        STA     sekdsk
        JMP     clrdir_gettrk
:
        JMP     clrdir_err1
clrdir_gettrk:
        JSR     INCWORKPTR      ;
        JSR     EATWHITESPACE   ; SKIP OVER THE WHITESPACE
        JSR     HEXIN
        BCS     clrdir_err
        ROL     A
        ROL     A
        ROL     A
        ROL     A
        STA     debcylm
        JSR     INCWORKPTR      ;
        JSR     HEXIN
        BCS     clrdir_err
        ORA     debcylm
        STA     debcylm
        JSR     INCWORKPTR      ;
        JSR     GETNUMBER
        BCS     clrdir_err
        LDA     TEMPWORD+1
        STA     debcyll
        LDA     TEMPWORD
        STA     debsehd
        JSR     EATWHITESPACE   ; SKIP OVER THE WHITESPACE
;       get number of tracks to process
        JSR     GETNUMBER
        BCS     clrdir_err
        LDA     TEMPWORD
        STA     BYTECT
        JMP     clrdir_CLRBUF
clrdir_err1:
        LDA     #<ERROR         ; LOAD LOW BYTE OF ERROR STRING
        STA     STRPTR          ; STORE IN POINTER LOW BYTE
        LDA     #>ERROR         ; LOAD HOGH BYTE OF ERROR STRING
        STA     STRPTR +1       ; STORE IN POINTER HIGH BYTE
        JMP     OUTSTR          ; OUTPUT THE STRING
clrdir_err:
        JMP     INVALID_NUMBER_ERROR
clrdir_CLRBUF:
;       clear buffer
        LDA     #$e5
        LDX     #$00
:
        STA     $200,X
        STA     $300,X
        INX
        CPX     #00
        BNE     :-

clrdir_Loopl:
        LDA     debcyll
        JSR     PRINT_BYTE
        LDA     #'.'
        JSR     IOF_OUTCH

clrdir_Loop:
        LDA     sekdsk
        CMP     #$00
        BEQ     :+
        JSR     PPP_WRITE_SECTOR
        JMP     :++
:
        JSR     IDE_WRITE_SECTOR
:
        INC     debsehd
        LDA     debsehd
        CMP     #$00
        BNE     clrdir_Loop
        INC     debcyll
        DEC     BYTECT
        LDA     BYTECT
        CMP     #$00
        BEQ     clrdir_EXIT
        JMP     clrdir_Loopl
clrdir_EXIT:
        LDA     #$0d
        JSR     IOF_OUTCH
        LDA     #$0a
        JSR     IOF_OUTCH
        RTS
