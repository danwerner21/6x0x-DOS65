PEMVEC:
        .WORD   $0000

DBGX:
        .BYTE   0
DBGY:
        .BYTE   0
DBGA:
        .BYTE   0

PEM:
        .IFDEF  DUODYNE
        JMP     $0103
        .ELSE
        JMP     (PEMVEC)
        .ENDIF



;___KILL____________________________________________________
;
; KILL COMMAND
;
;  RETURN TO DOS/65
;
;__________________________________________________________
LAB_KILL:
RETURN_TO_OS:
        JMP     $0100


;___SAVE____________________________________________________
;
; SAVE COMMAND
;
;  SAVE THE BASIC PROGRAM IN MEMORY TO DISK
;
;__________________________________________________________
V_SAVE: ; save BASIC program
DOS65SAVE:
        JSR     DOS65FCBPREP    ; parse parameters into FCB
        BCS     DOS65SAVE_ERR1  ; Error?, if so abort
        LDX     #13             ;
        JSR     PEM             ;
        LDA     #<FCB           ; CREATE File
        LDY     #>FCB           ;
        LDX     #22             ;
        JSR     PEM             ;
        LDA     #0              ; clear
        STA     FCB+32          ; record number
        LDA     #<FCB           ; Open File
        LDY     #>FCB           ;
        LDX     #15             ;
        JSR     PEM             ;
        CMP     #$FF            ; error?, if NOT, continue
        BNE     DOS65SAVE_1     ;
DOS65SAVE_ERR:
        LDX     #$24            ; IO ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
DOS65SAVE_ERR1:
        JMP     LAB_REM         ; comment out the remainder of the line (if any)
DOS65SAVE_1:
        LDA     #<FCBBUFFER     ; SETUP BUFFER
        STA     FCBPTR          ; STORE DEST BUFFER IN FCBPTR
        LDY     #>FCBBUFFER     ;
        STY     FCBPTR+1        ;
        LDX     #26             ; Setup Buffer
        JSR     PEM             ;
DOS65SAVE_2:
        LDA     <Smeml          ; All is well, file opened and continue
        STA     FCBPTR+2        ; point to base of RAM
        LDA     <Smemh          ;
        STA     FCBPTR+3        ;
        LDX     #$00            ; x=0 (null counter)
DOS65SAVE_2A:
        LDY     #$00            ; y=0 (Loop Index)
DOS65SAVE_3:
        LDA     (FCBPTR+2),Y    ; load from RAM
        STA     (FCBPTR),Y      ; save to Buffer
        CMP     #$00            ; is Zero?
        BNE     DOS65SAVE_3A    ; No, continue
        INX                     ; yes, bump Null Counter
        CPX     #$03            ; Three nulls in a row?
        BNE     DOS65SAVE_3B    ; No, continue
                                ; FINISH FILLING BUFFER WITH NULLS
:
        STA     (FCBPTR),Y      ; save to Buffer
        INY                     ; bump index
        CPY     #$80            ; end of buffer?
        BNE     :-              ; no, loop
        LDA     #<FCB           ; Write record in buffer to disk file AND END
        LDY     #>FCB           ;
        LDX     #21             ;
        JSR     PEM             ;
        AND     #$FE            ; strip out extension
        CMP     #$00            ; is error
        BNE     DOS65SAVE_ERR   ; ERROR, do error handling
        JMP     DOS65SAVE_4
DOS65SAVE_3A:                   ;
        LDX     #$00            ; prior char not null, reset null counter
DOS65SAVE_3B:                   ;
        INY                     ; bump index
        CPY     #$80            ; end of buffer?
        BNE     DOS65SAVE_3     ; no, loop
DOS65SAVE_3C:
        LDA     #<FCB           ; Write record in buffer to disk file
        LDY     #>FCB           ;
        LDX     #21             ;
        JSR     PEM             ;
        AND     #$FE            ; strip out extension
        CMP     #$00            ; is error
        BNE     DOS65SAVE_ERR   ; ERROR, do error handling
        CLC                     ;
        LDA     FCBPTR+2        ; add $80 to base counter
        ADC     #$80            ;
        STA     FCBPTR+2        ;
        BCC     DOS65SAVE_2A    ; is carry?, no loop
        CLC                     ; yes, carry, inc high byte
        LDA     FCBPTR+3        ;
        ADC     #$01            ;
        STA     FCBPTR+3        ;
        CMP     #>Ram_top       ; are we at the end of RAM?
        BEQ     DOS65SAVE_4
        JMP     DOS65SAVE_2A
DOS65SAVE_4:
        LDA     #<FCB           ;
        LDY     #>FCB           ;
        LDX     #16             ;
        JSR     PEM             ;
        CMP     #$FF            ;
        BNE     DOS65SAVE_5     ;
        JMP     DOS65SAVE_ERR   ; ERROR, DISPLAY IT
DOS65SAVE_5:
        LDA     #<LAB_RMSG      ; point to "Ready" message low byte
        LDY     #>LAB_RMSG      ; point to "Ready" message high byte
        JSR     LAB_18C3
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC

;___LOAD____________________________________________________
;
; LOAD COMMAND
;
;  LOAD A BASIC PROGRAM FROM DISK TO MEMORY
;
;__________________________________________________________
V_LOAD: ; load BASIC program
DOS65LOAD:
        LDA     #$00
        STA     NullCounter
        JSR     DOS65FCBPREP    ; parse parameters into FCB
        BCS     DOS65LOAD_ERR1  ; Error?, if so abort
        LDX     #13             ;
        JSR     PEM             ;
        LDA     #0              ; clear
        STA     FCB+32          ; record number
        LDA     #<FCB           ; Open File
        LDY     #>FCB           ;
        LDX     #15             ;
        JSR     PEM             ;
        CMP     #$FF            ; error?, if NOT, continue
        BNE     DOS65LOAD_1     ;
DOS65LOAD_ERR:
        LDX     #$24            ; IO ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
DOS65LOAD_ERR1:
        JMP     LAB_REM         ; comment out the remainder of the line (if any)
DOS65LOAD_1:
        LDA     <Smeml          ; All is well, file opened and continue
        STA     FCBPTR+2        ; point to base of RAM
        STA     EndOfBas        ;
        LDA     <Smemh          ;
        STA     FCBPTR+3        ;
        STA     EndOfBas+1      ;
        LDA     #<FCBBUFFER     ; SETUP BUFFER
        STA     FCBPTR          ; STORE DEST BUFFER IN FCBPTR
        LDY     #>FCBBUFFER     ;
        STY     FCBPTR+1        ; STORE DEST BUFFER IN FCBPTR
        LDX     #26             ; Setup Buffer
        JSR     PEM             ;
        JMP     DOS65LOAD_3C
DOS65LOAD_3:
        LDA     (FCBPTR),Y      ; load from BUFFER
        STA     (FCBPTR+2),Y    ; save to RAM
        INC     EndOfBas
        BNE     :+
        INC     EndOfBas+1
:
        CMP     #$00
        BNE     :+
        INC     NullCounter
        LDA     NullCounter
        CMP     #3
        BNE     :++
        JMP     DOS65LOAD_4
:
        LDA     #$00
        STA     NullCounter
:
        INY                     ; bump index
        CPY     #$80            ; end of buffer?
        BNE     DOS65LOAD_3     ; no, loop
        CLC                     ;
        LDA     FCBPTR+2        ; add $80 to base counter
        ADC     #$80            ;
        STA     FCBPTR+2        ;
        BCC     DOS65LOAD_3C    ; is carry?, no SKIP
        CLC                     ; yes, carry, inc high byte
        LDA     FCBPTR+3        ;
        ADC     #$01            ;
        STA     FCBPTR+3        ;
        CMP     #>Ram_top       ; are we at the end of RAM?
        BEQ     DOS65LOAD_4

DOS65LOAD_3C:
        LDA     #<FCB           ; READ record buffer from disk file
        LDY     #>FCB           ;
        LDX     #20             ;
        JSR     PEM             ;
        LDY     #$00            ; RESET INDEX
        CMP     #$00            ; is error or EOF
        BEQ     DOS65LOAD_3     ;
        CMP     #$01            ; EOF?
        BNE     DOS65LOAD_ERR   ; ERROR, do error handling
DOS65LOAD_4:
        LDA     #<FCB           ;
        LDY     #>FCB           ;
        LDX     #16             ;
        JSR     PEM             ;
                                ; setup pointers to return to basic
        LDA     EndOfBas
        STA     Svarl
        STA     Sarryl
        STA     Earryl
        LDA     EndOfBas+1
        STA     Svarh
        STA     Sarryh
        STA     Earryh
        LDA     #<LAB_RMSG      ; point to "Ready" message low byte
        LDY     #>LAB_RMSG      ; point to "Ready" message high byte
        JSR     LAB_18C3
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC



DOS65FCBPREP:
        .IFDEF  DUODYNE
        JSL     LAB_GBYT        ; scan memory
        .ELSE
        JSR     LAB_GBYT        ; scan memory
        .ENDIF

        SEC                     ;
        SBC     #64             ; PARSE DRIVE NUMBER
        STA     DOSDRIVE        ; STORE IT IN FCB
        .IFDEF  DUODYNE
        JSL     LAB_IGBY        ; increment and scan memory
        .ELSE
        JSR     LAB_IGBY        ; increment and scan memory
        .ENDIF

        CMP     #':'            ;
        BEQ     DOS65FCBPREP_1  ; YES, IT WAS A DRIVE, CONTINUE
        LDX     #$26            ; IO ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC

DOS65FCBPREP_1:
        LDA     #<FCB           ; SETUP FCBPTR
        STA     FCBPTR          ;
        LDA     #>FCB           ;
        STA     FCBPTR+1        ;
        LDY     #$01            ; POINT Y TO FCB FILE NAME
        LDA     #$20            ; LOAD SPACE CHAR INTO A
DOS65FCBPREP_1A:                ; BLANK OUT FCB
        STAINDIRECTY FCBPTR     ;
        INY                     ;
        CPY     #$09            ; IS DONE
        BNE     DOS65FCBPREP_1A ; NO, LOOP
        LDY     #$01            ; POINT Y TO FCB FILE NAME
DOS65FCBPREP_2:                 ; COPY FILE NAME PARAMETER INTO FCB
        .IFDEF  DUODYNE
        JSL     LAB_IGBY        ; increment and scan memory
        .ELSE
        JSR     LAB_IGBY        ; increment and scan memory
        .ENDIF
        BEQ     DOS65FCBPREP_3  ;
        CMP     #'.'
        BEQ     DOS65FCBPREP_3  ;
        STAINDIRECTY FCBPTR     ;
        INY                     ;
        CPY     #$09            ;
        BEQ     DOS65FCBPREP_3  ;
        JMP     DOS65FCBPREP_2  ;
DOS65FCBPREP_3:                 ;
        CLC
        RTS                     ;

EndOfBas:
        .BYTE   00,00
NullCounter:
        .BYTE   00




;___DIR_____________________________________________________
;
; DIR COMMAND
;
;  DISPLAY A DISK DIRECTORY
;
;__________________________________________________________

LAB_DIR:
        LDA     #$00
        STA     DIRET
        STA     DIRET+1
        STA     DIRET+2

        LDA     #<FCBBUFFER     ; SETUP BUFFER
        LDY     #>FCBBUFFER     ;
        LDX     #26             ; Setup Buffer
        JSR     PEM             ;

        .IFDEF  DUODYNE
        JSL     LAB_GBYT        ; scan memory
        .ELSE
        JSR     LAB_GBYT        ; scan memory
        .ENDIF

        SEC                     ;
        SBC     #64             ; PARSE DRIVE NUMBER
        STA     DIRDRIVE        ; STORE IT IN FCB
        .IFDEF  DUODYNE
        JSL     LAB_IGBY        ; increment and scan memory
        .ELSE
        JSR     LAB_IGBY        ; increment and scan memory
        .ENDIF

        CMP     #':'            ;
        BEQ     :+              ; YES, IT WAS A DRIVE, CONTINUE
        LDX     #$26            ; IO ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
:
        LDA     #<DIRFCB        ; GET FIRST DIR ENTRY
        LDY     #>DIRFCB        ;
        LDX     #17             ;
        JSR     PEM             ;

DIR_LOOP:
        CMP     #$FF
        BEQ     DIR_EXIT

; PRINT DIR ENTRY
        ASL     A               ; *2
        ASL     A               ; *4
        ASL     A               ; *8
        ASL     A               ; *16
        ASL     A               ; *32
        STA     Itempl
        TAX
        LDY     #0
DIR_PRINT:
                                ; PRINT FILENAME
        LDA     FCBBUFFER,X
        JSR     V_OUTP
        INX
        INY
        CPY     #9
        BEQ     DIR_SPACE
        CPY     #12
        BNE     DIR_PRINT
                                ; PRINT SPACE
        LDA     #$0D
        JSR     V_OUTP
        LDA     #$0A
        JSR     V_OUTP

        LDA     #<DIRFCB        ; GET NEXT DIR ENTRY
        LDY     #>DIRFCB        ;
        LDX     #18             ;
        JSR     PEM             ;
        JMP     DIR_LOOP
DIR_SPACE:
        LDA     #' '
        JSR     V_OUTP
        JSR     V_OUTP
        JMP     DIR_PRINT
DIR_EXIT:

        LDA     #<LAB_RMSG      ; point to "Ready" message low byte
        LDY     #>LAB_RMSG      ; point to "Ready" message high byte
        JSR     LAB_18C3
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC


;___OPEN____________________________________________________
;
; OPEN A FILE
;
;
; BASIC COMMAND EXPECTS FOUR VARS

; DRIVE,MODE, FILENAME$,FILE$
;
; FILE$ = STRING TO HOLD 33 BYTE FCB
; DRIVE = NUMERIC DRIVE -- 0=CURRENT, 1=A, 2=B . . .
; MODE = 0=EXISTING, 1=CREATE
; FILENAME$ = FILENAME STRING
;__________________________________________________________
V_OPEN:
; CLEAR FCB
        LDA     #0              ; FILL WITH NULL
        LDY     #0              ;
:
        STA     FCB,Y           ;
        INY                     ;
        CPY     #33             ;
        BNE     :-              ;
                                ;
        LDA     #32             ; FILL FILENAME AND EXTENSION WITH SPACES
        LDY     #1              ;
:
        STA     FCB,Y           ;
        INY                     ;
        CPY     #12             ;
        BNE     :-              ;
                                ;
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X (DRIVE)
        TXA
        STA     FCB             ; STORE DRIVE IN FCB
                                ;
        JSR     LAB_1C01        ; (AFTER ',') OR SYN ERR
        JSR     LAB_GTBY        ; GET THE SECOND PARAMETER, RETURN IN X (MODE)
                                ;
        CPX     #0
        BNE     :+
        LDA     #15
        STA     OPNTMP
        JMP     V_OPEN1
:
        LDA     #22
        STA     OPNTMP          ;
V_OPEN1:
        JSR     LAB_1C01        ; (AFTER ',') OR SYN ERR
        JSR     LAB_EVEX        ; GET THE THIRD PARAMETER
        LDA     <Dtypef         ; IS IT A STRING?
        BNE     :+              ; YES, CONTINUE ON
        LDX     #$02            ; NOPE, SYNTAX ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
:
        JSR     LAB_22B6        ; pop string off descriptor stack, or from top of string
                                ; space returns with A = length, X=$71=pointer low byte,
                                ; Y=$72=pointer high byte
        STA     str_ln          ; set string length
        STX     str_pl          ; set string pointer low byte
        STY     str_ph          ; set string pointer high byte
                                ; STORE IN FILENAME AND EXTENSION
        LDX     #0
        LDY     #0
:
        LDA     (str_pl),Y
        CMP     #'.'
        BEQ     V_OPEN_EXTENSION
        CPY     str_ln
        BEQ     V_OPEN_FNEND
        INY
        STA     FCB,Y
        CPY     #8
        BNE     :-
        DEY
:
        INY
        LDA     (str_pl),Y
        CMP     #'.'
        BEQ     V_OPEN_EXTENSION
        CPY     str_ln
        BEQ     V_OPEN_FNEND
        JMP     :-
        JMP     V_OPEN_FNEND
V_OPEN_EXTENSION:
        LDX     #9
:
        INY
        LDA     (str_pl),Y
        CPY     str_ln
        BEQ     V_OPEN_FNEND
        STY     Rbyte3
        STX     Rbyte2
        LDY     Rbyte2
        STA     FCB,Y
        INC     Rbyte2
        LDY     Rbyte3
        INX
        CPX     #12
        BNE     :-
V_OPEN_FNEND:
        LDX     #13             ; INITIALIZE SYSTEM
        JSR     PEM             ;
        LDX     OPNTMP          ; OPEN OR CREATE
        LDA     #<FCB           ; FCB
        LDY     #>FCB           ; FCB
        JSR     PEM
        CMP     #$FF
        BNE     :+
        LDX     #$24            ; IO ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
:
        JSR     LAB_1C01        ; (AFTER ',') OR SYN ERR
        JSR     LAB_GVAR        ; GET THE FOURTH PARAMETER
        STA     Lvarpl          ; save var address low byte
        STY     Lvarph          ; save var address high byte
        LDA     <Dtypef         ; IS IT A STRING?
        BNE     :+              ; YES, CONTINUE ON
        LDX     #$02            ; NOPE, SYNTAX ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
:
        LDA     #34
        JSR     LAB_MSSP        ; make string space A bytes long A=$AC=length,
                                ; X=$AD=<Sutill=ptr low byte, Y=$AE=<Sutilh=ptr high byte
                                ; COPY FCB TO STRING
        LDX     #33             ;
        LDY     #33             ;
:
        DEX
        DEY
        LDA     FCB,X
        STA     (str_pl),Y      ;
        CPX     #0              ;
        BNE     :-              ;
                                ;
        JSR     LAB_RTST        ; STORE STRING
        JMP     LAB_17D5        ; do string LET and return

OPNTMP:
        .BYTE   00

;___CLOSE____________________________________________________
;
; CLOSE A FILE
;
; BASIC COMMAND EXPECTS ONE VAR
;
; FILE$
;
; FILE$ = STRING HOLDING 33 BYTE FCB
;__________________________________________________________
V_CLOSE:
        JSR     LAB_GVAR        ; GET THE FIRST PARAMETER
        JSR     LAB_CTST        ; check if source is string, else do type mismatch
        LDY     #$02            ; index to string pointer high byte
        LDAINDIRECTY Cvaral     ; get string pointer high byte
        STA     TEMPW+1         ; set string pointer high byte
        DEY                     ; index to string pointer low byte
        LDAINDIRECTY Cvaral     ; get string pointer low byte
        STA     TEMPW           ; set string pointer low byte
        LDX     #16             ; CLOSE
        LDA     TEMPW           ; FCB
        LDY     TEMPW+1         ; FCB
        JSR     PEM
        CMP     #$FF
        BNE     :+
        LDX     #$24            ; IO ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
:
        RTS

;___FILEWRITE_______________________________________________
;
; WRITE A 128 BYTE BLOCK TO A FILE
;
; BASIC COMMAND EXPECTS TWO VARS
;
; FILE$,DATA$
;
; FILE$ = STRING HOLDING 33 BYTE FCB
; DATA$ = STRING HOLDING 128 BYTE BLOCK
;__________________________________________________________
V_FILEWRITE:
        JSR     LAB_GVAR        ; GET THE FIRST PARAMETER
        JSR     LAB_CTST        ; check if source is string, else do type mismatch
        LDY     #$02            ; index to string pointer high byte
        LDAINDIRECTY Cvaral     ; get string pointer high byte
        STA     TEMPW+1         ; set string pointer high byte
        DEY                     ; index to string pointer low byte
        LDAINDIRECTY Cvaral     ; get string pointer low byte
        STA     TEMPW           ; set string pointer low byte
        JSR     LAB_1C01        ; (AFTER ',') OR SYN ERR
        JSR     LAB_EVEX        ; GET THE SECOND PARAMETER
        LDA     <Dtypef         ; IS IT A STRING?
        BNE     :+              ; YES, CONTINUE ON
        LDX     #$02            ; NOPE, SYNTAX ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
:
        LDY     #00
        LDA     (des_pl),Y
        STA     str_ln          ; set string length
        INY
        LDA     (des_pl),Y
        STA     str_pl          ; set string pointer low byte
        INY
        LDA     (des_pl),Y
        STA     str_ph          ; set string pointer high byte

        LDX     #$00
        LDA     #$00
:
        STA     FCBBUFFER,X
        INX
        CPX     #$80
        BNE     :-
        LDY     #0
:
        LDA     (str_pl),Y
        STA     FCBBUFFER,Y
        INY
        CPY     #$80
        BEQ     :+
        CPY     str_ln
        BNE     :-
:
        LDX     #26             ; SET BUFFER
        LDA     #<FCBBUFFER     ; BUFFER
        LDY     #>FCBBUFFER     ;
        JSR     PEM
        LDX     #21             ; WRITE RECORD
        LDA     TEMPW           ; FCB
        LDY     TEMPW+1         ; FCB
        JSR     PEM
        CMP     #$FF
        BNE     :+
        LDX     #$24            ; IO ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
:
        RTS


;___FILEREAD_______________________________________________
;
; READ A 128 BYTE BLOCK FROM A FILE
;
; BASIC COMMAND EXPECTS TWO VARS
;
; FILE$,DATA$
;
; FILE$ = STRING HOLDING 33 BYTE FCB
; DATA$ = STRING HOLDING 128 BYTE BLOCK
;__________________________________________________________
V_FILEREAD:
        JSR     LAB_GVAR        ; GET THE FIRST PARAMETER
        JSR     LAB_CTST        ; check if source is string, else do type mismatch
        LDY     #$02            ; index to string pointer high byte
        LDAINDIRECTY Cvaral     ; get string pointer high byte
        STA     TEMPW+1         ; set string pointer high byte
        DEY                     ; index to string pointer low byte
        LDAINDIRECTY Cvaral     ; get string pointer low byte
        STA     TEMPW           ; set string pointer low byte
                                ;
        LDX     #26             ; SET BUFFER
        LDA     #<FCBBUFFER     ; BUFFER
        LDY     #>FCBBUFFER     ;
        JSR     PEM
        LDX     #20             ; READ RECORD
        LDA     TEMPW           ; FCB
        LDY     TEMPW+1         ; FCB
        JSR     PEM
        CMP     #$FF
        BNE     :+
        LDX     #$24            ; IO ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
:
        JSR     LAB_1C01        ; (AFTER ',') OR SYN ERR
        JSR     LAB_GVAR        ; GET THE SECOND PARAMETER
        STA     Lvarpl          ; save var address low byte
        STY     Lvarph          ; save var address high byte
        LDA     <Dtypef         ; IS IT A STRING?
        BNE     :+              ; YES, CONTINUE ON
        LDX     #$02            ; NOPE, SYNTAX ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
:
        LDA     #129            ;
        JSR     LAB_MSSP        ; make string space A bytes long A=$AC=length,
                                ; X=$AD=<Sutill=ptr low byte, Y=$AE=<Sutilh=ptr high byte
                                ; COPY FCB TO STRING
        LDX     #128            ;
        LDY     #128            ;
:
        DEX
        DEY
        LDA     FCBBUFFER,X
        STA     (str_pl),Y      ;
        CPX     #0              ;
        BNE     :-              ;
                                ;
        JSR     LAB_RTST        ; STORE STRING
        JMP     LAB_17D5        ; do string LET and return

;___DELETE_________________________________________________
;
; DELETE A FILE
;
;
; BASIC COMMAND EXPECTS TWO VARS
; DRIVE, FILENAME$
;
; DRIVE = NUMERIC DRIVE -- 0=CURRENT, 1=A, 2=B . . .
; FILENAME$ = FILENAME STRING
;__________________________________________________________
V_DELETE:
; CLEAR FCB
        LDA     #0              ; FILL WITH NULL
        LDY     #0              ;
:
        STA     FCB,Y           ;
        INY                     ;
        CPY     #33             ;
        BNE     :-              ;
                                ;
        LDA     #32             ; FILL FILENAME AND EXTENSION WITH SPACES
        LDY     #1              ;
:
        STA     FCB,Y           ;
        INY                     ;
        CPY     #12             ;
        BNE     :-              ;
                                ;
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X (DRIVE)
        TXA
        STA     FCB             ; STORE DRIVE IN FCB
                                ;
        JSR     LAB_1C01        ; (AFTER ',') OR SYN ERR
        JSR     LAB_EVEX        ; GET THE SECOND PARAMETER
        LDA     <Dtypef         ; IS IT A STRING?
        BNE     :+              ; YES, CONTINUE ON
        LDX     #$02            ; NOPE, SYNTAX ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
:
        JSR     LAB_22B6        ; pop string off descriptor stack, or from top of string
                                ; space returns with A = length, X=$71=pointer low byte,
                                ; Y=$72=pointer high byte
        STA     str_ln          ; set string length
        STX     str_pl          ; set string pointer low byte
        STY     str_ph          ; set string pointer high byte
                                ; STORE IN FILENAME AND EXTENSION
        LDX     #0
        LDY     #0
:
        LDA     (str_pl),Y
        CMP     #'.'
        BEQ     V_DEL_EXTENSION
        CPY     str_ln
        BEQ     V_DEL_FNEND
        INY
        STA     FCB,Y
        CPY     #8
        BNE     :-
        DEY
:
        INY
        LDA     (str_pl),Y
        CMP     #'.'
        BEQ     V_DEL_EXTENSION
        CPY     str_ln
        BEQ     V_DEL_FNEND
        JMP     :-
        JMP     V_DEL_FNEND
V_DEL_EXTENSION:
        LDX     #9
:
        INY
        LDA     (str_pl),Y
        CPY     str_ln
        BEQ     V_DEL_FNEND
        STY     Rbyte3
        STX     Rbyte2
        LDY     Rbyte2
        STA     FCB,Y
        INC     Rbyte2
        LDY     Rbyte3
        INX
        CPX     #12
        BNE     :-
V_DEL_FNEND:
        LDX     #13             ; INITIALIZE SYSTEM
        JSR     PEM             ;
        LDX     #19             ; DELETE
        LDA     #<FCB           ; FCB
        LDY     #>FCB           ; FCB
        JSR     PEM
        RTS

;___RENAME_________________________________________________
;
; RENAME A FILE
;
;
; BASIC COMMAND EXPECTS THREE VARS
; DRIVE, FILENAME$, NEWFILENAME$
;
; DRIVE = NUMERIC DRIVE -- 0=CURRENT, 1=A, 2=B . . .
; FILENAME$ = OLD FILENAME STRING
; FILENAME$ = NEW FILENAME STRING
;__________________________________________________________
V_RENAME:
; CLEAR FCB
        LDA     #0              ; FILL WITH NULL
        LDY     #0              ;
:
        STA     FCB,Y           ;
        INY                     ;
        CPY     #33             ;
        BNE     :-              ;
                                ;
        LDA     #32             ; FILL FILENAME AND EXTENSION WITH SPACES
        LDY     #1              ;
:
        STA     FCB,Y           ;
        INY                     ;
        CPY     #12             ;
        BNE     :-              ;
        LDY     #17             ;
:
        STA     FCB,Y           ;
        INY                     ;
        CPY     #29             ;
        BNE     :-              ;
                                ;
        JSR     LAB_GTBY        ; GET THE FIRST PARAMETER, RETURN IN X (DRIVE)
        TXA
        STA     FCB             ; STORE DRIVE IN FCB
        STA     FCB+16          ; STORE DRIVE IN FCB
                                ;
        JSR     LAB_1C01        ; (AFTER ',') OR SYN ERR
        JSR     LAB_EVEX        ; GET THE SECOND PARAMETER
        LDA     <Dtypef         ; IS IT A STRING?
        BNE     :+              ; YES, CONTINUE ON
        LDX     #$02            ; NOPE, SYNTAX ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
:
        JSR     LAB_22B6        ; pop string off descriptor stack, or from top of string
                                ; space returns with A = length, X=$71=pointer low byte,
                                ; Y=$72=pointer high byte
        STA     str_ln          ; set string length
        STX     str_pl          ; set string pointer low byte
        STY     str_ph          ; set string pointer high byte
                                ; STORE IN FILENAME AND EXTENSION
        LDX     #0
        LDY     #0
:
        LDA     (str_pl),Y
        CMP     #'.'
        BEQ     V_REN_EXTENSION
        CPY     str_ln
        BEQ     V_REN_FNEND
        INY
        STA     FCB,Y
        CPY     #8
        BNE     :-
        DEY
:
        INY
        LDA     (str_pl),Y
        CMP     #'.'
        BEQ     V_REN_EXTENSION
        CPY     str_ln
        BEQ     V_REN_FNEND
        JMP     :-
        JMP     V_REN_FNEND
V_REN_EXTENSION:
        LDX     #9
:
        INY
        LDA     (str_pl),Y
        CPY     str_ln
        BEQ     V_REN_FNEND
        STY     Rbyte3
        STX     Rbyte2
        LDY     Rbyte2
        STA     FCB,Y
        INC     Rbyte2
        LDY     Rbyte3
        INX
        CPX     #12
        BNE     :-
V_REN_FNEND:
        JSR     LAB_1C01        ; (AFTER ',') OR SYN ERR
        JSR     LAB_EVEX        ; GET THE THIRD PARAMETER
        LDA     <Dtypef         ; IS IT A STRING?
        BNE     :+              ; YES, CONTINUE ON
        LDX     #$02            ; NOPE, SYNTAX ERROR
        JSR     LAB_XERR
        JMP     LAB_1319        ; RESET VARS, STACK AND RETURN CONTROL TO BASIC
:
        JSR     LAB_22B6        ; pop string off descriptor stack, or from top of string
                                ; space returns with A = length, X=$71=pointer low byte,
                                ; Y=$72=pointer high byte
        STA     str_ln          ; set string length
        STX     str_pl          ; set string pointer low byte
        STY     str_ph          ; set string pointer high byte
                                ; STORE IN FILENAME AND EXTENSION
        LDX     #0
        LDY     #0
:
        LDA     (str_pl),Y
        CMP     #'.'
        BEQ     V_REN_EXTENSION1
        CPY     str_ln
        BEQ     V_REN_FNEND1
        INY
        STA     FCB+16,Y
        CPY     #8
        BNE     :-
        DEY
:
        INY
        LDA     (str_pl),Y
        CMP     #'.'
        BEQ     V_REN_EXTENSION1
        CPY     str_ln
        BEQ     V_REN_FNEND1
        JMP     :-
        JMP     V_REN_FNEND1
V_REN_EXTENSION1:
        LDX     #9
:
        INY
        LDA     (str_pl),Y
        CPY     str_ln
        BEQ     V_REN_FNEND1
        STY     Rbyte3
        STX     Rbyte2
        LDY     Rbyte2
        STA     FCB+16,Y
        INC     Rbyte2
        LDY     Rbyte3
        INX
        CPX     #12
        BNE     :-
V_REN_FNEND1:
        LDX     #13             ; INITIALIZE SYSTEM
        JSR     PEM             ;
        LDX     #23             ; RENAME
        LDA     #<FCB           ; FCB
        LDY     #>FCB           ; FCB
        JSR     PEM
        RTS



VDOS65SAVE:
        .WORD   DOS65SAVE
VDOS65LOAD:
        .WORD   DOS65LOAD

FCB:
DOSDRIVE:
        .BYTE   0               ; DRIVE NUMBER
DOSFN:
        .BYTE   "        "      ; FILE NAME
DOSEX:
        .BYTE   "BAS"           ; EXTENSION
DOSET:
        .BYTE   0,0,0           ; EXTENT
DOSNR:
        .BYTE   0               ; NUMBER OF RECORDS IN FILE
DOSBL:
        .BYTE   0,0,0,0,0,0,0,0 ; BLOCKS IN FILE
        .BYTE   0,0,0,0,0,0,0,0 ;
DOSNX:
        .BYTE   0               ; NEXT RECORD
DIRFCB:
DIRDRIVE:
        .BYTE   0               ; DRIVE NUMBER
DIRFN:
        .BYTE   "????????"      ; FILE NAME
DIREX:
        .BYTE   "???"           ; EXTENSION
DIRET:
        .BYTE   0,0,0           ; EXTENT
DIRNR:
        .BYTE   0               ; NUMBER OF RECORDS IN FILE
DIRBL:
        .BYTE   0,0,0,0,0,0,0,0 ; BLOCKS IN FILE
        .BYTE   0,0,0,0,0,0,0,0 ;
DIRNX:
        .BYTE   0               ; NEXT RECORD
