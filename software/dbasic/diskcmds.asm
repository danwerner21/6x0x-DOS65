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

LAB_KILL:
RETURN_TO_OS:
        JMP     $0100



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
        LDA     #<FILEERROR2    ; NO, ERROR OUT
        LDY     #>FILEERROR2
        LDX     #9              ; Print error message
        JSR     PEM             ;
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


V_LOAD: ; load BASIC program
DOS65LOAD:
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
        LDA     #<FILEERROR2    ; NO, ERROR OUT
        LDY     #>FILEERROR2
        LDX     #9              ; Print error message
        JSR     PEM             ;
DOS65LOAD_ERR1:
        JMP     LAB_REM         ; comment out the remainder of the line (if any)
DOS65LOAD_1:
        LDA     <Smeml          ; All is well, file opened and continue
        STA     FCBPTR+2        ; point to base of RAM
        LDA     <Smemh          ;
        STA     FCBPTR+3        ;
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
        JSR     RDERCR          ;
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
        LDA     #<FILEERROR1    ; NO, ERROR OUT
        LDY     #>FILEERROR1
        LDX     #9
        JSR     PEM
        JSR     LAB_REM
        SEC
        RTS
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

;SUBROUTINES
;OPEN FILE
OPNFIL:
        LDX     #15
        JMP     PEM
;CLOSE FILE
CLSFIL:
        LDX     #16
        JMP     PEM
;DELETE FILE
DLTFIL:
        LDX     #19
        JMP     PEM
;READ RECORD
RDERCR:
        LDX     #20
        JMP     PEM
;WRITE RECORD
WRTRCR:
        LDX     #21
        JMP     PEM
;CREATE FILE
CRTFIL:
        LDX     #22
        JMP     PEM
;RENAME FILE
RNMFIL:
        LDX     #23
        JMP     PEM
;SET BUFFER
SETBUF:
        LDX     #26
        JMP     PEM


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
