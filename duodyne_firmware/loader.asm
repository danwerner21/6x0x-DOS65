;________________________________________________________________________________________________________________________________
;
;	Duodyne CUBIX CP/M loader program
;
;  DWERNER 04/24/2022 	Initial
;  PSUMMERS 8/7/2022    Accept a command line argument for CPU to switch to (0-9)
;  DWERNER 10/15/2023   MODIFY CODE FOR CUBIX09
;  DWERNER 01/21/2024 	Duodyne conversion
;  DWERNER 05/5/2024 	Duodyne port from cubix to dos65
;________________________________________________________________________________________________________________________________
BDOS:           EQU $0005                         ; BDOS invocation vector
DEFFCB:         EQU $5C                           ; Location of default FCB

        SECTION ADDR0100
        ORG     0100H
; TODO:  RE-ENABLE THIS CODE
; Check for cpu unit
        LD      A,(DEFFCB+1)                      ; Get first char of filename

        CP      '9' + 1                           ; > '9'
        JR      NC,go                             ; YES, NOT 0-9, Invalid argument

        SUB     '0'                               ; < '0'?
        JR      C,go                              ; YES, NOT 0-9, Invalid argument

        ADD     $90
        LD      (CPUunit),A                       ; Unit 0 = FFH, 1 = FEH etc
;
go:
        DI                                        ; DISABLE INTERRUPTS
        LD      B,0F3H
        RST     08
        LD      B,0F4H
        LD      D,81H
        LD      E,C
        LD      HL,3F00H
        RST     08
        LD      B,0F5H
        LD      DE,0100H
        LD      HL,0100H
        RST     08

        LD      C,9
        LD      DE,MSGFIL
        CALL    BDOS                              ; Do it
        DI                                        ; DISABLE INTERRUPTS
        LD      A,(CPUunit)                       ; GET CPU PORT
        LD      C,$15
        IN      A,(C)                             ; ENABLE 65816
; should never get here
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        HALT

;
CPUunit:
        DB      015h                              ; Default CPU unit port
;

MSGFIL:
        DB      0AH,0DH
        DM      "DOS/65 LOADED INTO RAM."
        DB      0AH,0DH
        DM      "G 8800"
        DB      0AH,0DH
        DB      0AH,0DH
        DM      "INTO 65816 MONITOR"
        DM      "$"

.END
