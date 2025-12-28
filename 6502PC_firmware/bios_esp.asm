;__ESP DRIVERS______________________________________________________________________________________________________________
;
; 	CUBIX ISA DUAL ESP IO drivers for 6809PC
;
;	Entry points:
;		ESPINIT     - INIT HARDWARE
;		ESPVIDEOOUT - OUTPUT A CHARACTER TO VIDEO (ANSI)
;               ESPPS2IN    - read a character from the ps/2 keyboard ('A' POINTS TO BYTE)
;               ESPPS2BUFL  - return number of characters in the keyboard buffer in 'A'
;               ESPCURSORV  - Set Cursor Visibility (A=0 cursor off, A=1 cursor on)
;		ESPSER0OUT  - OUTPUT A CHARACTER TO Serial 0 ('A' POINTS TO BYTE)
;               ESPSER0IN   - read a character from Serial 0 ('A' POINTS TO BYTE)
;               ESPSER0BUFL - return number of characters in the Serial 0 buffer in 'A'
;		ESPSER1OUT  - OUTPUT A CHARACTER TO Serial 1 ('A' POINTS TO BYTE)
;               ESPSER1IN   - read a character from Serial 1 ('A' POINTS TO BYTE)
;               ESPSER1BUFL - return number of characters in the Serial 1 buffer in 'A'
;		ESPNETCOUT  - OUTPUT A CHARACTER TO Network Console Connection ('A' POINTS TO BYTE)
;               ESPNETCIN   - read a character from Network Console Connection ('A' POINTS TO BYTE)
;               ESPNETCBUFL - return number of characters in the Network Connection buffer in 'A'
;               PUTESP0     - put opcode/data to ESP0
;               PUTESP1     - put opcode/data to ESP1
;               GETESP0     - get opcode/data from ESP0
;               GETESP1     - get opcode/data from ESP1
;________________________________________________________________________________________________________________________________
;
;*
;*        HARDWARE I/O ADDRESSES
;*
;
ESP_BASE        = PC6502_IO+$100
ESP0_DAT        = ESP_BASE      ;
ESP1_DAT        = ESP_BASE+1    ;
ESP_STAT        = ESP_BASE+2    ;



;__________________________________________________________________________________________________
;
; STATUS BITS (FOR KBD_STATUS)
;
ESP0_RDY        = $01           ; BIT 0, ESP0 READY
ESP0_BUSY       = $02           ; BIT 1, ESP0 BUSY
ESP1_RDY        = $08           ; BIT 3, ESP1 READY
ESP1_BUSY       = $10           ; BIT 4, ESP1 BUSY
;
        .IFNDEF PC6502BIOS
;
;__________________________________________________________________________________________________
; DATA
;__________________________________________________________________________________________________
;
consoleConnect:
        .BYTE   00
;
;__________________________________________________________________________________________________
; ESP IO INITIALIZATION
;__________________________________________________________________________________________________
;
ESPINIT:
;
        JSR     LFCR            ; AND CRLF
        LDA     #<ESPMESSAGE1   ;
        STA     STRPTR          ;
        LDA     #>ESPMESSAGE1   ;
        STA     STRPTR+1        ;
        JSR     WRSTR
        JSR     LFCR            ; AND CRLF
; KEYBOARD INITIALIZATION
        LDA     #<MESSAGE2      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE2      ;
        STA     STRPTR+1        ;
        JSR     WRSTR           ; DO PROMPT
        LDA     #>ESP_BASE      ; GET BASE PORT
        JSR     PRINT_BYTE      ; PRINT BASE PORT
        LDA     #<ESP_BASE      ; GET BASE PORT
        JSR     PRINT_BYTE      ; PRINT BASE PORT
        JSR     LFCR            ; AND CRLF
        JSR     ESP_RESET0      ;
        JSR     ESP_RESET1      ;
;
        JSR     ESP0_PROBE      ; DETECT ESP0
        JSR     LFCR            ; AND CRLF
        JSR     ESP1_PROBE      ; DETECT ESP1
        JSR     LFCR            ; AND CRLF
        RTS                     ; DONE


ESP0_PROBE:
;
        LDA     #<ESPMESSAGE2   ;
        STA     STRPTR          ;
        LDA     #>ESPMESSAGE2   ;
        STA     STRPTR+1        ;
        JSR     WRSTR

        LDA     #$FF            ; ESP IDENTITY PROBE
        JSR     PUTESP0         ; SEND IT
        BCS     ESP_ERROR

        JSR     GETESP0
        BCS     ESP_ERROR
        CMP     #'E'
        BNE     ESP_ERROR
        JSR     GETESP0
        BCS     ESP_ERROR
        CMP     #'S'
        BNE     ESP_ERROR
        JSR     GETESP0
        BCS     ESP_ERROR
        CMP     #'P'
        BNE     ESP_ERROR
        JSR     GETESP0
        BCS     ESP_ERROR
        CMP     #'3'
        BNE     ESP_ERROR
        JSR     GETESP0
        BCS     ESP_ERROR
        CMP     #'2'
        BNE     ESP_ERROR
        JSR     GETESP0
        BCS     ESP_ERROR
        CMP     #'V'
        BNE     ESP_ERROR
        JSR     GETESP0
        BCS     ESP_ERROR
        CMP     #'1'
        BNE     ESP_ERROR
        LDA     #<ESPMESSAGE5   ;
        STA     STRPTR          ;
        LDA     #>ESPMESSAGE5   ;
        STA     STRPTR+1        ;
        JSR     WRSTR

ESP_RESET0:
        LDX     #$20
:
        LDA     #00
        JSR     PUTESP0
        DEX
        CPX     #$00
        BNE     :-
        CLC
        RTS
;
;
ESP_ERROR:
        LDA     #<ESPMESSAGE4   ;
        STA     STRPTR          ;
        LDA     #>ESPMESSAGE4   ;
        STA     STRPTR+1        ;
        JSR     WRSTR
        RTS

;
ESP1_PROBE:
;
        LDA     #<ESPMESSAGE3   ;
        STA     STRPTR          ;
        LDA     #>ESPMESSAGE3   ;
        STA     STRPTR+1        ;
        JSR     WRSTR

        LDA     #$FF            ; ESP IDENTITY PROBE
        JSR     PUTESP1         ; SEND IT
        BCS     ESP_ERROR

        JSR     GETESP1
        BCS     ESP_ERROR
        CMP     #'E'
        BNE     ESP_ERROR
        JSR     GETESP1
        BCS     ESP_ERROR
        CMP     #'S'
        BNE     ESP_ERROR
        JSR     GETESP1
        BCS     ESP_ERROR
        CMP     #'P'
        BNE     ESP_ERROR
        JSR     GETESP1
        BCS     ESP_ERROR
        CMP     #'3'
        BNE     ESP_ERROR
        JSR     GETESP1
        BCS     ESP_ERROR
        CMP     #'2'
        BNE     ESP_ERROR
        JSR     GETESP1
        BCS     ESP_ERROR
        CMP     #'V'
        BNE     ESP_ERROR
        JSR     GETESP1
        BCS     ESP_ERROR
        CMP     #'1'
        BNE     ESP_ERROR
        LDA     #<ESPMESSAGE5   ;
        STA     STRPTR          ;
        LDA     #>ESPMESSAGE5   ;
        STA     STRPTR+1        ;
        JSR     WRSTR
ESP_RESET1:
        LDX     #$20
:
        LDA     #00
        JSR     PUTESP1
        DEX
        CPX     #$00
        BNE     :-
        CLC
        RTS

;__________________________________________________________________________________________________
; ESPCURSORV  - Set Cursor Visibility (A=0 cursor off, A=1 cursor on)
;__________________________________________________________________________________________________
;
ESPCURSORV:
        PHA
        LDA     #05             ; ESP OPCODE
        JSR     PUTESP0         ; SEND IT
        PLA
        JSR     PUTESP0         ; SEND IT
        RTS
;__________________________________________________________________________________________________
; ESPSER0OUT  - OUTPUT A CHARACTER TO Serial 0 ('A' POINTS TO BYTE)
;__________________________________________________________________________________________________
;
ESPSER0OUT:
        PHA
        LDA     #08             ; ESP OPCODE
        JSR     PUTESP0         ; SEND IT
        PLA
        JSR     PUTESP0         ; SEND IT
        RTS
;__________________________________________________________________________________________________
; ESPSER0IN   - read a character from Serial 0 ('A' POINTS TO BYTE)
;__________________________________________________________________________________________________
;
ESPSER0IN:
        LDA     #10             ; ESP IN FROM Serial 0
        JMP     ESPCHIN
;__________________________________________________________________________________________________
; ESPSER1OUT  - OUTPUT A CHARACTER TO Serial 1 ('A' POINTS TO BYTE)
;__________________________________________________________________________________________________
;
ESPSER1OUT:
        PHA
        LDA     #08             ; ESP OPCODE
        JSR     PUTESP1         ; SEND IT
        PLA
        JSR     PUTESP1         ; SEND IT
        RTS
;__________________________________________________________________________________________________
; ESPSER1IN   - read a character from Serial 1 ('A' POINTS TO BYTE)
;__________________________________________________________________________________________________
;
ESPSER1IN:
        LDA     #10             ; ESP IN FROM Serial 1
ESPCH1IN:
        JSR     PUTESP1         ; SEND IT
        BCS     :+
        JSR     GETESP1         ; GET IT
        BCS     :+
        CMP     #$00
        BEQ     :+
        RTS
:
        LDA     #$FF
        RTS

;__________________________________________________________________________________________________
; ESPNETCOUT  - OUTPUT A CHARACTER TO Network Console Connection ('A' POINTS TO BYTE)
;               Connection Stored in 'consoleConnect' value
;__________________________________________________________________________________________________
;
ESPNETCOUT:
        PHA
        LDA     #25             ; ESP OPCODE
        JSR     PUTESP0         ; SEND IT
        LDA     consoleConnect
        JSR     PUTESP0         ; SEND IT
        PLA
        JSR     PUTESP0         ; SEND IT
        RTS
;__________________________________________________________________________________________________
; ESPNETCIN   - read a character from Network Console Connection ('A' POINTS TO BYTE)
;__________________________________________________________________________________________________
;
ESPNETCIN:
        LDA     #26             ; ESP OPCODE
        JSR     PUTESP0         ; SEND IT
        LDA     consoleConnect
        JMP     ESPCH1IN

;__________________________________________________________________________________________________
; ESPPS2BUFL - Return number of characters in keyboard buffer
;__________________________________________________________________________________________________
;
ESPPS2BUFL:
        LDA     #04             ; opcode to get buffer length
        JSR     PUTESP0         ; SEND IT
        BCS     :+
        JSR     GETESP0         ; GET IT
        BCS     :+
        RTS
:
        LDA     #$00
        RTS


;__________________________________________________________________________________________________
; ESPSER0BUFL - return number of characters in the Serial 0 buffer in 'A'
;__________________________________________________________________________________________________
;
ESPSER0BUFL:
        LDA     #11             ; opcode to get buffer length
        JSR     PUTESP0         ; SEND IT
        BCS     :+
        JSR     GETESP0         ; GET IT
        BCS     :+
        RTS
:
        LDA     #$00
        RTS

;__________________________________________________________________________________________________
; ESPSER1BUFL - return number of characters in the Serial 1 buffer in 'A'
;__________________________________________________________________________________________________
;
ESPSER1BUFL:
        LDA     #11             ; opcode to get buffer length
ESP1BUFL:
        JSR     PUTESP1         ; SEND IT
        BCS     :+
        JSR     GETESP1         ; GET IT
        BCS     :+
        RTS
:
        LDA     #$00
        RTS
;__________________________________________________________________________________________________
; ESPNETCBUFL - return number of characters in the Network Connection buffer in 'A'
;__________________________________________________________________________________________________
;
ESPNETCBUFL:
        LDA     #28             ; opcode to get buffer length
        JMP     ESP1BUFL

        .ENDIF
;__________________________________________________________________________________________________
; ESPVIDEOOUT - output character in 'A' to CRT (ANSI terminal emulation)
;__________________________________________________________________________________________________
;
ESPVIDEOOUT:
        PHA
        LDA     #01             ; ESP OUT TO SCREEN
        JSR     PUTESP0         ; SEND IT
        PLA
        JSR     PUTESP0         ; SEND IT
        RTS
;__________________________________________________________________________________________________
; ESPPS2IN - Fetch character out of Keyboard Buffer into 'A'  ($FF is no characters waiting)
;__________________________________________________________________________________________________
;
ESPPS2IN:
        LDA     #03             ; ESP IN FROM PS2
ESPCHIN:
        JSR     PUTESP0         ; SEND IT
        BCS     :+
        JSR     GETESP0         ; GET IT
        BCS     :+
        CMP     #$00
        BEQ     :+
        RTS
:
        LDA     #$FF
        RTS

ESPPS2INW:
        JSR     ESPPS2IN
        CMP     #$FF
        BEQ     ESPPS2INW
        RTS
;
;__________________________________________________________________________________________________
; HARDWARE INTERFACE
;__________________________________________________________________________________________________
;
; a=VALUE AND RETURN
; Carry set on timeout
;
;__________________________________________________________________________________________________
PUTESP0:
        PHA
        TXA
        PHA
        TYA
        PHA
        LDX     #$00
        LDY     #$00
:
        LDA     ESP_STAT
        AND     #ESP0_BUSY
        BEQ     :+
        DEX
        CPX     #$00
        BNE     :-
        DEY
        CPY     #$00
        BNE     :-
        PLA
        TAY
        PLA
        TAX
        PLA
        SEC
        RTS
:
        PLA
        TAY
        PLA
        TAX
        PLA
        STA     ESP0_DAT
        CLC
        RTS

GETESP0:
        TXA
        PHA
        TYA
        PHA
        LDX     #$00
        LDY     #$00
:
        LDA     ESP_STAT
        AND     #ESP0_BUSY
        BEQ     :+
        DEX
        CPX     #$00
        BNE     :-
        DEY
        CPY     #$00
        BNE     :-
        PLA
        TAY
        PLA
        TAX
        SEC
        RTS
:
        LDX     #$00
        LDY     #$00
:
        LDA     ESP_STAT
        AND     #ESP0_RDY
        BNE     :+
        DEX
        CPX     #$00
        BNE     :-
        DEY
        CPY     #$00
        BNE     :-
        PLA
        TAY
        PLA
        TAX
        SEC
        RTS
:
        PLA
        TAY
        PLA
        TAX
        LDA     ESP0_DAT
        CLC
        RTS

PUTESP1:
        PHA
        TXA
        PHA
        TYA
        PHA
        LDX     #$00
        LDY     #$00
:
        LDA     ESP_STAT
        AND     #ESP1_BUSY
        BEQ     :+
        DEX
        CPX     #$00
        BNE     :-
        DEY
        CPY     #$00
        BNE     :-
        PLA
        TAY
        PLA
        TAX
        PLA
        SEC
        RTS
:
        PLA
        TAY
        PLA
        TAX
        PLA
        STA     ESP1_DAT
        CLC
        RTS

GETESP1:
        TXA
        PHA
        TYA
        PHA
        LDX     #$00
        LDY     #$00
:
        LDA     ESP_STAT
        AND     #ESP1_BUSY
        BEQ     :+
        DEX
        CPX     #$00
        BNE     :-
        DEY
        CPY     #$00
        BNE     :-
        PLA
        TAY
        PLA
        TAX
        SEC
        RTS
:
        LDX     #$00
        LDY     #$00
:
        LDA     ESP_STAT
        AND     #ESP1_RDY
        BNE     :+
        DEX
        CPX     #$00
        BNE     :-
        DEY
        CPY     #$00
        BNE     :-
        PLA
        TAY
        PLA
        TAX
        SEC
        RTS
:
        PLA
        TAY
        PLA
        TAX
        LDA     ESP1_DAT
        CLC
        RTS


        .IFNDEF PC6502BIOS
;
; DRIVER DATA
;__________________________________________________________________________________________________
; MESSAGES
;__________________________________________________________________________________________________
ESPMESSAGE1:
        .BYTE   "DUAL ESP IO:"
        .BYTE   00
ESPMESSAGE2:
        .BYTE   "  ESP0: "
        .BYTE   00
ESPMESSAGE3:
        .BYTE   "  ESP1: "
        .BYTE   00
ESPMESSAGE4:
        .BYTE   "NOT "
ESPMESSAGE5:
        .BYTE   "FOUND."
        .BYTE   00
        .ENDIF
