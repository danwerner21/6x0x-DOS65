

;__RTC DRIVERS___________________________________________________________________________________________________________________
;
; 	DOS REAL TIME CLOCK drivers
;
;	Entry points:
;		DOSREADRTC   - called to RETURN RTC
;________________________________________________________________________________________________________________________________
;
;*
;* HARDWARE I/O ADDRESSES
;*
RTCBASE         = PC6502_IOSPACE+$90;


;__RTC_WRITE____________________________________________________
; write a value to the RTC
; address in X
; value in Y
;_______________________________________________________________
RTC_WRITE:
        CPX     #7
        BCS     :++
        CPX     #6
        BNE     :+
        DEX
:
        STY     RTCWORK
        TXA
        CLC
        ROL     A               ; ADDRESS * 2
        AND     #$0F            ; CLEAR UPPER BITS
        TAX                     ; address in x
        LDA     #$02
        STA     RTCBASE+$0F
        LDA     RTCWORK         ; value in A
        AND     #$0F
        STA     RTCBASE,X       ; SET LOW NIBBLE OF VALUE
        INX
        LDA     RTCWORK         ; value in A
        AND     #$F0
        CLC
        ROR     A
        ROR     A
        ROR     A
        ROR     A
        STA     RTCBASE,X       ; SET HIGH NIBBLE OF VALUE
        LDA     #$00
        STA     RTCBASE + $0D
        STA     RTCBASE + $0E
        LDA     #$01
        STA     RTCBASE + $0F
        LDA     #$05
        STA     RTCBASE + $0F
        LDA     #$04
        STA     RTCBASE + $0F
        LDA     #$00
        RTS
:
        LDA     #$FF
        RTS


;__RTC_READ______________________________________________________
; read a value from the RTC
; address in X
; value in Y
;_______________________________________________________________
RTC_READ:
        CPX     #6
        BNE     :+
        DEX
:
        TXA
        CLC                     ; CLEAR CARRY
        ROL     A               ; ADDRESS * 2
        AND     #$0F            ; CLEAR UPPER BITS
        TAX                     ; address in x
        LDA     RTCBASE,X       ; GET LOW NIBBLE OF VALUE
        AND     #$0F
        STA     RTCWORK
        INX
        LDA     RTCBASE,X       ; GET HIGH NIBBLE OF VALUE
        ROL     A
        ROL     A
        ROL     A
        ROL     A
        AND     #$F0
        ORA     RTCWORK         ; return value in a
        TAY
        LDA     #$00
        RTS

;_______________________________________________________________
; function RTC_BEEP
;
;_______________________________________________________________
RTC_BEEP:
        RTS                     ; NOT SUPPORTED NO HARDWARE

;_______________________________________________________________
; function RTC_BUTTON
;
;_______________________________________________________________
RTC_BUTTON:
        LDA     #$FF
        RTS                     ; NOT SUPPORTED NO HARDWARE

;_______________________________________________________________
; function RTC_LED
;
;_______________________________________________________________
RTC_LED:
        RTS                     ; NOT SUPPORTED NO HARDWARE

;__RTC_INIT________________________________________________________________________________________
;
;  INIT AND DISPLAY RTC INFO
;____________________________________________________________________________________________________
;
RTC_INIT:
        JSR     LFCR            ; AND CRLF
        LDA     #<RTCMESSAGE1   ;
        STA     STRPTR          ;
        LDA     #>RTCMESSAGE1   ;
        STA     STRPTR+1        ;
        JSR     WRSTR
        JSR     LFCR            ; AND CRLF
        LDA     #<MESSAGE2      ;
        STA     STRPTR          ;
        LDA     #>MESSAGE2      ;
        STA     STRPTR+1        ;
        JSR     WRSTR
        LDA     #>RTCBASE
        JSR     PRINT_BYTE
        LDA     #<RTCBASE
        JSR     PRINT_BYTE
        JSR     LFCR            ; AND CRLF
        LDA     #<RTCMESSAGE2   ;
        STA     STRPTR          ;
        LDA     #>RTCMESSAGE2   ;
        STA     STRPTR+1        ;
        JSR     WRSTR
        LDX     #2
        JSR     RTC_READ
        TYA
        JSR     PRINT_BYTE
        LDA     #':'
        JSR     DFT_CONSOLE_OUT
        LDX     #1
        JSR     RTC_READ
        TYA
        JSR     PRINT_BYTE
        LDA     #':'
        JSR     DFT_CONSOLE_OUT
        LDX     #0
        JSR     RTC_READ
        TYA
        JSR     PRINT_BYTE
        LDA     #' '
        JSR     DFT_CONSOLE_OUT
        LDX     #4
        JSR     RTC_READ
        TYA
        JSR     PRINT_BYTE
        LDA     #'/'
        JSR     DFT_CONSOLE_OUT
        LDX     #3
        JSR     RTC_READ
        TYA
        JSR     PRINT_BYTE
        LDA     #'/'
        JSR     DFT_CONSOLE_OUT
        LDX     #6
        JSR     RTC_READ
        TYA
        JSR     PRINT_BYTE
        JSR     LFCR            ; AND CRLF
        RTS



RTCWORK:
        .BYTE   00


RTCMESSAGE1:
        .BYTE   "RTC:"
        .BYTE   00
RTCMESSAGE2:
        .BYTE   " CURRENT DATE/TIME:  "
        .BYTE   00
