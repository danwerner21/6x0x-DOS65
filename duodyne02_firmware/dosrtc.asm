;__RTC DRIVERS____________________________________________________________________________________________________________________
;
; 	Duodyne Real Time Clock drivers (ROMRAM Card)
;
;	Entry points:
;		RTC_INIT        - called during OS init
;		RTC_READ        - read a sector from drive
;		RTC_WRITE       - write a sector to drive
;		RTC_LED         - UPDATE LEDS
;               RTC_BUTTON      - GET BUTTON STATUS
;               RTC_BEEP        - BEEP RTC SPEAKER
;
;________________________________________________________________________________________________________________________________
;
; RAM BANK $1E is RAM area for Drivers
; RAM BANK $1D is operating bank for DOS/65 $8000-$FFFF
;
; ROM BANKS $00 and $0C-$0F are reserved for ROMWBW code
; ROM Drive starts in bank $22
; RAM Drive starts in bank $02
;
;
;
RTCIO           = IO+$94        ; PORT A
;
; Write Bits
; Latch IO   MSB   7 6 5 4 3 2 1 0  LSB
;                  | | | | | | | |-- USER LED 0
;                  | | | | | | |---- USER LED 1
;                  | | | | | |------ SPEAKER
;                  | | | | |-------- /MEMORY ENABLE
;                  | | | |---------- RTC RESET
;                  | | |------------ RTC WRITE
;                  | |-------------- RTC CLK
;                  |---------------- RTC DATA
;
; Read Bits
; Latch IO   MSB   7 6 5 4 3 2 1 0  LSB
;                  X | X X X X | |-- RTC DQ
;                    |         |---- /MEMORY ENABLE
;                    |-------------- USER BUTTON
;
;
;


mask_data_out   = $01           ; RTC data line
mask_data_in    = $80           ; RTC data line
mask_clk        = $40           ; RTC Serial Clock line
mask_wrt_en     = $20           ; RTC Serial Clock line
mask_ce         = $10           ; RTC CHIP ENABLE

mask_mem_en     = $08
mask_spk        = $04
mask_led1       = $02
mask_led0       = $01

mask_button     = $40

;__RTC_INIT__________________________________________________________________________________________
;
;  INIT RTC
;____________________________________________________________________________________________________
;
RTC_INIT:
        PRTDBG  "RTC INIT:$"
        PRTS    "RTC: $"
        JSR     NEWLINE
        PRTS    " IO=0x$"
        LDA     #>RTCIO         ; GET BASE PORT
        JSR     PRTHEXBYTE      ; PRINT BASE PORT
        LDA     #<RTCIO         ; GET BASE PORT
        JSR     PRTHEXBYTE      ; PRINT BASE PORT
        JSR     NEWLINE         ;
        LDA     RTCVALUE        ; RESET PORT VALUE
        STA     f:RTCIO
        LDY     #$80
        LDX     #$40
        JSR     RTC_BEEP

        RTS

RTCVALUE:
        .BYTE   $00


;__RTC_WRITE____________________________________________________
; write a value to the DS1302
; address in X
; value in Y
;_______________________________________________________________
RTC_WRITE:
        PHA
        JSR     RTC_CE_ENABLE
        TXA                     ; bring into A the address from X
        AND     #%00111111      ; keep only bits 6 LSBs, discard 2 MSBs
        CLC
        ASL     A
        ORA     #%10000000      ; set MSB to one for DS1302 COMMAND BYTE (WRITE)
        JSR     RTC_WR          ; write address to DS1302
        TYA                     ; start processing value
        JSR     RTC_WR          ; write value to DS1302
        JSR     RTC_CE_DISABLE
        PLA
        RTS

;__RTC_READ______________________________________________________
; read a value from the DS1302
; address in X
; value in Y
;_______________________________________________________________
RTC_READ:
        PHA
        JSR     RTC_CE_ENABLE
        TXA                     ; bring into A the address from X
        AND     #%00111111      ; keep only bits 6 LSBs, discard 2 MSBs
        CLC
        ASL     A               ; rotate address bits to the left
        ORA     #%10000001      ; set MSB to one for DS1302 COMMAND BYTE (READ)
        JSR     RTC_WR          ; write address to DS1302
        JSR     RTC_RD          ; read value from DS1302 (value is in reg A)
        TAY
        JSR     RTC_CE_DISABLE
        PLA
        RTS


;__RTC_LED______________________________________________________
; Control the LEDs on the RomRam card
; LED#  in X
; value in Y
;_______________________________________________________________
RTC_LED:
        PHA
        TXA
        AND     #$01
        CMP     #00
        BNE     :+
        TYA                     ; LED 00
        AND     #$01
        ORA     RTCVALUE
        STA     RTCVALUE
        STA     f:RTCIO
        RTS
:
        TYA                     ; LED 01
        AND     #$01
        ASL
        ORA     RTCVALUE
        STA     RTCVALUE
        STA     f:RTCIO
        RTS
;__RTC_BUTTON___________________________________________________
; read the button state on the RomRam card
; button value in A
;_______________________________________________________________
RTC_BUTTON:
;        LDA     f:RTCIO
        AND     mask_button
        RTS

;__RTC_BEEP_____________________________________________________
; Produce a tone on the RomRam Speaker
; frequency in X (*FF)
; length in Y (*FF)
;_______________________________________________________________
RTC_BEEP:
        PHA
        STX     TEMPWORD
        STY     TEMPWORD1+1
        LDA     #$00
        STA     TEMPWORD+1
        STA     TEMPWORD1
        LDA     RTCVALUE
        STA     RTC_BEEP_TEMP

;        LDY     TEMPWORD1
;        LDX     TEMPWORD
;RTC_BEEP1:
;        DEX
;        CPX     #$00
;        BNE     :+
;        LDX     TEMPWORD
;        LDA     RTC_BEEP_TEMP
;        EOR     #mask_spk
;        STA     RTC_BEEP_TEMP
;        AND     #$F7
;        STA     f:RTCIO
;:
;        DEY
;        CPY     #$0000
;        BNE     RTC_BEEP1
;
;        INDEX8
;        LDA     RTCVALUE
;        STA     f:RTCIO
;        PLA
        RTS

RTC_BEEP_TEMP:
        .BYTE   00



;_______________________________________________________________
;
; RTC Internal Functions
;_______________________________________________________________
RTC_CE_ENABLE:
        LDA     RTCVALUE
        ORA     #mask_ce
        STA     RTCVALUE
        STA     f:RTCIO
        RTS

RTC_CE_DISABLE:
        LDA     RTCVALUE
        AND     #mask_ce^$ff
        STA     RTCVALUE
        STA     f:RTCIO
        RTS

; function RTC_WR
; send value in A
; uses X
RTC_WR:
        STA     TEMPWORD1       ; save accumulator as it is the DATA
        LDX     #$00            ; set X index counter of FOR loop
RTC_WR1:
        LDA     TEMPWORD1
        AND     #$01
        CMP     #$00            ; is LSB a 0 or 1?
        BEQ     :+              ; if its a 0, do not set data.
; LSB is a 1, handle it below
        LDA     RTCVALUE
        ORA     #mask_data_in
        STA     f:RTCIO
        JSR     RTC_BIT_DELAY   ; let it settle a while
:
        ORA     #mask_clk
        STA     f:RTCIO
        JSR     RTC_BIT_DELAY   ; let it settle a while
        LDA     RTCVALUE
        STA     f:RTCIO
        JSR     RTC_BIT_DELAY   ; let it settle a while

        LSR     TEMPWORD1       ; move next bit into LSB position for processing to RTC
        INX                     ; increment A in FOR loop (A=A+1)
        CPX     #$08            ; is A < $08 ?
        BNE     RTC_WR1         ; No, do FOR loop again
        RTS                     ; Yes, end function and return


; function RTC_RD
; get value into A
; uses X
RTC_RD:
        LDA     RTCVALUE
        ORA     #mask_wrt_en
        STA     RTCVALUE
        STA     f:RTCIO
        LDA     #$00            ; set A=0 output of RTC_RD is passed in A
        STA     TEMPWORD1
        LDX     #$00            ; set X index counter of FOR loop
RTC_RD1:
        LDA     RTCVALUE
        ORA     #mask_clk
        STA     f:RTCIO
        JSR     RTC_BIT_DELAY   ; let it settle a while
        LDA     f:RTCIO
        AND     #mask_data_out
        CMP     #$00
        BEQ     :+
        LDA     TEMPWORD1
        ORA     #1
        STA     TEMPWORD1
:
        LDA     RTCVALUE
        AND     #mask_wrt_en^$ff
        STA     RTCVALUE
        STA     f:RTCIO
        JSR     RTC_BIT_DELAY   ; let it settle a while
        ASL     TEMPWORD1       ; SHIFT
        INX                     ; increment FOR loop (A=A+1)
        CPX     #$08            ; is A < $08 ?
        BNE     RTC_RD1         ; No, do FOR loop again
        LDA     TEMPWORD1
        RTS                     ; Yes, end function and return.  Read RTC value is in A

RTC_BIT_DELAY:                  ; purpose is to delay ~36 uS
; (6) JSR INTO
        PHA                     ; 3
        LDA     #$02            ; 2  (1 REP AT 1 MHZ 6 REPS AT 2MHZ)
        STA     TEMPWORD        ; 3
RTC_BIT_DELAY1:
        DEC     TEMPWORD        ;5
        BNE     RTC_BIT_DELAY1  ;3

        NOP                     ; 2
        NOP                     ; 2
        PLA                     ; 4
        RTS                     ; 6
