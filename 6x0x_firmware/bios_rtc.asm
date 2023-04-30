

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
RTCDDRD         = M6X0X_IOSPACE+$D02
RTCPORTD        = M6X0X_IOSPACE+$D00

mask_data       = $20           ; RTC data line
mask_clk        = $40           ; RTC Serial Clock line
mask_ce         = $10           ; De-activate RTC reset line

;__RTC_WRITE____________________________________________________
; write a value to the DS1302
; address in X
; value in Y
;_______________________________________________________________
RTC_WRITE:

        PHA
        SEI                     ; disable interrupts during critical section
        JSR     RTC_RESET_OFF   ; turn off RTC reset
        TXA                     ; bring into A the address from D
        AND     #%00111111      ; keep only bits 6 LSBs, discard 2 MSBs
        CLC
        ASL     A
        ORA     #%10000000      ; set MSB to one for DS1302 COMMAND BYTE (WRITE)
        JSR     RTC_WR          ; write address to DS1302
        TYA                     ; start processing value
        JSR     RTC_WR          ; write value to DS1302
        JSR     RTC_RESET_ON    ; turn on RTC reset
        CLI
        PLA
        RTS

;__RTC_READ______________________________________________________
; read a value from the DS1302
; address in X
; value in Y
;_______________________________________________________________
RTC_READ:
        PHA
        SEI                     ; disable interrupts during critical section
        JSR     RTC_RESET_OFF   ; turn off RTC reset
        TXA                     ; bring into A the address from D
        AND     #%00111111      ; keep only bits 6 LSBs, discard 2 MSBs
        CLC
        ASL     A               ; rotate address bits to the left
        ORA     #%10000001      ; set MSB to one for DS1302 COMMAND BYTE (READ)
        JSR     RTC_WR          ; write address to DS1302
        JSR     RTC_RD          ; read value from DS1302 (value is in reg A)
        TAY
        JSR     RTC_RESET_ON    ; turn on RTC reset
        CLI
        PLA
        RTS

;_______________________________________________________________
; function RTC_RESET
;   Output a RTC reset signal
;_______________________________________________________________
RTC_RESET:
        PHA
        LDA     #$70
        STA     RTCDDRD
        LDA     #mask_data
        STA     RTCPORTD
        JSR     RTC_BIT_DELAY
        JSR     RTC_BIT_DELAY
        LDA     #mask_data + mask_ce
        STA     RTCPORTD
        JSR     RTC_BIT_DELAY
        JSR     RTC_BIT_DELAY
        PLA
        RTS

;_______________________________________________________________
;
; RTC Internal Functions
;_______________________________________________________________
RTC_WR:
        STA     TEMPWORD1       ; save accumulator as it is the DATA
        LDA     #$70
        STA     RTCDDRD
        LDX     #$00            ; set X index counter of FOR loop
RTC_WR1:
        LDA     TEMPWORD1

        AND     #$01

        CMP     #$00            ; is LSB a 0 or 1?
        BEQ     RTC_WR2         ; if it�s a 0, handle it at RTC_WR2.
; LSB is a 1, handle it below
; setup RTC latch with RST and DATA high, SCLK low
        LDA     #mask_ce + mask_data
        STA     RTCPORTD
        JSR     RTC_BIT_DELAY   ; let it settle a while
; setup RTC with RST, DATA, and SCLK high
        LDA     #mask_ce + mask_clk + mask_data
        STA     RTCPORTD
        JMP     RTC_WR3         ; exit FOR loop

RTC_WR2:
; LSB is a 0, handle it below
        LDA     #mask_ce        ; setup RTC latch with RST high, SCLK and DATA low
        STA     RTCPORTD        ; output to RTC latch
        JSR     RTC_BIT_DELAY   ; let it settle a while
; setup RTC with RST and SCLK high, DATA low
        LDA     #mask_ce + mask_clk
        STA     RTCPORTD

RTC_WR3:
        JSR     RTC_BIT_DELAY   ; let it settle a while
        LSR     TEMPWORD1       ; move next bit into LSB position for processing to RTC
        INX                     ; increment A in FOR loop (A=A+1)
        CPX     #$08            ; is A < $08 ?
        BNE     RTC_WR1         ; No, do FOR loop again
        RTS                     ; Yes, end function and return


; function RTC_RD
; output value in A
; uses X
RTC_RD:
        LDA     #$50
        STA     RTCDDRD
        LDX     #$00            ; set X index counter of FOR loop

        LDA     #$00            ; set A=0 output of RTC_RD is passed in A
        STA     TEMPWORD1
        LDA     #$01            ; mask value
        STA     TEMPWORD2
RTC_RD1:
; setup RTC with RST and RD high, SCLK low
        LDA     #mask_ce
        STA     RTCPORTD
        JSR     RTC_BIT_DELAY   ; let it settle a while
        LDA     RTCPORTD        ; input from RTC latch
        AND     #$20            ; is  0 or 1?
        CMP     #$00
        BEQ     RTC_RD2         ; if is a 0, handle it below
        LDA     TEMPWORD2
        CLC
        ADC     TEMPWORD1
        STA     TEMPWORD1
; if LSB is a 0, skip it (C=C+0)
RTC_RD2:
        ASL     TEMPWORD2       ; SHIFT MASK
        LDA     #mask_ce + mask_clk
        STA     RTCPORTD
        JSR     RTC_BIT_DELAY   ; let it settle
        INX                     ; increment FOR loop (A=A+1)
        CPX     #$08            ; is A < $08 ?
        BNE     RTC_RD1         ; No, do FOR loop again
        LDA     TEMPWORD1
        RTS                     ; Yes, end function and return.  Read RTC value is in C

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




; function RTC_RESET_ON
;  { Assert RTC reset signal }
RTC_RESET_ON:
        LDA     #$70
        STA     RTCDDRD
        LDA     #mask_data
        STA     RTCPORTD
        JSR     RTC_BIT_DELAY
        JSR     RTC_BIT_DELAY
        RTS

; function RTC_RESET_OFF
;  { De-assert RTC reset signal }
RTC_RESET_OFF:
        LDA     #$70
        STA     RTCDDRD
        LDA     #mask_data +  mask_ce
        STA     RTCPORTD
        JSR     RTC_BIT_DELAY
        JSR     RTC_BIT_DELAY
        RTS
