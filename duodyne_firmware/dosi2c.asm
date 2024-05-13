;__PCF DRIVER____________________________________________________________________________________________________________________
;
; PCF8584 I2C DRIVER
;
;       Entry points:
;               PCF_INIT
;               PCF_SENDBYTES
;               PCF_READBYTES
;
;
;________________________________________________________________________________________________________________________________
;
;
PCF_BASE        = IO_AREA+$56   ; PORT
PCF_ID          = $AA
CPU_CLK         = 8
;
PCF_RS0         = PCF_BASE
PCF_RS1         = PCF_RS0+1
PCF_OWN         = $55           ; PCF_ID>>1   PCF'S ADDRESS IN SLAVE MODE  (LWASM does not seem to have a bit shift operator)
;
;
; CONTROL REGISTER BITS
;
PCF_PIN         = %10000000
PCF_ES0         = %01000000
PCF_ES1         = %00100000
PCF_ES2         = %00010000
PCF_EN1         = %00001000
PCF_STA         = %00000100
PCF_STO         = %00000010
PCF_ACK         = %00000001
;
PCF_START_      = PCF_PIN|PCF_ES0|PCF_STA|PCF_ACK
PCF_STOP_       = PCF_PIN|PCF_ES0|PCF_STO|PCF_ACK
;PCF_REPSTART_   = PCF_ES0|PCF_STA|PCF_ACK
PCF_IDLE_       = PCF_PIN|PCF_ES0|PCF_ACK
;
; STATUS REGISTER BITS
;
;PCF_PIN        =  %10000000
PCF_INI         = %01000000     ; 1 if not initialized
PCF_STS         = %00100000
PCF_BER         = %00010000
PCF_AD0         = %00001000
PCF_LRB         = %00001000
PCF_AAS         = %00000100
PCF_LAB         = %00000010
PCF_BB          = %00000001
;
; THE PCF8584 TARGETS A TOP I2C CLOCK SPEED OF 90KHZ AND SUPPORTS DIVIDERS FOR
; 3, 4.43, 6, 8 AND 12MHZ TO ACHEIVE THIS.
;
; +--------------------------------------------------------------------------------------------+
; | div/clk |  2MHz |  4MHz  |  6MHz | 7.38Mhz |  10MHz | 12MHz |  16MHz | 18.432Mhz |  20MHz  |
; +----------------------------------------------------------------------------------+---------+
; |   3MHz  | 60Khz | 120Khz |       |         |        |       |        |           |         |
; | 4.43MHz |       |  81Khz |       |         |        |       |        |           |         |
; |   6MHz  |       |        | 90Khz | 110Khz  |        |       |        |           |         |
; |   8MHz  |       |        |       |  83Khz  | 112Khz |       |        |           |         |
; |  12MHz  |       |        |       |         |        | 90Khz | 120Khz |   138Khz  |  150Khz |
; +----------------------------------------------------------------------------------+---------+
;
; CLOCK CHIP FREQUENCIES
;
PCF_CLK3        = $00
PCF_CLK443      = $10
PCF_CLK6        = $14
PCF_CLK8        = $18
PCF_CLK12       = $1C
;
; TRANSMISSION FREQUENCIES
;
PCF_TRNS90      = $00           ;  90 kHz */
PCF_TRNS45      = $01           ;  45 kHz */
PCF_TRNS11      = $02           ;  11 kHz */
PCF_TRNS15      = $03           ; 1.5 kHz */
;
; BELOW VARIABLES CONTROL PCF CLOCK DIVISOR PROGRAMMING
; HARD-CODED FOR NOW
;
PCF_CLK         = PCF_CLK8
PCF_TRNS        = PCF_TRNS90
;
; TIMEOUT AND DELAY VALUES (ARBITRARY)
;
PCF_PINTO       = 65000
PCF_ACKTO       = 65000
PCF_BBTO        = 65000
PCF_LABDLY      = 65000
;
;
;__PCF_INIT___________________________________________________________________________________________
;
;  FRONT PANEL INIT
;____________________________________________________________________________________________________
;
PCF_INIT:
        JSR     NEWLINE         ; AND CRLF
        PRTS    "I2C PCF:$"
        JSR     NEWLINE         ; AND CRLF
        PRTS    " IO=0x$"
        LDA     #>PCF_BASE      ; GET BASE PORT
        JSR     PRTHEXBYTE      ; PRINT BASE PORT
        LDA     #<PCF_BASE      ; GET BASE PORT
        JSR     PRTHEXBYTE      ; PRINT BASE PORT
        JSR     SPACE
        JSR     PCF_INITDEV
        LDA     PCF_FAIL_FLAG
        CMP     #$00
        BNE     :+
        PRTS    "PRESENT$"
:
        JSR     NEWLINE         ; AND CRLF
        RTS                     ; DONE
;-----------------------------------------------------------------------------
PCF_INITDEV:
        LDA     #PCF_PIN        ; S1=80H: S0 SELECTED, SERIAL
        STA     PCF_RS1         ; INTERFACE OFF
        NOP
        LDA     PCF_RS1         ; CHECK TO SEE S1 NOW USED AS R/W
        AND     #$7F            ; CTRL. PCF8584 DOES THAT WHEN ESO
        BEQ     :+              ; IS ZERO
        PRTS    "NOT PRESENT$"
        LDA     #$FF
        STA     PCF_FAIL_FLAG
        RTS
:
;
        LDA     #PCF_OWN        ; LOAD OWN ADDRESS IN S0,
        STA     PCF_RS0         ; EFFECTIVE ADDRESS IS (OWN <<1)
        NOP
        LDA     PCF_RS0         ; CHECK IT IS REALLY WRITTEN
        CMP     #PCF_OWN
        BEQ     :+
        PRTS    "SETTING DEVICE ID FAILED$"
        LDA     #$FF
        STA     PCF_FAIL_FLAG
        RTS
:
;
        LDA     #PCF_PIN|PCF_ES1; S1=0A0H
        STA     PCF_RS1         ; NEXT BYTE IN S2
        NOP
        LDA     PCF_RS1
;
        LDA     #PCF_CLK|PCF_TRNS; LOAD CLOCK REGISTER S2
        STA     PCF_RS0
        NOP
        LDA     PCF_RS0         ; CHECK IT'S REALLY WRITTEN, ONLY
        AND     #$1F            ; THE LOWER 5 BITS MATTER
        CMP     #PCF_CLK|PCF_TRNS
        BNE     PCF_CLKERR
;
        LDA     #PCF_IDLE_
        STA     PCF_RS1
        NOP
        LDA     PCF_RS1
        JSR     PRTHEXBYTE      ; PRINT BASE PORT
        LDA     PCF_RS1
        CMP     #PCF_PIN|PCF_BB
        BEQ     :+
        PRTS    "BUS IDLE FAILED$"
        LDA     #$FF
        STA     PCF_FAIL_FLAG
        RTS
:
;
        LDA     #$00
        STA     PCF_FAIL_FLAG
        RTS
;



;-----------------------------------------------------------------------------
; DISPLAY ERROR MESSAGES
;


PCF_CLKERR:
        PRTS    "CLOCK SET FAIL$"
        LDA     #$FF
        STA     PCF_FAIL_FLAG
        RTS
;
PCF_FAIL_FLAG:
        .BYTE   0
;__PCF_SENDBYTES__________________________________________________________________________________________
;
; 	Send a buffer to i2c
;
; Entry:
;	A=BUFFER POINTER LOW BYTE
;	Y=BUFFER HIGH BYTE
;   X=i2c Address
;
;   First Two bytes of buffer is byte count
;
;   Return:
;       A = Return Status
;          FF=ERROR
;          00=SUCCESS
;________________________________________________________________________________________________________
PCF_SENDBYTES:
        STY     pcf_buffer+1
        STA     pcf_buffer
        STX     pcf_address
        LDA     PCF_FAIL_FLAG
        CMP     #$00
        BNE     PCF_WERROR
PCF_SENDBYTES_INTERNAL:
        JSR     PCF_WAIT_FOR_BB ; DO WE HAVE THE BUS?
        CMP     #$00
        BEQ     PCF_WB1         ; YES
        LDA     #$FF
        RTS
PCF_WB1:
        ACCUMULATORINDEX16
        LDX     pcf_buffer      ; SET X TO BUFFER SPACE
        LDA     0,x             ; SET Y TO BUFFER LENGTH
        TAY
        INX                     ; SKIP TO START OF DATA
        INX
        ACCUMULATOR8
        INDEX16
        LDA     pcf_address
        ASL
        AND     #$FE
        STA     PCF_RS0         ; send device address
        LDA     #PCF_START_     ; begin transmission
        STA     PCF_RS1
:
        JSR     PCF_WAIT_FOR_PIN
        CMP     #$00
        BNE     PCF_WERROR
        LDA     0,X
        INX
        STA     PCF_RS0
        DEY
        CPY     #$0000
        BNE     :-
        INDEX8
        JSR     PCF_WAIT_FOR_PIN
        CMP     #$00
        BNE     PCF_WERROR
        LDA     #PCF_STOP_      ; end transmission
        STA     PCF_RS1
        LDA     #$00
        RTS
PCF_WERROR:
        LDA     #PCF_STOP_      ; end transmission
        STA     PCF_RS1
        LDA     #$FF
        RTS

;__PCF_READBYTES__________________________________________________________________________________________
;
; 	READ i2c into a buffer
;
; Entry:
;	A=BUFFER POINTER LOW BYTE
;	Y=BUFFER HIGH BYTE
;   X=i2c Address
;
;   First Two bytes of buffer is byte count
;
;   Return:
;       A = Return Status
;          FF=ERROR
;          00=SUCCESS
;________________________________________________________________________________________________________
;
PCF_READBYTES:
        STY     pcf_buffer+1
        STA     pcf_buffer
        STX     pcf_address
        LDA     PCF_FAIL_FLAG
        CMP     #$00
        BNE     PCF_RERROR
PCF_READBYTES_INTERNAL:
        LDA     pcf_address
        ASL
        ORA     #$01
        STA     PCF_RS0         ; send device address
        JSR     PCF_WAIT_FOR_BB ; DO WE HAVE THE BUS?
        CMP     #$00
        BEQ     PCF_RB1         ; YES
        LDA     #$FF
        RTS
PCF_RB1:
        ACCUMULATORINDEX16
        LDX     pcf_buffer      ; SET X TO BUFFER SPACE
        LDA     0,x             ; SET Y TO BUFFER LENGTH
        TAY
        INX                     ; SKIP TO START OF DATA
        INX
        ACCUMULATOR8
        INDEX16
        LDA     #PCF_START_     ; begin rcv
        STA     PCF_RS1
        JSR     PCF_WAIT_FOR_PIN
        LDA     PCF_RS0
:
        JSR     PCF_WAIT_FOR_PIN
        CMP     #$00
        BNE     PCF_RERROR
        CPY     #$0001
        BEQ     :+
        LDA     PCF_RS0
        STA     0,X
        INX
        DEY
        BRA     :-
:
        LDA     #PCF_INI        ; ack
        STA     PCF_RS1
        LDA     PCF_RS0
        STA     0,X
        INX
        JSR     PCF_WAIT_FOR_PIN
        LDA     #PCF_STOP_      ; end RCV
        STA     PCF_RS1
        LDA     PCF_RS0
        STA     0,X
        INDEX8
        LDA     #$00
        RTS
PCF_RERROR:
        INDEX8
        LDA     #PCF_STOP_      ; end RCV
        STA     PCF_RS1
        LDA     #$FF
        RTS
;
;
;-----------------------------------------------------------------------------
;
; RETURN A=00/Z  IF SUCCESSFULL
; RETURN A=FF/NZ IF TIMEOUT
; RETURN A=01/NZ IF LOST ARBITRATION
; PCF_STATUS HOLDS LAST PCF STATUS
;
PCF_WAIT_FOR_PIN:
        STORECONTEXT
        INDEX16
        ACCUMULATOR8
        LDX     #PCF_PINTO      ; SET TIMEOUT VALUE
PCF_WFP0:
        LDA     PCF_RS1         ; GET BUS
        STA     PCF_STATUS      ; STATUS
        DEX                     ; HAVE WE TIMED OUT
        CPX     #$0000
        BEQ     PCF_WFP1        ; YES WE HAVE, GO ACTION IT
        AND     #PCF_PIN        ; IS TRANSMISSION COMPLETE?
        CMP     #$00
        BNE     PCF_WFP0        ; KEEP ASKING IF NOT OR
        LDA     PCF_STATUS      ; WE GOT PIN SO NOW
        AND     #PCF_LRB        ; CHECK WE HAVE
        CMP     #$00            ; CHECK WE HAVE
        BEQ     :+              ; RECEIVED ACKNOWLEDGE
        RESTORECONTEXT
        LDA     #$01
        RTS
:
        RESTORECONTEXT
        LDA     #$00
        RTS
PCF_WFP1:
        RESTORECONTEXT
        LDA     #$FF
        RTS
;
PCF_STATUS:
        .BYTE   $00
;-----------------------------------------------------------------------------
;
; POLL THE BUS BUSY BIT TO DETERMINE IF BUS IS FREE.
; RETURN WITH A=00H/Z STATUS IF BUS IS FREE
; RETURN WITH A=FFH/NZ STATUS IF BUS IS BUSY
;
; AFTER RESET THE BUS BUSY BIT WILL BE SET TO 1 I.E. NOT BUSY
;
PCF_WAIT_FOR_BB:
        STORECONTEXT
        INDEX16
        ACCUMULATOR8
        LDX     #PCF_BBTO
PCF_WFBB0:
        LDA     PCF_RS1
        AND     #PCF_BB
        CMP     #PCF_BB
        BEQ     :+
        DEX
        CMP     #$00
        BNE     PCF_WFBB0       ; REPEAT IF NOT TIMED OUT
        RESTORECONTEXT
        LDA     #$FF            ; RET NZ IF TIMEOUT
        RTS
:
        RESTORECONTEXT
        LDA     #$00
        RTS
;
