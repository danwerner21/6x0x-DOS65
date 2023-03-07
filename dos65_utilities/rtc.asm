
;
; Program:	rtc.asm
; Author:		Andrew Lynch
; Date:		22 Feb 2007
; Enviroment:	TASM MS-DOS Z80 Cross Assembler source for CP/M
;

; constants

RTC_WRITE       = $FD3F
RTC_READ        = $FD42
RTC_RESET       = $FD45

TEMPWORD        = $40
TEMP            = $42
TEMP1           = $43
TEMPPAUSE       = $44

PEM             = $103          ;PEM ENTRY
DFLFCB          = $107          ;DEFAULT FCB
TEA             = $800          ;TEA START


        .SEGMENT "TEA"
        .ORG    TEA
; print message 16 times
Print:


        LDA     #<MSG           ;SEND
        LDY     #>MSG
        LDX     #9              ;MESSAGE
        JSR     PEM


        JSR     RTC_TOP_LOOP


        LDX     #14             ;DEFAULT
        JMP     PEM             ;AND RETURN

        BRK                     ; This code is never reached



; function RTC_WR_PROTECT
; input x (address) $07
; input y (value) $80
; uses A
;
; based on following algorithm
;
;  PROCEDURE rtc_wr_protect;
;  BEGIN
;    rtc_write(7,128);
;  END;

RTC_WR_PROTECT:
        LDX     #%00000111
        LDY     #%10000000
        JSR     RTC_WRITE
        RTS


; function RTC_WR_UNPROTECT
; input D (address) $07
; input E (value) $00
; uses A
;
; based on following algorithm
;
;  PROCEDURE rtc_wr_unprotect;
;  BEGIN
;    rtc_write(7,0);
;  END;

RTC_WR_UNPROTECT:
        LDX     #%00000111
        LDY     #%00000000
        JSR     RTC_WRITE
        RTS


; function RTC_GET_TIME
; input HL (memory address of buffer)
; uses A,C,D,E
;
; based on following algorithm
;
;  PROCEDURE rtc_get_time(var buf: string);
;   var
;     n  : int;
;  BEGIN
;    lock();
;    rtc_reset_off();
;    { Write command, burst read }
;    rtc_wr(255 - 64);
;    { Read seconds }
;    n := rtc_rd(); 0
;    buf[16] := char(((n / 16) and $07)) + '0';
;    buf[17] := char((n and $0f)) + '0';
;    { Read minutes }
;    n := rtc_rd(); 1
;    buf[13] := char(((n / 16) and $07)) + '0';
;    buf[14] := char((n and $0f)) + '0';
;    buf[15] := ':';
;    { Read hours }
;    n := rtc_rd(); 2
;    buf[10] := char(((n / 16) and $03)) + '0';
;    buf[11] := char((n and $0f)) + '0';
;    buf[12] := ':';
;    { Read date }
;    n := rtc_rd(); 3
;    buf[7] := char(((n / 16) and $03)) + '0';
;    buf[8] := char((n and $0f)) + '0';
;    buf[9] := ' ';
;    { Read month }
;    n := rtc_rd(); 4
;    buf[4] := char(((n / 16) and $03)) + '0';
;    buf[5] := char((n and $0f)) + '0';
;    buf[6] := '-';
;    { Read day }
;    n := rtc_rd(); 5
;    {
;    buf[4] := char(((n / 16) and $03)) + '0';
;    buf[4] := char((n and $0f)) + '0';
;    }
;    { Read year }
;    n := rtc_rd(); 6
;    buf[1] := char(((n / 16) and $0f)) + '0';
;    buf[2] := char((n and $0f)) + '0';
;    buf[3] := '-';
;    length(buf) := 17;
;    rtc_reset_on();
;    unlock();
;  END rtc_get_time;

RTC_GET_TIME:
;    { Read seconds }

        LDX     #$00
        JSR     RTC_READ
        TYA
        PHA
; digit 16
        CLC
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #$07
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+15

; digit 17
        PLA
        AND     #$0F
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+16

;    { Read minutes }

        LDX     #$01
        JSR     RTC_READ
        TYA
        PHA
; digit 13
        CLC
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #$07
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+12

; digit 14
        PLA
        AND     #$0F
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+13

; digit 15
        LDA     #':'
        STA     RTC_PRINT_BUFFER+14

;    { Read hours }
        LDX     #$02
        JSR     RTC_READ
        TYA

        PHA
; digit 10
        CLC
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #$03
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+09

; digit 11
        PLA
        AND     #$0F
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+10

; digit 12
        LDA     #':'
        STA     RTC_PRINT_BUFFER+11

;    { Read date }
        LDX     #$03
        JSR     RTC_READ
        TYA

        PHA
; digit 07
        CLC
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #$03
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+06

; digit 08
        PLA
        AND     #$0F
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+07

; digit 09
        LDA     #' '
        STA     RTC_PRINT_BUFFER+08

;    { Read month }
        LDX     #$04
        JSR     RTC_READ
        TYA

        PHA
; digit 04
        CLC
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #$03
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+03

; digit 05
        PLA
        AND     #$0F
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+04

; digit 06
        LDA     #'-'
        STA     RTC_PRINT_BUFFER+05


;    { Read year }
        LDX     #$06
        JSR     RTC_READ
        TYA

        PHA
; digit 01
        CLC
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #$0F
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+00

; digit 02
        PLA
        AND     #$0F
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+01

; digit 03
        LDA     #'-'
        STA     RTC_PRINT_BUFFER+02

        RTS                     ; Yes, end function and return


; function RTC_SET
; uses A, D, E
;
; based on following algorithm
;
;  { Set time to 96-02-18 19:43:00 }
;  PROCEDURE rtc_set_now;
;  BEGIN
;    rtc_wr_unprotect();
;    { Set seconds }
;    rtc_write(0,0);
;    { Set minutes }
;    rtc_write(1,$43);
;    { Set hours }
;    rtc_write(2,$19);
;    { Set date }
;    rtc_write(3,$18);
;    { Set month }
;    rtc_write(4,$02);
;    { Set day }
;    rtc_write(5,$07);
;    { Set year }
;    rtc_write(6,$96);
;    rtc_wr_protect();
;  END;

RTC_SET:

        JSR     RTC_WR_UNPROTECT
        LDA     #20
        STA     RTC_PRINT_BUFFER

; seconds
        LDX     #$00
        LDY     #$00
        JSR     RTC_WRITE


        LDA     #<RTC_TOP_LOOP1_SET_TIME
        LDY     #>RTC_TOP_LOOP1_SET_TIME
        LDX     #9              ;MESSAGE
        JSR     PEM

        LDA     #<RTC_PRINT_BUFFER
        LDY     #>RTC_PRINT_BUFFER
        LDX     #10             ;MESSAGE
        JSR     PEM

        LDA     RTC_PRINT_BUFFER+6
        AND     #$0F
        STA     TEMP
        LDA     RTC_PRINT_BUFFER+5
        AND     #$0F
        CLC
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        CLC
        ADC     TEMP
        TAY

        JSR     $F528

; minutes
        LDX     #$01
        JSR     RTC_WRITE

        LDA     RTC_PRINT_BUFFER+3
        AND     #$0F
        STA     TEMP
        LDA     RTC_PRINT_BUFFER+2
        AND     #$0F
        CLC
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        CLC
        ADC     TEMP
        TAY

; hours
        LDX     #$02
        JSR     RTC_WRITE




        LDA     #<RTC_TOP_LOOP1_OTHER2
        LDY     #>RTC_TOP_LOOP1_OTHER2
        LDX     #9              ;MESSAGE
        JSR     PEM

        LDA     #<RTC_TOP_LOOP1_SET_DATE
        LDY     #>RTC_TOP_LOOP1_SET_DATE
        LDX     #9              ;MESSAGE
        JSR     PEM

        LDA     #<RTC_PRINT_BUFFER
        LDY     #>RTC_PRINT_BUFFER
        LDX     #10             ;MESSAGE
        JSR     PEM


        LDA     #<RTC_TOP_LOOP1_OTHER2
        LDY     #>RTC_TOP_LOOP1_OTHER2
        LDX     #9              ;MESSAGE
        JSR     PEM


        LDA     RTC_PRINT_BUFFER+6
        AND     #$0F
        STA     TEMP
        LDA     RTC_PRINT_BUFFER+5
        AND     #$0F
        CLC
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        CLC
        ADC     TEMP
        TAY


; date
        LDX     #$03
        JSR     RTC_WRITE

        LDA     RTC_PRINT_BUFFER+3
        AND     #$0F
        STA     TEMP
        LDA     RTC_PRINT_BUFFER+2
        AND     #$0F
        CLC
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        CLC
        ADC     TEMP
        TAY


; month
        LDX     #$04
        LDY     #$02
        JSR     RTC_WRITE

; day
        LDX     #$05
        LDY     #$00
        JSR     RTC_WRITE

        LDA     RTC_PRINT_BUFFER+9
        AND     #$0F
        STA     TEMP
        LDA     RTC_PRINT_BUFFER+8
        AND     #$0F
        CLC
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        CLC
        ADC     TEMP
        TAY


; year
        LDX     #$06
        JSR     RTC_WRITE

        JSR     RTC_WR_PROTECT
        RTS


; function RTC_RESTART
;
; uses A, D, E,
;
; based on the following algorithm
;
;  { Restart clock, set seconds to 00 }
;  PROCEDURE rtc_restart;
;  BEGIN
;    rtc_wr_unprotect();
;    { Set seconds }
;    rtc_write(0,0);
;    rtc_wr_protect();
;  END;

RTC_RESTART:
        JSR     RTC_WR_UNPROTECT
        LDX     #$00
        LDY     #$00
        JSR     RTC_WRITE
        JSR     RTC_WR_PROTECT
        RTS


; function RTC_CHARGE_ENABLE
;
; uses A, D, E
;
; based on following algorithm
;
;  PROCEDURE rtc_charge_enable;
;  BEGIN
;    rtc_wr_unprotect();
;    { Enable trickle charger, 2kohm, 1 diode }
;    rtc_write(8,$a5);
;    rtc_wr_protect();
;  END;

RTC_CHARGE_ENABLE:
        JSR     RTC_WR_UNPROTECT
        LDX     #$08
        LDY     #$A5
        JSR     RTC_WRITE
        JSR     RTC_WR_PROTECT
        RTS


; function RTC_CHARGE_DISABLE
;
; uses A, D, E
;
; based on following algorithm
;
;  PROCEDURE rtc_charge_disable;
;  BEGIN
;    rtc_wr_unprotect();
;    { Disable trickle charger}
;    rtc_write(8,$00);
;    rtc_wr_protect();
;  END;

RTC_CHARGE_DISABLE:
        JSR     RTC_WR_UNPROTECT
        LDX     #$08
        LDY     #$00
        JSR     RTC_WRITE
        JSR     RTC_WR_PROTECT
        RTS


; function TEST_BIT_DELAY
;
; based on the following algorithm
;
;
;  PROCEDURE test_bit_delay();
;   var
;     i,t0,t1 : int;
;  BEGIN
;    putln("Testing bit delay...");
;    t0 := sys_time();
;    for i := 0 while i < 1000 do inc(i) loop
;      rtc_bit_delay();
;    end loop;
;    t1 := sys_time();
;    putln(i," rtc_bit_delay calls took ",t1-t0," ms.");
;  END;

RTC_TEST_BIT_DELAY:
        LDA     #<TESTING_BIT_DELAY_MSG
        LDY     #>TESTING_BIT_DELAY_MSG
        LDX     #9              ;MESSAGE
        JSR     PEM

; test should take approximately 43 seconds based on the following code analysis
; of Z80 T-states on a 4 MHz processor
; =(4+15*(7+255*(7+255*(17+144+4+10)+4+10)+10)+7)/4/1000000

        LDY     #$FF
PAUSE1:
        LDA     #$FF            ; ADJUST THE TIME 13h IS FOR 4 MHZ
        STA     TEMPPAUSE
PAUSE2:

        DEC     TEMPPAUSE       ; DEC COUNTER.
        BNE     PAUSE2          ; JUMP TO PAUSE2 IF A <> 0.
        DEY                     ; DEC COUNTER
        BNE     PAUSE1          ; JUMP TO PAUSE1 IF C <> 0.


        LDA     #<TESTING_BIT_DELAY_OVER
        LDY     #>TESTING_BIT_DELAY_OVER
        LDX     #9              ;MESSAGE
        JSR     PEM
        RTS


; function RTC_HELP
;
; based on following algorithm
;
;  PROCEDURE help();
;  BEGIN
;    putln();
;    putln("rtc: ",version);
;    putln("rtc: Commands: (E)xit (T)ime st(A)rt (S)et (R)aw (L)oop (C)harge (N)ocharge (H)elp");
;  END;

RTC_HELP:
        LDA     #<RTC_HELP_MSG
        LDY     #>RTC_HELP_MSG
        LDX     #9              ;MESSAGE
        JSR     PEM
        RTS


; function RTC_TOP_LOOP
;
; based on following algorithm
;
;  PROCEDURE toploop();
;   var
;     err,i,n,fd  : int;
;  BEGIN
;    putln();
;    help();
;    rtc_reset_on();
;    hold(100);
;    test_bit_delay();
;    rtc_charge_disable();
;    putln("rtc: trickle charger disabled.");
;    loop
;       put("rtc>");
;       gets(line);
;       if line = "exit" then
;          putln("Bye.");
;          exit(0);
;       elsif line = "charge" then
;          putln("Trickle charger enabled.");
;          rtc_charge_enable();
;       elsif line = "nocharge" then
;          putln("Trickle charger disabled.");
;          rtc_charge_disable();
;       elsif line = "start" then
;          rtc_restart();
;          putln("Restarting RTC");
;       elsif line = "t" then
;          rtc_get_time(line);
;          putln("Current time: ",line);
;       elsif line = "raw" then
;          putln();
;          putln("Raw read loop, hit any key to stop...");
;          while read(0,@n,1 + RD_NOWAIT) = 0 loop
;             put(#13,"sec=",hexstr(rtc_read(0))^);
;             put(" min=",hexstr(rtc_read(1))^);
;             hold(500);
;          end loop;
;       elsif line = "loop" then
;          putln();
;          putln("Clock loop, hit any key to stop...");
;          while read(0,@n,1 + RD_NOWAIT) = 0 loop
;             rtc_get_time(line);
;             put(#13,line);
;             hold(200);
;          end loop;
;       elsif line = "set" then
;          putln("Setting RTC time to 96-02-18 19:43:00");
;          rtc_set_now();
;       elsif (line = "help") or (line = "?") then
;          help();
;       elsif length(line) <> 0 then
;          putln("You typed: """,line,"""");
;       end;
;    end loop;
;  END toploop;

RTC_TOP_LOOP:
        LDA     #<CRLF_MSG
        LDY     #>CRLF_MSG
        LDX     #9              ;MESSAGE
        JSR     PEM

        JSR     RTC_HELP

        LDA     #<RTC_TOP_LOOP1_MSG
        LDY     #>RTC_TOP_LOOP1_MSG
        LDX     #9              ;MESSAGE
        JSR     PEM


RTC_TOP_LOOP_1:
        LDA     #<RTC_TOP_LOOP1_PROMPT
        LDY     #>RTC_TOP_LOOP1_PROMPT
        LDX     #9              ;MESSAGE
        JSR     PEM


        LDX     #01             ; CP/M console input call
        JSR     PEM

        AND     #%01011111      ; handle lower case responses to menu

        CMP     #'E'
        BEQ     JRTC_TOP_LOOP_EXIT

        CMP     #'C'
        BEQ     JRTC_TOP_LOOP_CHARGE

        CMP     #'N'
        BEQ     JRTC_TOP_LOOP_NOCHARGE

        CMP     #'A'
        BEQ     JRTC_TOP_LOOP_START

        CMP     #'T'
        BEQ     JRTC_TOP_LOOP_TIME

        CMP     #'R'
        BEQ     JRTC_TOP_LOOP_RAW

        CMP     #'L'
        BEQ     JRTC_TOP_LOOP_LOOP

        CMP     #'H'
        BEQ     JRTC_TOP_LOOP_HELP

        CMP     #'D'
        BEQ     JRTC_TOP_LOOP_DELAY

        CMP     #'S'
        BEQ     JRTC_TOP_LOOP_SET

        PHA
        LDA     #<RTC_TOP_LOOP1_OTHER1
        LDY     #>RTC_TOP_LOOP1_OTHER1
        LDX     #9              ;MESSAGE
        JSR     PEM

        PLA
        LDX     #02             ; CP/M Console output call
        JSR     PEM

        LDA     #<RTC_TOP_LOOP1_OTHER2
        LDY     #>RTC_TOP_LOOP1_OTHER2
        LDX     #9              ;MESSAGE
        JSR     PEM

        JMP     RTC_TOP_LOOP_1


JRTC_TOP_LOOP_EXIT:
        JMP     $0100
JRTC_TOP_LOOP_CHARGE:
        JMP     RTC_TOP_LOOP_CHARGE
JRTC_TOP_LOOP_NOCHARGE:
        JMP     RTC_TOP_LOOP_NOCHARGE
JRTC_TOP_LOOP_START:
        JMP     RTC_TOP_LOOP_START
JRTC_TOP_LOOP_TIME:
        JMP     RTC_TOP_LOOP_TIME
JRTC_TOP_LOOP_RAW:
        JMP     RTC_TOP_LOOP_RAW
JRTC_TOP_LOOP_LOOP:
        JMP     RTC_TOP_LOOP_LOOP
JRTC_TOP_LOOP_HELP:
        JMP     RTC_TOP_LOOP_HELP
JRTC_TOP_LOOP_DELAY:
        JMP     RTC_TOP_LOOP_DELAY
JRTC_TOP_LOOP_SET:
        JMP     RTC_TOP_LOOP_SET



RTC_TOP_LOOP_CHARGE:
        LDA     #<RTC_TOP_LOOP1_CHARGE
        LDY     #>RTC_TOP_LOOP1_CHARGE
        LDX     #9              ;MESSAGE
        JSR     PEM
        JSR     RTC_CHARGE_ENABLE
        JMP     RTC_TOP_LOOP_1

RTC_TOP_LOOP_NOCHARGE:
        LDA     #<RTC_TOP_LOOP1_NOCHARGE
        LDY     #>RTC_TOP_LOOP1_NOCHARGE
        LDX     #9              ;MESSAGE
        JSR     PEM
        JSR     RTC_CHARGE_DISABLE
        JMP     RTC_TOP_LOOP_1

RTC_TOP_LOOP_START:
        LDA     #<RTC_TOP_LOOP1_START
        LDY     #>RTC_TOP_LOOP1_START
        LDX     #9              ;MESSAGE
        JSR     PEM
        JSR     RTC_RESTART
        JMP     RTC_TOP_LOOP_1

RTC_TOP_LOOP_TIME:
        LDA     #<RTC_TOP_LOOP1_TIME
        LDY     #>RTC_TOP_LOOP1_TIME
        LDX     #9              ;MESSAGE
        JSR     PEM
        JSR     RTC_GET_TIME
        LDA     #<RTC_PRINT_BUFFER
        LDY     #>RTC_PRINT_BUFFER
        LDX     #9              ;MESSAGE
        JSR     PEM
        JMP     RTC_TOP_LOOP_1

RTC_TOP_LOOP_RAW:
        LDA     #<RTC_TOP_LOOP1_RAW
        LDY     #>RTC_TOP_LOOP1_RAW
        LDX     #9              ;MESSAGE
        JSR     PEM

RTC_TOP_LOOP_RAW1:

;	{ Read seconds }
        LDX     #$00            ; seconds register in DS1302
        JSR     RTC_READ        ; read value from DS1302, value is in Reg C
        TYA
        PHA
; digit 16
        CLC
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #$07
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+15

; digit 17
        PLA
        AND     #$0F
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+16

;	{ Read minutes }

        LDX     #$01            ; minutes register in DS1302
        JSR     RTC_READ        ; read value from DS1302, value is in Reg C
        TYA
        PHA
; digit 13
        CLC
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        AND     #$07
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+12

; digit 14
        PLA
        AND     #$0F
        CLC
        ADC     #'0'
        STA     RTC_PRINT_BUFFER+13

; digit 15
        LDA     #':'
        STA     RTC_PRINT_BUFFER+14

; digits 1-12 and 18-20 are spaces
        LDA     #' '            ; space
        STA     RTC_PRINT_BUFFER+19
        STA     RTC_PRINT_BUFFER+18
        STA     RTC_PRINT_BUFFER+17
        STA     RTC_PRINT_BUFFER+11
        STA     RTC_PRINT_BUFFER+10
        STA     RTC_PRINT_BUFFER+09
        STA     RTC_PRINT_BUFFER+08
        STA     RTC_PRINT_BUFFER+07
        STA     RTC_PRINT_BUFFER+06
        STA     RTC_PRINT_BUFFER+05
        STA     RTC_PRINT_BUFFER+04
        STA     RTC_PRINT_BUFFER+03
        STA     RTC_PRINT_BUFFER+02
        STA     RTC_PRINT_BUFFER+01
        STA     RTC_PRINT_BUFFER+00

        LDA     #<RTC_PRINT_BUFFER
        LDY     #>RTC_PRINT_BUFFER
        LDX     #9              ;MESSAGE
        JSR     PEM

        LDX     #01             ; CP/M console input call
        JSR     PEM

        CMP     #' '            ; space
        BEQ     RTC_TOP_LOOP_RAW1

        JMP     RTC_TOP_LOOP_1

RTC_TOP_LOOP_LOOP:
        LDA     #<RTC_TOP_LOOP1_LOOP
        LDY     #>RTC_TOP_LOOP1_LOOP
        LDX     #9              ;MESSAGE
        JSR     PEM


RTC_TOP_LOOP_LOOP1:
        JSR     RTC_GET_TIME

        LDA     #<RTC_PRINT_BUFFER;SEND
        LDY     #>RTC_PRINT_BUFFER
        LDX     #9              ;MESSAGE
        JSR     PEM

        LDX     #01             ; CP/M console input call
        JSR     PEM

        CMP     #' '
        JMP     RTC_TOP_LOOP_LOOP1

        JMP     RTC_TOP_LOOP_1

RTC_TOP_LOOP_SET:
        JSR     RTC_SET
        JMP     RTC_TOP_LOOP_1

RTC_TOP_LOOP_DELAY:
        JSR     RTC_TEST_BIT_DELAY
        JMP     RTC_TOP_LOOP_1

RTC_TOP_LOOP_HELP:
        JSR     RTC_HELP
        JMP     RTC_TOP_LOOP_1

;
; Text Strings
;
MSG:
        .BYTE   "Start Real Time Clock Program"
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; Line terminator

CRLF_MSG:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; Line terminator

TESTING_BIT_DELAY_MSG:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "Testing bit delay.  Successful test is approximately xx seconds."
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "Start clock and press space bar to continue."
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; Line terminator

TESTING_BIT_DELAY_OVER:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "Test complete.  Stop clock."
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; Line terminator

RTC_HELP_MSG:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "RTC: Version 1.0"
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "RTC: Commands: (E)xit (T)ime st(A)rt (S)et (R)aw (L)oop (C)harge"
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "               (N)ocharge (D)elay (H)elp"
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; Line terminator

RTC_TOP_LOOP1_MSG:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "RTC: trickle charger disabled."
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; Line terminator

RTC_TOP_LOOP1_PROMPT:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "RTC>"
        .BYTE   "$"             ; Line terminator

RTC_TOP_LOOP1_CHARGE:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "Trickle charger enabled."
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; Line terminator

RTC_TOP_LOOP1_NOCHARGE:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "Trickle charger disabled."
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; Line terminator

RTC_TOP_LOOP1_START:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "Restarting RTC."
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; Line terminator

RTC_TOP_LOOP1_TIME:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "Current time: "
        .BYTE   "$"             ; Line terminator

RTC_TOP_LOOP1_RAW:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "Raw read Loop.  Press SPACE BAR for next or any other key to stop."
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; Line terminator

RTC_TOP_LOOP1_LOOP:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "Clock Loop.  Press SPACE BAR for next or any other key to stop."
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; Line terminator

RTC_TOP_LOOP1_SET_TIME:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "TIME (HH:MM)>"
        .BYTE   "$"             ; Line terminator

RTC_TOP_LOOP1_SET_DATE:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "DATE (MM/DD/YY)>"
        .BYTE   "$"             ; Line terminator


RTC_TOP_LOOP1_OTHER1:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "YOU TYPED: "
        .BYTE   "$"             ; Line terminator

RTC_TOP_LOOP1_OTHER2:
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; Line terminator

RTC_PRINT_BUFFER:
        .BYTE   "                     "; Buffer for formatted date & time to print
        .BYTE   $0A, $0D        ; line feed and carriage return
        .BYTE   "$"             ; line terminator


.end
