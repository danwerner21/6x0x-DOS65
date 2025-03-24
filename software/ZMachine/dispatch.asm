;        PAGE
;        SBTTL   "--- OPCODE DISPATCH TABLES ---"

; 0-OPS

OPT0:
        .WORD   ZRTRUE          ; 0
        .WORD   ZRFALS          ; 1
        .WORD   ZPRI            ; 2
        .WORD   ZPRR            ; 3
        .WORD   ZNOOP           ; 4
        .WORD   ZSAVE           ; 5
        .WORD   ZREST           ; 6
        .WORD   ZSTART          ; 7
        .WORD   ZRSTAK          ; 8
        .WORD   POPVAL          ; 9
        .WORD   ZQUIT           ; 10
        .WORD   ZCRLF           ; 11
        .WORD   ZUSL            ; 12
        .WORD   ZVER            ; 13

NOPS0           = 14            ; NUMBER OF 0-OPS

; 1-OPS

OPT1:
        .WORD   ZZERO           ; 0
        .WORD   ZNEXT           ; 1
        .WORD   ZFIRST          ; 2
        .WORD   ZLOC            ; 3
        .WORD   ZPTSIZ          ; 4
        .WORD   ZINC            ; 5
        .WORD   ZDEC            ; 6
        .WORD   ZPRB            ; 7
        .WORD   BADOP1          ; 8 (UNDEFINED)
        .WORD   ZREMOV          ; 9
        .WORD   ZPRD            ; 10
        .WORD   ZRET            ; 11
        .WORD   ZJUMP           ; 12
        .WORD   ZPRINT          ; 13
        .WORD   ZVALUE          ; 14
        .WORD   ZBCOM           ; 15

NOPS1           = 16            ; NUMBER OF 1-OPS

; 2-OPS

OPT2:
        .WORD   BADOP2          ; 0 (UNDEFINED)
        .WORD   ZEQUAL          ; 1
        .WORD   ZLESS           ; 2
        .WORD   ZGRTR           ; 3
        .WORD   ZDLESS          ; 4
        .WORD   ZIGRTR          ; 5
        .WORD   ZIN             ; 6
        .WORD   ZBTST           ; 7
        .WORD   ZBOR            ; 8
        .WORD   ZBAND           ; 9
        .WORD   ZFSETP          ; 10
        .WORD   ZFSET           ; 11
        .WORD   ZFCLR           ; 12
        .WORD   ZSET            ; 13
        .WORD   ZMOVE           ; 14
        .WORD   ZGET            ; 15
        .WORD   ZGETB           ; 16
        .WORD   ZGETP           ; 17
        .WORD   ZGETPT          ; 18
        .WORD   ZNEXTP          ; 19
        .WORD   ZADD            ; 20
        .WORD   ZSUB            ; 21
        .WORD   ZMUL            ; 22
        .WORD   ZDIV            ; 23
        .WORD   ZMOD            ; 24

NOPS2           = 25            ; NUMBER OF 2-OPS

; X-OPS

OPTX:
        .WORD   ZCALL           ; 0
        .WORD   ZPUT            ; 1
        .WORD   ZPUTB           ; 2
        .WORD   ZPUTP           ; 3
        .WORD   ZREAD           ; 4
        .WORD   ZPRC            ; 5
        .WORD   ZPRN            ; 6
        .WORD   ZRAND           ; 7
        .WORD   ZPUSH           ; 8
        .WORD   ZPOP            ; 9
        .WORD   ZSPLIT          ; 10
        .WORD   ZSCRN           ; 11

NOPSX           = 12            ; NUMBER OF X-OPS
