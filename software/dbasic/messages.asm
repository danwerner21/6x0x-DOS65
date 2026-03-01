

LAB_MSZM:
        .BYTE   $0D,$0A,"Memory size ",$00

LAB_SMSG:
        .BYTE   " Bytes free",$0D,$0A,$0A
        .BYTE   "Enhanced BASIC 2.22",$0A,$00

; BASIC messages, mostly error messages

LAB_BAER:
        .WORD   ERR_NF          ;$00 NEXT without FOR
        .WORD   ERR_SN          ;$02 syntax
        .WORD   ERR_RG          ;$04 RETURN without GOSUB
        .WORD   ERR_OD          ;$06 out of data
        .WORD   ERR_FC          ;$08 function call
        .WORD   ERR_OV          ;$0A overflow
        .WORD   ERR_OM          ;$0C out of memory
        .WORD   ERR_US          ;$0E undefined statement
        .WORD   ERR_BS          ;$10 array bounds
        .WORD   ERR_DD          ;$12 double dimension array
        .WORD   ERR_D0          ;$14 divide by 0
        .WORD   ERR_ID          ;$16 illegal direct
        .WORD   ERR_TM          ;$18 type mismatch
        .WORD   ERR_LS          ;$1A long string
        .WORD   ERR_ST          ;$1C string too complex
        .WORD   ERR_CN          ;$1E continue error
        .WORD   ERR_UF          ;$20 undefined function
        .WORD   ERR_LD          ;$22 LOOP without DO
        .WORD   ERR_IO          ;$24 IO ERROR
        .WORD   ERR_DR          ;$26 MISSING DRIVE ERROR

; I may implement these two errors to force definition of variables and
; dimensioning of arrays before use.

;	.word ERR_UV		;$24 undefined variable

; the above error has been tested and works (see code and comments below LAB_1D8B)

;	.word ERR_UA		;$26 undimensioned array

ERR_NF:
        .BYTE   "NEXT without FOR",$00
ERR_SN:
        .BYTE   "Syntax",$00
ERR_RG:
        .BYTE   "RETURN without GOSUB",$00
ERR_OD:
        .BYTE   "Out of DATA",$00
ERR_FC:
        .BYTE   "Function call",$00
ERR_OV:
        .BYTE   "Overflow",$00
ERR_OM:
        .BYTE   "Out of memory",$00
ERR_US:
        .BYTE   "Undefined statement",$00
ERR_BS:
        .BYTE   "Array bounds",$00
ERR_DD:
        .BYTE   "Double dimension",$00
ERR_D0:
        .BYTE   "Divide by zero",$00
ERR_ID:
        .BYTE   "Illegal direct",$00
ERR_TM:
        .BYTE   "Type mismatch",$00
ERR_LS:
        .BYTE   "String too long",$00
ERR_ST:
        .BYTE   "String too complex",$00
ERR_CN:
        .BYTE   "Can't continue",$00
ERR_UF:
        .BYTE   "Undefined function",$00
ERR_LD:
        .BYTE   "LOOP without DO",$00
ERR_IO:
        .BYTE   "I/O",$00
ERR_DR:
        .BYTE   "Drive missing",$00

;ERR_UV	.byte	"Undefined variable",$00

; the above error has been tested and works (see code and comments below LAB_1D8B)

;ERR_UA	.byte	"Undimensioned array",$00

LAB_BMSG:
        .BYTE   $0D,$0A,"Break",$00
LAB_EMSG:
        .BYTE   " Error",$00
LAB_LMSG:
        .BYTE   " in line ",$00
LAB_RMSG:
        .BYTE   $0D,$0A,"Ready",$0D,$0A,$00

LAB_IMSG:
        .BYTE   " Extra ignored",$0D,$0A,$00
LAB_REDO:
        .BYTE   " Redo from start",$0D,$0A,$00
