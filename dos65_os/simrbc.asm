;--------------------------------
;dos/65 system interface module (sim)
;--------------------------------


;dos/65 system interface module (sim)
;version 3.00
;this version is designed to work with the 6x0x

;fixed parameters
simstart:

;main program
;jump vector used by pem
sim:
        JMP     boot            ;from cold start
wboote:
        JMP     wboot           ;from warm boot
        JMP     consts          ;check for input
        JMP     conrde          ;get input
        JMP     conwrt          ;send to terminal
        JMP     prnwrt          ;printer output
        JMP     punwrt          ;punch output
        JMP     rdrinp          ;reader input
        JMP     home            ;home drive
        JMP     seldsk          ;select disk
        JMP     seltrk          ;set track
        JMP     selsec          ;set sector
        JMP     setdma          ;set buffer address
        JMP     read            ;read sector
        JMP     write           ;write sector
        LDA     #1              ;printer always ready
        RTS
        JMP     rdtime          ;clock entry
        JMP     xlate           ;translate

;console definition block
sysdef:
        .BYTE   8               ;backspace
        .BYTE   1               ;clear to end of line
        .BYTE   $c              ;forward space
        .BYTE   0               ;normal video
        .BYTE   '^'             ;invert video
        .BYTE   24              ;lines per screen
        .BYTE   80              ;char per line
        .BYTE   $c              ;formfeed
        .BYTE   $1e             ;home
        .BYTE   2               ;clear to end of screen

;opening id message
opnmsg:
        .BYTE   cr,lf,"DOS/65 ON THE RBC 3.00",0


;cold entry from loader
boot:
        SEI
        LDX     #$ff            ;set stack
        TXS                     ;pointer
        CLD                     ;set binary mode

        LDA     #<opnmsg        ;point to message
        LDY     #>opnmsg
        JSR     outmsg          ;send it
;set up jumps into dos/65 in page one

; setup diskconfig table
        LDX     #0
:
        LDA     dftdskcfg, x
        STA     dskcfg, x
        INX
        CPX     #$10
        BNE     :-

setup:
        LDX     #0              ;clear index
;first clear key dba variables
        STX     hstact          ;host buffer inactive
        STX     unacnt          ;clear unalloc count
setupl:
        LDA     inttbl,x        ;get byte
        STA     $100,x          ;insert at start
        INX
        CPX     #6
        BNE     setupl          ;loop until done
        LDA     #<dflbuf        ;get low buffer
        LDY     #>dflbuf        ;and high
        JSR     setdma          ;and set
        LDA     sekdsk          ;get disk

;            JSR     SETUPDRIVE
        JSR     PPP_SOFT_RESET
;    JSR     IDE_SOFT_RESET

;.IF     USEDSKY=1
;    JSR     DSKYINIT
;    JSR     SEGDISPLAY
;.ENDIF

        LDA     #DEFDRV         ;set zero
        JSR     seldsk          ;and select drive zero
        JSR     home            ;home that drive

        JMP     ccm             ;and go to ccm
;initialization table
inttbl:
        .BYTE   $4c,<wboote,>wboote,$4c,<pem,>pem
;warm boot-read dos/65 back except sim and then
; jump to ccm.


wboot:
        SEI
        LDX     #$ff            ;set stack
        TXS                     ;pointer
        CLD                     ;set binary mode

        JMP     setup           ;go setup



;__SELDSK_________________________________________________________________________________________________
;
; 	PERFORM DOS/65 DISK DRIVE SELECT
;________________________________________________________________________________________________________
;select disk
seldsk:
        AND     #7              ;eight drives only
        STA     sekdsk          ;save for later
        ASL     a               ;multiply by two
        TAX                     ;make an Index
        LDA     dcbtbl,x        ;get address
        LDY     dcbtbl+1,x
        RTS

;table of dcb addresses
dcbtbl:
        .WORD   dcba
        .WORD   dcbb
        .WORD   dcbc
        .WORD   dcbd
        .WORD   dcbe
        .WORD   dcbf
        .WORD   dcbg
        .WORD   dcbh



;__HOME__________________________________________________________________________________________________
;
; 	PERFORM DOS/65 HEAD HOME
;________________________________________________________________________________________________________
home:
        LDA     #$00
        LDY     #$00

;__SELTRK________________________________________________________________________________________________
;
; 	PERFORM DOS/65 SELECT TRACK
;
;	A=TRACK LOW BYTE
;	Y=TRACK HIGH BYTE
;________________________________________________________________________________________________________
seltrk:
        STA     sektrk          ;save number
        STY     sektrk+1
        RTS

;__SELSEC________________________________________________________________________________________________
;
; 	PERFORM DOS/65 SECTOR SELECT
;
;	A=SECTOR LOW BYTE
;	Y=SECTOR HIGH BYTE
;________________________________________________________________________________________________________
selsec:
        STA     seksec          ;save low and high
        STY     seksec+1
        RTS

;__READ__________________________________________________________________________________________________
;
; 	PERFORM DOS/65 SECTOR READ
;________________________________________________________________________________________________________
read:

        JSR     CONVERT_SECTOR_DOS;
        JSR     READlow         ;
        CMP     #$00            ;
        BNE     RDABEND         ;
        JSR     DEBSECR         ;
        LDA     #$00            ;
        RTS                     ;
RDABEND:                        ;
        LDA     #$FF            ;
        RTS                     ;

;__READlow_______________________________________________________________________________________________
;
; 	PERFORM LOW LEVEL SECTOR READ
;________________________________________________________________________________________________________
READlow:
        LDA     sekdsk          ; GET DRIVE
        CMP     #$00            ;
        BNE     RDNOTA          ;

		LDA #$0D
		JSR $FD06
		LDA #$0A
		JSR $FD06
		LDA 	$0512
		JSR 	$F92A
		LDA 	$0510
		JSR 	$F92A
		LDA 	$0511
		JSR 	$F92A
		LDA #':'
		JSR $FD06
		LDA 	$0515
		JSR 	$F92A
		LDA 	$0513
		JSR 	$F92A
		LDA 	$0514
		JSR 	$F92A

        JSR     PPP_READ_SECTOR
		RTS
RDNOTA: ;

RDlowABEND:                     ;
        LDA     #$FF            ;
        RTS                     ;



;__WRITE_________________________________________________________________________________________________
;
; 	PERFORM DOS/65 SECTOR WRITE
;________________________________________________________________________________________________________
write:
        JSR     CONVERT_SECTOR_DOS;
        JSR     READlow         ;
        CMP     #$00            ;
        BNE     WRABEND         ;
        JSR     BLKSECR         ;
;
        LDA     sekdsk          ; GET DRIVE
        CMP     #$00            ;
        BNE     WRNOTA          ;
;
        JSR     PPP_WRITE_SECTOR
        JMP     WREND           ;

        JMP     WRABEND         ;
;
WRNOTA: ;
        JMP     WRABEND         ;

WREND:  ;
        CMP     #$00            ;
        BNE     WRABEND         ;
        JSR     DEBSECR         ;
        LDA     #$00            ;
        RTS                     ;
WRABEND:                        ;
        LDA     #$FF            ;
        RTS                     ;

;__SETDMA________________________________________________________________________________________________
;
; 	PERFORM DOS/65 BUFFER ADDRESS SELECTION
;
;	A=BUFFER LOW BYTE
;	Y=BUFFER HIGH BYTE
;________________________________________________________________________________________________________
setdma:
        STA     dmaadr          ;store low
        STY     dmaadr+1        ;and high
        RTS


;__CONSTS________________________________________________________________________________________________
;
; 	GET DOS/65 CONSOLE STATUS
;________________________________________________________________________________________________________
consts:
        JMP     IOF_CONSTATUS

;__CONRDE________________________________________________________________________________________________
;
; 	PERFORM DOS/65 CONSOLE READ
;________________________________________________________________________________________________________
conrde:
        JMP     IOF_CONINW      ;console read

;__CONWRT________________________________________________________________________________________________
;
; 	PERFORM DOS/65 CONSOLE WRITE
;________________________________________________________________________________________________________
conwrt:
        JMP     IOF_OUTCH       ;console write

prnwrt:
        RTS                     ;printer
punwrt:
        RTS                     ;punch output
rdrinp:
        RTS                     ;reader input
rdtime:
;            JSR     DOSREADRTC
        RTS                     ;read clock
xlate:
        RTS                     ;sector translate


;__OUTMSG________________________________________________________________________________________________
;
; 	WRITE A NULL TERMINATED STRING TO THE CONSOLE
;
;	A=POINTER LOW BYTE
;	Y=POINTER HIGH BYTE
;________________________________________________________________________________________________________
outmsg: ;output message
        STA     OUTMSG_W
        STY     OUTMSG_W+1
        LDY     #$00
OUTSTRLP:
        LDA     (OUTMSG_W),Y    ; LOAD NEXT CHAR FROM STRING INTO ACC
        CMP     #$00            ; IS NULL?
        BEQ     ENDOUTSTR       ; YES, END PRINT OUT
        JSR     conwrt          ; PRINT CHAR IN ACC
        INY                     ; Y=Y+1 (BUMP INDEX)
        JMP     OUTSTRLP        ; DO NEXT CHAR
ENDOUTSTR:
        RTS                     ; RETURN


;___CONVERT_SECTOR_DOS________________________________________________________________________________
;
; 	TRANSLATE SECTORS
;________________________________________________________________________________________________________
CONVERT_SECTOR_DOS:
;        LDA     sekdsk          ; GET DISK TYPE
;        CMP     #$02
;        BEQ     CONVERT_SECTOR_DOS1; NOT ZERO, DO FULL TRANSLATE
        LDA     sektrk          ; LOAD TRACK # (LOW BYTE)
        AND     #$0F            ; ISOLATE HEAD IN LOW 4 BITS
        ASL     a               ; MOVE TO HIGH BYTE
        ASL     a
        ASL     a
        ASL     a
        TAX                     ; PARK IN X
        LDA     seksec          ; LOAD SECTOR # (LOW BYTE)
        LSR     A               ;
        LSR     A               ; DIVIDE BY 4 (FOR BLOCKING)
        AND     #$0F            ; CLEAR UPPER 4 BITS (JUST 'CAUSE)
        STA     debsehd         ; STORE IN SECTOR/HEAD
        TXA                     ; GET HEAD BACK
        ORA     debsehd
        STA     debsehd         ; STORE IN SECTOR/HEAD

        LDA     sektrk
        STA     debcyll         ; STORE IN TRACK (lsb)
        LDA     sektrk+1
        STA     debcylm         ; STORE IN TRACK (msb)
; REMOVE HEAD FROM TRACK VALUE (DIV/4)
        LDA     debcylm
        LSR     A
        STA     debcylm
        LDA     debcyll
        ROR     A
        STA     debcyll

        LDA     debcylm
        LSR     A
        STA     debcylm
        LDA     debcyll
        ROR     A
        STA     debcyll

        LDA     debcylm
        LSR     A
        STA     debcylm
        LDA     debcyll
        ROR     A
        STA     debcyll

        LDA     debcylm
        LSR     A
        STA     debcylm
        LDA     debcyll
        ROR     A
        STA     debcyll

;   .IF     USEDSKY=1
;       LDA     sekdsk
;       STA     DSKYDISPLAY
;       LDA     debhead
;       STA     DSKYDISPLAY+1
;       LDA     debcyl
;       STA     DSKYDISPLAY+2
;       LDA     debsec
;       STA     DSKYDISPLAY+3
;       JSR     HEXDISPLAY
;   .ENDIF

        RTS

;CONVERT_SECTOR_DOS1:
;        LDA     sektrk          ; LOAD TRACK # (LOW BYTE)
;        STA     debtmp+1        ;
;        LDA     seksec          ; LOAD SECTOR# (LOW BYTE)
;        STA     debtmp          ;
;        JSR     RRA16           ; ROTATE DEBTMP RIGHT (DIVIDE BY 2)
;        JSR     RRA16           ; ROTATE DEBTMP RIGHT (DIVIDE BY 2)
;        LDA     sektrk+1        ; GET HIGH BYTE OF TRACK INTO A
;        ASL     A               ;
;        ASL     A
;        ASL     A
;        ASL     A
;        ASL     A               ;
;        ASL     A               ;
;        CLC
;        ORA     debtmp+1        ;
;        STA     debtmp+1        ;
;        LDA     sektrk+1        ; GET HIGH BYTE OF TRACK INTO A
;        LSR     A
;        LSR     A
;        STA     debhead         ;
;        LDA     debtmp          ;
;        STA     debsec          ; LBA REGISTER IS 00TTTTSS / 4
;        LDA     debtmp+1        ;
;        STA     debcyl          ;
; .IF     USEDSKY=1
;     LDA     sekdsk
;     STA     DSKYDISPLAY
;     LDA     debhead
;     STA     DSKYDISPLAY+1
;     LDA     debcyl
;     STA     DSKYDISPLAY+2
;     LDA     debsec
;     STA     DSKYDISPLAY+3
;     JSR     HEXDISPLAY
; .ENDIF
;        RTS
;RRA16:
;        CLC                     ; CLEAR CARRY FLAG
;       LDA     debtmp+1        ; 16 BIT ROTATE HL WITH CARRY
;      ROR     A               ;
;     STA     debtmp+1        ; ROTATE HL RIGHT 1 BIT (DIVIDE BY 2)
;    LDA     debtmp          ;
;    ROR     A               ;
;    STA     debtmp          ;
;   RTS

;___DEBSECR______________________________________________________________________________________________
;
;	DEBLOCK 512 BYTE SECTOR FOR DOS/65
;
;________________________________________________________________________________________________________
DEBSECR:
        LDA     seksec          ;
        AND     #$03            ; GET SECTOR INDEX
        CLC                     ;
        ROL     A               ;
        TAX                     ;
        LDA     DEBTAB,X        ;
        STA     SRC
        INX
        LDA     DEBTAB,X        ;
        STA     SRC+1           ;
        LDA     dmaadr          ;
        STA     DEST            ;
        LDA     dmaadr+1        ;
        STA     DEST+1          ;
        JSR     COPY_DOS_SECTOR ;
        RTS

DEBTAB:
        .WORD   hstbuf          ;
        .WORD   hstbuf+128      ;
        .WORD   hstbuf+256      ;
        .WORD   hstbuf+384      ;


;___BLKSECR______________________________________________________________________________________________
;
;	BLOCK 512 BYTE SECTOR FOR DOS/65
;
;________________________________________________________________________________________________________
BLKSECR:
        LDA     seksec          ;
        AND     #$03            ; GET SECTOR INDEX
        CLC                     ;
        ROL     A               ;
        TAX                     ;
        LDA     DEBTAB,X        ;
        STA     DEST
        INX
        LDA     DEBTAB,X        ;
        STA     DEST+1          ;
        LDA     dmaadr          ;
        STA     SRC             ;
        LDA     dmaadr+1        ;
        STA     SRC+1           ;
        JSR     COPY_DOS_SECTOR ;
        RTS


;___COPY_DOS_SECTOR______________________________________________________________________________________
;
;	COPY 128 BYTE SECTOR FOR DOS/65
;
;________________________________________________________________________________________________________
COPY_DOS_SECTOR:
        LDY     #$00            ;
COPY_DOS_SECTOR1:
        LDA     (SRC),Y         ;
        STA     (DEST),Y        ;
        INY                     ;
        TYA                     ;
        CMP     #$80            ;
        BNE     COPY_DOS_SECTOR1;
        RTS


;------------------------------------------------------------------------------------


;disk control blocks
dcba:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpa           ;address of map for a
        .BYTE   0               ;do checksums
        .WORD   ckmp            ;checksum map
dcbb:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpb           ;address of map for a
        .BYTE   0               ;do checksums
        .WORD   ckmp            ;checksum map
dcbc:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpc           ;address of map for a
        .BYTE   0               ;do checksums
        .WORD   ckmp            ;checksum map
dcbd:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpd           ;address of map for a
        .BYTE   0               ;do checksums
        .WORD   ckmp            ;checksum map
dcbe:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpe           ;address of map for a
        .BYTE   0               ;do checksums
        .WORD   ckmp            ;checksum map
dcbf:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpf           ;address of map for a
        .BYTE   0               ;do checksums
        .WORD   ckmp            ;checksum map
dcbg:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpg           ;address of map for a
        .BYTE   0               ;do checksums
        .WORD   ckmp            ;checksum map
dcbh:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almph           ;address of map for a
        .BYTE   0               ;do checksums
        .WORD   ckmp            ;checksum map

;data area


hstwrt:
        .BYTE   0               ;0=written,1=pending host write

;allocate the following data areas to unused ram space
LASTCHAR:
        .BYTE   0               ;save sector for warm boot
savsec:
        .BYTE   0               ;save sector for warm boot
count:
        .BYTE   0               ;counter in warm boot
temp:
        .BYTE   0               ;save hstdsk for warm boot
hstact:
        .BYTE   0               ;host active flag
unacnt:
        .BYTE   0               ;unalloc rec cnt
debhead:
        .BYTE   0               ; DEBLOCKED HEAD
debcyl:
        .BYTE   0               ; DEBLOCKED CYLINDER ID
debsec:
        .BYTE   0               ; DEBLOCKED SECTOR
debtmp:
        .WORD   0               ; DEBLOCK TEMP VAR
Cdebhead:
        .BYTE   $FF             ; DEBLOCKED HEAD
Cdebcyl:
        .BYTE   $FF             ; DEBLOCKED CYLINDER ID
Cdebsec:
        .BYTE   $FF             ; DEBLOCKED SECTOR
DEBDIRTY:
        .BYTE   0               ; DIRTY FLAG

;allocation maps
almpa:
        .RES    254
almpb:
        .RES    254
almpc:
        .RES    254
almpd:
        .RES    254
almpe:
        .RES    254
almpf:
        .RES    254
almpg:
        .RES    254
almph:
        .RES    254

;checksum maps
;drive a
ckmp:
        .RES    128

dftdskcfg:
        .BYTE   $00, $00        ; disk A: unit, slice (invalid for floppy and RAM disks)
        .BYTE   $00, $00        ; disk B: unit, slice (invalid for floppy and RAM disks)
        .BYTE   $00, $00        ; disk C: unit, slice
        .BYTE   $00, $00        ; disk D: unit, slice
        .BYTE   $00, $00        ; disk E: unit, slice
        .BYTE   $00, $00        ; disk F: unit, slice
        .BYTE   $00, $00        ; disk G: unit, slice
        .BYTE   $00, $00        ; disk H: unit, slice
