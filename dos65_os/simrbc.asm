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
        .BYTE   cr, lf

        .BYTE   "d8888b.  .d88b.  .d8888.    dD     ooooo", cr, lf
        .BYTE   "88  `8D .8P  Y8. 88'  YP   d8'    8P~~~~", cr, lf
        .BYTE   "88   88 88    88 `8bo.    d8'    dP", cr, lf
        .BYTE   "88   88 88    88   `Y8b. d8888b. V8888b.", cr, lf,0
opnmsg1:
        .BYTE   "88  .8D `8b  d8' db   8D 88' `8D     `8D ", cr, lf
        .BYTE   "Y8888D'  `Y88P'  `8888Y' `8888P  88oobY'", cr, lf
        .BYTE   17, "DOS / 65 V3.00", cr, lf, 0

DSKYMSG:
        .BYTE   $54, $6E, $5C, $5E, $6E, $54, $79, $40

;cold entry from loader
boot:
        SEI
        LDX     #$ff            ;set stack
        TXS                     ;pointer
        CLD                     ;set binary mode

        LDA     #<opnmsg        ;point to message
        LDY     #>opnmsg
        JSR     outmsg          ;send it
        LDA     #<opnmsg1       ;point to message
        LDY     #>opnmsg1
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

        LDA     #40             ; DSKY INITIALIZE
        STA     farfunct
        JSR     DO_FARCALL

        LDA     #52             ; RTC_RESET
        STA     farfunct
        JSR     DO_FARCALL

        LDA     #60             ; IDE INITIALIZE
        STA     farfunct
        JSR     DO_FARCALL

        LDA     #63             ; SD INITIALIZE
        STA     farfunct
        JSR     DO_FARCALL

        LDA     #66             ; FLOPPY INITIALIZE
        STA     farfunct
        JSR     DO_FARCALL

        LDA     #<cnstxt        ; STORE POINTER TO COMMAND LINE
        STA     cmdlnp
        LDA     #>cnstxt
        STA     cmdlnp + 1

        LDA     #<dskcfg        ; STORE POINTER TO DISK CONFIG TABLE FOR APPS
        STA     dskcfpc
        LDA     #>dskcfg
        STA     dskcfpc + 1

        LDA     #<dcba          ; STORE POINTER TO DCB TABLES FOR APPS
        STA     dcbpc
        LDA     #>dcba
        STA     dcbpc + 1




setup:
;        LDX     #0              ;clear index
;first clear key dba variables
;        STX     hstact          ;host buffer inactive
;        STX     unacnt          ;clear unalloc count
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

        LDX     #$00            ; SHOW A STARTUP MESSAGE ON DSKY
:
        LDA     DSKYMSG,x
        STA     DSKY_BUF,x
        INX
        CPX     #8
        BNE     :-
        LDA     #41             ; DSKY_SHOW
        STA     farfunct
        JSR     DO_FARCALL

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
; PERFORM DOS / 65 SECTOR READ
;________________________________________________________________________________________________________
read:
        JSR     GET_DRIVE_DEVICE;
        AND     #$F0            ; only want first nybble
        CMP     #$00
        BNE     :+              ; not SD drive
;SD
        JSR     CONVERT_SECTOR_LBA
        LDA     #64             ; sd read sector
        STA     farfunct
        JSR     DO_FARCALL
        JSR     DEBSECR
        RTS
:
        CMP     #$20
        BNE     :+              ; not floppy drive
;FD
        JSR     SETUP_FD_CHS
        LDA     #67             ; floppy read sector
        STA     farfunct
        JSR     DO_FARCALL
        JSR     DEBSECR
        RTS
:
        CMP     #$30
        BNE     :+              ; invalid drive
;PPIDE
        JSR     CONVERT_SECTOR_LBA
        LDA     #61             ; IDE_READ_SECTOR
        STA     farfunct
        JSR     DO_FARCALL
        JSR     DEBSECR
        RTS
:
        LDA     #$FF            ; signal error
        RTS                     ;


;__WRITE_________________________________________________________________________________________________
;
; PERFORM DOS / 65 SECTOR WRITE
;________________________________________________________________________________________________________
write:
        JSR     GET_DRIVE_DEVICE;
        AND     #$F0            ; only want first nybble

        CMP     #$00
        BNE     :+              ; not SD drive
;SD
        JSR     CONVERT_SECTOR_LBA
        JSR     BLKSECR
        LDA     #65             ;PPP_WRITE_SECTOR
        STA     farfunct
        JSR     DO_FARCALL
        RTS
:
        CMP     #$20
        BNE     :+              ; not floppy drive
;FD
        JSR     SETUP_FD_CHS
        JSR     BLKSECR
        LDA     #68             ; floppy write sector
        STA     farfunct
        JSR     DO_FARCALL
        RTS
:
        CMP     #$30
        BNE     :+              ; invalid drive
;PPIDE
        JSR     CONVERT_SECTOR_LBA
        JSR     BLKSECR
        LDA     #62             ; IDE_WRITE_SECTOR
        STA     farfunct
        JSR     DO_FARCALL
        RTS
:
        LDA     #$FF            ; signal error
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
        LDA     #03
        STA     farfunct
        JMP     DO_FARCALL

;__CONRDE________________________________________________________________________________________________
;
; 	PERFORM DOS/65 CONSOLE READ
;________________________________________________________________________________________________________
conrde:
        LDA     #02
        STA     farfunct
        JMP     DO_FARCALL      ;console read

;__CONWRT________________________________________________________________________________________________
;
; 	PERFORM DOS/65 CONSOLE WRITE
;________________________________________________________________________________________________________
conwrt:
        PHA
        LDA     #00
        STA     farfunct
        PLA
        JMP     DO_FARCALL

prnwrt:
        RTS                     ;printer
punwrt:
        RTS                     ;punch output
rdrinp:
        RTS                     ;reader input
rdtime:
        LDX     #128
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


;___CONVERT_SECTOR_LBA___________________________________________________________________________________
;
; 	TRANSLATE LBA SECTORS
;________________________________________________________________________________________________________
CONVERT_SECTOR_LBA:
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
;	ADD SLICE OFFSET
        LDA     sekdsk          ; GET DRIVE#
        AND     #7              ; ONLY FIRST 8 DEVICES SUPPORTED
        ASL     a               ; DOUBLE NUMBER FOR TABLE LOOKUP
        TAX                     ; MOVE TO X REGISTER
        INX                     ; WANT SECOND BYTE OF ENTRY
        LDA     dskcfg,X        ; GET SLICE#
        STA     slicetmp+1      ; SLICE OFFSET MSB
        LDA     #0              ; GET SLICE#
        STA     slicetmp        ; SLICE OFFSET LSB
        CLC                     ; VOODOO MATH TO TAKE SLICE*$4000
        ROR     slicetmp+1
        ROR     slicetmp
        ROR     slicetmp+1
        ROR     slicetmp

        LDA     dskcfg,X        ; GET SLICE#
        CLC
        ADC     slicetmp
        STA     slicetmp
        LDA     #$00            ; LOGIC ERROR FOR SLICES THAT CARRY?
        ADC     slicetmp+1      ;
        STA     slicetmp+1      ;

; ADD SLICE OFFSET TO TRACK #
        CLC                     ; clear carry
        LDA     slicetmp
        ADC     debcyll
        STA     debcyll         ; store sum of LSBs
        LDA     slicetmp+1
        ADC     debcylm         ; add the MSBs using carry from
        STA     debcylm         ; the previous calculation


; DISPLAY ON DSKY IF PRESENT
        LDA     sekdsk
        STA     DSKY_HEXBUF
        LDA     debcylm
        STA     DSKY_HEXBUF+1
        LDA     debcyll
        STA     DSKY_HEXBUF+2
        LDA     debsehd
        STA     DSKY_HEXBUF+3
        LDA     #42             ; DSKY_BIN2SEG
        STA     farfunct
        JSR     DO_FARCALL
        LDA     #41             ; DSKY_SHOW
        STA     farfunct
        JSR     DO_FARCALL
        RTS

;__SETUP_FD_CHS__________________________________________________________________________________________________________________
;
; 	TRANSFORM DOS65 CHS TO FLOPPY
;________________________________________________________________________________________________________________________________
;
SETUP_FD_CHS:
        LDA     sektrk          ; LOAD TRACK # (LOW BYTE)
        AND     #$01            ; FILTER OUT HEAD
        STA     debcylm         ; STORE HEAD
        LDA     sektrk          ; SAVE TRACK IN A
        LSR     A               ; REMOVE HEAD BIT
        STA     debcyll         ; STORE IN TRACK
        LDA     seksec          ; LOAD SECTOR # (LOW BYTE)
        LSR     A               ;
        LSR     A               ; DIVIDE BY 4 (FOR BLOCKING)
        STA     debsehd         ; STORE IN SECTOR

        LDA     sekdsk
        STA     DSKY_HEXBUF
        LDA     debcylm
        STA     DSKY_HEXBUF+1
        LDA     debcyll
        STA     DSKY_HEXBUF+2
        LDA     debsehd
        STA     DSKY_HEXBUF+3
        LDA     #42             ; DSKY_BIN2SEG
        STA     farfunct
        JSR     DO_FARCALL
        LDA     #41             ; DSKY_SHOW
        STA     farfunct
        JSR     DO_FARCALL
        RTS


;___DEBSECR______________________________________________________________________________________________
;
;	DEBLOCK 512 BYTE SECTOR FOR DOS/65
;
;________________________________________________________________________________________________________
DEBSECR:
        PHA
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
        PLA
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

;___GET_DRIVE_DEVICE_____________________________________________________________________________________
;
; GET SELECTED DEVICE TYPE AND UNIT, RETURN IN "A"
;
;________________________________________________________________________________________________________
GET_DRIVE_DEVICE:
        STX     GET_DRIVE_DEVICE_TMP
        LDA     sekdsk          ; GET DRIVE
        AND     #7              ; ONLY FIRST 8 DEVICES SUPPORTED
        ASL     a               ; DOUBLE NUMBER FOR TABLE LOOKUP
        TAX                     ; MOVE TO X REGISTER
        LDA     dskcfg, X       ; GET device
        AND     #$0F
        STA     CURRENT_IDE_DRIVE
        LDA     dskcfg, X       ; GET device
; SETUP FLOPPY CONTROL WHILE WE ARE HERE
        AND     #$01
        STA     DSKUNIT
        LDA     dskcfg, X       ; GET device
        LDX     GET_DRIVE_DEVICE_TMP
        RTS

GET_DRIVE_DEVICE_TMP:
        .BYTE   00
;------------------------------------------------------------------------------------


;disk control blocks
dcba:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpa           ;address of map for a
        .BYTE   128             ;no checksums
        .WORD   ckmp            ;checksum map
dcbb:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpb           ;address of map for a
        .BYTE   128             ;no checksums
        .WORD   ckmp            ;checksum map
dcbc:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpc           ;address of map for a
        .BYTE   128             ;no checksums
        .WORD   ckmp            ;checksum map
dcbd:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpd           ;address of map for a
        .BYTE   128             ;no checksums
        .WORD   ckmp            ;checksum map
dcbe:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpe           ;address of map for a
        .BYTE   128             ;no checksums
        .WORD   ckmp            ;checksum map
dcbf:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpf           ;address of map for a
        .BYTE   128             ;no checksums
        .WORD   ckmp            ;checksum map
dcbg:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almpg           ;address of map for a
        .BYTE   128             ;no checksums
        .WORD   ckmp            ;checksum map
dcbh:
        .WORD   2047            ;max block number
        .WORD   64              ;sectors per track
        .WORD   16              ;number system tracks
        .BYTE   2               ;block size = 4096
        .WORD   511             ;max directory number
        .WORD   almph           ;address of map for a
        .BYTE   128             ;no checksums
        .WORD   ckmp            ;checksum map



;allocation maps
almpa:
        .RES    256
almpb:
        .RES    256
almpc:
        .RES    256
almpd:
        .RES    256
almpe:
        .RES    256
almpf:
        .RES    256
almpg:
        .RES    256
almph:
        .RES    256

;checksum maps
;drive a
ckmp:
        .RES    128


; See Platform Documentation for Drive Types.

dftdskcfg:
        .BYTE   $00, $00        ; disk A: unit, slice (invalid for floppy disks)
        .BYTE   $30, $00        ; disk B: unit, slice (invalid for floppy disks)
        .BYTE   $90, $00        ; disk C: unit, slice (invalid for floppy disks)
        .BYTE   $90, $00        ; disk D: unit, slice (invalid for floppy disks)
        .BYTE   $90, $00        ; disk E: unit, slice (invalid for floppy disks)
        .BYTE   $90, $00        ; disk F: unit, slice (invalid for floppy disks)
        .BYTE   $90, $00        ; disk G: unit, slice (invalid for floppy disks)
        .BYTE   $90, $00        ; disk H: unit, slice (invalid for floppy disks)
