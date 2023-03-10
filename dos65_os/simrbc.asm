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
        .BYTE   cr,lf,"DOS/65 ON THE 6x0x RBC",cr,lf,0

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
;set up jumps into dos/65 in page one

; setup diskconfig table
        LDX     #0
:
        LDA     dftdskcfg, x
        STA     dskcfg, x
        INX
        CPX     #$10
        BNE     :-

        JSR     PPP_INITIALIZE
        JSR     IDE_INITIALIZE


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

        JSR     DSKY_INIT
        LDX     #$00            ; SHOW A STARTUP MESSAGE ON DSKY
:
        LDA     DSKYMSG,x
        STA     DSKY_BUF,x
        INX
        CPX     #8
        BNE     :-
        JSR     DSKY_SHOW

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
        JSR     PPP_READ_SECTOR ; sd read sector
        JSR     DEBSECR
        RTS
:
        CMP     #$20
        BNE     :+              ; not floppy drive
;FD
        LDA     #$ff
        RTS                     ;
:
        CMP     #$30
        BNE     :+              ; invalid drive
;PPIDE
        JSR     CONVERT_SECTOR_LBA
        JSR     IDE_READ_SECTOR ; sd read sector
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
        JSR     PPP_WRITE_SECTOR; sd read sector
        RTS
        CMP     #$20
        BNE     :+              ; not floppy drive
;FD
        LDA     #$ff
        RTS                     ;
:
        CMP     #$30
        BNE     :+              ; invalid drive
;PPIDE
        JSR     CONVERT_SECTOR_LBA
        JSR     BLKSECR
        JSR     IDE_WRITE_SECTOR; sd read sector
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
        LDA     debhead
        STA     DSKY_HEXBUF+1
        LDA     debcyl
        STA     DSKY_HEXBUF+2
        LDA     debsec
        STA     DSKY_HEXBUF+3
        JSR     DSKY_BIN2SEG
        JSR     DSKY_SHOW       ; SHOW DSKY SEGMENTS

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
        PHX
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
        CMP     #$00
        BNE     :+
        LDA     #%00010000
        STA     DSKUNIT
        JMP     GET_DRIVE_DEVICE_1
:
        LDA     #%00100001
        STA     DSKUNIT
GET_DRIVE_DEVICE_1:
        LDA     dskcfg, X       ; GET device
        PLX
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


; $00 = sd card
; $20 = diskIO V3 Floppy card
; $30 = diskIO V3 IDE card

dftdskcfg:
        .BYTE   $00, $00        ; disk A: unit, slice (invalid for floppy disks)
        .BYTE   $30, $00        ; disk B: unit, slice (invalid for floppy disks)
        .BYTE   $90, $00        ; disk C: unit, slice (invalid for floppy disks)
        .BYTE   $90, $00        ; disk D: unit, slice (invalid for floppy disks)
        .BYTE   $90, $00        ; disk E: unit, slice (invalid for floppy disks)
        .BYTE   $90, $00        ; disk F: unit, slice (invalid for floppy disks)
        .BYTE   $90, $00        ; disk G: unit, slice (invalid for floppy disks)
        .BYTE   $90, $00        ; disk H: unit, slice (invalid for floppy disks)
