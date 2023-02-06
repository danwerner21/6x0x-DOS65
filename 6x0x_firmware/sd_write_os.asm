
;__sd_write_os________________________________________________________________________________________
;
;	WRITE OS ONTO BOOT TRACK OF SD
;
;	WRITTEN BY: DAN WERNER -- 1/1/2013
;	updated for new BIOS   -- 2/5/2023
;
;__________________________________________________________________________________________________
;
; BIOS JUMP TABLE
IOF_CONIN       = $FD00         ; read a byte from CONSOLE ('A' POINTS TO BYTE)
IOF_CONINW      = $FD03         ; read a byte from CONSOLE ('A' POINTS TO BYTE, WAIT FOR BYTE)
IOF_OUTCH       = $FD06         ; write a byte from CONSOLE  ('A' POINTS TO BYTE)
IOF_CONSTATUS   = $FD09         ; RETURN CONSOLE STATUS
SERIALINIT      = $FD0C         ; called during OS init
RDSER1          = $FD0F         ; read a byte from serial port ('A' POINTS TO BYTE)
WRSER1          = $FD12         ; write a byte from serial port  ('A' POINTS TO BYTE)
RDSER1W         = $FD15         ; read a byte from serial port ('A' POINTS TO BYTE, WAIT FOR INPUT)
SERIALSTATUS    = $FD18         ; GET UART STATUS
SETUPDRIVE      = $FD1B         ; init floppy drive
READFL          = $FD1E         ; read sector from floppy
WRITEFL         = $FD21         ; write sector to floppy
PPP_SOFT_RESET  = $FD24         ; reset ppp sd drive
PPP_READ_SECTOR = $FD27         ; read ppp sd drive sector
PPP_WRITE_SECTOR = $FD2A        ; write ppp sd drive sector
IDE_SOFT_RESET  = $FD2D         ; reset ide drive
IDE_READ_SECTOR = $FD30         ; ide read sector
IDE_WRITE_SECTOR = $FD33        ; ide write sector
LOADS19         = $FD33         ; load s19 from serial port into ram
PPP_INITIALIZE  = $FD39         ; Initialize/Detect SD card
IDE_INITIALIZE  = $FD3C         ; Initialize/Detect IDE

WORKPTR         = $32           ; WORK POINTER FOR COMMAND PROCESSOR
TEMPWORD        = $36           ;
TEMPWORD1       = $38           ;
COUNTER         = $45           ; COUNTER


sektrk          = $050C         ; seek track number
seksec          = $050E         ; seek sector number
debcyll         = $0510         ; DEBLOCKED CYLINDER LSB
debcylm         = $0511         ; DEBLOCKED CYLINDER MSB
debsehd         = $0512         ; DEBLOCKED SECTOR AND HEAD (HS)
sekdsk          = $0516         ; seek disk number
dskcfg          = $0517         ; 16 bytes disk configuration table
DSKUNIT         = $0528         ; seek disk number
slicetmp        = $0531         ; (word)
CURRENT_IDE_DRIVE = $0534

STARTADDRESS    = $B800         ; OS DEST ADDRESS

INBUFFER        = $0200         ; DISK BUFFER
OSENDPAGE       = $E0           ; stop loading when we hit this page
BOOTCODESTART   = $0800         ; LOCATION BOOT CODE WILL RUN IN


        .PC02
        .SEGMENT "TEA"
        .ORG    $0800

        LDA     #$00
        STA     debcylm         ; SET BOOT CODE LOCATION
        STA     debcyll         ; SET BOOT CODE LOCATION
        LDA     #$01            ;
        STA     debsehd         ; SET BOOT CODE LOCATION
;
        LDA     #<STARTADDRESS  ; SETUP OS LOAD LOCATION
        STA     TEMPWORD1       ;
        LDA     #>STARTADDRESS  ;
        STA     TEMPWORD1 +1    ;

BOOTLOOP:
        LDA     #<INBUFFER      ; SETUP DISK BUFFER
        STA     WORKPTR         ;
        LDA     #>INBUFFER      ;
        STA     WORKPTR +1      ;
;
        LDY     #$00            ;
:
        LDA     (TEMPWORD1),Y   ;
        STA     (WORKPTR),Y     ;
        INY                     ;
        BNE     :-              ;
        INC     TEMPWORD1+1     ;
        INC     WORKPTR+1       ;
:
        LDA     (TEMPWORD1),Y   ;
        STA     (WORKPTR),Y     ;
        INY                     ;
        BNE     :-              ;
        JSR     PPP_WRITE_SECTOR;
        INC     TEMPWORD1+1     ;
        LDA     TEMPWORD1+1     ;
        CMP     #OSENDPAGE
        BEQ     EXITBOOT        ;
        CMP     #OSENDPAGE+1
        BEQ     EXITBOOT        ;
        INC     debsehd         ;
        BNE     BOOTLOOP        ;

EXITBOOT:
        BRK


        .END
