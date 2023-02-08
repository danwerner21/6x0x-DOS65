
;__WRITEOS_________________________________________________________________________________________
;
;	WRITE OS ONTO BOOT TRACK OF SELECTED DRIVE
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
LOADS19         = $FD36         ; load s19 from serial port into ram
PPP_INITIALIZE  = $FD39         ; Initialize/Detect SD card
IDE_INITIALIZE  = $FD3C         ; Initialize/Detect IDE

WORKPTR         = $32           ; WORK POINTER FOR COMMAND PROCESSOR
TEMPWORD        = $36           ;
TEMPWORD1       = $38           ;
COUNTER         = $45           ; COUNTER
PEM             = $103          ;PEM ENTRY
WARMBOOT        = $100          ;WARM BOOT

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
STARTOS         = $B800         ; OS START ADDRESS

INBUFFER        = $0200         ; DISK BUFFER
OSENDPAGE       = $E0           ; stop loading when we hit this page
BOOTCODESTART   = $0800         ; LOCATION BOOT CODE WILL RUN IN


        .PC02
        .SEGMENT "TEA"
        .ORG    $0800

        LDA     #<MSG
        LDY     #>MSG
        LDX     #9              ; intro message
        JSR     PEM

        JSR     IOF_CONINW

        CMP     #'I'
        BEQ     DO_IDE

        CMP     #'S'
        BEQ     DO_SD

        CMP     #'F'
        BEQ     DO_FL

; ABORT
        LDA     #<ABORTMSG
        LDY     #>ABORTMSG
        LDX     #9              ; intro message
        JSR     PEM
        JMP     WARMBOOT

DO_SD:
        LDA     #<SDMSG
        LDY     #>SDMSG
        LDX     #9              ; intro message
        JSR     PEM
        LDA     #$01
        STA     CURRENT_IDE_DRIVE
        JSR     PPP_SOFT_RESET
        JSR     sd_write_bootsector
        JMP     DO_OS_WRITE
DO_FL:
        LDA     #<FLMSG
        LDY     #>FLMSG
        LDX     #9              ; intro message
        JSR     PEM
        LDA     #$02
        STA     CURRENT_IDE_DRIVE
        JSR     SETUPDRIVE
        JSR     fl_write_bootsector
        JMP     DO_OS_WRITE

DO_IDE:
        LDA     #<IDEMSG
        LDY     #>IDEMSG
        LDX     #9              ; intro message
        JSR     PEM
        LDA     #$00
        STA     CURRENT_IDE_DRIVE
        JSR     IDE_SOFT_RESET
        JSR     ide_write_bootsector


DO_OS_WRITE:
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

        LDA     CURRENT_IDE_DRIVE
        CMP     #$00
        BNE     :+
        JSR     IDE_WRITE_SECTOR;
        JMP     BOOTCONTINUE
:
        CMP     #$01
        BNE     :+
        JSR     PPP_WRITE_SECTOR;
        JMP     BOOTCONTINUE
:
        JSR     WRITEFL
BOOTCONTINUE:
        INC     TEMPWORD1+1     ;
        LDA     TEMPWORD1+1     ;
        CMP     #OSENDPAGE
        BEQ     EXITBOOT        ;
        CMP     #OSENDPAGE+1
        BEQ     EXITBOOT        ;
        INC     debsehd         ;
        LDA     CURRENT_IDE_DRIVE
        CMP     #$02
        BEQ     FLINC
        JMP     BOOTLOOP        ;
FLINC:
; INC FOR FLOPPY DEVICES (9 sectors)
        LDA     debsehd
        CMP     #$09
        BNE     BOOTLOOP
        INC     debcyll
        LDA     #00
        STA     debsehd
        JMP     BOOTLOOP

EXITBOOT:
        JMP     WARMBOOT


MSG:
        .BYTE   $0D,$0A,"WRITEOS - WRITE DOS/65 FROM MEMORY TO BOOT TRACK",$0D,$0A,$0D,$0A
        .BYTE   "SELECT DRIVE:",$0D,$0A
        .BYTE   "(I) IDE PRIMARY",$0D,$0A
        .BYTE   "(S) SD CARD",$0D,$0A
        .BYTE   "(F) FLOPPY A:",$0D,$0A
        .BYTE   "$"

IDEMSG:
        .BYTE   $0D,$0A,"WRITING DOS/65 FROM MEMORY TO PRIMARY IDE.",$0D,$0A,$0D,$0A
        .BYTE   "$"

SDMSG:
        .BYTE   $0D,$0A,"WRITING DOS/65 FROM MEMORY TO SD.",$0D,$0A,$0D,$0A
        .BYTE   "$"

FLMSG:
        .BYTE   $0D,$0A,"WRITING DOS/65 FROM MEMORY TO FLOPPY A:.",$0D,$0A,$0D,$0A
        .BYTE   "$"


ABORTMSG:
        .BYTE   $0D,$0A,"ABORTED.",$0D,$0A,$0D,$0A
        .BYTE   "$"

;_______________________________________________________________
sd_bootsector_image:
; RELOACTE CODE FROM DISK BUFFER, AND JUMP
        LDX     #$00
:
        LDA     INBUFFER,x
        STA     BOOTCODESTART,x
        LDA     INBUFFER+$100,x
        STA     BOOTCODESTART+$100,x
        INX
        CPX     #$00
        BNE     :-
        JMP     BOOTCODESTART+(SDBOOT-sd_bootsector_image)
; PERFORM SYSTEM BOOT  -- CODE MUST BE RELOCATABLE!
SDBOOT:
        LDA     #$00
        STA     debcylm         ; SET BOOT CODE LOCATION
        STA     debcyll         ; SET BOOT CODE LOCATION
        LDA     #$01            ;
        STA     debsehd         ; SET BOOT CODE LOCATION
;
        LDA     #<STARTADDRESS  ; SETUP OS LOAD LOCATION
        STA     TEMPWORD1       ;
        LDA     #>STARTADDRESS  ;
        STA     TEMPWORD1+1     ;
SDBOOTLOOP:
        LDA     #<INBUFFER      ; SETUP DISK BUFFER
        STA     WORKPTR         ;
        LDA     #>INBUFFER      ;
        STA     WORKPTR +1      ;
;
        JSR     PPP_READ_SECTOR ;
        LDY     #$00            ;
:
        LDA     (WORKPTR),Y     ;
        STA     (TEMPWORD1),Y   ;
        INY                     ;
        BNE     :-              ;
        INC     TEMPWORD1+1     ;
        INC     WORKPTR+1
:
        LDA     (WORKPTR),Y     ;
        STA     (TEMPWORD1),Y   ;
        INY                     ;
        BNE     :-              ;
        INC     TEMPWORD1+1     ;
        LDA     TEMPWORD1+1
        CMP     #OSENDPAGE
        BEQ     :+              ;
        CMP     #OSENDPAGE+1
        BEQ     :+              ;
        INC     debsehd         ;
        BNE     SDBOOTLOOP      ;
:
        JMP     STARTOS         ; RUN THE OS
;_______________________________________________________________
sd_write_bootsector:
        LDX     #$00
:
        LDA     sd_bootsector_image,x
        STA     INBUFFER,x
        LDA     sd_bootsector_image+$100,x
        STA     INBUFFER+$100,x
        INX
        CPX     #$00
        BNE     :-

        LDA     #$00
        STA     debcylm         ; SET BOOT CODE LOCATION
        STA     debcyll         ; SET BOOT CODE LOCATION
        STA     debsehd         ; SET BOOT CODE LOCATION
        JSR     PPP_WRITE_SECTOR;
        RTS

;_______________________________________________________________
ide_bootsector_image:
; RELOACTE CODE FROM DISK BUFFER, AND JUMP
        LDX     #$00
:
        LDA     INBUFFER,x
        STA     BOOTCODESTART,x
        LDA     INBUFFER+$100,x
        STA     BOOTCODESTART+$100,x
        INX
        CPX     #$00
        BNE     :-
        JMP     BOOTCODESTART+(IDEBOOT-ide_bootsector_image)
; PERFORM SYSTEM BOOT  -- CODE MUST BE RELOCATABLE!
IDEBOOT:
        LDA     #$00
        STA     CURRENT_IDE_DRIVE
        STA     debcylm         ; SET BOOT CODE LOCATION
        STA     debcyll         ; SET BOOT CODE LOCATION
        LDA     #$01            ;
        STA     debsehd         ; SET BOOT CODE LOCATION
;
        LDA     #<STARTADDRESS  ; SETUP OS LOAD LOCATION
        STA     TEMPWORD1       ;
        LDA     #>STARTADDRESS  ;
        STA     TEMPWORD1+1     ;
IDEBOOTLOOP:
        LDA     #<INBUFFER      ; SETUP DISK BUFFER
        STA     WORKPTR         ;
        LDA     #>INBUFFER      ;
        STA     WORKPTR +1      ;
;
        JSR     IDE_READ_SECTOR ;
        LDY     #$00            ;
:
        LDA     (WORKPTR),Y     ;
        STA     (TEMPWORD1),Y   ;
        INY                     ;
        BNE     :-              ;
        INC     TEMPWORD1+1     ;
        INC     WORKPTR+1
:
        LDA     (WORKPTR),Y     ;
        STA     (TEMPWORD1),Y   ;
        INY                     ;
        BNE     :-              ;
        INC     TEMPWORD1+1     ;
        LDA     TEMPWORD1+1
        CMP     #OSENDPAGE
        BEQ     :+              ;
        CMP     #OSENDPAGE+1
        BEQ     :+              ;
        INC     debsehd         ;
        BNE     IDEBOOTLOOP     ;
:
        JMP     STARTOS         ; RUN THE OS
;_______________________________________________________________
ide_write_bootsector:
        LDX     #$00
:
        LDA     ide_bootsector_image,x
        STA     INBUFFER,x
        LDA     ide_bootsector_image+$100,x
        STA     INBUFFER+$100,x
        INX
        CPX     #$00
        BNE     :-

        LDA     #$00
        STA     debcylm         ; SET BOOT CODE LOCATION
        STA     debcyll         ; SET BOOT CODE LOCATION
        STA     debsehd         ; SET BOOT CODE LOCATION
        JSR     IDE_WRITE_SECTOR;
        RTS

;_______________________________________________________________
fl_bootsector_image:
; RELOACTE CODE FROM DISK BUFFER, AND JUMP
        LDX     #$00
:
        LDA     INBUFFER,x
        STA     BOOTCODESTART,x
        LDA     INBUFFER+$100,x
        STA     BOOTCODESTART+$100,x
        INX
        CPX     #$00
        BNE     :-
        JMP     BOOTCODESTART+(FLBOOT-fl_bootsector_image)
; PERFORM SYSTEM BOOT  -- CODE MUST BE RELOCATABLE!
FLBOOT:
        LDA     #$00
        STA     sekdsk
        STA     debcylm         ; SET BOOT CODE LOCATION
        STA     debcyll         ; SET BOOT CODE LOCATION
        LDA     #$01            ;
        STA     debsehd         ; SET BOOT CODE LOCATION
;
        LDA     #<STARTADDRESS  ; SETUP OS LOAD LOCATION
        STA     TEMPWORD1       ;
        LDA     #>STARTADDRESS  ;
        STA     TEMPWORD1+1     ;
FLBOOTLOOP:
        LDA     #<INBUFFER      ; SETUP DISK BUFFER
        STA     WORKPTR         ;
        LDA     #>INBUFFER      ;
        STA     WORKPTR +1      ;
;
        JSR     READFL          ;
        LDY     #$00            ;
:
        LDA     (WORKPTR),Y     ;
        STA     (TEMPWORD1),Y   ;
        INY                     ;
        BNE     :-              ;
        INC     TEMPWORD1+1     ;
        INC     WORKPTR+1
:
        LDA     (WORKPTR),Y     ;
        STA     (TEMPWORD1),Y   ;
        INY                     ;
        BNE     :-              ;
        INC     TEMPWORD1+1     ;
        LDA     TEMPWORD1+1
        CMP     #OSENDPAGE
        BEQ     :+              ;
        CMP     #OSENDPAGE+1
        BEQ     :+              ;
        INC     debsehd         ;
        LDA     debsehd
        CMP     #$09
        BNE     FLBOOTLOOP      ;
        LDA     #$00
        STA     debsehd
        INC     debcyll
        JMP     FLBOOTLOOP      ;
:
        JMP     STARTOS         ; RUN THE OS
;_______________________________________________________________
fl_write_bootsector:
        LDX     #$00
:
        LDA     fl_bootsector_image,x
        STA     INBUFFER,x
        LDA     fl_bootsector_image+$100,x
        STA     INBUFFER+$100,x
        INX
        CPX     #$00
        BNE     :-

        LDA     #$00
        STA     sekdsk
        STA     debcylm         ; SET BOOT CODE LOCATION
        STA     debcyll         ; SET BOOT CODE LOCATION
        STA     debsehd         ; SET BOOT CODE LOCATION
        JSR     WRITEFL         ;
        RTS



        .END
