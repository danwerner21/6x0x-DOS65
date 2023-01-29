;________________________________________________________________________________________________________________________________
;
;	DOS/65 base addresses and definitions
;
;  DWERNER 04/24/2022 	Initial
;________________________________________________________________________________________________________________________________

;base addresses and definitions
btejmp          = $0100         ; warm boot jump
pemjmp          = $0103         ; jump to pem
iostat          = $0106         ; i/o status
dflfcb          = $0107         ; default fcb
dflbuf          = $0128         ; default buffer
hstbuf          = $0200         ; 0200-03ff host buffer

;
; DRIVER WORKING STORAGE
;

DSKY_BUF        = $0500         ; Eight Bytes DSKY display buffer
DSKY_BUFLEN     = 8             ;
DSKY_HEXBUF     = $0508         ; Four Bytes DSKY hex buffer
DSKY_HEXBUFLEN  = 4             ;
sektrk          = $050C         ; seek track number
seksec          = $050E         ; seek sector number
debcyll         = $0510         ; DEBLOCKED CYLINDER LSB
debcylm         = $0511         ; DEBLOCKED CYLINDER MSB
debsehd         = $0512         ; DEBLOCKED SECTOR AND HEAD (HS)
sekdsk          = $0516         ; seek disk number
dskcfg          = $0517         ; 16 bytes disk configuration table
DSKUNIT         = $0528         ; seek disk number

tea             = $800          ;tea start

;zero page for setup
addinp          = $02           ;initialized to a,y
bufadd          = $04           ;buffer address
alcpnt          = $06           ;allocation map pointer
chkpnt          = $08           ;checksum map pointer
numvar          = 8             ;eight bytes

IRQVECTOR       = $35           ; VECTOR FOR USER IRQ RTN
NMIVECTOR       = $37           ; VECTOR FOR USER NMI RTN
CONSOLE         = $3A

;page zero and system ram assignments
DEST            = $EC           ;pointer for OutMsg
SRC             = $EE           ;pointer for OutMsg
OUTMSG_W        = $F0           ;pointer for OutMsg
dmaadr          = $f4           ;pointer for r/w


;pem constants on entry to write
wrall           = 0             ;write to allocated
wrdir           = 1             ;write to directory
wrual           = 2             ;write to unallocated

;fixed parameters
lf              = $a            ;linefeeed
cr              = $d            ;return
eof             = $1a           ;end of file
null            = 0             ;null
ctlc            = 3             ;abort
ctle            = 5             ;physical cr lf
ctli            = 9             ;tab character
ctlp            = $10           ;toggle printer
ctlr            = $12           ;repeat line
ctls            = $13           ;freeze
ctlx            = $18           ;cancel
semico          = $3b           ;semicolon
delete          = $08           ;delete character
numcmd          = 36            ;number commands
DEFDRV          = 0             ; SET TO DEFAULT DRIVE LETTER

M6X0X_IOSPACE   = $E000
M6X0X_SHADOW_ROM = $F000

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
