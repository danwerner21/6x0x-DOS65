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
memmovr         = $0200         ; 0200-02ff subr to move data from ram/rom disks
MD_PAGERA       = $0200         ; PAGE DRIVER ADDRESS
IO              = $0300         ; 0300-03FF Memory mapped IO
MPCL_ROM        = $037C         ; ROM MAPPER
MPCL_RAM        = $0378         ; RAM MAPPER
hstbuf          = $0400         ; 0400-05ff host buffer
;
; DRIVER WORKING STORAGE
;


DSKY_BUF        = $0600         ; Eight Bytes DSKY display buffer
DSKY_BUFLEN     = 8             ;
DSKY_HEXBUF     = $0608         ; Four Bytes DSKY hex buffer
DSKY_HEXBUFLEN  = 4             ;
sektrk          = $060C         ; seek track number
seksec          = $060E         ; seek sector number
debcyll         = $0610         ; DEBLOCKED CYLINDER LSB
debcylm         = $0611         ; DEBLOCKED CYLINDER MSB
debsehd         = $0612         ; DEBLOCKED SECTOR AND HEAD (HS)
sekdsk          = $0616         ; seek disk number
dskcfg          = $0617         ; 16 bytes disk configuration table
DSKUNIT         = $0628         ; seek disk number
slicetmp        = $0631         ; (word)
STACKA          = $0635
nmsstr          = $0636
FLRETRY         = $0637         ;
FLRETRY1        = $0638         ;
ST0             = $0639         ;
FLERR           = $063A         ;
FCMD            = $063B         ;
PPIDEINDEX      = $063C
CURRENT_IDE_DRIVE = $063D
DSKY_X_STORAGE  = $063E
DSKY_Y_STORAGE  = $063F
DSKY_TEMP_VAL   = $0640
DSKY_PPIX_VAL   = $0641
FLOPPY_DETCT    = $0642
DSKY_PRESENT    = $0643
Cdebcyll        = $0644         ; DEBLOCKED CYLINDER LSB (IN CACHE)
Cdebcylm        = $0645         ; DEBLOCKED CYLINDER MSB (IN CACHE)
Cdebsehd        = $0646         ; DEBLOCKED SECTOR AND HEAD (HS)  (IN CACHE)

tea             = $800          ;tea start

;zero page for setup
addinp          = $02           ;initialized to a,y
bufadd          = $04           ;buffer address
alcpnt          = $06           ;allocation map pointer
chkpnt          = $08           ;checksum map pointer
numvar          = 8             ;eight bytes


msgptr          = chkpnt+2      ;message pointer
movptr          = msgptr        ;and move pointer
dcbloc          = msgptr+2      ;pointer to dcb

dcbpc           = $2C           ;pointer to DCB table
dskcfpc         = $2E           ;pointer to disk configuration table
cmdlnp          = $30           ;pointer to command line buffer
farfunct        = $32           ; function to call in driver area
farpointer      = $33           ;
IRQVECTOR       = $35           ; VECTOR FOR USER IRQ RTN
NMIVECTOR       = $37           ; VECTOR FOR USER NMI RTN
CONSOLE         = $3A

DO_FARCALL      = $FFF0

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

    .DEFINE COMSUFFIX "COM"
DEBUG           = 0
BANKED_DRIVER_DISPATCHER=$8800
