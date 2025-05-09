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
MD_PAGERA       = $0200         ; PAGE DRIVER ADDRESS
IO              = $0300         ; 0300-03FF Memory mapped IO
hstbuf          = $0400         ; 0400-05ff host buffer

; Bank Registers
BANK00          = IO+$50
BANK40          = IO+$51
BANK80          = IO+$52
BANKC0          = IO+$53
OPTIONREGISTER  = IO+$51   ;   OPTION REG.

DO_FARCALL      = $0200
BANKED_DRIVER_DISPATCHER        = $8800
;
; DRIVER WORKING STORAGE
;
DSKY_BUF        = $0600         ; Eight Bytes DSKY display buffer
DSKY_BUFLEN     = 8             ;
DSKY_HEXBUF     = $0608         ; Four Bytes DSKY hex buffer
DSKY_HEXBUFLEN  = 4             ;
debsehd         = $0610         ; DEBLOCKED SECTOR AND HEAD (HS)
debcyll         = $0611         ; DEBLOCKED CYLINDER LSB
debcylm         = $0612         ; DEBLOCKED CYLINDER MSB
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
DSKY_X_STORAGE  = $063D
DSKY_Y_STORAGE  = $063E
DSKY_TEMP_VAL   = $063F
DSKY_PPIX_VAL   = $0640
FLOPPY_DETCT    = $0641
DSKY_PRESENT    = $0642
Cdebsehd        = $0643         ; DEBLOCKED SECTOR AND HEAD (HS)  (IN CACHE)
Cdebcyll        = $0644         ; DEBLOCKED CYLINDER LSB (IN CACHE)
Cdebcylm        = $0645         ; DEBLOCKED CYLINDER MSB (IN CACHE)
CacUnit         = $0646         ; UNIT (IN CACHE)
CONSOLE         = $060F         ; CONSOLE DEVICE
tea             = $800          ;tea start

;zero page for setup
addinp          = $02           ;initialized to a,y
bufadd          = $04           ;buffer address
alcpnt          = $06           ;allocation map pointer
chkpnt          = $08           ;checksum map pointer
numvar          = $50           ;RESERVED ZERO PAGE SPACE


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
TEMPWORD        = $3B           ;
TEMPWORD1       = $3D           ;
TEMPWORD2       = $3F           ;
STRPTR          = $41           ;
DSKYMODE        = $43           ; DSKY MODE (0=NONE, 1=DSKY, 2=DSKY NG
sektrk          = $44           ; seek track number
seksec          = $46           ; seek sector number
sekdsk          = $48           ; seek disk number
currentDrive    = $49
pcf_buffer      = $4A           ; only used in PCF driver, free for use outside as temp (word)
pcf_address     = $4C           ; only used in PCF driver, free for use outside as temp (byte)

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


        .IFDEF RBC6X0X
        .DEFINE COMSUFFIX "COM"
        .ENDIF

        .IFDEF NHYODYNE
        .DEFINE COMSUFFIX "CO6"
        .ENDIF

        .IFDEF DUODYNE
        .DEFINE COMSUFFIX "CO6"
        .ENDIF

        .IFDEF DUODYNE02
        .DEFINE COMSUFFIX "CO6"
        .ENDIF

DEBUG           = 0
