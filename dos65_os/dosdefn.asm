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
slicetmp        = $0531         ; (word)
CURRENT_IDE_DRIVE = $0534

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

M6X0X_IOSPACE   = $E000
M6X0X_SHADOW_ROM = $F000
