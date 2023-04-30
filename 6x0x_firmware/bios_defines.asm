;__________________________________________________________________________________________________
;
; CONFIGURATION
;__________________________________________________________________________________________________
;
M6X0X_IOSPACE   = $E000
M6X0X_SHADOW_ROM = $F000

; PAGER
M6X0X_ACT_TASK  = M6X0X_IOSPACE+$A00
M6X0X_MAP_SETUP = M6X0X_IOSPACE+$A10
M6X0X_MAP_SPACE = M6X0X_IOSPACE+$A20
M6X0X_MMU_ENA   = M6X0X_IOSPACE+$A30

;
;
;__________________________________________________________________________________________________
;
; DATA CONSTANTS
;__________________________________________________________________________________________________
;ZERO PAGE	ADDRESS			; FUNCTION
farfunct        = $32           ; function to call in driver area
farpointer      = $33           ;
IRQVECTOR       = $35           ; VECTOR FOR USER IRQ RTN
NMIVECTOR       = $37           ; VECTOR FOR USER NMI RTN
CONSOLE         = $3A           ; CURRENT CONSOLE
TEMPWORD        = $3B           ;
TEMPWORD1       = $3D           ;
TEMPWORD2       = $3F           ;
STRPTR          = $41           ;
DSKYMODE        = $43           ; DSKY MODE (0=NONE, 1=DSKY, 2=DSKY NG
hstbuf          = $0200         ; 0200-03ff host buffer

;
; DRIVER WORKING STORAGE
;
INBUFFER        = $0400         ;

DSKY_BUF        = $0500         ; Eight Bytes DSKY display buffer
DSKY_BUFLEN     = 8             ;
DSKY_HEXBUF     = $0508         ; Four Bytes DSKY hex buffer
DSKY_HEXBUFLEN  = 4             ;
sektrk          = $050C         ; seek track number
seksec          = $050E         ; seek sector number
debcyll         = $0510         ; DEBLOCKED CYLINDER LSB
debcylm         = $0511         ; DEBLOCKED CYLINDER MSB
debsehd         = $0512         ; DEBLOCKED SECTOR AND HEAD (HS)
Cdebcyll        = $0513         ; DEBLOCKED CYLINDER LSB (IN CACHE)
Cdebcylm        = $0514         ; DEBLOCKED CYLINDER MSB (IN CACHE)
Cdebsehd        = $0515         ; DEBLOCKED SECTOR AND HEAD (HS)  (IN CACHE)
sekdsk          = $0516         ; seek disk number
dskcfg          = $0517         ; 16 bytes disk configuration table
DSKUNIT         = $0528         ; seek disk number
ST0             = $0529         ;
FLERR           = $052A         ;
FCMD            = $052B         ;
FLRETRY         = $052C         ;
FLRETRY1        = $052D         ;
FLATCH_STORE    = $052E         ;
PPIDETIMEOUT    = $052F         ; (word)
slicetmp        = $0531         ; (word)
PPIDEINDEX      = $0533
CURRENT_IDE_DRIVE = $0534
DSKY_X_STORAGE  = $0535
DSKY_Y_STORAGE  = $0536
DSKY_TEMP_VAL   = $0537
DSKY_PPIX_VAL   = $0538
FLOPPY_DETCT    = $053A
DSKY_PRESENT    = $053B