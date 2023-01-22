;________________________________________________________________________________________________________________________________
;
;	DOS/65 base addresses and definitions
;
;  DWERNER 04/24/2022 	Initial
;________________________________________________________________________________________________________________________________

;base addresses and definitions
btejmp	        =	$0100		; warm boot jump
pemjmp	        =	$0103		; jump to pem
iostat	        =	$0106		; i/o status
dflfcb	        =	$0107		; default fcb
dflbuf	        =	$0128		; default buffer
memmovr         =	$0200		; 0200-02ff subr to move data from ram/rom disks
MD_PAGERA       =   $0200       ; PAGE DRIVER ADDRESS

IO              =   $0300       ; 0300-03FF Memory mapped IO
MPCL_ROM	    =	$037C		; ROM MAPPER
MPCL_RAM	    =	$0378		; RAM MAPPER

MD_PAGEBU       =   $0400       ; 0400-04FF PAGE BUFFER ADDRESS
MD_PAGESE       =   pointr      ; PAGE SECTOR STORAGE

;
; DRIVER WORKING STORAGE
;
DSKY_BUF        =       $0500	        ; Eight Bytes DSKY display buffer
DSKY_BUFLEN	    =       8               ;
DSKY_HEXBUF     =       $0508           ; Four Bytes DSKY hex buffer
DSKY_HEXBUFLEN	=       4               ;
sektrk          =       $050C		    ; seek track number
seksec          =       $050E		    ; seek sector number
debcyll         =       $0510	        ; DEBLOCKED CYLINDER LSB
debcylm         =	    $0511           ; DEBLOCKED CYLINDER MSB
debsehd         =	    $0512           ; DEBLOCKED SECTOR AND HEAD (HS)
sekdsk          =       $0513           ; seek disk number
dskcfg          =       $0514           ; 16 bytes disk configuration table
DSKUNIT         =       $0525           ; seek disk number


tea	=	$800		;tea start

;zero page for setup
addinp	=	$02		;initialized to a,y
bufadd	=	$04		;buffer address
alcpnt	=	$06		;allocation map pointer
chkpnt	=	$08		;checksum map pointer
numvar	=	8		;eight bytes

trknum	=	$02			;current track
dcbadd	=	$04			;dcb address
nmsstr	=	$06			;number system tracks
nsectr	=	$08			;number sectors per track
ttlsec	=	$0A			;total sectors to write
trkcnt	=	ttlsec
size	=	$0B			;ascii size
lokim	=	$0D			;low kim limit
hikim	=	$0F			;high kim limit
offset	=	$11			;relocation offset
kimcnt	=	$13			;kim counter
pointr	=	$14			;pointer
lengt	=	$16			;inst length
point	=	$17			;relocate pointer
adjust	=	$19			;relocate distance
kimpnt	=	$1B			;kim file index
savex	=	$1C			;save for x
savey	=	$1D			;save for y
number	=	$1E			;input pack buffer
dstdrv	=	$20			;destination drive
defalt	=	$21			;default drive
seccnt	=	$22			;sector count
secnum	=	$24			;sector number
curccm	=	$26			;start of current ccm
simlng	=	$28			;length of sim
room	=	$2A			;memory needed for sysgen
stksav	=	$2C			;save stack register
frstsc	=	$2D			;first sector number of disk
dskcfpc	=	$2E			;pointer to disk configuration table
cmdlnp	=	$30			;pointer to command line buffer
farfunct=   $32         ;function to call in driver area
farpointer= $33         ;WORD POINTER to call in driver area
IRQVECTOR=	$35   		; VECTOR FOR USER IRQ RTN
NMIVECTOR=	$37   		; VECTOR FOR USER NMI RTN
zptemp	=	$39
CONSOLE	=	$3A
lastzp	=	$3B


;page zero and system ram assignments
dmaadr	=	$f4		;pointer for r/w
mvepnt	=	$f2		;host buffer location
OUTMSG_W =	$F0		;pointer for OutMsg
SRC	 =	$EE		;pointer for OutMsg
DEST	 =	$EC		;pointer for OutMsg

nsects	=	(simstart-ccm)/128	;number sectors


;pem constants on entry to write
wrall	=	0		;write to allocated
wrdir	=	1		;write to directory
wrual	=	2		;write to unallocated

;fixed parameters
lf	    =	$a		;linefeeed
cr	    =	$d		;return
eof	    =	$1a		;end of file
null	=	0		;null
ctlc	=	3		;abort
ctle	=	5		;physical cr lf
ctli	=	9		;tab character
ctlp	=	$10		;toggle printer
ctlr	=	$12		;repeat line
ctls	=	$13		;freeze
ctlx	=	$18		;cancel
semico	=	$3b		;semicolon
delete	=	$08		;delete character
numcmd	=	36		;number commands


USEECB		=	0	; SET TO ONE IF HOST PROCESSOR MODE (0= STAND ALONE)
USESERIAL 	= 	0	; SET TO ONE FOR STAND ALONE SERIAL CONSOLE IO
USECONPPP 	= 	1	; SET TO ONE FOR STAND ALONE PAR-PORT-PROP CONSOLE IO
USEFLOPPYA 	= 	0	; SET TO ONE FOR STAND ALONE FLOPPY = "A"
USEPPPSDA 	= 	1	; SET TO ONE FOR STAND ALONE ParPortProp SD Card  = "A"
USEFLOPPYB 	= 	1	; SET TO ONE FOR STAND ALONE FLOPPY = "B"
USEIDEC 	= 	1	; SET TO ONE FOR STAND ALONE IDE HDD="C"
USEDSKY 	= 	1	; SEND DEBUGGING INFO TO DSKY
DEFDRV  	=	0	; SET TO DEFAULT DRIVE LETTER
USEDISKIOV1     = 	0	; Floppy and IDE card is  DISK IO V1
USEDISKIOV3     = 	1	; Floppy and IDE card is  DISK IO V3


COLOSSUS6X0X	=	1	; USE COLOSSUS 6X0X HARDWARE
ORIGINAL6X0X	=	0	; USE ORIGINAL 6X0X HARDWARE

M6X0X_IOSPACE		=	$E000
M6X0X_SHADOW_ROM 	=	$F000
