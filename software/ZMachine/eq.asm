;	PAGE
;	SBTTL "--- MEMORY ORGANIZATION ---"

TRUE	=	$FF
FALSE	=	0
LO	=	0
HI	=	1

IOBUFF	=	MSTART		; 256-BYTE DISK BUFFER
ZSTAKL	=	MSTART+$100	; Z-STACK LSBS
ZSTAKH	=	MSTART+$200	; Z-STACK MSBS
PTABL	=	MSTART+$300	; PAGING TABLE LSBS ($A0 BYTES)
PTABH	=	MSTART+$3A0	; PAGING TABLE MSBS ($A0 BYTES)
LRUMAP	=	MSTART+$450	; TIMESTAMP MAP ($A0 BYTES) (BM 11/24/84)
LOCALS  =     MSTART+$500     ; LOCAL VARIABLE STORAGE (32 BYTES)
BUFSAV  =     MSTART+$520     ; I/O AUX BUFFER (80 BYTES)

ZIP	=	MSTART+$600	; START OF EXECUTABLE CODE
ZBEGIN	=	ZIP+$1D00	; START OF Z-CODE

	; ---------------------
	; Z-CODE HEADER OFFSETS
	; ---------------------

ZVERS	=	0		; VERSION BYTE
ZMODE	=	1		; MODE SELECT BYTE
ZID	=	2		; GAME ID WORD
ZENDLD	=	4		; START OF NON-PRELOADED Z-CODE
ZGO	=	6		; EXECUTION ADDRESS
ZVOCAB	=	8		; START OF VOCABULARY TABLE
ZOBJEC	=	10		; START OF OBJECT TABLE
ZGLOBA	=	12		; START OF GLOBAL VARIABLE TABLE
ZPURBT	=	14		; START OF "PURE" Z-CODE
ZSCRIP	=	16		; FLAG WORD
ZSERIA	=	18		; 3-WORD ASCII SERIAL NUMBER
ZFWORD	=	24		; START OF FWORDS TABLE
ZLENTH	=	26		; LENGTH OF Z-PROGRAM IN WORDS
ZCHKSM	=	28		; Z-CODE CHECKSUM WORD

;	PAGE
;	SBTTL "--- ZIP Z-PAGE VARIABLES ---"

OPCODE	=	ZEROPG		; (BYTE) CURRENT OPCODE
NARGS	=	OPCODE+1	; (BYTE) # ARGUMENTS
ARG1	=	OPCODE+2	; (WORD) ARGUMENT #1
ARG2	=	OPCODE+4	; (WORD) ARGUMENT #2
ARG3	=	OPCODE+6	; (WORD) ARGUMENT #3
ARG4	=	OPCODE+8	; (WORD) ARGUMENT #4
ABYTE	=	OPCODE+10	; (BYTE) X-OP ARGUMENT BYTE
ADEX	=	OPCODE+11	; (BYTE) X-OP ARGUMENT INDEX

VALUE	=	OPCODE+12	; (WORD) VALUE RETURN REGISTER
I	=	VALUE+2		; (WORD) GEN-PURPOSE REGISTER #1
J	=	VALUE+4		; (WORD) GEN-PURPOSE REGISTER #2
K	=	VALUE+6		; (WORD) GEN-PURPOSE REGISTER #3

ZSP	=	VALUE+8		; (BYTE) Z-STACK POINTER
OLDZSP	=	ZSP+1		; (BYTE) OLD Z-STACK POINTER

ZPC	=	ZSP+2		; (3 BYTES) ZIP PROGRAM COUNTER
ZPCL	=	ZPC		; (BYTE) LOW 8 BITS OF [ZPC]
ZPCM	=	ZPC+1		; (BYTE) MIDDLE 8 BITS OF [ZPC]
ZPCH	=	ZPC+2		; (BYTE) HIGH BIT OF [ZPC]
ZPCFLG	=	ZPC+3		; (BYTE) FLAG: "TRUE" IF [ZPCPNT] VALID
ZPCPNT	=	ZPC+4		; (WORD) ABS POINTER TO CURRENT Z-PAGE

MPC	=	ZPC+6		; (3 BYTES) MEMORY PROGRAM COUNTER
MPCL	=	MPC		; (BYTE) LOW 8 BITS OF [MPC]
MPCM	=	MPC+1		; (BYTE) MIDDLE 8 BITS OF [MPC]
MPCH	=	MPC+2		; (BYTE) HIGH BIT OF [MPC]
MPCFLG	=	MPC+3		; (BYTE) FLAG: "TRUE" IF [MPCPNT] VALID
MPCPNT	=	MPC+4		; (WORD) ABS POINTER TO CURRENT M-PAGE

LRU	=	MPC+6		; (BYTE) PAGING INDEX
ZCODE	=	LRU+1		; (BYTE) 1ST ABSOLUTE PAGE OF PRELOAD
ZPURE	=	LRU+2		; (BYTE) 1ST VIRTUAL PAGE OF "PURE" Z-CODE
PAGE0	=	LRU+3		; (BYTE) 1ST PAGE OF ACTUAL SWAPPING SPACE
PMAX	=	LRU+4		; (BYTE) MAXIMUM # OF SWAPPING PAGES
ZPAGE	=	LRU+5		; (BYTE) CURRENT SWAPPING PAGE
TARGET	=	LRU+6		; (WORD) TARGET PAGE FOR SWAPPING
STAMP	=	LRU+8		; (BYTE) CURRENT TIMESTAMP (BM 11/24/84)
SWAP	=	LRU+9		; (BYTE) EARLIEST PAGE (BM 11/24/84)

GLOBAL	=	LRU+10		; (WORD) GLOBAL VARIABLE POINTER
VOCAB	=	GLOBAL+2	; (WORD) VOCAB TABLE POINTER
FWORDS	=	GLOBAL+4	; (WORD) F-WORDS TABLE POINTER
OBJTAB	=	GLOBAL+6	; (WORD) OBJECT TABLE POINTER

	; Z-STRING MANIPULATION VARIABLES

IN	=	GLOBAL+8	; (6 BYTES) INPUT BUFFER
OUT	=	IN+6		; (6 BYTES) OUTPUT BUFFER

SOURCE	=	OUT+6		; (BYTE) SOURCE BUFFER POINTER
RESULT	=	SOURCE+1	; (BYTE) RESULT TABLE POINTER
LINLEN	=	SOURCE+2	; (BYTE) LENGTH OF CURRENT LINE
WRDLEN	=	SOURCE+3	; (BYTE) LENGTH OF CURRENT WORD
ENTRY	=	SOURCE+4	; (WORD) ADDR OF CURRENT RESULT ENTRY
NENTS	=	SOURCE+6	; (WORD) # ENTRIES IN VOCAB TABLE
ESIZE	=	SOURCE+8	; (BYTE) SIZE OF VOCAB TABLE ENTRIES
PSET	=	SOURCE+9	; (BYTE) PERMANENT CHARSET
TSET	=	SOURCE+10	; (BYTE) TEMPORARY CHARSET
ZCHAR	=	SOURCE+11	; (BYTE) CURRENT Z-CHAR
OFFSET	=	SOURCE+12	; (BYTE) F-WORD TABLE OFFSET
ZFLAG	=	SOURCE+13	; (BYTE) Z-WORD ACCESS FLAG
ZWORD	=	SOURCE+14	; (WORD) CURRENT Z-WORD
CONCNT	=	SOURCE+16	; (BYTE) Z-STRING SOURCE COUNTER
CONIN	=	SOURCE+17	; (BYTE) CONVERSION SOURCE INDEX
CONOUT	=	SOURCE+18	; (BYTE) CONVERSION DEST INDEX

QUOT	=	SOURCE+19	; (WORD) QUOTIENT FOR DIVISION
REMAIN	=	QUOT+2		; (WORD) REMAINDER FOR DIVISION
MTEMP	=	QUOT+4		; (WORD) MATH TEMPORARY REGISTER
QSIGN	=	QUOT+6		; (BYTE) SIGN OF QUOTIENT
RSIGN	=	QUOT+7		; (BYTE) SIGN OF REMAINDER
DIGITS	=	QUOT+8		; (BYTE) DIGIT COUNT FOR "PRINTN"

TIMEFL	=	QUOT+9		; (BYTE) "TRUE" IF TIME MODE
LENGTH	=	TIMEFL+1	; (BYTE) LENGTH OF LINE IN [LINBUF]
OLDLEN	=	TIMEFL+2	; (BYTE) OLD LINE LENGTH
SCRIPT	=	TIMEFL+3	; (BYTE) SCRIPT ENABLE FLAG
OLDX	=	TIMEFL+4	; (BYTE) OLD CURSOR X
OLDY	=	TIMEFL+5	; (BYTE) OLD CURSOR Y
LINCNT	=	TIMEFL+6	; (BYTE) LINE COUNTER
LMAX	=	TIMEFL+7	; (BYTE) MAX # LINES/SCREEN

IOCHAR	=	TIMEFL+8	; (BYTE) CHARACTER BUFFER
SLINE	=	IOCHAR+1	; (BYTE) BORDERLINE FOR SPLIT
SPSTAT	=	IOCHAR+2	; (BYTE) SPLIT SCREEN STATUS FLAG
LFROM	=	IOCHAR+3	; (WORD) "FROM" LINE ADDRESS
LTO	=	IOCHAR+5	; (WORD) "TO" LINE ADDRESS
PSTAT	=	IOCHAR+7	; (BYTE) PRINTER STATUS FLAG
PRLEN	=	IOCHAR+8	; (BYTE) SCRIPT LINE LENGTH

DBLOCK	=	IOCHAR+9	; (WORD) Z-BLOCK TO READ
DBUFF	=	DBLOCK+2	; (WORD) RAM PAGE TO ACCESS (LSB = 0)
;TRACK	=	DBLOCK+4	; (BYTE) TARGET TRACK
;SECTOR	=	DBLOCK+5	; (BYTE) TARGET SECTOR
GPOSIT	=	DBLOCK+6	; (BYTE) DEFAULT SAVE POSITION
GDRIVE	=	DBLOCK+7	; (BYTE) DEFAULT SAVE DRIVE
TPOSIT	=	DBLOCK+8	; (BYTE) TEMP SAVE POSITION
TDRIVE	=	DBLOCK+9	; (BYTE) TEMP SAVE DRIVE

BLINK	=	DBLOCK+11	; (WORD) CURSOR BLINK TIMER

;DVD	=	DBLOCK+13	; (WORD) DISK DIVIDEND
;DSOR	=	DBLOCK+15	; (WORD) DISK DIVISOR
DTEMP	=	DBLOCK+17	; (WORD) DISK TEMP VARIABLE
