;        PAGE
;        SBTTL   "--- WARMSTART ROUTINE ---"

; -------------
; ZIP WARMSTART
; -------------

WARM2:

; PROBABLY NOT THE BEST WAY TO DO THIS
; I NEED TO USE THE CLOCK TO SEED THE RNG
        LDA     $44
        STA     state+0
        LDA     $46
        STA     state1+0
        LDA     $3B
        STA     state+1
        LDA     $3D
        STA     state1+1
        LDA     $0635
        STA     state+2
        STA     state1+2
        PHP
        PLA
        STA     state+3
        TSX
        STX     state1+3
        LDA     $0400
        STA     state+4
        LDA     $0401
        STA     state1+4


        LDA     #0              ; CLEAR ALL Z-PAGE VARIABLES
        LDX     #ZEROPG
ST0:
        STA     0,X
        INX
        CPX     #ZPGTOP
        BCC     ST0

; INIT THE PAGING TABLES

        TAX                     ; = 0
        LDA     #$FF
ST1A:
        STA     PTABL,X
        STA     PTABH,X
        INX
        CPX     #$A0
        BCC     ST1A

; INIT THE TIMESTAMP MAP (BM 11/24/84)\

        LDA     #0
        TAX
ST1B:
        STA     LRUMAP,X
        INX
        CPX     #$A0
        BCC     ST1B

        INC     ZSP             ; INIT Z-STACK POINTERS
        INC     OLDZSP          ; TO "1"
        INC     STAMP           ; INIT TIMESTAMP (BM 11/24/84)

; GRAB THE FIRST BLOCK OF PRELOAD

        LDA     #>ZBEGIN        ; MSB OF PRELOAD START ADDRESS
        STA     ZCODE           ; FREEZE IT HERE
        STA     DBUFF+HI        ; LSB IS ALWAYS ZERO
        JSR     GETDSK          ; [DBLOCK] SET TO Z-BLOCK 0

; EXTRACT GAME DATA FROM Z-CODE HEADER

        LDX     ZBEGIN+ZENDLD   ; MSB OF ENDLOAD POINTER
        INX                     ; ADD 1 TO GET
        STX     ZPURE           ; 1ST "PURE" PAGE OF Z-CODE

        TXA                     ; ADD START PAGE OF PRELOAD
        CLC                     ; TO CALC ABSOLUTE START ADDRESS
        ADC     ZCODE           ; OF PAGING SPACE
        STA     PAGE0

        JSR     MEMTOP          ; RETURNS TOP RAM PAGE IN [A]
        SEC
        SBC     PAGE0           ; SUBTRACT ADDRESS OF PAGING SPACE
        BEQ     NORAM
        BCS     SETNP           ; ERROR IF NOT ENOUGH RAM

; *** ERROR #0 -- INSUFFICIENT RAM ***

NORAM:
        LDA     #0
        JMP     ZERROR

SETNP:
        CMP     #$A0            ; DON'T ALLOW MORE THAN $A0 PAGES
        BCC     SETA0
        LDA     #$A0
SETA0:
        STA     PMAX            ; SET # SWAPPING PAGES

        LDA     ZBEGIN+ZMODE
        ORA     #%00100000      ; ENABLE SPLIT-SCREEN
        STA     ZBEGIN+ZMODE

        AND     #%00000010      ; ISOLATE STATUS-FORMAT BIT
        STA     TIMEFL          ; 0=SCORE, NZ=TIME

        LDA     ZBEGIN+ZGLOBA   ; GET MSB OF GLOBAL TABLE ADDR
        CLC                     ; CONVERT TO
        ADC     ZCODE           ; ABSOLUTE ADDRESS
        STA     GLOBAL+HI
        LDA     ZBEGIN+ZGLOBA+1 ; LSB NEEDN'T CHANGE
        STA     GLOBAL+LO

        LDA     ZBEGIN+ZFWORD   ; DO SAME FOR FWORDS TABLE
        CLC
        ADC     ZCODE
        STA     FWORDS+HI
        LDA     ZBEGIN+ZFWORD+1 ; NO CHANGE FOR LSB
        STA     FWORDS+LO

        LDA     ZBEGIN+ZVOCAB   ; NOW DO VOCABULARY TABLE
        CLC
        ADC     ZCODE
        STA     VOCAB+HI
        LDA     ZBEGIN+ZVOCAB+1 ; LSB SAME
        STA     VOCAB+LO

        LDA     ZBEGIN+ZOBJEC   ; NOT TO MENTION
        CLC                     ; THE OBJECT TABLE
        ADC     ZCODE
        STA     OBJTAB+HI
        LDA     ZBEGIN+ZOBJEC+1 ; LSB SAME
        STA     OBJTAB+LO

; FETCH THE REST OF THE PRELOAD

LDPRE:
        LDA     DBLOCK+LO       ; CHECK CURRENT BLOCK #
        CMP     ZPURE           ; LOADED LAST PRELOAD PAGE YET?
        BCS     WARMEX          ; YES, TIME TO PLAY!
        JSR     GETDSK          ; ELSE GRAB NEXT Z-BLOCK
        JMP     LDPRE

WARMEX:
        LDA     ZBEGIN+ZGO      ; GET START ADDRESS OF Z-CODE
        STA     ZPCM            ; MSB
        LDA     ZBEGIN+ZGO+1    ; AND LSB
        STA     ZPCL            ; HIGH BIT ALREADY ZEROED

        INC     SCRIPT          ; ENABLE SCRIPTING
        LDA     ZBEGIN+ZSCRIP+1 ; STUFF IN THE
        ORA     SFLAG           ; PREVIOUS SCRIPT MODE
        STA     ZBEGIN+ZSCRIP+1 ; (BM 5/14/85)

        JSR     CLS             ; CLEAR SCREEN, DISABLE SPLIT

; ... AND FALL INTO MAIN LOOP
