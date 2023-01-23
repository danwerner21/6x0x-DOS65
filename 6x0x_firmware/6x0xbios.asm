
;__6x0xbios__________________________________________________________________________________________
;
;	BIOS for the 6502 6x0x Retrobrew Computers SBC
;
;	ORIGINALLY WRITTEN BY: DAN WERNER -- 1/1/2014
; 	Code cleanup: Dan Werner -- 1/22/2023
;
; ** NOTE THAT THIS BIOS NEEDS PAGED MEMORY TO OPERATE
; ** K17 MUST BE OPEN FOR PAGED MEMORY TO OPERATE ON THE 6502 CPU
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
IRQVECTOR       = $35           ; VECTOR FOR USER IRQ RTN
NMIVECTOR       = $37           ; VECTOR FOR USER NMI RTN
CONSOLE         = $3A           ; CURRENT CONSOLE
WORKPTR         = $3B           ; WORK POINTER FOR COMMAND PROCESSOR
JUMPPTR         = $3D           ; JUMP VECTOR FOR LOOKUP TABLE
TEMPWORD        = $3F           ;
TEMPWORD1       = $41           ;
TEMPWORD2       = $43           ;
TEMPBYTE        = $45           ;
ACC             = $46           ; ACC STORAGE
XREG            = $47           ; X REG STORAGE
YREG            = $48           ; Y REG STORAGE
PREG            = $49           ; CURRENT STACK POINTER
PC              = $4A           ; PROGRAM COUNTER
SPTR            = $4C           ; CPU STATUS REGISTER
CKSM            = $4D           ; CHECKSUM
BYTECT          = $4E           ; BYTE COUNT
STRPTR          = $4F           ;

; working memory for assembler -- only used during assembler operation
savx            = $1c
tmpc            = $1d
length          = $1f
wrap            = $26
aflg            = $28
acmd            = $2a
nemo            = $44
tmp0            = $c1
tmp2            = $c3
stage           = $0210         ; assembler stage area (SHARED WITH HOST BUFFER	)
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




        .SEGMENT "DRIVERS"
;	dsky? (both)
; 	rtc?
        .INCLUDE "bios_ppp_hd.asm"
        .INCLUDE "bios_diov3_flp.asm"
        .INCLUDE "bios_diov3_ide.asm"


        .SEGMENT "TROM"
        .INCLUDE "miniassembler.asm"
        .INCLUDE "bios_ppp_common.asm"
        .INCLUDE "bios_serial.asm"
        .INCLUDE "bios_ppp_console.asm"


;__COLD_START___________________________________________________
;
; PERFORM SYSTEM COLD INIT
;
;_______________________________________________________________
COLD_START:
        SEI                     ; DISABLE INTERRUPTS
        CLD                     ;  VERIFY DECIMAL MODE IS OFF
        LDX     #$FF            ;
        TXS                     ; CLEAR STACK
        TXA

        LDA     #$00            ; SET CONSOLE (00=PPP, 01= SERIAL)
;;; TODO  ADD AUTODETECT/OPTION AT SOME POINT
        STA     CONSOLE

        LDA     #<IRQROUTINE
        STA     IRQVECTOR
        STA     NMIVECTOR
        LDA     #>IRQROUTINE
        STA     IRQVECTOR+1
        STA     NMIVECTOR+1

;	INIT HARDWARE
        JSR     INIT_PPP        ;
        JSR     SERIALINIT      ;
        JSR     INITPAGES       ;


        LDA     #<STARTUP       ; OUTPUT STARTUP STRING
        STA     STRPTR          ;
        LDA     #>STARTUP       ;
        STA     STRPTR+1        ;
        JSR     OUTSTR          ;

        LDA     #$00            ;
        STA     INBUFFER        ; MAKE SURE INPUT BUFFER IS EMPTY
;

        BRK                     ; PERFORM BRK (START MONITOR)


;__BRKROUTINE___________________________________________________
;
; HANDLE CPU BRK INTERRUPT PROCESING AND START MONITOR
;
;_______________________________________________________________
BRKROUTINE:
; MONITOR'S BREAK HANDLER
        CLC
        PLA                     ;
        TAX                     ; LOW BYTE OF PC
        PLA                     ;
        TAY                     ; HIGH BYTE OF PC
        SEC                     ;
        TXA                     ;
        SBC     #$02            ; REMOVE BREAK INSTRUCTION
        STA     PC              ;
        BCS     BRK2            ;
        DEY
BRK2:
        STY     PC+1            ; SAVE PC
        TSX                     ; GET STACK POINTER
        STX     SPTR            ; SAVE STACK POINTER
        JSR     PRINT_REG       ; DUMP REGISTER CONTENTS
        LDX     #$FF            ;
        TXS                     ; CLEAR STACK
        CLI                     ; ENABLE INTERRUPTS AGAIN
        JMP     COMMAND_PROCESSOR; START THE MONITOR


;__IRQROUTINE___________________________________________________
;
; HANDLE INTERRUPT PROCESING
;
;_______________________________________________________________
IRQROUTINE:
        CLI                     ; ENABLE INTERRUPTS AGAIN
        RTI

;__INTERRUPT____________________________________________________
;
; HANDLE IRQ INTERRUPT AND DETERMINE IF IT IS A BRK OR AN IRQ
;
;_______________________________________________________________
INTERRUPT:
        SEI                     ; DISABLE INTERRUPTS
        STY     YREG            ; SAVE Y
        STX     XREG            ; SAVE X
        STA     ACC             ; SAVE A
        PLA                     ; GET STATUS REGISTER
        STA     PREG            ; SAVE STATUS REGISTER
        AND     #$10            ; MASK BRK
        BNE     BRKCMD          ; BRK CMD
        JMP     (IRQVECTOR)     ; LET USER ROUTINE HAVE IT (USER DEFINED IRQ)
BRKCMD:
        JMP     BRKROUTINE      ; MONITOR BRK ROUTINE

NINTERRUPT:
        JMP     (NMIVECTOR)     ; LET USER ROUTINE HAVE IT (USER DEFINED NMI)



;__PRINT_REG____________________________________________________
;
; PRINT OUT REGISTERS ON THE DISPLAY
;
;_______________________________________________________________
PRINT_REG:
        LDA     #<REGDATA       ; OUTPUT HEADER STRING
        STA     STRPTR          ;
        LDA     #>REGDATA       ;
        STA     STRPTR+1        ;
        JSR     OUTSTR          ;
        LDA     PC+1            ; OUTPUT PROGRAM COUNTER HIGH BYTE
        JSR     PRINT_BYTE      ;
        LDA     PC              ; OUTPUT PROGRAM COUNTER LOW BYTE
        JSR     PRINT_BYTE      ;
        LDA     #$20            ; OUTPUT SPACE
        JSR     IOF_OUTCH       ;
        LDA     ACC             ; OUTPUT ACCUMULATOR
        JSR     PRINT_BYTE      ;
        LDA     #$20            ; OUTPUT 2 SPACES
        JSR     IOF_OUTCH       ;
        LDA     #$20            ;
        JSR     IOF_OUTCH       ;
        LDA     XREG            ; OUTPUT X REGISTER
        JSR     PRINT_BYTE      ;
        LDA     #$20            ; OUTPUT 2 SPACES
        JSR     IOF_OUTCH       ;
        LDA     #$20            ;
        JSR     IOF_OUTCH       ;
        LDA     YREG            ; OUTPUT Y REGISTER
        JSR     PRINT_BYTE      ;
        LDA     #$20            ; OUTPUT 2 SPACES
        JSR     IOF_OUTCH       ;
        LDA     #$20            ;
        JSR     IOF_OUTCH       ;
        LDA     SPTR            ; OUTPUT STACK POINTER
        JSR     PRINT_BYTE      ;
        LDA     #$20            ; OUTPUT 2 SPACES
        JSR     IOF_OUTCH       ;
        LDA     #$20            ;
        JSR     IOF_OUTCH       ;
        LDA     PREG            ; OUTPUT STATUS REGISTER
        JSR     PRINT_BYTE      ; OUTPUT IN HEX
        LDA     #$0D            ; PRINT NEW LINE
        JMP     IOF_OUTCH       ;




;__COMMAND_PROCESSOR____________________________________________
;
; PROMPT FOR, INPUT, AND PROCESS INCOMMING USER COMMANDS
;
;_______________________________________________________________
COMMAND_PROCESSOR:

        JSR     DISPLAY_PROMPT  ; PRINT PROMPT STRING

        LDA     #<INBUFFER      ; SETUP INPUT COMMAND BUFFER
        STA     STRPTR          ;
        LDA     #>INBUFFER      ;
        STA     STRPTR +1       ;

        JSR     INSTR           ; GET A STRING FROM THE CONSOLE

        LDA     #$0D            ;
        JSR     IOF_OUTCH       ;
        LDA     #$0A            ;
        JSR     IOF_OUTCH       ;

        LDY     #$00            ; SET INDEX = 0

        LDA     #<COMMAND_LOOKUP_TABLE; SETUP INPUT COMMAND POINTER
        STA     WORKPTR         ;
        LDA     #>COMMAND_LOOKUP_TABLE;
        STA     WORKPTR +1      ;
        LDA     INBUFFER,Y      ; MOVE FIRST BYTE OF COMMAND BUFFER TO ACC
        CMP     #$00            ; IS NULL?
        BEQ     COMMAND_PROCESSOR; YES, GET NEXT COMMAND

COMMAND_PROCESSOR_CMP:
        LDX     #$00            ; X=0
        LDA     INBUFFER,Y      ; ACC= NEXT BYTE OF INPUT BUFFER
        CMP     (WORKPTR,X)     ; DOES NEXT BYTE OF INPUT BUFFER MATCH NEXT BYTE OF LOOKUP TABLE
        BNE     CMD_PROCESOR_NEXT_CMD; NO, GO TO NEXT COMMAND IN LOOKUP TABLE
        INY                     ; YES, Y=Y+1
        LDA     INBUFFER,Y      ; LOAD NEXT BYTE OF INPUT BUFFER
        CMP     #$20            ; IS IT A SPACE (SINGALING END OF COMMAND)
        BEQ     CMD_PROCESSOR_MATCH_FOUND; YES, POSSIBLE MATCH FOUND
        CMP     #$00            ; IS IT A NULL (SINGALING END OF COMMAND)
        BEQ     CMD_PROCESSOR_MATCH_FOUND; YES, POSSIBLE MATCH FOUND
        JSR     INCWORKPTR      ; NO, INCREMENT POINTER TO LOOKUP TABLE
        LDX     #$00            ;
        LDA     (WORKPTR,X)     ; A= NEXT BYTE OF LOOKUP TABLE
        CMP     #$00            ; IS IT A NULL? (SIGNALING END OF TABLE ENTRY)
        BEQ     CMD_PROCESOR_NEXT_CMD1; YES, ADVANCE TO NEXT COMMAND IN TABLE
        JMP     COMMAND_PROCESSOR_CMP; LOOP TO CHECK NEXT CHAR

CMD_PROCESOR_NEXT_CMD:
        JSR     INCWORKPTR      ; INCREMENT POINTER TO LOOKUP TABLE
        LDX     #$00            ;
        LDA     (WORKPTR,X)     ; A = NEXT BYTE OF LOOKUP TABLE
        CMP     #$00            ; IS IT A NULL?
        BNE     CMD_PROCESOR_NEXT_CMD; NO, LOOP

CMD_PROCESOR_NEXT_CMD1:
        LDA     #3
        JSR     INCWORKPTRX     ; INCREMENT POINTER TO LOOKUP TABLE
        LDX     #$00            ;
        LDA     (WORKPTR,X)     ; A = NEXT BYTE OF LOOKUP TABLE
        CMP     #$01            ; IS IT $01 (SINGALING END OF LOOKUP TABLE)
        BEQ     CMD_PROCESOR_NOT_FOUND; YES, DISPLAY NOT FOUND MESSAGE
        LDY     #$00            ; NO RESET INPUT BUFFER COUNTER
        JMP     COMMAND_PROCESSOR_CMP; LOOP

CMD_PROCESSOR_MATCH_FOUND:
        JSR     INCWORKPTR      ; INCREMENT POINTER TO LOOKUP TABLE
        LDX     #$00            ;
        LDA     (WORKPTR,X)     ; A = NEXT BYTE OF LOOKUP TABLE
        CMP     #$00            ; IS IT A NULL?
        BNE     CMD_PROCESOR_NEXT_CMD; NO, TRY NEXT COMMAND
        JSR     INCWORKPTR      ; YES, INCREMENT POINTER TO LOOKUP TABLE
        LDX     #$00            ;
        LDA     (WORKPTR,X)     ; A = NEXT BYTE OF LOOKUP TABLE
        STA     JUMPPTR         ; STORE A INTO LOW BYTE OF JUMP VECTOR
        JSR     INCWORKPTR      ; INCREMENT POINTER TO LOOKUP TABLE
        LDX     #$00            ;
        LDA     (WORKPTR,X)     ; A = NEXT BYTE OF LOOKUP TABLE
        STA     JUMPPTR+1       ; INCREMENT POINTER TO LOOKUP TABLE
        JSR     CMD_PROCESOR_RUN; RUN COMMAND
        JMP     COMMAND_PROCESSOR; GET NEXT COMMAND

CMD_PROCESOR_NOT_FOUND:
        LDA     #<ERROR         ; LOAD LOW BYTE OF ERROR STRING
        STA     STRPTR          ; STORE IN POINTER LOW BYTE
        LDA     #>ERROR         ; LOAD HOGH BYTE OF ERROR STRING
        STA     STRPTR +1       ; STORE IN POINTER HIGH BYTE

        JSR     OUTSTR          ; OUTPUT THE STRING
        JMP     COMMAND_PROCESSOR;
CMD_PROCESOR_RUN:
        JMP     (JUMPPTR)       ; JUMP TO COMMAND VECTOR

EXIT_MONITOR:
        JMP     ($FFFC)         ;


;__BOOT_________________________________________________________
;
; BOOT OS
;
; B X
;_______________________________________________________________
IOF_BOOT:
;
;	        LDA #(INBUFFER & $FF)   	; SETUP WORK BUFFER
;        	STA WORKPTR			;
;       	LDA #(INBUFFER >> 8)    	;
;      	STA WORKPTR +1 			;
;		LDA #4
;		JSR INCWORKPTRX			; JUMP OVER "BOOT"

;		JSR EATWHITESPACE		; SKIP OVER THE WHITESPACE
;        	JSR HEXIN			;
;       	STA UNIT			;

;      	LDA UNIT			;
;     	CMP #$01			;
;    	BEQ BOOTFDD
;
;   	LDA UNIT			;
;  	CMP #$04			;
; 	BEQ BOOTHDD

;          	JSR PPP_SOFT_RESET		;
;	LDA #$00
;    	STA debsec			;
;   	STA debcyl			;
;  	STA debhead			;
; 	JSR PPP_READ_SECTOR		;
;		JMP $0200			;
;
;BOOTHDD:
;
;		JSR IDE_SOFT_RESET		;
;		LDA #$00
;        	STA debsec			;
;       	STA debcyl			;
;      	STA debhead			;
;     	JSR IDE_READ_SECTOR		;
;		JMP $0200			;
;BOOTFDD:
;
;		lda #$01			;
;		STA sekdsk			;
;		STA UNIT			;
;		JSR SETUPDRIVE			;
;		LDA #$00
;         	STA debsec			;
;         	STA debcyl			;
;         	STA debhead			;
;		JSR READFL			;
;		JMP $0200			;
        BRK

;__GO______________________________________________________
;
; GO COMMAND
;
; GO XXXX
;_______________________________________________________________
GO:

        LDA     #<INBUFFER      ; SETUP WORK BUFFER
        STA     WORKPTR         ;
        LDA     #>INBUFFER      ;
        STA     WORKPTR +1      ;

        LDA     #2
        JSR     INCWORKPTRX     ; JUMP OVER "GO"

        JSR     EATWHITESPACE   ; SKIP OVER THE WHITESPACE
        JSR     GETNUMBER       ; GET THE STARTING ADDRESS
        BCS     DUMP_ERROR      ; IF NOT A NUMBER, REPORT ERROR

        JMP     (TEMPWORD)      ;




;__DUMPMEM______________________________________________________
;
; DUMP MEMORY COMMAND
;
; DUMP XXXX (XXXX)
;_______________________________________________________________
DUMP:

        LDA     #<INBUFFER      ; SETUP WORK BUFFER
        STA     WORKPTR         ;
        LDA     #>INBUFFER      ;
        STA     WORKPTR +1      ;

        LDA     #4
        JSR     INCWORKPTRX     ; JUMP OVER "DUMP"

        JSR     EATWHITESPACE   ; SKIP OVER THE WHITESPACE
        JSR     GETNUMBER       ; GET THE STARTING ADDRESS
        BCS     DUMP_ERROR      ; IF NOT A NUMBER, REPORT ERROR

        LDA     TEMPWORD        ; STORE STARTING ADDRESS IN WORD POINTER (TEMPWORD1)
        STA     TEMPWORD1       ;
        LDA     TEMPWORD+1      ;
        STA     TEMPWORD1+1     ;

        JSR     EATWHITESPACE   ; SKIP OVER ANY WHITESPACE
        JSR     GETNUMBER       ; GET THE ENDING ADDRESS
        BCS     DUMP_ERROR      ; IF NOT A NUMBER, REPORT ERROR

        LDA     TEMPWORD1       ; STORE ENDING ADDRESS IN WORD POINTER (WORKPTR)
        STA     WORKPTR         ;
        LDA     TEMPWORD1+1     ;
        STA     WORKPTR+1       ;
DUMP_LOOP:
        JSR     PRINT_MEM_LINE  ;
        LDA     #$0D            ;
        JSR     IOF_OUTCH       ;
        LDA     #$0A            ;
        JSR     IOF_OUTCH       ;
        LDA     WORKPTR+1       ; COMPARE HIGH BYTES
        CMP     TEMPWORD+1
        BCC     DUMP_LOOP       ; IF NUM1H < NUM2H THEN NUM1 < NUM2
        BNE     DUMP_DONE       ; IF NUM1H <> NUM2H THEN NUM1 > NUM2 (SO NUM1 >= NUM2)
        LDA     WORKPTR         ; COMPARE LOW BYTES
        CMP     TEMPWORD
        BCC     DUMP_LOOP       ; IF NUM1L < NUM2L THEN NUM1 < NUM2
DUMP_DONE:
        RTS
DUMP_ERROR:
        JMP     INVALID_NUMBER_ERROR



;__ENTERMEM_____________________________________________________
;
; ENTER MEMORY COMMAND
;
; ENTER XXXX (XX XX XX XX XX XX XX XX XX XX XX XX XX XX )
;_______________________________________________________________
ENTERMEM:

        LDA     #<INBUFFER      ; SETUP WORK BUFFER
        STA     WORKPTR         ;
        LDA     #>INBUFFER      ;
        STA     WORKPTR +1      ;

        LDA     #5
        JSR     INCWORKPTRX     ; JUMP OVER "ENTER"

        JSR     EATWHITESPACE   ; SKIP OVER ANY WHITESPACE
        JSR     GETNUMBER       ; GET NUMBER
        BCS     ENTER_ERROR     ; IF NOT A NUMBER REPORT ERROR

        LDA     TEMPWORD        ; STORE ADDRESS INTO WORD POINTER (TEMPWORD1)
        STA     TEMPWORD1       ;
        LDA     TEMPWORD+1      ;
        STA     TEMPWORD1+1     ;
        LDY     #$00            ; RESET COUNTER TO 0
ENTERLOOP:
        LDX     #$00            ;
        LDA     (WORKPTR,X)     ; GET NEXT BYTE FROM BUFFER
        CMP     #$00            ; IS NULL?
        BEQ     ENTER_DONE      ; YES, WE'RE DONE
        JSR     EATWHITESPACE   ; SKIP OVER ANY WHITESPACE
        JSR     GETNUMBER       ; GET NEXT NUMBER
        BCS     ENTER_ERROR     ; IF NOT A NUMBER REPORT ERROR
        LDA     TEMPWORD        ; STORE BYTE IN ADDRESS (INDEXED BY Y)
        STA     (TEMPWORD1),Y   ;
        INY                     ; GO TO NEXT BYTE
        JMP     ENTERLOOP       ; LOOP
ENTER_DONE:
        CPY     #$00            ; WAS LINE BLANK?
        BNE     ENTER_CONTINUE  ; NO, PREPARE FOR NEXT LINE
        RTS                     ; YES, END DATA ENTRY
ENTER_CONTINUE:
        LDA     #$0D            ;
        JSR     IOF_OUTCH       ;
        LDA     #$0A            ;
        JSR     IOF_OUTCH
        LDA     #$3A            ; OUTPUT ":" TO SCREEN
        JSR     IOF_OUTCH       ;
        CLC                     ; CLEAR CARRY
        TYA                     ; A=Y (LAST COUNTER)
        ADC     TEMPWORD1       ; ADD LAST COUNT TO BEGINNING POINTER
        STA     TEMPWORD1       ; STORE RESULT IN BEGINNING POINTER
        BNE     ENTER_INCREMENT ; NOT ZERO?, DONE
        INC     TEMPWORD1+1     ; ZERO, INC HIGH BYTE
ENTER_INCREMENT:
        LDA     TEMPWORD1+1     ; PRINTOUT STARTING ADDRESS TO SCREEN
        JSR     PRINT_BYTE      ; (HIGH)
        LDA     TEMPWORD1       ;
        JSR     PRINT_BYTE      ; (LOW)
        LDA     #$20            ; OUTPUT SPACE TO SCREEN
        JSR     IOF_OUTCH       ;
        LDA     #<INBUFFER      ; SETUP INPUT COMMAND BUFFER
        STA     STRPTR          ;
        LDA     #>INBUFFER      ;
        STA     STRPTR +1       ;
        JSR     INSTR           ; GET A STRING FROM THE CONSOLE
        LDA     #<INBUFFER      ; SETUP WORK BUFFER
        STA     WORKPTR         ;
        LDA     #>INBUFFER      ;
        STA     WORKPTR +1      ;
        LDY     #$00            ;
        JMP     ENTERLOOP       ; LOOP


ENTER_ERROR:
        JMP     INVALID_NUMBER_ERROR

;__PRINT_MEM_LINE_______________________________________________
;
; PRINT MEMORY DUMP LINE
;
; PRINT 16 HEX LOCATIONS STARTING WITH ADDRESS WORKPTR
;_______________________________________________________________
PRINT_MEM_LINE:
        LDA     #$3A            ; LOAD ':' INTO ACC
        JSR     IOF_OUTCH       ; PRINT ':'
        LDA     WORKPTR+1       ; PRINT ADDRESS
        JSR     PRINT_BYTE      ;
        LDA     WORKPTR         ;
        JSR     PRINT_BYTE      ;
        LDA     #$2D            ; LOAD '-'
        JSR     IOF_OUTCH       ; PRINT '-'
        LDY     #$00            ;
PRINT_MEM_LINE_LOOP:
        LDA     (WORKPTR),Y     ; LOAD NEXT BYTE
        JSR     PRINT_BYTE      ; PRINT BYTE
        LDA     #$20            ; LOAD ' '
        JSR     IOF_OUTCH       ; PRINT ' '
        INY                     ; INCREMENT COUNTER
        CPY     #$10            ; HAVE WE PRINTED 16 ADDRESSES
        BNE     PRINT_MEM_LINE_LOOP; NO, LOOP
        LDA     #$3A            ; LOAD ':' INTO ACC
        JSR     IOF_OUTCH       ; PRINT ':'
        LDX     #$00            ;
        LDY     #$00            ;
PRINT_MEM_LINE_LOOP_ASCII:
        LDA     (WORKPTR,X)     ; GET NEXT BYTE
        JSR     OUTASCII        ; PRINT ASCII VALUE OF BYTE
        INY                     ; INCREMENT COUNTER
        JSR     INCWORKPTR      ; INCREMENT BUFFER POINTER
        CPY     #$10            ; HAVE WE PRINTED 16 ADDRESSES
        BNE     PRINT_MEM_LINE_LOOP_ASCII; NO, LOOP
        LDA     #$0D            ; YES, PRINT CR
        JSR     IOF_OUTCH       ;
        RTS                     ; RETURN



;__DISPLAY_PROMPT______________________________________________
;
; DISPLAY THE INPUT PROMPT ON THE SCREEN
;
;______________________________________________________________
DISPLAY_PROMPT:
        LDA     #<PROMPT        ; LOAD LOW BYTE OF PROMPT STRING
        STA     STRPTR          ; STORE IN POINTER LOW BYTE
        LDA     #>PROMPT        ; LOAD HOGH BYTE OF PROMPR STRING
        STA     STRPTR +1       ; STORE IN POINTER HIGH BYTE

        JMP     OUTSTR          ; OUTPUT THE STRING


;__INCWORKPTR___________________________________________________
; INCREMENT THE 16BIT WORK POINTER
;
;_______________________________________________________________
INCWORKPTR:
        INC     WORKPTR         ; INCREMENT LOWBYTE
        BNE     :+              ; NOT ZERO?, DONE
        INC     WORKPTR+1       ; ZERO, INC HIGH BYTE
:
        RTS                     ; RETURN

;__INCWORKPTRX__________________________________________________
; INCREMENT THE 16BIT WORK POINTER X TIMES
;   A= NUMBER OF TIMES TO INCREMENT
;_______________________________________________________________
INCWORKPTRX:
        CLC
        ADC     WORKPTR         ; INCREMENT LOWBYTE
        BCC     :+              ; NO CARRY?
        INC     WORKPTR+1       ; CARRY, INC HIGH BYTE
:
        STA     WORKPTR         ; STORE
        RTS                     ; RETURN



;__INCTEMPWORD2__________________________________________________
;
; INCREMENT THE 16BIT WORK POINTER
;
;
;_______________________________________________________________
INCTEMPWORD2:
        INC     TEMPWORD2       ; INCREMENT LOWBYTE
        BNE     :+              ; NOT ZERO?, DONE
        INC     TEMPWORD2+1     ; ZERO, INC HIGH BYTE
:
        RTS                     ; RETURN



;__OUTASCII_____________________________________________________
;
; PRINT CHAR IF VALID, ELSE PRINT '.'
;
;_______________________________________________________________
OUTASCII:
        CMP     #$20            ; IS < 20
        BCC     :+              ; YES, SKIP
        CMP     #$80
        BCS     :+
        JMP     IOF_OUTCH       ; NO, PRINT CHAR AND RETURN
:
        LDA     #$2E            ; A= '.'
        JMP     IOF_OUTCH       ; PRINT '.' AND RETURN



;__INVALID_NUMBER_ERROR__________________________________________
;
; PRINT "INVALID HEX NUMBER MESSAGE"
;
;_______________________________________________________________
INVALID_NUMBER_ERROR:
        LDA     #<INERROR       ; LOAD LOW BYTE OF ERROR STRING
        STA     STRPTR          ; STORE IN POINTER LOW BYTE
        LDA     #>INERROR       ; LOAD HOGH BYTE OF ERROR STRING
        STA     STRPTR +1       ; STORE IN POINTER HIGH BYTE
        JMP     OUTSTR          ; OUTPUT THE STRING


;__GETNUMBER______________________________________________________
;
; GET ASCII NUMBER FROM BUFFER AND PARSE INTO TEMPWORD
;
;_______________________________________________________________
GETNUMBER:
        LDA     #$00            ;
        STA     TEMPWORD        ; CLEAR OUT TEMPWORD (OUTPUT OF GETNUMBER)
        STA     TEMPWORD+1      ;
        LDX     #$00            ;
GETNUMBER_LOOP:
        LDA     (WORKPTR,X)     ; GET NEXT BYTE FROM BUFFER
        CMP     #$20            ; IS SPACE?
        BEQ     GETNUMBER_DONE  ; YES, WE'RE DONE
        CMP     #$00            ; IS NULL?
        BEQ     GETNUMBER_DONE  ; YES, WE'RE DONE
        CMP     #$2C            ; IS ","?
        BEQ     GETNUMBER_DONE  ; YES, WE'RE DONE
        CMP     #$29            ; IS ")"?
        BEQ     GETNUMBER_DONE  ; YES, WE'RE DONE
        JSR     HEXIN           ; GET HEX DIGIT
        BCS     GETNUMBER_ERROR ; IS INVALID DIGIT?, YES PRINT ERROR AND ABORT
        CLC                     ; CLEAR CARRY
        ROL     TEMPWORD        ; MOVE WORD OVER 4 BITS TO LEFT
        ROL     TEMPWORD+1      ;
        CLC                     ;
        ROL     TEMPWORD        ;
        ROL     TEMPWORD+1      ;
        CLC                     ;
        ROL     TEMPWORD        ;
        ROL     TEMPWORD+1      ;
        CLC                     ;
        ROL     TEMPWORD        ;
        ROL     TEMPWORD+1      ;
        ORA     TEMPWORD        ; ADD IN NEW DIGIT
        STA     TEMPWORD        ; STORE BACK TO TEMPWORD
        JSR     INCWORKPTR      ; INCREMENT BUFFER POINTER
        JMP     GETNUMBER_LOOP  ; LOOP
GETNUMBER_ERROR:
        SEC                     ; SET ERROR FLAG (CARRY)
        RTS                     ; RETURN
GETNUMBER_DONE:
        CLC                     ; CLEAR ERROR FLAG (CARRY)
        RTS                     ; RETURN

;__HEXIN________________________________________________________
;
; GET NEXT CHAR FROM INPUT BUFFER AND CHANGE TO HEX DIGIT
;
; IF INVALID, SET CARRY FLAG
;_______________________________________________________________
HEXIN:
        LDX     #$00            ;
        LDA     (WORKPTR,X)     ; GET NEXT CHAR FROM BUFFER
        CMP     #$3A            ; LESS THAN 9?
        BCS     HEXIN_BIG       ; NO, SKIP NEXT
        SBC     #$2F            ; CONVERT 0-9
HEXIN_BIG:
        CMP     #$41            ; A OR MORE?
        BCC     HEXIN_SMALL     ; NO, SKIP NEXT
        SBC     #$37            ; CONVERT A-F
HEXIN_SMALL:
        CMP     #$10            ; RESULT TOO BIG?
        RTS


;__EATWHITESPACE___________________________________________________
;
; FORWARD THE BUFFER POINTER PAST ANY WHITE SPACE IN THE INPUT BUFFER
;
;_______________________________________________________________
EATWHITESPACE:
        LDX     #$00            ;
        LDA     (WORKPTR,X)     ; GET NEXT CHAR FROM BUFFER
        CMP     #$20            ; IS SPACE
        BNE     EATWHITESPACE_OUT; NO, DONE
        JSR     INCWORKPTR      ; YES, INCREMENT BUFFER POINTER
        JMP     EATWHITESPACE   ; LOOP
EATWHITESPACE_OUT:
        RTS                     ; RETURN


;__PRINT_BYTE__________________________________________________
;
; PRINT OUT ACCUMULATOR AS HEX NUMBER
;
;______________________________________________________________
PRINT_BYTE:
        TAX                     ; SAVE A REGISTER
        LSR     A               ; SHIFT HIGH NIBBLE TO LOW NIBBLE
        LSR     A               ;
        LSR     A               ;
        LSR     A               ;
        CLC                     ; CLEAR CARRY
        JSR     PRINT_DIGIT     ; PRINT LOW NIBBLE
        TXA                     ; RESTORE ACCUMULATOR
        JMP     PRINT_DIGIT     ; PRINT LOW NIBBLE

;__PRINT_DIGIT_________________________________________________
;
; PRINT OUT LOW NIBBLE OF ACCUMULATOR IN HEX
;
;______________________________________________________________
PRINT_DIGIT:
        AND     #$0F            ; STRIP OFF HIGH NIBBLE
        ORA     #$30            ; ADD $30 TO PRODUCE ASCII
        CMP     #$3A            ; IS GREATER THAN 9
        BMI     PRINT_DIGIT_OUT ; NO, SKIP ADD
        CLC                     ; CLEAR CARRY
        ADC     #$07            ; ADD ON FOR LETTER VALUES
PRINT_DIGIT_OUT:                ;
        JMP     IOF_OUTCH       ; PRINT OUT CHAR



;__OUTSTR______________________________________________________
;
; OUTPUT THE STRING POINTED TO BY OUTSTR TO THE SCREEN
;
;______________________________________________________________
OUTSTR:
        LDY     #$00            ; LOAD $00 INTO Y
OUTSTRLP:
        LDA     (STRPTR),Y      ; LOAD NEXT CHAR FROM STRING INTO ACC
        CMP     #$00            ; IS NULL?
        BEQ     ENDOUTSTR       ; YES, END PRINT OUT
        JSR     IOF_OUTCH       ; PRINT CHAR IN ACC
        INC     STRPTR
        BNE     OUTSTRLP
        INC     STRPTR+1
        JMP     OUTSTRLP        ; DO NEXT CHAR
ENDOUTSTR:
        RTS                     ; RETURN

;__INSTR_______________________________________________________
;
; INPUT STRING FROM KEYBOARD INTO KEYBOARD BUFFER
;
;______________________________________________________________
INSTR:
        LDY     #$00            ; LOAD $00 INTO Y
INSTRLP:
        JSR     IOF_CONINW
        CMP     #$60            ; IS LOWER CASE
        BCC     INSTRUC         ; NO, SKIP
        AND     #$DF            ; TO UPPER CASE
INSTRUC:
        CMP     #$0D            ; IS CR?
        BEQ     ENDINSTR        ; YES, DONE WITH INPUT
        CMP     #$08            ; IS BACKSPACE?
        BNE     INSTR_NOTBS     ; NO, SKUP BACKSPACE RTN
        CPY     #$00            ; IS INDEX =0 ?
        BEQ     INSTR_EMPTY_BS  ; YES, SKIP BACKSPACE
        JSR     IOF_OUTCH       ; OUTPUT CHAR TO SCREEN
        DEY                     ; Y=Y-1
        LDA     #$00            ;
        STA     (STRPTR),Y      ; NULL TERMINATE INPUT BUFFER
        DEY                     ; Y=Y-1
        JMP     INSTR_SKIP_STORE; SKIP STORE OF CHAR TO INPUT BUFFER
INSTR_NOTBS:
        STA     (STRPTR),Y      ; STORE CHAR IN KEYBAORD BUFFER
        JSR     IOF_OUTCH       ; OUTPUT CHAR TO SCREEN
INSTR_SKIP_STORE:
        INY                     ; Y=Y+1
        CPY     #$FF            ; DOES Y=$FF
        BNE     INSTRLP         ; NO, LOOP FOR NEXT CHAR
ENDINSTR:
        LDA     #$00            ; A=0
        STA     (STRPTR),Y      ; NULL TERMINATE INPUT BUFFER
        RTS
INSTR_EMPTY_BS:
        LDA     #$00            ; BLANK OUT KEYBOARD CHAR, TO SIGNAL READY FOR NEXT CHAR
        JMP     INSTRLP         ; JUMP TO INPUT LOOP


;__LOAD_________________________________________________________

; LOAD A MOTOROLA FORMATTED HEX FILE
;
;_______________________________________________________________
LOAD:
        JSR     RDSER1W         ;
        CMP     #'S'            ;
        BNE     LOAD            ; FIRST CHAR NOT (S)
        JSR     RDSER1W         ; READ CHAR
        CMP     #'9'            ;
        BEQ     LOAD21          ;
        CMP     #'1'            ;
        BNE     LOAD            ; SECOND CHAR NOT (1)
        LDA     #$00            ;
        STA     CKSM            ; ZERO CHECKSUM
        JSR     LGETBYTE        ; READ BYTE
        SBC     #$01            ;
        STA     BYTECT          ; BYTE COUNT
        JSR     BADDR           ; BUILD ADDRESS
        LDY     #$00            ;
LOAD11:
        JSR     LGETBYTE        ;
        DEC     BYTECT          ;
        BEQ     LOAD15          ; ZERO BYTE COUNT
        STA     (TEMPWORD1),Y   ; STORE DATA
        JSR     INCTEMPWORD1    ;
        JMP     LOAD11          ;

LOAD15:
        INC     CKSM            ;
        BEQ     LOAD            ;
LOAD19:
        LDA     #'?'            ;
        JSR     WRSER1          ;
LOAD21:
        RTS
LGETBYTE:
        JSR     INHEX           ; GET HEX CHAR
        ASL     A               ;
        ASL     A               ;
        ASL     A               ;
        ASL     A               ;
        STA     TEMPBYTE        ;
        JSR     INHEX           ;
        AND     #$0F            ; MASK TO 4 BITS
        ORA     TEMPBYTE        ;
        PHA                     ;
        CLC                     ;
        ADC     CKSM            ;
        STA     CKSM            ;
        PLA                     ;
        RTS                     ;
; INPUT HEX CHAR
INHEX:
        JSR     RDSER1W         ;
        PHA                     ;
        JSR     WRSER1          ;
        PLA                     ;
        CMP     #$3A            ; LESS THAN 9?
        BCS     INHEX_BIG       ; NO, SKIP NEXT
        SBC     #$2F            ; CONVERT 0-9
INHEX_BIG:
        CMP     #$41            ; A OR MORE?
        BCC     INHEX_SMALL     ; NO, SKIP NEXT
        SBC     #$37            ; CONVERT A-F
INHEX_SMALL:
        RTS                     ;

; BUILD ADDRESS
BADDR:
        JSR     LGETBYTE        ; READ 2 FRAMES
        STA     TEMPWORD1+1     ;
        JSR     LGETBYTE        ;
        STA     TEMPWORD1       ;
        RTS


INCTEMPWORD1:
        INC     TEMPWORD1       ; INCREMENT LOWBYTE
        BNE     INCTEMPWORD1_OUT; NOT ZERO?, DONE
        INC     TEMPWORD1+1     ; ZERO, INC HIGH BYTE
INCTEMPWORD1_OUT:
        RTS                     ; RETURN


;__IOF_CONIN____________________________________________________
;
; read a byte from CONSOLE ('A' POINTS TO BYTE)
;_______________________________________________________________
IOF_CONIN:
        LDA     CONSOLE
        CMP     #$01
        BEQ     CONINSERIAL
        JMP     PPPCONIN
CONINSERIAL:
        JMP     RDSER1
        RTS

;__IOF_CONINW____________________________________________________
;
; read a byte from CONSOLE ('A' POINTS TO BYTE, WAIT FOR BYTE)
;_______________________________________________________________
IOF_CONINW:
        LDA     CONSOLE
        CMP     #$01
        BEQ     CONINSERIALW
        JMP     PPPCONINW
CONINSERIALW:
        JMP     RDSER1W
        RTS

;__IOF_OUTCH____________________________________________________
;
; write a byte from CONSOLE  ('A' POINTS TO BYTE)
;_______________________________________________________________
IOF_OUTCH:
        PHA
        LDA     CONSOLE
        CMP     #$01
        BEQ     CONOUTSERIAL
        PLA
        JMP     PPPOUTCH
CONOUTSERIAL:
        PLA
        JMP     WRSER1
        RTS

;__IOF_CONSTATUS________________________________________________
;
; RETURN CONSOLE STATUS
;______________________________________________________________
IOF_CONSTATUS:
        LDA     CONSOLE
        CMP     #$01
        BEQ     CONSTATUSSERIAL
        JMP     PPPCONSTATUS
CONSTATUSSERIAL:
        JMP     SERIALSTATUS
        RTS


;__INITPAGES____________________________________________________
;
; SETUP MMU FOR BIOS PAGED MEMORY OPERATION
;
; SETUP:
; 	TASK 0, NORMAL OPERATION
;   TASK 1, ADDITIONAL ROM DRIVERS PAGED INTO C000-D000
;	TASKS 2-15 -- OPEN FOR OS/USER USE
;_______________________________________________________________
INITPAGES:
        LDA     #$00            ; ENSURE MMU IS DISABLED (SHOULD BE ALREADY, BUT . . . )
        STA     M6X0X_MMU_ENA
        STA     M6X0X_MAP_SETUP ; START WITH TASK 0
        JSR     INITPAGE        ; FILL TASK 0 WITH A 1:1 MAP
        LDA     #$01
        STA     M6X0X_MAP_SETUP ; NOW TASK 2
        JSR     INITPAGE        ; FILL TASK 2 WITH A 1:1 MAP
        LDA     #$8C            ; BUT, MAP Cxxx AND Dxxx TO ROM Cxxx AND Dxxx RATHER THAN RAM
        STA     M6X0X_MAP_SPACE+$0C
        LDA     #$8D
        STA     M6X0X_MAP_SPACE+$0D
;
        LDA     #$00
        STA     M6X0X_ACT_TASK  ; SET ACTIVE TASK TO 00
        LDA     #$01
        STA     M6X0X_MMU_ENA   ; ENABLE MMU --- FEEEEEL THE POOOOWERRRR
        RTS


INITPAGE:
        LDX     #$00
:
        TXA
        STA     M6X0X_MAP_SPACE,X; CREATE A 1:1 MAP OF BANK
        INX
        CPX     #$10
        BNE     :-
        RTS


        RTS
; COMMAND PROCESSOR JUMP TABLE
COMMAND_LOOKUP_TABLE:
        .BYTE   "REGISTER",0,<PRINT_REG,>PRINT_REG
        .BYTE   "DUMP",0,<DUMP,>DUMP
        .BYTE   "ENTER",0,<ENTERMEM,>ENTERMEM
        .BYTE   "GO",0,<GO,>GO
        .BYTE   "LOAD",0,<LOAD,>LOAD
        .BYTE   "BOOT",0,<IOF_BOOT,>IOF_BOOT
        .BYTE   "DISASSEMBLE",0,<DISASSEMBLE,>DISASSEMBLE
        .BYTE   "ASSEMBLE",0,<ASSEMBLE,>ASSEMBLE
        .BYTE   01,0
; COMMAND PROMPT STRING
PROMPT:
        .BYTE   $0D,$0A,".",0
; ERROR STRING
ERROR:
        .BYTE   $0D,$0A,"? BAD COMMAND",$0D,0
INERROR:
        .BYTE   $0D,$0A,"? BAD HEX NUMBER",$0D,0
; STRINGS FOR REGISTER DISPLY
REGDATA:
        .BYTE   $0D,$0A
        .BYTE   "   PC  AC  XR  YR  SP  SR"
        .BYTE   $0D,$0A,"! ",0


STARTUP:
        .BYTE   $0D,$0A

        .BYTE   "  RetroBrew Computers 6x0x",$0D,$0A,$0D,$0A
        .BYTE   " .d8888b.            .d8888b. ",$0D,$0A
        .BYTE   "d88P  Y88b          d88P  Y88b ",$0D,$0A
        .BYTE   "888                 888    888 ",$0D,$0A
        .BYTE   "888d888b.  888  888 888    888 888  888 ",$0D,$0A
        .BYTE   "888P  Y88b `Y8bd8P' 888    888 `Y8bd8P' ",$0D,$0A
        .BYTE   "888    888   X88K   888    888   X88K ",$0D,$0A
        .BYTE   "Y88b  d88P .d8  8b. Y88b  d88P .d8  8b. ",$0D,$0A
        .BYTE   "  Y8888P   888  888   Y8888P   888  888 ",$0D,$0A,$0D,$0A
        .BYTE   "  6502 MONITOR",$0D,$0A,$00

;BIOS JUMP TABLE
        .SEGMENT "JUMPTABLE"
        .ORG    $FD00           ; JUMP TABLE LOCATION
        JMP     IOF_CONIN       ; read a byte from CONSOLE ('A' POINTS TO BYTE)
        JMP     IOF_CONINW      ; read a byte from CONSOLE ('A' POINTS TO BYTE, WAIT FOR BYTE)
        JMP     IOF_OUTCH       ; write a byte from CONSOLE  ('A' POINTS TO BYTE)
        JMP     IOF_CONSTATUS   ; RETURN CONSOLE STATUS
        JMP     SERIALINIT      ; called during OS init
        JMP     RDSER1          ; read a byte from serial port ('A' POINTS TO BYTE)
        JMP     WRSER1          ; write a byte from serial port  ('A' POINTS TO BYTE)
        JMP     RDSER1W         ; read a byte from serial port ('A' POINTS TO BYTE, WAIT FOR INPUT)
        JMP     SERIALSTATUS    ; GET UART STATUS
        JMP     SETUPDRIVE      ; init floppy drive
        JMP     READFL          ; read sector from floppy
        JMP     WRITEFL         ; write sector to floppy
        JMP     PPP_SOFT_RESET  ; reset ppp sd drive
        JMP     PPP_READ_SECTOR ; read ppp sd drive sector
        JMP     PPP_WRITE_SECTOR; write ppp sd drive sector
        JMP     IDE_SOFT_RESET  ; reset ide drive
        JMP     IDE_READ_SECTOR ; ide read sector
        JMP     IDE_WRITE_SECTOR; ide write sector







        .SEGMENT "VECTORS"
NNTVECTOR:
        .WORD   NINTERRUPT      ;
RSTVECTOR:
        .WORD   COLD_START      ;
INTVECTOR:
        .WORD   INTERRUPT       ; ROM VECTOR FOR IRQ

        .END
