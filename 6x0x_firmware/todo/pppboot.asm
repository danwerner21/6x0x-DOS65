
;__PPPBOOT_________________________________________________________________________________________
;
;	Stage two boot for 6502 DOS/65
;
;	WRITTEN BY: DAN WERNER -- 1/1/2013
;
;__________________________________________________________________________________________________
;
; DATA CONSTANTS
;__________________________________________________________________________________________________
;REGISTER		IO PORT		; FUNCTION


COLOSSUS6X0X	.EQU	1	; USE COLOSSUS 6X0X HARDWARE
ORIGINAL6X0X	.EQU	0	; USE ORIGINAL 6X0X HARDWARE

WRITEPPPBSEC  	.EQU	0	; WRITE BOOT SECTOR TO SD CARD
WRITEFLPBSEC  	.EQU	0	; WRITE BOOT SECTOR TO FLOPPY
WRITEIDEBSEC  	.EQU	1	; WRITE BOOT SECTOR TO IDE DRIVE


.IF WRITEPPPBSEC=1
SOFT_RESET	.EQU	$FFE9
READ_SECTOR	.EQU	$FFEC
WRITE_SECTOR	.EQU	$FFDA
.ENDIF

.IF WRITEFLPBSEC=1
SOFT_RESET	.EQU	$FFD1
READ_SECTOR	.EQU	$FFD4
WRITE_SECTOR	.EQU	$FFD7
.ENDIF

.IF WRITEIDEBSEC=1
SOFT_RESET	.EQU	$FFE0
READ_SECTOR	.EQU	$FFE3
WRITE_SECTOR	.EQU	$FFE6
.ENDIF


WORKPTR		.EQU   	$32		; WORK POINTER FOR COMMAND PROCESSOR
TEMPWORD	.EQU	$36		;
TEMPWORD1	.EQU   	$38		;
COUNTER		.EQU	$45		; COUNTER

UNIT	 	.EQU	$50		;
TRACK		.EQU	$51		;
HEAD	 	.EQU	$52		;
SECTOR	 	.EQU	$53		;



INBUFFER	.EQU	$0200		; DISK BUFFER
BOOTSECTORS	.EQU	$13		; NUMBER OF SECTORS IN OS
BOOTCODESTART	.EQU	$0800		; LOCATION BOOT CODE WILL RUN IN

	.IF ORIGINAL6X0X=1
STARTADDRESS	.EQU	$CA00		; OS DEST ADDRESS
STARTOS		.EQU	$DE00		; VECTOR TO START OS
	.ENDIF


	.IF COLOSSUS6X0X=1
STARTADDRESS	.EQU	$B800		; OS DEST ADDRESS
STARTOS		.EQU	$CC00		; VECTOR TO START OS
	.ENDIF

	.ORG	$0200
;_______________________________________________________________
;
; RELOACTE CODE FROM DISK BUFFER, AND JUMP
;
;_______________________________________________________________

       	        LDA #(INBUFFER & $FF)   	; SETUP DISK BUFFER
         	STA WORKPTR			;
         	LDA #(INBUFFER >> 8)    	;
         	STA WORKPTR +1 			;
	        LDA #(BOOTCODESTART & $FF)   	; SETUP OS LOAD LOCATION
         	STA TEMPWORD1			;
         	LDA #(BOOTCODESTART >> 8)  	;
         	STA TEMPWORD1 +1 		;
         	LDY #$00			;
RELOCATELOOP:
		LDA (WORKPTR),Y			;
		STA (TEMPWORD1),Y		;
		INY				;
		BNE RELOCATELOOP		;
		INC TEMPWORD1+1			;
		INC WORKPTR+1			;
RELOCATELOOP1:
		LDA (WORKPTR),Y			;
		STA (TEMPWORD1),Y		;
		INY				;
		BNE RELOCATELOOP1		;


		JMP BOOTCODESTART+(BOOT-$0200)


;__BOOT_________________________________________________________
;
; PERFORM SYSTEM BOOT  -- CODE MUST BE RELOCATABLE!
;
;_______________________________________________________________
BOOT:
         	LDA #$00
         	STA TRACK			; SET BOOT CODE LOCATION
         	STA HEAD			; SET BOOT CODE LOCATION
         	STA COUNTER			; SET SECTOR COUNTER
          	LDA #$01			;
         	STA SECTOR			; SET BOOT CODE LOCATION
         					;
	        LDA #(STARTADDRESS & $FF)   	; SETUP OS LOAD LOCATION
         	STA TEMPWORD1			;
         	LDA #(STARTADDRESS >> 8)  	;
         	STA TEMPWORD1 +1 		;

BOOTLOOP:
         	LDA #(INBUFFER & $FF)   	; SETUP DISK BUFFER
         	STA WORKPTR			;
         	LDA #(INBUFFER >> 8)    	;
         	STA WORKPTR +1 			;
         					;
		JSR READ_SECTOR			;
		LDY #$00			;
MOVELOOP:
		LDA (WORKPTR),Y			;
		STA (TEMPWORD1),Y		;
		INY				;
		BNE MOVELOOP			;
		INC TEMPWORD1+1			;
		INC WORKPTR+1			;
MOVELOOP1:
		LDA (WORKPTR),Y			;
		STA (TEMPWORD1),Y		;
		INY				;
		BNE MOVELOOP1			;
		INC TEMPWORD1+1			;
		INC COUNTER			;
		LDA COUNTER			;
		CMP #BOOTSECTORS		;
		BEQ EXITBOOT			;
		INC SECTOR			;
.IF WRITEFLPBSEC=1
		LDA SECTOR			;
		CMP #$09			;
		BNE SKIPA			;
		LDA #$00			;
		STA SECTOR			;
		INC HEAD			;
		LDA HEAD			;
		CMP #$02			;
		BNE SKIPA			;
		LDA #$00			;
		STA HEAD			;
		INC TRACK			;
SKIPA:						;
.ENDIF						;
		LDA #$01			;
		BNE BOOTLOOP			;

EXITBOOT:
		JMP STARTOS			; RUN THE OS


	.ORG	$0300
;_______________________________________________________________
;
; 	STORE CODE ON BOOT SECTOR
;
;_______________________________________________________________
	.IF WRITEFLPBSEC=1
		LDA #$01
		STA UNIT
	.ENDIF
		JSR SOFT_RESET
         	LDA #$00
         	STA TRACK			; SET BOOT CODE LOCATION
         	STA HEAD			; SET BOOT CODE LOCATION
         	STA SECTOR			; SET BOOT CODE LOCATION

		JSR WRITE_SECTOR		;
		BRK

	.END
