
;__IDEBOOT_________________________________________________________________________________________
;
;	Stage two boot for 6502 DOS/65, IDE Drive
;
;	WRITTEN BY: DAN WERNER -- 5/30/2011
;
;__________________________________________________________________________________________________
;
; DATA CONSTANTS
;__________________________________________________________________________________________________
;REGISTER		IO PORT		; FUNCTION

IDE_SOFT_RESET	.EQU	$FFE0
IDE_SETUP_LBA	.EQU	$FFE3
IDE_READ_SECTOR	.EQU	$FFE6
SETUPDRIVE	.EQU	$FFE9	
READFL		.EQU	$FFEC


IDERESETVEC	.EQU 	$FFEA
IDESETUPVEC	.EQU 	$FFEC
IDEREADVEC	.EQU 	$FFEE		
PRINTVEC	.EQU	$FFF0
INPVEC		.EQU	$FFF2
INPWVEC		.EQU	$FFF4	
FLPSETUPVEC    	.EQU 	$FFF6			
FLPREADVEC     	.EQU 	$FFF8			


IRQVECTOR   	.EQU   	$30   		; VECTOR FOR USER IRQ RTN       
WORKPTR		.EQU   	$32		; WORK POINTER FOR COMMAND PROCESSOR		
JUMPPTR		.EQU	$34		; JUMP VECTOR FOR LOOKUP TABLE	
TEMPWORD	.EQU	$36		;
TEMPWORD1	.EQU   	$38		;
TEMPWORD2	.EQU   	$3A		;
TEMPBYTE	.EQU	$3B		;
ACC      	.EQU   	$3D		; ACC STORAGE
XREG     	.EQU   	$3E 		; X REG STORAGE
YREG     	.EQU   	$3F 		; Y REG STORAGE
PREG     	.EQU   	$40 		; CURRENT STACK POINTER
PCL      	.EQU   	$41 		; PROGRAM COUNTER LOW
PCH      	.EQU   	$42 		; PROGRAM COUNTER HIGH
SPTR     	.EQU   	$43 		; CPU STATUS REGISTER
CKSM		.EQU	$44		; CHECKSUM
BYTECT		.EQU	$45		; BYTE COUNT
STRPTR	 	.EQU	$48		;
COUNTER	 	.EQU	$4A		;
SRC	 	.EQU	$4C		;
DEST	 	.EQU	$4E		;
FLERR	 	.EQU	$4F		;
UNIT	 	.EQU	$50		;
TRACK	 	.EQU	$51		;
HEAD	 	.EQU	$52		;
FCMD	 	.EQU	$53		;
ST0	 	.EQU	$54		;
FLATCH_STORE	.EQU	$55		;
FLRETRY	 	.EQU	$56		;
SECTOR	 	.EQU	$57		;
	

INBUFFER	.EQU	$0200		; DISK BUFFER
STARTADDRESS	.EQU	$CA00		; OS DEST ADDRESS
BOOTSECTORS	.EQU	$13		; NUMBER OF SECTORS IN OS
STARTOS		.EQU	$DE00		; VECTOR TO START OS
BOOTCODESTART	.EQU	$0800		; LOCATION BOOT CODE WILL RUN IN


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
		JSR IDE_SETUP_LBA		;
		JSR IDE_READ_SECTOR		;
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
		LDA #$01			;
		BNE BOOTLOOP			;
				
EXITBOOT:
		JMP STARTOS			; RUN THE OS

		

	.END
	
	