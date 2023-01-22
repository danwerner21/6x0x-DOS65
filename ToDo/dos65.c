/*
 * DOS/65 Utility Functions
 *
 * 
 *
 */

#include "DOS65.h"


/*****************************************************************************/
/*     	      	    	  	     Data	     			     */
/*****************************************************************************/

int	Result;



/*****************************************************************************/
/*     	      	    	  	     Code	     			     */
/*****************************************************************************/



void DOS65_WriteLowLevelSector(char Drive ,int Track,int Sector ,char *DMABuffer)
{
	/* Select Disk */	
	__asm__ ("ldy #%o", Drive);
	__asm__ ("lda (sp),y");
	__asm__ ("jsr $DE1B ");  /* SIM = SELDSK */          

	
	/* Select Track */		
	__asm__ ("ldy #%o+1", Track);
	__asm__ ("jsr     ldaxysp");
	__asm__ ("pha");
	__asm__ ("txa");
	__asm__ ("tay");
	__asm__ ("pla");
	__asm__ ("jsr $DE1E ");  /* SIM = SELTRK */          
	
	
	/* Select Sector */
	__asm__ ("ldy #%o+1", Sector);
	__asm__ ("jsr     ldaxysp");
	__asm__ ("pha");
	__asm__ ("txa");
	__asm__ ("tay");
	__asm__ ("pla");
	__asm__ ("jsr $DE21 ");  /* SIM = SELSEC */          

	
	/* Set Buffer */
	__asm__ ("ldy #%o+1", DMABuffer);
	__asm__ ("jsr     ldaxysp");
	__asm__ ("pha");
	__asm__ ("txa");
	__asm__ ("tay");
	__asm__ ("pla");
	__asm__ ("jsr $DE24 ");  /* SIM = SETDMA */          

	
	/* Write Sector */
	Result=255;
/*	while(Result)*/
	{
	__asm__ ("jsr $DE2A ");  /* SIM = WRITE */          
	__asm__ ("sta %v",Result);   		
	}
		
	
	
}

void DOS65_SetDrive(char Drive)
{
	/* Select Disk */	
	__asm__ ("ldy #%o", Drive);
	__asm__ ("lda (sp),y");
	__asm__ ("ldx #14");
	__asm__ ("jsr $0103 ");           	
}

int DOS65_GetDPB()
{
	/* Get Disk */	
	__asm__ ("ldx #34");
	__asm__ ("jsr $0103 ");  
	__asm__ ("sta _Result");
	__asm__ ("sty _Result+1");  
		
	return Result;
}



