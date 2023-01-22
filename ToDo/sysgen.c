/*
 * SYSGEN  -- load DOS/65 system onto media for booting
 *
 * Dan Werner (5/30/2011)
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <conio.h>
#include <dbg.h>
#include "DOS65.h"
#include "BOOTA.h"
#include "BOOTC.h"



/*****************************************************************************/
/*     	      	    	  	     Data	     			     */
/*****************************************************************************/

  char *Buffer;
  char *Buffer1;
  char *Memory=0;


/*****************************************************************************/
/*     	      	    	  	     Code	     			     */
/*****************************************************************************/



int main ()
{
	int Track;
	int Sector;
	int x=0;
	int y=0;
	char Drive;
	int SourceCounter=0;
	Buffer=malloc(256);
	Buffer1=malloc(256);
	
	DOS65_ConsoleOut("Drive to load system:$");
	DOS65_ConsoleIn(Buffer);
	
	Drive = *(Buffer+2);

	if (Drive>140) Drive=Drive-76;  /* Convert to Upper case if required */
	Drive=Drive-65;
	
	if(Drive>3)
	{	
		DOS65_ConsoleOut("Invalid Drive Specified, Drive must be A-D\n\r$");
		return 0;
	}
	

	sprintf(Buffer1,"Installing Boot Sector on Drive %c:\n\r$",Drive+65);
        DOS65_ConsoleOut(Buffer1);
        
	
	for(y=0;y<4;y++)
	{
		if(Drive==2)
		{
			for(x=0;x<128;x++) 
			{
				  *(Buffer+x)=BOOTC[SourceCounter];	
				  SourceCounter++;
			}
		}
		else
		{
			for(x=0;x<128;x++) 
			{
				  *(Buffer+x)=BOOTA[SourceCounter];	
				  SourceCounter++;
			}
		}        	        	
       	        DOS65_WriteLowLevelSector(Drive,0,y,Buffer);
	}
        	
	sprintf(Buffer1,"Installing DOS/65 on Drive %c:\n\r$",Drive+65);
        DOS65_ConsoleOut(Buffer1);
	SourceCounter=0xCA00;
	
	Track=0;
	Sector=3;
	
	while(SourceCounter<0xF000)
	{
		Sector++;
		if(Drive==2)
		{
			if(Sector==256)
			{
				Sector=0;
				Track++;
			}
		}
		else
		{
			if(Sector==36)
			{
				Sector=0;
				Track++;
			}			
		}
		
		
		for(x=0;x<128;x++) 
			{
				  *(Buffer+x)=*(Memory+SourceCounter);	
				  SourceCounter++;
			}
	        	        	
       	        DOS65_WriteLowLevelSector(Drive,Track,Sector,Buffer);	
       	        sprintf(Buffer1,"Track %i,Sector %i,OS Address %x \r$",Track,Sector,SourceCounter);
        	DOS65_ConsoleOut(Buffer1);
	}
        sprintf(Buffer1,"OPERATION COMPLETE.                               \n\r\n\r$",*(Buffer+0),*(Buffer+1),*(Buffer+2),*(Buffer+3),*(Buffer+4),*(Buffer+5),*(Buffer+6),*(Buffer+7));
        DOS65_ConsoleOut(Buffer1);
		
	        
    /* Done */
    return 0;
}




