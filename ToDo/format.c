/*
 *   Clear directory sectors from media.
 *
 * 
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <conio.h>
#include <dbg.h>
#include "DOS65.h"




/*****************************************************************************/
/*     	      	    	  	     Data	     			     */
/*****************************************************************************/

  char *Buffer;
  char *Buffer1;
  char *Memory=0;


/*****************************************************************************/
/*     	      	    	  	     Code	     			     */
/*****************************************************************************/



int main (void)
{
    int	x=0;
    int DrivePointer;
    char Drive;
    int StartingSector;
    int SecPerTrack;
    int NumDirSectors;
    int Track;
    int Sector;

   
    Buffer=malloc(256);
    Buffer1=malloc(256);
    
    
    	DOS65_ConsoleOut("Drive to Clear Directory:$");
	DOS65_ConsoleIn(Buffer);
	
	Drive = *(Buffer+2);

	if (Drive>140) Drive=Drive-76;  /* Convert to Upper case if required */
	Drive=Drive-65;
	
	if(Drive>3)
	{	
		DOS65_ConsoleOut("Invalid Drive Specified, Drive must be A-D\n\r$");
		return 0;
	}

  	DOS65_SetDrive(Drive);
	DrivePointer=DOS65_GetDPB();

	DOS65_ConsoleOut("\n\r$");
        	
        StartingSector=(int) (*(Memory+DrivePointer+4) + (*(Memory+DrivePointer+5))*256);
        SecPerTrack=(int) (*(Memory+DrivePointer+2)+ (*(Memory+DrivePointer+3))*256);
        NumDirSectors=(int) (*(Memory+DrivePointer+7)+ (*(Memory+DrivePointer+8))*256);
        NumDirSectors=(NumDirSectors/4)+1;

        sprintf(Buffer1,"Starting Track=%i\n\r$",StartingSector);
	DOS65_ConsoleOut(Buffer1);
        sprintf(Buffer1,"Sectors Per Track=%i\n\r$",SecPerTrack);
	DOS65_ConsoleOut(Buffer1);
        sprintf(Buffer1,"Number of Directory Sectors=%i\n\r$",NumDirSectors);
	DOS65_ConsoleOut(Buffer1);

	        
        for(x=0;x<128;x++) *(Buffer+x)=0xE5;
        	
	Sector=0;
	Track=StartingSector;
 	for(x=0;x<NumDirSectors;x++)
  	{
	  	DOS65_WriteLowLevelSector(Drive,Track,Sector,Buffer);
	  	Sector++;
		if(Sector==SecPerTrack)
		{
			Sector=0;
			Track++;
		}
       	        sprintf(Buffer1,"Track %i,Sector %i                  \r$",Track,Sector);
        	DOS65_ConsoleOut(Buffer1);

  	}
       	        sprintf(Buffer1,"Erase Complete.                  \n\r$");
        	DOS65_ConsoleOut(Buffer1);

     
    /* Done */
    return 0;
}




