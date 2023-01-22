/*
 * DOS/65 Utility Functions
 *
 * 
 *
 */

extern void __fastcall__ DOS65_ConsoleOut (char *str);
extern void __fastcall__ DOS65_ConsoleIn (char *str);
extern void DOS65_SetDrive(char );
extern int  DOS65_GetDPB();
extern void DOS65_warmBoot ();
extern void DOS65_WriteLowLevelSector(char ,int ,int ,char * );



