
#ifndef __rbc_H
#define __rbc_H

#define DISKCFG (*(unsigned int *)0x002e)
#define DCBPTR  (*(unsigned int *)0x002c)
#define CMDLINE (*(unsigned int *)0x0030)

extern void __fastcall__ cputc(char c);
extern void __fastcall__ cputserial(char c);
extern char __fastcall__ cgetc();
extern int  __fastcall__ cgetserial();
extern unsigned char __fastcall__ readrtc(unsigned char address);
extern void __fastcall__ writertc(unsigned char address,unsigned char value);

#endif