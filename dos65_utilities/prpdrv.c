#include <conio.h>
#include <string.h>
#include <stdlib.h>
#include "rbc.h"

char hdddcb[9] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

unsigned char secbuffer[129];
unsigned char parms[5];

void prtusage();
void prtdevice(char);
void prttable(char *);
int parsecmd(char *, char *, char *, char *);
void mapdrive(char *, char *, char *, char *);
void getdosmap(char drive, char dcb[]);
void toupper(char *);
void cgets(char *buffer, int length);
void clearbuff();
void simjumper(unsigned char acc, unsigned char yreg, char offset);
void clearSector(char drive, unsigned int track, unsigned int sector);
void simbuffer(char offset);

int main()
{
        unsigned int **dskcfgptr = DISKCFG;
        unsigned int frmCounter, toCounter, secper, direntries, t, s;
        char drive;
        char buffer[80];

        prttable((unsigned char *)dskcfgptr);

        cputs("\n\r CHOOSE DRIVE TO PREP FOR DOS/65:");
        cgets(buffer, 79);

        drive = (buffer[0] & 0x5F) - 65;
        getdosmap(drive, hdddcb);

        secper = hdddcb[2] + (hdddcb[3] * 256);
        frmCounter = hdddcb[4] + (hdddcb[5] * 256);
        direntries = hdddcb[7] + (hdddcb[8] * 256) + 1;
        cprintf("\n\r\n\r Prepping drive %c:\n\r", buffer[0]);
        cprintf("    Max blocks: %u\n\r", hdddcb[0] + (hdddcb[1] * 256));
        cprintf("    Sectors per track: %u\n\r", secper);
        cprintf("    System tracks: %u\n\r", frmCounter);
        cprintf("    Block size: %u\n\r", hdddcb[6]);
        cprintf("    Max directory entries: %u\n\r", direntries);

        toCounter = frmCounter + ((direntries / 4) / secper) - 1;
        cprintf("\n\r Clearing sectors 1-%u on tracks %u-%u\n\r", secper, frmCounter, toCounter);

        clearbuff();

        for (t = frmCounter; t <= toCounter; t++)
                for (s = 1; s <= secper; s++)
                {
                        cprintf(" Clearing sectors %u:%u       \r", t, s);
                        clearSector(drive, t, s);
                }

        cprintf("       ** DONE **                           \r\n\r\n", t, s);

        return (0);
}

int parsecmd(char *cmdline, char *token1, char *token2, char *flags)
{
        int r = 0;
        unsigned char Mx = *(cmdline - 1);
        char *token;
        char *flag;
        *(token1) = 0; // null terminate tokens
        *(token2) = 0;
        *(flags) = 0;
        if (Mx > 127)
                Mx = 127;
        *(cmdline + Mx) = 0; // let's null terminate the string
        token = strtok(cmdline, " ");
        token = strtok(NULL, " "); // discard the "Assign" token
        flag = strtok(NULL, " ");  // any flags?
        if (flag != NULL)
        {
                strncpy(flags, flag, 5);
        }

        if (token != NULL)
        {
                token = strtok(token, "=");
                if (token != NULL)
                {
                        strncpy(token1, token, 29);
                        r = 1;
                        token = strtok(NULL, "=");
                        if (token != NULL)
                        {
                                strncpy(token2, token, 29);
                                r = 2;
                        }
                }
        }
        return r;
}

void prttable(char *bytes)
{
        int i;
        cputs("\n\r DOS/65 Drive assignment:\n\r");
        for (i = 0; i < 16; i++)
        {
                cprintf("  %c:=", i / 2 + 'A');
                prtdevice(*(bytes + i++));
                cprintf(":%i\n\r", *(bytes + i));
        }
}

void prtdevice(char dev)
{
        switch (dev & 0xf0)
        {
        case 0x00:
                cputs("SD");
                break;
        case 0x10:
                cputs("USB");
                return;
        case 0x20:
                cputs("FD");
                break;
        case 0x30:
                cputs("PPIDE");
                break;
        default:
                cputs("UNKNOWN");
                return;
        }
        cprintf("%i", dev & 0x0f);
}

void toupper(char *name)
{
        while (*name)
        {
                if ((*name > 96) && (*name < 123))
                        *name = *name & 0x5F;
                name++;
        }
}

void getdosmap(char drive, char dcb[])
{
        unsigned int **dcbtable = DCBPTR;
        char *table = (unsigned char *)dcbtable + (drive * 14);
        int i;

        for (i = 0; i < 9; i++)
        {
                dcb[i] = *(table + i);
        }
}

void cgets(char *buffer, int length)
{
        unsigned char ch, cont = 1, x = 0;
        length--;

        if (length > 0)
        {
                while (cont)
                {

                        ch = cgetc();
                        if ((ch == 8) && (x > 0))
                        {
                                cputc(ch);
                                cputc(' ');
                                cputc(ch);
                                buffer[x] = 0;
                                x--;
                        }
                        if ((ch > 31) && (ch < 127))
                        {
                                buffer[x++] = ch;
                                cputc(ch);
                        }
                        if ((length == x) || (ch == 13))
                        {
                                buffer[x++] = 0;
                                cont = 0;
                        }
                }
        }
        buffer[x] = 0;
}

void clearbuff()
{
        int counter;

        for (counter = 0; counter < 128; counter++)
                secbuffer[counter] = 0xe5;
}

void clearSector(char drive, unsigned int track, unsigned int sector)
{
        unsigned char a;
        unsigned char y;
        unsigned int bufferaddress = &secbuffer;

        //  select drive
        simjumper(drive, 0, 27);
        //  select track
        a = track;
        y = (track >> 8);
        simjumper(a, y, 30);
        //  select sector
        a = sector;
        y = (sector >> 8);
        simjumper(a, y, 33);
        //  set buffer
        a = bufferaddress;
        y = (bufferaddress >> 8);
        simjumper(a, y, 36);
        //  write sector
        simjumper(0, 0, 42);
}

void simjumper(unsigned char acc, unsigned char yreg, char offset)
{
        unsigned int jumpto;
        unsigned int sim = *((unsigned int *)(0x101));
        unsigned int *jmpinst = (unsigned int *)0x004A;
        unsigned int *pointer = (unsigned int *)0x004B;

        parms[0] = acc;
        parms[1] = yreg;
        jumpto = offset + sim - 3;

        *jmpinst = 0x4c;
        *pointer = jumpto;

        __asm__("lda %v", parms);
        __asm__("ldy %v+1", parms);
        __asm__("jsr $004A");
}
