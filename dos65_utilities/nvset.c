#include <conio.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "rbc.h"

unsigned char dectobcd(unsigned char in);
unsigned char bcdtodec(unsigned char in);
void UpdateTime(void);
void UpdateDate(void);
void UpdateConsole(void);
void UpdateDsky(void);
void PrintConsoleType(unsigned char in);
void PrintDskyType(unsigned char in);
void cgets(char *buffer, int length);

int main()
{
    unsigned char x = 0;
    unsigned char mo, da, yr, hr, mi, se, am = 'A';

    cputs("\n\r6x0x NVRAM Utility\n\r\n\r");
    writertc(7,0);
    writertc(8,165);

    se = bcdtodec(readrtc(0));
    mi = bcdtodec(readrtc(1));
    hr = bcdtodec(readrtc(2));
    da = bcdtodec(readrtc(3));
    mo = bcdtodec(readrtc(4));
    yr = bcdtodec(readrtc(6));

    writertc(32, 165);

    if (hr >= 12)
    {
        am = 'P';
        if (hr > 12)
            hr = hr - 12;
    }
    if (hr == 0)
        hr = 12;

    cprintf("Current System time is %i:%02i:%02i %cM %i/%i/%02i\n\r", hr, mi, se, am, mo, da, yr);
    cputs("would you like to change the system time? (y/N)");
    x = cgetc();
    cputs("\n\r");
    if ((x == 'y') || (x == 'Y'))
    {
        UpdateTime();
        UpdateDate();
    }
    cputs("Console is currently set to:");
    PrintConsoleType(readrtc(33));
    cputs("\n\rwould you like to change the system Console? (y/N)");
    x = cgetc();
    cputs("\n\r");
    if ((x == 'y') || (x == 'Y'))
        UpdateConsole();

    cputs("DSKY is currently set to:");
    PrintDskyType(readrtc(34));
    cputs("\n\rwould you like to change the DSKY settings? (y/N)");
    x = cgetc();
    cputs("\n\r");
    if ((x == 'y') || (x == 'Y'))
        UpdateDsky();

    writertc(7,128);
    return 0;
}

unsigned char bcdtodec(unsigned char in)
{
    return (in & 0x0f) + (((in & 0xf0) >> 4) * 10);
}

unsigned char dectobcd(unsigned char in)
{
    return ((in/10)*16)+(in-((in/10)*10));
}

void UpdateTime(void)
{
    char *buffer;
    char *token;
    int hr, mi, se, am;
    unsigned char x;

    buffer = malloc(80);

    cputs("Enter Time (HH:MM:SS A/P): ");
    cgets(buffer, 79);
    token = strtok(buffer, ":");
    sscanf(token, "%d", &hr);
    token = strtok(NULL, ":");
    sscanf(token, "%d", &mi);
    token = strtok(NULL, " ");
    sscanf(token, "%d", &se);
    token = strtok(NULL, "");
    sscanf(token, "%c", &am);

    if ((am == 'p') || (am == 'P'))
    {
        am = 'P';
    }
    else
    {
        am = 'A';
    }

    cprintf("\n\rChange to: %2d:%02d:%02d %cM? (y/N)\n\r", hr, mi, se, am);
    x = cgetc();
    if ((x == 'y') || (x == 'Y'))
    {
        if (am == 'P')
            hr = hr + 12;
        if (hr == 12)
            hr = 0;
        if (hr == 24)
            hr = 12;

        writertc(0, dectobcd(se));
        writertc(1, dectobcd(mi));
        writertc(2, dectobcd(hr));
    }
}

void UpdateDate(void)
{
    char *buffer;
    char *token;
    int mo, da, yr;
    unsigned char x;

    buffer = malloc(80);

    cputs("Enter Date (MM/DD/YY): ");
    cgets(buffer, 79);
    token = strtok(buffer, "/");
    sscanf(token, "%d", &mo);
    token = strtok(NULL, "/");
    sscanf(token, "%d", &da);
    token = strtok(NULL, " ");
    sscanf(token, "%d", &yr);
    cprintf("\n\rChange to: %d/%d/%02d (y/N)\n\r", mo, da, yr);
    x = cgetc();
    if ((x == 'y') || (x == 'Y'))
    {
        writertc(3, dectobcd(da));
        writertc(4, dectobcd(mo));
        writertc(6, dectobcd(yr));
    }
}

void UpdateConsole(void)
{
    unsigned char x;

    cputs("Select:\n\r");
    cputs("1 -> Internal Console\n\r");
    cputs("2 -> Serial Port 1\n\r");
    x = cgetc();
    if (x == '2')
    {
        writertc(33, 4);
        cputs("Set to Serial Port 1\n\r");
    }
    else
    {
        writertc(33, 9);
        cputs("Set to Internal Console\n\r");
    }
    cputs("\n\r");
}

void UpdateDsky(void)
{
    unsigned char x;

    cputs("Select:\n\r");
    cputs("1 -> None\n\r");
    cputs("2 -> Original DSKY\n\r");
    cputs("3 -> Next Generation DSKY\n\r");
    x = cgetc();
    if (x == '1')
    {
        writertc(34, 0);
        cputs("Set DSKY to none\n\r");
    }
    if (x == '2')
    {
        writertc(34, 1);
        cputs("Set DSKY to original DSKY\n\r");
    }
    if (x == '3')
    {
        writertc(34, 2);
        cputs("Set DSKY to Next Generation DSKY\n\r");
    }
    cputs("\n\r");
}

void PrintConsoleType(unsigned char in)
{
    switch (in)
    {
    case 4:
        cputs("SERIAL PORT 1");
        break;
    case 9:
        cputs("INTERNAL");
        break;
    default:
        cputs("UNKNOWN");
        break;
    }
}

void PrintDskyType(unsigned char in)
{
    switch (in)
    {
    case 0:
        cputs("NONE");
        break;
    case 1:
        cputs("Original");
        break;
    case 2:
        cputs("Next Generation");
        break;
    default:
        cputs("UNKNOWN");
        break;
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