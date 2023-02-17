# 6x0x-DOS65
DOS/65 port for the retrobrewcomputers.org 6x0x sbc

Note that the new system uses banked ram/rom.  To function K17 needs to be off.

todo:
* cleanup quickstart documentation and delete quickstart folder

* cleanup wiki documentation
* assign utility
* floppy support
* floppy formatter

* rtc utility (Needs working VIAs)
* dsky new (jumper?) (Needs working VIAs)
* default console jumper (Needs working VIAs)
* dsky old (jumper?) (Needs working VIAs)
* IO ASSIGNMENT?
    ** prnwrt (SERIAL, ETH,  OR PARALLEL SUPPORT)
    ** punwrt (SERIAL, ETH,  OR CASSETTE SUPPORT) s19?
    ** rdrinp (SERIAL, ETH,  OR CASSETTE SUPPORT) s19?

---------------------------------------------------------------------------------------------------------------------


DOS/65 for the N8VEM 6502
COLOSSUS




DOS/65 IS A PRODUCT OF
RICHARD A. LEARY
http://www.z80.eu/dos65.html

DOS/65 software and documentation are provided
as shareware for non-profit, educational, or private
use. Any other use is prohibited without approval of
Richard A. Leary.


CONVERSION FOR THE N8VEM
BY DAN WERNER






I. REQUIRED FOR OPERATION

  6x0x Colossus board*
  PC Connected to serial port (P31) of 6x0x Colossus
  SD card (>32mb)
*Early versions of this board require fixes to function properly.  Refer to the N8VEM WIKI for details.


II. BRINGING UP THE SYSTEM


1> Program the R52PPP.BIN rom image on to EPROM or EEPROM

The Rom Commands are as follows:
REGISTER  Print Processor Registers
DUMP XXXX YYYY  Dump memory from xxxx (in hex) to yyyy (in hex)
ENTER XXXX YY Change Memory byte at location xxxx (in hex) to value yy
GO XXXX  Transfer execution to location xxxx (in hex)
LOAD  Load a Motorola format image
BOOT X  Load DOS/65 image from device X and boot it
o 0= SD CARD
o 1= FLOPPY
o 4= IDE

The following two commands are optional:
ASSEMBLE XXXX Assemble a 6502 program from the console to location XXXX
DISASSEMBLE XXXX Disassemble a 6502 program from location XXXX  to the console

Note that optional commands and Floppy/IDE support may not be included in the Quick Start Image,
to use optional features, download the latest DOS/65 source package from the N8VEM wiki.

2> Program the ParPortProp.eeprom rom image to the P.P.P. (ParPortProp ) board

3> Power on system (should see 6502 monitor prompt on PPP )

4> Install second stage loader onto SD Card
On the P.P.P. “.” prompt type:
LOAD

The P.P.P prompt will appear to freeze, but it is actually waiting for a Motorola
“S19” file to be sent to the serial port from your PC.  From a terminal program
on your PC (9600 baud, 8 bit, no stop bit, no parity), do a file dump of the
“PPPboot.S19”.  You may need to tell your terminal program to insert a delay
between characters and between lines in order to ensure that you do not send
the characters too quickly.  On my TeraTerm, 5ms between characters and
10ms between lines seems to work out about right.  If the file transfer is working
normally, you should see the s19 file echoed to your pc terminal window.   You
should not see any “?” characters in the stream, if you do, there was a
checksum error and you need to increase your delay settings.   When the
transfer is complete the P.P.P screen should return to the “.” Prompt.

On the P.P.P. “.” prompt type:
GO 0300

This will run the program and copy the second stage boot loader to the SD card
inserted in the P.P.P.





5> Install DOS/65 onto the SD Card
Following the same process as step 4, load the following software
On the P.P.P. “.” prompt type:
LOAD

 Dump dos65.S19 to serial port from your PC terminal program, wait for the
P.P.P to return to the prompt when the load is complete.

On the P.P.P. “.” prompt type:
LOAD

 Dump pppwros.S19 to serial port from your PC terminal program, wait for the
P.P.P to return to the prompt when the load is complete.

On the P.P.P. “.” prompt type:
GO 0800

This will load DOS/65 to the boot track of the SD card.


6> Boot DOS/65
At the P.P.P. “.” prompt type:
BOOT 0


7> Clear SD directory track
At the P.P.P. “A>” prompt type:
ERA A:*.*
At the P.P.P. “ALL FILES(Y/N)” prompt type:
Y

The system will think for a bit -- the SD card is defined with a VERY large
directory area so this will take some time.   It is possible to speed SD disk
access up by changing the number of directory sectors in the SIM.  See the
DOS/65 guides for more details.


8> Load file transfer utility
 On the P.P.P. “A>” prompt type:
GO $FFDD

 Dump S19.S19 to serial port from your PC terminal program, wait for the P.P.P
to return to the prompt when the load is complete.

On the P.P.P. “A>” prompt type:
SAVE 1 A:S19.COM
CONGRATULATIONS!  You have just saved your first utility to the DOS/65 disk!


9> Load DBASIC
 On the P.P.P. “A>” prompt type:
S19

 Dump DBASIC.S19 to serial port from your PC terminal program, wait for the
P.P.P to return to the prompt when the load is complete.

On the P.P.P. “A>” prompt type:
SAVE 43 A:DBASIC.COM

* note: A new command was added to DBASIC that is not in the docs
 KILL - will return to DOS/65 from DBASIC


10> Load ALLOC
 On the P.P.P. “A>” prompt type:
S19

 Dump ALLOC203.S19 to serial port from your PC terminal program, wait for the
P.P.P to return to the prompt when the load is complete.

On the P.P.P. “A>” prompt type:
SAVE 2 A:ALLOC.COM


11> Load Asm
 On the P.P.P. “A>” prompt type:
S19

Dump ASM211.S19 to serial port from your PC terminal program, wait for the
P.P.P to return to the prompt when the load is complete.

On the P.P.P. “A>” prompt type:
SAVE 39 A:ASM.COM


12> Load COMPL
On the P.P.P. “A>” prompt type:
S19

Dump COMPL203.S19 to serial port from your PC terminal program, wait for the
P.P.P to return to the prompt when the load is complete.

On the P.P.P. “A>” prompt type:
SAVE 42 A:COMPL.COM


13> Load COMPR
On the P.P.P. “A>” prompt type:
S19

Dump COMPR202.S19 to serial port from your PC terminal program, wait for the
P.P.P to return to the prompt when the load is complete.

On the P.P.P. “A>” prompt type:
SAVE 12 A:COMPR.COM


14> Load COPY
 On the P.P.P. “A>” prompt type:
S19

Dump COPY201.S19 to serial port from your PC terminal program, wait for the
P.P.P to return to the prompt when the load is complete.

On the P.P.P. “A>” prompt type:
SAVE 3 A:COPY.COM


15> Load EDIT
On the P.P.P. “A>” prompt type:
S19

Dump EDIT203.S19 to serial port from your PC terminal program, wait for the
P.P.P to return to the prompt when the load is complete.

On the P.P.P. “A>” prompt type:
SAVE 28 A:EDIT.COM


16> Load MKCOM
 On the P.P.P. “A>” prompt type:
S19

Dump MKCOM202.S19 to serial port from your PC terminal program, wait for the
P.P.P to return to the prompt when the load is complete.

On the P.P.P. “A>” prompt type:
SAVE 5 A:MKCOM.COM
