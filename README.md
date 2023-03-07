# 6x0x-DOS65
## DOS/65 port for the retrobrewcomputers.org 6x0x sbc

[DOS/65 IS A PRODUCT OF RICHARD A. LEARY.](http://www.z80.eu/dos65.html)

DOS/65 software and documentation are provided as shareware for non-profit, educational, or private use. Any other use is prohibited without approval of Richard A. Leary.

Conversion for the Retrobrew Computers 6x0x by Dan Werner

Please note that this version of DOS/65 uses the ROMWBW track/sector mapping and is thus not compatible with disk images created with the old distribution.  Please backup your data prior to installing this version.


## todo:
1. cleanup wiki documentation
1. assign utility
1. floppy support
1. floppy formatter
1. dsky new (jumper?)
1. default console jumper
1. dsky old (jumper?)
1. IO ASSIGNMENT?
 * prnwrt (SERIAL, ETH,  OR PARALLEL SUPPORT)
 * punwrt (SERIAL, ETH,  OR CASSETTE SUPPORT) s19?
 * rdrinp (SERIAL, ETH,  OR CASSETTE SUPPORT) s19?

---

## System install

### REQUIRED FOR OPERATION

1. 6x0x
1. PC Connected to serial port (P31) of 6x0x
1. SD card (>32mb)

The following pieces of optional equipment are supported:
 *  ECB backplane
 *  DSKY [V1](https://retrobrewcomputers.org/doku.php?id=boards:ecb:dsky:start) or [V2](https://retrobrewcomputers.org/doku.php?id=boards:ecb:dskyng)
 *  [Disk Controller V3](https://retrobrewcomputers.org/doku.php?id=boards:ecb:diskio-v3:start)

See XXX for more information.


### Jumper Settings
See the Retrobrew computers Wiki [here](https://retrobrewcomputers.org/doku.php?id=boards:sbc:6x0x-atx-6u:start) for instructions on setting the jumpers for your particular hardware configuration.   Note that as this version of DOS/65 uses the MMU, it is important that there is no jumper on K17.  Also, to utilize floppy drives with DOS/65 the CPU must be running at 2Mhz or better.


### Building the system
See the Retrobrew computers Wiki [here](https://retrobrewcomputers.org/doku.php?id=boards:sbc:6x0x-atx-6u:start) for instructions on how to construct the 6x0x PCB.

### BRINGING UP THE SYSTEM
#### Program the rom image on to EPROM or EEPROM from the bin folder in this repo
The default build ROM.HEX file can be programmed into your selected ROM chip type and should provide sane defaults for
the 6x0x SBC.  See the "Building a Custom ROM Image" section for details on what configuration options are available and how to build a custom ROM image.

Selection and use of a suitable EPROM programmer is required and beyond the scope of this document.


#### Program the ParPortProp.eeprom rom image to the 6x0x board
In this repo in the folder 6x0x_firmware/BusPortProp/FullColorTerm is the source code and binary image for the Propeller microcontroller that handles the Keyboard, Video, Sound, and SD card interface for the 6x0x.  Note that there are two versions of the Propeller firmware, “ParPortPropFullColor.eeprom” which is a full-color terminal using a 1280×1024 tile driver and “ParPortPropOriginal.eeprom” which is an alternate version for older monitors using a different Propeller driver.

Programming of the Propeller EEPROM(U25) can be accomplished in any of the following ways:

1. Via a standalone EEPROM programmer
1. Via a serial cable to the serial port on the 6x0x
1. Via a PropPlug adapter available from Parallax

For #1, selection and use of a suitable programmer is required and beyond the scope of this document.

For #2 or #3, you must download the Propeller Tool from Parallax (free). This tool includes documentation on using either a serial cable or a PropPlug to program your device. In a nutshell, you will follow this procedure to program your selected eeprom file:

* Start the Propeller Tool
* Use File → Open… to select and open ParPortProp.eeprom
* Choose “Load EEPROM” from the subsequent “Object Info” dialog box.

#### Power on system
If the system is running properly, you should be greeted with the 6x0x power on screen on your VGA monitor.

```
        RetroBrew Computers 6x0x

         .d8888b.            .d8888b.
        d88P  Y88b          d88P  Y88b
        888                 888    888
        888d888b.  888  888 888    888 888  888
        888P  Y88b `Y8bd8P' 888    888 `Y8bd8P'
        888    888   X88K   888    888   X88K
        Y88b  d88P .d8  8b. Y88b  d88P .d8  8b.
          Y8888P   888  888   Y8888P   888  888

        SD CARD: INITIALIZED
        PPIDE: NOT FOUND

           PC  AC  XR  YR  SP  SR"
        ! F5CE A5  D3  00  FF  3A
        .
```
(note that the register values may be different for your build)

At this juncture it is convenient (but not essential) to redirect the console to the serial port so that all operations can be done from your attached PC. (9600 baud, 8 bit, no stop bit, no parity)

To do this type:
```
ENTER 003A 01
```
at the monitor prompt, then you can continue to interact with the 6x0x system using a terminal program on your PC attached to the 6x0x SBC on the Primary serial port using either:

* P25, TTL levels serial, pin 1 = lower left, above pin 16 of left MAX232
* P31, RS232 levels serial, pin 1 = lower left, above pin 9 of left MAX232


#### Clear the Boot Device
DOS/65 is a bit sensitive to garbage that is in the boot and directory areas of it's storage devices.  Therefore it is important to have a way to clear these areas.   The 6x0x monitor provides a command to accomplish this task.

```
CLRDIR  D TTTTTT NN
             D     = Device (I)DE Primary (J)IDE Secondary or (S)D
             TTTTTT= Starting Track
             NN    = Number of Tracks
```

Prior to starting DOS/65 with an attached SD card or IDE device it is important to run this command on a blank device.  Note that you only need to run this command on a NEW device -- if it is ran on an existing drive DATA WILL BE LOST.

This version supports the ROMWBW concept of "Slices" to allow large drives to be partitioned off to allow DOS/65 to make use of a much larger device.   See the "Building a Custom ROM Image" section of this document for more information.  For now, it is only important to clear the first few tracks of the device (eventually it will be important to ensure that you have cleared the first few tracks of each Slice).

To clear the first 16 tracks of Slice 0 of the SD card type:
```
CLRDIR S 000000 10
```
If you have configured the ROM image to support more Slices in the SD card the command should be repeated for the first few tracks in each configured slice.
If you have any attached IDE devices, you need to repeat this process for any configured slices on those drives.

If you are using the default configuration and have no attached IDE devices, only the above command is required.


#### load the OS into RAM
To load the OS image into RAM, at the 6x0x “.” prompt type:
```
LOAD
```

The 6x0x will appear to freeze, but it is actually waiting for a Motorola “S19” file to be sent to the serial port from your PC.  From a terminal program on your PC (9600 baud, 8 bit, no stop bit, no parity), do a file dump of the “dos65.s19” from the bin folder of this repo.  You may need to tell your terminal program to insert a delay between characters and between lines in order to ensure that you do not send the characters too quickly.  On my TeraTerm, 5ms between characters and 10ms between lines seems to work out about right.  If the file transfer is working normally, you should see the s19 file echoed to your pc terminal window.   You should not see any “?” characters in the stream, if you do, there was a checksum error and you need to increase your delay settings.   When the transfer is complete the 6x0x screen should return to the “.” Prompt.  Note that some terminal programs will insert some extra control characters after sending S19 files.  It is a good idea to hit "enter" after any of the file transmissions referenced in this document complete, just to be sure you have a clean prompt with no extra characters.

To start DOS/65 type:
```
GO B800
```

This will start DOS/65.  If DOS has started successfully you should see:
```
DOS/65 ON THE 6x0x RBC
SD CARD: INITIALIZED
PPIDE : NOT PRESENT

A>
```

CONGRATULATIONS! Your 6x0x SBC is now running DOS/65.   You should be able to see your SD card as drive "A" and can now run any of the internal DOS commands.

#### Save the running OS in RAM on to the boot area of the SD card
The next steps we need to accomplish is to load a few key programs on to the SD card, and save the OS into the boot area of the SD card to allow us to boot the system without an attached PC.

We want to begin by loading a DOS/65 program that will access the BIOS S19 file loader.

At the DOS/65 “A>” prompt type:
```
GO $FD36
```
This calls the BIOS S19 loader and just like before is now waiting for you to dump a s19 file to the serial port from your PC terminal program.   The file we want to send now is the "S19.S19" file from the bin folder of the repo.  Once the file is sent, DOS/65 should return to a "A>" prompt.
On the 6x0x “A>” prompt type:
```
SAVE 1 A:S19.COM
```
You have just saved your first utility to the DOS/65 SD disk!

The next utility we want to save to the SD card is the "WRITEOS" utility.  This program will copy the running version of DOS/65 to a selected drive.   To upload this utility type:

```
S19
```
at the DOS/65 prompt. . .  and again the 6x0x is now waiting for you to dump a s19 file to the serial port from your PC terminal program.   The file we want to send now is the "WRITEOS.S19" file from the bin folder of the repo.  Once the file is sent, DOS/65 should return to a "A>" prompt.
On the 6x0x “A>” prompt type:
```
SAVE 5 A:WRITEOS.COM
```

Once this is complete, you can then call the WRITEOS.COM program to copy DOS/65 to the SD Card.
On the 6x0x “A>” prompt type:

```
WRITEOS
```

The 6x0x should respond with:
```
WRITEOS - WRITE DOS/65 FROM MEMORY TO BOOT TRACK

SELECT DRIVE:
(I) IDE PRIMARY
(S) SD CARD
(F) FLOPPY A:
```

Type:
```
S
```
(make sure it is in caps) to write the running image to the SD card and . . .  That's it!  you should now be able to power off your 6x0x system and boot DOS/65 from the SD card by typing:
```
BOOT 0
```
From the system monitor's "." prompt.


#### Load the remaining DOS/65 utilities to the SD Card

##### Load DBASIC
 On the 6x0x “A>” prompt type:

```
S19
```

 Dump DBASIC.S19 to serial port from your PC terminal program, wait for the
6x0x to return to the prompt when the load is complete.

On the 6x0x “A>” prompt type:
```
SAVE 43 A:DBASIC.COM
```

* note: A new command was added to DBASIC that is not in the docs
 KILL - will return to DOS/65 from DBASIC


##### Load ALLOC
 On the 6x0x “A>” prompt type:
```
S19
```
 Dump ALLOC.S19 from the bin folder in the repo to serial port from your PC terminal program, wait for the 6x0x to return to the prompt when the load is complete.

On the 6x0x “A>” prompt type:
```
SAVE 2 A:ALLOC.COM
```

##### Load Asm
 On the 6x0x “A>” prompt type:
```
S19
```
Dump ASM.S19 from the bin folder in the repo to serial port from your PC terminal program, wait for the 6x0x to return to the prompt when the load is complete.

On the 6x0x “A>” prompt type:
```
SAVE 39 A:ASM.COM
```

##### Load BCOMPILE
On the 6x0x “A>” prompt type:
```
S19
```
Dump BCOMPILE.S19 from the bin folder in the repo to serial port from your PC terminal program, wait for the 6x0x to return to the prompt when the load is complete.

On the 6x0x “A>” prompt type:
```
SAVE 42 A:BCOMPILE.COM
```

##### Load COMPARE
On the 6x0x “A>” prompt type:
```
S19
```
Dump COMPARE.S19 from the bin folder in the repo to serial port from your PC terminal program, wait for the 6x0x to return to the prompt when the load is complete.

On the 6x0x “A>” prompt type:
```
SAVE 12 A:COMPARE.COM
```

##### Load COPY
 On the 6x0x “A>” prompt type:
```
S19
```

Dump COPY.S19 from the bin folder in the repo to serial port from your PC terminal program, wait for the 6x0x to return to the prompt when the load is complete.

On the 6x0x “A>” prompt type:
```
SAVE 3 A:COPY.COM
```

##### Load DEBUG
 On the 6x0x “A>” prompt type:
```
S19
```

Dump DEBUG.S19 from the bin folder in the repo to serial port from your PC terminal program, wait for the 6x0x to return to the prompt when the load is complete.

On the 6x0x “A>” prompt type:
```
SAVE 15 A:DEBUG.COM
```

##### Load EDIT
On the 6x0x “A>” prompt type:
```
S19
```

Dump EDIT.S19 from the bin folder in the repo to serial port from your PC terminal program, wait for the 6x0x to return to the prompt when the load is complete.

On the 6x0x “A>” prompt type:
```
SAVE 28 A:EDIT.COM
```

##### Load MKCOM
On the 6x0x “A>” prompt type:
```
S19
```
Dump MKCOM.S19 from the bin folder in the repo to serial port from your PC terminal program, wait for the 6x0x to return to the prompt when the load is complete.

On the 6x0x “A>” prompt type:
```
SAVE 5 A:MKCOM.COM
```

##### Load SEDIT
On the 6x0x “A>” prompt type:
```
S19
```
Dump SEDIT.S19 from the bin folder in the repo to serial port from your PC terminal program, wait for the 6x0x to return to the prompt when the load is complete.

On the 6x0x “A>” prompt type:
```
SAVE 19 A:SEDIT.COM
```

##### Load RUN
 On the 6x0x “A>” prompt type:
```
S19
```

Dump RUN.S19 from the bin folder in the repo to serial port from your PC terminal program, wait for the 6x0x to return to the prompt when the load is complete.

On the 6x0x “A>” prompt type:
```
SAVE 44 A:RUN.COM
```

##### Load RTC
 On the 6x0x “A>” prompt type:
```
S19
```

Dump RTC.S19 from the bin folder in the repo to serial port from your PC terminal program, wait for the 6x0x to return to the prompt when the load is complete.

On the 6x0x “A>” prompt type:
```
SAVE 16 A:RTC.COM
```


### Monitor Commands
The Rom Commands are as follows:
 command | description
----------------------- | ----------------------------
REGISTER | Print Processor Registers
DUMP XXXX YYYY | Dump memory from xxxx (in hex) to yyyy (in hex)
ENTER XXXX YY | Change Memory byte at location xxxx (in hex) to value yy
GO XXXX | Transfer execution to location xxxx (in hex)
LOAD | Load a Motorola format image
BOOT X | Load DOS/65 image from device X and boot it
| | 0= SD CARD
| | 1= FLOPPY
| | 4= IDE
ASSEMBLE XXXX | Assemble a 6502 program from the console to location XXXX
DISASSEMBLE XXXX | Disassemble a 6502 program from location XXXX to the console
CLRDIR  D TTTTTT NN | Clear the directory area of a mass storage device
| | D     = Device (I)DE Primary (J)IDE Secondary or (S)D
| | TTTTTT= Starting Track
| | NN    = Number of Tracks

### Building a Custom ROM Image
 To Do
### Memory Map

 From | To | Description
------ | ----------------------- | ----------------------------
0000 | 00FF | Zero Page
0100 | 01FF | Stack Page
0200 | 02FF |
