{{                

  ************************************
  *  ParPortProp for RomWBW          *
  *  Interface to N8VEM ParPortProp  *
  *  Version 0.80                    *
  *  May 4, 2012 (Experimental)      *
  ************************************

  Wayne Warthen
  wwarthen@gmail.com

  Substantially derived from work by:
  
  David Mehaffy (yoda)

  Credits:

  Andrew Lynch (lynchaj)        for creating the N8VEM
  Vince Briel (vbriel)          for the PockeTerm with which a lot of code is shared here
  Jeff Ledger (oldbitcollector) for base terminal code
  Ray Rodrick (cluso99)         for the TriBladeProp that shares some of these ideas
                                for using the CPM to SD

  ToDo:
  
  Updates:

}}

CON
  _CLKMODE = XTAL1 + PLL16X
  _XINFREQ = 5_000_000
  
  SPK_VOL = 75

  SPK_MAXFRQ = 1_200
  SPK_MINFRQ = 200

  KBD_BASE = 14                 ' PS/2 Keyboard pins 14-15 (DATA, CLK)
  SD_BASE = 24                  ' SD Card pins 24-27 (DO, CLK, DI, CS)
  SPK_BASE = 13                 ' Speaker pin


  PPI_CMD = $0100               ' pin 8, PC0, active ???
  PPI_STB = $0200               ' pin 9, PC4, active low
  PPI_IBF = $0400               ' pin 10, PC5, active high
  PPI_ACK = $0800               ' pin 11, PC6, active low
  PPI_OBF = $1000               ' pin 12, PC7, active low

  PPI_DIRRD = PPI_STB + PPI_ACK 
  PPI_DIRWR = PPI_STB + PPI_ACK + $FF

  FUNC_PUTBYTE =  $10           ' Buf[0] -> PPI (one byte)
  FUNC_GETBYTE =  $11           ' PPI -> Buf[0] (one byte)
  FUNC_PUTBUF =   $20           ' Buf -> PPI (arbitrary buffer) 
  FUNC_GETBUF =   $21           ' PPI -> Buf (arbitrary buffer)

  CMD_NOP =       $00           ' Do nothing
  CMD_ECHOBYTE =  $01           ' Receive a byte, invert it, send it back
  CMD_ECHOBUF =   $02           ' Receive 512 byte buffer, send it back

  CMD_DSKRES =    $10           ' Restart SD card support
  CMD_DSKSTAT =   $11           ' Send last SD card status (4 bytes)
  CMD_DSKPUT =    $12           ' PPI -> sector buffer -> PPP
  CMD_DSKGET =    $13           ' PPP -> sector buffer -> PPI
  CMD_DSKRD =     $14           ' Read sctor from SD card into PPP buffer, return 1 byte status
  CMD_DSKWR =     $15           ' Write sector to SD card from PPP buffer, return 1 byte status

  CMD_VIDOUT =    $20           ' Write a byte to the terminal emulator

  CMD_KBDSTAT =   $30           ' Return a byte with number of characters in buffer
  CMD_KBDRD =     $31           ' Return a character, wait if necessary

  CMD_SPKTONE =   $40           ' Emit speaker tone at specified frequency and duration

  CMD_SIOINIT =   $50           ' Reset serial port and establish a new baud rate (4 byte baud rate)
  CMD_SIORX =     $51           ' Receive a byte in from serial port
  CMD_SIOTX =     $52           ' Transmit a byte out of the serial port 
  CMD_SIORXST =   $53           ' Serial port receive status (returns # bytes of rx buffer used)                                   
  CMD_SIOTXST =   $54           ' Serial port transmit status (returns # bytes of tx buffer space available) 
  CMD_SIORXFL =   $55           ' Serial port receive buffer flush                                   
  CMD_SIOTXFL =   $56           ' Serial port transmit buffer flush (not implemented) 

  CMD_RESET =     $F0           ' Soft reset Propeller

OBJ
  dsp : "TERM_FIRMWARE"                                 ' VGA Terminal Driver
  kbd : "Keyboard"                                      ' PS/2 Keyboard Driver
  sdc : "safe_spi"                                      ' SD Card Driver
  'dbg : "Parallax Serial Terminal"                      ' Serial Port Driver (debug output)
  dbg : "Parallax Serial Terminal Null"                 ' Do nothing for debug output
  spk : "E555_SPKEngine"                                ' Speaker Driver
  sio : "FullDuplexSerial"                              ' Serial I/O                                              
  'sio : "FullDuplexSerialNull"                          ' Dummy driver to use when debugging                                              

VAR
  long  tmp
  long  Cmd
  long  Func
  byte  ByteVal
  long  BufPtr  
  long  BufSize

  long  DskBuf[128]             ' 512 byte, declared as long's to ensure long-aligned
  long  DskStat
  long  DskBlock

  long  FuncTmp

  long  Cnt10ms
   
PUB main
  dbg.Start(115200)



  Result := dsp.Start


  Cmd := 0
  Func := 0
  ByteVal := 0
  BufPtr := @DskBuf
  DskStat := 0
  DskBlock := 0
  
  CmdAdr := @Cmd
  FuncAdr := @Func
  ByteValAdr := @ByteVal
  BufPtrAdr := @BufPtr
  BufSizeAdr := @BufSize

  DIRA[11]~   'Set ACK Input
  DIRA[8]~   'Set CMD Input
  DIRA[12]~~   'Set OBF Output
  DIRA[9]~~   'Set STB Output

  OUTA[12]~   'Set OBF Low
  OUTA[9]~~   'Set STB High



  Result := kbd.Start(KBD_BASE, KBD_BASE + 1)
  if (Result < 0)
    MsgStr(string("Keyboard Failed!"))
    MsgNewLine
    
  Result := \sdc.Start(SD_BASE) 
  if (Result < 0)
    MsgStr(string("SD Card Failed!"))
    MsgNewLine

  Result := sio.Start(31, 30, 0, 9600) 
  if (Result < 0)
    MsgStr(string("Serial Port Failed!"))
    MsgNewLine

  spk.speakerFrequency(1000, SPK_BASE)
  waitcnt((clkfreq >> 4) + cnt)
  spk.speakerFrequency(-1, SPK_BASE)

  DSP.Process_Char($102)
  DSP.Process_Char($111)

  SendByte($AA)



  repeat
    TMP:=0  
    ' Wait for cmd to go High
    tmp := GetCommandByte
    IF(INA[8]<>0)
      TMP:=0
    'dbg.Str(String("Cmd="))
    'dbg.Hex(Cmd, 8)
    'dbg.NewLine
    if (tmp <> 0)
      'dbg.Str(string("Command: "))
      'dbg.Hex(tmp, 8)
      'dbg.NewLine

      case tmp
        CMD_NOP:

        CMD_ECHOBYTE:
          EchoByte

        CMD_ECHOBUF:
          EchoBuf

        CMD_DSKRES:
          DiskReset
          
        CMD_DSKSTAT:
          DiskStatus
          
        CMD_DSKPUT:
          DiskPutBuf
          
        CMD_DSKGET:
          DiskGetBuf
          
        CMD_DSKRD:
          DiskRead
          
        CMD_DSKWR:
          DiskWrite

        CMD_VIDOUT:
          VideoOut

        CMD_KBDSTAT:
          KeyboardStatus

        CMD_KBDRD:
          KeyboardRead

        CMD_SPKTONE:
          SpeakerTone
          
        CMD_SIOINIT:
          SerialInit

        CMD_SIORX:
          SerialRx

        CMD_SIOTX:
          SerialTx

        CMD_SIORXST:
          SerialRxStat

        CMD_SIOTXST:
          SerialTxStat
          
        CMD_SIORXFL:
          SerialRxFlush
          
        CMD_SIOTXFL:
          SerialTxFlush
          
        CMD_RESET:
          Reboot

      Cmd := 0
      'dbg.Str(string("*End of Command*"))
      'dbg.NewLine
      
  MsgNewLine
  MsgStr(string("ParPortProp Shutdown!"))
  MsgNewLine

PRI EchoByte
  ByteVal:=GetByte
  !ByteVal
  SendByte(ByteVal)
  return

PRI EchoBuf
  BufPtr := @DskBuf
  BufSize := 512

  bytefill(BufPtr, $FF, BufSize) 
  'DumpBuffer(@DskBuf)

  GETBUF
  'DumpBuffer(@DskBuf)

  PUTBUF

  return

PRI DiskReset

  'dbg.Str(string("sdc.Start:"))
  DskStat := \sdc.Start(SD_BASE)
  Result := DskStat
 ' dbg.Dec(DskStat)
 ' dbg.NewLine

  ByteVal := (DskStat < 0)
  SendByte(ByteVal)  

  return

PRI DiskStatus | Stat

 ' dbg.Str(string("Disk Status:"))
 ' dbg.Dec(DskStat)
 ' dbg.NewLine

  BufPtr := @DskStat
  BufSize := 4
  PUTBUF 

  return

PRI DiskPutBuf

  BufPtr := @DskBuf
  BufSize := 512

  bytefill(BufPtr, $00, BufSize) 

  GETBUF

  'DumpBuffer(@DskBuf)

  return

PRI DiskGetBuf

  BufPtr := @DskBuf
  BufSize := 512

  PUTBUF

  return

PRI DiskRead

  BufPtr := @DskBlock
  BufSize := 4

  GETBUF

  'dbg.Str(string("sdc.ReadBlock("))
  'dbg.Hex(DskBlock, 8)
  'dbg.Str(string("): "))
  DskStat := \sdc.ReadBlock(DskBlock, @DskBuf)
  Result := DskStat
  'dbg.Dec(DskStat)
  'dbg.NewLine

  ByteVal := (DskStat <> 0)
  SendByte(ByteVal)

  'DumpBuffer(@DskBuf)  

  return

PRI DiskWrite

  'DumpBuffer(@DskBuf)  

  BufPtr := @DskBlock
  BufSize := 4

  GETBUF

  'dbg.Str(string("sdc.WriteBlock("))
  'dbg.Hex(DskBlock, 8)
  'dbg.Str(string("): "))
  DskStat := \sdc.WriteBlock(DskBlock, @DskBuf)
  Result := DskStat
  'dbg.Dec(DskStat)
  'dbg.NewLine

  ByteVal := (DskStat <> 0)
  SendByte(ByteVal)  

  return

PRI VideoOut


  ByteVal:=GetByte

  'dbg.Str(string("VideoOut: "))
  'dbg.Hex(ByteVal, 2)

  dsp.Process_Char(ByteVal)

  'dbg.Str(string(" <done>"))
  'dbg.NewLine

  return

PRI KeyboardStatus

  'dbg.Str(string("KeyboardStatus: "))

  ByteVal := kbd.GotKey
 
  'dbg.Hex(ByteVal, 2)
  'dbg.Str(string(" <done>"))

  SendByte(ByteVal)

  return

PRI KeyboardRead

  IF kbd.GotKey
      ByteVal := kbd.GetKey
  ELSE
      ByteVal := 00
  
  SendByte(ByteVal)

  return

PRI SpeakerTone | Freq, Duration

    ByteVal:=GetByte    ' tone
  Freq := (ByteVal * 10)

   ByteVal:=GetByte    ' duration
  Duration := ((CLKFREQ >> 8) * ByteVal)

  'dbg.Str(String("Speaker Tone: "))
  'dbg.Dec(Freq)
  'dbg.Str(String("Hz, "))
  tmp := (CLKFREQ / 1000)
  'dbg.Dec(Duration / tmp) 
  'dbg.Str(String("ms"))
  'dbg.NewLine

  spk.speakerFrequency(Freq, SPK_BASE)
  waitcnt(Duration + cnt)
  spk.speakerFrequency(-1, SPK_BASE)

  return

PRI SerialInit | Baudrate

  BufPtr := @Baudrate
  BufSize := 4

  GETBUF

  sio.Start(31, 30, 0, Baudrate) 

  return

PRI SerialRx

  ByteVal := sio.rx
 SendByte(ByteVal)

  return

PRI SerialTx

    ByteVal:=GetByte
  sio.tx(ByteVal)

  return

PRI SerialRxStat

  ByteVal := sio.rxcount
  SendByte(ByteVal)

PRI SerialTxStat

  ByteVal := sio.txcount                                        
  SendByte(ByteVal)

PRI SerialRxFlush

  sio.rxflush

  return

PRI SerialTxFlush

  ' not implemented by serial driver...

  return

PRI GETBUF | i


    repeat i from 0 to BufSize-1
        IF(INA[8]==0)
           QUIT
        byte[BufPtr+i]:=GetByte


PRI PUTBUF | i

    repeat i from 0 to BufSize-1
        IF(INA[8]==0)
          QUIT
        SendByte(byte[BufPtr+i])

  
PRI MsgNewLine
  'dbg.NewLine
  dsp.process_char(13)
  dsp.process_char(10)


PRI MsgStr(StrPtr)
  'dbg.Str(StrPtr)
  dsp.print_string(StrPtr)


PRI Reverse(Val) | i

  repeat i from 0 to 3
    Result.byte[i] := Val.byte[3 - i]


PRI SendByte(sendByteVal)

  DIRA[9]~~   'Set STB Output
  DIRA[11]~   'Set ACK Input
  DIRA[8]~   'Set CMD Input
  DIRA[0..7]~~ 'Set Data Output

  '      MsgStr(string("wAIT FOR ack TO GO hi"))  
  ' Wait for Ack to go High
    REPEAT UNTIL (INA[11]==1)
      IF(INA[8]==0)
        OUTA[9]~~ 
        RETURN
        
   '     MsgStr(string("pLACE BYTE ON BUS"))  
  ' Place Byte on Bus
   OUTA[7..0]:=sendByteVal 


 '  MsgStr(string("BRING STB LOW"))  
  ' Bring STB Low
   OUTA[9]~

 '  MsgStr(string("WAIT FOR ACK TO GO LOW"))  
 ' Wait for Ack to go Low
    REPEAT UNTIL(INA[11]==0)
      IF(INA[8]==0)
        OUTA[9]~~ 
        RETURN  


  ' MsgStr(string("BRING STB HIGH"))        
  ' Bring STB High
   OUTA[9]~~
  


PRI GetByte  : ReturnByte

  DIRA[12]~~   'Set OBF Output
  DIRA[11]~   'Set ACK Input
  DIRA[8]~   'Set CMD Input
  DIRA[0..7]~ 'Set Data Input

  ' Wait for Ack to go High
    REPEAT   UNTIL(INA[11]==1)  
      IF(INA[8]==0)

        OUTA[12]~
        RETURN  
 
  ' Bring OBF High
   OUTA[12]~~

  ' Wait for Ack to go Low
    REPEAT  UNTIL (INA[11]==0)    
      IF(INA[8]==0)
        OUTA[12]~
        RETURN  
    
  ' Get Data
    ReturnByte := INA[7..0]
    
  ' Bring OBF Low
   OUTA[12]~
   

PRI GetCommandByte  : ReturnByte

  DIRA[12]~~   'Set OBF Output
  DIRA[11]~   'Set ACK Input
  DIRA[8]~   'Set CMD Input
  DIRA[0..7]~ 'Set Data Input

  ' Wait for Ack to go High
    REPEAT UNTIL(INA[11]==1)   

  ' Bring OBF High
   OUTA[12]~~

  ' Wait for Ack to go Low
    REPEAT UNTIL(INA[11]==0)
    
  ' Get Data
    ReturnByte := INA[7..0]
    
  ' Bring OBF Low
   OUTA[12]~



    
DAT

                        
CmdAdr                  long    0
FuncAdr                 long    0
ByteValAdr              long    0
BufPtrAdr               long    0
BufSizeAdr              long    0

Zero                    long    0
NegOne                  long    -1

xppi_cmd                long    PPI_CMD                 ' pin 8, active high
xppi_stb                long    PPI_STB                 ' pin 9, active low
xppi_ibf                long    PPI_IBF                 ' pin 10, active high
xppi_ack                long    PPI_ACK                 ' pin 11, active low
xppi_obf                long    PPI_OBF                 ' pin 12, active low

xppi_idle               long    PPI_STB + PPI_ACK
xppi_ibfcmd             long    PPI_IBF + PPI_CMD
xppi_obfcmd             long    PPI_OBF + PPI_CMD

xppi_dirrd              long    PPI_STB + PPI_ACK
xppi_dirwr              long    PPI_STB + PPI_ACK + $FF

xtmp                    long    $FFFF

TempVal                 res     1
TempAdr                 res     1
TempCnt                 res     1

                        fit