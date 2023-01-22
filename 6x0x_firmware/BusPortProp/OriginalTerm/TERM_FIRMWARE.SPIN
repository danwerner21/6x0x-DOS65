''________________________________________________________________________________
''  
''  Terminal Firmware
''  Dan Werner
''  6/11/2011
''________________________________________________________________________________
''
''    **** TO DO:
''            SETUP PROGRAM
''            ANSI COLOR SUPPORT
''            GRAPHICS SUPPORT
''            CTS/RTS SUPPORT
''            AUDIO BELL
''            KEY CLICK




CON

  _clkmode = xtal1 + pll16x
  _xinfreq = 5_000_000

  cols = 80
  rows = 64
  tiles = cols * rows

  spacetile = $220



OBJ

  vga     : "VGA_1280x1024_Tile_Driver_With_Cursor"
     

  
VAR

  long col, row, color
  long  cursor_x, cursor_y, cursor_col, cursor_def
  byte escMode
  byte escBrak

  long  array[tiles/2]

  byte  escValues[32]
  byte  escValCounter


  
  

PUB start | i, j, k

  'start mouse and set bound parameters
  'start vga tile driver
  vga.start(16, @array, @vgacolors, @cursor_x, 0, 0)

  escMode:=0
  escBrak:=0  

  'fill screen with text
  Process_Char($100)
  
  PrintAt(0,29,0,string("________________________________________________________________________________"))
'                       "01234567890123456789012345678901234567890123456789012345678901234567890123456789"
'                       "          1         2         3         4         5         6         7         "
 PrintAt(60,30,4,string("N8VEM PROJECT V0.1"))
 PrintAt(2,30,2,string("ANSI TERM/BUS ATTACHED"))                       

  Process_Char($102)
  Process_Char($111)

  cursor_def := @Cursor1
  cursor_col := $001112

  cursor_x :=0+16
  cursor_y := 1023-32

 
   
PUB PrintAt(left,top,clr,str)    | Tcolor 
   Tcolor := color  
  color := clr
  col := left
  row := top*2
  print_string(str)
  color := Tcolor    

PUB PrintAtC(left,top,clr,str)  | Tcolor
  Tcolor := color
  color := clr
  col := left
  row := top*2
  print_string(str)
  color := Tcolor


PUB PlaceCursor

    cursor_x := (col*16)
    cursor_y := 1023-(row*16)
  
PUB print_string(ptr)

  repeat while byte[ptr]
    Process_Char(byte[ptr++])
    

Pub Process_Char(c) | i, k, X,y

'' Print a character
''
''       $0D = new line
''  $20..$FF = character
''      $100 = clear screen
''      $101 = home
''      $108 = backspace
''$110..$11F = select color



  if escBrak==1
     case c
     
        "B":
          if (row) < rows-6
            Row+=2
          c:=0
          escMode:=0
          escBrak:=0   

          
        "I":
          RLineFeed  
          c:=0
          escMode:=0
          escBrak:=0   
            
        "A":
          if row>0   
            row-=2
          c:=0
          escMode:=0
          escBrak:=0   
          
        "C":
          if col<80
            col++
          c:=0
          escMode:=0
          escBrak:=0   
          
        "D":
          if col
            col--
          c:=0
          escMode:=0
          escBrak:=0   
               
        "H":

          i:=0
          y:=0
          repeat while escValues[y]>47  and escValues[y]<58 and y<3
             i:=i*10                                
             i:=i+(escValues[y]-48)
             y:=y+1
          i:=i-1
             
          row:=i*2

          i:=0
          y:=y+1
          repeat while escValues[y]>47  and escValues[y]<58 and y<7
             i:=i*10                                
             i:=i+(escValues[y]-48)
             y:=y+1
          i:=i-1
          col:=i
          if row>56
            row:=56
          if col>80
            col:=80
          c:=0
          escMode:=0
          escBrak:=0   
          
        "f":
          i:=0
          y:=0
          repeat while escValues[y]>47  and escValues[y]<58 and y<3
             i:=i*10                                
             i:=i+(escValues[y]-48)
             y:=y+1
          i:=i-1          
          row:=i*2

          i:=0
          y:=y+1
          repeat while escValues[y]>47  and escValues[y]<58 and y<7
             i:=i*10                                
             i:=i+(escValues[y]-48)
             y:=y+1             
          i:=i-1
          col:=i
          if row>56
            row:=56
          if col>80
            col:=80
          c:=0
          escMode:=0
          escBrak:=0   
             
        "K":
           case escValues[0]

             "1":
               wordfill(@array.word[0], spacetile, (row+1) * cols + col)

             "2":
               wordfill(@array.word[row * cols], spacetile, 80)
               wordfill(@array.word[(row+1) * cols], spacetile, 80)

             OTHER:
               wordfill(@array.word[row * cols + col], spacetile, cols-col)
               wordfill(@array.word[(row+1) * cols + col], spacetile, cols-col)
          c:=0
          escMode:=0
          escBrak:=0   
           
        "J":
           case escValues[0]
             "1":
               wordfill(@array.word[0], spacetile, (row+1) * cols + col)

             "2":
                wordfill(@array, spacetile, tiles)
        
             OTHER:
               wordfill(@array.word[row * cols + col], spacetile, cols-col)               
               wordfill(@array.word[(row+1) * cols + col], spacetile, cols-col)
               wordfill(@array.word[(row+2) * cols + col], spacetile, ((cols*(rows-8)))-(row * cols + col))
          c:=0
          escMode:=0
          escBrak:=0   

        "m":
          case escValues[0] 
             48:
               color :=  19
             49:
               color :=  3
             50:
               color :=  1
             51:
               color :=   5
             52:
               color :=   0
             53:
               color :=   4
             54:
               color :=   2
             55:
               color :=   6
             
          c:=0
          escMode:=0
          escBrak:=0   


           
        OTHER:
          escValues[escValCounter]:=c
          if escValCounter<32
             escValCounter++
          else
             escMode:=0
             escBrak:=0   

          c:=0
                      
  PlaceCursor       
             
  if escMode==1
     case c
        "[":
          escBrak:=1
          repeat X from 0 to 31        
            escValues[X]:=0
          escValCounter:=0
          escMode:=0 
        "M":
          RLineFeed  
          c:=0
          escMode:=0
          escBrak:=0
        "D":          
          LineFeed  
          c:=0
          escMode:=0
          escBrak:=0  
        "E":          
          LineFeed
          Col:=0  
          c:=0
          escMode:=0
          escBrak:=0
  
      OTHER:
         escMode:=0
         escBrak:=0 
      


  if escMode==0  and escBrak==0
    case c

      $09:
        if col<74
         col:=((col/8)+1)*8
   
      $0D:             'return?
        col:=0

      $0A:
        LineFeed

      $1B:
         escMode:=1
      
      $C4:               'home?
         col := row := 0

      $08:               'backspace?
        if col
          col--
        
      $20..$FF:           'character?
        k := color << 1 + c & 1
        i := $200 + (c & $FE) + k << 10
        array.word[row * cols + col] := i
        array.word[(row + 1) * cols + col] := i | 1
        if ++col == cols
          newline

      $100:               'clear screen?
        wordfill(@array, spacetile, tiles)
        col := row := 0

      $102:               'clear screen?
        wordfill(@array, spacetile, tiles-400)
        col := row := 0

      $110..$11F:         'select color?
        color := c & $F

  PlaceCursor


 k := 3 << 1 + (escMode+48) & 1
 i := $200 + ((escMode+48) & $FE) + k << 10
 array.word[60 * cols + 33] := i
 array.word[61 * cols + 33] := i | 1
 k := 3 << 1 + (escBrak+48) & 1
 i := $200 + ((escBrak+48) & $FE) + k << 10
 array.word[60 * cols + 34] := i
 array.word[61 * cols + 34] := i | 1
PRI newline    
   col:=0
   LineFeed

   
PRI LineFeed | i
  if (row += 2) == rows-6
    row -= 2
    'scroll lines
    repeat i from 0 to rows-9
      wordmove(@array.word[i*cols], @array.word[(i+2)*cols], cols)
    'clear new line
    wordfill(@array.word[(rows-8)*cols], spacetile, cols<<1)


PRI rLineFeed | i

  if (row -= 2) < 0
     row := 0
    'scroll lines
    repeat i from rows-9 to 0 
      wordmove(@array.word[(i+2)*cols],@array.word[i*cols], cols)
    'clear new line
    wordfill(@array.word[0], spacetile, cols<<1)
    

DAT


cursorptr  long

   long  $00000000     'cursor_x
   long  $00000000     'cursor_y
   long  $00000000     'cursor_color
   long  $00000000     ' cursor_shape  - 0 for arrow, 1 for crosshair, or pointer to a cursor
''                                       definition. A cursor definition consists of 32 longs
''                                       containing a 32x32 pixel cursor image, followed by two
''                                       bytes which define the X and Y center-pixel offsets
''                                       within the image.
Cursor1 long

  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  byte 0,0


Cursor2 long

  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %00000000_00000000_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  long %11111111_11111111_00000000_00000000
  byte 0,0  
  
vgacolors long

  long $0C000C00         '       Blue    Black   0
  long $0C0C0000                                
  long $30003000         '       Green   Black   1
  long $30300000                                
  long $3C003C00         '       Cyan    Black   2
  long $3C3C0000                                
  long $C000C000         '       Red     Black   3
  long $C0C00000                                
  long $CC00CC00         '       Purple  Black   4
  long $CCCC0000                                
  long $F000F000         '       Yellow  Black   5
  long $F0F00000
  long $00280028         '       Black   Cyan    6
  long $00002828                                
  long $0C280C28         '       Blue    Cyan    7
  long $0C0C2828                                
  long $30283028         '       Green   Cyan    8
  long $30302828                                
  long $3C283C28         '       Cyan    Cyan    9
  long $3C3C2828                                
  long $C028C028         '       Red     Cyan    10
  long $C0C02828                                
  long $CC28CC28         '       Purple  Cyan    11
  long $CCCC2828                                
  long $F028F028         '       Yellow  Cyan    12
  long $F0F02828                                
  long $00800080         '       Black   Red     13
  long $00008080                                
  long $0C800C80         '       Blue    Red     14
  long $0C0C8080                                
  long $30803080         '       Green   Red     15
  long $30308080                                
  long $3C803C80         '       Cyan    Red     16
  long $3C3C8080                                
  long $CC80CC80         '       Purple  Red     17
  long $CCCC8080                                
  long $F080F080         '       Yellow  Red     18
  long $F0F08080                                
  long $00A800A8         '       Black   Grey    19
  long $0000A8A8                                
  long $0CA80CA8         '       Blue    Grey    20
  long $0C0CA8A8                                
  long $30A830A8         '       Green   Grey    21
  long $3030A8A8                                
  long $3CA83CA8         '       Cyan    Grey    22
  long $3C3CA8A8                                
  long $C0A8C0A8         '       Red     Grey    23
  long $C0C0A8A8                                
  long $CCA8CCA8         '       Purple  Grey    24
  long $CCCCA8A8                                
  long $F0A8F0A8         '       Yellow  Grey    25
  long $F0F0A8A8                                
  long $00200020         '       Black   Green   26
  long $00002020                                
  long $0C200C20         '       Blue    Green   27
  long $0C0C2020                                
  long $3C203C20         '       Cyan    Green   28
  long $3C3C2020                                
  long $C020C020         '       Red     Green   29
  long $C0C02020                                
  long $C0408080       'redbox
  long $3010F020       'greenbox
  long $3C142828       'cyanbox
  long $FC54A8A8       'greybox



  

     