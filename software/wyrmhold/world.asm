;______________________________________________________________________________
;  world.asm - map data, tile property tables and lookup helpers
;
;  Maps are authored as ASCII rows for readability and converted to
;  tile codes once at startup (map_decode).  The decoded tile grids
;  live in RAM (owmap / locmap).  Authoring characters:
;     . grass   & forest  ^ mountain  ~ water   T town    O dungeon
;     # castle  : road     = bridge    space->grass
;  For interiors (town/dungeon):
;     . floor   # wall     + door(exit)  $ treasure  < stairs up
;     S shop    ~ water
;______________________________________________________________________________

;----------------------------------------------------------------
; Tile glyph table  (tile code -> ASCII char)
;----------------------------------------------------------------
tile_glyph:
        .BYTE   G_GRASS         ; 0  T_GRASS
        .BYTE   G_FOREST        ; 1  T_FOREST
        .BYTE   G_MOUNT         ; 2  T_MOUNT
        .BYTE   G_WATER         ; 3  T_WATER
        .BYTE   G_TOWN          ; 4  T_TOWN
        .BYTE   G_DUNG          ; 5  T_DUNG
        .BYTE   G_CASTLE        ; 6  T_CASTLE
        .BYTE   G_ROAD          ; 7  T_ROAD
        .BYTE   G_BRIDGE        ; 8  T_BRIDGE
        .BYTE   G_FLOOR         ; 9  T_FLOOR
        .BYTE   G_WALL          ; 10 T_WALL
        .BYTE   G_DOOR          ; 11 T_DOOR
        .BYTE   G_TREAS         ; 12 T_TREAS
        .BYTE   G_UPSTAIR       ; 13 T_UPSTAIR
        .BYTE   G_SHOP          ; 14 T_SHOP

;----------------------------------------------------------------
; Tile color table  (tile code -> color byte)
;----------------------------------------------------------------
tile_color:
        .BYTE   C_GRASS         ; 0
        .BYTE   C_FOREST        ; 1
        .BYTE   C_MOUNT         ; 2
        .BYTE   C_WATER         ; 3
        .BYTE   C_TOWN          ; 4
        .BYTE   C_DUNG          ; 5
        .BYTE   C_CASTLE        ; 6
        .BYTE   C_ROAD          ; 7
        .BYTE   C_BRIDGE        ; 8
        .BYTE   C_FLOOR         ; 9
        .BYTE   C_WALL          ; 10
        .BYTE   C_EXIT          ; 11 door (bright = exit)
        .BYTE   C_TREAS         ; 12
        .BYTE   C_EXIT          ; 13 stairs (bright = exit)
        .BYTE   C_TOWN          ; 14 shop

;----------------------------------------------------------------
; Tile property table  (tile code -> property bits)
;   bit0 PASS    passable on foot
;   bit1 WATER   water (impassable without ship)
;   bit2 TOWN    stepping here enters a town
;   bit3 DUNG    stepping here enters the dungeon
;   bit4 EXIT    stepping here leaves the interior (town/dungeon)
;   bit5 TREAS   treasure to collect
;   bit6 SHOP    shop counter (open shop when adjacent/entering)
;----------------------------------------------------------------
P_PASS          = $01
P_WATER         = $02
P_TOWN          = $04
P_DUNG          = $08
P_EXIT          = $10
P_TREAS         = $20
P_SHOP          = $40

tile_prop:
        .BYTE   P_PASS                  ; 0 grass
        .BYTE   P_PASS                  ; 1 forest (passable, slow flavor)
        .BYTE   $00                     ; 2 mountain (blocked)
        .BYTE   P_WATER                 ; 3 water (blocked on foot)
        .BYTE   P_PASS|P_TOWN           ; 4 town
        .BYTE   P_PASS|P_DUNG           ; 5 dungeon
        .BYTE   $00                     ; 6 castle (blocked, decorative)
        .BYTE   P_PASS                  ; 7 road
        .BYTE   P_PASS                  ; 8 bridge
        .BYTE   P_PASS                  ; 9 floor
        .BYTE   $00                     ; 10 wall
        .BYTE   P_PASS|P_EXIT           ; 11 door (exit interior)
        .BYTE   P_PASS|P_TREAS          ; 12 treasure
        .BYTE   P_PASS|P_EXIT           ; 13 stairs up (exit dungeon)
        .BYTE   P_PASS|P_SHOP           ; 14 shop

;----------------------------------------------------------------
; Authoring char -> tile code translation (used by map_decode).
; Two parallel tables: chars[] and codes[], terminated by $00.
; (Distinct chars only; '#' = wall by default. The overworld uses
;  'C' for the decorative castle so it does not collide with walls.)
;----------------------------------------------------------------
dec_chars:
        .BYTE   ".&^~TOC:=#+$<S", $00
dec_codes:
        .BYTE   T_GRASS         ; .
        .BYTE   T_FOREST        ; &
        .BYTE   T_MOUNT         ; ^
        .BYTE   T_WATER         ; ~
        .BYTE   T_TOWN          ; T
        .BYTE   T_DUNG          ; O
        .BYTE   T_CASTLE        ; C
        .BYTE   T_ROAD          ; :
        .BYTE   T_BRIDGE        ; =
        .BYTE   T_WALL          ; #
        .BYTE   T_DOOR          ; +
        .BYTE   T_TREAS         ; $
        .BYTE   T_UPSTAIR       ; <
        .BYTE   T_SHOP          ; S

; map a single authoring char (A) to a tile code -> A
xlate_char:
        STA     tmp0
        LDX     #0
@f:
        LDA     dec_chars,X
        BEQ     @nf
        CMP     tmp0
        BEQ     @ok
        INX
        BNE     @f
@nf:
        LDA     #T_GRASS
        RTS
@ok:
        LDA     dec_codes,X
        RTS

;----------------------------------------------------------------
; map_decode - translate an authored ASCII map into a tile grid,
; row by row.  Each source row is a NUL-terminated string.  Rows
; shorter than the map width are padded with grass; longer rows are
; truncated.  This makes exact source-row width unimportant.
;
;   IN : srcp -> first source row (rows packed back-to-back, each
;                NUL-terminated)
;        dstp -> destination tile-code buffer (width*height bytes)
;        tmp2 =  map width   (<=255)
;        tmp3 =  map height  (<=255)
;----------------------------------------------------------------
; NOTE: the column counter is kept in cnt0 (memory), NOT in X,
; because xlate_char clobbers X (it uses X as a search index).
map_decode:
        LDA     #0
        STA     rowidx          ; row counter
@nextrow:
        LDA     rowidx
        CMP     tmp3
        BCS     @done
        LDA     #0
        STA     cnt0            ; column within row
@col:
        LDA     cnt0
        CMP     tmp2
        BCS     @rowfull
        ; cnt0 tracks column for width logic; srcp is the read head.
        LDY     #0
        LDA     (srcp),Y
        BEQ     @padrow         ; early NUL -> pad rest with grass
        JSR     xlate_char      ; A=tile code (clobbers X)
        LDY     #0
        STA     (dstp),Y
        ; advance read head and dest
        INC     srcp
        BNE     :+
        INC     srcp+1
:
        INC     dstp
        BNE     :+
        INC     dstp+1
:
        INC     cnt0
        JMP     @col
@rowfull:
        ; consume any extra source chars up to the terminating NUL
@skip:
        LDY     #0
        LDA     (srcp),Y
        BEQ     @aftrow
        INC     srcp
        BNE     @skip
        INC     srcp+1
        JMP     @skip
@padrow:
        ; fill remaining columns (cnt0..width-1) with grass in dest
@pad:
        LDA     cnt0
        CMP     tmp2
        BCS     @aftrow
        LDA     #T_GRASS
        LDY     #0
        STA     (dstp),Y
        INC     dstp
        BNE     :+
        INC     dstp+1
:
        INC     cnt0
        JMP     @pad
@aftrow:
        ; step srcp past the row terminator NUL
        INC     srcp
        BNE     :+
        INC     srcp+1
:
        INC     rowidx
        JMP     @nextrow
@done:
        RTS

;----------------------------------------------------------------
; decode_world / decode_town / decode_dung - convenience wrappers
;----------------------------------------------------------------
decode_world:
        SETW16  srcp, ow_src
        SETW16  dstp, owmap
        LDA     #OWW
        STA     tmp2
        LDA     #OWH
        STA     tmp3
        JMP     map_decode

decode_town:
        SETW16  srcp, town_src
        SETW16  dstp, locmap
        LDA     #TOWNW
        STA     tmp2
        LDA     #TOWNH
        STA     tmp3
        JMP     map_decode

decode_dung:
        SETW16  srcp, dung_src
        SETW16  dstp, locmap
        LDA     #DUNGW
        STA     tmp2
        LDA     #DUNGH
        STA     tmp3
        JMP     map_decode

;----------------------------------------------------------------
; tileat - read the tile code at (tgtx,tgty) from the ACTIVE map.
;   IN : tgtx, tgty ; loc (LOC_*) selects map + width
;   OUT: A = tile code, also stored in tgttile
;        if out of bounds -> returns T_WATER (overworld) / T_WALL (int)
;   Trashes ptr, tmp0, tmp1
;----------------------------------------------------------------
tileat:
        LDA     loc
        BNE     @interior
;--- overworld ---
        ; bounds check 0..OWW-1 / 0..OWH-1
        LDA     tgtx
        CMP     #OWW
        BCS     @oob_world
        LDA     tgty
        CMP     #OWH
        BCS     @oob_world
        ; offset = tgty*OWW + tgtx  (OWW=64 -> *64)
        LDA     tgty
        STA     ptr
        LDA     #0
        STA     ptr+1
        ASL     ptr
        ROL     ptr+1           ; *2
        ASL     ptr
        ROL     ptr+1           ; *4
        ASL     ptr
        ROL     ptr+1           ; *8
        ASL     ptr
        ROL     ptr+1           ; *16
        ASL     ptr
        ROL     ptr+1           ; *32
        ASL     ptr
        ROL     ptr+1           ; *64
        CLC
        LDA     ptr
        ADC     tgtx
        STA     ptr
        LDA     ptr+1
        ADC     #0
        STA     ptr+1
        ; ptr += owmap base
        CLC
        LDA     ptr
        ADC     #<owmap
        STA     ptr
        LDA     ptr+1
        ADC     #>owmap
        STA     ptr+1
        LDY     #0
        LDA     (ptr),Y
        STA     tgttile
        RTS
@oob_world:
        LDA     #T_WATER
        STA     tgttile
        RTS
;--- interior (town/dungeon): width = locw, height = loch ---
@interior:
        LDA     tgtx
        CMP     locw
        BCS     @oob_int
        LDA     tgty
        CMP     loch
        BCS     @oob_int
        ; offset = tgty*locw + tgtx  (locw is 32)
        LDA     tgty
        STA     ptr
        LDA     #0
        STA     ptr+1
        ASL     ptr
        ROL     ptr+1           ; *2
        ASL     ptr
        ROL     ptr+1           ; *4
        ASL     ptr
        ROL     ptr+1           ; *8
        ASL     ptr
        ROL     ptr+1           ; *16
        ASL     ptr
        ROL     ptr+1           ; *32  (locw assumed 32)
        CLC
        LDA     ptr
        ADC     tgtx
        STA     ptr
        LDA     ptr+1
        ADC     #0
        STA     ptr+1
        CLC
        LDA     ptr
        ADC     #<locmap
        STA     ptr
        LDA     ptr+1
        ADC     #>locmap
        STA     ptr+1
        LDY     #0
        LDA     (ptr),Y
        STA     tgttile
        RTS
@oob_int:
        LDA     #T_WALL
        STA     tgttile
        RTS

;----------------------------------------------------------------
; settile - write tile code A at (tgtx,tgty) in the active map.
;   Used to clear treasure chests once collected, etc.
;   Recomputes the address exactly like tileat.
;----------------------------------------------------------------
settile:
        STA     tmp1            ; new tile
        JSR     tile_addr       ; ptr -> cell
        LDY     #0
        LDA     tmp1
        STA     (ptr),Y
        RTS

; tile_addr - compute ptr -> active-map cell for (tgtx,tgty).
; (assumes in-bounds; callers that might be OOB use tileat first)
tile_addr:
        LDA     loc
        BNE     @int
        LDA     tgty
        STA     ptr
        LDA     #0
        STA     ptr+1
        ASL     ptr
        ROL     ptr+1
        ASL     ptr
        ROL     ptr+1
        ASL     ptr
        ROL     ptr+1
        ASL     ptr
        ROL     ptr+1
        ASL     ptr
        ROL     ptr+1
        ASL     ptr
        ROL     ptr+1           ; *64
        CLC
        LDA     ptr
        ADC     tgtx
        STA     ptr
        LDA     ptr+1
        ADC     #0
        STA     ptr+1
        CLC
        LDA     ptr
        ADC     #<owmap
        STA     ptr
        LDA     ptr+1
        ADC     #>owmap
        STA     ptr+1
        RTS
@int:
        LDA     tgty
        STA     ptr
        LDA     #0
        STA     ptr+1
        ASL     ptr
        ROL     ptr+1
        ASL     ptr
        ROL     ptr+1
        ASL     ptr
        ROL     ptr+1
        ASL     ptr
        ROL     ptr+1
        ASL     ptr
        ROL     ptr+1           ; *32
        CLC
        LDA     ptr
        ADC     tgtx
        STA     ptr
        LDA     ptr+1
        ADC     #0
        STA     ptr+1
        CLC
        LDA     ptr
        ADC     #<locmap
        STA     ptr
        LDA     ptr+1
        ADC     #>locmap
        STA     ptr+1
        RTS

;----------------------------------------------------------------
; Authored overworld map (64 x 64).
; Surrounded by water; a central continent with two towns, a
; castle, the dungeon entrance, forests, mountains and a river
; crossed by a bridge.
;----------------------------------------------------------------
; Maps are stored as NUL-terminated rows.  The decoder pads short
; rows with grass and truncates long ones, so the exact authored
; width of each row is not critical (target widths shown in the
; header rulers for readability).
ow_src:
        ;        0         1         2         3         4         5         6
        ;        0123456789012345678901234567890123456789012345678901234567890123
        .BYTE   "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",0
        .BYTE   "~..............................................................~~",0
        .BYTE   "~..&&&......^^^^.........&&&&&...........^^^...........&&&.....~~~",0
        .BYTE   "~.&&&&&....^^^^^^.......&&&&&&&.........^^^^^.........&&&&&....~~~~",0
        .BYTE   "~.&&&......^^^^^^^.....&&&...&&........^^^^^^........&&&&&&&...~~~~",0
        .BYTE   "~..&.......^^^^^^.....&&.......&&......^^^^^.........&&&&&....~~~~~",0
        .BYTE   "~.........^^^^.......&&.........&......^^^...........&&&......~~~~~",0
        .BYTE   "~....................................................T.......~~~~~",0
        .BYTE   "~...............:::::::::::....................::::::::......~~~~~~",0
        .BYTE   "~..&&......^^^..:..............:::::::::::::::::.......:.....~~~~~~",0
        .BYTE   "~.&&&&....^^^^^.:..............................:......:.....~~~~~~",0
        .BYTE   "~.&&&....^^^^^^.:.............#####............:......:....~~~~~~~",0
        .BYTE   "~..&....^^^^^...:.............#####............:......:...~~~~~~~~",0
        .BYTE   "~......^^^^....::.............#####............:......:...~~~~~~~~",0
        .BYTE   "~.....^^^....::...............:::::............:......:..~~~~~~~~~",0
        .BYTE   "~..........::................:....:...........:......:..~~~~~~~~~~",0
        .BYTE   "~.........::...............:.......:..........:.....::..~~~~~~~~~~",0
        .BYTE   "~........::...............:.........:.........:.....:...~~~~~~~~~~",0
        .BYTE   "~~~~~~~~==~~~~~~~~~~~~~~~~~:.........:~~~~~~~~~~:~~~~~:~~~~~~~~~~~~~",0
        .BYTE   "~.......::...............:..........:.........:.....:.......~~~~~~",0
        .BYTE   "~......::...............:............:........:.....:.......~~~~~~",0
        .BYTE   "~.....::.........&&&...:.............:........:.....:.......~~~~~~",0
        .BYTE   "~....::.........&&&&&.:..............:........:.....:.......~~~~~~",0
        .BYTE   "~...::..........&&&&&:...............:........:.....:.......~~~~~~",0
        .BYTE   "~..::...........&&&&:................:........:.....:.......~~~~~~",0
        .BYTE   "~.::...........&&&:.................:.........:.....:......~~~~~~~",0
        .BYTE   "~::...........&&:.........^^^^^....:..........:....::......~~~~~~~",0
        .BYTE   "~:...........&:..........^^...^^..:...........:....:.......~~~~~~~",0
        .BYTE   "~...........::..........^^..O..^^.:...........:....:.......~~~~~~~",0
        .BYTE   "~..........:...........^^^^...^^^:............:....:.......~~~~~~~",0
        .BYTE   "~.........:...........^^^^^^..^^^^............:....:.......~~~~~~~",0
        .BYTE   "~........:...........^^^^^^^...^^^^...........:....:.......~~~~~~~",0
        .BYTE   "~.......:...........................&&&.......:....:.......~~~~~~~",0
        .BYTE   "~......:...........&&&&............&&&&&......:....:........~~~~~~",0
        .BYTE   "~.....:...........&&&&&&..........&&&&&&&.....:....:........~~~~~~",0
        .BYTE   "~....:...........&&&&&&&.........&&&&&&&&.....:....:........~~~~~~",0
        .BYTE   "~...:...........&&&&&&&..........&&&&&&&......:....:........~~~~~~",0
        .BYTE   "~..:............&&&&&&.............&&&&.......:....::.......~~~~~~",0
        .BYTE   "~..:...........&&&&&................&&........:.....:.......~~~~~~",0
        .BYTE   "~..:........................T................:.....:.......~~~~~~",0
        .BYTE   "~..:......................:::::::............:.....:.......~~~~~~",0
        .BYTE   "~..:.....................:.......:...........:.....:.......~~~~~~",0
        .BYTE   "~..::...................:.........:..........:.....:.......~~~~~~",0
        .BYTE   "~...:..................:..^^^^.....:.........:.....:.......~~~~~~",0
        .BYTE   "~...:.................:..^^^^^^.....:........:.....:.......~~~~~~",0
        .BYTE   "~...:................:..^^^^^^^^.....:.......:.....:.......~~~~~~",0
        .BYTE   "~...&&..............:..^^^^^^^^^^....:.......:.....:.......~~~~~~",0
        .BYTE   "~..&&&&............:..^^^^^^^^^^^^....:......:.....:.......~~~~~~",0
        .BYTE   "~..&&&&...........:..^^^^^^^^^^^^^^...:......:.....:.......~~~~~~",0
        .BYTE   "~...&&...........:....^^^^^^^^^^^^...:.......:.....:.......~~~~~~",0
        .BYTE   "~...............:......^^^^^^^^^^...:........:.....:.......~~~~~~",0
        .BYTE   "~..............:........^^^^^^^...:.........::.....:.......~~~~~~",0
        .BYTE   "~.............:...........^^^...:..........:......::.......~~~~~~",0
        .BYTE   "~............:..................:..........:.......:.......~~~~~~",0
        .BYTE   "~...........:.................::...........:.......&&&......~~~~~~",0
        .BYTE   "~..........................................:......&&&&&.....~~~~~~",0
        .BYTE   "~....&&&.................................&&&:......&&&&&.....~~~~~~",0
        .BYTE   "~...&&&&&..............................&&&&&.......&&&&......~~~~~~",0
        .BYTE   "~...&&&&&............................&&&&&&&................~~~~~~~",0
        .BYTE   "~....&&&............................&&&&&&.................~~~~~~~~",0
        .BYTE   "~................................&&&&...................~~~~~~~~~~",0
        .BYTE   "~....................................................~~~~~~~~~~~~~",0
        .BYTE   "~~..............................................~~~~~~~~~~~~~~~~~~",0
        .BYTE   "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",0

;----------------------------------------------------------------
; Authored town map (32 x 20).  '+' on the border is the exit.
;----------------------------------------------------------------
town_src:
        ;                 1         2         3
        ;        12345678901234567890123456789012
        .BYTE   "################################",0
        .BYTE   "#..............................#",0
        .BYTE   "#..####....####....####....###.#",0
        .BYTE   "#..#..#....#..#....#SS#....#.#.#",0
        .BYTE   "#..#..#....#..#....#..#....#.#.#",0
        .BYTE   "#..####....####....#..#....###.#",0
        .BYTE   "#..............................#",0
        .BYTE   "#...........::::::::...........#",0
        .BYTE   "#..####.....:......:....####...#",0
        .BYTE   "#..#..#.....:......:....#..#...#",0
        .BYTE   "#..#..#.....:......:....#..#...#",0
        .BYTE   "#..####.....:......:....####...#",0
        .BYTE   "#...........::::::::...........#",0
        .BYTE   "#..............................#",0
        .BYTE   "#..~~~~....####....####...~~~..#",0
        .BYTE   "#..~~~~....#..#....#..#...~~~..#",0
        .BYTE   "#..........#..#....#..#........#",0
        .BYTE   "#..........####....####........#",0
        .BYTE   "##############++################",0
        .BYTE   "################################",0

;----------------------------------------------------------------
; Authored dungeon map (32 x 20).  '<' is stairs up (exit),
; '$' are treasure chests, the boss is placed by entity spawn.
;----------------------------------------------------------------
dung_src:
        .BYTE   "################################",0
        .BYTE   "#<.....#......#........#.......#",0
        .BYTE   "#.####.#.####.#.######.#.#####.#",0
        .BYTE   "#.#..#.#.#..#.#.#....#.#.#...#.#",0
        .BYTE   "#.#$.#...#..#...#.$..#...#.$.#.#",0
        .BYTE   "#.#..####..#.####.####.###...#.#",0
        .BYTE   "#.#.......#....#....#......#.#.#",0
        .BYTE   "#.#.#####.####.#.##.#.####.#.#.#",0
        .BYTE   "#...#...#....#.#..#.#..$.#.#.#.#",0
        .BYTE   "###.#.#.####.#.##.#.####.#.#.#.#",0
        .BYTE   "#...#.#....#.#..#.#....#.#.#.#.#",0
        .BYTE   "#.###.####.#.#.##.####.#.#.#.#.#",0
        .BYTE   "#.#......#.#.#..#....#.#...#...#",0
        .BYTE   "#.#.####.#.#.##.####.#.#####.#.#",0
        .BYTE   "#.#.#..#...#..#....#.......#.#.#",0
        .BYTE   "#.#.#.############.#######.#.#.#",0
        .BYTE   "#...#...........$........#...#.#",0
        .BYTE   "#.#############.########.#.###.#",0
        .BYTE   "#.............................$#",0
        .BYTE   "################################",0
