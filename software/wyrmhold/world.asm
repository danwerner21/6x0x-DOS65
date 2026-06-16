;______________________________________________________________________________
;  world.asm - map data, tile property tables and lookup helpers
;
;  Maps are authored as ASCII rows for readability and converted to
;  tile codes once at startup (map_decode).  The decoded tile grids
;  live in RAM (owmap / locmap).  Authoring characters:
;     . grass   & forest  ^ mountain  ~ water   h hills   % marsh
;     T town    O dungeon C castle    : road    = bridge
;     space->grass
;  For interiors (town/dungeon/castle/shrine):
;     . floor   # wall     + door(exit)  $ treasure  < stairs up
;     S shop    ~ water
;______________________________________________________________________________

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
        .BYTE   C_HILLS         ; 15
        .BYTE   C_MARSH         ; 16

;----------------------------------------------------------------
; Tile variant table  (tile code -> alternate metatile base, or 0)
; A non-zero entry means this terrain has a 2nd art variant; the
; renderer may use it instead of the MG_* base for some map cells
; to break up the repeating-grid look. 0 = no variant (use base).
;----------------------------------------------------------------
tile_variant:
        .BYTE   MGV_GRASS       ; 0  grass
        .BYTE   MGV_FOREST      ; 1  forest
        .BYTE   MGV_MOUNT       ; 2  mountain
        .BYTE   MGV_WATER       ; 3  water
        .BYTE   0               ; 4  town
        .BYTE   0               ; 5  dungeon
        .BYTE   0               ; 6  castle
        .BYTE   0               ; 7  road
        .BYTE   0               ; 8  bridge
        .BYTE   0               ; 9  floor
        .BYTE   0               ; 10 wall
        .BYTE   0               ; 11 door
        .BYTE   0               ; 12 treasure
        .BYTE   0               ; 13 stairs
        .BYTE   0               ; 14 shop
        .BYTE   0               ; 15 hills
        .BYTE   0               ; 16 marsh

;----------------------------------------------------------------
; Tile property table  (tile code -> property bits)
;   bit0 PASS    passable on foot
;   bit1 WATER   water (impassable without ship)
;   bit2 TOWN    stepping here enters a town
;   bit3 DUNG    stepping here enters the dungeon
;   bit4 EXIT    stepping here leaves the interior (town/dungeon)
;   bit5 TREAS   treasure to collect
;   bit6 SHOP    shop counter (open shop when adjacent/entering)
;   bit7 CASTLE  stepping here enters Wyrmhold Castle
;----------------------------------------------------------------
P_PASS          = $01
P_WATER         = $02
P_TOWN          = $04
P_DUNG          = $08
P_EXIT          = $10
P_TREAS         = $20
P_SHOP          = $40
P_CASTLE        = $80

tile_prop:
        .BYTE   P_PASS          ; 0 grass
        .BYTE   P_PASS          ; 1 forest (passable, slow flavor)
        .BYTE   $00             ; 2 mountain (blocked)
        .BYTE   P_WATER         ; 3 water (blocked on foot)
        .BYTE   P_PASS|P_TOWN   ; 4 town
        .BYTE   P_PASS|P_DUNG   ; 5 dungeon
        .BYTE   P_PASS|P_CASTLE ; 6 castle
        .BYTE   P_PASS          ; 7 road
        .BYTE   P_PASS          ; 8 bridge
        .BYTE   P_PASS          ; 9 floor
        .BYTE   $00             ; 10 wall
        .BYTE   P_PASS|P_EXIT   ; 11 door (exit interior)
        .BYTE   P_PASS|P_TREAS  ; 12 treasure
        .BYTE   P_PASS|P_EXIT   ; 13 stairs up (exit dungeon)
        .BYTE   P_PASS|P_SHOP   ; 14 shop
        .BYTE   P_PASS          ; 15 hills
        .BYTE   P_PASS          ; 16 marsh

;----------------------------------------------------------------
; Authoring char -> tile code translation (used by map_decode).
; Two parallel tables: chars[] and codes[], terminated by $00.
; The current decoder supplies the map-specific default tile:
; grass outdoors and floor indoors. Thus '.' and unknown/space
; characters become the appropriate default without a table entry.
;----------------------------------------------------------------
dec_chars:
        .BYTE   "&^~TOC:=#+$<Sh%", $00
dec_codes:
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
        .BYTE   T_HILLS         ; h
        .BYTE   T_MARSH         ; %

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
        LDA     cnt1            ; map-specific default tile
        RTS
@ok:
        LDA     dec_codes,X
        RTS

;----------------------------------------------------------------
; map_decode - translate an authored ASCII map into a tile grid,
; row by row.  Each source row is a NUL-terminated string.  Rows
; shorter than the map width are padded with the map default; longer
; rows are truncated.  This makes exact source-row width unimportant.
;
;   IN : srcp -> first source row (rows packed back-to-back, each
;                NUL-terminated)
;        dstp -> destination tile-code buffer (width*height bytes)
;        tmp2 =  map width   (<=255)
;        tmp3 =  map height  (<=255)
;        cnt1 = default tile (grass outdoors, floor indoors)
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
; fill remaining columns (cnt0..width-1) with the map default
@pad:
        LDA     cnt0
        CMP     tmp2
        BCS     @aftrow
        LDA     cnt1
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
; decode_world / decode_town / decode_dung / decode_castle / decode_shrine
;----------------------------------------------------------------
decode_world:
        SETW16  srcp, ow_src
        SETW16  dstp, owmap
        LDA     #T_GRASS
        STA     cnt1
        LDA     #OWW
        STA     tmp2
        LDA     #OWH
        STA     tmp3
        JMP     map_decode

decode_town:
        LDA     town_id
        CMP     #TOWN_VALEHAVEN
        BEQ     @valehaven
        SETW16  srcp, eastmere_src
        JMP     @decode
@valehaven:
        SETW16  srcp, valehaven_src
@decode:
        SETW16  dstp, locmap
        LDA     #T_FLOOR
        STA     cnt1
        LDA     #TOWNW
        STA     tmp2
        LDA     #TOWNH
        STA     tmp3
        JMP     map_decode

decode_dung:
        SETW16  srcp, dung_src
        SETW16  dstp, locmap
        LDA     #T_FLOOR
        STA     cnt1
        LDA     #DUNGW
        STA     tmp2
        LDA     #DUNGH
        STA     tmp3
        JMP     map_decode

decode_castle:
        SETW16  srcp, castle_src
        SETW16  dstp, locmap
        LDA     #T_FLOOR
        STA     cnt1
        LDA     #CASTLEW
        STA     tmp2
        LDA     #CASTLEH
        STA     tmp3
        JMP     map_decode

decode_shrine:
        SETW16  srcp, shrine_src
        SETW16  dstp, locmap
        LDA     #T_FLOOR
        STA     cnt1
        LDA     #SHRINEW
        STA     tmp2
        LDA     #SHRINEH
        STA     tmp3
        JMP     map_decode

;----------------------------------------------------------------
; region_from_y - classify an overworld latitude.
;   IN : A = overworld y coordinate
;   OUT: A = REGION_* id
;----------------------------------------------------------------
region_from_y:
        CMP     #REGION_ROW_VALE
        BCC     @northreach
        CMP     #REGION_ROW_SUNKEN
        BCC     @vale
        LDA     #REGION_SUNKEN
        RTS
@vale:
        LDA     #REGION_VALE
        RTS
@northreach:
        LDA     #REGION_NORTHREACH
        RTS

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
; rows with the map default and truncates long ones, so the exact
; authored width of each row is not critical (target widths shown
; in the header rulers for readability).
ow_src:
;        0         1         2         3         4         5         6
;        0123456789012345678901234567890123456789012345678901234567890123
        .BYTE   "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",0
        .BYTE   "~..............................................................~~",0
        .BYTE   "~..&&&....hh^^^^hh.......&&&&&.........hh^^^hh.........&&&.....~~~",0
        .BYTE   "~.&&&&&..hh^^^^^^hh.....&&&&&&&.......hh^^^^^hh.......&&&&&....~~~~",0
        .BYTE   "~.&&&....hh^^^^^^^hh...&&&...&&......hh^^^^^^hh......&&&&&&&...~~~~",0
        .BYTE   "~..&.....hh^^^^^^hh...&&.......&&....hh^^^^^hh.......&&&&&....~~~~~",0
        .BYTE   "~.......hh^^^^hh.....&&.........&....hh^^^hh.........&&&......~~~~~",0
        .BYTE   "~....................................................T.......~~~~~",0
        .BYTE   "~...............:::::::::::....................::::::::......~~~~~~",0
        .BYTE   "~..&&......^^^..:..............:::::::::::::::::.......:.....~~~~~~",0
        .BYTE   "~.&&&&....^^^^^.:..............................:......:.....~~~~~~",0
        .BYTE   "~.&&&....^^^^^^.:.............hhhhh............:......:....~~~~~~~",0
        .BYTE   "~..&....^^^^^...:.............h~~~h............:......:...~~~~~~~~",0
        .BYTE   "~......^^^^....::.............h~C~h............:......:...~~~~~~~~",0
        .BYTE   "~.....^^^....::..............::::::............:......:..~~~~~~~~~",0
        .BYTE   "~..........::................:....:...........:......:..~~~~~~~~~~",0
        .BYTE   "~.........::...............:::....::..........:.....::..~~~~~~~~~~",0
        .BYTE   "~........::...............:.........:.........:.....:..%%~~~~~~~~~~",0
        .BYTE   "~~~~~~~~==~~~~~~~~~~~~~~~~:........::~~~~~~~~~:~~~~::~~~~~~~~~~~~~~",0
        .BYTE   "~.......::...............:..........:.........:.....:..%%....~~~~~~",0
        .BYTE   "~......::...............::...........:........:.....:.......~~~~~~",0
        .BYTE   "~.....::.........&&&...::.....~~.....:........:.....:.......~~~~~~",0
        .BYTE   "~....::.........&&&&&.::.....~~~~....:........:.....:.......~~~~~~",0
        .BYTE   "~...::..........&&&&&::.......~~.....:........:.....:.......~~~~~~",0
        .BYTE   "~..::...........&&&&::...............:........:.....:.......~~~~~~",0
        .BYTE   "~.::...........&&&:::...............::........:.....:......~~~~~~~",0
        .BYTE   "~::...........&&:&......hh^^^^^hh..::.........:....::......~~~~~~~",0
        .BYTE   "~:...........&:::&&....hh^^...^^hh:...........:....:.......~~~~~~~",0
        .BYTE   "~...........::::......hh^^^^O^^^^h:...........::...::......~~~~~~~",0
        .BYTE   "~..........:::.......hh^^^^...^^^h:...........::...::.......~~~~~~~",0
        .BYTE   "~.........:::.......hh^^^^^^..^^^^hh..........:....:.......~~~~~~~",0
        .BYTE   "~........:::.......hh^^^^^^^...^^^^hh.........:....:.......~~~~~~~",0
        .BYTE   "~.......:::.........hhh.........hhh.&&&.......:....:.......~~~~~~~",0
        .BYTE   "~......:::.........&&&&............&&&&&......:....:........~~~~~~",0
        .BYTE   "~.....:::.........&&&&&&..........&&&&&&&.....:....:........~~~~~~",0
        .BYTE   "~....:::.........&&&&&&&.........&&&&&&&&.....:....:........~~~~~~",0
        .BYTE   "~...:::.........&&&&&&&..........&&&&&&&......:....:........~~~~~~",0
        .BYTE   "~..::...........&&&&&&.............&&&&.......:....::.......~~~~~~",0
        .BYTE   "~..:...........&&&&&................&&........:.....:.......~~~~~~",0
        .BYTE   "~..:........................T................::.....::......~~~~~~",0
        .BYTE   "~..:......................:::::::............:.....:.......~~~~~~",0
        .BYTE   "~..:.....................:.......:...........:.....:.......~~~~~~",0
        .BYTE   "~..::...................hhhh......:..........:....::.......~~~~~~",0
        .BYTE   "~...:................:hh^^^^hh...:.........:::...:::.......~~~~~~~",0
        .BYTE   "~...:...............:hh^^^^^^hh...:........:.....:.......~~~~~~~",0
        .BYTE   "~...:..............:hh^^^^^^^^hh...:.......:.....:.......~~~~~~~",0
        .BYTE   "~...&&............:hh^^^^^^^^^^hh..:.......:.....:.......~~~~~~~",0
        .BYTE   "~..&&&&..........:hh^^^^^^^^^^^^hh..:......:.....:.......~~~~~~~",0
        .BYTE   "~..&&&&.........:hh^^^^^^^^^^^^^^hh.:......:.....:.......~~~~~~~",0
        .BYTE   "~...&&.........:..hh^^^^^^^^^^^^hh.:.......:.....:.......~~~~~~~",0
        .BYTE   "~.............:....hh^^^^^^^^^^hh.:........:.....:.......~~~~~~~",0
        .BYTE   "~............:......hh^^^^^^^hh.:.........::.....:.......~~~~~~~",0
        .BYTE   "~...........:.........hh^^^hh.:..........::.....::.......~~~~~~~",0
        .BYTE   "~............:.........hhhhh....:..........:.....:::.......~~~~~~",0
        .BYTE   "~...........:.................::...........:.......&&&......~~~~~~",0
        .BYTE   "~..........................................:......&&&&&.....~~~~~~",0
        .BYTE   "~....&&&.................................&&&:......&&&&&.....~~~~~~",0
        .BYTE   "~...&&&&&.........~%%..................&&&&&.......&&&&......~~~~~~",0
        .BYTE   "~...&&&&&........~~~O................&&&&&&&................~~~~~~~",0
        .BYTE   "~....&&&..........~%%...........%%..&&&&&&.................~~~~~~~~",0
        .BYTE   "~..............................%%&&&&%%.................~~~~~~~~~~",0
        .BYTE   "~...............................%%................%%%~~~~~~~~~~~~~",0
        .BYTE   "~~..............................................~~~~~~~~~~~~~~~~~~",0
        .BYTE   "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",0

;----------------------------------------------------------------
; Eastmere: a dense coastal town and outfitter. '+' is the exit.
;----------------------------------------------------------------
eastmere_src:
        ;                 1         2         3
        ;        12345678901234567890123456789012
        .BYTE   "################################",0
        .BYTE   "#..............................#",0
        .BYTE   "#.:####...:####..:#####::::###.#",0
        .BYTE   "#.:#..#...:#..#..:#.S.#:..:#.#.#",0
        .BYTE   "#.:#..#...:#..#..:#...#:..:#.#.#",0
        .BYTE   "#.:#.##...:####..:##..#:..:#.#.#",0
        .BYTE   "#.:.......:......:::::::..::::.#",0
        .BYTE   "#.:.......::::::::::...........#",0
        .BYTE   "#.:####...::::....::....####...#",0
        .BYTE   "#.:#..#:::::::....::::::...#...#",0
        .BYTE   "#.:#...:::::::....::::::#..#...#",0
        .BYTE   "#.:####...::::::::::...:####...#",0
        .BYTE   "#.:.......::::::::::...:::.....#",0
        .BYTE   "#.:.......:....:...:::::::.....#",0
        .BYTE   "#.:~~~~...:####:...#.##.::~~~..#",0
        .BYTE   "#.:~~~~...:...#:...#..#..:~~~..#",0
        .BYTE   "#.:.......:#..#:...#..#..:.....#",0
        .BYTE   "#.:::::::::####:...####..:.....#",0
        .BYTE   "###############+################",0
        .BYTE   "################################",0

;----------------------------------------------------------------
; Valehaven: an open market town built around canals and gardens.
;----------------------------------------------------------------
valehaven_src:
        .BYTE   "################################",0
        .BYTE   "#..............................#",0
        .BYTE   "#....~~~~............~~~~......#",0
        .BYTE   "#....~~~~............~~~~......#",0
        .BYTE   "#..............................#",0
        .BYTE   "#..##.###............######....#",0
        .BYTE   "#..#....#............#....#....#",0
        .BYTE   "#..#.S..#............#....#....#",0
        .BYTE   "#..######............######....#",0
        .BYTE   "#..............................#",0
        .BYTE   "#::::::::::::....::::::::::::::#",0
        .BYTE   "#..............................#",0
        .BYTE   "#....####............####......#",0
        .BYTE   "#....#..#............#..#......#",0
        .BYTE   "#....####............####......#",0
        .BYTE   "#..............................#",0
        .BYTE   "#.............:::..............#",0
        .BYTE   "#.............:::..............#",0
        .BYTE   "###############+################",0
        .BYTE   "################################",0

;----------------------------------------------------------------
; Authored dungeon map (32 x 20). '<' is stairs up (exit).
; The upper halls, flooded crossing, and open southern dragon
; chamber create three distinct encounter zones.
;----------------------------------------------------------------
dung_src:
        .BYTE   "################################",0
        .BYTE   "#<..........#..................#",0
        .BYTE   "#...........#..................#",0
        .BYTE   "#..######...#....~~~~..........#",0
        .BYTE   "#..#....#...#....~~~~..........#",0
        .BYTE   "#..#.$..#........~~~~..........#",0
        .BYTE   "#..######...#....~~~~..........#",0
        .BYTE   "#...........#....====..........#",0
        .BYTE   "######.#######...====.##########",0
        .BYTE   "#.................==...........#",0
        .BYTE   "#.######.########.==.########..#",0
        .BYTE   "#......#..........==...........#",0
        .BYTE   "#.~~~~.#.########.==.########..#",0
        .BYTE   "#.~~~~.#..........==...........#",0
        .BYTE   "#.~~~~.##########.==.########..#",0
        .BYTE   "#.~~~~............==...........#",0
        .BYTE   "#.##################.########..#",0
        .BYTE   "#..............................#",0
        .BYTE   "#.............................$#",0
        .BYTE   "################################",0

;----------------------------------------------------------------
; Authored castle audience chamber (32 x 20). The ruler is drawn
; over the central throne at CASTLE_RULER_X/Y. '+' is the exit.
;----------------------------------------------------------------
castle_src:
        .BYTE   "################################",0
        .BYTE   "#..............................#",0
        .BYTE   "#..........##########..........#",0
        .BYTE   "#..........#........#..........#",0
        .BYTE   "#..........#...#....#..........#",0
        .BYTE   "#..........#........#..........#",0
        .BYTE   "#..........####..####..........#",0
        .BYTE   "#..............................#",0
        .BYTE   "#....#####..........#####......#",0
        .BYTE   "#....#..................#......#",0
        .BYTE   "#....#..................#......#",0
        .BYTE   "#....#####..........#####......#",0
        .BYTE   "#..............................#",0
        .BYTE   "#..............................#",0
        .BYTE   "#..............::..............#",0
        .BYTE   "#..............::..............#",0
        .BYTE   "#..............::..............#",0
        .BYTE   "#..............::..............#",0
        .BYTE   "###############+################",0
        .BYTE   "################################",0

;----------------------------------------------------------------
; Authored Sunken Shrine (32 x 20). '<' returns to the overworld.
; The Wyrm Warden is spawned in the southeast chamber.
;----------------------------------------------------------------
shrine_src:
        .BYTE   "################################",0
        .BYTE   "#<.............................#",0
        .BYTE   "#..............................#",0
        .BYTE   "#....~~~~..........~~~~........#",0
        .BYTE   "#....~~~~..........~~~~........#",0
        .BYTE   "#....~~~~..........~~~~........#",0
        .BYTE   "#..............................#",0
        .BYTE   "#.######................######.#",0
        .BYTE   "#.#....#................#....#.#",0
        .BYTE   "#.#....#................#....#.#",0
        .BYTE   "#.######................######.#",0
        .BYTE   "#..............................#",0
        .BYTE   "#....~~~~..........~~~~........#",0
        .BYTE   "#....~~~~..........~~~~........#",0
        .BYTE   "#....~~~~..........~~~~........#",0
        .BYTE   "#..............................#",0
        .BYTE   "#..........########............#",0
        .BYTE   "#..............................#",0
        .BYTE   "#..............................#",0
        .BYTE   "################################",0
