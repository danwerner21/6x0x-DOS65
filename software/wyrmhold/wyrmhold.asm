;______________________________________________________________________________
;  WYRMHOLD  -  an original top-down fantasy RPG for the 6502PC / DOS/65
;
;  Written in 6502 assembly (ca65).  Uses the memory-mapped video card
;  (direct VRAM via MMU paging, the SpeedScript way) and the AY-3-8910
;  PSG for a title melody and sound effects.
;
;  Program loads at $0800 (DOS/65 TEA) and exits back to DOS with JMP $0100.
;______________________________________________________________________________
        .PSC02

        .SEGMENT "TEA"
        .ORG    $0800

        .INCLUDE "macro.asm"
        .INCLUDE "defines.asm"

;==============================================================================
; Entry point
;==============================================================================
BEGIN:
        CLD
        JSR     psg_init
        JSR     rng_seed
; All direct-VRAM rendering below assumes 80-byte screen rows.
; Establish that mode before uploading glyphs or drawing anything.
        LDA     #FC_SETMODE
        STA     farfunct
        LDA     #1              ; 80-column text mode
        JSR     DO_FARCALL
        JSR     chargen_init    ; upload custom terrain/monster tiles
; hide the firmware text cursor; we draw our own world
        LDA     #0
        STA     SHOWCRSR
title_loop_outer:
        JSR     title_screen    ; returns when a key is pressed
        JSR     new_game
        JSR     game_loop       ; returns on death / victory / quit
; game_loop decides what to do; if it returns we go to DOS
        JMP     exit_to_dos

;==============================================================================
; Title screen - draws a banner, plays the melody, waits for a key.
;==============================================================================
title_screen:
        JSR     draw_title_scene; full graphical title (title.asm)

; Settle period: for the first stretch of the title we PLAY the
; music but ignore (and keep draining) any keys.  This swallows
; the Enter typed to launch the program - on re-runs that key is
; sometimes delivered just after we start, and without this it
; would dismiss the title instantly (the "no intro on 2nd run"
; bug).
        JSR     music_start
        LDX     #40             ; settle ticks
@settle:
        PHX
        JSR     music_tick
        JSR     rng_timing_tick
        JSR     getkey          ; read & discard whatever shows up
        PLX
        DEX
        BNE     @settle
        JSR     drainkeys       ; clear anything still buffered
@spin:
        JSR     music_tick
        JSR     rng_timing_tick
        JSR     getkey
        BEQ     @spin
        STA     keych
        JSR     rng_mix_timing
        JSR     psg_silence
        RTS

;==============================================================================
; new_game - initialise all game state and draw the initial screen.
;==============================================================================
new_game:
        JSR     player_init
        JSR     decode_world
        LDA     #LOC_WORLD
        STA     loc
        JSR     spawn_overworld_monsters
        JSR     draw_frame
        JSR     msg_clear
        PRINTMSG_MSG intro_msg
        JSR     full_redraw
        RTS

;==============================================================================
; full_redraw - viewport + panel.
;==============================================================================
full_redraw:
        JSR     render_view
        JSR     draw_panel
        RTS

;==============================================================================
; game_loop - the main turn loop.
;==============================================================================
game_loop:
@loop:
; check end conditions first
        LDA     bosskilled
        BEQ     @alive
        JMP     victory
@alive:
        LDA     phealth
        BNE     @hasfood
        JMP     game_over_hp
@hasfood:
        LDA     pfood
        ORA     pfood+1
        BNE     @input
        JMP     game_over_food

@input:
        JSR     getkey
        BEQ     @loop           ; no key - idle (no time passes)
        STA     keych

; movement keys -> set dx,dy then try_move
        LDA     #0
        STA     dx
        STA     dy

        LDA     keych
        CMP     #'w'
        BNE     :+
        JMP     @up
:
        CMP     #'W'
        BNE     :+
        JMP     @up
:
        CMP     #'k'
        BNE     :+
        JMP     @up
:
        CMP     #'s'
        BNE     :+
        JMP     @down
:
        CMP     #'S'
        BNE     :+
        JMP     @down
:
        CMP     #'j'
        BNE     :+
        JMP     @down
:
        CMP     #'a'
        BNE     :+
        JMP     @left
:
        CMP     #'A'
        BNE     :+
        JMP     @left
:
        CMP     #'h'
        BNE     :+
        JMP     @left
:
        CMP     #'d'
        BNE     :+
        JMP     @right
:
        CMP     #'D'
        BNE     :+
        JMP     @right
:
        CMP     #'l'
        BNE     :+
        JMP     @right
:

        CMP     #'g'
        BNE     :+
        JMP     @guard
:
        CMP     #'G'
        BNE     :+
        JMP     @guard
:
        CMP     #'t'
        BNE     :+
        JMP     @use
:
        CMP     #'T'
        BNE     :+
        JMP     @use
:
        CMP     #'?'
        BNE     :+
        JMP     @help
:
        CMP     #'q'
        BNE     :+
        JMP     @quit
:
        CMP     #'Q'
        BNE     :+
        JMP     @quit
:
        JMP     @loop           ; unknown key

@up:
        LDA     #$FF            ; dy = -1
        STA     dy
        JMP     @domove
@down:
        LDA     #1
        STA     dy
        JMP     @domove
@left:
        LDA     #$FF
        STA     dx
        JMP     @domove
@right:
        LDA     #1
        STA     dx
@domove:
        JSR     try_move
        JMP     @doturn
@guard:
        LDA     #1
        STA     guard_active
        STA     did_move
        PRINTMSG_MSG m_guard
@doturn:
; if a turn was consumed, monsters and status effects act
        LDA     did_move
        BEQ     @after
        JSR     mon_act
        JSR     process_status
@after:
        LDA     #0
        STA     guard_active
        JSR     full_redraw
        JMP     @loop

@use:
        LDA     #0
        STA     did_move
        JSR     use_action
        LDA     did_move
        BEQ     @use_redraw
        JSR     mon_act
        JSR     process_status
@use_redraw:
        JSR     full_redraw
        JMP     @loop

@help:
        JSR     help_menu
        JSR     full_redraw
        JSR     msg_redraw
        JMP     @loop

@quit:
        JSR     confirm_quit
        BEQ     @doquit         ; Z=1 means Yes
        JMP     @loop
@doquit:
        RTS

;==============================================================================
; confirm_quit - ask Y/N. Returns Z=1 if quitting.
;==============================================================================
confirm_quit:
        JSR     msg_clear
        PRINTMSG_MSG q_quit
        JSR     yorn            ; Z=1 if yes
        PHP
        JSR     msg_clear
        PLP
        RTS

;==============================================================================
; End-game screens
;==============================================================================
victory:
        JSR     cls_vram
        LDX     #20
        LDY     #8
        JSR     locate
        LDA     #C_TITLE
        STA     CURCOLOR
        PRINTMSG win1
        LDX     #16
        LDY     #10
        JSR     locate
        LDA     #C_PANEL
        STA     CURCOLOR
        PRINTMSG win2
        LDX     #18
        LDY     #14
        JSR     locate
        PRINTMSG anykey
        JSR     sfx_win
        JSR     getkey_block
        RTS

game_over_hp:
        LDA     #<go_hp
        LDY     #>go_hp
        JMP     game_over_common
game_over_food:
        LDA     #<go_food
        LDY     #>go_food
; fall through
game_over_common:
; stash the cause-string pointer somewhere that survives both
; cls_vram and prmsg. NOT strp (prmsg uses it as scratch) and NOT
; tmp0 (rowbase, called by cls_vram, clobbers it) - parking it in
; either reprinted the "THOU HAST FALLEN" banner a second time
; instead of the cause line. numarg is untouched on this path.
        STA     numarg
        STY     numarg+1
        JSR     cls_vram
        LDX     #22
        LDY     #8
        JSR     locate
        LDA     #COLOR(CO_BRRED, CO_BLACK)
        STA     CURCOLOR
        PRINTMSG over1
        LDX     #16
        LDY     #10
        JSR     locate
        LDA     #C_PANEL
        STA     CURCOLOR
        LDA     numarg
        LDY     numarg+1
        JSR     prmsg
        LDX     #18
        LDY     #14
        JSR     locate
        PRINTMSG anykey
        JSR     sfx_lose
        JSR     getkey_block
        RTS

;==============================================================================
; exit_to_dos
;==============================================================================
exit_to_dos:
        JSR     psg_silence
; restore a normal console: 80-col, default colors, cursor on
        LDA     #FC_SETMODE
        STA     farfunct
        LDA     #1
        JSR     DO_FARCALL
        LDX     #C_BLANK
        LDY     #CSRCOLOR_DEF
        FARCALL FC_COLOR
        FARCALL FC_SCNCLR
        LDA     #1
        STA     SHOWCRSR
        JMP     BOOT            ; back to DOS/65

CSRCOLOR_DEF    = $E1

;==============================================================================
; Title / end strings
;==============================================================================
ttl1:
        .BYTE   "+--------------------------------+",0
ttl2:
        .BYTE   "|       W Y R M H O L D          |",0
ttl3:
        .BYTE   "|     a quest for the 6502PC     |",0
ttl4:
        .BYTE   "+--------------------------------+",0
ttl_by:
        .BYTE   "Answer the summons of Wyrmhold Castle",0
ttl_prompt:
        .BYTE   "Press any key to begin thy quest",0
ttl_keys1:
        .BYTE   "Move: W A S D (or H J K L)   G: guard   T: use/shop",0
ttl_keys2:
        .BYTE   "Bump monsters to fight.  ?: help   Q: quit to DOS",0

intro_msg:
        .BYTE   "Welcome to Wyrmhold. Seek the castle and speak with its ruler.",0
q_quit:
        .BYTE   "Quit to DOS? (Y/N)",0

win1:
        .BYTE   "*** VICTORY! ***",0
win2:
        .BYTE   "King Aldren names thee Champion of Wyrmhold!",0
over1:
        .BYTE   "*** THOU HAST FALLEN ***",0
go_hp:
        .BYTE   "Thy wounds were too grave.",0
go_food:
        .BYTE   "Thou hast starved in the wilds.",0
anykey:
        .BYTE   "Press any key...",0

;==============================================================================
; Include the rest of the engine
;==============================================================================
        .INCLUDE "rng.asm"
        .INCLUDE "sound.asm"
        .INCLUDE "tiles.asm"
        .INCLUDE "title.asm"
        .INCLUDE "world.asm"
        .INCLUDE "video.asm"
        .INCLUDE "ui.asm"
        .INCLUDE "entity.asm"
        .INCLUDE "player.asm"
        .INCLUDE "combat.asm"
        .INCLUDE "town.asm"
        .INCLUDE "castle.asm"
        .INCLUDE "shrine.asm"

;==============================================================================
; Uninitialised game state (RAM).  Placed after all code/data.  The
; map buffers are the largest consumers.  Everything lives well below
; the $A000 video window.
;==============================================================================
gamevars:
; player
px:
        .BYTE   0
py:
        .BYTE   0
phealth:
        .BYTE   0
pmaxhp:
        .BYTE   0
plevel:
        .BYTE   0
pxp:
        .WORD   0
pgold:
        .WORD   0
pfood:
        .WORD   0
pweapon:
        .BYTE   0
parmor:
        .BYTE   0
loc:
        .BYTE   0
town_id:
        .BYTE   0
discovery_flags:
        .BYTE   0
locw:
        .BYTE   0
loch:
        .BYTE   0
owretx:
        .BYTE   0
owrety:
        .BYTE   0
bosskilled:
        .BYTE   0
queststate:
        .BYTE   0
boss_breath_dir:
        .BYTE   0
poison_turns:
        .BYTE   0
turn_phase:
        .BYTE   0
guard_active:
        .BYTE   0
attack_flags:
        .BYTE   0
did_move:
        .BYTE   0

; music player state - three voice cursors (ptr) + hold counters
mvA_ptr:
        .WORD   0
mvA_cnt:
        .BYTE   0
mvB_ptr:
        .WORD   0
mvB_cnt:
        .BYTE   0
mvC_ptr:
        .WORD   0
mvC_cnt:
        .BYTE   0

; shop status-line pointer (last action / greeting)
shopstat:
        .WORD   0

; title-screen logo drawing scratch
logo_li:
        .BYTE   0
logo_lx:
        .BYTE   0
logo_ry:
        .BYTE   0
logo_cx:
        .BYTE   0
logo_bits:
        .BYTE   0

; message log buffers (one screen row each)
msgbuf0:
        .RES    SCRW
msgbuf1:
        .RES    SCRW
; message builder scratch
mbuf:
        .RES    96
mblen:
        .BYTE   0

; monster table (parallel arrays)
mon_type:
        .RES    MAXMON
mon_x:
        .RES    MAXMON
mon_y:
        .RES    MAXMON
mon_hp:
        .RES    MAXMON
mon_state:
        .RES    MAXMON

; overworld monster snapshot while an interior owns mon_*
ow_mon_type:
        .RES    MAXMON
ow_mon_x:
        .RES    MAXMON
ow_mon_y:
        .RES    MAXMON
ow_mon_hp:
        .RES    MAXMON
ow_mon_state:
        .RES    MAXMON

; map tile buffers
owmap:
        .RES    OWW*OWH
locmap:
        .RES    TOWNW*TOWNH     ; all 32x20 interiors share this buffer

        .END
