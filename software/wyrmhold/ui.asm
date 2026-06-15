;______________________________________________________________________________
;  ui.asm - message log, stat panel, prompts, decimal output
;______________________________________________________________________________

;----------------------------------------------------------------
; displaynum - print the 16-bit value in numarg as decimal using
;              the firmware (honours CURCOLOR).  No leading spaces.
;              (adapted from speedscript displaynum)
;----------------------------------------------------------------
displaynum:
        LDY     #8              ; index into nums (5 entries x2 bytes)
@find:
        LDA     numarg+1
        CMP     nums+1,Y
        BCC     @next
        BNE     @start
        LDA     numarg
        CMP     nums,Y
        BCS     @start
@next:
        DEY
        DEY
        BNE     @find
@start:
        LDA     #'0'
        STA     numspace
@dig:
        LDA     numarg+1
        CMP     nums+1,Y
        BCC     @out
        BNE     @nocmp
        LDA     numarg
        SBC     nums,Y          ; carry set
        BCC     @out
        BCS     @sub
@nocmp:
        LDA     numarg
        SBC     nums,Y          ; carry set
@sub:
        INC     numspace
        STA     numarg
        LDA     numarg+1
        SBC     nums+1,Y
        STA     numarg+1
        JMP     @dig
@out:
        LDA     numspace
        JSR     putc
        DEY
        DEY
        BPL     @start
        RTS

; place value table (1,10,100,1000,10000) as 16-bit LE words
nums:
        .WORD   1
        .WORD   10
        .WORD   100
        .WORD   1000
        .WORD   10000

;----------------------------------------------------------------
; print a single 8-bit value (in A) as decimal.
;----------------------------------------------------------------
print_byte:
        STA     numarg
        LDA     #0
        STA     numarg+1
        JMP     displaynum

;----------------------------------------------------------------
; Message log: two lines at rows MSGY0 and MSGY0+1.
; msg_print scrolls the bottom line up and prints a new line.
;
;   msg_print:  IN A=lo,Y=hi of 0-terminated string
;----------------------------------------------------------------
msg_print:
        STA     strp
        STY     strp+1
; copy current bottom line (msgbuf1) up to (msgbuf0)
        LDX     #0
@up:
        LDA     msgbuf1,X
        STA     msgbuf0,X
        INX
        CPX     #SCRW
        BNE     @up
; copy new string into msgbuf1 (truncate/pad to SCRW)
        LDY     #0
@cp:
        LDA     (strp),Y
        BEQ     @padb
        STA     msgbuf1,Y
        INY
        CPY     #SCRW
        BNE     @cp
        JMP     @draw
@padb:
        LDA     #space
@pad:
        STA     msgbuf1,Y
        INY
        CPY     #SCRW
        BNE     @pad
@draw:
        JMP     msg_redraw

;----------------------------------------------------------------
; msg_redraw - blit both message buffers to the message rows via
;              direct VRAM (so it does not disturb the text cursor).
;----------------------------------------------------------------
msg_redraw:
        JSR     vid_enter
; row MSGY0 <- msgbuf0
        LDA     #MSGY0
        JSR     rowbase
        LDY     #0
@r0:
        LDA     msgbuf0,Y
        STA     (vptr),Y
        LDA     #C_MSG
        STA     (cptr),Y
        INY
        CPY     #SCRW
        BNE     @r0
; row MSGY0+1 <- msgbuf1
        LDA     #MSGY0+1
        JSR     rowbase
        LDY     #0
@r1:
        LDA     msgbuf1,Y
        STA     (vptr),Y
        LDA     #C_MSG
        STA     (cptr),Y
        INY
        CPY     #SCRW
        BNE     @r1
        JMP     vid_exit

;----------------------------------------------------------------
; msg_clear - blank both message buffers and redraw.
;----------------------------------------------------------------
msg_clear:
        LDX     #0
        LDA     #space
@mc:
        STA     msgbuf0,X
        STA     msgbuf1,X
        INX
        CPX     #SCRW
        BNE     @mc
        JMP     msg_redraw

;----------------------------------------------------------------
; Stat panel.  Drawn on the right side using firmware text output.
; Labels are static; values are redrawn each turn.
;----------------------------------------------------------------
draw_panel:
; HP line:  "HP   : nn/nn"
        LDX     #PANX
        LDY     #3
        JSR     locate
        LDA     #C_PANEL
        STA     CURCOLOR
        PRINTMSG lbl_hp
        LDA     phealth
        JSR     print_byte
        LDA     #'/'
        JSR     putc
        LDA     pmaxhp
        JSR     print_byte
        JSR     pad_clear       ; clear trailing junk

; Level / XP
        LDX     #PANX
        LDY     #5
        JSR     locate
        PRINTMSG lbl_lvl
        LDA     plevel
        JSR     print_byte
        LDX     #PANX+12
        LDY     #5
        JSR     locate
        PRINTMSG lbl_xp
        COPY16  pxp, numarg
        JSR     displaynum
        JSR     pad_clear

; Gold / Food
        LDX     #PANX
        LDY     #7
        JSR     locate
        PRINTMSG lbl_gold
        COPY16  pgold, numarg
        JSR     displaynum
        LDX     #PANX+12
        LDY     #7
        JSR     locate
        PRINTMSG lbl_food
        COPY16  pfood, numarg
        JSR     displaynum
        JSR     pad_clear

; Weapon
        LDX     #PANX
        LDY     #9
        JSR     locate
        PRINTMSG lbl_wpn
        LDA     pweapon
        JSR     name_weapon     ; strp -> name
        LDA     strp
        LDY     strp+1
        JSR     prmsg
        JSR     pad_clear

; Armor
        LDX     #PANX
        LDY     #10
        JSR     locate
        PRINTMSG lbl_arm
        LDA     parmor
        JSR     name_armor
        LDA     strp
        LDY     strp+1
        JSR     prmsg
        JSR     pad_clear

; Location
        LDX     #PANX
        LDY     #12
        JSR     locate
        PRINTMSG lbl_loc
        LDA     loc
        JSR     name_loc
        LDA     strp
        LDY     strp+1
        JSR     prmsg
        JSR     pad_clear

; Current objective
        LDX     #PANX
        LDY     #14
        JSR     locate
        LDA     #C_PANELHDR
        STA     CURCOLOR
        PRINTMSG lbl_obj
        JSR     pad_clear
        LDX     #PANX
        LDY     #15
        JSR     locate
        LDA     #C_PANEL
        STA     CURCOLOR
        LDA     queststate
        JSR     name_objective
        LDA     strp
        LDY     strp+1
        JSR     prmsg
        JSR     pad_clear
        RTS

; pad_clear - print spaces out to the right screen edge to erase
;             any leftover characters from a previous longer value.
;             Stops at the last column to avoid auto-scroll.
pad_clear:
@pc:
        LDA     CURX
        CMP     #SCRW-1
        BCS     @done
        LDA     #space
        JSR     putc
        JMP     @pc
@done:
        RTS

;----------------------------------------------------------------
; name_weapon / name_armor / name_loc - set strp to a name string
;   IN: A = index
;----------------------------------------------------------------
name_weapon:
        ASL     A
        TAX
        LDA     wpn_names,X
        STA     strp
        LDA     wpn_names+1,X
        STA     strp+1
        RTS
name_armor:
        ASL     A
        TAX
        LDA     arm_names,X
        STA     strp
        LDA     arm_names+1,X
        STA     strp+1
        RTS
name_loc:
        ASL     A
        TAX
        LDA     loc_names,X
        STA     strp
        LDA     loc_names+1,X
        STA     strp+1
        RTS
name_objective:
        ASL     A
        TAX
        LDA     objective_names,X
        STA     strp
        LDA     objective_names+1,X
        STA     strp+1
        RTS

;----------------------------------------------------------------
; yorn - wait for a Y/N keypress.  Returns Z=1 if Yes (A='Y').
;----------------------------------------------------------------
yorn:
@wait:
        JSR     getkey_block
        CMP     #'Y'
        BEQ     @yes
        CMP     #'y'
        BEQ     @yes
        CMP     #'N'
        BEQ     @no
        CMP     #'n'
        BEQ     @no
        JMP     @wait
@yes:
        LDA     #0              ; Z=1
        RTS
@no:
        LDA     #1              ; Z=0
        RTS

;----------------------------------------------------------------
; getkey_block - block until a key is pressed; return it in A.
;----------------------------------------------------------------
getkey_block:
@gk:
        FARCALL FC_CHRIN
        CMP     #$FF
        BEQ     @gk
        RTS

;----------------------------------------------------------------
; getkey - non-blocking read; A=key or 0 if none.
;----------------------------------------------------------------
getkey:
        FARCALL FC_CHRIN
        CMP     #$FF
        BNE     @got
        LDA     #0
@got:
        RTS

;----------------------------------------------------------------
; drainkeys - consume and discard all buffered keystrokes.
;----------------------------------------------------------------
drainkeys:
@dk:
        FARCALL FC_CHRIN
        CMP     #$FF
        BNE     @dk             ; got a key - keep draining
        RTS

;----------------------------------------------------------------
; Panel labels and name tables
;----------------------------------------------------------------
lbl_hp:
        .BYTE   "HP   : ",0
lbl_lvl:
        .BYTE   "Lvl: ",0
lbl_xp:
        .BYTE   "XP: ",0
lbl_gold:
        .BYTE   "Gold: ",0
lbl_food:
        .BYTE   "Food: ",0
lbl_wpn:
        .BYTE   "Weapon: ",0
lbl_arm:
        .BYTE   "Armor : ",0
lbl_loc:
        .BYTE   "Where : ",0
lbl_obj:
        .BYTE   "OBJECTIVE",0

wpn_names:
        .WORD   wn0, wn1, wn2, wn3
wn0:
        .BYTE   "Fists",0
wn1:
        .BYTE   "Dagger",0
wn2:
        .BYTE   "Sword",0
wn3:
        .BYTE   "Axe",0

arm_names:
        .WORD   an0, an1, an2, an3
an0:
        .BYTE   "Clothes",0
an1:
        .BYTE   "Leather",0
an2:
        .BYTE   "Chain",0
an3:
        .BYTE   "Plate",0

loc_names:
        .WORD   ln0, ln1, ln2, ln3, ln4
ln0:
        .BYTE   "Wyrmhold",0
ln1:
        .BYTE   "Town",0
ln2:
        .BYTE   "Dungeon",0
ln3:
        .BYTE   "Wyrmhold Castle",0
ln4:
        .BYTE   "Sunken Shrine",0

objective_names:
        .WORD   obj0, obj1, obj2, obj3
obj0:
        .BYTE   "Seek Wyrmhold Castle",0
obj1:
        .BYTE   "Find the Wyrm Key",0
obj2:
        .BYTE   "Return to the ruler",0
obj3:
        .BYTE   "Enter the dragon's lair",0
