;______________________________________________________________________________
;  castle.asm - Wyrmhold Castle interaction and opening commission
;______________________________________________________________________________

;----------------------------------------------------------------
; castle_use - contextual USE action inside the audience chamber.
;----------------------------------------------------------------
castle_use:
        JSR     ruler_adjacent
        BCS     @ruler
        PRINTMSG_MSG c_approach
        RTS
@ruler:
        LDA     queststate
        CMP     #QUEST_NONE
        BEQ     @commission
        CMP     #QUEST_FIND_KEY
        BEQ     @reminder
        CMP     #QUEST_HAVE_KEY
        BEQ     @unlock
; QUEST_DUNG_OPEN and any later state
        PRINTMSG_MSG c_open
        RTS
@commission:
        LDA     #QUEST_FIND_KEY
        STA     queststate
        JSR     sfx_levelup
        PRINTMSG_MSG c_commission1
        PRINTMSG_MSG c_commission2
        RTS
@reminder:
        PRINTMSG_MSG c_reminder1
        PRINTMSG_MSG c_reminder2
        RTS
@unlock:
        LDA     #QUEST_DUNG_OPEN
        STA     queststate
; royal reward: full healing and fresh provisions
        LDA     pmaxhp
        STA     phealth
        CLC
        LDA     pfood
        ADC     #100
        STA     pfood
        LDA     pfood+1
        ADC     #0
        STA     pfood+1
        JSR     sfx_door
        PRINTMSG_MSG c_unlock1
        PRINTMSG_MSG c_unlock2
        RTS

;----------------------------------------------------------------
; ruler_adjacent - C=1 if player is orthogonally adjacent to the
; ruler's throne. The throne itself is a wall and cannot be entered.
;----------------------------------------------------------------
ruler_adjacent:
        LDA     px
        CMP     #CASTLE_RULER_X
        BNE     @side
        LDA     py
        CMP     #CASTLE_RULER_Y-1
        BEQ     @yes
        CMP     #CASTLE_RULER_Y+1
        BEQ     @yes
        CLC
        RTS
@side:
        LDA     py
        CMP     #CASTLE_RULER_Y
        BNE     @no
        LDA     px
        CMP     #CASTLE_RULER_X-1
        BEQ     @yes
        CMP     #CASTLE_RULER_X+1
        BEQ     @yes
@no:
        CLC
        RTS
@yes:
        SEC
        RTS

;----------------------------------------------------------------
; Audience chamber messages
;----------------------------------------------------------------
m_castle:
        .BYTE   "Wyrmhold Castle. Press T beside the ruler's throne.",0
c_approach:
        .BYTE   "The ruler awaits upon the northern throne.",0
c_commission1:
        .BYTE   "King Aldren: A wyrm stirs beneath the eastern peaks.",0
c_commission2:
        .BYTE   "Recover the Wyrm Key from the Sunken Shrine in the southern marshes.",0
c_reminder1:
        .BYTE   "King Aldren: Seek the Sunken Shrine in the southern marshes.",0
c_reminder2:
        .BYTE   "Search the realm, then return to me with the key.",0
c_unlock1:
        .BYTE   "King Aldren raises the Wyrm Key. The ancient ward breaks!",0
c_unlock2:
        .BYTE   "Thou art healed and provisioned. Enter the dragon's lair!",0
c_open:
        .BYTE   "King Aldren: The way is open. Wyrmhold stands with thee.",0
