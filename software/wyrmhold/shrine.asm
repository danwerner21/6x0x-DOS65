;______________________________________________________________________________
;  shrine.asm - Sunken Shrine transition and Wyrm Key reward
;______________________________________________________________________________

;----------------------------------------------------------------
; enter_shrine - enter the southern-marsh quest location.
;----------------------------------------------------------------
enter_shrine:
        JSR     sfx_door
        JSR     decode_shrine
        LDA     #LOC_SHRINE
        STA     loc
        LDA     #SHRINEW
        STA     locw
        LDA     #SHRINEH
        STA     loch
        LDA     px
        STA     owretx
        LDA     py
        STA     owrety
; begin just east of the stairs-up tile
        LDA     #2
        STA     px
        LDA     #1
        STA     py
        JSR     spawn_shrine_monsters
        LDA     #1
        STA     did_move
        LDA     queststate
        CMP     #QUEST_FIND_KEY
        BNE     @quiet
        PRINTMSG_MSG m_shrine1
        PRINTMSG_MSG m_shrine2
        RTS
@quiet:
        PRINTMSG_MSG m_shrine_empty
        RTS

;----------------------------------------------------------------
; award_wyrm_key - called when the Wyrm Warden dies.
;----------------------------------------------------------------
award_wyrm_key:
        LDA     queststate
        CMP     #QUEST_FIND_KEY
        BNE     @done
        LDA     #QUEST_HAVE_KEY
        STA     queststate
        JSR     sfx_treasure
        PRINTMSG_MSG m_key1
        PRINTMSG_MSG m_key2
@done:
        RTS

;----------------------------------------------------------------
; Shrine and sealed-lair messages
;----------------------------------------------------------------
m_shrine_dormant:
        .BYTE   "Ancient runes bar this sunken place. Seek counsel at Wyrmhold Castle.",0
m_dung_sealed:
        .BYTE   "A dragon-marked ward seals the lair. The Wyrm Key is required.",0
m_shrine1:
        .BYTE   "The Sunken Shrine. Black water surrounds a silent guardian.",0
m_shrine2:
        .BYTE   "The Wyrm Warden surges forward with unnatural speed!",0
m_shrine_empty:
        .BYTE   "The Sunken Shrine lies quiet. Its ancient guardian is gone.",0
m_key1:
        .BYTE   "The Wyrm Warden falls. The lost Wyrm Key is thine!",0
m_key2:
        .BYTE   "Return the Wyrm Key to King Aldren at Wyrmhold Castle.",0
