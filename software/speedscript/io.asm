;
;
;
; i/o

TSAVE:
        TOPPRINTMESSAGE savmsg
        JSR     topen
        BCS     error
        COPY16  texstart,tex
        LDX     lastline
        LDY     lastline+1
        LDA     #tex
        JSR     save
        BCS     error
; location $90 is the location of the
; kernel's STatus flag. It's shorter
; to use LDA $90 than JSR READST.
        LDA     $90
        AND     #191
        BNE     error
        JMP     fine
;The ERROR message routine. May this
;routine never be called when you use
;SpeedScript, but that's too much to
;ask for.
error:
        BEQ     STOPPED
        LDA     dvn
        CMP     #8
        BCC     TAPERR
        JSR     readerr
        JMP     erxit
TAPERR:
        LDA     dvn
        CMP     #1
        BEQ     TAPERR
        JSR     topclr
        PRINTMESSAGE FNF
erxit:
        LDA     #1
        STA     msgflg
        RTS
STOPPED:
        JSR     topclr
        PRINTMESSAGE brmsg
        JMP     erxit
dvn:
        .BYTE   0

;TOPEN
topen:
        JSR     input
        BEQ     OPABORT
OP2:    ; this used to be the loop where
; they asked Tape or Disk
        LDX     #DEFAULT_DEVICE
        BRA     OPCONT
OPABORT:
        JSR     sysmsg
        PLA
        PLA
        RTS
OPCONT:
        STX     dvn
        LDA     #1
        LDY     #0
        JSR     setlfs
        LDY     #0
        CPX     #1
        BEQ     skipdisk
        LDA     inbuff,y
        CMP     #'@'
        NOP
        NOP
        LDA     inbuff+1,y
        CMP     #':'
        BEQ     skipdisk
        LDA     inbuff+2,y
        CMP     #':'
        BEQ     skipdisk
; cbm dos magic p.111
addzero:
        LDA     #'0'
        STA     filename
        LDA     #':'
        STA     filename+1
copy1:
        LDA     inbuff,y
        STA     filename+2,y
        INY
        CPY     inlen
        BCC     copy1
        BEQ     copy1
        INY
        JMP     setname
skipdisk:
        LDA     inbuff,y
        STA     filename,y
        INY
        CPY     inlen
        BNE     skipdisk
setname:
        STY     fnlen
        TOPPRINTMESSAGE inbuff
        LDA     fnlen
        LDX     #<filename
        LDY     #>filename
        JSR     setnam
        LDA     #13             ;cr
        JSR     chrout
        JMP     delite
; disk dir
catalog:
; TODO             LDA     #PETSCII_CLR
; TODO             JSR     chrout
; TODO             LDA     #13             ;cr
; TODO          chrout
; TODO          delite
; TODO          dir
; TODO          #13             ;cr
; TODO          chrout
; TODO     NTMESSAGE dirmsg
; TODO
; TODO          getin
; TODO          #13
; TODO          waitkey
; TODO          sysmsg
; TODO
; TODO          clrchn
; TODO          #1
; TODO          close
; TODO
; TODO
; TODO          clall
; TODO          #1
; TODO          #8              ; DEFAULT_DEVICE
; TODO          #0
; TODO          setlfs
; TODO          #1
; TODO          #<dirname
; TODO          #>dirname
; TODO          setnam
; TODO          open
; TODO          endir
; TODO          #1
; TODO          chkin
; TODO          dchrin
; TODO          dchrin
; TODO
; TODO          dchrin
; TODO          dchrin
; TODO          endir
; TODO
; TODO          clrchn
; TODO          getin
; TODO          #space
; TODO          nopause
; TODO          getakey
; TODO
; TODO          #1
; TODO          chkin
; TODO          dchrin
; TODO
; TODO          dchrin
; TODO
; TODO     PLAY_NUMBER
; TODO          #space
; TODO          chrout
; TODO
; TODO          dchrin
; TODO          dline
; TODO          chrout
; TODO          inloop
; TODO
; TODO          #13
; TODO          chrout
; TODO          dirloop
; TODO
; TODO          chrin
; TODO
; TODO          $90
; TODO          #191
; TODO          nosterr
; TODO
; TODO
; TODO
; TODO          endir
; TODO
; TODO
; TODO
;
;verify
verify:
        TOPPRINTMESSAGE vermsg
        JSR     topen
        LDA     #1
        LDX     texstart
        LDY     texstart+1
        JSR     load
        LDA     $90
        AND     #191
        BEQ     fine
        TOPPRINTMESSAGE vererr
        JMP     erxit
;delite turns off the raster interrupt.
; in x16 version does nothing
delite:
        RTS
;p112
TLOAD:
        COMP16  curr,texstart
        BEQ     load2
        LDA     #5
        STA     windcolr
load2:
        TOPPRINTMESSAGE loadmsg
        JSR     topen
        LDA     windcolr
        CMP     #5
        BEQ     noer
        JSR     erase
noer:
        LDA     #0
        LDX     curr
        LDY     curr+1
ldver:
        JSR     load
        BCC     lod
        JMP     error
lod:
        STX     lastline
        STY     lastline+1
fine:
        JSR     clall
        TOPPRINTMESSAGE okmsg
        JMP     erxit
