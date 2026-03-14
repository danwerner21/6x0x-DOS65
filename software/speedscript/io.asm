;
;
;
; i/o
;--------------------------------------------------------------
; TLOAD: Load a text file from disk into the editor.
; Prompts for filename, opens via DOS/65 PEM, reads sequential
; 128-byte records into text memory, stops at CP/M EOF ($1A).
;--------------------------------------------------------------
TLOAD:
        TOPPRINTMESSAGE loadmsg ; "Load:" on command line
        LDA     #1
        STA     SHOWCRSR        ; TURN ON CURSOR
        JSR     input           ; get filename in inbuff, length in A
        BEQ     @tabort         ; empty = abort
        STA     inlen
        JSR     fcb_prep        ; parse inbuff → fcb

; initialize DOS/65 file system
        LDX     #13
        JSR     PEM

        STZ     SHOWCRSR        ; TURN OFF CURSOR
; open the file (before erasing text!)
        LDA     #0
        STA     fcb+32          ; next record = 0
        LDA     #<fcb
        LDY     #>fcb
        LDX     #15             ; PEM: open file
        JSR     PEM
        CMP     #$FF
        BNE     :+
        JMP     @notfound       ; file not found
:
; file opened successfully - now erase existing text
        JSR     erase           ; resets curr/toplin/lastline to texstart

; set DMA buffer to codebuffer (128 bytes)
        LDA     #<codebuffer
        LDY     #>codebuffer
        LDX     #26             ; PEM: set DMA address
        JSR     PEM

; destination pointer: load at curr (= texstart after erase)
        COPY16  curr,tex

@readloop:
        LDA     #<fcb
        LDY     #>fcb
        LDX     #20             ; PEM: read sequential
        JSR     PEM
        LDY     #0              ; default: no bytes to copy
        CMP     #$00
        BEQ     @gotdata        ; success - 128 bytes in codebuffer
        CMP     #$01
        BEQ     @closefile      ; EOF - done reading
        BRA     @ioerror        ; any other value = error

@tabort:
        JSR     sysmsg          ; restore banner
        RTS

@gotdata:
@copybyte:
        LDA     codebuffer,Y
        CMP     #$1A            ; CP/M EOF marker?
        BEQ     @closefile
        CMP     #10
        BNE     :++
        LDA     tex
        BNE     :+
        DEC     tex+1
:
        DEC     tex
        JMP     @skipbyte
:
        CMP     #13
        bne     :+
        LDA     #retchar
:
        STA     (tex),Y         ; store in text memory
@skipbyte:
        INY
        CPY     #128
        BNE     @copybyte

; advance destination by 128 bytes
        CLC
        LDA     tex
        ADC     #128
        STA     tex
        LDA     tex+1
        ADC     #0
        STA     tex+1

; check for text area overflow
        LDA     tex+1
        CMP     texend+1
        BCC     @readloop       ; still room
; fall through to close (text area full)

@closefile:
; lastline = tex + Y (end of loaded data)
        TYA
        CLC
        ADC     tex
        STA     lastline
        LDA     tex+1
        ADC     #0
        STA     lastline+1

; close the file
        LDA     #<fcb
        LDY     #>fcb
        LDX     #16             ; PEM: close file
        JSR     PEM

        TOPPRINTMESSAGE okmsg
        LDA     #1
        STA     msgflg
        RTS                     ; returns to main (via stacked return addr)

@notfound:
        TOPPRINTMESSAGE fnfmsg
        LDA     #1
        STA     msgflg
        RTS

@ioerror:
; close file on error
        LDA     #<fcb
        LDY     #>fcb
        LDX     #16
        JSR     PEM
        TOPPRINTMESSAGE ioerrmsg
        LDA     #1
        STA     msgflg
        RTS



;--------------------------------------------------------------
; fcb_prep: Parse filename from inbuff into fcb (33-byte FCB).
; Handles optional "X:" drive prefix, 8.3 format, uppercase.
; Input: inbuff (null-terminated filename)
; Output: fcb filled with drive, name, extension, zeros
;--------------------------------------------------------------
fcb_prep:
; clear entire fcb to zeros
        LDX     #32
        LDA     #0
@clr:
        STA     fcb,X
        DEX
        BPL     @clr

; fill name (bytes 1-8) and extension (bytes 9-11) with spaces
        LDX     #1
        LDA     #space
@spc:
        STA     fcb,X
        INX
        CPX     #12
        BNE     @spc

; check for drive prefix (e.g., "A:")
        LDY     #0              ; Y = index into inbuff
        LDA     inbuff+1
        CMP     #':'
        BNE     @nodrive
; extract drive letter
        LDA     inbuff
        JSR     @toupper
        SEC
        SBC     #'A'-1          ; A=1, B=2, etc.
        STA     fcb             ; fcb+0 = drive number
        LDY     #2              ; skip past "X:"
@nodrive:
; copy filename (up to 8 chars, until '.' or null)
        LDX     #1              ; X = FCB position (1-8)
@fname:
        LDA     inbuff,Y
        BEQ     @done           ; null terminator = end
        CMP     #'.'
        BEQ     @doext          ; dot = start extension
        JSR     @toupper
        STA     fcb,X
        INY
        INX
        CPX     #9              ; max 8 chars for name
        BNE     @fname
; if name > 8 chars, skip to dot or end
@skip:
        LDA     inbuff,Y
        BEQ     @done
        CMP     #'.'
        BEQ     @doext
        INY
        BRA     @skip

@doext:
        INY                     ; skip the '.'
        LDX     #9              ; X = FCB position (9-11)
@ext:
        LDA     inbuff,Y
        BEQ     @done           ; null terminator
        JSR     @toupper
        STA     fcb,X
        INY
        INX
        CPX     #12             ; max 3 chars for extension
        BNE     @ext
@done:
        RTS

@toupper:
        CMP     #'a'
        BCC     @noup
        CMP     #'z'+1
        BCS     @noup
        AND     #$DF            ; clear bit 5 → uppercase
@noup:
        RTS

TSAVE:
catalog:
print:
        RTS
