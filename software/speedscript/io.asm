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

; save filename for reuse by TSAVE
        JSR     savefn

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

;--------------------------------------------------------------
; TSAVE: Save text to disk as a text file.
; Prompts for filename (shows previous name if available).
; Converts retchar to CR/LF. Pads final record with $1A.
; Overwrites existing files (delete + create).
;--------------------------------------------------------------
TSAVE:
        TOPPRINTMESSAGE savmsg  ; "Save:" on command line
        LDA     #1
        STA     SHOWCRSR        ; TURN ON CURSOR
; pre-fill inbuff with stored filename (if any) so user can edit it
        LDX     fnlen
        BEQ     @noprev
        LDY     #0
@prefill:
        LDA     filename,Y
        STA     inbuff,Y        ; copy to inbuff
        JSR     chrout          ; echo to screen
        INY
        DEX
        BNE     @prefill
; enter input loop with Y = fnlen (backspace works on pre-filled text)
        LDA     #39
        STA     limit
        JSR     cursin          ; returns length in A
        BRA     @chkname
@noprev:
        JSR     input           ; no previous filename, start fresh
@chkname:
        STZ     SHOWCRSR        ; TURN OFF CURSOR
        BNE     @gotname
; empty = abort (user backspaced everything or pressed Enter with nothing)
        JSR     sysmsg          ; restore banner
        RTS

@gotname:
        STA     inlen
; save this as the working filename
        JSR     savefn

@doprep:
        JSR     fcb_prep        ; parse inbuff -> fcb

; initialize DOS/65
        LDX     #13
        JSR     PEM

; delete existing file (ignore error - file may not exist)
        LDA     #<fcb
        LDY     #>fcb
        LDX     #19             ; PEM: delete file
        JSR     PEM

; re-prep FCB (delete modifies it)
        JSR     fcb_prep

; create new file
        LDA     #<fcb
        LDY     #>fcb
        LDX     #22             ; PEM: create file
        JSR     PEM
        CMP     #$FF
        BNE     :+
        JMP     @diskerr        ; directory full
:

; clear record number
        LDA     #0
        STA     fcb+32

; open file
        LDA     #<fcb
        LDY     #>fcb
        LDX     #15             ; PEM: open file
        JSR     PEM
        CMP     #$FF
        BNE     :+
        JMP     @diskerr
:

; set DMA buffer to codebuffer
        LDA     #<codebuffer
        LDY     #>codebuffer
        LDX     #26             ; PEM: set DMA address
        JSR     PEM

; source pointer: start of text
        COPY16  texstart,tex
        LDY     #0              ; Y = codebuffer index

; --- main save loop ---
@saveloop:
; check if tex >= lastline
        LDA     tex+1
        CMP     lastline+1
        BCC     @getbyte        ; high byte less = more to go
        BNE     @padeof         ; high byte greater = past end
        LDA     tex
        CMP     lastline
        BCS     @padeof         ; low byte >= = at or past end

@getbyte:
        LDA     (tex)           ; 65C02 indirect (no index)
        CMP     #retchar
        BEQ     @savecrlf

; normal character - strip high bit and store
        AND     #$7F
        STA     codebuffer,Y
        INY
        CPY     #128
        BNE     @nexttex
        JSR     @flush
        BNE     @writerr
        LDY     #0
        BRA     @nexttex

@savecrlf:
; convert retchar to CR + LF
        LDA     #13             ; CR
        STA     codebuffer,Y
        INY
        CPY     #128
        BNE     :+
        JSR     @flush
        BNE     @writerr
        LDY     #0
:
        LDA     #10             ; LF
        STA     codebuffer,Y
        INY
        CPY     #128
        BNE     @nexttex
        JSR     @flush
        BNE     @writerr
        LDY     #0

@nexttex:
; advance source pointer
        INC     tex
        BNE     @saveloop
        INC     tex+1
        BRA     @saveloop

; --- end of text: pad with $1A and write final record ---
@padeof:
        CPY     #0
        BEQ     @closesave      ; nothing to flush
        LDA     #$1A            ; CP/M EOF marker
@padlp:
        STA     codebuffer,Y
        INY
        CPY     #128
        BNE     @padlp
        JSR     @flush
        BNE     @writerr

@closesave:
; close the file
        LDA     #<fcb
        LDY     #>fcb
        LDX     #16             ; PEM: close file
        JSR     PEM

        TOPPRINTMESSAGE okmsg
        LDA     #1
        STA     msgflg
        RTS

@writerr:
@diskerr:
; close file on error (harmless if never opened)
        LDA     #<fcb
        LDY     #>fcb
        LDX     #16
        JSR     PEM
        TOPPRINTMESSAGE ioerrmsg
        LDA     #1
        STA     msgflg
        RTS

; --- write codebuffer to disk ---
; Returns Z=1 (BEQ) on success, Z=0 (BNE) on error.
@flush:
        LDA     #<fcb
        LDY     #>fcb
        LDX     #21             ; PEM: write sequential
        JSR     PEM
        CMP     #0              ; Z flag set if success
        RTS

;--------------------------------------------------------------
; savefn: Copy inbuff/inlen to filename/fnlen for reuse.
;--------------------------------------------------------------
savefn:
        LDX     inlen
        STX     fnlen
        BEQ     @sfnret
        DEX
@sfnlp:
        LDA     inbuff,X
        STA     filename,X
        DEX
        BPL     @sfnlp
@sfnret:
        RTS

;--------------------------------------------------------------
; catalog: Display disk directory in 5-column layout.
; Uses direct video writes so text memory is untouched.
; Paginates at 22 rows. Refresh restores editor on return.
;--------------------------------------------------------------
catalog:
; set up wildcard FCB (match all files on default drive)
        LDX     #32
        LDA     #0
@clrfcb:
        STA     fcb,X
        DEX
        BPL     @clrfcb
        LDX     #1
        LDA     #'?'
@wcfcb:
        STA     fcb,X
        INX
        CPX     #12
        BNE     @wcfcb

; initialize DOS/65
        LDX     #13
        JSR     PEM

; set DMA buffer to codebuffer
        LDA     #<codebuffer
        LDY     #>codebuffer
        LDX     #26             ; PEM: set DMA address
        JSR     PEM

; clear editing area (rows 1-22) via direct video
        JSR     @catclr

; show header
        JSR     topclr
        PRINTMESSAGE @hdrmsg

; initialize display position
        LDA     #1
        STA     scrrow          ; current display row (1-22)
        STZ     csrcol          ; column offset (0,16,32,48,64)
        LDA     #$50            ; video pointer = $A050 (row 1)
        STA     indir
        LDA     #$A0
        STA     indir+1

; find first file
        LDA     #<fcb
        LDY     #>fcb
        LDX     #17             ; PEM: find first match
        JSR     PEM
        CMP     #$FF
        BNE     @catshow
        JMP     @nofiles

@catshow:
; A = dirmod (0-3), compute offset = dirmod * 32
        ASL
        ASL
        ASL
        ASL
        ASL
        TAX                     ; X = base offset in codebuffer
        INX                     ; +1 to skip user number byte

; map video and write filename at current position
        PHX                     ; save codebuffer offset
        LDY     #VIDTEXT_PAGE
        JSR     vid_enter
        PLX                     ; restore offset

; Y = starting column in current video row
        LDY     csrcol

; write 8 filename chars
        LDA     #8
        STA     temp
@cfname:
        LDA     codebuffer,X
        AND     #$7F            ; strip read-only flag
        STA     (indir),Y
        INX
        INY
        DEC     temp
        BNE     @cfname

; write dot separator
        LDA     #'.'
        STA     (indir),Y
        INY

; write 3 extension chars
        LDA     #3
        STA     temp
@cfext:
        LDA     codebuffer,X
        AND     #$7F
        STA     (indir),Y
        INX
        INY
        DEC     temp
        BNE     @cfext

; pad remainder to 16-char column with spaces
        LDA     #space
@cfpad:
        CPY     #80             ; don't write past end of row
        BCS     @cfpaddn
        TYA
        SEC
        SBC     csrcol
        CMP     #16
        BCS     @cfpaddn
        LDA     #space
        STA     (indir),Y
        INY
        BRA     @cfpad
@cfpaddn:

        JSR     vid_exit

; advance to next column
        CLC
        LDA     csrcol
        ADC     #16
        STA     csrcol
        CMP     #80
        BCC     @catnext        ; still on same row

; wrap to next row
        STZ     csrcol
        INC     scrrow
; advance video pointer by 80
        CLC
        LDA     indir
        ADC     #80
        STA     indir
        LDA     indir+1
        ADC     #0
        STA     indir+1
; check if page full (row 23 = past editing area)
        LDA     scrrow
        CMP     #23
        BCC     @catnext

; page full - prompt and reset
        JSR     topclr
        PRINTMESSAGE @moremsg
        JSR     getakey
        JSR     @catclr
; reset display position to row 1
        LDA     #1
        STA     scrrow
        STZ     csrcol
        LDA     #$50
        STA     indir
        LDA     #$A0
        STA     indir+1

@catnext:
; find next file
        LDA     #<fcb
        LDY     #>fcb
        LDX     #18             ; PEM: find next match
        JSR     PEM
        BMI     @catdone
        JMP     @catshow

@nofiles:
        JSR     topclr
        PRINTMESSAGE @nfmsg
        JSR     getakey
        JMP     @catreturn

@catdone:
; all files listed - wait for key
        JSR     topclr
        PRINTMESSAGE @donemsg
        JSR     getakey

@catreturn:
; restore banner (main loop will refresh editing area)
        JSR     sysmsg
        RTS

; --- clear rows 1-22 via direct video writes ---
@catclr:
        PHY
        LDY     #VIDTEXT_PAGE
        JSR     vid_enter
        LDA     #$50
        STA     indir
        LDA     #$A0
        STA     indir+1
        LDX     #22             ; 22 rows to clear
@clrrow:
        LDY     #79
        LDA     #space
@clrcol:
        STA     (indir),Y
        DEY
        BPL     @clrcol
; advance indir by 80
        CLC
        LDA     indir
        ADC     #80
        STA     indir
        LDA     indir+1
        ADC     #0
        STA     indir+1
        DEX
        BNE     @clrrow
        JSR     vid_exit
        PLY
        RTS

; --- catalog messages ---
@hdrmsg:
        .ASCIIZ "Directory"
@moremsg:
        .ASCIIZ "-- more -- (press any key)"
@nfmsg:
        .ASCIIZ "No files found"
@donemsg:
        .ASCIIZ "Press any key to return"

;--------------------------------------------------------------
; print: Print formatted document to list device (printer).
; Word-wraps at 80 columns (same as screen display).
; Converts retchar to CR/LF. Press any key to abort.
;--------------------------------------------------------------
print:
        TOPPRINTMESSAGE @prtmsg
; source pointer: start of text
        COPY16  texstart,tex

@prtpage:
; check if done: tex >= lastline
        LDA     tex+1
        CMP     lastline+1
        BCC     @prtscan        ; high byte less = more to go
        BNE     @prtdone        ; high byte greater = past end
        LDA     tex
        CMP     lastline
        BCS     @prtdone        ; low byte >= = done

@prtscan:
; scan for line break (same word-wrap algorithm as refresh)
        LDY     #0
@scanloop:
        LDA     (tex),Y
        INY
        AND     #$7F
        CMP     #retchar
        BEQ     @gotbreak
        CPY     #80
        BNE     @scanloop
; hit 80 chars without paragraph break - word wrap
; scan backward for a space to break at
        DEY
@wrapback:
        LDA     (tex),Y
        AND     #$7F
        CMP     #space
        BEQ     @wrapfound
        DEY
        BNE     @wrapback
; no space in entire line - force break at column width
        LDY     #79
@wrapfound:
        INY                     ; wrap point (char after space)
@gotbreak:
        STY     temp            ; temp = chars to consume this line

; output the line characters to list device
        LDY     #0
@prtchar:
        CPY     temp
        BEQ     @endline
        LDA     (tex),Y
        AND     #$7F
        CMP     #retchar
        BEQ     @skipchar       ; don't print paragraph marker
        CMP     #space
        BCS     @printable
        LDA     #space          ; replace control chars with space
@printable:
        PHY                     ; save index (PEM trashes regs)
        LDX     #5              ; PEM fn 5: list output
        JSR     PEM
        PLY                     ; restore index
@skipchar:
        INY
        BRA     @prtchar

@endline:
; send CR + LF to list device
        LDA     #13             ; CR
        LDX     #5
        JSR     PEM
        LDA     #10             ; LF
        LDX     #5
        JSR     PEM

; advance tex past consumed characters
        CLC
        LDA     temp
        ADC     tex
        STA     tex
        LDA     tex+1
        ADC     #0
        STA     tex+1

; check for abort (any key pressed?)
        JSR     chrin           ; non-blocking keyboard check
        BNE     @prtabort       ; non-zero = key pressed, abort
        JMP     @prtpage

@prtabort:
        TOPPRINTMESSAGE @abrtmsg
        LDA     #1
        STA     msgflg
        RTS

@prtdone:
; send form feed to eject page
        LDA     #12             ; FF (form feed)
        LDX     #5              ; PEM fn 5: list output
        JSR     PEM
        TOPPRINTMESSAGE okmsg
        LDA     #1
        STA     msgflg
        RTS

@prtmsg:
        .ASCIIZ "Printing..."
@abrtmsg:
        .ASCIIZ "Aborted"
