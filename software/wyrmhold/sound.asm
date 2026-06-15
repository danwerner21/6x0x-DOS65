;______________________________________________________________________________
;  sound.asm - AY-3-8910 PSG sound effects and cooperative music player
;
;  PSG access ported from software/dbasic/ay38910.asm (PPI at $E230).
;  psgwr: A=register, Y=value        psgrd: A=register -> Y=value
;
;  AY register map:
;    0/1  ch A tone period (lo / hi 4 bits)
;    2/3  ch B tone period
;    4/5  ch C tone period
;    6    noise period (5 bits)
;    7    mixer: bit clear = enabled. 0-2 tone A/B/C, 3-5 noise A/B/C
;    8/9/10 ch A/B/C volume (0-15, bit4 = use envelope)
;    11/12 envelope period
;    13   envelope shape
;______________________________________________________________________________

PPI_BASE        = $E230
ppiPortA        = PPI_BASE+0
ppiPortB        = PPI_BASE+1
ppiPortC        = PPI_BASE+2
ppiControl      = PPI_BASE+3

;----------------------------------------------------------------
; psg_init - put the PSG into a known, silent state
;----------------------------------------------------------------
psg_init:
        LDA     #%10000000
        STA     ppiControl
        LDA     #%00010000
        STA     ppiPortC
        LDA     #$00
        STA     ppiPortB
        JSR     clrpsg
        RTS

;----------------------------------------------------------------
; clrpsg - zero all 16 registers, then disable all channels
;----------------------------------------------------------------
clrpsg:
        LDX     #00
        LDY     #00
@cp1:
        TXA
        JSR     psgwr
        INX
        CPX     #16
        BNE     @cp1
        LDA     #07
        LDY     #%00111111      ; all tone + noise disabled
        JSR     psgwr
        RTS

;----------------------------------------------------------------
; psg_silence - silence all three channels (volumes -> 0)
;----------------------------------------------------------------
psg_silence:
        LDA     #8
        LDY     #0
        JSR     psgwr
        LDA     #9
        LDY     #0
        JSR     psgwr
        LDA     #10
        LDY     #0
        JSR     psgwr
        LDA     #07
        LDY     #%00111111
        JSR     psgwr
        RTS

;----------------------------------------------------------------
; psgwr - write Y into PSG register A     (trashes A)
;----------------------------------------------------------------
psgwr:
        STA     ppiPortB        ; select register
        LDA     #%00011100      ; latch address
        STA     ppiPortC
        STA     ppiPortC
        STA     ppiPortC
        LDA     #%00010000      ; inactive
        STA     ppiPortC
        STA     ppiPortC
        STA     ppiPortC
        TYA
        STA     ppiPortB        ; store data
        STA     ppiPortB
        STA     ppiPortB
        LDA     #%00010100      ; latch data
        STA     ppiPortC
        STA     ppiPortC
        STA     ppiPortC
        LDA     #%00010000      ; inactive
        STA     ppiPortC
        RTS

;----------------------------------------------------------------
; psgrd - read PSG register A into Y      (trashes A)
;----------------------------------------------------------------
psgrd:
        STA     ppiPortB        ; select register
        LDA     #%00011100      ; latch address
        STA     ppiPortC
        STA     ppiPortC
        STA     ppiPortC
        LDA     #%00010000      ; inactive
        STA     ppiPortC
        STA     ppiPortC
        LDA     #%10000010
        STA     ppiControl
        LDA     #%00011000      ; latch data
        STA     ppiPortC
        STA     ppiPortC
        STA     ppiPortC
        LDA     ppiPortB        ; read data
        TAY
        LDA     #%10000000
        STA     ppiControl
        LDA     #%00010000      ; inactive
        STA     ppiPortC
        RTS

;----------------------------------------------------------------
; tone_on - start a tone on channel A.
;   ptr (16-bit) = 12-bit tone period (lo in ptr, hi in ptr+1)
;   tmp0         = volume (0-15)
;----------------------------------------------------------------
tone_on:
        LDA     #0              ; ch A period low
        LDY     ptr
        JSR     psgwr
        LDA     #1              ; ch A period high
        LDY     ptr+1
        JSR     psgwr
        LDA     #07             ; mixer: enable tone A only
        LDY     #%00111110
        JSR     psgwr
        LDA     #8              ; ch A volume
        LDY     tmp0
        JSR     psgwr
        RTS

;----------------------------------------------------------------
; busywait - crude delay.  X = outer count.
;----------------------------------------------------------------
busywait:
@bw0:
        LDY     #0
@bw1:
        LDA     #0
@bw2:
        CLC
        ADC     #1
        CMP     #40
        BNE     @bw2
        INY
        BNE     @bw1
        DEX
        BNE     @bw0
        RTS

;----------------------------------------------------------------
; mus_finewait - a short, fine-grained delay for tempo trimming.
; Spins MUS_FINEDELAY outer times, 256 inner each.  Much smaller
; than one busywait pass, so it gives sub-step tempo control.
;----------------------------------------------------------------
mus_finewait:
        LDX     #MUS_FINEDELAY
        BEQ     @done
@fw0:
        LDY     #0
@fw1:
        INY
        BNE     @fw1
        DEX
        BNE     @fw0
@done:
        RTS

;----------------------------------------------------------------
; SFX helpers.  Each plays a short, self-contained sound and
; silences the PSG afterwards.  All preserve nothing - callers
; must save anything they need.
;----------------------------------------------------------------

; sfx_move - very short low blip
sfx_move:
        SETW16  ptr, $0240
        LDA     #6
        STA     tmp0
        JSR     tone_on
        LDX     #2
        JSR     busywait
        JMP     psg_silence

; sfx_blocked - low buzz (use noise)
sfx_blocked:
        LDA     #6              ; noise period
        LDY     #20
        JSR     psgwr
        LDA     #07             ; enable noise A
        LDY     #%00110111
        JSR     psgwr
        LDA     #8
        LDY     #10
        JSR     psgwr
        LDX     #3
        JSR     busywait
        JMP     psg_silence

; sfx_hit - quick high zap
sfx_hit:
        SETW16  ptr, $00C0
        LDA     #12
        STA     tmp0
        JSR     tone_on
        LDX     #2
        JSR     busywait
        SETW16  ptr, $0140
        LDA     #10
        STA     tmp0
        JSR     tone_on
        LDX     #2
        JSR     busywait
        JMP     psg_silence

; sfx_breath_warn - rising intake before the dragon breathes
sfx_breath_warn:
        SETW16  ptr, $0320
        LDA     #9
        STA     tmp0
        JSR     tone_on
        LDX     #2
        JSR     busywait
        SETW16  ptr, $0220
        LDA     #12
        STA     tmp0
        JSR     tone_on
        LDX     #3
        JSR     busywait
        JMP     psg_silence

; sfx_breath - forceful noise burst for dragon fire
sfx_breath:
        LDA     #6
        LDY     #3
        JSR     psgwr
        LDA     #7
        LDY     #%00110111
        JSR     psgwr
        LDA     #8
        LDY     #15
        JSR     psgwr
        LDX     #5
        JSR     busywait
        LDA     #6
        LDY     #10
        JSR     psgwr
        LDA     #8
        LDY     #11
        JSR     psgwr
        LDX     #4
        JSR     busywait
        JMP     psg_silence

; sfx_killed - falling tone + noise
sfx_killed:
        LDA     #6
        LDY     #16
        JSR     psgwr
        LDA     #07
        LDY     #%00110111
        JSR     psgwr
        LDA     #8
        LDY     #13
        JSR     psgwr
        LDX     #5
        JSR     busywait
        JMP     psg_silence

; sfx_treasure - bright rising arpeggio
sfx_treasure:
        SETW16  ptr, $0150
        LDA     #13
        STA     tmp0
        JSR     tone_on
        LDX     #2
        JSR     busywait
        SETW16  ptr, $00FC
        JSR     tone_on
        LDX     #2
        JSR     busywait
        SETW16  ptr, $00A8
        JSR     tone_on
        LDX     #3
        JSR     busywait
        JMP     psg_silence

; sfx_levelup - three ascending happy notes
sfx_levelup:
        SETW16  ptr, $00FC
        LDA     #14
        STA     tmp0
        JSR     tone_on
        LDX     #3
        JSR     busywait
        SETW16  ptr, $00C8
        JSR     tone_on
        LDX     #3
        JSR     busywait
        SETW16  ptr, $00A8
        JSR     tone_on
        LDX     #4
        JSR     busywait
        JMP     psg_silence

; sfx_door - short two-tone chime
sfx_door:
        SETW16  ptr, $0180
        LDA     #11
        STA     tmp0
        JSR     tone_on
        LDX     #2
        JSR     busywait
        SETW16  ptr, $0120
        JSR     tone_on
        LDX     #3
        JSR     busywait
        JMP     psg_silence

; sfx_lose - long descending sad tone
sfx_lose:
        SETW16  ptr, $0120
        LDA     #13
        STA     tmp0
        JSR     tone_on
        LDX     #6
        JSR     busywait
        SETW16  ptr, $01C0
        JSR     tone_on
        LDX     #6
        JSR     busywait
        SETW16  ptr, $0280
        JSR     tone_on
        LDX     #10
        JSR     busywait
        JMP     psg_silence

; sfx_win - triumphant fanfare
sfx_win:
        SETW16  ptr, $00FC
        LDA     #15
        STA     tmp0
        JSR     tone_on
        LDX     #4
        JSR     busywait
        SETW16  ptr, $00C8
        JSR     tone_on
        LDX     #4
        JSR     busywait
        SETW16  ptr, $00A8
        JSR     tone_on
        LDX     #4
        JSR     busywait
        SETW16  ptr, $007E
        JSR     tone_on
        LDX     #12
        JSR     busywait
        JMP     psg_silence

;==============================================================================
;  Cooperative 3-channel title music player with hardware envelope.
;
;  Three independent voice tracks play simultaneously:
;     channel A = lead melody   (uses the AY envelope generator so each
;                                note has a soft attack/decay shape)
;     channel B = counter/harmony (fixed mid volume)
;     channel C = bass line       (fixed mid volume)
;
;  Each track is a list of events: (note, duration) byte pairs.
;     note 0      = rest (silence this channel for the duration)
;     note 1..47  = index into note_lo/note_hi period tables
;     note $FF    = end marker -> loop the track from its start
;     duration    = number of music ticks to hold (1..254)
;
;  music_tick is called once per title spin-loop iteration.  It paces
;  itself with a short delay, then advances all three voices.
;==============================================================================

; Tempo: each music tick waits busywait(1) plus an extra fine spin of
; MUS_FINEDELAY * 256 loops.  delay=1 alone (FINE=0) was a little fast;
; FINE=128 is about 1.5x slower.  Raise/lower MUS_FINEDELAY to taste.
MUS_TICKDELAY   = 1             ; busywait outer count per music tick
MUS_FINEDELAY   = 70            ; extra fine delay (0..255), tempo trim

; envelope shape values (reg 13)
ENV_DECAY       = %00000000     ; \____  single decay then off (plucky)
ENV_RAMP        = %00001000     ; \\\\   repeating saw (sustained pulse)
ENV_PERIOD      = $1800         ; 16-bit envelope period (attack/decay len)
MUS_VOL_B       = 8             ; harmony volume (gentle, under the lead)
MUS_VOL_C       = 13            ; bass volume (strong, so it is audible)

;----------------------------------------------------------------
; music_start - reset all three voice cursors and program the AY
; for music: all three tone channels enabled, envelope set up for
; the lead.
;----------------------------------------------------------------
music_start:
; voice cursors -> track starts
        SETW16  mvA_ptr, trackA
        SETW16  mvB_ptr, trackB
        SETW16  mvC_ptr, trackC
; all hold-counters = 0 so every voice fetches on the first tick.
; (must load #0 fresh each time - SETW16 above left A non-zero,
;  which is why B/C previously got garbage counts and went silent)
        LDA     #0
        STA     mvA_cnt
        STA     mvB_cnt
        STA     mvC_cnt

; mixer: enable tone on A, B, C (noise off)  -> bits 0..2 clear
        LDA     #07
        LDY     #%00111000
        JSR     psgwr
; envelope period (regs 11/12) - sets attack/decay length.
; longer = the plucked lead note rings out for more of its
; duration before fading.  Tune ENV_PERIOD to taste.
        LDA     #11
        LDY     #<ENV_PERIOD
        JSR     psgwr
        LDA     #12
        LDY     #>ENV_PERIOD
        JSR     psgwr
; channel A volume register = use envelope (bit4 set)
        LDA     #8
        LDY     #%00010000
        JSR     psgwr
; channel B: gentle harmony pad ; channel C: strong bass
        LDA     #9
        LDY     #MUS_VOL_B
        JSR     psgwr
        LDA     #10
        LDY     #MUS_VOL_C
        JSR     psgwr
        RTS

;----------------------------------------------------------------
; music_tick - advance all three voices by one tick.
;----------------------------------------------------------------
music_tick:
        LDX     #MUS_TICKDELAY
        JSR     busywait
        JSR     mus_finewait    ; fine tempo trim

; --- voice A (lead, envelope) ---
        LDA     mvA_cnt
        BEQ     @nextA
        DEC     mvA_cnt
        JMP     @doB
@nextA:
        SETW16  srcp, trackA    ; (loop base, used if end marker hit)
        LDA     mvA_ptr
        STA     ptr2
        LDA     mvA_ptr+1
        STA     ptr2+1
        JSR     mus_fetch       ; -> tmp0=note, tmp1=dur, ptr2 advanced
; program channel A tone (regs 0/1) from the note
        LDX     tmp0
        BEQ     @restA          ; rest
        JSR     note_period     ; -> ptr = period for note X
        LDA     #0
        LDY     ptr
        JSR     psgwr
        LDA     #1
        LDY     ptr+1
        JSR     psgwr
; (re)trigger the envelope by rewriting the shape register
        LDA     #13
        LDY     #ENV_DECAY
        JSR     psgwr
        JMP     @setA
@restA:
; silence A for the rest: drop its volume to 0 (no envelope)
        LDA     #8
        LDY     #0
        JSR     psgwr
@setA:
; for a non-rest, make sure A uses the envelope again
        LDA     tmp0
        BEQ     @cntA
        LDA     #8
        LDY     #%00010000
        JSR     psgwr
@cntA:
        LDA     tmp1
        STA     mvA_cnt
        LDA     ptr2
        STA     mvA_ptr
        LDA     ptr2+1
        STA     mvA_ptr+1

@doB:
; --- voice B (harmony, fixed volume on channel B) ---
        LDA     mvB_cnt
        BEQ     @nextB
        DEC     mvB_cnt
        JMP     @doC
@nextB:
        SETW16  srcp, trackB
        LDA     mvB_ptr
        STA     ptr2
        LDA     mvB_ptr+1
        STA     ptr2+1
        JSR     mus_fetch
        LDX     tmp0
        BEQ     @restB
        JSR     note_period
        LDA     #2
        LDY     ptr
        JSR     psgwr
        LDA     #3
        LDY     ptr+1
        JSR     psgwr
        LDA     #9              ; restore B volume
        LDY     #MUS_VOL_B
        JSR     psgwr
        JMP     @cntB
@restB:
        LDA     #9
        LDY     #0
        JSR     psgwr
@cntB:
        LDA     tmp1
        STA     mvB_cnt
        LDA     ptr2
        STA     mvB_ptr
        LDA     ptr2+1
        STA     mvB_ptr+1

@doC:
; --- voice C (bass, fixed volume on channel C) ---
        LDA     mvC_cnt
        BEQ     @nextC
        DEC     mvC_cnt
        RTS
@nextC:
        SETW16  srcp, trackC
        LDA     mvC_ptr
        STA     ptr2
        LDA     mvC_ptr+1
        STA     ptr2+1
        JSR     mus_fetch
        LDX     tmp0
        BEQ     @restC
        JSR     note_period
        LDA     #4
        LDY     ptr
        JSR     psgwr
        LDA     #5
        LDY     ptr+1
        JSR     psgwr
        LDA     #10             ; restore C volume
        LDY     #MUS_VOL_C
        JSR     psgwr
        JMP     @cntC
@restC:
        LDA     #10
        LDY     #0
        JSR     psgwr
@cntC:
        LDA     tmp1
        STA     mvC_cnt
        LDA     ptr2
        STA     mvC_ptr
        LDA     ptr2+1
        STA     mvC_ptr+1
        RTS

;----------------------------------------------------------------
; mus_fetch - read one (note,dur) event from the track at ptr2.
;   IN : ptr2 -> current event ; srcp -> track loop base
;   OUT: tmp0 = note, tmp1 = duration, ptr2 advanced past the event.
;        On an $FF end marker, ptr2 is reset to srcp and the first
;        event of the track is returned.
;----------------------------------------------------------------
mus_fetch:
        LDY     #0
        LDA     (ptr2),Y
        CMP     #$FF
        BNE     @ok
; loop: reset ptr2 to track base (srcp)
        LDA     srcp
        STA     ptr2
        LDA     srcp+1
        STA     ptr2+1
        LDY     #0
        LDA     (ptr2),Y
@ok:
        STA     tmp0            ; note
        INY
        LDA     (ptr2),Y
; The voice loops "fetch when counter hits 0", and the fetch
; tick itself is the first tick of the note.  So a note must
; hold for (dur-1) MORE ticks after the fetch tick to last
; exactly dur ticks.  Subtract 1 here.  (All durations are >=2,
; so no underflow.)  This keeps voices with different note
; counts the same real length - otherwise B/C drift away from
; the lead because they have fewer/more notes per loop.
        SEC
        SBC     #1
        STA     tmp1            ; duration - 1
; advance ptr2 by 2
        CLC
        LDA     ptr2
        ADC     #2
        STA     ptr2
        LDA     ptr2+1
        ADC     #0
        STA     ptr2+1
        RTS

;----------------------------------------------------------------
; note_period - look up the 16-bit AY tone period for note X (1..N)
;   OUT: ptr = period (lo/hi).  Note index is 1-based.
;----------------------------------------------------------------
note_period:
        DEX                     ; tables are 0-based
        LDA     note_lo,X
        STA     ptr
        LDA     note_hi,X
        STA     ptr+1
        RTS

;----------------------------------------------------------------
; Note period tables (16-bit AY tone periods), two+ octaves.
; Note numbers (1-based) used by the tracks:
;   1=C3  2=D3 3=E3 4=F3 5=G3 6=A3 7=B3
;   8=C4  9=D4 10=E4 11=F4 12=G4 13=A4 14=B4
;  15=C5 16=D5 17=E5 18=F5 19=G5 20=A5 21=B5
;  22=C6
; (a diatonic C-major set is plenty for a heroic theme)
;----------------------------------------------------------------
note_lo:
        .BYTE   <1008, <898, <800, <756, <672, <600, <534; C3..B3
        .BYTE   <504,  <450, <400, <378, <336, <300, <267; C4..B4
        .BYTE   <252,  <225, <200, <189, <168, <150, <133; C5..B5
        .BYTE   <126            ; C6
note_hi:
        .BYTE   >1008, >898, >800, >756, >672, >600, >534
        .BYTE   >504,  >450, >400, >378, >336, >300, >267
        .BYTE   >252,  >225, >200, >189, >168, >150, >133
        .BYTE   >126

; note name -> index defines for readable tracks
N_REST          = 0
C3              = 1
D3              = 2
E3              = 3
F3              = 4
G3              = 5
A3              = 6
B3              = 7
C4              = 8
D4              = 9
E4              = 10
F4              = 11
G4              = 12
A4              = 13
B4              = 14
C5              = 15
D5              = 16
E5              = 17
F5              = 18
G5              = 19
A5              = 20
B5              = 21
C6              = 22

;----------------------------------------------------------------
; The theme: a stately heroic tune in C major, 3 voices.
; Durations are in ticks; the three tracks are written so their
; total lengths line up over each phrase.
;----------------------------------------------------------------

; --- Channel A : lead melody ---
; Every phrase line totals 16 ticks so the lead stays locked in step
; with the 16-tick harmony (B) and bass (C) lines.  (Line 1 was 20
; ticks before, which drifted B/C out of phase each loop.)
trackA:
        .BYTE   G4,4,  C5,4,  E5,4,  G5,2,  E5,2; 16
        .BYTE   F5,4,  E5,4,  D5,8; 16
        .BYTE   E5,4,  G5,4,  C6,6,  B5,2; 16
        .BYTE   A5,4,  G5,4,  E5,8; 16
        .BYTE   F5,4,  A5,4,  G5,4,  E5,4; 16
        .BYTE   D5,4,  F5,4,  E5,8; 16
        .BYTE   C5,4,  E5,4,  G5,4,  C6,4; 16
        .BYTE   G5,4,  E5,4,  C5,8; 16
        .BYTE   N_REST,4
        .BYTE   $FF

; --- Channel B : sustained harmony, one note per phrase line.
;     To stay consonant under a diatonic melody we use the FIFTH of
;     each chord (perfect fifths never clash), in octave 4.
;     Chords per line:  C  G  C  Am  F  G  C  C
;     Fifths:           G  D  G  E   C  D  G  G
trackB:
        .BYTE   G4,16           ; C chord  (5th = G)
        .BYTE   D4,16           ; G chord  (5th = D)
        .BYTE   G4,16           ; C chord
        .BYTE   E4,16           ; Am chord (5th = E)
        .BYTE   C4,16           ; F chord  (5th = C)
        .BYTE   D4,16           ; G chord
        .BYTE   G4,16           ; C chord
        .BYTE   G4,16           ; C chord
        .BYTE   N_REST,4
        .BYTE   $FF

; --- Channel C : bass - the ROOT of each chord, re-struck halfway
;     for a gentle pulse.  Same progression: C G C Am F G C C
trackC:
        .BYTE   C3,8,  C3,8     ; C
        .BYTE   G3,8,  G3,8     ; G
        .BYTE   C3,8,  C3,8     ; C
        .BYTE   A3,8,  A3,8     ; Am
        .BYTE   F3,8,  F3,8     ; F
        .BYTE   G3,8,  G3,8     ; G
        .BYTE   C3,8,  C3,8     ; C
        .BYTE   C3,8,  C3,8     ; C
        .BYTE   N_REST,4
        .BYTE   $FF
