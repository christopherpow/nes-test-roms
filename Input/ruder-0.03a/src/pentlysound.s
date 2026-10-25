; sound.s
; part of sound engine for LJ65, Concentration Room, and Thwaite

; Copyright 2009-2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.

; Summary of changes:
;
; Pently 3.1 (Zap Ruder, late 2011)
;   Now with an NSF shell
;   Fixed problem with disabling certain channels in pently_init
;   Relicensed permissively
;   Option to call a callback function for each row
;   Exposed subroutine to play a note on a channel
; Pently 3 (Thwaite, early 2011)
;   Moved sound effects to musicseq.s
;   A single drum note can start two sound effects
;   A music note can override the quiet tail of a sound effect
;   Phrase data can include instrument changes
;   NTSC/PAL automatic adaptation
;   Added macro definitions for conductor events
; Pently 2.1 (Concentration Room, 2010)
;   Fixed bug with interrupting a note with a playPattern command,
;   occurring especially with non-multiple-of-16 waitRows
;   Added macro definitions for note pitches and durations
; Pently 2 (LJ65, 2009)
;   add music
; Pently 1 (LJ65, 2009)
;   initial release

.import periodTableLo, periodTableHi
.importzp pently_zp_state  ; a 32 byte buffer in zp?
.import update_music, update_music_ch, pently_music_playing, pently_sfx_table
.export pently_init, pently_start_sound, pently_update, pentlyBSS

SNDCHN = $4015

; Ordinarily, the effect engine will move a pulse sound effect from
; $4000 to $4004 if $4004 is idle and $4000 is not, or if $4004 has
; less sfx data left to play than $4000.  Turn this off to force all
; pulse sfx to be played on $4000.
SQUARE_POOLING = 1

; As of 2011-03-10, a sound effect interrupts a musical instrument on
; the same channel only if the volume of the sfx is greater than that
; of the instrument.  Turn this off to force sound fx to interrupt
; the music whenever sfx data remains on that channel, even if the
; music is louder.
KEEP_MUSIC_IF_LOUDER = 1

.segment "BSS"
pentlyBSS: .res 64

sfx_datalo = pently_zp_state + 0
sfx_datahi = pently_zp_state + 1
ch_lastfreqhi = pently_zp_state + 18
sfx_remainlen = pently_zp_state + 19
sfx_rate = pentlyBSS + 3
sfx_ratecd = pentlyBSS + 19

.ifndef PENTLY_USE_PAL_ADJUST
PENTLY_USE_PAL_ADJUST = 1
.endif
.if PENTLY_USE_PAL_ADJUST
.importzp tvSystem
.endif

.segment "CODE"

;;
; Initializes all sound channels.
; Use it at the start of a program or as a "panic button" before
; entering a long stretch of code where you don't pently_update.
;
.proc pently_init
  lda #$0F
  sta SNDCHN
  lda #$30
  sta $4000
  sta $4004
  sta $400C
  sta ch_lastfreqhi+0
  sta ch_lastfreqhi+8
  sta ch_lastfreqhi+4
  lda #$80
  sta $4008
  lda #8
  sta $4001
  sta $4005
  lda #0
  sta $4003
  sta $4007
  sta $400F
  sta sfx_remainlen+0
  sta sfx_remainlen+4
  sta sfx_remainlen+8
  sta sfx_remainlen+12
  sta pently_music_playing
  lda #64
  sta $4011
  rts
.endproc

;;
; Starts a sound effect.
; (Trashes $0000-$0004 and X.)
;
; @param A sound effect number (0-63)
;
.proc pently_start_sound
snddatalo = 0
snddatahi = 1
sndchno = 2
sndlen = 3
sndrate = 4

  asl a
  asl a
  tax
  lda pently_sfx_table,x
  sta snddatalo
  lda pently_sfx_table+1,x
  sta snddatahi
  lda pently_sfx_table+2,x
  and #$0C
  sta sndchno
  lda pently_sfx_table+2,x
  lsr a
  lsr a
  lsr a
  lsr a
  sta sndrate
  
  lda pently_sfx_table+3,x
  sta sndlen

  ; split up square wave sounds between $4000 and $4004
  .if ::SQUARE_POOLING
    lda sndchno
    bne not_ch0to4  ; if not ch 0, don't try moving it
      lda sfx_remainlen+4
      cmp sfx_remainlen
      bcs not_ch0to4
      lda #4
      sta sndchno
    not_ch0to4:
  .endif 

  ldx sndchno
  lda sndlen
  cmp sfx_remainlen,x
  bcs ch_not_full
  rts
ch_not_full:

  lda snddatalo
  sta sfx_datalo,x
  lda snddatahi
  sta sfx_datahi,x
  lda sndlen
  sta sfx_remainlen,x
  lda sndrate
  sta sfx_rate,x
  lda #0
  sta sfx_ratecd,x
  rts
.endproc


;;
; Updates sound effect channels.
;
.proc pently_update
  jsr update_music
  ldx #12
loop:
  jsr update_music_ch
  jsr update_one_ch
  dex
  dex
  dex
  dex
  bpl loop
  rts
.endproc

.proc update_one_ch

  ; At this point, the music engine should have left duty and volume
  ; in 2 and pitch in 3.
  lda sfx_remainlen,x
  bne ch_not_done
  lda 2
  bne update_channel_hw

  ; Turn off the channel and force a reinit of the length counter.
  cpx #8
  beq not_triangle_kill
    lda #$30
  not_triangle_kill:
  sta $4000,x
  lda #$FF
  sta ch_lastfreqhi,x
  rts
ch_not_done:

  ; playback rate divider
  dec sfx_ratecd,x
  bpl rate_divider_cancel
  lda sfx_rate,x
  sta sfx_ratecd,x

  ; fetch the instruction
  lda sfx_datalo+1,x
  sta 1
  lda sfx_datalo,x
  sta 0
  clc
  adc #2
  sta sfx_datalo,x
  bcc :+
  inc sfx_datahi,x
:
  ldy #0
  .if ::KEEP_MUSIC_IF_LOUDER
    lda 2
    and #$0F
    sta 4
    lda (0),y
    and #$0F
    
    ; At this point: A = sfx volume; 4 = musc volume
    cmp 4
    bcc music_was_louder
  .endif
  lda (0),y
  sta 2
  iny
  lda (0),y
  sta 3
music_was_louder:
  dec sfx_remainlen,x

update_channel_hw:
  lda 2
  ora #$30
  cpx #12
  bne notnoise
  sta $400C
  lda 3
  sta $400E
rate_divider_cancel:
  rts

notnoise:
  sta $4000,x
  ldy 3
.if ::PENTLY_USE_PAL_ADJUST
  lda tvSystem
  beq :+
    iny
  :
.endif
  lda periodTableLo,y
  sta $4002,x
  lda periodTableHi,y
  cmp ch_lastfreqhi,x
  beq no_change_to_hi_period
  sta ch_lastfreqhi,x
  sta $4003,x
no_change_to_hi_period:
  rts
.endproc
