;
; Axe music toy for NES
; Copyright 2012 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"
.export music_row_callback, music_dalsegno_callback, axe_callback_on

; overlay some of tennis's variables
.importzp paddle_yhi, paddle_ylo, ball_dxlo, ball_dxhi

AXE_SCROLL_SPEED = 16*256/7
SOFT_ACCENT_THRESHOLD = 10

.segment "BSS"
lastNotes: .res 128
rowNumber: .res 1
axe_callback_on: .res 1
axe_xoffset_lo = ball_dxlo
axe_xoffset_hi = ball_dxhi
noteAccent = paddle_ylo
explosion_y = paddle_yhi+1

scroll_colbuf = $0100
scroll_dsthi = $0118
scroll_dstlo = $0119

new_dot_shape = $011D
new_dot_dsthi = $011E
new_dot_dstlo = $011F

.segment "CODE"
.align 128
.proc zapkernel_dummy_ntsc
  lda #$00
loop:
  ; 96
  jsr waste_12
  jsr waste_12
  jsr waste_12
  jsr waste_12
  jsr waste_12
  jsr waste_12
  jsr waste_12
  jsr waste_12

  ; 6
  bit $00
  bit $00

  ; 11.667
  clc
  adc #$AA
  bcs :+
:
  dey
  bne loop
waste_12:
  rts
.endproc

.proc axe
  lda #VBLANK_NMI
  sta PPUCTRL
  sta doubleshoot_time
  sta axe_callback_on
  ldx #$00
  stx noteAccent
  stx held_time
  stx PPUMASK
  lda #$3F
  sta PPUADDR
  stx PPUADDR
copypal:
  lda axe_palette,x
  sta PPUDATA
  inx
  cpx #$20
  bcc copypal
  
  jsr axe_init_bg
  
  ldx #127
  lda #$FF
  sta rowNumber
clear_notes:
  sta lastNotes,x
  dex
  bpl clear_notes
  lda #3
  jsr pently_start_music
  lda #$FF
  sta new_dot_dsthi
  sta scroll_dsthi

loop:
  jsr prepare_vram_transfers
  
  lda nmis
:
  cmp nmis
  beq :-
  lda PPUSTATUS
  
  lda #VBLANK_NMI|VRAM_DOWN
  sta PPUCTRL
  lda scroll_dsthi
  bmi no_new_scroll
  sta PPUADDR
  lda scroll_dstlo
  sta PPUADDR
  ldx #23
scroll_blit_loop:
  lda scroll_colbuf,x
  sta PPUDATA
  dex
  bpl scroll_blit_loop

  lda #$FF
  sta scroll_dsthi
no_new_scroll:

  ; Draw new dot  
  lda new_dot_dsthi
  bmi no_new_dot
  sta PPUADDR
  lda new_dot_dstlo
  sta PPUADDR
  lda new_dot_shape
  sta PPUDATA
  lda #$FF
  sta new_dot_dsthi
no_new_dot:

  ; TO DO: draw new column of notes if needed

  ldx 2
  ldy #0
  sty OAMADDR
  lda #>OAM
  sta OAM_DMA  
  lda 3
  sec
  jsr ppu_screen_on
  lda #$FF
  sta scroll_dsthi
  
  jsr read_pads
  clc
  lda #<AXE_SCROLL_SPEED
  adc axe_xoffset_lo
  sta axe_xoffset_lo
  lda #>AXE_SCROLL_SPEED
  adc axe_xoffset_hi
  sta axe_xoffset_hi

  ; compute held_time for accent
  lda cur_keys+1
  ora cur_keys
  and #KEY_A
  sta cur_trigger
  beq notA
  lda #1
  sta noteAccent
  inc held_time
  lda held_time
  cmp #20
  bcc doneA
  lda #20
  bne doneA
notA:
  lda held_time
  beq doneA
  cmp #SOFT_ACCENT_THRESHOLD
  bcs noHardAccent
  lda #2
  sta noteAccent
noHardAccent:
  lda #0
doneA:
  sta held_time
  
  jsr pently_update

  ; read the zapper
  jsr s0wait
  ldy #191
  jsr zapkernel_yonoff_ntsc
  cpy #0
  beq :+
  jsr zapkernel_dummy_ntsc
:
  lda #VBLANK_NMI|BG_0000|OBJ_1000
  sta PPUCTRL
  lda #0
  sta PPUSCROLL
  sta PPUSCROLL

  lda 0
  ldx 1
  bne not_offscreen
  lda #$EF
not_offscreen:
  and #%11111000
  ora #%00000011
  sta paddle_yhi
  

  ; If player 2's gun has shot twice in quick succession
  ; and is pointed offscreen, cancel
  cpx #0
  bne not_triggered
  lda new_keys+1
  bpl not_triggered
  lda doubleshoot_time
  cmp #DOUBLESHOOT_FRAMES
  bcs triggered_once
  lda #KEY_B
  ora new_keys+0
  sta new_keys+0
triggered_once:
  lda #0
  sta doubleshoot_time
not_triggered:
  inc doubleshoot_time
  bne :+
  dec doubleshoot_time
:

  lda new_keys
  and #KEY_B
  bne done
  jmp loop
done:
  jsr pently_init
  lda #0
  sta axe_callback_on
  rts
.endproc

.proc music_row_callback
  bit axe_callback_on
  bmi :+
  rts
:

  lda #0
  sta axe_xoffset_lo
  sta axe_xoffset_hi
  lda #$FF
  sta explosion_y
  inc rowNumber
  lda rowNumber
  and #$7F
  sta rowNumber

; triangle: play 128 notes back
  sec
  ldy rowNumber
  lda lastNotes,y
  cmp #$FF
  beq no_minus128

  lsr a
  lsr a
  tax
  ldy #9  ; instrument for triangle echo
  lda pentatonic_notes,x
  clc
  adc #12
  ldx #8
  jsr pently_play_note
  
no_minus128:
  
  lda #190
  sec
  sbc paddle_yhi
  cmp #191
  bcs clear_echo
  lsr a
  lsr a
  lsr a
  tax
  
  ; prepare the note number to be drawn
  lda pentatonic_keycolors,x
  ora #$02
  sta new_dot_shape
  jsr get_dot_pos_x
  
  ; store the note number
  txa
  asl a
  asl a
  ora noteAccent
  ldy rowNumber
  sta lastNotes,y
  
  ; set the instrument (3-5: normal; 6-8: echoed)
  lda noteAccent
  clc
  adc #3
  tay
  lda pentatonic_notes,x
  ldx #4  ; square 2
  jsr pently_play_note

  dec noteAccent
  bpl no_clip_accent
  lda #0
  sta noteAccent
no_clip_accent:
  jmp no_minusthree
clear_echo:
  lda #$FF
  ldy rowNumber
  sta lastNotes,y

echo_minusthree:
  sec
  lda rowNumber
  sbc #3
  and #$7F
  tay
  lda lastNotes,y
  cmp #$FF
  beq no_minusthree

  lsr a
  lsr a
  tax
  lda lastNotes,y
  and #$03
  clc
  adc #6
  tay
  lda pentatonic_notes,x
  ldx #4
  jsr pently_play_note
  
no_minusthree:

  rts
.endproc

.proc music_dalsegno_callback
  lda #0
  sta rowNumber
  rts
.endproc

; BG GRAPHICS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.proc axe_init_bg
nts_left = 7
rows_left = 6

  ; First establish the stripes across the nametables.
  lda #$20
  sta PPUADDR
  lda #$00
  sta PPUADDR
  lda #2
  sta nts_left
ntloop:
  lda #0
  ldy #1
  jsr ppu_sta32y
  lda #'_'
  ldy #1
  jsr ppu_sta32y
  lda #23
  sta rows_left
rowloop:
  ldy rows_left
  lda pentatonic_keycolors,y
  ldy #1
  jsr ppu_sta32y
  dec rows_left
  bpl rowloop
  lda #0
  ldy #6
  jsr ppu_sta32y
  dec nts_left
  bpl ntloop

  ; now draw the bar lines for measures and beats
barline_type = rows_left
  lda #VBLANK_NMI|VRAM_DOWN
  sta PPUCTRL
  lda #$20
  sta nts_left
barline_ntloop:
  ldx #$47
barlineloop:

  ; beat dots in columns 7, 15, 23; measure dots in 31
  lda #$06
  cpx #$5F
  bcc barline_not_measure
  lda #$04
barline_not_measure:
  sta barline_type

  lda nts_left
  sta PPUADDR
  stx PPUADDR
  ldy #23
barcell_loop:
  lda pentatonic_keycolors,y
  ora barline_type
  sta PPUDATA
  dey
  bpl barcell_loop
  
  ; next barline
  txa
  clc
  adc #8
  tax
  cpx #$60
  bcc barlineloop

  ; next nametable  
  lda nts_left
  adc #4-1
  sta nts_left
  cmp #$28
  bcc barline_ntloop
  lda #VBLANK_NMI
  sta PPUCTRL
  rts
.endproc

TRAILING_ROWS = 6
VERTICAL_BAR_TILE = $09

.proc get_dot_pos_x
  lda #0
  sta new_dot_dstlo
  txa
  eor #$1F
  sec
  sbc #6
  ror a
  ror new_dot_dstlo
  lsr a
  ror new_dot_dstlo
  ror a
  ror new_dot_dstlo
  sta new_dot_dsthi
  lda rowNumber
  and #$0F
  asl a
  ora new_dot_dstlo
  sta new_dot_dstlo
  lda rowNumber
  and #$10
  lsr a
  lsr a
  ora new_dot_dsthi
  sta new_dot_dsthi
  txa
  asl a
  asl a
  asl a
  eor #$FF
  adc #202
  sta explosion_y
  rts
.endproc

.proc prepare_vram_transfers

  ; 1. Draw next column to be seen in 16 ticks
  lda rowNumber
  clc
  adc #16
  and #$7F
  tay
  lda lastNotes,y
  lsr a
  lsr a
  sta 0
  
  ldx #23
makecolloop:
  lda #0
  cpx 0
  bne :+
  lda #2
:
  ora pentatonic_keycolors,x
  sta scroll_colbuf,x
  dex
  bpl makecolloop

  ; 2. Compute destination address for next row 
  lda rowNumber
  and #$0F
  asl a
  ora #$40
  sta scroll_dstlo
  lda rowNumber
  and #$10
  lsr a
  lsr a
  eor #$24
  sta scroll_dsthi

  ; 3. Draw aiming cursor
  lda paddle_yhi
  clc
  adc #16
  tay
  ldx #8
  lda #0
  jsr draw_y_arrow_sprite
  
  ; 4. Draw explosion
  ldx oam_used
  lda explosion_y
  cmp #$EF
  bcs no_explosion

  sta OAM+0,x
  sta OAM+4,x
  sta OAM+8,x
  sta OAM+12,x
  sta OAM+16,x
  sta OAM+20,x
  lda #8
  sta OAM+1,x
  sta OAM+5,x
  sta OAM+9,x
  sta OAM+13,x
  sta OAM+17,x
  sta OAM+21,x
  lda #1
  sta OAM+2,x
  sta OAM+6,x
  sta OAM+10,x
  sta OAM+14,x
  sta OAM+18,x
  sta OAM+22,x
  lda #96
  clc
  adc axe_xoffset_hi
  sta OAM+3,x
  clc
  adc axe_xoffset_hi
  sta OAM+7,x
  clc
  adc axe_xoffset_hi
  sta OAM+11,x
  lda #96
  sec
  sbc axe_xoffset_hi
  sta OAM+15,x
  sec
  sbc axe_xoffset_hi
  sta OAM+19,x
  sec
  sbc axe_xoffset_hi
  sta OAM+23,x
  txa
  clc
  adc #24
  tax
no_explosion:

  ; 5. Draw timeline
  lda nmis
  and #$01
  beq timeline_flickering_top
  lda #$08
timeline_flickering_top:
  clc
  adc #15
  sta 2
  ldy #12
timeline_rowloop:
  lda 2
  sta OAM,x
  clc
  adc #16
  sta 2
  lda #VERTICAL_BAR_TILE
  sta OAM+1,x
  lda #%00000000
  sta OAM+2,x
  lda #16*TRAILING_ROWS
  sta OAM+3,x
  inx
  inx
  inx
  inx
  dey
  bne timeline_rowloop
  jsr ppu_clear_oam
  
  ; 6. Set up sprite 0
  lda #4
  sta oam_used
  lda #7
  sta OAM+0
  lda #4
  sta OAM+1
  lda #%00000000
  sta OAM+2
  lda #128
  sta OAM+3
  
  ; 7. Calculate scroll position
  lda #0
  sta 3
  lda rowNumber
  sec
  sbc #TRAILING_ROWS
  asl a
  rol 3
  asl a
  rol 3
  asl a
  rol 3
  asl a
  rol 3
  adc axe_xoffset_hi
  sta 2
  lda 3
  adc #0
  and #$01
  ora #VBLANK_NMI|BG_0000|OBJ_1000
  sta 3

  rts
.endproc

.segment "RODATA"
pentatonic_notes:
  .repeat 5, I
    .byt 4+12*I, 7+12*I, 9+12*I, 11+12*I, 14+12*I
  .endrepeat
pentatonic_keycolors:
  .repeat 5
    .byt $10, $11, $10, $10, $11
  .endrepeat
axe_palette:
  .byt $0F,$10,$21,$30, $0F,$10,$21,$30, $0F,$10,$21,$30, $0F,$10,$21,$30
  .byt $0F,$26,$26,$26, $0F,$16,$27,$38, $0F,$00,$00,$00, $0F,$00,$00,$00
