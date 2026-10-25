;
; Simple sprite demo for NES
; Copyright 2011-2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;

.include "nes.inc"
.include "global.inc"

OAM = $0200

.segment "ZEROPAGE"
ly_values:     .res 8
nmis:          .res 1
cur_keys:      .res 1
accum_keys:    .res 1
test_progress: .res 1

.segment "CODE"
;;
; This NMI handler is good enough for a simple "has NMI occurred?"
; vblank-detect loop.  But sometimes there are things that you always
; want to happen every frame, even if the game logic takes far longer
; than usual.  These might include music or a scroll split.  In these
; cases, you'll need to put more logic into the NMI handler.
.proc nmi_handler
  inc nmis
  rti
.endproc

; A null IRQ handler that just does RTI is useful to add breakpoints
; that survive a recompile.  Set your debugging emulator to trap on
; reads of $FFFE, and then you can BRK $00 whenever you need to add
; a breakpoint.
;
; But sometimes you'll want a non-null IRQ handler.
; On NROM, the IRQ handler is mostly used for the DMC IRQ, which was
; designed for gapless playback of sampled sounds but can also be
; (ab)used as a crude timer for a scroll split (e.g. status bar).
.proc irq_handler
  rti
.endproc

.proc main

  ; Load the grayscale palette
  lda #VBLANK_NMI
  sta PPUCTRL
  asl a
  sta PPUMASK
  jsr load_main_palette

  ; Load tilemaps for title screen and failure message
  lda #0
  tay
  ldx #$20
  jsr ppu_clear_nt
  ldx #$24
  jsr ppu_clear_nt
  lda #<titlenam
  sta ciSrc+0
  lda #>titlenam
  sta ciSrc+1
  lda #$21  ; "telling LYs?"
  ldy #$00
  ldx #8
  jsr unpb53_xtiles_to_ay
  lda #$21  ; copyright
  ldy #$C0
  ldx #4
  jsr unpb53_xtiles_to_ay
  lda #$22  ; press button
  ldy #$80
  ldx #4
  jsr unpb53_xtiles_to_ay
  lda #$25  ; Failure message
  ldy #$80
  ldx #12
  jsr unpb53_xtiles_to_ay

  ; Load tiles for title screen, conversation, and failure message
  lda #<text_chr
  sta ciSrc+0
  lda #>text_chr
  sta ciSrc+1
  ldy #$00
  tya
  ldx #163
  jsr unpb53_xtiles_to_ay
  lda #<convo_chr
  sta ciSrc+0
  lda #>convo_chr
  sta ciSrc+1
  lda #$10
  ldy #$00
  ldx #1 + CONVO_ARROW_TILE
  jsr unpb53_xtiles_to_ay

  ; Turn on rendering
  jsr wait_keys_up
  jsr ppu_vsync
  lda #VBLANK_NMI|BG_0000
  jsr ppu_screen_on_xy0_noobj

  ; Read the first Y position and save it
  jsr tell_ly
  sty ly_values+0
  sta accum_keys
  sty OAM+0
  lda #1
  sta test_progress
  
  ; Title screen is over.  Load the test screen
  jsr ppu_vsync
  lda #0
  sta PPUMASK
  tay
  ldx #$20
  jsr ppu_clear_nt
  lda #<convo_nam
  sta ciSrc+0
  lda #>convo_nam
  sta ciSrc+1
  lda #$20
  ldy #$C0
  ldx #40
  jsr unpb53_xtiles_to_ay
  lda #CONVO_BLACK_TILE
  ldx #128
  :
    sta PPUDATA
    dex
    bne :-

  ; Show position of first press as one sprite and clear the rest
  lda #CONVO_ARROW_TILE
  sta OAM+1
  lda #0
  sta OAM+2
  lda #224
  sta OAM+3
  ldx #$04
  lda #$FF
  :
    sta OAM,x
    inx
    bne :-

testloop:
  jsr convo_update
testloop_not_new_key:
  jsr ppu_vsync
  lda #0
  sta OAMADDR
  lda #>OAM
  sta OAM_DMA
  jsr wait_keys_up
  jsr tell_ly
  sta cur_keys+0
  sty OAM+0
  ; reject if repress
  bit accum_keys
  bne testloop_not_new_key
  ; reject if more than one key pressed
  sec
  sbc #1
  and cur_keys
  bne testloop_not_new_key
  
  lda accum_keys
  ora cur_keys
  sta accum_keys

  ; save value
  ldx test_progress
  sty ly_values,x
  inx
  stx test_progress
  cpx #8
  bcc testloop

  ; Evaluate whether test passed
minimum = $00
maximum = $01
  lda ly_values
  sta minimum
  sta maximum
  ldx #7
  getminmaxloop:
    lda ly_values,x
    cmp minimum
    bcs :+
      sta minimum
    :
    cmp maximum
    bcc :+
      sta maximum
    :
    dex
    bne getminmaxloop

  ; If all are within 2 of the minimum or maximum, it's a failure
  ldx #7
  checkminmaxloop:
    sec
    lda ly_values,x
    sbc minimum
    cmp #3
    bcc checkminmaxloop_continue
    lda maximum
    sbc ly_values,x
    cmp #3
    bcs not_all_near_min_max
  checkminmaxloop_continue:
    dex
    bpl checkminmaxloop
  jmp failed
not_all_near_min_max:

  ; If not at least 5 bits of difference between low and high values,
  ; it's also a failure
  lda ly_values
  sta minimum
  sta maximum
  ldx #7
  getbitsloop:
    lda ly_values,x
    and minimum
    sta minimum
    lda ly_values,x
    ora maximum
    sta maximum
    dex
    bne getbitsloop
  lda maximum
  eor minimum
  ;ldx #0  ; already true after untaken DEX BNE
  stx $4444
  countbitsloop:
    lsr a
    bcc :+
      inx
    :
    bne countbitsloop
  cpx #5
  bcs show_pass_animation

  ; Show failure message
failed:
  jsr ppu_vsync
  lda #VBLANK_NMI|BG_0000|1
  bne have_final_PPUCTRL

show_pass_animation:
  ; At this point we passed. Animate the ending
  jsr convo_update_2000_ms
  inc test_progress  ; show final row
  jsr convo_update_2000_ms

  ; Show "Pass!" graphic
  lda #0
  sta PPUMASK
  tay
  ldx #$20
  jsr ppu_clear_nt
  lda #<pass_chr
  sta ciSrc+0
  lda #>pass_chr
  sta ciSrc+1
  lda #$00
  tay
  ldx #66
  jsr unpb53_xtiles_to_ay
  lda #<pass_nam
  sta ciSrc+0
  lda #>pass_nam
  sta ciSrc+1
  lda #$21  ; "Pass!"
  ldy #$80
  ldx #12
  jsr unpb53_xtiles_to_ay

  jsr ppu_vsync
  lda #VBLANK_NMI|BG_0000
have_final_PPUCTRL:
  jsr ppu_screen_on_xy0_noobj

:
  jmp :-

.endproc

CONVO_BLACK_TILE = $77
CONVO_ARROW_TILE = $96


.proc convo_update

  ; Update attribute table to show one more line
  jsr ppu_vsync
  lda #$23
  sta PPUADDR
  lda #$D0
  sta PPUADDR
  ldx test_progress
  dex
  ldy #4
  rowloop:
    lda #$50
    cpx #1
    beq have_row_attr
    lda #$00
    bcs have_row_attr
    lda #$55
  have_row_attr:
    .repeat 8
      sta PPUDATA
    .endrepeat
    dex
    dex
    bpl :+
      ldx #0
    :
    dey
    bne rowloop

  lda #0
  sta OAMADDR
  lda #>OAM
  sta OAM_DMA
  lda #VBLANK_NMI|BG_1000|OBJ_1000
  sec
  jsr ppu_screen_on_xy0
  jsr ppu_vsync

  ; Erase keys that have been pressed
  lda #$08
  jsr write_accum_once
  lda #$28
  jsr write_accum_once
  lda #VBLANK_NMI|BG_1000|OBJ_1000
  sec
  jmp ppu_screen_on_xy0

write_accum_once:
  ldy #$23
  sty PPUADDR
  sta PPUADDR
  ldx #0
  ldy #CONVO_BLACK_TILE
  keyloop:
    lda key_at_each_position,x
    and accum_keys
    bne is_pressed
      bit PPUDATA
      bit PPUDATA
      jmp nextkey
    is_pressed:
      sty PPUDATA
      sty PPUDATA
    nextkey:
    inx
    cpx #8
    bcc keyloop
  rts
.endproc

.proc convo_update_2000_ms
  jsr convo_update
  ldy #120
  sty $00
  :
    jsr ppu_vsync
    sec
    lda #VBLANK_NMI|BG_1000|OBJ_1000
    jsr ppu_screen_on_xy0
    dec $00
    bne :-
  rts
.endproc	

;;
; Wait for vblank then wait for all keys to be released
.proc wait_keys_up
  ; Wait for all keys to be released, then turn on screen
  jsr ppu_vsync
  lda #1
  sta $4016
  lsr a
  sta $4016
  ldx #8
  bitloop:
    lda $4016
    and #$03
    bne wait_keys_up
    dex
    bne bitloop
  rts
.endproc

.proc ppu_screen_on_xy0_noobj
  clc
.endproc
.proc ppu_screen_on_xy0
  ldx #0
  ldy #0
  jmp ppu_screen_on
.endproc

.proc unpb53_xtiles_to_ay
  sta PPUADDR
  sty PPUADDR
  jmp unpb53_xtiles
.endproc


.proc load_main_palette
  ; seek to the start of palette memory ($3F00-$3F1F)
  ldx #$3F
  stx PPUADDR
  ldx #$00
  stx PPUADDR
copypalloop:
  lda initial_palette,x
  sta PPUDATA
  inx
  cpx #18
  bcc copypalloop
  rts
.endproc

.segment "RODATA"
initial_palette:
  .byt $20,$10,$00,$0F, $20,$20,$20,$20, $20,$FF,$FF,$FF, $20,$FF,$FF,$FF
  .byt $20,$16

key_at_each_position:
  .byt KEY_LEFT, KEY_DOWN, KEY_UP, KEY_RIGHT
  .byt KEY_SELECT, KEY_START, KEY_B, KEY_A

; Include the CHR ROM data
text_chr:
  .incbin "obj/nes/text.u.chr.pb53"
titlenam:
  .incbin "obj/nes/text.nam.pb53"
convo_chr:
  .incbin "obj/nes/convo.u.chr.pb53"
  .byte $00,$03,$0F,$3F,$FF,$3F,$0F,$03,$00, $80
convo_nam:
  .incbin "obj/nes/convo.nam.pb53"
pass_chr:
  .incbin "obj/nes/pass.u.chr.pb53"
pass_nam:
  .incbin "obj/nes/pass.nam.pb53"

