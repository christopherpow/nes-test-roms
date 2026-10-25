;
; Button test for Super NES controllers
;
; Copyright 2014-2015 Damian Yerrick
; 
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"

LIGHTS_TILE = $EF

;;
; Draws the currently pressed buttons on both controllers.
.proc draw_lights
keys = $00
axlr = $02
  ldx #3
initloop:
  lda cur_keys,x
  sta keys,x
  dex
  bpl initloop

  ldy oam_used
  ldx #0
loop:
  asl axlr
  rol keys
  bcc nop1ball
  lda #32
  jsr draw_ball
nop1ball:
  asl axlr+1
  rol keys+1
  bcc nop2ball
  lda #160
  jsr draw_ball
nop2ball:
  inx
  cpx #16
  bcc loop
  sty oam_used
  rts

draw_ball:

  ; OAM+3: x coordinate
  ; OAM+0: y coordinate
  ; OAM+1: tile number
  ; OAM+2: flipping, priority, and palette
  clc
  adc button_x,x
  sta OAM+3,y
  lda #63
  clc
  adc button_y,x
  sta OAM+0,y
  lda #LIGHTS_TILE
  sta OAM+1,y
  lda #$00
  sta OAM+2,y
  iny
  iny
  iny
  iny
  rts
.endproc

.segment "RODATA"

;               B   Y   Sel Sta Up  Dn  Lt  Rt  A   X   L1  R1  E1  E2  E3  E4
button_x: .byte 48, 41, 24, 31, 11, 11,  7, 15, 55, 48, 19, 41, 16, 24, 32, 40
button_y: .byte 20, 13, 15, 15,  8, 17, 13, 13, 14,  7,<-3,<-3, 28, 28, 28, 28

