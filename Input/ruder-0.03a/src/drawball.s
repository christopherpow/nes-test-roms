;
; Ball for Zapper demo
; Copyright 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"

;;
; X: x position of ball center
; Y: y position of ball center
; A: radius (bits 5-0) and palette (bits 7-6) of ball
.proc draw_ball
ypos = 0
base_tileno = 1
attrs = 2
xpos = 3
rows_left = 4  ; actually height minus 1
widthcd = 5
leftside = 6
width = 7  ; actually width minus 1

  dey
  sty ypos
  stx leftside
  sta base_tileno
  rol a
  rol a
  rol a
  and #%00000011
  sta attrs
  
  ; clip radius to 1-16
  lda base_tileno
  and #%00111111
  bne clip_not_below_1
  lda #1
clip_not_below_1:
  cmp #16
  bcc clip_not_above_16
  lda #16
clip_not_above_16:

  sec
  sbc #1
  sta base_tileno
  lsr a
  lsr a
  sta width
  sta rows_left
  tax
  
  ; compute offset for whole sprite
  asl a
  asl a
  eor #$FC
  pha
  adc leftside
  sta leftside
  pla
  clc
  adc ypos
  sta ypos
  
  lda widthinstructions,x
  tax ; X is data pointer
  lda base_tileno
  asl a
  and #%00001110
  ora #$40
  sta base_tileno
  ldy oam_used
  
  ; X: Pointer into array of tile numbers
  ; Y: Pointer into OAM
rowloop:
  lda leftside
  sta xpos
  lda width
  sta widthcd
charloop:
  lda ypos
  sta OAM,y
  lda widthinstructions,x
  cmp #2
  beq got_tileno
  and #%00111111
  ora base_tileno
got_tileno:
  sta OAM+1,y
  lda widthinstructions,x
  and #%11000000
  ora attrs
  sta OAM+2,y
  lda xpos
  sta OAM+3,y
  clc
  adc #8
  sta xpos
  iny
  iny
  iny
  iny
  beq bail
  inx
  dec widthcd
  bpl charloop
  lda ypos
  clc
  adc #8
  sta ypos
  dec rows_left
  bpl rowloop
bail:
  sty oam_used
  rts
.endproc

.segment "RODATA"
widthinstructions:
  .byt widthinstructions_0-widthinstructions
  .byt widthinstructions_1-widthinstructions
  .byt widthinstructions_2-widthinstructions
  .byt widthinstructions_3-widthinstructions
widthinstructions_0:
  .byt $11
widthinstructions_1:
  .byt $11,$51,$91,$D1
widthinstructions_2:
  .byt $00,$01,$40,$10,$02,$50,$80,$81,$C0
widthinstructions_3:
  .byt $00,$01,$41,$40,$10,$02,$02,$50,$90,$02,$02,$D0,$80,$81,$C1,$C0
; bits 5-0: OR'd with tile number
; bit 6: horizontal flip
; bit 7: vertical flip

PADDLE_ENDCAP_TILE = $05
PADDLE_BODY_TILE = $02

.segment "CODE"
;;
; X: x position of paddle top center
; Y: y position of paddle top center
; A: length (bits 5-0) and palette (bits 7-6) of paddle
.proc draw_paddle
ypos = 0
base_tileno = 1
attrs = 2
xpos = 3
ht_left = 4  ; actually height minus 1
widthcd = 5
leftside = 6
width = 7  ; actually width minus 1

  sta ht_left
  rol a
  rol a
  rol a
  and #%00000011
  sta attrs
  lda ht_left
  and #%00111111
  sta ht_left
  txa
  sec
  sbc #4
  sta xpos
  tya
  sec
  sbc #5
  sta ypos
  
  ldy oam_used
  cpy #$F8
  bcs bail

  ; Draw top and bottom segments
  sta OAM,y
  clc
  adc ht_left
  sta OAM+4,y
  lda #PADDLE_ENDCAP_TILE
  sta OAM+1,y
  sta OAM+5,y
  lda xpos
  sta OAM+7,y
  sta OAM+3,y
  lda attrs
  sta OAM+2,y
  eor #%10000000
  sta OAM+6,y
  tya
  clc
  adc #8
  tay
  beq bail

  ; Figure how many center segments are needed
  ; 9-16: one; 17-32: two; 
  dec ht_left
  lda ht_left
  cmp #8
  bcc bail
  and #%00000111
  eor #%00001000
  lsr a
  adc ypos
  sta ypos
  lda ht_left
  lsr a
  lsr a
  lsr a
  tax
loop:
  lda ypos
  sta OAM,y
  clc
  adc #8
  sta ypos
  lda #PADDLE_BODY_TILE
  sta OAM+1,y
  lda attrs
  sta OAM+2,y
  lda xpos
  sta OAM+3,y
  iny
  iny
  iny
  iny
  beq bail
  dex
  bne loop
  
bail:
  sty oam_used
  rts
.endproc

