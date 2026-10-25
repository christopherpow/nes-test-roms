.include "nes.inc"
.include "global.inc"
.segment "CODE"
.proc draw_bg
src = $00

  lda #<bg_nam
  sta src
  lda #>bg_nam
  sta src+1
  ldy #0
  sty PPUMASK
  bit PPUSTATUS
  lda #VBLANK_NMI
  sta PPUCTRL
  lda #$20
  sta PPUADDR
  sty PPUADDR
  ldx #4
  byteloop:
    lda (src),y
    sta PPUDATA
    iny
    bne byteloop
    inc src+1
    dex
    bne byteloop
  rts
.endproc

bg_nam: .incbin "obj/nes/bg.nam"
