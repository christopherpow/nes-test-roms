;
; Trivial CHR RAM loader for NES
; Copyright 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.export load_chrdata

.rodata
chrdata:  .incbin "obj/nes/bggfx1.chr"
NUM_TILES = (* - chrdata) / 8

.code
.proc load_chrdata
srclo = $00
srchi = $01
  lda #>chrdata
  sta srchi
  lda #<chrdata
  sta srclo
  lda #VBLANK_NMI
  sta PPUCTRL
  ldy #$00
  sty PPUMASK
  bit PPUSTATUS
  sty PPUADDR
  sty PPUADDR
  ldx #NUM_TILES
  tileloop:
    byteloop1:
      lda (srclo),y
      sta PPUDATA
      iny
      cpy #8
      bcc byteloop1
    lda #8-1
    adc srclo
    sta srclo
    lda #0
    byteloop2:
      sta PPUDATA
      dey
      bne byteloop2
    adc srchi
    sta srchi
    dex
    bne tileloop
  rts
.endproc
