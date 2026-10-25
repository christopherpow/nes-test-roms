;
; Button test for Super NES controllers on NES
;
; Copyright 2014-2016 Damian Yerrick
; 
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"

.proc load_bg_tiles
  ; Copy background palette to the PPU.
  lda #VBLANK_NMI
  sta PPUCTRL
  lda #$3F
  sta PPUADDR
  ldx #$00
  stx PPUADDR
  palloop:
    lda palette,x
    sta PPUDATA
    inx
    cpx #palette_size
    bcc palloop

  ; Copy background tiles to PPU.
  ldy #$00
  sty PPUADDR
  sty PPUADDR
  lda #<padgfx_chr
  sta $00
  lda #>padgfx_chr
  sta $01
  ldx #>-(padgfx_chr_size)
  chrloop:
    lda ($00),y
    sta PPUDATA
    iny
    bne chrloop
    inc $01
    inx
    bne chrloop
  rts
.endproc

.proc draw_bg
cursor_pos = $00
src = $02
linehalf = $04

  ; This demo's background tile set includes glyphs at ASCII code
  ; points $20 (space) through $5F (underscore).  Clear the map
  ; to all spaces.
  ldx #$20
  lda #$40
  ldy #$00
  jsr ppu_clear_nt
  
  ; Draw text
  lda #0
  sta PPUCTRL
  ldx #$21
  stx cursor_pos+1
  ldx #$C2
  stx cursor_pos
  ldx #>desc_msg
  stx src+1
  ldx #<desc_msg
  stx src

lineloop:
  lda cursor_pos+1
  sta PPUADDR
  ldx cursor_pos
  stx PPUADDR
  txa
  clc
  adc #32
  sta cursor_pos
  bcc :+
    inc cursor_pos+1
  :
  txa
  and #%00100000
  lsr a
  sta linehalf
  ldy #0
charloop:
  lda (src),y
  beq nul
  cmp #10
  beq newline
  and #$70
  clc
  adc (src),y
  ora linehalf
  sta PPUDATA
  iny
  bne charloop
newline:
  lda linehalf
  beq lineloop
  tya
  sec
  adc src
  sta src
  bcc lineloop
    inc src+1
    bcs lineloop
nul:
  lda linehalf
  beq lineloop

  ; Draw the controllers
  ldx #$21
  stx PPUADDR
  ldx #$00
  stx PPUADDR
  lda #$08
loop:
  sta src
  jsr draw_half_line
  jsr draw_half_line
  lda src
  clc
  adc #16
  cmp #64
  bcc loop
  rts
  
draw_half_line:
  lda #$40
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  ldx #8
  lda src
  clc
:
  sta PPUDATA
  adc #1
  dex
  bne :-
  lda #$40
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  rts
.endproc

.segment "RODATA"
padgfx_chr:
  .incbin "obj/nes/padgfx.chr"
padgfx_chr_size = * - padgfx_chr

; The background palette
palette:
  .byte $0F,$00,$10,$13, $0F,$01,$01,$01, $0F,$01,$01,$01, $0F,$01,$01,$01
  .byte $0F,$06,$16,$26
palette_size = * - palette

desc_msg:
  .byte "Super NES Controller test",10
  .byte "for NES",10
  .byte "Copr. 2016 Damian Yerrick",10
  .byte 10
  .byte "Press buttons on controllers",10
  .byte "1 and 2 to ensure they work.",0
