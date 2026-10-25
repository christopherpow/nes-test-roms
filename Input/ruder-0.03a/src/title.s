;
; Title screen for Zapper demo
; Copyright 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"

.proc title_screen

  lda #VBLANK_NMI
  sta PPUCTRL
  ldy #0
  sty PPUMASK
  lda nmis
:
  cmp nmis
  beq :-

  ; set palette, very quickly
  ldx #$3F
  stx PPUADDR
  sty PPUADDR

  lda #$2A  ; background color
  sta PPUDATA
  lda #8  ; number of iterations
  sta 0
  ldy #$10
  ldx #$16
  lda #$0F
palload:
  sty PPUDATA
  stx PPUDATA
  sta PPUDATA
  bit PPUDATA
  dec 0
  bne palload

  lda #$20
  sta PPUADDR
  ldx #$00
  stx PPUADDR
oamload:
  lda title_oam,x
  sta OAM,x
  inx
  cpx #title_oam_end-title_oam
  bcc oamload
  jsr ppu_clear_oam

  ; load title screen
  lda #<title_pkb
  sta 0
  lda #>title_pkb
  sta 1
  jsr PKB_unpackblk

  lda #0
  jsr pently_start_music
loop:
  lda nmis
:
  cmp nmis
  beq :-
  
  lda #0
  sta PPUSCROLL
  sta PPUSCROLL
  sta OAMADDR
  lda #>OAM
  sta OAM_DMA
  lda #VBLANK_NMI|BG_0000|OBJ_1000
  sta PPUCTRL
  lda #BG_ON|OBJ_ON
  sta PPUMASK

  jsr pently_update
  jsr read_pads
  
  lda new_keys+0
  ora new_keys+1
  and #KEY_A|KEY_START
  beq loop

  jsr pently_init  ; always call this before stopping pently_update
  rts
.endproc

.segment "RODATA"
title_pkb:
  .incbin "src/title.pkb"
title_oam:
  .byt $3F,$0D,$00,$8C
  .byt $3F,$0E,$00,$B4
  .byt $3F,$0F,$00,$CC
  .byt $47,$1D,$00,$8C
  .byt $47,$1E,$00,$B4
  .byt $47,$1F,$00,$CC
title_oam_end:

