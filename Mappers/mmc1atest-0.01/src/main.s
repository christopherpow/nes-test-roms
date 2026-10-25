;
; Simple sprite demo for NES
; Copyright 2011 Damian Yerrick
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
nmis:          .res 1

test_80: .res 1
test_E0: .res 1
vramdst: .res 2

.segment "CODE"
;;
; This NMI handler is good enough for a simple "has NMI occurred?"
; vblank-detect loop.
.proc nmi_handler
  inc nmis
return:
  rti
.endproc

; Null IRQ handler good for BRKpoints
irq_handler := nmi_handler::return

.proc main
  ; PPU just finished initing so it's safe to write palettes
  ; without making a rainbow streak
  ; seek to the start of palette memory ($3F00-$3F1F)
  lda #VBLANK_NMI
  sta PPUCTRL
  ldx #$3F
  stx PPUADDR
  ldx #$00
  stx PPUADDR
  lda #$10  ; light gray
  sta PPUDATA
  lda #$0F  ; black
  sta PPUDATA

  jsr load_chrdata
  lda #0
  tay
  ldx #$20
  jsr ppu_clear_nt

  lda #$20
  sta vramdst+1
  lda #$82
  sta vramdst+0
  jsr start_line
  lda #>title_line1
  ldy #<title_line1
  jsr puts
  jsr start_line
  lda #>title_line2
  ldy #<title_line2
  jsr puts
  jsr start_line  ; blank row

  ; Actually run the test
  lda #0
  loop80:
    sta test_80
    sta $8000
    .repeat 4
      lsr a
      sta $8000
    .endrepeat
    jsr start_line
    lda #'8'
    sta PPUDATA
    lda #'0'
    sta PPUDATA
    lda #':'
    sta PPUDATA
    lda #'0'
    sta PPUDATA
    ora test_80
    sta PPUDATA
    lda #0
    loopE0:
      sta test_E0
      and #$07
      bne nowrite
        jsr start_line
        lda #' '
        sta PPUDATA
        lda test_E0
        lsr a
        lsr a
        lsr a
        lsr a
        ora #'0'
        sta PPUDATA
        lda test_E0
        and #$0F
        ora #'0'
        sta PPUDATA
        bit PPUDATA
      nowrite:
      lda test_E0
      sta $E000
      .repeat 4
        lsr a
        sta $E000
      .endrepeat
      lda $BFD0
      ora #EVENHEXDIGIT
      sta PPUDATA
      lda $FFD0
      ora #ODDHEXDIGIT
      sta PPUDATA
      inc test_E0
      lda test_E0
      cmp #$20
      bcc loopE0
    lda test_80
    clc
    adc #$04
    cmp #$10
    bcs loop80done
    jmp loop80
  loop80done:

forever:

  ; Good; we have the full screen ready.  Wait for a vertical blank
  ; and set the scroll registers to display it.
  lda nmis
vw3:
  cmp nmis
  beq vw3
  
  ; Turn the screen on
  lda #0
  sta PPUSCROLL
  sta PPUSCROLL
  lda #VBLANK_NMI|BG_0000
  sta PPUCTRL
  lda #BG_ON
  sta PPUMASK
  jmp forever
.endproc

.proc start_line
  lda vramdst+1
  sta PPUADDR
  lda vramdst+0
  sta PPUADDR
  clc
  adc #32
  sta vramdst
  bcc :+
    inc vramdst+1
  :
  rts
.endproc

.proc puts
srclo = $00
srchi = $01
  sta srchi
  sty srclo
  ldy #0
  loop:
    lda (srclo),y
    bmi done
    sta PPUDATA
    iny
    bne loop
  done:
  rts
.endproc

.rodata
title_line1: .byte "MMC1A TEST",EOT
title_line2: .byte "@ 2022 DAMIAN YERRICK",EOT
