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
.smart
.export main, nmi_handler

.segment "ZEROPAGE"
nmis:     .res 1
oam_used: .res 1

.segment "BSS"
OAM = $0200

.segment "CODE"
;;
; Minimalist NMI handler that only acknowledges NMI and signals
; to the main thread that NMI has occurred.
.proc nmi_handler
  inc nmis
  rti
.endproc

;;
; This program doesn't use IRQs either.
.proc irq_handler
  rti
.endproc

; init.s sends us here
.proc main

  ; this program does not use SPC

  jsr load_bg_tiles  ; fill pattern table
  jsr draw_bg        ; fill nametable

  ; Programming the PPU for the display mode
  ; is done by ppu_screen_on on the NES
  lda #VBLANK_NMI
  sta PPUCTRL

forever:

  ; Draw the lights to a display list in main memory
  ldx #0
  stx oam_used
  jsr draw_lights

  ; Mark remaining sprites as offscreen, then convert sprite size
  ; data from the convenient-to-manipulate format described by
  ; psycopathicteen to the packed format that the PPU actually uses.
  ldx oam_used
  jsr ppu_clear_oam

  ; Backgrounds and OAM can be modified only during vertical blanking.
  ; Wait for vertical blanking and copy prepared data to OAM.
  lda nmis
:
  cmp nmis
  beq :-
  ldx #0
  stx OAMADDR
  lda #>OAM
  sta OAM_DMA
  ldy #0
  lda #VBLANK_NMI|OBJ_0000|BG_0000
  sec
  jsr ppu_screen_on

  ; wait for control reading to finish
  jsr read_spads

  jmp forever
.endproc

