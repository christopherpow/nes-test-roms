;
; Menu for Zapper demo
; Copyright 2012 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"

.segment "ZEROPAGE"
yonoff_starty = 0
yonoff_ht = 1
pointat_state:    .res 1
pointat_last_y:   .res 1
group1color:      .res 1
group2color:      .res 1
pointat_cursor_x: .res 1
pointat_cursor_y: .res 1
cur_trigger:      .res 1
new_trigger:      .res 1

.segment "RODATA"
pointat_procs:
  .addr pointat_nothing_proc-1
  .addr pointat_nothing_if_moved-1
  .addr pointat_nothing_if_moved-1
  .addr pointat_testgrp1_proc-1
  .addr pointat_testgrp2_proc-1
  .addr pointat_nothing_if_moved-1
POINTAT_NOTHING = 0
POINTAT_GRP1 = 2
POINTAT_GRP2 = 4
POINTAT_TESTGRP1 = 6
POINTAT_TESTGRP2 = 8
POINTAT_NONTARGET = 10

.segment "CODE"

;;
; Calls the appropriate pointat_*_proc.
.proc pointat_dispatch
  ldx pointat_state
  lda pointat_procs+1,x
  pha
  lda pointat_procs,x
  pha
  rts
.endproc

.proc pointat_nothing_proc
  lda yonoff_ht
  beq nope
  lda yonoff_starty
  cmp #44
  bcc nope  ; within menu headings
  sta pointat_last_y
  lda #COLOR_BLACK
  sta group1color
  lda #POINTAT_TESTGRP1
  sta pointat_state
nope:
  rts
.endproc

.proc pointat_testgrp1_proc
  lda #COLOR_WHITE
  sta group1color
  lda yonoff_ht
  beq hit
  lda #COLOR_BLACK
  sta group2color
  lda #POINTAT_TESTGRP2
  sta pointat_state
  rts
hit:
  lda #POINTAT_GRP1
  sta pointat_state
  rts
.endproc

.proc pointat_testgrp2_proc
  lda #COLOR_WHITE
  sta group2color
  lda yonoff_ht
  beq hit
  lda #POINTAT_NONTARGET
  sta pointat_state
  rts
hit:
  lda #POINTAT_GRP2
  sta pointat_state
  rts
.endproc

;;
; Changes the state to POINTAT_NOTHING if the Y coordinate has
; moved from the point where light was detected.
.proc pointat_nothing_if_moved
  lda yonoff_ht
  beq has_moved

  ; has the gun moved more than 16 pixels?
  lda yonoff_starty
  sec
  sbc pointat_last_y
  bcs dist_not_neg
  eor #$FF
  adc #1
dist_not_neg:
  cmp #16
  bcs has_moved
  lda pointat_state
  rts
has_moved:
  lda #POINTAT_NOTHING
  sta pointat_state
  rts
.endproc

.proc pointat_menu
  ldy #VBLANK_NMI
  sty PPUCTRL
  lda nmis
:
  cmp nmis
  beq :-
  
  jsr load_main_palette
  lda #$20
  ldx #$00
  stx PPUMASK
  sta PPUADDR
  stx PPUADDR
  stx pointat_state
  ldx #COLOR_WHITE
  stx group1color
  stx group2color
  lsr a
  sta cur_trigger
  lda #<menu_pkb
  sta 0
  lda #>menu_pkb
  sta 1
  jsr PKB_unpackblk

  ; Place sprite 0 in the bar at the top of each test pattern screen
  lda #7
  sta OAM
  lda #4
  sta OAM+1
  lda #%00000000
  sta OAM+2
  lda #20
  sta OAM+3
  ldx #4
  stx oam_used

loop:
  lda nmis
vwait:
  cmp nmis
  beq vwait
  lda PPUSTATUS
  lda #VBLANK_NMI
  sta PPUCTRL
  lda #$3F
  sta PPUADDR
  lda #$00
  sta PPUADDR
  lda #$0F
  sta PPUDATA
  lda group1color
  sta PPUDATA
  lda group2color
  sta PPUDATA
  lda #COLOR_GREEN
  sta PPUDATA
  ldx #$00
  stx OAMADDR
  lda #>OAM
  sta OAM_DMA
  ldy #0
  lda #VBLANK_NMI|BG_0000|OBJ_1000
  sec
  jsr ppu_screen_on

  ; handle standard controller
  jsr read_pads

  lda new_keys
  and #KEY_DOWN
  beq not_down
  inc pointat_cursor_y
  lda pointat_cursor_y
  cmp #5
  bcc not_down
  dec pointat_cursor_y
not_down:
  lda new_keys
  and #KEY_UP
  beq not_up
  dec pointat_cursor_y
  bpl not_up
  inc pointat_cursor_y
not_up:
  lda new_keys
  and #KEY_LEFT
  beq not_left
  lda #0
  sta pointat_cursor_x
not_left:
  lda new_keys
  and #KEY_RIGHT
  beq not_right
  lda #1
  sta pointat_cursor_x
not_right:

  jsr s0wait
  ldy #208
  jsr zapkernel_yonoff_ntsc
  jsr pointat_dispatch

  ; If it's pointed at a menu target,
  ; move the cursor to that target.
  lda pointat_state
  cmp #POINTAT_GRP2
  beq is_pointed_grp2
  cmp #POINTAT_GRP1
  bne not_pointed
  lda #0
  sta pointat_cursor_x
  beq get_pointed_y
is_pointed_grp2:
  lda #1
  sta pointat_cursor_x
get_pointed_y:
  sec
  lda pointat_last_y
  sbc #44
  bcs :+
  lda #0
:
  lsr a
  lsr a
  lsr a
  lsr a
  lsr a
  cmp #5
  bcc :+
  lda #4
:
  sta pointat_cursor_y
  lda #POINTAT_NONTARGET
  sta pointat_state
not_pointed:

  lda pointat_cursor_y
  asl a
  asl a
  asl a
  asl a
  asl a
  adc #72
  tay
  ldx #8
  lda pointat_cursor_x
  beq not_pointed_left
  ldx #128
  lda #0
not_pointed_left:
  jsr draw_y_arrow_sprite

  ldx oam_used
  jsr ppu_clear_oam
  lda #4
  sta oam_used

  lda new_keys+0
  ora new_keys+1
  and #KEY_A|KEY_START
  bne done
  jmp loop
done:
  lda pointat_cursor_y
  asl a
  ora pointat_cursor_x
  rts
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
  cpx #32
  bcc copypalloop
  rts
.endproc
.segment "RODATA"

; normal sky is $22; white is $20
initial_palette:
  .byt $0F, $10,$30,$2A,$0F,$00,$00,$00,$0F,$00,$00,$00,$0F,$00,$00,$00
  .byt $0F, $08,$15,$27,$0F,$00,$10,$27,$0F,$0A,$1A,$2A,$0F,$02,$12,$22

.segment "CODE"

