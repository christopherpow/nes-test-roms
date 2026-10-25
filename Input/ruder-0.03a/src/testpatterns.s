;
; Test patterns for Zapper demo
; Copyright 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"

MEDIAN_SIZE = 7

.segment "ZEROPAGE"
cur_hue:    .res 1
cur_bright: .res 1
cur_radius: .res 1
held_time:  .res 1
doubleshoot_time: .res 1
median_ring: .res MEDIAN_SIZE
DOUBLESHOOT_FRAMES = 30

.segment "RODATA"
menu_pkb:        .incbin "src/menu.pkb"
fullbright_pkb:  .incbin "src/fullbright.pkb"
hlines_pkb:      .incbin "src/hlines.pkb"
vlines_pkb:      .incbin "src/vlines.pkb"
ballbg_pkb:      .incbin "src/ballbg.pkb"
pulltrigger_pkb: .incbin "src/pulltrigger.pkb"

yonoff_backgrounds:
  .addr fullbright_pkb, hlines_pkb, vlines_pkb

.segment "CODE"
.proc test_vlines_yonoff
  ldx #4
  bne test_basic_yonoff
.endproc
.proc test_hlines_yonoff
  ldx #2
  bne test_basic_yonoff
.endproc
.proc test_fullbright_yonoff
  ldx #0
.endproc
.proc test_basic_yonoff
  lda yonoff_backgrounds+0,x
  sta PKB_source+0
  lda yonoff_backgrounds+1,x
  sta PKB_source+1
  lda #$FF
  sta doubleshoot_time
  stx pointat_state  ; save whether or not brightness goes by twos
  
  ; turn off rendering and decompress to the first nametable
  lda #$20
  ldx #$00
  ldy #VBLANK_NMI
  stx PPUMASK
  sty PPUCTRL
  sta PPUADDR
  stx PPUADDR
  jsr PKB_unpackblk

  lda #0
  sta $100

loop:
  jsr basic_tests_next_frame

  ; Control the hue and brightness
  ldy pointat_state
  jsr hue_bright_control
  ldy pointat_state
  beq not_by_twos
  lda cur_bright
  cmp #7
  bcc not_by_twos_under_7
  lda #7
not_by_twos_under_7:
  ora #$01  ; Force odd for single-color patterns
  sta cur_bright
not_by_twos:

  jsr s0wait
  bvc no_s0
  ldy #180
  jsr zapkernel_yonoff_ntsc
  lda 0
  sta 2
  lda 1
  sta 3

  ldx #12
:
  lda yht_msg,x
  sta $100,x
  dex
  bpl :-
  lda 2
  ldx #2
  jsr bcdout1xx
  lda 3
  ldx #9
  jsr bcdout1xx
  
  lda 2
  clc
  adc #16
  tay
  clc
  adc 3
  pha
  lda #$00
  ldx #8
  jsr draw_y_arrow_sprite
  pla
  tay
  lda #$01
  ldx #8
  jsr draw_y_arrow_sprite
no_s0:

  lda new_keys+0
  and #KEY_B
  beq loop
  rts 
.endproc

.proc test_fullbright_yon2p
  lda #<fullbright_pkb
  sta PKB_source+0
  lda #>fullbright_pkb
  sta PKB_source+1
  lda #$FF
  sta doubleshoot_time
  stx pointat_state  ; save whether or not brightness goes by twos
  
  ; turn off rendering and decompress to the first nametable
  lda #$20
  ldx #$00
  ldy #VBLANK_NMI
  stx PPUMASK
  sty PPUCTRL
  sta PPUADDR
  stx PPUADDR
  jsr PKB_unpackblk

  lda #0
  sta $100

loop:
  jsr basic_tests_next_frame

  ; Control the hue and brightness
  ldy #0
  jsr hue_bright_control
  jsr s0wait
  bvc no_s0
  ldy #180
  jsr zapkernel_yon2p_ntsc
  lda 0
  sta 2
  lda 1
  sta 3

  ldx #13
:
  lda y1y2_msg,x
  sta $100,x
  dex
  bpl :-
  lda 2
  ldx #3
  jsr bcdout1xx
  lda 3
  ldx #10
  jsr bcdout1xx
  
  lda 3
  pha
  lda 2
  clc
  adc #16
  tay
  lda #$01
  ldx #8
  jsr draw_y_arrow_sprite
  pla
  clc
  adc #16
  tay
  lda #$40
  ldx #240
  jsr draw_y_arrow_sprite
no_s0:

  lda new_keys+0
  and #KEY_B
  bne bail
  lda $4016
  and $4017
  and #$10
  beq loop
bail:
  rts 
.endproc

.proc test_fullbright_xyon
tmpy = 6
tmpx = 7
  lda #<fullbright_pkb
  sta PKB_source+0
  lda #>fullbright_pkb
  sta PKB_source+1
  lda #$FF
  sta doubleshoot_time
  stx pointat_state  ; save whether or not brightness goes by twos
  
  ; turn off rendering and decompress to the first nametable
  lda #$20
  ldx #$00
  ldy #VBLANK_NMI
  stx PPUMASK
  sty PPUCTRL
  sta PPUADDR
  stx PPUADDR
  jsr PKB_unpackblk

  lda #0
  sta $100

loop:
  jsr basic_tests_next_frame
  ldx #MEDIAN_SIZE-2
median_shift:
  lda median_ring,x
  sta median_ring+1,x
  dex
  bpl median_shift  

  ; Control the hue and brightness
  ldy #0
  jsr hue_bright_control
  jsr s0wait
  bvc no_s0
  
  ; align to where i ended up putting s0
  ldy #15
:
  dey
  bne :-
  ldy #180
  jsr zapkernel_xyon_ntsc

  ; clip X to right of screen
  cpx #14
  bcc :+
  ldx #14
:
  stx tmpx

  ; Convert to screen X coordinate
  txa
  asl a
  asl a
  asl a
  adc tmpx
  asl a
  sta median_ring+0
  tya
  eor #$FF
  sec
  adc #180+16
  sta tmpy

  ldx #13
:
  lda xy_msg,x
  sta $100,x
  dex
  bpl :-

  ; Draw text
  lda tmpy
  ldx #7
  jsr bcdout1xx
  lda tmpx
  ldx #1
  jsr bcdout1xx

  ; Draw vertical arrow
  ldy tmpy
  lda #$00
  ldx #8
  jsr draw_y_arrow_sprite
  
  ; Draw current position horizontal arrow
  ldx median_ring+0
  lda #$01
  ldy #16
  jsr draw_x_arrow_sprite

  ; Calculate smoothed position
  jsr do_x_median
  tax
  lda #$81
  ldy #192
  jsr draw_x_arrow_sprite
no_s0:

  lda new_keys+0
  and #KEY_B
  bne bail
  lda $4016
  and $4017
  and #$10
  bne bail
  jmp loop
bail:
  rts 
.endproc

.proc do_x_median
median_temp = $0110
  ldx #MEDIAN_SIZE-1
medcopy:
  lda median_ring,x
  sta median_temp,x
  dex
  bpl medcopy
  inx  

  ; now insertion sort them
medgnome:
  lda median_temp+1,x
  cmp median_temp,x
  bcs medgnome_noswap
  pha
  lda median_temp,x
  sta median_temp+1,x
  pla
  sta median_temp,x
  dex
  bpl medgnome
  inx
medgnome_noswap:
  inx
  cpx #MEDIAN_SIZE-1
  bcc medgnome

  ; calculate L/4+C/2+R/4
  clc
  lda median_ring+(MEDIAN_SIZE/2)-1
  adc median_ring+(MEDIAN_SIZE/2)+1
  ror a
  adc median_ring+(MEDIAN_SIZE/2)
  ror a
  adc #0
  rts
.endproc

.proc test_ball_yonoff
  lda #<ballbg_pkb
  sta PKB_source+0
  lda #>ballbg_pkb
  sta PKB_source+1
  lda #$FF
  sta doubleshoot_time
  
  ; turn off rendering and decompress to the first nametable
  lda #$20
  ldx #$00
  ldy #VBLANK_NMI
  stx PPUMASK
  sty PPUCTRL
  sta PPUADDR
  stx PPUADDR
  jsr PKB_unpackblk

  lda #0
  sta $100

loop:
  jsr basic_tests_next_frame

  ; Control the hue and brightness
  ldy #1
  lda #KEY_A
  and cur_keys
  bne is_held_a
  jsr hue_bright_control
  
  jmp after_control
is_held_a:
  jsr radius_control
after_control:

  lda cur_radius
  ora #%11000000
  ldx #128
  ldy #120
  jsr draw_ball

  lda #120
  sec
  sbc cur_radius
  pha
  tay
  lda cur_radius
  asl a
  ora #%11000000
  ldx #240
  jsr draw_paddle

  pla
  tay
  lda cur_radius
  asl a
  ora #%11000001
  ldx #16
  jsr draw_paddle

  lda cur_bright
  cmp #7
  bcc not_by_twos_under_7
  lda #7
not_by_twos_under_7:
  ora #$01
  sta cur_bright

  jsr s0wait
  bvc no_s0
  ldy #180
  jsr zapkernel_yonoff_ntsc
  lda 0
  sta 2
  lda 1
  sta 3

  ldx #17
:
  lda yhtr_msg,x
  sta $100,x
  dex
  bpl :-
  lda 2
  ldx #2
  jsr bcdout1xx
  lda 3
  ldx #9
  jsr bcdout1xx
  lda cur_radius
  ldx #14
  jsr bcdout1xx
  
  lda 2
  clc
  adc #16
  tay
  clc
  adc 3
  pha
  lda #$00
  ldx #8
  jsr draw_y_arrow_sprite
  pla
  tay
  lda #$01
  ldx #8
  jsr draw_y_arrow_sprite
no_s0:

  lda new_keys+0
  and #KEY_B
  bne bail
  jmp loop
bail:
  rts 
.endproc

.segment "RODATA"
yht_msg:  .byt "Y=    HT=   ",0
yhtr_msg: .byt "Y=    HT=    R=  ",0
y1y2_msg: .byt "Y1=    Y2=   ",0
xy_msg:   .byt "X=   Y=   ",0

.segment "CODE"
.proc bcdout1xx
  jsr bcd8bit
  pha
  lda 0
  beq no_tens
  lsr a
  lsr a
  lsr a
  lsr a
  beq no_hundreds
  ora #'0'
  sta $100,x
no_hundreds:
  lda 0
  and #$0F
  ora #'0'
  sta $101,x
no_tens:
  pla
  ora #'0'
  sta $102,x
  rts
.endproc

.proc basic_tests_next_frame

  ; Draw a sprite to let the player know it's still working, then
  ; clear unused display list entries.
;  jsr move_player
;  jsr draw_player_sprite
  ldx oam_used
  jsr ppu_clear_oam

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

  ; Compute the printable hue number
  ldx #' '
  lda cur_hue
  cmp #10
  bcc hue_not_10
  sbc #10
  ldx #'1'
hue_not_10:
  ora #'0'
  stx 0
  sta 1
  
  jsr calc_colors
  lda nmis
:
  cmp nmis
  beq :-
  lda PPUSTATUS
  lda #VBLANK_NMI
  sta PPUCTRL
  
  ; write current hue and brightness
  ldy #$23
  sty PPUADDR
  lda #$70
  sta PPUADDR
  lda 0
  sta PPUDATA
  lda 1
  sta PPUDATA
  sty PPUADDR
  lda #$69
  sta PPUADDR
  lda #'0'
  ora cur_bright
  sta PPUDATA

  ; set bg palette
  ldx #$3F
  stx PPUADDR
  lda #$00
  sta PPUADDR
  lda #COLOR_BLACK
  sta PPUDATA
  lda group1color
  sta PPUDATA
  lda group2color
  sta PPUDATA
  lda #COLOR_GREEN
  sta PPUDATA
  
  ; set ball palette
  stx PPUADDR
  lda #$1D
  sta PPUADDR
  lda group2color
  cmp #$30
  bcc :+
  sbc #$10
:
  sec
  sbc #$10
  bcs :+
  lda #$0F
:
  sta PPUDATA
  lda group2color
  sta PPUDATA
  
  ; write string at $2342
  lda #<$100
  sta 0
  lda #>$100
  sta 1
  lda #$23
  ldx #$42
  jsr puts

  lda #VBLANK_NMI|BG_0000|OBJ_1000
  sta PPUCTRL
  lda #0
  sta PPUSCROLL
  sta PPUSCROLL
  sta $100
  sta OAMADDR
  lda #>OAM
  sta OAM_DMA
  lda #BG_ON|OBJ_ON
  sta PPUMASK
  jsr read_pads

  ; If player 2's gun has shot twice in quick succession,
  ; cancel
  lda new_keys+1
  bpl not_triggered
  lda doubleshoot_time
  cmp #DOUBLESHOOT_FRAMES
  bcs triggered_once
  lda #KEY_B
  ora new_keys+0
  sta new_keys+0
triggered_once:
  lda #0
  sta doubleshoot_time
not_triggered:
  inc doubleshoot_time
  bne :+
  dec doubleshoot_time
:
  rts
.endproc

;;
; write the string at (0) to PPUDATA
; @param 0 pointer to string
; @param AX destination PPU address
.proc puts
  sta PPUADDR
  stx PPUADDR
  ldy #0
loop:
  lda (0),y
  beq bail
  sta PPUDATA
  iny
  bne loop
bail:
  rts
.endproc

;;
; @param y if nonzero, Down decreases brightness by 2
.proc hue_bright_control
  lda new_keys
  and #KEY_LEFT
  beq notLeft
  dec cur_hue
  bpl notLeft
  lda #12
  sta cur_hue
notLeft:

  lda new_keys
  and #KEY_RIGHT
  beq notRight
  inc cur_hue
  lda cur_hue
  cmp #13
  bcc notRight
  lda #0
  sta cur_hue
notRight:

  lda new_keys
  and #KEY_DOWN
  beq notDown
  cpy #0
  beq notDown2
  dec cur_bright
notDown2:
  dec cur_bright
  bpl notDown
  lda #0
  sta cur_bright
notDown:

  lda new_keys
  and #KEY_UP
  beq notUp
  inc cur_bright
  lda cur_bright
  cmp #8
  bcc notUp
  lda #8
  sta cur_bright
notUp:

  rts
.endproc

.proc radius_control
  lda new_keys
  and #KEY_DOWN
  beq notDown
  dec cur_radius
  beq downToZero
  bpl notDown
downToZero:
  lda #1
  sta cur_radius
notDown:

  lda new_keys
  and #KEY_UP
  beq notUp
  inc cur_radius
  lda cur_radius
  cmp #16
  bcc notUp
  lda #16
  sta cur_radius
notUp:

  rts
.endproc

;;
; Waits for sprite 0 hit.
; @return PPUSTATUS at end.  V is true iff sprite 0 was present.
.proc s0wait
wait_end:
  bit PPUSTATUS
  bvs wait_end
wait_start:
  bit PPUSTATUS
  bmi wait_bail
  bvc wait_start
wait_bail:
  rts
.endproc

;;
; Translates cur_bright and cur_hue into group1color and group2color.
.proc calc_colors
  lda cur_bright
  asl a
  asl a
  asl a
  pha
  and #$F0
  ora cur_hue
  cmp #$40
  bcc not_overbright
  lda #$30
not_overbright:
  sta group1color

  pla
  sec
  sbc #8
  and #$F0
  ora cur_hue
  bpl not_underblack
  lda #$0F
not_underblack:
  sta group2color
  rts
.endproc

.proc test_trigger_time
  lda #<pulltrigger_pkb
  sta PKB_source+0
  lda #>pulltrigger_pkb
  sta PKB_source+1
  
  ; turn off rendering and decompress to the first nametable
  lda #$20
  ldx #$00
  ldy #VBLANK_NMI
  stx PPUMASK
  sty PPUCTRL
  sta PPUADDR
  stx PPUADDR
  jsr PKB_unpackblk
  ; set bg palette
  ldx #$3F
  stx PPUADDR
  ldy #$00
  sty PPUADDR
  sty held_time
  lda #COLOR_BLACK
  sta PPUDATA
  lda #COLOR_GREEN
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA

loop:
  
  ; Compute the printable hue number
  lda held_time
  jsr bcd8bit
  ora #'0'
  sta 1
  lda 0
  beq under_ten
  ora #'0'
under_ten:
  sta 0

  lda nmis
:
  cmp nmis
  beq :-
  
  ; write current time
  ldy #$23
  sty PPUADDR
  lda #$6E
  sta PPUADDR
  lda 0
  sta PPUDATA
  lda 1
  sta PPUDATA

  lda #VBLANK_NMI|BG_0000|OBJ_1000
  sta PPUCTRL
  lda #0
  sta PPUSCROLL
  sta PPUSCROLL
  lda #BG_ON
  sta PPUMASK

  ; control the time
  jsr read_pads
  lda cur_keys+1
  and #KEY_A
  sta cur_trigger
  lda new_keys+1
  and #KEY_A
  sta new_trigger

  beq no_reset_held_time
  lda #0
  sta held_time
no_reset_held_time:

  lda held_time
  cmp #99
  bcs no_inc_trigger
  lda cur_trigger
  beq no_inc_trigger
  inc held_time
no_inc_trigger:

  lda new_keys+0
  and #KEY_B
  beq loop
  rts 
.endproc


