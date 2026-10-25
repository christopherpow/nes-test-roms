;
; Zapper demo for NES
; Copyright 2012 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;

.include "nes.inc"
.include "global.inc"

.import axe_callback_on
.export pently_zp_state

.segment "ZEROPAGE"
nmis:          .res 1
oam_used:      .res 1  ; starts at 0
cur_keys:      .res 2
new_keys:      .res 2

; Game variables
player_xlo:       .res 1  ; horizontal position is xhi + xlo/256 px
player_xhi:       .res 1
player_dxlo:      .res 1  ; speed in pixels per 256 s
player_yhi:       .res 1
player_facing:    .res 1
player_frame:     .res 1
player_frame_sub: .res 1

; Used by music engine
pently_zp_state: .res 32


.segment "INESHDR"
  .byt "NES",$1A  ; magic signature
  .byt 1          ; PRG ROM size in 16384 byte units
  .byt 1          ; CHR ROM size in 8192 byte units
  .byt $01        ; mirroring type and mapper number lower nibble
  .byt $00        ; mapper number upper nibble

FOR_ACTION53 = 1

.segment "VECTORS"
.proc allvectors
e0:
.if ::FOR_ACTION53
  jmp tennis_only
  jmp axe_only
  jmp calibration_only
.endif
  .res e0+$1A-*
fa:
.addr nmi, reset, irq
.endproc

.segment "CODE"
;;
; Notify the main thread that NMI has occurred.  (There aren't any
; scroll splits or the like that'd need a more complex NMI handler.)
.proc nmi
  inc nmis
  rti
.endproc

; IRQ handler that does nothing so that BRK $00 can be a breakpoint
.proc irq
  rti
.endproc

.if ::FOR_ACTION53
; Ordinarily I put it at $07FF, but MineShaft has some sort of
; stack at $07FF. Fortunately, it only ever uses about 16 bytes
; during gameplay, so I'll allow 32.
mineshaft_zapruder_nmi_switch = $07DF
.proc tennis_only
  sei
  ldx #$FF
  txs
  stx mineshaft_zapruder_nmi_switch
  inx
  stx axe_callback_on
  jsr pently_init
  jsr tennis
  jmp ($FFFC)
.endproc

.proc axe_only
  sei
  ldx #$FF
  txs
  stx mineshaft_zapruder_nmi_switch
  inx
  stx axe_callback_on
  jsr pently_init
  jsr axe
  jmp ($FFFC)
.endproc

.proc calibration_only
  sei
  ldx #$FF
  txs
  stx mineshaft_zapruder_nmi_switch
  inx
  stx cur_hue
  stx PPUCTRL
  stx PPUMASK
  lda #6
  sta cur_bright
  lda #$3F
  sta PPUADDR
  stx PPUADDR
palloop:
  lda initial_palette,x
  sta PPUDATA
  inx
  cpx #32
  bcc palloop

  jsr pently_init
  jsr test_fullbright_yonoff
  jmp ($FFFC)
.endproc
.endif

.proc reset
  ; Put all sources of interrupts into a known state.
  sei             ; Disable interrupts
  ldx #$00
  stx PPUCTRL     ; Disable NMI and set VRAM increment to 32
  stx PPUMASK     ; Disable rendering
  stx $4010       ; Disable DMC IRQ
  dex             ; Subtracting 1 from $00 gives $FF, which is a
  txs             ; quick way to set the stack pointer to $01FF
  bit PPUSTATUS   ; Acknowledge stray vblank NMI across reset
  bit SNDCHN      ; Acknowledge DMC IRQ
  lda #$40
  sta P2          ; Disable APU Frame IRQ
  lda #$0F
  sta SNDCHN      ; Disable DMC playback, initialize other channels

vwait1:
  bit PPUSTATUS   ; It takes one full frame for the PPU to become
  bpl vwait1      ; stable.  Wait for the first frame's vblank.

  ; We have about 29700 cycles to burn until the second frame's
  ; vblank.  Use this time to get most of the rest of the chipset
  ; into a known state.

  cld  ; just in case running on a famiclone with working decimal hw
  
  ; jmp calibration_only

  ; Clear OAM and the zero page here.
  ldx #0
  jsr ppu_clear_oam  ; clear out OAM from X to end and set X to 0
  txa
clear_zp:
  sta $00,x
  inx
  bne clear_zp
  jsr pently_init

vwait2:
  bit PPUSTATUS  ; After the second vblank, we know the PPU has
  bpl vwait2     ; fully stabilized.
  
  ; There are two ways to wait for vertical blanking: spinning on
  ; bit 7 of PPUSTATUS (as seen above) and waiting for the NMI
  ; handler to run.  Before the PPU has stabilized, you want to use
  ; the PPUSTATUS method because NMI might not be reliable.  But
  ; afterward, you want to use the NMI method because if you read
  ; PPUSTATUS at the exact moment that the bit turns on, it'll flip
  ; from off to on to off faster than the CPU can see.

  ; Now the PPU has stabilized, we're still in vblank.  Copy the
  ; palette right now because if you load a palette during forced
  ; blank (not vblank), it'll be visible as a rainbow streak.

  ; Set up game variables, as if it were the start of a new level.
  lda #0
  sta player_xlo
  sta player_dxlo
  sta player_facing
  sta player_frame
  sta axe_callback_on
  sta cur_hue
  sta axe_callback_on
  lda #6
  sta cur_bright
  lda #4
  sta cur_radius
  lda #48
  sta player_xhi
  lda #199
  sta player_yhi
  
  jsr title_screen

forever:
  jsr pointat_menu
  jsr menu_dispatch
  jmp forever
.endproc

.proc menu_dispatch
  asl a
  tax
  lda menu_item_procs+1,x
  pha
  lda menu_item_procs,x
  pha
  rts

nothing:
  rts
.pushseg
.segment "RODATA"
menu_item_procs:
  .addr test_fullbright_yonoff-1
  .addr test_fullbright_yon2p-1
  .addr test_fullbright_xyon-1
  .addr test_ball_yonoff-1
  .addr test_vlines_yonoff-1
  .addr test_hlines_yonoff-1
  .addr test_trigger_time-1
  .addr axe-1
  .addr tennis-1
  .addr nothing-1
.popseg
.endproc

; constants used by move_player
; PAL frames are about 20% longer than NTSC frames.  So if you make
; dual NTSC and PAL versions, or you auto-adapt to the TV system,
; you'll want PAL velocity values to be 1.2 times the corresponding
; NTSC values, and PAL accelerations should be 1.44 times NTSC.
WALK_SPD = 85   ; speed limit in 1/256 px/frame
WALK_ACCEL = 4  ; movement acceleration in 1/256 px/frame^2
WALK_BRAKE = 8  ; stopping acceleration in 1/256 px/frame^2

.proc move_player

  lda cur_keys
  sta abl_keys
  lda #WALK_SPD
  sta abl_maxVel
  ldy #$00
  sty abl_maxVel+1
  lda player_dxlo
  bpl :+
  dey
:
  sta abl_vel
  sty abl_vel+1
  lda #WALK_ACCEL
  sta abl_accelRate
  lda #WALK_BRAKE
  sta abl_brakeRate
  jsr accelBrakeLimit

  ; Write back facing direction based on velocity
  lda abl_vel
  sta player_dxlo
  beq noChangeFacing
  bpl rightFacing
  lda player_facing
  ora #$40
  bne writebackFacing
rightFacing:
  lda player_facing
  and #<~$40
writebackFacing:
  sta player_facing
noChangeFacing:

  ; In a real game, you'd respond to A, B, Up, Down, etc. here.

  ; Move the player by adding the velocity to the 16-bit X position.
  lda player_dxlo
  bpl player_dxlo_pos
  ; if velocity is negative, subtract 1 from high byte to sign extend
  dec player_xhi
player_dxlo_pos:
  clc
  adc player_xlo
  sta player_xlo
  lda #0          ; add high byte
  adc player_xhi
  sta player_xhi

  ; Test for collision with side walls
  cmp #28
  bcs notHitLeft
  lda #28
  sta player_xhi
  lda #0
  sta player_dxlo
  beq doneWallCollision
notHitLeft:
  cmp #212
  bcc notHitRight
  lda #211
  sta player_xhi
  lda #0
  sta player_dxlo
notHitRight:
doneWallCollision:
  
  ; Animate the player
  ; If stopped, freeze the animation on frame 0 or 1
  lda player_dxlo
  bne notStop1
  lda #$80
  sta player_frame_sub
  lda player_frame
  cmp #2
  bcc have_player_frame
  lda #0
  beq have_player_frame
notStop1:

  ; Take absolute value of velocity (negate it if it's negative)
  bpl player_animate_noneg
  eor #$FF
  clc
  adc #1
player_animate_noneg:

  lsr a  ; Multiply abs(velocity) by 5/16
  lsr a
  sta 0
  lsr a
  lsr a
  adc 0

  ; And 16-bit add it to player_frame, mod $600  
  adc player_frame_sub
  sta player_frame_sub
  lda player_frame
  adc #0
  cmp #6
  bcc have_player_frame
  lda #0
have_player_frame:
  sta player_frame

  rts
.endproc

;;
; Draws the player's character to the display list as six sprites.
; In the template, we don't need to handle half-offscreen actors,
; but a scrolling game will need to "clip" sprites (skip drawing the
; parts that are offscreen).
.proc draw_player_sprite
draw_y = 0
cur_tile = 1
x_add = 2         ; +8 when not flipped; -8 when flipped
draw_x = 3
rows_left = 4
row_first_tile = 5
draw_x_left = 7

  lda #3
  sta rows_left
  
  ; In platform games, the Y position is often understood as the
  ; bottom of a character because that makes certain things related
  ; to platform collision easier to reason about.  Here, the
  ; character is 24 pixels tall, and player_yhi is the bottom.
  ; On the NES, sprites are drawn one scanline lower than the Y
  ; coordinate in the OAM entry (e.g. the top row of pixels of a
  ; sprite with Y=8 is on scanline 9).  But in a platformer, it's
  ; also common practice to overlap the bottom row of a sprite's
  ; pixels with the top pixel of the background platform that they
  ; walk on to suggest depth in the background.
  lda player_yhi
  sec
  sbc #24
  sta draw_y

  ; set up increment amounts based on flip value
  ; A: distance to move the pen (8 or -8)
  ; X: relative X position of first OAM entry
  lda player_xhi
  ldx #8
  bit player_facing
  bvc not_flipped
  clc
  adc #8
  ldx #(256-8)
not_flipped:
  sta draw_x_left
  stx x_add

  ; the six frames start at $10, $12, ..., $1A  
  lda player_frame
  asl a
  ora #$10
  sta row_first_tile

  ldx oam_used
rowloop:
  ldy #2              ; Y: remaining width on this row in 8px units
  lda row_first_tile
  sta cur_tile
  lda draw_x_left
  sta draw_x
tileloop:

  ; draw an 8x8 pixel chunk of the character using one entry in the
  ; display list
  lda draw_y
  sta OAM,x
  lda cur_tile
  inc cur_tile
  sta OAM+1,x
  lda player_facing
  sta OAM+2,x
  lda draw_x
  sta OAM+3,x
  clc
  adc x_add
  sta draw_x
  
  ; move to the next entry of the display list
  inx
  inx
  inx
  inx
  dey
  bne tileloop

  ; move to the next row, which is 8 scanlines down and on the next
  ; row of tiles in the pattern table
  lda draw_y
  clc
  adc #8
  sta draw_y
  lda row_first_tile
  clc
  adc #16
  sta row_first_tile
  dec rows_left
  bne rowloop

  stx oam_used
  rts
.endproc

;;
; @param Y vertical position
; @param X horizontal position
; @param A bit 7 on: draw right
.proc draw_y_arrow_sprite
ypos = 0
tilepos = 1
flip = 2
xpos = 3

  stx xpos
  ldx oam_used
  sta OAM+2,x
  tya
  sec
  sbc #4
  sta OAM,x
  lda #$06
  sta OAM+1,x
  lda xpos
  sta OAM+3,x
  txa
  clc
  adc #4
  sta oam_used
  rts
.endproc

;;
; @param Y vertical position
; @param X horizontal position
; @param A bit 6 on: draw right
.proc draw_x_arrow_sprite
ypos = 0
tilepos = 1
flip = 2
xpos = 3
  cpx #3
  bcs nope
  ldx #3
nope:

  stx xpos
  ldx oam_used
  sta OAM+2,x
  dey
  tya
  sta OAM,x
  lda #$07
  sta OAM+1,x
  lda xpos
  sec
  sbc #3
  sta OAM+3,x
  txa
  clc
  adc #4
  sta oam_used
  rts
.endproc


.segment "CHR"
.incbin "obj/nes/bggfx.chr"
.incbin "obj/nes/spritegfx.chr"
