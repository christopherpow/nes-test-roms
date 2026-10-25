;
; Tennis game
; Copyright 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "tennis.inc"

PADDLE_ACCEL = 100
PADDLE_MAX_SPEED = 8
MAX_SERVE_TIME = 300
; reduce threshold during debug
WIN_THRESHOLD = 21
WINBY2_THRESHOLD = 11

.segment "ZEROPAGE"
paddle_ylo: .res 2
paddle_yhi: .res 2
paddle_dylo: .res 2
paddle_dyhi: .res 2
ball_xlo: .res 1
ball_xhi: .res 1
ball_ylo: .res 1
ball_yhi: .res 1
ball_dxlo: .res 1
ball_dxhi: .res 1
ball_dylo: .res 1
ball_dyhi: .res 1
ball_speed: .res 1
ball_peak_speed: .res 1
ball_ypred: .res 1
paddle_ypred: .res 2
player_score: .res 2
player_joined: .res 2
using_gun: .res 2  ; nonzero if using Zapper
serve_turn: .res 1  ; 0, 1: Podge; 2, 3: Daffle
state_timer_hi: .res 1
dirty: .res 1

.segment "CODE"
.proc tennis
  jsr tennis_title
  lda player_joined+0
  ora player_joined+1
  bne playersHaveJoined
  rts
playersHaveJoined:
  jsr tennis_game
  jmp tennis
.endproc

; GAME LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.proc tennis_game
  lda #VBLANK_NMI
  sta PPUCTRL
  ldx #$00
  stx PPUMASK

  jsr tennis_load_bg
  jsr tennis_new_game
  
  ldx #240
  lda #0
  sta using_gun+0
  sta using_gun+1
  jsr tennis_draw_score_digits
  jsr tennis_tip_serve

gameloop:
  jsr tennis_draw_sprites
  lda nmis
:
  cmp nmis
  beq :-

  jsr tennis_blit_something
  
  ; turn rendering back on
  ldx #0
  stx OAMADDR
  lda #>OAM
  sta OAM_DMA
  ldy #0
  lda #VBLANK_NMI|BG_0000|OBJ_1000
  sec
  jsr ppu_screen_on

  ; A gun not plugged in will show up as a constantly on photosensor.
  ; So if the photosensor is on ($401x.D3 is false) right in the
  ; middle of vblank, assume a gun isn't plugged in.
  lda $4016
  and #$08
  sta using_gun+0
  lda $4017
  and #$08
  sta using_gun+1
  jsr read_pads
  jsr pently_update

  ; moving paddles must happen after the screen turns on
  ; because the positions are read from the screen
  jsr tennis_move_paddles

  ; moving the ball happens below moving the paddles because there's
  ; more time there
  jsr tennis_move_ball
  jsr tennis_move_ball
  jsr tennis_state_dispatch

  lda game_state
  bmi done
  jmp gameloop
done:
  rts
.endproc

.proc tennis_new_game
  lda #BALL_START_SPEED
  sta ball_peak_speed
  sta ball_speed
  lda #192/2+16-8
  sta paddle_yhi+0
  sta paddle_yhi+1
  ldx #1
  lda #0
:
  sta player_score,x
  sta paddle_ylo,x
  sta paddle_dylo,x
  sta paddle_dyhi,x
  dex
  bpl :-
  jmp choose_first_serve
.endproc

;;
; The parity of nmis at game start determines who will serve first.
.proc choose_first_serve

  ; Get parity in bit 7
  ; per http://forum.6502.org/viewtopic.php?p=4354#4354
  lda nmis
  asl a
  eor nmis
  and #%10101010 
  adc #%01100110
  and #%10001000
  adc #%01111000

  ; Get parity into bit 1
  asl a
  lda #0
  rol a
  rol a
  
  sta serve_turn
  ; fall through
.endproc
.proc tennis_set_serve_time
  lda #STATE_SERVE
  sta game_state
  lda #<-MAX_SERVE_TIME
  sta state_timer
  lda #>-MAX_SERVE_TIME
  sta state_timer_hi
  
  ; if AI, randomize the serve time
  lda serve_turn
  lsr a
  tax
  lda player_joined,x
  bne not_ai
  lda nmis
  asl a
  asl a
  asl a
  asl a
  eor nmis
  adc state_timer
  sta state_timer
  bcc not_ai
  inc state_timer_hi
not_ai:
  rts
.endproc

.proc tennis_state_dispatch
  ldx game_state
  lda handlers+1,x
  pha
  lda handlers,x
  pha
  rts
handlers:
  .addr tennis_state_serve-1
  .addr tennis_state_active-1
  .addr tennis_state_win_point-1
  .addr tennis_state_win_game-1
.endproc

.proc tennis_state_serve
  lda serve_turn
  and #%00000010
  lsr a
  tax
  lda #0
  sta ball_xlo
  sta ball_ylo
  lda service_x,x
  sta ball_xhi
  lda paddle_yhi,x
  clc
  adc #PADDLE_LENGTH / 2
  sta ball_yhi
  
  ; Move AI paddles up and down
  lda nmis
  and #%00100000
  beq pred_up_phase
  lda #(TOP_WALL+BOTTOM_WALL)/2
pred_up_phase:
  clc
  adc #(TOP_WALL+BOTTOM_WALL)/4
  sta ball_ypred

  ; Serve automatically after serve timer expires
  inc state_timer
  bne no_autoserve
  inc state_timer_hi
  beq launch
no_autoserve:

  ; allow human players (only) to serve by pressing a button
  lda player_joined,x
  beq no_launch
  lda new_keys,x
  and #KEY_A
  bne launch
no_launch:

  sta ball_dxlo
  sta ball_dylo
  sta ball_dxhi
  sta ball_dyhi
  rts
launch:
  lda #STATE_ACTIVE
  sta game_state
  
  ; If above the center line, aim 22.5 deg down; otherwise, aim
  ; 22.5 deg up.
  lda #2
  ldx ball_yhi
  cpx #(TOP_WALL+BOTTOM_WALL)/2
  bcc above_centerline
  lda #<-2
above_centerline:
  jsr tennis_set_ball_vel
  jmp tennis_hide_tip

.segment "RODATA"
service_x: .byt SERVE_1P_X, SERVE_2P_X
.segment "CODE"
.endproc

.proc tennis_state_active
  rts
.endproc

.proc tennis_state_win_point
  lda #$F8
  sta ball_yhi
  dec state_timer
  bne not_done
  jmp tennis_set_serve_time
not_done:
  lda state_timer
  cmp #50
  bne not_msg
  jsr tennis_tip_serve
not_msg:
  rts
.endproc

.proc tennis_state_win_game
  lda nmis
  and #$01
  bne not_done

  dec state_timer
  bne not_done
  lda #$FF
  sta game_state
not_done:
  rts
.endproc

; BALL PHYSICS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; Shoot the ball from where it is, at the current ball speed.
; @param A the angle (-7: up; 0: straight; 7: down)
.proc tennis_set_ball_vel
angle = 2
absangle = 3
  cmp #0
  bne not_straight
  
  tax
  lda ball_speed
  sta ball_dxlo
  lda #0
  sta ball_dylo
  sta ball_dyhi
  
  ; now convert speed from 4.4 to 8.8
  .repeat 4
    asl ball_dxlo
    rol a
  .endrepeat
  sta ball_dxhi
  jmp flip_x_toward_center
  
not_straight:
  sta angle
  cmp #$80
  bcc absangle_is_pos
  eor #$FF
  adc #0
absangle_is_pos:

  ; Y = absolute value of angle (1-7)
  cmp #7
  bcc not_over_7
  lda #7
not_over_7:
  tax  ; X = absolute value of angle
  
  ; compute the vertical component
  lda sine256Q1,x
  ldy ball_speed
  jsr mul8
  sta ball_dyhi
  lda 0
  .repeat 4
    lsr ball_dyhi
    ror a
  .endrepeat
  sta ball_dylo
  bit angle
  bpl no_flip_y
  sec
  lda #0
  sbc ball_dylo
  sta ball_dylo
  lda #0
  sbc ball_dyhi
  sta ball_dyhi
no_flip_y:
  
  ; compute the horizontal component
  lda cosine256Q1,x
  ldy ball_speed
  jsr mul8
  sta ball_dxhi
  lda 0
  .repeat 4
    lsr ball_dxhi
    ror a
  .endrepeat
  sta ball_dxlo
flip_x_toward_center:
  bit ball_xhi
  bpl no_flip_x
  sec
  lda #0
  sbc ball_dxlo
  sta ball_dxlo
  lda #0
  sbc ball_dxhi
  sta ball_dxhi
no_flip_x:

  ; The ball velocity is final.
  jsr tennis_update_ball_ypred
  lda #4
  jmp pently_start_sound
.endproc

;;
; Calculates where the ball will land if it is traveling toward an AI
; player.
.proc tennis_update_ball_ypred
  ; If the new velocity is headed toward an AI player,
  ; position the paddles as such.
  ldx #0
  bit ball_dxhi
  bmi is_ai_2p
  inx
is_ai_2p:
  lda player_joined,x
  bne is_joined
  jsr tennis_compute_target_y
  jmp have_ai_pred
is_joined:
  lda #<(TOP_WALL+BOTTOM_WALL-PADDLE_LENGTH)/2
have_ai_pred:
  sta ball_ypred
  rts
.endproc

.proc tennis_move_ball

  ; If the ball fell off and put the game into win point state,
  ; don't move the ball and cause a repeated state transition
  lda game_state
  cmp #STATE_ACTIVE
  beq state_ok
  rts
state_ok:

  clc
  lda ball_dxlo
  adc ball_xlo
  sta ball_xlo
  lda ball_dxhi
  adc ball_xhi
  sta ball_xhi

  ; If the carry bit doesn't match the upper bit of dxhi, the ball
  ; wrapped around.
  ror a
  eor ball_dxhi
  bmi fall_off
  lda ball_xhi
  cmp #256-BALL_RADIUS
  bcs fall_off
  cmp #BALL_RADIUS
  bcs no_fall_off
fall_off:
  jmp tennis_point_won

no_fall_off:
  clc
  lda ball_dylo
  adc ball_ylo
  sta ball_ylo
  lda ball_dyhi
  adc ball_yhi
  sta ball_yhi

  bit ball_dyhi
  bpl bounce_bottom_instead

  ; bounce off top wall
  lda ball_yhi
  cmp #TOP_WALL+BALL_RADIUS
  bcs not_bounce_off_wall
  ldy #2*(TOP_WALL+BALL_RADIUS)
  bne reflect_yvel

bounce_bottom_instead:
  lda ball_yhi
  cmp #BOTTOM_WALL-BALL_RADIUS
  bcc not_bounce_off_wall
  ldy #<(2*(BOTTOM_WALL-BALL_RADIUS))
reflect_yvel:
  ; Reflect the Y velocity
  sec
  lda #0
  sbc ball_dylo
  sta ball_dylo
  lda #0
  sbc ball_dyhi
  sta ball_dyhi

  ; Reflect the Y position
  sec
  lda #0
  sbc ball_ylo
  sta ball_ylo
  tya
  sbc ball_yhi
  sta ball_yhi

  jsr tennis_update_ball_ypred
  lda #3
  jsr pently_start_sound
not_bounce_off_wall:

  ;fall through: jmp tennis_check_collision
.endproc

;;
; Decides whether the ball is overlapping the paddle and calculates
; a rebound angle.
.proc tennis_check_collision
xdist = 0
ydist = 1

  lda game_state
  cmp #STATE_ACTIVE
  bne reject_1

  ; decide which paddle to check for collision
  lda #<-PADDLE_1P_X
  ldx #0
  bit ball_dxhi
  bmi is_1p_1
  lda #<-PADDLE_2P_X
  ldx #1
is_1p_1:
  clc
  adc ball_xhi
  
  ; If checking player 2, mirror the ball position relative to the
  ; paddle so that angle code need only consider player 1
  bit ball_dxhi
  bmi is_1p_2
  eor #$FF
  clc
  adc #1
is_1p_2:
  sta xdist
  
  ; Trivial rejection: x distances 2r+1 through 255-2r mean no collision
  cmp #256 - 2 * BALL_RADIUS
  bcs x_accept
  cmp #1 + 2 * BALL_RADIUS
  bcs reject_1
  
x_accept:
  sec
  lda ball_yhi
  sbc paddle_yhi,x
  
  ; Trivial rejection: y distances 2r+len+1 through 255-2r mean no collision
  cmp #256 - 2 * BALL_RADIUS
  bcs y_accept
  cmp #1 + PADDLE_LENGTH + 2 * BALL_RADIUS
  bcc y_accept
reject_1:
  rts
y_accept:

  ; And here's where it forks.  Separate formulas are used for the
  ; front of the paddle and the side of the paddle.
  cmp #PADDLE_LENGTH + 1
  bcs hit_paddle_side
  
  ; hit top half: -4 through -1
  ; hit bottom half: 1 through 4
  cmp #PADDLE_LENGTH/2
  sbc #PADDLE_LENGTH/2  ; now -8...-1 or 1...9
  cmp #$80
  ror a  ; now -4...-1 or 0...4
  bne have_angle
  lda #1
have_angle:
  ; At this point, we know we have a collision, and we know its
  ; angle.  So set the velocity based on this angle, play a sound,
  ; calculate where the ball will end up, and change the speed for
  ; the next play.
  jsr tennis_set_ball_vel
  jmp tennis_inc_speed

hit_paddle_side:
  cmp #$80
  bcs d2yabove_1
  sbc #PADDLE_LENGTH
d2yabove_1:
  sta ydist
  
  ; Now test against the endcaps.
  cmp #$80
  bcc d2ynotneg
  eor #$FF
  adc #0
d2ynotneg:
  tay

  lda xdist
  cmp #$80
  bcc d2xnotneg
  eor #$FF
  adc #0
d2xnotneg:
  tax

  ; Reject where the distance from the ball center to the endcap
  ; center exceeds the square of the sum of their radii.
  clc
  lda dsquared,x
  adc dsquared,y
  cmp #BALL_RADIUS*BALL_RADIUS*4
  bcs reject_1
  
  lda #6     ; if ball is behind paddle, use 6
  bit xdist
  bmi know_angle
  cpy xdist  ; if ydist > xdist, use 6;  otherwise, use 5
  bcs know_angle
  lda #5
know_angle:
  bit ydist
  bpl no_invert_angle
  eor #$FF
  clc
  adc #1
no_invert_angle:
  jmp have_angle

.pushseg
.segment "RODATA"
dsquared:
  .repeat 15,I
    .byt I*I
  .endrepeat
.popseg
.endproc

.proc tennis_inc_speed
  lda ball_speed
  cmp ball_peak_speed
  bcs increasing_speed
  
  ; increase speed by 1/32 of peak speed
  lda ball_peak_speed
  lsr a
  lsr a
  lsr a
  lsr a
  lsr a
  
  ; if peak is too low, don't let it get stuck rounding to 0
  bne is_nonzero
  sec
is_nonzero:
  adc ball_speed
  
  ; don't let it increase past peak
  cmp ball_peak_speed
  bcc not_increase_too_far
  lda ball_peak_speed
not_increase_too_far:
  sta ball_speed
  rts
  
increasing_speed:
  lda ball_speed
  adc #BALL_INCREASE_SPEED-1  ; carry is set!
  bcs exceeded_max
  cmp #BALL_MAX_SPEED
  bcc not_exceeded_max
exceeded_max:
  lda #BALL_MAX_SPEED
not_exceeded_max:
  sta ball_speed
  sta ball_peak_speed
  rts
.endproc

.proc tennis_point_won
  inc serve_turn
  lda serve_turn
  and #%00000011
  sta serve_turn
  
  ; cool off
  ; TO DO: unblock cool off once inc_speed is ready
  lda ball_speed
  lsr a
  adc ball_speed
  ror a
  cmp #BALL_START_SPEED
  bcs no_clamp_speed
  lda #BALL_START_SPEED
no_clamp_speed:
  sta ball_speed

  ; figure out who won the point
  lda ball_dxhi
  asl a
  lda #0
  rol a
  tax
  inc player_score,x

  ; At this point, it's based on how may points you have compared
  ; to the other player.  Get the other player number in Y.
  txa
  eor #$01
  tay

  ; Tiebreaker rule: If you reach the tiebreaker win threshold, you
  ; win the game.  If you're more than 1 less, don't apply the
  ; tiebreaker rule.
  lda player_score,x
  cmp #WIN_THRESHOLD-1
  bcc not_tiebreaker_game_point
  bne is_win

  ; At 1 less than tiebreaker, it's game point if you're tied.
  ; Otherwise handle it like any ordinary score.
  cmp player_score,y
  beq is_game_point

not_tiebreaker_game_point:
  ; If you have not reached one less than the win by 2 threshold,
  ; you need more than 1.
  cmp #WINBY2_THRESHOLD-1
  bcc not_game_point

  ; If the other player's score is greater than or equal to yours,
  ; you need more than 1.
  ; Use clc/sbc to compute yours-theirs-1.  
  clc
  lda player_score,x
  sbc player_score,y
  bcc not_game_point
  beq is_game_point
  lda player_score,x
  cmp #WINBY2_THRESHOLD
  bcc is_game_point
  
is_win:
  jsr tennis_tip_winner_player_x
  lda #150
  sta state_timer
  lda #2
  jsr pently_start_music
  lda #STATE_WIN_GAME
  sta game_state
  jmp tennis_draw_score_digits
is_game_point:
  jsr tennis_tip_game_point_player_x
  jmp tip_drawn
not_game_point:  
  jsr tennis_tip_point_player_x
tip_drawn:
  lda #150
  sta state_timer
  lda #1
  jsr pently_start_music
  lda #STATE_WIN_POINT
  sta game_state
  jmp tennis_draw_score_digits

.endproc


; INPUT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.proc tennis_move_paddles

  ; Reading the guns is very time consuming.  Skip it if needed.
  lda using_gun+0
  ora using_gun+1
  beq nobody_using_guns
  jsr s0wait
  ldy #BOTTOM_WALL-TOP_WALL-PADDLE_LENGTH
  jsr zapkernel_yon2p_ntsc
  ldx #1
guns_loop:
  lda using_gun,x
  beq not_using_gun
  lda player_joined,x
  beq not_using_gun
  lda 0,x
  cmp #BOTTOM_WALL-TOP_WALL-PADDLE_LENGTH
  bcs not_using_gun
  clc
  adc #TOP_WALL
  sta paddle_yhi,x
not_using_gun:
  dex
  bpl guns_loop
nobody_using_guns:

  ldx #1
controllers_loop:
  lda player_joined,x
  beq using_ai_controller
  lda using_gun,x
  bne not_using_controller
  lda cur_keys,x
  lsr a
  lsr a
  jmp have_cur_keys
using_ai_controller:
  lda ball_ypred
  sec
  sbc paddle_ypred,x
  bcc ai_use_carry
  cmp #PADDLE_LENGTH/4
  bcc ai_use_carry
  cmp #PADDLE_LENGTH*3/4
  bcs ai_use_carry
  lda #0
  beq have_cur_keys
ai_use_carry:
  lda #0
  adc #1
  eor #3
have_cur_keys:
  sta abl_keys
  lda paddle_dylo,x
  sta abl_vel
  lda paddle_dyhi,x
  sta abl_vel+1
  lda #0
  sta abl_maxVel+0
  lda #PADDLE_MAX_SPEED
  sta abl_maxVel+1
  lda #PADDLE_ACCEL
  sta abl_accelRate
  .if ::PADDLE_ACCEL >= 128
    lda #255
  .else
    asl a
  .endif
  sta abl_brakeRate
  jsr accelBrakeLimit
  clc
  lda abl_vel
  sta paddle_dylo,x
  adc paddle_ylo,x
  sta paddle_ylo,x
  lda abl_vel+1
  sta paddle_dyhi,x
  adc paddle_yhi,x
  sta paddle_yhi,x
  jsr tennis_predict_paddle_motion
  sta paddle_ypred,x

not_using_controller:
  dex
  bpl controllers_loop

  ; clip both paddles
  ldy #0
  ldx #1
clip_loop:
  lda paddle_yhi,x
  cmp #TOP_WALL+BALL_RADIUS
  bcs nocliptop
  lda #TOP_WALL+BALL_RADIUS
  sty paddle_dyhi,x
  sty paddle_dylo,x
  sty paddle_ylo,x
nocliptop:
  cmp #BOTTOM_WALL - PADDLE_LENGTH - BALL_RADIUS + 1
  bcc noclipbottom
  lda #BOTTOM_WALL - PADDLE_LENGTH - BALL_RADIUS
  sty paddle_dyhi,x
  sty paddle_dylo,x
  sty paddle_ylo,x
noclipbottom:
  sta paddle_yhi,x
  dex
  bpl clip_loop

  rts
.endproc

;;
; Makes a rough guess at where the paddle will end up if the player
; lets go of the Control Pad.
; @param X player number
; @return Y position in A
.proc tennis_predict_paddle_motion
  ldy paddle_dyhi,x
  bmi predict_up
  cpy #7
  bcc down_noclip7
  ldy #7
  clc
down_noclip7:
  lda paddle_yhi,x
  adc predfactors,y
  bcs down_clipbottom
  cmp #BOTTOM_WALL-BALL_RADIUS-PADDLE_LENGTH
  bcc have_y
down_clipbottom:
  lda #BOTTOM_WALL-BALL_RADIUS-PADDLE_LENGTH
  bne have_y
  
predict_up:
  tya
  eor #$FF
  tay
  cpy #7
  bcc up_noclip7
  ldy #7
up_noclip7:
  sec
  lda paddle_yhi,x
  sbc predfactors,y
  bcc up_cliptop
  cmp #TOP_WALL+BALL_RADIUS
  bcs have_y
up_cliptop:
  lda #TOP_WALL+BALL_RADIUS
have_y:
  rts

.pushseg
.segment "RODATA"
predfactors: .byt 0, 2, 5, 10, 15, 22, 30, 40
.popseg
.endproc

