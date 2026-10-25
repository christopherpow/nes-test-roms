;
; kinematics for a tennis game
; Copyright 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "tennis.inc"

;; 
; Applies acceleration, braking, and speed limit.
; XY untouched.
.proc accelBrakeLimit
  lsr abl_keys
  bcc notAccelRight

  ; if traveling to left, brake instead
  lda abl_vel+1
  bmi notAccelRight
  
  ; Case 1: nonnegative velocity, accelerating positive
  clc
  lda abl_accelRate
  adc abl_vel
  sta abl_vel
  lda #0
  adc abl_vel+1
  sta abl_vel+1
  
  ; clamp maximum velocity
  lda abl_vel
  cmp abl_maxVel
  lda abl_vel+1
  sbc abl_maxVel+1
  bcc notOverPosLimit
  lda abl_maxVel
  sta abl_vel
  lda abl_maxVel+1
  sta abl_vel+1
notOverPosLimit:
  rts
notAccelRight:

  lsr abl_keys
  bcc notAccelLeft
  ; if traveling to right, brake instead
  lda abl_vel+1
  bmi isAccelLeft
  ora abl_vel
  bne notAccelLeft
isAccelLeft:

  ; Case 2: nonpositive velocity, accelerating negative
  ;sec  ; already guaranteed set from bcc statement above
  lda abl_accelRate
  eor #$FF
  adc abl_vel
  sta abl_vel
  lda #$FF
  adc abl_vel+1
  sta abl_vel+1

  ; clamp maximum velocity
  clc
  lda abl_maxVel
  adc abl_vel
  lda abl_maxVel+1
  adc abl_vel+1
  bcs notUnderNegLimit
  sec
  lda #0
  sbc abl_maxVel
  sta abl_vel
  lda #0
  sbc abl_maxVel+1
  sta abl_vel+1
notUnderNegLimit:
  rts
notAccelLeft:

  lda abl_vel+1
  bmi brakeNegVel
  
  ; Case 3: Velocity > 0 and brake
  sec
  lda abl_vel
  sbc abl_brakeRate
  sta abl_vel
  lda abl_vel+1
  sbc #0
  bcs notZeroVelocity
zeroVelocity:
  lda #0
  sta abl_vel
notZeroVelocity:
  sta abl_vel+1
  rts

brakeNegVel:
  ; Case 4: Velocity < 0 and brake
  clc
  lda abl_vel
  adc abl_brakeRate
  sta abl_vel
  lda abl_vel+1
  adc #0
  bcs zeroVelocity
  sta abl_vel+1
  rts
.endproc


; Prediction of Y coordinate of bouncing ball

;;
; @return A: Y coordinate where ball will hit
; @return Y: Number of bounces
.proc tennis_compute_target_y
abs_xdist = 2
cur_y = 3
runlo = 4
riselo = 5
shiftAmt = 6

  ; calculate absolute distance to target paddle
  lda ball_xhi
  sec
  bit ball_dxhi
  bpl calcdist_to_2p
  sbc #SERVE_1P_X
  jmp have_abs_xdist
calcdist_to_2p:
  eor #$FF
  adc #SERVE_2P_X
have_abs_xdist:
;  bcs :+
;  lda #0
;:
  sta abs_xdist

  ; getSlope1 requires a LOT of preprocessing.  The rise and run
  ; have to be in the first octant (0-45 degrees), and each less
  ; than 256.  
  ; rise/run * x distance  
  lda #8        ; getSlope1 multiplies the rise by 2^8
  sta shiftAmt  ; so scale the final product down by the same

  ; requirement 1: run > 0
  lda ball_dxlo
  sta runlo
  lda ball_dxhi
  bpl x_positive_1
  sec
  lda #0
  sbc ball_dxlo
  sta runlo
  lda #0
  sbc ball_dxhi
x_positive_1:

  ; requirement 2: run < 256, so scale it down and
  ; prepare to adjust the final product to compensate
  beq no_x_shift
x_shift_loop:
  lsr a
  ror runlo
  inc shiftAmt
  cmp #0
  bne x_shift_loop
no_x_shift:

  ; requirement 3: rise >= 0
  lda ball_dylo
  sta riselo
  lda ball_dyhi
  bpl y_positive_1
  sec
  lda #0
  sbc ball_dylo
  sta riselo
  lda #0
  sbc ball_dyhi
y_positive_1:

  ; requirement 4: rise < 256, so scale and compensate
  beq no_y_shift
y_shift_loop:
  lsr a
  ror riselo
  dec shiftAmt
  cmp #0
  bne y_shift_loop
no_y_shift:

  ; requirement 5: rise < run
  lda riselo
  bne rise_is_nonzero
  ; If rise is zero at this point, we get off easy: the prediction
  ; is 0.
  ; TO DO
  rts
rise_still_greater:
  dec shiftAmt
  lsr a
rise_is_nonzero:
  cmp runlo
  bcs rise_still_greater

  ; If rounding produces rise = run, assume 45 deg line
  adc #0
  sta riselo
  cmp runlo
  bcc not_on_45_deg_line
  lda #0
  sta 0
  lda abs_xdist
  bcs ahigh

not_on_45_deg_line:
  ; getSlope1 wants rise in A and run in Y and returns the
  ; slope in A
  ldy runlo
  jsr getSlope1
  ldy abs_xdist
  jsr mul8
  ; at this point, A = high bits and 0 = low bits
ahigh:

  ; rescale Y distance
  ldy shiftAmt
  beq no_rescale_shift
rescale_shift:
  lsr a
  ror 0
  dey
  bne rescale_shift
no_rescale_shift:
  sta 1
  
  ; at this point, 1:0 is abs(Y distance)
  bit ball_dyhi
  bpl y_positive_2
  sec
  lda #0
  sbc 0
  sta 0
  lda #0
  sbc 1
  sta 1
y_positive_2:

  ; at this point, 1:0 is Y distance
  clc
  lda ball_yhi
  adc 0
  sta 0
  lda #0
  adc 1
  sta 1


  ; at this point, 1:0 is destination y
  ldy #0  ; number of bounces
reflect_check_top:
  lda 0
  bit 1
  bmi reflect_top
  cmp #TOP_WALL+BALL_RADIUS
  bcs reflect_check_bottom
reflect_top:
  sec
  lda #<2*(TOP_WALL+BALL_RADIUS)
  sbc 0
  sta 0
  lda #>2*(TOP_WALL+BALL_RADIUS)
  sbc 1
  sta 1
  iny
reflect_check_bottom:
  lda 0
  cmp #BOTTOM_WALL-BALL_RADIUS
  lda 1
  sbc #$00
  bcc reflect_done
  lda #<(2*(BOTTOM_WALL-BALL_RADIUS)-1)
  sbc 0
  sta 0
  lda #>(2*(BOTTOM_WALL-BALL_RADIUS)-1)
  sbc 1
  sta 1
  iny
  bne reflect_check_top
reflect_done:
  lda 0
  
  ; if there are at least two bounces, act unsure
  cpy #2
  bcc is_sure
  lsr a
  adc #(TOP_WALL+BOTTOM_WALL)/4
is_sure:
  rts
.endproc

