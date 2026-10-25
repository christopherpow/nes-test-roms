.include "nes.inc"
.include "global.inc"


PRESS_TILE = $10
PRESS_ATTR = $03
CENTROID_TILE = $11
CENTROID_ATTR = $02
TRAIL_TILE = $12
TRAIL_ATTR = $03
OBJ_LEFT = 76
OBJ_TOP = 91

CENTROID_LIFETIME = 8

TRAIL_LEN = 11
TRAIL_MIN_DISTANCE_FROM_CENTROID = 6
TRAIL_LERP_LOG_RATE = 2

HISTORY_LEN = 8
HISTORY_STOP_TIME = 6
HISTORY_SPAWN_X = 220
HISTORY_MOVE_SPEED = 4
HISTORY_SAME_FRAME_SPACING = 2
HISTORY_DESPAWN_X = 32
HISTORY_PRESS_Y = 199
HISTORY_RELEASE_Y = 191

.zeropage
since_last_held: .res 1
since_last_transition: .res 1

; Centroid and trail coordinates are in half pixels:
; X in 0-192 and Y in 0-128
last_num_held: .res 1
last_centroid_x:  .res 1
last_centroid_y:  .res 1
trail_x: .res TRAIL_LEN
trail_y: .res TRAIL_LEN

history_press_x:      .res HISTORY_LEN
history_release_x:    .res HISTORY_LEN
history_press_tile:   .res HISTORY_LEN
history_release_tile: .res HISTORY_LEN

.segment "CODE"

.proc init_player
  lda #$7F
  sta since_last_held
  sta since_last_transition
  lda #0
  sta last_centroid_x
  sta last_centroid_y
  ldx #2 * HISTORY_LEN - 1
  .assert history_release_x = history_press_x + HISTORY_LEN, error, "history_press_x and history_release_x are not consecutive"
  :
    sta history_press_x,x
    dex
    bpl :-
  txa
  ldx #TRAIL_LEN-1
  :
    sta trail_y,x
    dex
    bpl :-
  rts
.endproc

;;
; Calculates centroid, history, and smoke trail
.proc move_player
pressbits = $00
xsum = $02
ysum = $03
num_held = $04
prev_since_last_held = $05

  lda since_last_held
  sta prev_since_last_held
  
  ; Update centroid
  lda cur_d3
  sta pressbits+0
  lda cur_d4
  sta pressbits+1
  ora pressbits+0
  beq no_centroid
    ; Sum press locations
    ldy #0
    sty xsum
    sty ysum
    sty num_held
    centroid_loop:
      asl pressbits+1
      rol pressbits+0
      bcc centroid_not_this_button
        lda powerpad_bit_to_button,y
        and #%00001100
        lsr a
        lsr a
        adc ysum
        sta ysum
        lda powerpad_bit_to_button,y
        and #%00000011
        adc xsum
        sta xsum
        inc num_held
      centroid_not_this_button:
      iny
      cpy #12
      bcc centroid_loop

    ; Divide by number of presses
    lda num_held
    beq no_centroid
    sta last_num_held
    asl a
    asl a
    pha
    tay
    ldx #0
    lda xsum
    jsr divaxbyy
    lda $00
    sta last_centroid_x
    pla
    tay
    lda ysum
    ldx #0
    stx since_last_held
    jsr divaxbyy
    lda $00
    sta last_centroid_y
    jmp centroid_done
  no_centroid:
    inc since_last_held
    bne centroid_done
    dec since_last_held
  centroid_done:

  ; Update trails
  ldx #TRAIL_LEN-2
  :
    lda trail_x,x
    sta trail_x+1,x
    lda trail_y,x
    sta trail_y+1,x
    dex
    bpl :-
  lda since_last_held
  cmp #CENTROID_LIFETIME
  bcc trail_not_hide
    lda #$FF
    bcs trail_have_y0
  trail_not_hide:
  lda prev_since_last_held
  cmp #CENTROID_LIFETIME
  bcc trail_lerp
    lda last_centroid_x
    sta trail_x+0
    lda last_centroid_y
    bcs trail_have_y0
  trail_lerp:
    lda last_centroid_x
    sec
    sbc trail_x+0
    ror a
    eor #$80
    .repeat ::TRAIL_LERP_LOG_RATE-1
      cmp #$80
      ror a
    .endrepeat
    adc trail_x+0
    sta trail_x+0
    lda last_centroid_y
    sec
    sbc trail_y+0
    ror a
    eor #$80
    .repeat ::TRAIL_LERP_LOG_RATE-1
      cmp #$80
      ror a
    .endrepeat
    adc trail_y+0
  trail_have_y0:
  sta trail_y+0

  ; Add presses to history  
releasebits = $02
press_spawn_x = $04
release_spawn_x = $05

  lda #HISTORY_SPAWN_X
  sta press_spawn_x
  sta release_spawn_x

  lda prev_d3
  eor #$FF
  and cur_d3
  sta pressbits+0
  lda prev_d4
  eor #$F0
  and cur_d4
  sta pressbits+1

  lda cur_d3
  eor #$FF
  and prev_d3
  sta releasebits+0
  lda cur_d4
  eor #$F0
  and prev_d4
  sta releasebits+1

  ora releasebits+0
  ora pressbits+1
  ora pressbits+0
  beq no_transition
    ldy #0
    sty since_last_transition
    history_loop:
      asl pressbits+1
      rol pressbits+0
      bcc history_not_press
        ldx #HISTORY_LEN-2
        :
          lda history_press_x,x
          sta history_press_x+1,x
          lda history_press_tile,x
          sta history_press_tile+1,x
          dex
          bpl :-
        lda powerpad_bit_to_button,y
        sta history_press_tile+0
        lda press_spawn_x
        sta history_press_x+0
        clc
        adc #HISTORY_SAME_FRAME_SPACING
        sta press_spawn_x
      history_not_press:

      asl releasebits+1
      rol releasebits+0
      bcc history_not_release
        ldx #HISTORY_LEN-2
        :
          lda history_release_x,x
          sta history_release_x+1,x
          lda history_release_tile,x
          sta history_release_tile+1,x
          dex
          bpl :-
        lda powerpad_bit_to_button,y
        sta history_release_tile+0
        lda release_spawn_x
        sta history_release_x+0
        clc
        adc #HISTORY_SAME_FRAME_SPACING
        sta release_spawn_x
      history_not_release:

      iny
      cpy #12
      bcc history_loop
    jmp history_transition_done
  no_transition:
    inc since_last_transition
    bne history_transition_done
    dec since_last_transition
  history_transition_done:

  ; Scroll history
  lda since_last_transition
  cmp #HISTORY_STOP_TIME
  bcs history_no_advance
    ldx #2 * HISTORY_LEN - 1
    .assert history_release_x = history_press_x + HISTORY_LEN, error, "history_press_x and history_release_x are not consecutive"
    history_advance_loop:
      lda history_press_x,x
      sec
      sbc #HISTORY_MOVE_SPEED
      bcs :+
        lda #0
      :
      sta history_press_x,x
      dex
      bpl history_advance_loop
  history_no_advance:
  
  rts
.endproc


;;
; Draws the player's character to the display list as six sprites.
; In the template, we don't need to handle half-offscreen actors,
; but a scrolling game will need to "clip" sprites (skip drawing the
; parts that are offscreen).
.proc draw_player_sprite
pressbits = $00
  ldx oam_used

  ; Draw presses
  lda cur_d3+0
  sta pressbits
  lda cur_d4
  sta pressbits+1
  ldy #0
  pressloop:
    asl pressbits+1
    rol pressbits+0
    bcc not_this_press
      lda powerpad_bit_to_button,y
      and #%00001100
      asl a
      asl a
      asl a
      adc #OBJ_TOP
      sta OAM,x
      inx
      lda #PRESS_TILE
      sta OAM,x
      inx
      lda #PRESS_ATTR
      sta OAM,x
      inx
      lda powerpad_bit_to_button,y
      and #%00000011
      asl a
      asl a
      asl a
      asl a
      asl a
      adc #OBJ_LEFT
      sta OAM,x
      inx
    not_this_press:
    iny
    cpy #12
    bcc pressloop

  ; Draw centroid
  lda last_num_held
  lsr a
  beq skip_centroid  ; hide unless at least 2
  lda since_last_held
  cmp #CENTROID_LIFETIME
  bcs skip_centroid
    lda last_centroid_y
    lsr a
    adc #OBJ_TOP
    sta OAM,x
    inx
    lda #CENTROID_TILE
    sta OAM,x
    inx
    lda #CENTROID_ATTR
    sta OAM,x
    inx
    lda last_centroid_x
    lsr a
    adc #OBJ_LEFT
    sta OAM,x
    inx
  skip_centroid:

  ; TODO: draw trails
  ldy #0
  trail_loop:
    lda trail_y,y
    cmp #$C0
    bcs not_this_trail
    
    ; If the centroid is showing, don't draw trail particles that
    ; are too close to it.  Use supercat's comparison idiom from
    ; https://forums.atariage.com/topic/71120-6502-killer-hacks/?do=findComment&comment=1054049
    lda last_num_held
    beq trail_not_close_to_centroid
    ; clc
    sbc last_centroid_y
    sbc #TRAIL_MIN_DISTANCE_FROM_CENTROID-1
    adc #TRAIL_MIN_DISTANCE_FROM_CENTROID*2+1
    bcc trail_not_close_to_centroid
    clc
    lda trail_x,y
    sbc last_centroid_x
    sbc #TRAIL_MIN_DISTANCE_FROM_CENTROID-1
    adc #TRAIL_MIN_DISTANCE_FROM_CENTROID*2+1
    bcs not_this_trail
    trail_not_close_to_centroid:
      lda trail_y,y
      lsr a
      adc #OBJ_TOP
      sta OAM+0,x
      tya
      lsr a
      lsr a
      clc
      adc #TRAIL_TILE
      sta OAM+1,x
      lda #TRAIL_ATTR
      sta OAM+2,x
      lda trail_x,y
      lsr a
      adc #OBJ_LEFT
      sta OAM+3,x
      inx
      inx
      inx
      inx
    not_this_trail:
    iny
    iny
    cpy #TRAIL_LEN
    bcc trail_loop

  ; Draw history
  ldy #HISTORY_LEN - 1
  history_loop:
    lda history_press_x,y
    cmp #HISTORY_DESPAWN_X
    bcc history_not_this_press
      sta OAM+3,x
      lda #HISTORY_PRESS_Y
      sta OAM+0,x
      lda history_press_tile,y
      sta OAM+1,x
      and #$02
      lsr a
      sta OAM+2,x
      inx
      inx
      inx
      inx
    history_not_this_press:
    lda history_release_x,y
    cmp #HISTORY_DESPAWN_X
    bcc history_not_this_release
      sta OAM+3,x
      lda #HISTORY_RELEASE_Y
      sta OAM+0,x
      lda history_release_tile,y
      sta OAM+1,x
      and #$02
      lsr a
      sta OAM+2,x
      inx
      inx
      inx
      inx
    history_not_this_release:
    dey
    bpl history_loop

  stx oam_used
  rts
.endproc
