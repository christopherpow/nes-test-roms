;
; Memory used by tennis game
; Copyright 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.include "global.inc"

PADDLE_1P_X = 16
PADDLE_2P_X = 240
SERVE_1P_X = PADDLE_1P_X + 2 * BALL_RADIUS
SERVE_2P_X = PADDLE_2P_X - 2 * BALL_RADIUS
BALL_RADIUS = 4
PADDLE_LENGTH = 16
TOP_WALL = 16
BOTTOM_WALL = 192 + TOP_WALL

BALL_START_SPEED = 16  ; in 1/16 pixels per half-frame
BALL_MAX_SPEED = 200   ; dunno if this will be implemented
BALL_INCREASE_SPEED = 2

; use even state numbers so that no shift is needed in the dispatcher
STATE_SERVE = 0
STATE_ACTIVE = 2
STATE_WIN_POINT = 4
STATE_WIN_GAME = 6

whatNeedsDrawn = $014E
whatToBlit = $014F
DRAW_SCORE = 1
DRAW_TIP = 2

; fields
.globalzp ball_dxhi, ball_dxlo, ball_dyhi, ball_dylo
.globalzp ball_xhi, ball_yhi, paddle_yhi, paddle_ylo, player_score
.globalzp paddle_yhi, serve_turn, state_timer_hi, player_joined
.globalzp ball_ypred, paddle_ypred
game_state = pointat_state
state_timer = held_time

; tennisgfx methods
.global tennis_title
.global tennis_blit_something
.global tennis_draw_score_digits
.global tennis_draw_sprites
.global tennis_load_bg
.global tennis_hide_tip
.global tennis_tip_point_player_x, tennis_tip_serve
.global tennis_tip_game_point_player_x, tennis_tip_winner_player_x

