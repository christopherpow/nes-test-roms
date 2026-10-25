;
; Memory used by Zapper demo
; Copyright 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
OAM = $0200

; main.s fields
.globalzp cur_keys, new_keys, nmis
; main.s methods
.global draw_player_sprite, move_player
.global draw_y_arrow_sprite, draw_x_arrow_sprite

; pads.s methods
.global read_pads

; PPU related constants
COLOR_BLACK = $0F
COLOR_WHITE = $30
COLOR_GREEN = $2A

; ppuclear.s fields
.globalzp oam_used
; ppuclear.s methods
.global ppu_clear_oam, ppu_screen_on, ppu_clear_nt, ppu_sta32y

; unpkb.s fields
.globalzp PKB_source, PKB_len
; unpkb.s methods
.global PKB_unpackblk, PKB_unpack

; title.s fields
.global title_screen

; menu.s fields
.globalzp pointat_state, group1color, group2color
.globalzp cur_trigger, new_trigger
; menu.s methods
.global pointat_menu

; zapkernels.s methods
.global zapkernel_yonoff_ntsc, zapkernel_yon2p_ntsc, zapkernel_xyon_ntsc

; testpatterns.s constants
.globalzp DOUBLESHOOT_FRAMES
; testpatterns.s fields
.globalzp cur_hue, cur_bright, cur_radius, held_time, doubleshoot_time
; testpatterns.s tables
.global fullbright_pkb, hlines_pkb, vlines_pkb, ballbg_pkb, menu_pkb
.global initial_palette
; testpatterns.s methods
.global test_fullbright_yonoff, test_hlines_yonoff, test_vlines_yonoff
.global test_fullbright_yon2p, test_fullbright_xyon, test_ball_yonoff
.global test_trigger_time, s0wait

; bcd.s methods
.global bcd8bit

; drawball.s methods
.global draw_ball  ; X: x center; Y: y center; A: radius | (pal << 6)
.global draw_paddle  ; X: x center; Y: y top center; A: ht | (pal << 6)

; math.s tables
.global sine256Q1, cosine256Q1

; math.s methods
.global mul8        ; calc A*Y, put high byte in A and low in $0000
.global getSlope1   ; A = 256*A/Y
.global tennis_compute_target_y

; kinematics.s args
abl_vel = 0  ; 16-bit
abl_maxVel = 2
abl_accelRate = 4
abl_brakeRate = 5
abl_keys = 6  ; bit 0: increase; bit 1: decrease
; kinematics.s methods
.global accelBrakeLimit

; tennis.s main method
; (most internal fields and methods are described in tennis.h)
.global tennis

; axe.s fields
.global axe_callback_on
; axe.s methods
.global axe

; Pently sound methods
.global pently_init, pently_update, pently_start_sound
; Pently music methods
.global pently_start_music, pently_stop_music, pently_play_note
