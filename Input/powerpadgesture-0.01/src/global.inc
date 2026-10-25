.ifndef GLOBAL_INC
.define GLOBAL_INC

; init.s
.global reset_handler

; main.s
.global main, nmi_handler, irq_handler
.global OAM
.globalzp cur_keys, new_keys, oam_used

; bg.s
.global draw_bg

; player.s
.global init_player, move_player, draw_player_sprite

; ppuclear.s
.global ppu_clear_nt, ppu_clear_oam, ppu_screen_on

; powerpad.s
.global read_powerpad, powerpad_bit_to_button
.globalzp cur_d3, cur_d4, prev_d3, prev_d4

; math.s
.global divaxbyy, div_a2by1

.endif
