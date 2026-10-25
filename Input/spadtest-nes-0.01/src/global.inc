; init.s
.global reset_handler

; main.s
.global main, nmi_handler, irq_handler
.globalzp oam_used

; ppuclear.s
.global ppu_clear_nt, ppu_clear_oam, ppu_screen_on
.global OAM, OAMHI

; bg.s
.global load_bg_tiles, draw_bg

; lights.s
.global draw_lights

; spads.s
.global read_spads
.globalzp cur_keys, cur_axlr, new_keys, new_axlr
