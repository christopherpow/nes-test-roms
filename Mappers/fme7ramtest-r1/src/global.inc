.ifndef GLOBAL_INC
.define GLOBAL_INC

; init.s
.global reset_handler

; fme7.s
.global init_fme7

; main.s
.global main, nmi_handler, irq_handler
.global OAM
.globalzp cur_keys, new_keys, oam_used

; bg.s
.global puts_16, puts_multiline_16, hexdump8, cls_puts_multiline

; ppuclear.s
.global ppu_clear_nt, ppu_clear_oam, ppu_screen_on

; pads.s
.global read_pads

.endif
