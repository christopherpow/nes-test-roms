.ifndef GLOBAL_INC
.define GLOBAL_INC

; init.s
.global reset_handler

; main.s
.global main, nmi_handler, irq_handler
.globalzp nmis, cur_keys

; ppuclear.s
.global ppu_clear_nt, ppu_screen_on, ppu_vsync

; unpb53.s
.global unpb53_some, unpb53_xtiles
.global PB53_outbuf
.globalzp ciSrc, ciBufStart, ciBufEnd

; telling.s
.global tell_ly

.endif
