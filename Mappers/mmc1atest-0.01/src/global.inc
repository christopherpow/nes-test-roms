.ifndef GLOBAL_INC
.define GLOBAL_INC

; init.s
.global reset_handler

; main.s
.global OAM
.global main, nmi_handler, irq_handler
.globalzp cur_keys, new_keys, oam_used

; mmc1.s
.global load_chrdata

; pads.s
.global read_pads

; ppuclear.s
.global ppu_clear_nt, ppu_clear_oam, ppu_screen_on


; Character map
.charmap $20, $00
.charmap $3A, $06
.charmap $40, $07  ; @ as proxy for copyright
.charmap $49, $08
.charmap $4B, $09
.charmap $4D, $0A
.charmap $4E, $0B
.charmap $52, $0C
.charmap $53, $0D
.charmap $54, $0E
.charmap $59, $0F
.charmap $30, $10
.charmap $31, $11
.charmap $32, $12
.charmap $33, $13
.charmap $34, $14
.charmap $35, $15
.charmap $36, $16
.charmap $37, $17
.charmap $38, $18
.charmap $39, $19
.charmap $41, $1A
.charmap $42, $1B
.charmap $43, $1C
.charmap $44, $1D
.charmap $45, $1E
.charmap $46, $1F
LF = $40
EOT = $FF

EVENHEXDIGIT = $20
ODDHEXDIGIT = $30


.endif

