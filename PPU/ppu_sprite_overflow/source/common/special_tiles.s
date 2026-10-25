; Special tile images

tile_blank              = 0
tile_color1             = 1
tile_color2             = 2
tile_solid              = 3
tile_upper_left         = 4
tile_upper_right        = 5
tile_lower_left         = 6
tile_lower_right        = 7
tile_upper_left_lower_right = 8

.pushseg
.segment CHARS
	; already put here by ROM
	;.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	.byte $80,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00
	.byte $01,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$80
	.byte $00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$01
	.byte $80,$00,$00,$00,$00,$00,$00,$01,$80,$00,$00,$00,$00,$00,$00,$01
.popseg
