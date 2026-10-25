; Tests sprite 0 hit with regard to column 255 (ignored) and off
; right edge of screen.

.include "sprite_hit.inc"

main:   jsr init_sprite_hit

	lda #tile_solid
	jsr fill_screen
	setb sprite_attr,0
	setb sprite_y,120
	
	; Basic
	
	set_test 2,"Should always miss when X = 255"
	setb sprite_tile,tile_solid
	setb sprite_x,255
	ldx #$1E
	jsr sprite_should_miss
	
	set_test 3,"Should hit; sprite has pixels < 255"
	setb sprite_x,254
	ldx #$1E
	jsr sprite_should_hit
	
	; Detailed
	
	set_test 4,"Should miss; sprite pixel is at 255"
	setb sprite_tile,tile_upper_right
	setb sprite_x,248
	ldx #$1E
	jsr sprite_should_miss
	
	set_test 5,"Should hit; sprite pixel is at 254"
	setb sprite_x,247
	ldx #$1E
	jsr sprite_should_hit
	
	set_test 6,"Should also hit; sprite pixel is at 254"
	setb sprite_tile,tile_upper_left
	setb sprite_x,254
	ldx #$1E
	jsr sprite_should_hit
	
	jmp tests_passed

