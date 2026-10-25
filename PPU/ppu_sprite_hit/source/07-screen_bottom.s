; Tests sprite 0 hit with regard to bottom of screen.

.include "sprite_hit.inc"

main:   jsr init_sprite_hit
	
	lda #tile_solid
	jsr fill_screen
	setb sprite_attr,0
	setb sprite_x,128
	
	; Basic
	
	set_test 2,"Should always miss when Y >= 239"
	setb sprite_tile,tile_solid
	setb sprite_y,239
	ldx #$1E
	jsr sprite_should_miss
	
	set_test 3,"Can hit when Y < 239"
	setb sprite_y,238
	ldx #$1E
	jsr sprite_should_hit
	
	set_test 4,"Should always miss when Y = 255"
	setb sprite_y,255
	ldx #$1E
	jsr sprite_should_miss
	
	; Detailed
	
	set_test 5,"Should hit; sprite pixel is at 238"
	setb sprite_tile,tile_lower_right
	setb sprite_y,231
	ldx #$1E
	jsr sprite_should_hit
	
	set_test 6,"Should miss; sprite pixel is at 239"
	setb sprite_y,232
	ldx #$1E
	jsr sprite_should_miss
	
	set_test 7,"Should hit; sprite pixel is at 238"
	setb sprite_tile,tile_upper_left
	setb sprite_y,238
	ldx #$1E
	jsr sprite_should_hit
	
	jmp tests_passed

