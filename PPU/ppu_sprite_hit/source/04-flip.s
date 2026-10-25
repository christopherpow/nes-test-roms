; Tests sprite 0 hit for single pixel sprite and background.

.include "sprite_hit.inc"

main:   jsr init_sprite_hit

	; Single solid tile in middle of screen
	set_ppuaddr $21F0
	setb PPUDATA,tile_solid
	
	setb sprite_x,121
	setb sprite_y,112
	
	set_test 2,"Horizontal flipping doesn't work"
	setb sprite_attr,$40
	setb sprite_tile,tile_lower_left
	ldx #$18
	jsr sprite_should_hit
	
	set_test 3,"Vertical flipping doesn't work"
	setb sprite_attr,$80
	setb sprite_tile,tile_upper_right
	ldx #$18
	jsr sprite_should_hit
	
	set_test 4,"Horizontal + Vertical flipping doesn't work"
	setb sprite_attr,$C0
	setb sprite_tile,tile_upper_left
	ldx #$18
	jsr sprite_should_hit
	
	jmp tests_passed
