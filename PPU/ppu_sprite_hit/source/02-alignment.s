; Tests alignment of sprite hit with background.
; Places a solid background tile in the middle of the screen and
; places the sprite on all four edges both overlapping and
; non-overlapping.

.include "sprite_hit.inc"

main:   jsr init_sprite_hit

	; Single solid tile in middle of screen
	set_ppuaddr $21F0
	setb $2007,tile_solid
	
	setb sprite_attr,0
	setb sprite_tile,tile_solid
	
	set_test 2,"Basic sprite-background alignment is way off"
	setb sprite_x,128
	setb sprite_y,119
	ldx #$18
	jsr sprite_should_hit
	
	; Left
	
	set_test 3,"Sprite should miss left side of bg tile"
	setb sprite_x,120
	setb sprite_y,119
	ldx #$18
	jsr sprite_should_miss
	
	set_test 4,"Sprite should hit left side of bg tile"
	setb sprite_x,121
	setb sprite_y,119
	ldx #$18
	jsr sprite_should_hit
	
	; Right
	
	set_test 5,"Sprite should miss right side of bg tile"
	setb sprite_x,136
	setb sprite_y,119
	ldx #$18
	jsr sprite_should_miss
	
	set_test 6,"Sprite should hit right side of bg tile"
	setb sprite_x,135
	setb sprite_y,119
	ldx #$18
	jsr sprite_should_hit
	
	; Above
	
	set_test 7,"Sprite should miss top of bg tile"
	setb sprite_x,128
	setb sprite_y,111
	ldx #$18
	jsr sprite_should_miss
	
	set_test 8,"Sprite should hit top of bg tile"
	setb sprite_x,128
	setb sprite_y,112
	ldx #$18
	jsr sprite_should_hit
	
	; Below
	
	set_test 9,"Sprite should miss bottom of bg tile"
	setb sprite_x,128
	setb sprite_y,127
	ldx #$18
	jsr sprite_should_miss
	
	set_test 10,"Sprite should hit bottom of bg tile"
	setb sprite_x,128
	setb sprite_y,126
	ldx #$18
	jsr sprite_should_hit
	
	jmp tests_passed
