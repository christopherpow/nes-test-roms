; Tests basic sprite 0 hit double-height operation.

.include "sprite_hit.inc"

main:   jsr init_sprite_hit
	
	setb $2000,$20          ; double-height sprites
	
	; Single solid tile in middle of screen
	set_ppuaddr $21F0
	setb PPUDATA,tile_solid
	
	setb sprite_attr,0
	setb sprite_tile,0              ; tiles 0 and 1
	
	set_test 2,"Lower sprite tile should miss bottom of bg tile"
	setb sprite_x,128
	setb sprite_y,119
	ldx #$18
	jsr sprite_should_miss
	
	set_test 3,"Lower sprite tile should hit bottom of bg tile"
	setb sprite_x,128
	setb sprite_y,118
	ldx #$18
	jsr sprite_should_hit
	
	set_test 3,"Lower sprite tile should miss top of bg tile"
	setb sprite_x,128
	setb sprite_y,103
	ldx #$18
	jsr sprite_should_miss
	
	set_test 4,"Lower sprite tile should hit top of bg tile"
	setb sprite_x,128
	setb sprite_y,104
	ldx #$18
	jsr sprite_should_hit
	
	jmp tests_passed
