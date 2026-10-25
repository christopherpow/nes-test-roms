; Tests sprite 0 hit using a sprite with a single pixel set,
; for each of the four corners.

.include "sprite_hit.inc"

test_corner:
	sta sprite_tile
	eor #$03
	pha
	set_ppuaddr $21F0
	pla
	sta PPUDATA
	ldx #$18
	jsr sprite_should_hit
	rts

main:   jsr init_sprite_hit
	
	setb sprite_attr,0
	
	set_test 2,"Lower-right pixel should hit"
	setb sprite_x,121
	setb sprite_y,112
	lda #tile_lower_right
	jsr test_corner
	
	set_test 3,"Lower-left pixel should hit"
	setb sprite_x,135
	setb sprite_y,112
	lda #tile_lower_left
	jsr test_corner
	
	set_test 4,"Upper-right pixel should hit"
	setb sprite_x,121
	setb sprite_y,126
	lda #tile_upper_right
	jsr test_corner
	
	set_test 5,"Upper-left pixel should hit"
	setb sprite_x,135
	setb sprite_y,126
	lda #tile_upper_left
	jsr test_corner
	
	jmp tests_passed
