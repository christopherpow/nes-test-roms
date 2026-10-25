; Tests sprite 0 hit with regard to clipping of left 8 pixels of screen.

.include "sprite_hit.inc"

main:   jsr init_sprite_hit

	lda #tile_solid
	jsr fill_screen
	setb sprite_attr,0
	setb sprite_y,120
	
	; Basic
	
	setb sprite_tile,tile_solid
	
	set_test 2,"Should miss when entirely in left-edge clipping"
	setb sprite_x,0
	ldx #$18
	jsr sprite_should_miss
	
	set_test 3,"Left-edge clipping occurs when $2001 is not $1E"
	ldx #$1A
	jsr sprite_should_miss
	lda #3
	ldx #$1C
	jsr sprite_should_miss
	
	set_test 4,"Left-edge clipping is off when $2001 = $1E"
	ldx #$1E
	jsr sprite_should_hit
	
	set_test 5,"Left-edge clipping should block hits only when X = 0"
	setb sprite_x,1
	ldx #$18
	jsr sprite_should_hit
	
	; Detailed
	
	set_test 6,"Should miss; sprite pixel covered by left-edge clip"
	setb sprite_tile,tile_upper_left
	setb sprite_x,7
	ldx #$18
	jsr sprite_should_miss
	
	set_test 7,"Should hit; sprite pixel outside left-edge clip"
	setb sprite_x,8
	ldx #$18
	jsr sprite_should_hit
	
	set_test 8,"Should hit; sprite pixel outside left-edge clip"
	setb sprite_tile,tile_upper_right
	setb sprite_x,1
	ldx #$18
	jsr sprite_should_hit

	jmp tests_passed

