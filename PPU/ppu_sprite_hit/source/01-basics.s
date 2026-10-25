; Tests basic sprite 0 hit behavior (nothing timing related).

.include "sprite_hit.inc"

main:   jsr init_sprite_hit

	lda #tile_solid
	jsr fill_screen
	
	; Put sprite in middle of screen
	setb sprite_attr,0
	setb sprite_x,128
	setb sprite_y,120
	
	set_test 2,"Flag isn't working at all"
	setb sprite_tile,tile_solid
	ldx #$18
	jsr sprite_should_hit
	
	set_test 3,"Should hit even when completely behind background"
	setb sprite_attr,$20
	ldx #$18
	jsr sprite_should_hit
	
	set_test 4,"Should miss when background rendering is off"
	ldx #$10
	jsr sprite_should_miss
	
	set_test 5,"Should miss when sprite rendering is off"
	ldx #$08
	jsr sprite_should_miss
	
	set_test 6,"Should miss when all rendering is off"
	ldx #$00
	jsr sprite_should_miss
	
	setb sprite_tile,tile_blank
	
	set_test 7,"All-transparent sprite should miss"
	ldx #$18
	jsr sprite_should_miss
	
	set_test 8,"Only low two palette index bits are relevant"
	setb sprite_attr,$0F
	ldx #$18
	jsr sprite_should_miss
	
	set_test 9,"Any non-zero palette index should hit with any other"
	lda #1
	jsr fill_screen
	setb sprite_tile,2
	setb sprite_attr,0
	ldx #$18
	jsr sprite_should_hit
	
	set_test 10,"Should miss when background is all transparent"
	lda #0
	jsr fill_screen
	ldx #$18
	jsr sprite_should_miss
	
	set_test 11,"Should always miss other sprites"
	setb sprite_y   +4,120
	setb sprite_tile+4,3
	setb sprite_attr+4,0
	setb sprite_x   +4,128
	ldx #$18
	jsr sprite_should_miss

	jmp tests_passed

