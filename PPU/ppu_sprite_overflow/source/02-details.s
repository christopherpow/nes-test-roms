; Tests details of sprite overflow flag

.include "sprite_overflow.inc"

main:   jsr init_sprite_overflow
	
	; Move first 9 sprites to Y = 128, X = 0
	lda #128
	ldx #<sprite_y
	ldy #9
	jsr move_sprites
	lda #0
	ldx #<sprite_x
	ldy #9
	jsr move_sprites
	
	set_test 2,"Should set flag even when sprites are under left clip"
	jsr should_set_flag
	
	set_test 3,"Disabling rendering shouldn't clear flag"
	jsr begin_test
	delay_scanlines 208
	lda #0
	sta $2001
	lda $2002
	and #$20
	jeq test_failed
	
	set_test 4,"Should clear flag at the end of VBL even when $2001=0"
	jsr begin_test
	delay_scanlines 267
	lda #0
	sta $2001       ; middle of VBL of next frame
	delay_scanlines 20
	lda $2002       ; after VBL of next frame
	and #$20
	jne test_failed
	
	set_test 5,"Should set flag even when sprite Y coordinates are 239"
	jsr clear_sprites
	lda #239
	ldx #<sprite_y
	ldy #9
	jsr move_sprites
	jsr should_set_flag
	
	set_test 6,"Shouldn't set flag when sprite Y coordinates are 240 (off screen)"
	jsr clear_sprites
	lda #240
	ldx #<sprite_y
	ldy #64
	jsr move_sprites
	jsr should_clear_flag
	
	set_test 7,"Shouldn't set flag when sprite Y coordinates are 255 (off screen)"
	jsr clear_sprites
	lda #255
	ldx #<sprite_y
	ldy #64
	jsr move_sprites
	jsr should_clear_flag
	
	set_test 8,"Should set flag regardless of which sprites are involved"
	jsr clear_sprites
	ldx #<sprite_y
	ldy #11
	lda #128
	jsr move_sprites
	setb sprites+0,240
	setb sprites+8,240
	jsr should_set_flag
	
	set_test 9,"Shouldn't set flag when all scanlines have 7 or fewer sprites"
	ldy #0
	lda #0
:       sta sprites,y
	clc
	adc #1
	iny
	iny
	iny
	iny
	bne :-
	jsr should_clear_flag
	
	set_test 10,"Double-height sprites aren't handled properly"
	jsr clear_sprites
	ldx #8+<sprite_y
	ldy #7
	lda #128
	jsr move_sprites
	setb sprites+0,113
	setb sprites+4,113
	ldy #$20
	sty $2000
	jsr should_set_flag
	
	jmp tests_passed
