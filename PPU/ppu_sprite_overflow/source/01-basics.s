; Tests basic operation of sprite overflow flag (bit 5 of $2002).

.include "sprite_overflow.inc"

main:   jsr init_sprite_overflow
	
	; move first 9 sprites to Y = 128
	lda #128
	ldx #<sprite_y
	ldy #9
	jsr move_sprites
	
	set_test 2,"Should set flag when 9 sprites are on scanline"
	jsr begin_test
	delay_scanlines 208
	lda $2002
	and #$20
	jeq test_failed
	
	set_test 3,"Reading $2002 shouldn't clear flag"
	jsr begin_test
	delay_scanlines 208
	lda $2002       ; shouldn't clear flag
	lda $2002
	and #$20
	jeq test_failed
	
	set_test 4,"Shouldn't clear flag at beginning of VBL"
	jsr begin_test
	delay_scanlines 267
	lda $2002       ; middle of VBL of next frame
	and #$20
	jeq test_failed
	
	set_test 5,"Should clear flag at end of VBL"
	jsr begin_test
	delay_scanlines 287
	lda $2002       ; after VBL of next frame
	and #$20
	jne test_failed
	
	set_test 6,"Shouldn't set flag when $2001=$00"
	ldx #$00
	jsr get_overflow_flag
	jne test_failed
	
	set_test 7,"Should set normally when $2001=$08"
	ldx #$08
	jsr get_overflow_flag
	jeq test_failed
	
	set_test 8,"Should set normally when $2001=$10"
	ldx #$10
	jsr get_overflow_flag
	jeq test_failed
	
	jmp tests_passed
