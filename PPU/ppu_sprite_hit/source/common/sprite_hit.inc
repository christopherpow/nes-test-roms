.include "shell.inc"
.include "sprites.s"
.include "special_tiles.s"

init_sprite_hit:
	jsr disable_rendering
	jsr clear_sprites
	lda #tile_blank
	jsr fill_screen
	rts

test_for_hit:
	jsr wait_vbl
	stx PPUMASK
	setb PPUSCROLL,0
	setb PPUSCROLL,0
	dma_sprites
	delay 29240
	lda $2002
	jsr wait_vbl
	ldx #0
	stx PPUMASK
	rts

sprite_should_hit:
	jsr test_for_hit
	and #$40
	jeq test_failed
	rts

sprite_should_miss:
	jsr test_for_hit
	and #$40
	jne test_failed
	rts
