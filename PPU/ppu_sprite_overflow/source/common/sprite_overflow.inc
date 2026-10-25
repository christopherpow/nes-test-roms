.include "shell.inc"
.include "sprites.s"
.include "special_tiles.s"

init_sprite_overflow:
	jsr disable_rendering
	jsr clear_sprites
	lda #tile_blank
	jsr fill_screen
	rts

; Sets every 4th sprite byte starting at X
; to A, modifying Y sprites in all.
move_sprites:
:       sta sprites,x
	inx
	inx
	inx
	inx
	dey
	bne :-
	rts

begin_test:
	setb PPUMASK,0
	jsr wait_vbl
	setb PPUMASK,$18
	jmp dma_sprites
	rts

; Begins test, sets $2001 to X, then reads $2002
; a few times in current frame, then in middle of VBL
; of next frame into A and masks off overflow flag.
get_overflow_flag:
	jsr begin_test
	stx PPUMASK
	; read early in frame to help buggy emulator
	delay 9148
	lda $2002
	delay 14549
	lda $2002
	delay 7106
	lda $2002       ; raw scanline 10 of next frame
	ldx #0          ; disable ppu
	stx $2001
	and #$20
	rts

should_set_flag:
	ldx #$18
	jsr get_overflow_flag
	jeq test_failed
	rts

should_clear_flag:
	ldx #$18
	jsr get_overflow_flag
	jne test_failed
	rts
