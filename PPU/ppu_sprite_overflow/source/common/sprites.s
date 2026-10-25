sprites         = $600
sprite_y        = sprites+0
sprite_tile     = sprites+1
sprite_attr     = sprites+2
sprite_x        = sprites+3


; Clears sprite bytes to $F8.
; Preserved: X, Y
clear_sprites:
	lda #$F8
	; FALL THROUGH


; Fills sprites with A.
; Preserved: A, X, Y
fill_sprites:
	ldx #0
:       sta sprites,x
	inx
	bne :-
	rts


; Copies sprites to OAM.
; Preserved: A, X, Y
dma_sprites:
	pha
.macro dma_sprites
	setb SPRADDR,0
	setb SPRDMA,>sprites
.endmacro
	dma_sprites
	pla
	rts
