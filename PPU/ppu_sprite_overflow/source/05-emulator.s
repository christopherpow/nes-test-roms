; Tests things that an optimized emulator is likely get wrong

.include "sprite_overflow.inc"

main:   jsr init_sprite_overflow
	
	set_test 2,"Didn't set flag when $2002 wasn't read during frame"
	jsr clear_sprites
	lda #128
	ldx #<sprite_y
	ldy #9
	jsr move_sprites
	jsr begin_test
	delay_scanlines 266
	lda $2002
	and #$20
	jeq test_failed
	
	set_test 3,"Disabling rendering didn't recalculate flag time"
	jsr clear_sprites
	lda #127                ; 9 sprites at 127, 9 at 230
	ldx #<sprite_y
	ldy #9
	jsr move_sprites
	lda #230
	ldy #9
	jsr move_sprites
	jsr wait_vbl
	lda #$18                ; enable rendering
	sta $2001
	jsr dma_sprites         ; 4.5 scanlines
	delay_scanlines 142
	lda $2002               ; have emulator think it'll occur next
	delay 4                 ; hack to make it reliable on NES
	lda #$00                ; disable rendering
	sta $2001
	delay_scanlines 12
	lda #$18                ; enable rendering
	sta $2001
	delay_scanlines 90
	lda $2002
	and #$20
	jne test_failed
	delay_scanlines 4
	lda $2002
	and #$20
	jeq test_failed
	
	set_test 4,"Changing sprite RAM didn't recalculate flag time"
	lda #248
	ldx #<sprite_y
	ldy #2
	jsr move_sprites
	jsr wait_vbl
	lda #$18                ; enable rendering
	sta $2001
	delay_scanlines 142
	lda $2002               ; have emulator think it'll occur next
	lda #$00                ; disable rendering
	sta $2001
	jsr dma_sprites
	lda #$18                ; enable rendering
	sta $2001
	delay_scanlines 104
	lda $2002
	pha
	delay_scanlines 4
	ldx $2002
	pla
	and #$20
	jne test_failed
	txa
	and #$20
	jeq test_failed
	
	set_test 5,"Changing sprite height didn't recalculate time"
	jsr clear_sprites
	lda #100
	ldx #<sprite_y
	ldy #7
	jsr move_sprites
	lda #115
	ldy #2
	jsr move_sprites
	lda #200
	ldy #9
	jsr move_sprites
	jsr begin_test
	delay_scanlines 66
	lda $2002               ; have emulator think it'll occur at 200
	lda #$20                ; change sprite height so it'll occur at 115
	sta $2000
	delay_scanlines 100
	lda $2002
	and #$20
	jeq test_failed

	jmp tests_passed
	
