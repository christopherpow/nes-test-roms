; Tests sprite overflow flag timing to PPU clock accuracy

.include "sprite_overflow.inc"
.include "sync_vbl.s"

clear_time      = 6819
overflow_time   = 7290

; Reads $2002 at n PPU clocks into frame, where 0 is the point at which
; VBL flag first reads back as set.
.macro test_timing n
	.local n_
	n_ = (n) + 3
	jsr test_timing_
	delay 29781
	delay_ppu_even (n_ .MOD 3) + 9
	lda #$18
	sta $2001
	delay n_/3 - 3 + 29781 - 33 - 1
	lda $2002
	tax
	and #$20
.endmacro

test_timing_:
	jsr wait_vbl
	lda #>sprites
	jsr sync_vbl_even_dma
	lda $2002
	rts
	
main:   jsr init_sprite_overflow
	
	; First 9 sprites Y = 128
	lda #128
	ldx #<sprite_y
	ldy #9
	jsr move_sprites
	
	set_test 2,"PPU VBL timing is wrong"
	
	test_timing -1
	cpx #0
	jmi test_failed
	
	set_test 3,"PPU VBL timing is wrong"
	test_timing 0
	cpx #0
	jpl test_failed
	
	set_test 3,"Flag cleared too early at end of VBL"
	test_timing clear_time - 1
	jeq test_failed
	
	set_test 4,"Flag cleared too late at end of VBL"
	test_timing clear_time
	jne test_failed

	; First 9 sprites at top left
	jsr clear_sprites
	lda #0
	ldx #<sprite_y
	ldy #9
	jsr move_sprites
	lda #0
	ldx #<sprite_x
	ldy #9
	jsr move_sprites
	
	set_test 5,"Flag set too early for first scanline"
	test_timing overflow_time - 1
	jne test_failed
	
	set_test 6,"Flag set too late for first scanline"
	test_timing overflow_time
	jeq test_failed
	
	; First 9 sprites at right side
	lda #255
	ldx #<sprite_x
	ldy #9
	jsr move_sprites
	
	set_test 7,"Horizontal positions shouldn't affect timing"
	test_timing overflow_time-6
	ldx $2002
	and #$20
	jne test_failed
	txa
	and #$20
	jeq test_failed
	
	; Last 9 sprites at top
	jsr clear_sprites
	lda #0
	ldx #220+<sprite_y
	ldy #9
	jsr move_sprites
	
	set_test 8,"Set too early for last sprites on first scanline"
	test_timing overflow_time + 110 - 1
	jne test_failed
	
	set_test 9,"Set too late for last sprites on first scanline"
	test_timing overflow_time + 110
	jeq test_failed
	
	; First 9 sprites at Y=239
	jsr clear_sprites
	lda #239
	ldx #<sprite_y
	ldy #9
	jsr move_sprites

	set_test 10,"Set too early for last scanline"
	test_timing overflow_time + 239*341 - 1
	jne test_failed
	
	set_test 11,"Set too late for last scanline"
	test_timing overflow_time + 239*341
	jeq test_failed
	
	; First 8 sprites at top, and last sprite at top
	jsr clear_sprites
	lda #0
	ldx #<sprite_y
	ldy #8
	jsr move_sprites
	lda #0
	sta sprites + 255
	
	set_test 12,"Set too early when 9th sprite # is way after 8th"
	test_timing overflow_time + 110 - 1
	jne test_failed

	set_test 13,"Set too late when 9th sprite # is way after 8th"
	test_timing overflow_time + 110
	jeq test_failed
	
	; First 9 sprites at Y=1
	jsr clear_sprites
	lda #1
	ldx #<sprite_y
	ldy #9
	jsr move_sprites

	set_test 14,"Overflow on second scanline occurs too early"
	test_timing overflow_time + 341 - 1
	jne test_failed

	set_test 15,"Overflow on second scanline occurs too late"
	test_timing overflow_time + 341
	jeq test_failed

	jmp tests_passed
