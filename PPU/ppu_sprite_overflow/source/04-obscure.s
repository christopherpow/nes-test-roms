; Tests the pathological behavior when 8 sprites are on a scanline
; and the one just after the 8th is not on the scanline. After that,
; the PPU interprets different bytes of each following sprite as
; its Y coordinate. 1 2 3 4 5 6 7 8 9 10 11 12 13 14: If 1-8 are
; on the same scanline, 9 isn't, then the second byte of 10, the
; third byte of 11, fourth byte of 12, first byte of 13, second byte
; of 14, etc. are treated as those sprites' Y coordinates for the
; purpose of setting the overflow flag. This search continues until
; all sprites have been scanned or one of the (erroneously interpreted)
; Y coordinates places the sprite within the scanline.

.include "sprite_overflow.inc"

main:   jsr init_sprite_overflow
	
	set_test 2,"Checks that second byte of sprite #10 is treated as its Y "
	jsr clear_sprites
	lda #128
	sta sprites + 0
	sta sprites + 4
	sta sprites + 8
	sta sprites + 12
	sta sprites + 16
	sta sprites + 20
	sta sprites + 24
	sta sprites + 28
	; leave sprites + 32 off screen
	sta sprites + 37
	jsr should_set_flag
	
	set_test 3,"Checks that third byte of sprite #11 is treated as its Y "
	jsr clear_sprites
	lda #128
	sta sprites + 0
	sta sprites + 4
	sta sprites + 8
	sta sprites + 12
	sta sprites + 16
	sta sprites + 20
	sta sprites + 24
	sta sprites + 28
	; leave sprites + 32 off screen
	; leave sprites + 37 off screen
	sta sprites + 42
	jsr should_set_flag
	
	set_test 4,"Checks that fourth byte of sprite #12 is treated as its Y "
	jsr clear_sprites
	lda #128
	sta sprites + 0
	sta sprites + 4
	sta sprites + 8
	sta sprites + 12
	sta sprites + 16
	sta sprites + 20
	sta sprites + 24
	sta sprites + 28
	; leave sprites + 32 off screen
	; leave sprites + 37 off screen
	; leave sprites + 42 off screen
	sta sprites + 47
	jsr should_set_flag
	
	set_test 5,"Checks that first byte of sprite #13 is treated as its Y "
	jsr clear_sprites
	lda #128
	sta sprites + 0
	sta sprites + 4
	sta sprites + 8
	sta sprites + 12
	sta sprites + 16
	sta sprites + 20
	sta sprites + 24
	sta sprites + 28
	; leave sprites + 32 off screen
	; leave sprites + 37 off screen
	; leave sprites + 42 off screen
	; leave sprites + 47 off screen
	sta sprites + 48
	jsr should_set_flag
	
	set_test 6,"Checks that second byte of sprite #14 is treated as its Y "
	jsr clear_sprites
	lda #128
	sta sprites + 0
	sta sprites + 4
	sta sprites + 8
	sta sprites + 12
	sta sprites + 16
	sta sprites + 20
	sta sprites + 24
	sta sprites + 28
	; leave sprites + 32 off screen
	; leave sprites + 37 off screen
	; leave sprites + 42 off screen
	; leave sprites + 47 off screen
	; leave sprites + 48 off screen
	sta sprites + 53
	jsr should_set_flag
	
	set_test 7,"Checks that search stops at the last (64th) sprite"
	jsr clear_sprites
	lda #128
	sta sprites + 1 ; if search erroneously wraps,
	sta sprites + 2 ; it will grab one of these
	sta sprites + 3
	sta sprites + 8
	sta sprites + 12
	sta sprites + 16
	sta sprites + 20
	sta sprites + 24
	sta sprites + 28
	sta sprites + 32
	sta sprites + 36
	jsr should_clear_flag
	
	set_test 8,"Same as test #2 but using a different range of sprites"
	jsr clear_sprites
	lda #128
	sta sprites + 4
	sta sprites + 8
	sta sprites + 12
	sta sprites + 16
	sta sprites + 20
	sta sprites + 24
	sta sprites + 28
	sta sprites + 32
	; leave sprites + 36 off screen
	sta sprites + 41
	jsr should_set_flag
	
	jmp tests_passed
