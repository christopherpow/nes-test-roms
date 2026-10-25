; Tests sprite 0 hit timing to PPU clock accuracy.

.include "sprite_hit.inc"
.include "sync_vbl.s"

clear_time      = 6819
earliest_time   = 7502
	
; Reads $2002 at n PPU clocks into frame, where 0 is the point at which
; VBL flag first reads back as set.
.macro test_timing n
	.local n_
	n_ = (n) + 1
	jsr test_timing_
	delay_ppu_even (n_ .MOD 3) + 9
	lda #0
	sta $2005
	sta $2005
	setb $2001,$1E
	delay n_/3 - 3 + 29781 - 43
	lda $2002
	tax
	and #$40
.endmacro

test_timing_:
	jsr wait_vbl
	setb SPRADDR,0
	setb SPRDMA,>sprites
	jsr sync_vbl_odd
	lda $2002
	rts
	
main:   jsr init_sprite_hit
	
	lda #tile_solid
	jsr fill_screen
	
	setb sprite_attr,0
	setb sprite_tile,tile_solid
	
	set_test 2,"PPU VBL timing is wrong"
	test_timing -1
	cpx #0
	jmi test_failed
	test_timing 0
	cpx #0
	jpl test_failed
	
	setb sprite_x,0
	setb sprite_y,0
	
	set_test 3,"Flag set too soon for upper-left corner"
	test_timing earliest_time - 1
	jne test_failed

	set_test 4,"Flag set too late for upper-left corner"
	test_timing earliest_time
	jeq test_failed

	setb sprite_x,254
	setb sprite_y,0
	
	set_test 5,"Flag set too soon for upper-right corner"
	test_timing earliest_time + 254 - 1
	jne test_failed

	set_test 6,"Flag set too late for upper-right corner"
	test_timing earliest_time + 254
	jeq test_failed

	setb sprite_x,0
	setb sprite_y,238
	
	set_test 7,"Flag set too soon for lower-left corner"
	test_timing earliest_time + 238*341 - 1
	jne test_failed

	set_test 8,"Flag set too late for lower-left corner"
	test_timing earliest_time + 238*341
	jeq test_failed

	setb sprite_x,0
	setb sprite_y,0
	
	set_test 9,"Flag cleared too soon at end of VBL"
	test_timing clear_time - 1
	jeq test_failed

	set_test 10,"Flag cleared too late at end of VBL"
	test_timing clear_time
	jne test_failed

	jmp tests_passed
