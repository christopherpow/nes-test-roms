; Ensures that hit time is based on position on screen, and unaffected
; by which pixel of sprite is being hit (upper-left, lower-right, etc.)

.include "sprite_hit.inc"
.include "sync_vbl.s"

.macro test_hit_time n
	jsr begin_sprite_hit_timing
	delay n
	jsr test_hit_time_
.endmacro

begin_sprite_hit_timing:
	jsr sync_vbl
	setb PPUSCROLL,0
	setb PPUSCROLL,0
	setb PPUMASK,PPUMASK_BG0CLIP+PPUMASK_SPR
	setb SPRADDR,0
	setb SPRDMA,>sprites
	delay 29781-10
	rts

test_hit_time_:
	lda $2002
	ldx $2002
	
	jsr disable_rendering
	
	and #$40
	jne test_failed
	
	txa
	and #$40
	jeq test_failed
	
	rts

main:   jsr init_sprite_hit
	
	; Solid tile in middle of screen
	set_ppuaddr $21F0
	setb PPUDATA,tile_solid

	setb sprite_attr,0
	setb sprite_tile,tile_solid
	
	set_test 2,"Upper-left corner"
	setb sprite_x,128
	setb sprite_y,119
	test_hit_time 15505
	
	set_test 3,"Upper-right corner"
	setb sprite_x,123
	setb sprite_y,119
	test_hit_time 15505
	
	set_test 4,"Lower-left corner"
	setb sprite_x,128
	setb sprite_y,114
	test_hit_time 15505
	
	set_test 5,"Lower-right corner"
	setb sprite_x,123
	setb sprite_y,114
	test_hit_time 15505

	lda #tile_solid
	jsr fill_screen

	setb sprite_y,120
	setb sprite_tile,tile_upper_left_lower_right
	setb sprite_attr,0
	
	set_test 6,"Hit time shouldn't be based on pixels under left clip"
	setb sprite_x,7
	test_hit_time 16378
	
	setb sprite_attr,$80
	
	set_test 7,"Hit time shouldn't be based on pixels at X=255"
	setb sprite_x,248
	test_hit_time 16456
	
	set_test 8,"Hit time shouldn't be based on pixels off right edge"
	setb sprite_x,249
	test_hit_time 16456

	jmp tests_passed
