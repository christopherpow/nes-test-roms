.pushseg
.segment "LIB"
zp_res scanline_wait_temp
	; NTSC AND DENDY.
	; Wait A*341/3-X cycles, including JSR. X must be 0..66.
	;
	; I.e. 113.6666666666667 cycles per scanline.
	; This means a repeating pattern of 1*113,2*114 cycles.
	;
	; On PAL, the following would be required:
	; Wait A*1705/16-X cycles.
	;
	; I.e. 106.5625 cycles per scanline.
	; This means a repeating pattern of 7*106,9*107 cycles.
	;                 107,106,107,106,107,106,107,107 ; 3+5 (853)
	;                 106,107,106,107,106,107,106,107 ; 4+4 (852)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
delay_n_scanlines_ntsc:
	cmp #1	;2
	beq @last_line
	bcc @none_remain ; should never happen
	;	;6 so far
	cmp #3	;+2 = 8
	bcs @have_at_least_3 ;+2 = 10
	; Only 2 remaining. Do 114 cycles + the last scanline.
	delay (114-10+5)
	; ^Subtract the already done 10, but add 5 because the
	;  following part expects that much.
@last_line:	;5 so far
	stx scanline_wait_temp	;+3 = 8
	lda #113		;+2 = 10
	sec			;+2 = 12
	sbc scanline_wait_temp	;+3 = 15
	sec			;+2 = 17
	sbc #(22+25)		;+2 = 19
	jmp delay_a_25_clocks	;+3 = 22
@none_remain:
	; 7 cycles so far. Can't help it.
	rts
@have_at_least_3:
	; 11 cycles so far.
	beq @have_exactly_3 ; +2 = 13
	delay (113+114+114 - 17 - 3)
	sec		;+2 = 15
	sbc #3		;+2 = 17
	jmp delay_n_scanlines_ntsc
@have_exactly_3:
	; 14 cycles so far
	delay (113+114+114 - 14-3+5-113)
	jmp @last_line

.if 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
delay_n_scanlines_pal:
	cmp #16+1		;2
	bcs @have_more_than_16	;2
	cmp #8+1		;2 =6
	bcs @have_more_than_8	;2
	cmp #2+1		;2
	bcs @have_more_than_2	;2 =12

	; Number of cycles to sleep: A*1705/16-X-6 with A=1..8
	sec		;+2 =14
	sbc #1		;+2 =16
	beq @last_line	;+2 =18
	delay (106+1)	;
@last_line:		; 19 so far
	stx scanline_wait_temp	;+3 = 22
	lda #107		;+2 = 24
	sec			;+2 = 26
	sbc scanline_wait_temp	;+3 = 29
	sec			;+2 = 31
	sbc #(34+25)		;+2 = 33
	jmp delay_a_25_clocks	;+3 = 36
@have_more_than_16:	;5 so far
	delay (1705-12)
	sec		;+2
	sbc #16		;+2 = 9
	jmp delay_n_scanlines_pal	;+3 = 12
@have_more_than_8:	;9 so far
	delay (853-16)
	sec		;+2
	sbc #8		;+2 = 13
	jmp delay_n_scanlines_pal	;+3 = 16
@have_more_than_2:	;13 so far
	delay (213-20)
	sec		;+2
	sbc #2		;+2 = 17
	jmp delay_n_scanlines_pal	;+3 = 20
.endif

.popseg
