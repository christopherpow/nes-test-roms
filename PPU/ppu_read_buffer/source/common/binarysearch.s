.if 0

zp_res binarysearch_min, 2
zp_res binarysearch_max, 2
zp_res binarysearch_temp

; Implements 16-bit binary search.
;
; min_value = Minimum value (inclusive)
; max_value = Maximum value (exclusive)
;
; function: JSR target for a function
; Finds the first value where function returns carry clear.
; Assuming that C has 1 for small values, 0 for large values.
;              In:  X:A = current value (X=hi 8 bits, A=lo 8 bits)
;              Out: C:  1=needs larger value, 0=does not
;		Function does not need to preserve any register.
; Returns X:A = The first value where function returns carry clear.
; Wastes Y

.macro lower_bound_jsr min_value, max_value, function
	.local @loop, @body, @done, @smaller, @notsmaller
	
	setw binarysearch_min, min_value
	setw binarysearch_max, max_value
@loop:
	pha
	 setw addr, function
	pla
	jsr binarysearch_loop
	bcc @loop

.endmacro



.pushseg
.segment "LIB"


binarysearch_loop:
	; Find mid-point:
	; plan = min + (max-min)/2. min and max are both 16-bit. sums and differences are 17-bit.
	lda binarysearch_max+0
	sec
	sbc binarysearch_min+0
	sta binarysearch_temp ; low byte of (max-min)

	lda binarysearch_max+1
	sbc binarysearch_min+1 ; A = high byte of (max-min)
	
	pha
	 ora binarysearch_temp
	 ;A=0 = over;  nonzero = not over
	 eor #$FF
	 tax
	 ;X=$FF = over; other = not over
@its_not_over:
	pla

	clc
	ror
	pha
	 lda binarysearch_temp
	 ror
	 clc
	 adc binarysearch_min+0
	 tay ;Y = low
	pla
	adc binarysearch_min+1
	; A = high, Y = low
	; should not overflow here.
	cpx #$FF
	bcs @done
	pha
	 tax ;X = high
	 tya ;A = low
	 pha
	  ; There is no jsr (abs), so we simulate it...
	  lda #> (@cont-1)
	  pha
	   lda #< (@cont-1)
	   pha
	    tya ;A = low
	    jmp (addr)
@cont:	 pla
	 bcs @smaller
	 ; C was clear. Needs equal or smaller. Set max = current
	 ; A = low
	 sta binarysearch_max+0
	pla ;A =high
	sta binarysearch_max+1
	clc
	; The function may have gobbled addr, so we return to the
	; macro which will reinitialize the addr variable.
	rts
@smaller:
	 ; C was set. Needs larger. Set minimum = current+1
	 clc
	 adc #1
	 sta binarysearch_min+0
	pla
	adc #0
	sta binarysearch_min+1
	;clc - should already be clear; I don't think that adc can overflow.
	rts
@done:
	tax ;X = high
	tya ;A = low
	;sec - already set (we got here with a bcs)
	rts
.popseg

.endif
