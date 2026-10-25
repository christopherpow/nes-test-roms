
; NES APU Square Pitch Test

; Tests square's frequency timer. A tone of unchanging timbre
; should play; if it continually shifts, check timer period
; handling, then CPU instruction timing.

; Shay Green <mail.com@blargg>

main:
	sei
	
	ldx	#0		; wait for hardware
hw_delay:
	dey
	bne	hw_delay
	dex
	bne	hw_delay
	
	lda	#0		; disable nmi
	sta	$2000
	
	lda	#$01		; enable square 1
	sta	$4015
	lda	#$7f		; volume = 15
	sta	$4000
	lda	#$23		; period = $123
	sta	$4002
	lda	#$01
	sta	$4003
	
	lda	#0
tone_loop:
	sta	0
	pha
	pla
	ldx	#193
tone_delay_loop:
	pha
	pla
	dex
	bne	tone_delay_loop
	sta	$4011
	eor	#32
	jmp	tone_loop
	
irq:
nmi:	rti

	.org $fffa
	.word	nmi
	.word	main
	.word	irq
