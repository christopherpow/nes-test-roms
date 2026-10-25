
; NES APU Noise Pitch Test

; Tests noise's frequency timer. Noise should play without
; any phasing; if present, check timer period handling, then
; CPU instruction timing. Refer to noise_pitch.wav for correct
; and noise_pitch_bad.wav for incorrect output.

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
	
	lda	#$08		; enable noise
	sta	$4015
	lda	#$7f		; volume = 15
	sta	$400c
	lda	#15		; frequency 15
	sta	$400e
	lda	#$00
	sta	$400f
	
	lda	#0
tone_loop:
	pha			; 3
	pla			; 4
	nop			; 2
	nop			; 2
	ldx	#213		; 2
tone_delay_loop:
	pha			; *3
	pla			; *4
	pha			; *3
	pla			; *4
	dex			; 2
	bne	tone_delay_loop	; 3
				; -1
	sta	$4011		; 4
	eor	#25		; 2
	jmp	tone_loop	; 3
	
irq:
nmi:	rti

	.org $fffa
	.word	nmi
	.word	main
	.word	irq
