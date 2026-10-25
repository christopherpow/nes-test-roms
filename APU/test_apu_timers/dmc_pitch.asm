
; NES APU DMC Pitch Test

; Tests DMC's frequency timer. Sixteen tones of unchanging timbre
; should play. If one continually shifts, check the corresponding
; timer table entry. If all shift, check timer period handling, CPU
; wait-state addition during a DMC DMA fetch, and CPU instruction
; timing.

; ** Some tones seem to shift a bit, even on NES hardware. I'm not
; sure why. Refer to dmc_pitch.wav. **

; Shay Green <mail.com@blargg>

; Zero-page variables
delay1	= 10
delay2	= 11
remain	= 12
entry		= 13

	.org	$e000
	; DMC sample. Four cycles of triangle wave with 34 bits per cycle.
	.byte	$ff,$ff,$01,$00,$fc,$ff,$07,$00
	.byte	$f0,$ff,$1f,$00,$c0,$ff,$7f,$00,$00
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
	
	lda	#$80		; dmc addr = $e000
	sta	$4012
	lda	#$01		; dmc len = 17 bytes
	sta	$4013
	lda	#$4f		; dmc freq = 15, loop
	sta	$4010
	lda	#$10		; start dmc
	sta	$4015
	
	; Play DMC sample at each of the 16 frequencies, and play
	; CPU-timed square wave at matching frequency using $4011.
	
	lda	#15
	sta	entry
freq_loop:
	lda	entry		; set dmc freq
	ora	#$40
	sta	$4010
	
	ldx	entry		; copy table entries to zero-page
	lda	table1,x
	sta	delay1
	lda	table2,x
	sta	delay2
	lda	table3,x
	sta	remain
	
	lda	#64		; a = $4011 dac amplitude
	ldy	#8		; have to wait extra 4 cycles every 8 loops
tone_loop_extra:
	pha			; 3
	pla			; 4
tone_loop:
	ldx	delay1	; 3
tone_delay_loop1:
	pha			; *3
	pla			; *4
	dex			; *2
	bne	tone_delay_loop1	; *3
				; -1
	ldx	delay2	; 3
tone_delay_loop2:
	pha			; *3
	pla			; *4
	pha			; *3
	pla			; *4
	dex			; *2
	bne	tone_delay_loop2	; *3
				; -1
	sta	$4011		; 4
	sta	$4011		; 4
	eor	#31		; 2
	dey			; 2
	bne	tone_loop_extra	; 3
				; -1
	dec	remain	; 5
	beq	next_freq	; 2
	ldy	#8		; 2
	bne	tone_loop	; 3
	
next_freq:
	dec	entry
	bpl	freq_loop
	
	lda	#0
	sta	$4015
	
forever:
	jmp	forever
	
table1:
    .byte   206, 138,  75,  53,   8,   7,  18,   1
    .byte     5,  10,  13,   9,  19,  10,  12,  15
table2:
    .byte   251, 251, 255, 251, 249, 221, 189, 189
    .byte   165, 135, 117, 107,  81,  67,  55,  37
table3:
    .byte    50,  56,  63,  67,  75,  85,  95, 100
    .byte   113, 135, 152, 168, 203, 255, 255, 255

irq:
nmi:	rti

	.org $fffa
	.word	nmi
	.word	main
	.word	irq
