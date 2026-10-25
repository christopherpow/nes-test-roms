
; NES APU Sweep Cutoff Test

; Tests NES APU sweep cutoff (silence) thresholds. When run, there
; should be silence, white noise, several tones without any silence
; between, then a tone sweep up (refer to sweep_cutoff.wav).

main:
	sei
	
	lda   #250        ; wait for hardware
	jsr   delay_msec
	
	lda   #$01        ; enable square 1
	sta   $4015
	lda   #$bf        ; square volume = 15
	sta   $4000
	
	; First, do things that shouldn't make any sound
	
	lda   #$a1        ; square period = 7
	sta   $4001
	lda   #$07
	sta   $4002
	lda   #$00
	sta   $4003
	lda   #200
	jsr   delay_msec
	
	lda   #1          ; maximum square periods + 1
	jsr   test_cutoffs
	
	lda   #$c0        ; synchronize APU
	sta   $4017
	lda   #$89        ; sweep square of period 16 down to 7
	sta   $4001
	lda   #16
	sta   $4002
	lda   #$00
	sta   $4003
	lda   #$c0        ; clock sweep
	sta   $4017
				; period should now be 7 (silenced)
	lda   #$91        ; put sweep in add mode to catch period >= 8
	sta   $4001
	lda   #200
	jsr   delay_msec
	
	; Put noise marker between
	
	lda   #$08
	sta   $4015
	lda   #$3f
	sta   $400c
	lda   #$04
	sta   $400e
	lda   #$08
	sta   $400f
	lda   #200
	jsr   delay_msec
	lda   #$01
	sta   $4015
	
	lda   #0          ; maximum square periods
	jsr   test_cutoffs
	
	lda   #$91        ; square period = 8 should be audible
	sta   $4001
	lda   #$08
	sta   $4002
	lda   #$00
	sta   $4003
	lda   #200
	jsr   delay_msec
	
	lda   #$00        ; silence
	sta   $4015

forever:
	jmp   forever
	
test_cutoffs:
	sta   0
	ldy   #7
cutoff_loop:
	lda   cutoffs_l,y ; square period = cutoffs [y] + [0]
	clc
	adc   0
	sta   $4002
	lda   #0
	adc   cutoffs_h,y
	sta   $4003
	sty   $4001       ; sweep shift = y
	lda   #200
	jsr   delay_msec
	dey
	bpl   cutoff_loop
	rts
	
cutoffs_h:  .byte   3,  5,  6,  7,  7,  7,  7,  7
cutoffs_l:  .byte $ff,$55,$66,$1c,$87,$c1,$e0,$f0
	
	; Delay a milliseconds
	; Preserved: x, y
delay_msec:
	pha                     ; 3
	lda   #253              ; 2
	sec                     ; 2
dmslp:
	nop                     ; 2
	adc   #-2               ; 2
	bne   dmslp             ; 3
					; -1
	pla                     ; 4
	clc                     ; 2
	adc   #-1               ; 2
	bne   delay_msec        ; 3
	rts

irq:
nmi:  rti

	.org $fffa
	.word nmi
	.word main
	.word irq
