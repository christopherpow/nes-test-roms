
; NES APU Sweep Subtract Test

; Tests NES APU sweep subtract mode. When run, a continuous note
; should play (with slight periodic clicks). Half way through it should
; lower very slightly in pitch (refer to sweep_sub.wav).

main:
	sei
	
	lda   #250        ; wait for hardware
	jsr   delay_msec
	
	lda   #$c0        ; synchronize APU
	sta   $4017
	
	lda   #$01        ; enable square 1
	sta   $4015
	lda   #$bf        ; square volume = 15
	sta   $4000
	
	lda   #0          ; final low byte of period = $ff
	jsr   run_test
	
	lda   #1          ; final low byte of period = $00
	jsr   run_test
	
	lda   #$00        ; silence
	sta   $4015

forever:
	jmp   forever
	
run_test:
	sta   0
	ldy   #7
loop1:
	lda   table_l,y
	clc
	adc   0
	sta   $4002
	lda   table_h,y
	sta   $4003
	tya
	ora   #$88
	sta   $4001
	lda   #$c0        ; clock sweep
	sta   $4017
	lda   0
	clc
	adc   #1
	sta   $4003
	lda   #$00        ; disable sweep
	sta   $4001
	lda   #200
	jsr   delay_msec
	dey
	bne   loop1
	rts
	
table_h:    .byte   1,  4,  2,  2,  2,  2,  2,  2
table_l:    .byte $ff,$00,$aa,$49,$22,$10,$08,$04
	
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
