; This code tests a NES emulator's APU envelope for correctness.
; When run, a tone should slowly fade in and stay on (refer to
; test_apu_env.wav for correct output).

main:
	sei
	
	lda   #255        ; wait for hardware
	jsr   delay_01msec
	
	lda   #$01        ; enable square
	sta   $4015
	
	lda   #$c0        ; reset sequencer and clock envelope now
	sta   $4017
	
	lda   #$c0        ; square period
	sta   $4002
	
	ldy   #16
loop:
	lda   #$08        ; schedule envelope reset on next clock
	sta   $4003
	lda   #$20        ; enable envelope now, loop
	sta   $4000
	
	ldx   #80
wlp:  lda   #$40        ; reset sequencer without clocking envelope
	sta   $4017
	lda   #39         ; wait until just before envelope clock
	jsr   delay_01msec
	dex
	bne   wlp
	
	lda   #$10        ; volume = 0, disable envelope
	sta   $4000
	
	tya               ; clock envelope 'y' times
	tax
rlp:  lda   #$c0        ; reset sequencer and clock envelope now
	sta   $4017
	dex
	bne   rlp
	
	dey
	bne   loop
	
	lda   #$1f        ; volume = 15, disable envelope
	sta   $4000
	
	jmp   exit
	
delay_01msec:
	pha                     ; 3
	lda   #18               ; 2
	sec                     ; 2
dlp:  nop                     ; 2
	nop                     ; 2
	adc   #-2               ; 2
	bne   dlp               ; 3
					; -1
	pla                     ; 4
	clc                     ; 2
	adc   #-1               ; 2
	bne   delay_01msec      ; 3
	rts

exit:
	jmp   exit

irq:
nmi:  rti

.org $fffa
	.word nmi
	.word main
	.word irq
