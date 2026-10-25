
; NES APU Triangle Linear Counter Test

; Tests triangle's linear counter and clocking by the frame counter.
; When run, there should be silence (with a few slight pops or clicks),
; a noise percussion sound, then a continuous triangle wave without any
; breaks of silence (with occasional slight clicks). Refer to lin_ctr.wav.

; Shay Green <mail.com@blargg>

	; delay routines first to avoid page-crossing that would affect timing
	
	; Delay a milliseconds
	; Preserved: x, y
delay_msec:                   ; 1788 cycles * a; 0.999009524 msec
	pha                     ; 3
	lda   #253              ; 2
	sec                     ; 2
dmslp:
	nop                     ; *2
	adc   #-2               ; *2
	bne   dmslp             ; *3
					; -1
	pla                     ; 4
	clc                     ; 2
	adc   #-1               ; 2
	bne   delay_msec        ; 3
	rts

	; Delay a/10 milliseconds (i.e. a = 10 delays 1 msec).
	; Preserved: x, y
delay_01msec:                 ; 178 cycles * a; 0.099453968 msec
	pha                     ; 3
	lda   #23               ; 2
	sec                     ; 2
d01lp:
	nop                     ; *2
	adc   #-2               ; *2
	bne   d01lp             ; *3
					; -1
	pla                     ; 4
	clc                     ; 2
	adc   #-1               ; 2
	bne   delay_01msec      ; 3
	rts


main:
	sei
	
	lda   #250        ; wait for hardware
	jsr   delay_msec
	
	lda   #0          ; disable nmi
	sta   $2000
	lda   #$00        ; silence everything
	sta   $4015
	lda   #$40        ; 4-step frame mode, synchronize
	sta   $4017
	lda   #$20        ; period = $20
	sta   $400a
	lda   #$04        ; enable triangle
	sta   $4015
	
	; Do things which shouldn't make any sound
	
	lda   #$00        ; linear reload = $00
	sta   $4008
	lda   #$08        ; schedule linear reload on next clock
	sta   $400b
	lda   #$c0        ; clock linear counter
	sta   $4017
	lda   #$7f        ; linear reload = $7f
	sta   $4008
	lda   #200        ; won't get reloaded
	jsr   delay_msec
	lda   #$c0        ; synchronize APU
	sta   $4017
	lda   #$08        ; schedule linear reload on next clock
	sta   $400b
	ldx   #50         ; suspend linear clocks
	jsr   suspend_apu
	
	lda   #$00        ; linear reload = $00
	sta   $4008
	lda   #$c0        ; clock linear counter; will get reloaded with 0
	sta   $4017
	lda   #$01        ; linear reload = $01
	sta   $4008
	lda   #$08        ; schedule linear reload on next clock
	sta   $400b
	lda   #$c0        ; clock linear counter; will get reloaded with 1
	sta   $4017
	sta   $4017       ; clock linear counter; will decrement to 0
	ldx   #50         ; suspend linear clocks
	jsr   suspend_apu
	
	lda   #$c0        ; synchronize apu
	sta   $4017
	lda   #$ff        ; linear reload = $7f, control = 1
	sta   $4008
	lda   #200        ; counter won't get reloaded
	jsr   delay_msec
	lda   #$c0        ; synchronize apu
	sta   $4017
	lda   #$08        ; schedule linear reload on next clock
	sta   $400b
	ldx   #50         ; suspend linear clocks; won't get reloaded
	jsr   suspend_apu
	lda   #$c0        ; clock counter; loads with $7f, halt stays set
	sta   $4017
	lda   #$01        ; linear reload = $01, control = 0
	sta   $4008
	lda   #$c0        ; clock counter; loads with $01, clears halt
	sta   $4017
	sta   $4017       ; clock counter; decrements to 0
	ldx   #50         ; suspend linear clocks; should already be 0 (not $7f)
	jsr   suspend_apu
	lda   #$7f        ; linear reload = $7f, control = 0
	sta   $4008
	lda   #200        ; won't get reloaded because halt is clear
	jsr   delay_msec
	lda   #$ff        ; linear reload = $7f, control = 1
	sta   $4008
	lda   #200        ; won't get reloaded because halt is clear
	jsr   delay_msec
	
	; Check frame counter clocking
	ldy   #9
frame_loop:
	ldx   #$c0        ; synchronize apu
	stx   $4017
	lda   count_table,y ; linear reload value + 2
	clc
	adc   #2
	sta   $4008
	lda   #$18        ; schedule reload on next clock; length counter = 2
	sta   $400b
	stx   $4017       ; decrement length counter to 0
	stx   $4017
	lda   seq_table,y ; start 4-frame or 5-frame sequence
	sta   $4017
	lda   delay_table,y
	jsr   delay_01msec; wait until just after linear counter should be 0
	lda   #$7f        ; linear counter reload = $7f
	sta   $4008
	lda   #$08        ; length counter = $fe
	sta   $400b
	ldx   #50         ; linear counter won't be reloaded
	jsr   suspend_apu
	dey
	bpl   frame_loop
	lda   #$00        ; linear counter reload = 0
	sta   $4008
	
	jmp   noise_marker

count_table:
	.byte   1,   2,   3,   4,   5,        1,   2,   3,   4,   5
seq_table:
	.byte $40, $40, $40, $40, $40,      $c0, $c0, $c0, $c0, $c0
delay_table:
	.byte  84, 126, 168, 210, 252,       42,  84, 126, 210, 252
	
noise_marker:
	; Put noise marker between
	
	lda   #200
	jsr   delay_msec
	
	lda   #$02
	sta   $400c
	lda   #$08
	sta   $4015
	sta   $400e
	sta   $400f
	lda   #250
	jsr   delay_msec
	
	; Do things which shouldn't leave any silence
	
	lda   #$04        ; enable triangle
	sta   $4015
	jsr   play_triangle     ; start with triangle that probably works
	
	; Check frame counter clocking
	ldy   #9
frame_loop2:
	lda   #$40        ; synchronize apu
	sta   $4017
	lda   count_table,y ; linear reload value + 1
	sta   $4008
	lda   #$08        ; schedule reload on next clock; length = $fe
	sta   $400b
	lda   seq_table,y ; start 4-frame or 5-frame sequence
	sta   $4017
	lda   delay_table,y
	clc
	adc   #-1
	jsr   delay_01msec; wait until just after linear counter should be 1
	ldx   #50         ; linear counter won't be reloaded
	jsr   suspend_apu
	dey
	bpl   frame_loop2
	
	lda   #$c0        ; synchronize apu
	sta   $4017
	lda   #$01        ; linear reload = $01, control = 0
	sta   $4008
	lda   #$08        ; schedule linear reload on next clock
	sta   $400b
	lda   #$c0        ; clock linear counter; will get reloaded with $01
	sta   $4017
	ldx   #50         ; suspend linear clocks; won't get reloaded
	jsr   suspend_apu
	lda   #$7f        ; linear reload = $7f, control = 0
	sta   $4008
	lda   #$08        ; schedule linear reload on next clock
	sta   $400b
	ldx   #$7f        ; first clock loads with $7f, rest decrement to 1
	jsr   clock_counter
	ldx   #50         ; suspend linear clocks; won't get reloaded
	jsr   suspend_apu
	
	lda   #$00        ; linear reload = $00, control = 0
	sta   $4008
	lda   #$08        ; schedule linear reload on next clock
	sta   $400b
	lda   #$80        ; linear reload = $00, control = 1
	sta   $4008
	lda   #$c0        ; clock counter; reloads with $00, halt stays set
	sta   $4017
	lda   #$81        ; linear reload = $00, control = 0
	sta   $4008
	lda   #$81        ; linear reload = $01, control = 1
	sta   $4008
	lda   #$c0        ; clock linear counter; will get reloaded with $01
	sta   $4017
	lda   #250        ; counter won't decrement because control = 1
	jsr   delay_msec
	lda   #$c0        ; synchronize
	sta   $4017
	lda   #$00        ; linear reload = $00, control = 0
	sta   $4008
	ldx   #50         ; suspend linear clocks; no reload or change of halt
	jsr   suspend_apu
	lda   #$7f        ; linear reload = $7f, control = 0
	sta   $4008
	lda   #$c0        ; clock linear counter; will get reloaded with $7f
	sta   $4017
	lda   #$01        ; linear reload = $01, control = 0
	sta   $4008
	lda   #250        ; counter won't get reloaded with 1
	jsr   delay_msec
	
	; End with triangle playing so if last test fails it will be noticed
	jsr   play_triangle
	
	lda   #0
	sta   $4015
	
forever:
	jmp   forever
	;lda  #250
	;jsr  delay_msec
	;jmp  $ff04

play_triangle:
	lda   #$c0        ; synchronize
	sta   $4017
	lda   #$04        ; enable triangle
	sta   $4015
	lda   #$ff        ; linear reload = $7f, control = 1
	sta   $4008
	lda   #$08        ; schedule linear reload on next clock
	sta   $400b
	lda   #$c0        ; clock linear counter
	sta   $4017
	lda   #200
	jsr   delay_msec
	rts
	
clock_counter:
	lda   #$c0        ; clock immediately
	sta   $4017
	dex
	bne   clock_counter
	rts
	
suspend_apu:
	lda   #$40        ; delay next clock
	sta   $4017
	lda   #40         ; wait almost 1/240 second
	jsr   delay_01msec
	dex
	bne   suspend_apu
	rts
	
irq:
nmi:  rti

	.org $fffa
	.word nmi
	.word main
	.word irq
