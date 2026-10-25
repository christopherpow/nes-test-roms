.include "defines.inc"
.include "global.inc"

.segment "BANK0"
.proc NMI_HANDLER			; push regs onto stack to preserve them and we can pop them off later
	PHA
	TYA
	PHA
	TXA
	PHA
	
	LDA #0
	STA PPUMASK				; disable rendering

	; transfer OAM data from $02xx
	; to get into a good alignment
	LDA #0
	STA OAMADDR
	LDA #$02
	STA OAMDMA

	;	requirements:
	;	1.	something to capture pin 1 and pin 2's output, preferrably a good
	;		ADC or oscilloscope which can be triggered to capture samples using
	;		an external clock
	;	2.	a switch on pin 30 to set test mode
	;	3.	figure out x and y

	; check if already done
	LDA system_state
	AND #STATUS_PU1PU2
	BNE @FinishedPU1PU2

	;	1.	iterate through 256 possible combinations of the two pulse
	;		channels. this can be achieved by resetting phase, play a 3/4
	;		waveform at the lowest note, and returning to silence after x
	;		amount of cycles.
	JSR iterate_Pu1Pu2
@FinishedPU1PU2:
	LDA system_state
	AND #STATUS_TRINOIDMC
	BNE @FinishedTRINOIDMC
	;	2.	iterate through 32,768 possible combinations of triangle, noise of
	;		dpcm by setting triangle and dpcm dac levels, and letting noise run
	;		at y pitch to compensate for near-random phase, then return to
	;		silence after x amount of cycles.
	JSR iterate_TriNoiDMC

@FinishedTRINOIDMC:

	; display test register values
	LDA #0
	STA temp_8_0
	JSR display_registers

	; update scrolling
	JSR update_scrolling

	; update mask and ctrl state
	LDA s_PPUMASK
	STA PPUMASK
	LDA s_PPUCTRL
	STA PPUCTRL

	INC framecounter

	PLA
	TAX
	PLA
	TAY
	PLA
	RTI
.endproc

.proc iterate_Pu1Pu2
	LDA DAC_LEVEL_PU2
	CMP #$10
	BEQ @Finish

	; write to pulse registers
	LDA #$F0
	ORA DAC_LEVEL_PU1
	STA $4000
	LDA #$F0
	ORA DAC_LEVEL_PU2
	STA $4004
	
	LDA DAC_LEVEL_PU1
	CMP #$0F
	BNE :+
	LDA #0
	STA DAC_LEVEL_PU1
	INC DAC_LEVEL_PU2
	RTS
:
	INC DAC_LEVEL_PU1
	RTS

@Finish:
	; finished iterating
	LDA system_state
	ORA #STATUS_PU1PU2
	STA system_state

	; write to pulse registers
	; duty 3/4, muted
	LDA #$F0
	STA $4000
	STA $4004

	RTS
.endproc

.proc iterate_TriNoiDMC
	LDA DAC_LEVEL_DMC
	CMP #$80
	BEQ @Finish
	
	; DPCM level
	LDA DAC_LEVEL_DMC
	STA $4011

	; write to registers
	; noise level
	LDA #$30
	ORA DAC_LEVEL_NOI
	STA $400C

	; triangle sequence
	LDA #%10000000
	ORA DAC_SEQ_TRI
	STA TEST_TRI_SET
	
	; increment counters
	LDA DAC_SEQ_TRI
	CMP #$1F
	BNE :+
	LDA #$10
	STA DAC_SEQ_TRI
	INC DAC_LEVEL_NOI
	JMP :++
:
	INC DAC_SEQ_TRI
:
	LDA DAC_LEVEL_NOI
	CMP #$10
	BNE :+
	LDA #0
	STA DAC_LEVEL_NOI
	INC DAC_LEVEL_DMC
	RTS
:
	RTS

@Finish:
	; finished iterating
	LDA system_state
	ORA #STATUS_TRINOIDMC
	STA system_state
	
	; noise level
	LDA #$30
	STA $400C
	
	; DPCM level
	LDA #$00
	STA $4011

	; triangle sequence
	LDA #%10000000
	STA TEST_TRI_SET
	RTS
.endproc
