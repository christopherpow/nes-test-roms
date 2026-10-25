.include "defines.inc"
.include "global.inc"

.segment "BANK0"
bg_00:
	.incbin "../gfx/bg_00.pal"

palette_tableLo:
	.byte <bg_00
palette_tableHi:
	.byte >bg_00

.include "../gfx/text_nametable.asm"

nametable_tableLo:
	.byte $00
	.byte <text_nametable
nametable_tableHi:
	.byte $00
	.byte >text_nametable

; these subroutines have to do with palettes and nametables
; temp_16_1:				address of nametable/palette data
; temp_8_0:					which nametable to draw/which palette set to write to
; palette_state:			index of all palettes
; nametable_state:			index of all nametables. 0 means to clear nametable

.proc load_palette
; loads a palette set to the shadow palette registers
; base palette set is chosen by temp_8_0; 0=BG, 1=FG
	LDY palette_state
	LDA palette_tableLo,y
	STA temp_16_1
	LDA palette_tableHi,y
	STA temp_16_1 + 1

	LDY #$00
	LDA temp_8_0
	ASL A
	ASL A
	ASL A
	ASL A
	TAX
	:
	LDA (temp_16_1), Y
	STA palette_table, X
	INY
	INX
	CPY #$10
	BNE :-

	RTS
.endproc

.proc write_palette
; copies the palette table to PPU
	LDA #0
	STA PPUMASK				; disable rendering
	STA PPUCTRL				; writes to PPUDATA will increment by 1 to the next PPU address

	LDA PPUSTATUS
	; address of palettes, $3F00
	LDA #$3F
	STA PPUADDR
	LDA #$00
	STA PPUADDR

	LDY #$00
	:
	LDA palette_table, Y
	STA PPUDATA
	INY
	CPY #$20					; #$20 entries per palette
	BNE :-

	LDA s_PPUCTRL				; enable NMI
	STA PPUCTRL

	RTS
.endproc

.proc load_nametable
; loads a single-screen nametable at where temp_16_1 points
; base nametable is chosen by temp_8_0
	LDA #0
	STA PPUMASK				; disable rendering
	STA PPUCTRL				; writes to PPUDATA will increment by 1 to the next PPU address

	LDA nametable_state
	CMP $00
	BNE :+
	JSR clear_nametable
	RTS
:
	LDY nametable_state
	LDA nametable_tableLo,y
	STA temp_16_1
	LDA nametable_tableHi,y
	STA temp_16_1 + 1

	LDA PPUSTATUS
	; address of base nametable (chosen by temp_8_0)
	LDA temp_8_0
	ASL A
	ASL A
	ADC #$20
	STA PPUADDR
	LDA #$00
	STA PPUADDR

	LDY #0
	LDX #4
:
	LDA (temp_16_1), Y 
	STA PPUDATA
	INY
	BNE :-
	INC temp_16_1 + 1
	DEX
	BNE :-

	LDA s_PPUCTRL				; enable NMI
	STA PPUCTRL

	RTS
.endproc

.proc clear_nametable
; clears a given nametable chosen by temp_8_0
	LDA #0
	STA PPUMASK					; disable rendering
	STA PPUCTRL					; writes to PPUDATA will increment by 1 to the next PPU address

	LDA PPUSTATUS
	LDA temp_8_0
	ASL A
	ASL A
	ADC #$20
	STA PPUADDR
	LDA #$00
	STA PPUADDR

	LDA #0
	TAY
	LDX #4
:
	STA PPUDATA
	INY
	BNE :-
	DEX
	BNE :-

	LDA s_PPUCTRL				; enable NMI
	STA PPUCTRL

	RTS
.endproc

.proc display_registers
; read from test registers and display their values
; base nametable is chosen by temp_8_0
CHR_OFFSET = $30
	LDA #0
	STA PPUMASK					; disable rendering
	STA PPUCTRL					; writes to PPUDATA will increment by 1 to the next PPU address

	; write PU1 status
	LDA PPUSTATUS
	LDA temp_8_0
	ASL A
	ASL A
	CLC
	ADC #$20
	ADC #$01
	STA PPUADDR
	LDA #$5A					; to PU1 nametable offset
	STA PPUADDR
	LDA #$30					; value 0
	STA PPUDATA
	LDA TEST_DAC_PU1PU2			; load value
	AND #$0F					; get low nybble
	CLC
	ADC #CHR_OFFSET				; offset to match CHR tile
	STA PPUDATA
	
	; write PU2 status
	LDA PPUSTATUS
	LDA temp_8_0
	ASL A
	ASL A
	CLC
	ADC #$20
	ADC #$01
	STA PPUADDR
	LDA #$9A					; to PU2 nametable offset
	STA PPUADDR
	LDA #$30					; value 0
	STA PPUDATA
	LDA TEST_DAC_PU1PU2
	LSR							; get high nybble
	LSR
	LSR
	LSR
	CLC
	ADC #CHR_OFFSET				; offset to match CHR tile
	STA PPUDATA
	
	; write TRI status
	LDA PPUSTATUS
	LDA temp_8_0
	ASL A
	ASL A
	CLC
	ADC #$20
	ADC #$01
	STA PPUADDR
	LDA #$DA					; to TRI nametable offset
	STA PPUADDR
	LDA #$30					; value 0
	STA PPUDATA
	LDA TEST_DAC_TRINOI			; load value
	AND #$0F					; get low nybble
	CLC
	ADC #CHR_OFFSET				; offset to match CHR tile
	STA PPUDATA
	
	; write NOI status
	LDA PPUSTATUS
	LDA temp_8_0
	ASL A
	ASL A
	CLC
	ADC #$20
	ADC #$02
	STA PPUADDR
	LDA #$1A					; to NOI nametable offset
	STA PPUADDR
	LDA #$30					; value 0
	STA PPUDATA
	LDA TEST_DAC_TRINOI
	LSR
	LSR
	LSR
	LSR							; get high nybble
	CLC
	ADC #CHR_OFFSET				; offset to match CHR tile
	STA PPUDATA
	
	; write DMC status
	LDA PPUSTATUS
	LDA temp_8_0
	ASL A
	ASL A
	CLC
	ADC #$20
	ADC #$02
	STA PPUADDR
	LDA #$5A					; to DMC nametable offset
	STA PPUADDR
	LDA TEST_DAC_DPCM			; load value
	STA temp_8_1				; write to scratch variable; value may change over time!
	LSR A
	LSR A
	LSR A
	LSR A						; get high nybble
	CLC
	ADC #CHR_OFFSET				; offset to match CHR tile
	STA PPUDATA
	LDA temp_8_1
	AND #$0F					; get low nybble
	CLC
	ADC #CHR_OFFSET				; offset to match CHR tile
	STA PPUDATA

	LDA s_PPUCTRL				; enable NMI
	STA PPUCTRL
	RTS
.endproc

; bugs the PPU to update the scroll position
.proc update_scrolling
	BIT PPUSTATUS
	LDA ppu_scroll_x
	STA PPUSCROLL
	LDA ppu_scroll_y
	STA PPUSCROLL
	RTS
.endproc