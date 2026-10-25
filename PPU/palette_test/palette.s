; NTSC NES palette test
; Brad Smith, 2015
; Updated for PAL, 2018
;
; displays the NES palette
;

.if .defined(NTSC)
	.out "NTSC build selected."
	INES_TV = $00
.elseif .defined(PAL)
	.out "PAL build selected."
	INES_TV = $01
.else
	.error "NTSC or PAL must be defined."
.endif

; iNES header
.segment "HEADER"

INES_MAPPER = 0
INES_MIRROR = 0 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG bank count
.byte $01 ; 4k CHR bank count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000) | %00001000 ; iNES 2
; iNES 2
.byte $00 ; extended mapper
.byte $00 ; extended ROM size
.byte $00 ; RAM size
.byte $00 ; VRAM size
.byte INES_TV
.byte $00 ; Vs. hardware
.byte $00 ; misc.
.byte $00 ; reserved

; CHR ROM
.segment "TILES"
.incbin "test.chr"
.incbin "test.chr"

; Vectors, defined in CODE segment.
.segment "VECTORS"
.word nmi
.word reset
.word irq

; zero page variables
.segment "ZEROPAGE"
dummy:         .res 1 ; for EOR dummy (3 cycle "nop")
palette_start: .res 1 ; 0 for regular, 64 to include forbidden $0D
palette_index: .res 1 ; current index to palette data
ppu_ctrl:      .res 1 ; value to be written to $2001
ppu_emphasis:  .res 1 ; value for emphasis view
gamepad:       .res 1 ; polled gamepad
gamepad_last:  .res 1 ; last frame's gamepad
temp:          .res 1 ; temporary

.segment "OAM"
.assert ((* & $FF) = 0),error,"oam not aligned to page"
oam:     .res 256

; RAM variables
.segment "BSS"

; CODE
.segment "CODE"

palette: ; must be placed on a single page to keep timing consistent
; (0-63) without forbidden $0D
.byte $00, $10, $20, $30, $08, $18, $28, $38
.byte $01, $11, $21, $31, $09, $19, $29, $39
.byte $02, $12, $22, $32, $0A, $1A, $2A, $3A
.byte $03, $13, $23, $33, $0B, $1B, $2B, $3B
.byte $04, $14, $24, $34, $0C, $1C, $2C, $3C
.byte $05, $15, $25, $35, $0F, $1D, $2D, $3D
.byte $06, $16, $26, $36, $0E, $1E, $2E, $3E
.byte $07, $17, $27, $37, $0F, $1F, $2F, $3F
; (64-127) with forbidden $0D
.byte $00, $10, $20, $30, $08, $18, $28, $38
.byte $01, $11, $21, $31, $09, $19, $29, $39
.byte $02, $12, $22, $32, $0A, $1A, $2A, $3A
.byte $03, $13, $23, $33, $0B, $1B, $2B, $3B
.byte $04, $14, $24, $34, $0C, $1C, $2C, $3C
.byte $05, $15, $25, $35, $0D, $1D, $2D, $3D
.byte $06, $16, $26, $36, $0E, $1E, $2E, $3E
.byte $07, $17, $27, $37, $0F, $1F, $2F, $3F
.assert ((* & $FF00) = (palette & $FF00)),error,"palette data may not cross a page"

default_palette:
.byte $0F,$16,$00,$30

nametable:
.byte $00,$00,$00,$00,$00,$00,$01,$01
.byte $02,$02,$03,$03,$01,$01,$00,$00
.byte $00,$00,$02,$02,$03,$03,$01,$01
.byte $02,$02,$00,$00,$00,$00,$00,$00

CX = 88
CY = 9

oam_fill:
.byte CY+ 0, 'E', 0, CX + (0*8) ; 0
.byte CY+ 0, '1', 0, CX + (1*8) ; 1 emphasis b
.byte CY+ 0, '1', 0, CX + (2*8) ; 2 emphasis g
.byte CY+ 0, '1', 0, CX + (3*8) ; 3 emphasis r
.byte CY+ 0, 'S', 0, CX + (5*8) ; 4
.byte CY+ 0, '1', 0, CX + (6*8) ; 5 greyscale
.byte CY+ 0, 'D', 0, CX + (8*8) ; 6
.byte CY+ 0, '0', 0, CX + (9*8) ; 7 $0D
.byte CY+ 8, $BA, 0, CX + (1*8) ; 8
.byte CY+ 8, $B8, 0, CX + (2*8) ; 9
.byte CY+ 8, $BB, 0, CX + (3*8) ; 10
.byte CY+ 8, $B1, 0, CX + (6*8) ; 11
.byte CY+ 8, $B0, 0, CX + (9*8) ; 12
;.byte CY+16, $B9, 0, CX + (2*8) ; 13
; fill remainder with $FF 
.repeat 256
	.if ((* - oam_fill) < 256)
		.byte $FF
	.endif
.endrepeat

oam_tile_b = oam + (1*4)+1
oam_tile_g = oam + (2*4)+1
oam_tile_r = oam + (3*4)+1
oam_tile_s = oam + (5*4)+1
oam_tile_d = oam + (7*4)+1

attribute:
.byte $00,$00,$00,$55,$55,$99,$AA,$00

.macro PPU_LATCH addr
	lda $2002
	lda #>addr
	sta $2006
	lda #<addr
	sta $2006
.endmacro

load_nametable:
	ldy #30 ; 30 rows of tiles
	:
		ldx #0
		:
			lda nametable, X
			sta $2007
			inx
			cpx #32
			bcc :-
		dey
		bne :--
	ldy #8 ; 8 rows of attributes
	:
		ldx #0
		:
			lda attribute, X
			sta $2007
			inx
			cpx #8
			bcc :-
		dey
		bne :--
	rts

main:
	; setup sprites
	ldx #0
	:
		lda oam_fill, X
		sta oam, X
		inx
		bne :-
	; setup default palettes
	PPU_LATCH $3F00
	ldy #16
	:
		ldx #0
		:
			lda default_palette, X
			sta $2007
			inx
			cpx #4
			bcc :-
		dey
		bne :--
	; setup nametable
	PPU_LATCH $2000
	jsr load_nametable
	jsr load_nametable
	jsr load_nametable
	jsr load_nametable
	; setup variables that don't start as 0
	lda #%11101010
	sta ppu_emphasis
	; start NMI
	lda #$80
	sta $2000
	; enter infinite loop
main_loop:
	.repeat 509 ; large number of nops in loop to reduce NMI jitter
		nop
	.endrepeat
	jmp main_loop

; for guaranteed branch timing, directly follows branch instruction
.macro assert_branch_page label_
   .assert >(label_) = >(*), error, "Page crossing detected!"
.endmacro

; timed delays, includes jsr and rts in cycle count
delay_16:
	nop
	nop
	rts
delay_32:
	jsr delay_16
	nop
	nop
	rts
delay_64:
	jsr delay_32
	jsr delay_16
	nop
	nop
	rts
delay_128:
	jsr delay_64
	jsr delay_32
	jsr delay_16
	nop
	nop
	rts
delay_256:
	jsr delay_128
	jsr delay_64
	jsr delay_32
	jsr delay_16
	nop
	nop
	rts

.macro delay_pal4
	.if .defined(PAL)
		nop
		nop
	.endif
.endmacro

.macro delay_pal3
	.if .defined(PAL)
		eor dummy
	.endif
.endmacro

swap_palette_0:
	; load next 3 palette colors onto stack
	ldy palette_index
	lda palette+0, Y
	pha
	lda palette+1, Y
	pha
	lda palette+2, Y
	pha
	iny
	iny
	iny
	sty palette_index
	nop
	nop
	nop
	nop
	nop
	.if .defined(NTSC)
		nop
		nop
		nop
	.elseif .defined(PAL)
	.endif
	PPU_LATCH $3F00
	pla
	tay
	pla
	tax
	pla
	; pixel 263+
	bit $2007
	sta $2007
	stx $2007
	sty $2007
	rts

swap_palette_0_start:
	ldy palette_start
	lda palette+0, Y
	pha
	lda palette+1, Y
	pha
	lda palette+2, Y
	pha
	iny
	iny
	iny
	sty palette_index
	nop
	nop
	nop
	nop
	nop
	.if .defined(NTSC)
		nop
		nop
		nop
	.elseif .defined(PAL)
	.endif
	PPU_LATCH $3F00
	pla
	tay
	pla
	tax
	pla
	; pixel 263+
	bit $2007
	sta $2007
	stx $2007
	sty $2007
	rts

swap_palette_1:
	ldy palette_index
	lda palette+0, Y
	pha
	lda palette+1, Y
	pha
	lda palette+2, Y
	pha
	iny
	iny
	iny
	sty palette_index
	pla
	tay
	pla
	tax
	pla
	.if .defined(NTSC)
		jsr delay_32
	.elseif .defined(PAL)
		jsr delay_16
		eor dummy
		nop
		nop
		nop
	.endif
	nop
	nop
	; pixel 261+
	bit $2007
	sta $2007
	stx $2007
	sty $2007
	rts

swap_palette_2:
	ldy palette_index
	lda palette+0, Y
	pha
	lda palette+1, Y
	pha
	lda #0
	pha
	iny
	iny
	sty palette_index
	pla
	tay
	pla
	tax
	pla
	.if .defined(NTSC)
		jsr delay_32
	.elseif .defined(PAL)
		jsr delay_16
		eor dummy
		nop
		nop
		nop
	.endif
	nop
	nop
	nop
	nop
	; pixel 259+
	bit $2007
	sta $2007
	stx $2007
	sty $2007
	rts

nmt_on:
	lda #0
	sta $2006
	sta $2005
	sta $2005
	sta $2006
	.if .defined(NTSC)
		jsr delay_64
		eor dummy
	.elseif .defined(PAL)
		jsr delay_32
		jsr delay_16
		nop
		nop
		nop
		nop
		nop
		nop
	.endif
	lda ppu_ctrl
	; pixel 260+
	sta $2001
	rts

nmt_on_start_emph:
	lda #0
	sta $2006
	sta $2005
	sta $2005
	sta $2006
	jsr delay_32
	jsr delay_16
	nop
	nop
	nop
	.if .defined(NTSC)
		nop
		nop
		eor dummy
	.elseif .defined(PAL)
	.endif
	lda ppu_emphasis
	sta ppu_ctrl
	lda ppu_ctrl
	; pixel 260+
	sta $2001
	rts

wait_line: ; including jsr: waits 1 scanline - 2 pixels
	jsr delay_64
	.if .defined(NTSC)
		jsr delay_32
		nop
		eor dummy
	.elseif .defined(PAL)
		jsr delay_16
		nop
		nop
		nop
		nop
		nop
		nop
		nop
	.endif
	rts

nmt_off:
	lda #%00000000
	jsr delay_64
	.if .defined(NTSC)
		jsr delay_16
	.elseif .defined(PAL)
		eor dummy
		nop
		nop
		nop
		nop
	.endif
	; pixel 265+
	sta $2001
	rts

render_palette_row: ; exactly 12 scanlines (4 blank to set palette, 8 rendering)
	jsr swap_palette_0
	jsr swap_palette_1
	jsr swap_palette_2
	jsr nmt_on
	jsr wait_line
	jsr wait_line
	jsr wait_line
	jsr wait_line
	jsr wait_line
	jsr wait_line
	jsr wait_line
	jsr delay_16
	.if .defined(NTSC)
		nop
		nop
		nop
		nop
	.elseif .defined(PAL)
		; on PAL this routine loses 12 pixels
		; this is compensated by an external delay of 4,3,4,4 cycles
		; 4+3+4+4 = 15 cycles * 3.2 pixels/cycle = 48 pixels (divided over 4 rows * -12 pixels)
	.endif
	jsr nmt_off
	rts

render_palette_row_start_emph: ; exactly 12 scanlines (4 blank to set palette, 8 rendering)
	jsr swap_palette_0_start
	jsr swap_palette_1
	jsr swap_palette_2
	jsr nmt_on_start_emph
	jsr wait_line
	jsr wait_line
	jsr wait_line
	jsr wait_line
	jsr wait_line
	jsr wait_line
	jsr wait_line
	jsr delay_16
	.if .defined(NTSC)
		nop
		nop
		nop
		nop
	.elseif .defined(PAL)
		; see note in render_palette_row above
	.endif
	jsr nmt_off
	rts

PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80

gamepad_poll:
	lda #1
	sta $4016
	lda #0
	sta $4016
	ldx #8
	:
		pha
		lda $4016
		and #%00000011
		cmp #%00000001
		pla
		ror
		dex
		bne :-
	sta gamepad
	lda gamepad
	rts

nmi:
	lda #0
	sta $2003
	lda #>oam
	sta $4014
	lda #%00010100 ; show sprites at top
	sta $2001
	; delay
	.if .defined(NTSC)
		ldx #18
		:
			jsr delay_256
			dex
			bne :-
			assert_branch_page :-
		jsr delay_128
		jsr delay_64
		jsr delay_16
		eor dummy
		nop
		nop
	.elseif .defined(PAL)
		jsr delay_128
		jsr delay_64
		jsr delay_32
		jsr delay_16
		nop
		ldx #37
		:
			jsr delay_256
			dex
			bne :-
			assert_branch_page :-
	.endif
	; now at beginning of scanline 27
	jsr nmt_off
	; regular palette
	lda #%00001010
	sta ppu_ctrl
	eor dummy
	nop
	jsr render_palette_row ; scanline 27 pixel 325+
	delay_pal4
	jsr render_palette_row
	delay_pal3
	jsr render_palette_row
	delay_pal4
	jsr render_palette_row
	delay_pal4
	jsr render_palette_row
	delay_pal4
	jsr render_palette_row
	delay_pal3
	jsr render_palette_row
	delay_pal4
	jsr render_palette_row
	delay_pal4
	; emphasized palette
	jsr render_palette_row_start_emph
	delay_pal4
	jsr render_palette_row
	delay_pal3
	jsr render_palette_row
	delay_pal4
	jsr render_palette_row
	delay_pal4
	jsr render_palette_row
	delay_pal4
	jsr render_palette_row
	delay_pal3
	jsr render_palette_row
	delay_pal4
	jsr render_palette_row
	; rendering finished
	lda #%00000000
	sta $2001
	; setup for next frame
	jsr gamepad_poll
	lda gamepad_last
	bne @gamepad_end ; wait for all buttons released
	lda gamepad
	cmp #PAD_L
	bne :+
		lda ppu_emphasis
		eor #%10000000
		sta ppu_emphasis
		jmp @gamepad_end
	:
	lda gamepad
	cmp #PAD_U
	bne :+
		lda ppu_emphasis
		eor #%01000000
		sta ppu_emphasis
		jmp @gamepad_end
	:
	lda gamepad
	cmp #PAD_R
	bne :+
		lda ppu_emphasis
		eor #%00100000
		sta ppu_emphasis
		jmp @gamepad_end
	:
	lda gamepad
	cmp #PAD_D
	bne :+
		lda ppu_emphasis
		and #%00011111
		sta ppu_emphasis
		jmp @gamepad_end
	:
	lda gamepad
	cmp #PAD_B
	bne :+
		lda ppu_emphasis
		eor #%00000001
		sta ppu_emphasis
		jmp @gamepad_end
	:
	lda gamepad
	cmp #PAD_A
	bne :+
		lda palette_start
		eor #64
		sta palette_start
		jmp @gamepad_end
	:
	@gamepad_end:
	lda gamepad
	sta gamepad_last
	; redraw sprites
	lda oam_tile_b
	and #%11111110
	sta temp
	lda ppu_emphasis
	rol
	rol
	and #1
	ora temp
	sta oam_tile_b
	lda oam_tile_g
	and #%11111110
	sta temp
	lda ppu_emphasis
	rol
	rol
	rol
	and #1
	ora temp
	sta oam_tile_g
	lda oam_tile_r
	and #%11111110
	sta temp
	lda ppu_emphasis
	rol
	rol
	rol
	rol
	and #1
	ora temp
	sta oam_tile_r
	lda oam_tile_s
	and #%11111110
	sta temp
	lda ppu_emphasis
	and #1
	ora temp
	sta oam_tile_s
	lda oam_tile_d
	and #%11111110
	sta temp
	lda palette_start
	rol
	rol
	rol
	and #1
	ora temp
	sta oam_tile_d
	; reload palette index for next frame
	ldy palette_start
	sty palette_index
	; debug timing with grey
	;lda #%00011111
	;sta $2001
	; wait until next frame
	rti

irq:
	rti

reset:
	sei
	cld
	ldx #$40
	stx $4017
	ldx $ff
	txs
	ldx #$00
	stx $2000
	stx $2001
	stx $4010
	bit $2002
	:
		bit $2002
		bpl :-
	lda #$00
	tax
	:
		sta $0000, X
		sta $0100, X
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	:
		bit $2002
		bpl :-
	jmp main

