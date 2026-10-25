; full screen color tester
; Brad Smith, 2015
;
; displays a single color on the full screen
; allows emphasis / greyscale toggle
;

.feature force_range
.macpack longbranch

; iNES header
.segment "HEADER"

INES_MAPPER = 0
INES_MIRROR = 0 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG bank count
.byte $01 ; 4k CHR bank count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding


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
ppu_emphasis:  .res 1
color:         .res 1
temp:          .res 1
gamepad:       .res 1
gamepad_last:  .res 1

.segment "OAM"
.assert ((* & $FF) = 0),error,"oam not aligned to page"
oam:     .res 256

; RAM variables
.segment "BSS"

; CODE
.segment "CODE"

palette:
.byte $00,$16,$2D,$30

CX = 16
CY = 23

oam_fill:
.byte CY+(0*10), $B8, 0, CX+(2*8) ; 0
.byte CY+(0*10), $BA, 0, CX+(3*8) ; 1
.byte CY+(1*10), 'C', 0, CX+(0*8) ; 2
.byte CY+(1*10), '0', 0, CX+(2*8) ; 3 color h
.byte CY+(1*10), '0', 0, CX+(3*8) ; 4 color l
.byte CY+(2*10), $B9, 0, CX+(2*8) ; 5
.byte CY+(2*10), $BB, 0, CX+(3*8) ; 6
.byte CY+(4*10), $BA, 0, CX+(2*8) ; 7
.byte CY+(4*10), $B8, 0, CX+(3*8) ; 8
.byte CY+(4*10), $BB, 0, CX+(4*8) ; 9
.byte CY+(4*10), '+', 0, CX+(5*8) ; 10
.byte CY+(4*10), $B1, 0, CX+(6*8) ; 11
.byte CY+(5*10), 'E', 0, CX+(0*8) ; 12
.byte CY+(5*10), '0', 0, CX+(2*8) ; 13 emph b
.byte CY+(5*10), '0', 0, CX+(3*8) ; 14 emph g
.byte CY+(5*10), '0', 0, CX+(4*8) ; 15 emph r
.byte CY+(7*10), $B0, 0, CX+(2*8) ; 16
.byte CY+(8*10), 'S', 0, CX+(0*8) ; 17
.byte CY+(8*10), '0', 0, CX+(2*8) ; 18 sat
.byte CY+(10*10),'H', 0, CX+(0*8) ; 19
.byte CY+(10*10),$B2, 0, CX+(2*8) ; 20
.byte CY+(10*10),$B3, 0, CX+(3*8) ; 21
.byte CY+(10*10),$B4, 0, CX+(4*8) ; 22
; fill remainder with $FF 
.repeat 256
	.if ((* - oam_fill) < 256)
		.byte $FF
	.endif
.endrepeat

oam_tile_ch = oam + ( 3*4)+1
oam_tile_cl = oam + ( 4*4)+1
oam_tile_b  = oam + (13*4)+1
oam_tile_g  = oam + (14*4)+1
oam_tile_r  = oam + (15*4)+1
oam_tile_s  = oam + (18*4)+1

.macro PPU_LATCH addr
	lda $2002
	lda #>addr
	sta $2006
	lda #<addr
	sta $2006
.endmacro

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
			lda palette, X
			sta $2007
			inx
			cpx #4
			bcc :-
		dey
		bne :--
	; setup nametable
	PPU_LATCH $2000
	ldy #16
	:
		ldx #0
		:
			lda #0
			sta $2007
			inx
			bne :-
		dey
		bne :--
	; setup variables that don't start as 0
	lda #%00011110
	sta ppu_emphasis
	; start NMI
	lda #$80
	sta $2000
	; enter infinite loop
main_loop:
	jmp main_loop

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
	; update sprites
	lda #0
	sta $2003
	lda #>oam
	sta $4014
	; set background color
	PPU_LATCH $3F00
	lda color
	sta $2007
	; set foreground color
	cmp #$20
	bcc @dark_background
	@light_background:
		lda ppu_emphasis
		and #%00000001
		bne :+
			; if not in greyscale mode, E/F columns are all black
			lda color
			and #$0F
			cmp #$0E
			bcs @dark_background
		:
		lda #$0F
		jmp @background_chosen
	@dark_background:
		lda #$30
	@background_chosen:
	pha
	PPU_LATCH $3F13
	pla
	sta $2007
	; set scroll
	lda #0
	sta $2005
	sta $2005
	; set emphasis state
	lda ppu_emphasis
	sta $2001
	; respond to gamepad
	jsr gamepad_poll
	jsr gamepad_poll
	lda gamepad_last
	jne @gamepad_end ; wait for all buttons released
	lda gamepad
	and #(PAD_U | PAD_D | PAD_L | PAD_R | PAD_B)
	cmp #PAD_U
	bne :+
		lda color
		clc
		adc #$F0
		and #$3F
		sta color
	:
	lda gamepad
	and #(PAD_U | PAD_D | PAD_L | PAD_R | PAD_B)
	cmp #PAD_D
	bne :+
		lda color
		clc
		adc #$10
		and #$3F
		sta color
	:
	lda gamepad
	and #(PAD_U | PAD_D | PAD_L | PAD_R | PAD_B)
	cmp #PAD_L
	bne :+
		lda color
		and #$30
		sta temp
		lda color
		sec
		sbc #1
		and #$0F
		ora temp
		sta color
	:
	lda gamepad
	and #(PAD_U | PAD_D | PAD_L | PAD_R | PAD_B)
	cmp #PAD_R
	bne :+
		lda color
		and #$30
		sta temp
		lda color
		clc
		adc #1
		and #$0F
		ora temp
		sta color
	:
	lda gamepad
	and #PAD_A
	beq :+
		lda ppu_emphasis
		eor #%00000001
		sta ppu_emphasis
	:
	lda gamepad
	and #(PAD_U | PAD_D | PAD_L | PAD_R | PAD_B)
	cmp #(PAD_L | PAD_B)
	bne :+
		lda ppu_emphasis
		eor #%10000000
		sta ppu_emphasis
	:
	lda gamepad
	and #(PAD_U | PAD_D | PAD_L | PAD_R | PAD_B)
	cmp #(PAD_U | PAD_B)
	bne :+
		lda ppu_emphasis
		eor #%01000000
		sta ppu_emphasis
	:
	lda gamepad
	and #(PAD_U | PAD_D | PAD_L | PAD_R | PAD_B)
	cmp #(PAD_R | PAD_B)
	bne :+
		lda ppu_emphasis
		eor #%00100000
		sta ppu_emphasis
	:
	lda gamepad
	and #(PAD_U | PAD_D | PAD_L | PAD_R | PAD_B)
	cmp #(PAD_D | PAD_B)
	bne :+
		lda ppu_emphasis
		and #%00011111
		sta ppu_emphasis
	:
	lda gamepad
	cmp #PAD_SELECT
	bne :+
		lda ppu_emphasis
		eor #%00010100
		sta ppu_emphasis
	:
	@gamepad_end:
	lda gamepad
	and #(PAD_U | PAD_D | PAD_L | PAD_R | PAD_A | PAD_SELECT)
	sta gamepad_last
	; redraw sprites
	lda color
	lsr
	lsr
	lsr
	lsr
	ora #$A0
	sta oam_tile_ch ; color high
	lda color
	and #$0F
	ora #$A0
	sta oam_tile_cl ; color low
	lda ppu_emphasis
	rol
	rol
	and #1
	ora #$A0
	sta oam_tile_b ; emphasis B
	lda ppu_emphasis
	rol
	rol
	rol
	and #1
	ora #$A0
	sta oam_tile_g ; emphasis G
	lda ppu_emphasis
	rol
	rol
	rol
	rol
	and #1
	ora #$A0
	sta oam_tile_r ; emphasis R
	lda ppu_emphasis
	and #1
	ora #$A0
	sta oam_tile_s ; saturate
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

