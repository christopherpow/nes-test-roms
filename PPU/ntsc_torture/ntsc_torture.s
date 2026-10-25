; NTSC NES pattern torture test
; Brad Smith, 2016
;
; displays patterns designed to expose and examine NES NTSC artifacts
;

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
dummy:         .res 1 ; for EOR dummy (3 cycle "nop")
gamepad:       .res 1 ; polled gamepad
gamepad_last:  .res 1
gamepad_rep:   .res 1
gamepad_rep2:  .res 1
temp:          .res 2 ; temporary
scroll_x:      .res 1
scroll_y:      .res 1
mode:          .res 1
color_bg:      .res 1
color_fg:      .res 1
color_anti:    .res 1
repframes:     .res 1

.segment "OAM"
.assert ((* & $FF) = 0),error,"oam not aligned to page"
oam:     .res 256

; RAM variables
.segment "BSS"

; CODE
.segment "CODE"

nametable:
.incbin "torture.nam"

palette:
.byte $0F, $16, $00, $30

CX = 25 * 8
CY = (27 * 8) - 3

oam_fill:
.byte CY+ 0, '0', 0, CX + (0*8) ; scroll X / BG
.byte CY+ 0, '0', 0, CX + (1*8) ;
.byte CY+ 0, ',', 0, CX + (2*8) ; separator
.byte CY+ 0, '0', 0, CX + (3*8) ; scroll Y / FG
.byte CY+ 0, '0', 0, CX + (4*8) ;
; fill remainder with $FF 
.repeat 256
	.if ((* - oam_fill) < 256)
		.byte $FF
	.endif
.endrepeat

oam_tile_0 = oam + (0*4)+1
oam_tile_1 = oam + (1*4)+1
oam_tile_2 = oam + (2*4)+1
oam_tile_3 = oam + (3*4)+1
oam_tile_4 = oam + (4*4)+1

.macro PPU_LATCH addr
	lda $2002
	lda #>addr
	sta $2006
	lda #<addr
	sta $2006
.endmacro

load_nametable:
	.repeat 4, I
		ldx #0
		:
			lda nametable+(256*I), X
			sta $2007
			inx
			bne :-
	.endrepeat
	rts

set_anti_color:
	lda color_bg
	and #$0F
	cmp #$0E
	bcs @dark_bg
	lda color_bg
	cmp #$20
	bcc @dark_bg
@light_bg:
	lda #$0F
	sta color_anti
	lda color_fg
	and #$0F
	cmp #$03
	bcs :+
	lda color_fg
	cmp #$20
	bcc :+
		rts
	:
	; FG is in row $00/$10 or black, pick a complementary row $10 colour
	and #$0F
	clc
	adc #6
	cmp #13
	bcc :+
		sec
		sbc #12
	:
	ora #$10
	sta color_anti
	rts
@dark_bg:
	lda #$30
	sta color_anti
	lda color_fg
	cmp #$20
	bcs :+
		rts
	:
	; FG is in row $20/30, pick a complementary row $20 colour
	and #$0F
	clc
	adc #6
	cmp #13
	bcc :+
		sec
		sbc #12
	:
	ora #$20
	sta color_anti
	rts

init:
	lda #0
	sta mode
	sta scroll_x
	sta scroll_y
	lda #$0F
	sta color_bg
	lda #$30
	sta color_fg
	jsr set_anti_color
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
			lda palette, X
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
	jsr init
	; start NMI
	lda #$80
	sta $2000
	; enter infinite loop
main_loop:
	.repeat 509 ; large number of nops in loop to reduce NMI jitter
		nop
	.endrepeat
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
	lda #0
	sta $2003
	lda #>oam
	sta $4014
	lda #%00011110
	sta $2001
	; palettes
	PPU_LATCH $3F00
	lda color_bg
	sta $2007
	PPU_LATCH $3F03
	lda color_fg
	sta $2007
	PPU_LATCH $3F13
	lda color_anti
	sta $2007
	; scroll
	lda $2002
	lda scroll_x
	sta $2005
	lda scroll_y
	sta $2005
interact_modes:
	jsr gamepad_poll
	and #(PAD_SELECT | PAD_START)
	sta temp
	lda gamepad_last
	and #(PAD_SELECT | PAD_START)
	cmp temp
	beq @ss_end ; filter SELECT/START only on keydown
		lda gamepad
		and #PAD_SELECT
		beq @select_end
			; change mode (0=scroll, 1=BG, 2=FG)
			inc mode
			lda mode
			cmp #3
			bcc :+
				lda #0
				sta mode
			:
		@select_end:
		lda gamepad
		and #PAD_START
		beq @start_end
			jsr init
		@start_end:
	@ss_end:
	lda gamepad
	sta gamepad_last
	; don't need anything but dpad now
	and #(PAD_L | PAD_R | PAD_U | PAD_D)
	sta gamepad
	; mode based input, filtered by repeat
	lda gamepad_rep
	beq :++
		dec gamepad_rep
		lda gamepad
		bne :+
			lda #0
			sta gamepad_rep ; buttons released already
			sta gamepad_rep2
		:
		jmp @mode_end
	:
	lda mode
	beq @mode_scroll
	cmp #1
	beq @mode_bg
	cmp #2
	beq @mode_fg
@mode_scroll:
	lda gamepad
	and #PAD_L
	beq :+
		dec scroll_x
		jsr gamepad_wait
	:
	lda gamepad
	and #PAD_R
	beq :+
		inc scroll_x
		jsr gamepad_wait
	:
	lda gamepad
	and #PAD_U
	beq :+
		jsr gamepad_wait
		dec scroll_y
		lda scroll_y
		cmp #240
		bcc :+
		lda #239
		sta scroll_y
	:
	lda gamepad
	and #PAD_D
	beq :+
		jsr gamepad_wait
		inc scroll_y
		lda scroll_y
		cmp #240
		bcc :+
		lda #0
		sta scroll_y
	:
	jmp @mode_end
@mode_bg:
	lda color_bg
	jsr color_edit
	sta color_bg
	jsr set_anti_color
	jmp @mode_end
@mode_fg:
	lda color_fg
	jsr color_edit
	sta color_fg
	jsr set_anti_color
	jmp @mode_end
@mode_end:
draw_modes:
	lda mode
	beq @draw_scroll
	cmp #1
	beq @draw_bg
	cmp #2
	beq @draw_fg
@draw_scroll:
	lda scroll_x
	sta temp+0
	lda scroll_y
	sta temp+1
	lda #','
	sta oam_tile_2
	jmp @draw_common
@draw_bg:
	lda #$BA ; left arrow
	sta oam_tile_2
	jmp @draw_bg_or_fg
@draw_fg:
	lda #$BB ; right arrow
	sta oam_tile_2
	;jmp @draw_bg_or_fg
@draw_bg_or_fg:
	lda color_bg
	sta temp+0
	lda color_fg
	sta temp+1
	;jmp @draw_common
@draw_common:
	; hex numbers
	lda temp+0
	lsr
	lsr
	lsr
	lsr
	ora #$A0
	sta oam_tile_0
	lda temp+0
	and #$0F
	ora #$A0
	sta oam_tile_1
	lda temp+1
	lsr
	lsr
	lsr
	lsr
	ora #$A0
	sta oam_tile_3
	lda temp+1
	and #$0F
	ora #$A0
	sta oam_tile_4
nmi_end:
	rti

gamepad_wait:
	lda mode
	bne @color_wait
@scroll_wait:
	lda gamepad_rep2
	bne :+
		lda #1
		sta gamepad_rep2
		lda #20 ; long wait followed by...
		jmp @wait_end
	:
	lda #5 ; ...shorter wait
	jmp @wait_end
@color_wait:
	lda #15 ; wait a few frames before accepting more dpad input
	;jmp @wait_end
@wait_end:
	sta gamepad_rep
	rts

color_edit:
	; break into 2 values
	pha
	and #$0F
	sta temp+0
	pla
	and #$30
	sta temp+1
	; adjust
	lda gamepad
	and #PAD_L
	beq :+
		dec temp+0
		lda temp+0
		and #$0F
		sta temp+0
		jsr gamepad_wait
	:
	lda gamepad
	and #PAD_R
	beq :+
		inc temp+0
		lda temp+0
		and #$0F
		sta temp+0
		jsr gamepad_wait
	:
	lda gamepad
	and #PAD_U
	beq :+
		lda temp+1
		sec
		sbc #$10
		and #$30
		sta temp+1
		jsr gamepad_wait
	:
	lda gamepad
	and #PAD_D
	beq :+
		lda temp+1
		clc
		adc #$10
		and #$30
		sta temp+1
		jsr gamepad_wait
	:
	; recombine
	lda temp+0
	ora temp+1
	rts

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
