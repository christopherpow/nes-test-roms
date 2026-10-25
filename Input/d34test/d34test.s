;
; d34test.s
; Extended input test on data lines 3 and 4, by Brad Smith 2024
; http://rainwarrior.ca
;

.macpack longbranch

; PORT=0/1 define selects $4016/4017 for input port
_port = PORT
.exportzp _port

; ===
; RAM
; ===

.segment "ZEROPAGE"
_ptr:          .res 2 ; shared pointer for C interface
ppu_post_mode: .res 1
_input:        .res 12

; convenient index values
_i:    .res 1

.segment "BSS"
temp: .res 4

.segment "STACK"
ppu_2000:        .res 1
ppu_2001:        .res 1
ppu_2005x:       .res 1
ppu_2005y:       .res 1
_ppu_send_addr:  .res 2
_ppu_send_count: .res 1
_ppu_send:       .res 64
_palette:        .res 32

.segment "OAM"
.align 256
_oam: .res 256

.exportzp _ptr
.exportzp _input;
.exportzp _i

.export _ppu_send_addr
.export _ppu_send_count
.export _ppu_send
.export _palette
.export _oam

; cc65 temporaries
.importzp tmp1, tmp2, tmp3, tmp4

.enum
	POST_OFF    = 1
	POST_NONE   = 2
	POST_UPDATE = 3
.endenum

; =====
; Tiles
; =====

.export _sprite_chr

.segment "RODATA"
_sprite_chr: .incbin "sprite.chr"

; =========
; Utilities
; =========

.export _input_poll

.segment "CODE"

_input_poll:
	; strobe
	ldy #1
	sty $4016
	dey
	sty $4016
	; read 4 bytes each from D0 D3 D4
input_poll_raw_strobed:
	ldx #0
@poll_byte:
	.repeat 4, I
		ldy #8
		:
			lda $4016 + PORT
			lsr
			rol _input+0+I
			lsr
			lsr
			lsr
			rol _input+4+I
			lsr
			rol _input+8+I
			dey
			bne :-
	.endrepeat
	rts

; =====================
; NES hardware handling
; =====================

.export _ppu_latch
.export _ppu_direction
.export _ppu_write
.export _ppu_load
.export _ppu_fill
.export _ppu_ctrl
.export _ppu_mask
.export _ppu_scroll_x
.export _ppu_scroll_y
.export _ppu_post
.export _ppu_profile

.import _main
.import popa ; cc65 stack manipulator

.segment "CODE"

_ppu_latch:
	bit $2002
	stx $2006
	sta $2006
	rts

_ppu_direction:
	asl
	asl
	and #%00000100
	pha
	lda ppu_2000
	and #%11111011
	sta ppu_2000
	pla
	ora ppu_2000
	sta ppu_2000
	rts

_ppu_write:
	sta $2007
	rts

_ppu_load:
	; A:X = count
	stx tmp1
	tax
	cpx #0
	beq @page
	; finish incomplete page first
	ldy #0
	:
		lda (_ptr), Y
		sta $2007
		iny
		dex
		bne :-
	tya
	clc
	adc _ptr+0
	sta _ptr+0
	lda #0
	adc _ptr+1
	sta _ptr+1
@page: ; remaining 256 byte pages
	;     X = 0
	txa ; A = 0
	cmp tmp1
	beq @done
	tay ; Y = 0, temp1 = remaining page count
	:
		lda (_ptr), Y
		sta $2007
		iny
		bne :-
	inc _ptr+1
	dec tmp1
	jmp @page
@done:
	rts

_ppu_fill:
	; A:X = count
	; stack = fill value
	sta tmp1
	jsr popa
	ldy tmp1
	cpy #0
	beq @no_partial
@page:
	:
		sta $2007
		dey
		bne :-
@no_partial:
	cpx #0
	beq @done
	dex
	ldy #0
	jmp @page
@done:
	rts

_ppu_ctrl:
	and #%00111000
	pha
	lda ppu_2000
	and #%11000111
	sta ppu_2000
	pla
	ora ppu_2000
	sta ppu_2000
	rts

_ppu_mask:
	sta ppu_2001
	rts

_ppu_scroll_x:
	; A:X = scroll
	sta ppu_2005x
	txa
	and #1
	pha
	lda ppu_2000
	and #%11111110
	sta ppu_2000
	pla
	ora ppu_2000
	sta ppu_2000
	rts

_ppu_scroll_y:
	; A:X = scroll
	sta ppu_2005y
	txa
	and #1
	asl
	pha
	lda ppu_2000
	and #%11111101
	sta ppu_2000
	pla
	ora ppu_2000
	sta ppu_2000
	rts

_ppu_post:
	sta ppu_post_mode
	:
		lda ppu_post_mode
		bne :-
	rts

_ppu_profile:
	ora ppu_2001
	sta $2001
	rts

nmi:
	pha
	txa
	pha
	tya
	pha
	ldx #0
	lda ppu_post_mode
	stx ppu_post_mode ; signal the post is complete (after RTI)
	jeq @end
	cmp #POST_NONE
	jeq @post_none
	cmp #POST_UPDATE
	beq @post_update
	; otherwise POST_OFF
@post_off:
	lda ppu_2001
	and #%11100001
	sta $2001
	jmp @end
@post_update:
	jsr @post_common
	lda ppu_2000
	sta $2000 ; set direction
	ldx #0
	cpx _ppu_send_count
	bcs :++
	:
		lda _ppu_send, X
		sta $2007
		inx
		cpx _ppu_send_count
		bcc :-
	:
	ldx #0
	stx _ppu_send_count
	jmp @post_on
@post_common:
	; OAM
	lda #0
	sta $4003
	lda #>_oam
	sta $4014
	; palettes
	lda #0
	sta $2000 ; set horizontal increment
	bit $2002
	ldx #>$3F00
	stx $2006
	;lda #<$3F00
	sta $2006
	ldx #0
	:
		lda _palette, X
		sta $2007
		inx
		cpx #32
		bcc :-
	; prepare address for send
	lda _ppu_send_addr+1
	sta $2006
	lda _ppu_send_addr+0
	sta $2006
	rts
@post_none:
@post_on:
	lda ppu_2000
	sta $2000
	lda ppu_2005x
	sta $2005
	lda ppu_2005y
	sta $2005
	lda ppu_2001
	sta $2001
@end:
	pla
	tay
	pla
	tax
	pla
	rti

irq:
	rti

reset:
	; already done in reset_stub
	;sei       ; disable maskable interrupts
	;lda #0
	;sta $2000 ; disable non-maskable interrupt
	lda #0
	sta $2001 ; rendering off
	sta $4010 ; disable DMC IRQ
	sta $4015 ; disable APU sound
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; setup tack
	; wait for vblank #1
	bit $2002
	:
		bit $2002
		bpl :-
	lda #0 ; clear RAM
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
	; wipe OAM
	;ldx #0
	lda #$FF
	:
		sta _oam, X
		inx
		bne :-
	; wait for vblank #2
	:
		bit $2002
		bpl :-
	; initialize internal variables
	lda #%00011110 ; No emphasis, no greyscale, BG and sprite shown, no hidden column
	sta ppu_2001
	lda #%10001000 ; NMI on, 8-pixel sprites, BG page 0, Sprite page 1
	sta ppu_2000
	sta $2000 ; turn NMI on permanently
	; initialize CC65 and enter main()
	jsr cc65_init
	jsr _main
	jmp ($FFFC)

; ==========
; CC65 setup
; ==========

; simplified version of cc65 libsrc/nes/crt0.s

.import copydata ; cc65 "DATA" segment setup
.importzp sp ; cc65 C stack pointer
.export __STARTUP__: absolute = 1

; stack reservation
CSTACK_SIZE = 128
.segment "CSTACK"
cstack: .res CSTACK_SIZE

.segment "CODE"
cc65_init:
	jsr copydata
	lda #<(cstack + CSTACK_SIZE)
	sta sp+0
	lda #>(cstack + CSTACK_SIZE)
	sta sp+1
	rts

; =======
; NES ROM
; =======

.segment "HEADER"

INES_MAPPER     = 2 ; UNROM
INES_MIRROR     = 1 ; horizontal nametables
INES_PRG_16K    = 2 ; 32K
INES_CHR_8K     = 0
INES_BATTERY    = 0
INES2           = %00001000 ; NES 2.0 flag for bit 7
INES2_SUBMAPPER = 0
INES2_PRGRAM    = 0
INES2_PRGBAT    = 0
INES2_CHRRAM    = 7 ; 8K
INES2_CHRBAT    = 0
INES2_REGION    = 2 ; 0=NTSC, 1=PAL, 2=Dual

; iNES 1 header
.byte 'N', 'E', 'S', $1A ; ID
.byte <INES_PRG_16K
.byte INES_CHR_8K
.byte INES_MIRROR | (INES_BATTERY << 1) | ((INES_MAPPER & $f) << 4)
.byte (<INES_MAPPER & %11110000) | INES2
; iNES 2 section
.byte (INES2_SUBMAPPER << 4) | (INES_MAPPER>>8)
.byte ((INES_CHR_8K >> 8) << 4) | (INES_PRG_16K >> 8)
.byte (INES2_PRGBAT << 4) | INES2_PRGRAM
.byte (INES2_CHRBAT << 4) | INES2_CHRRAM
.byte INES2_REGION
.byte $00 ; VS system
.byte $00, $00 ; padding/reserved
.assert * = 16, error, "NES header must be 16 bytes."

.segment "STUB"
reset_stub:
	sei       ; disable maskable interrupts
	lda #0
	sta $2000 ; disable non-maskable interrupt
	sta $8000 ; setup up UxROM bank 0
	jmp reset

.segment "VECTORS"
.word nmi
.word reset_stub
.word irq
