;
; kbmtest.s
; Keyboard Mouse Host test by Brad Smith 2024
; http://rainwarrior.ca
;

_nmi_count = nmi_count
.exportzp _nmi_count        ; uint8 nmi_count
.exportzp _input            ; uint8 input[16]
.exportzp _ascii_offset     ; uint8 ascii_offset
.exportzp _i,_j,_k,_l       ; uint8 i,j,k,l
.exportzp _mx,_nx,_ox,_px   ; uint16 mx,nx,ox,px

.export _palette            ; uint8 palette[32]
.export _ppu_send           ; uint8 ppu_send[72]
.export _oam                ; uint8 oam[256]

.export _ppu_latch          ; void ppu_latch(uint16 addr)
.export _ppu_latch_at       ; void ppu_latch_at(uint8 x, uint8 y)
.export _ppu_write          ; void ppu_write(uint8 value);
.export _ppu_load           ; void ppu_load(const uint8* data, uint16 count)
.export _ppu_fill           ; void ppu_fill(uint8 value, uint16 count)
.export _ppu_string         ; void ppu_string(const char* s)
.export _ppu_packet         ; void ppu_packet(uint16 addr, const uint8* data, uint8 count)
.export _ppu_packet_apply   ; void ppu_packet_apply()
.export _ppu_ctrl           ; void ppu_ctrl(uint8 v)
.export _ppu_ctrl_apply     ; void ppu_control_apply(uint8 v)
.export _ppu_mask           ; void ppu_mask(uint8 v)
.export _ppu_mask_apply     ; void ppu_mask_apply(uint8 v)
.export _ppu_scroll         ; void ppu_scroll(uint16 x, uint16 y)
.export _ppu_render_frame   ; void ppu_render_frame()
.export _ppu_wait_frame     ; void ppu_wait_frame()
.export _ppu_render_off     ; void ppu_render_off()
.export _ppu_nmi_off        ; void ppu_nmi_off()
.export _ppu_nmi_on         ; void ppu_nmi_on()

.export _sprite_begin       ; void sprite_begin()
.export _sprite_end         ; void sprite_end()
.export _sprite_tile        ; void sprite_tile(uint8 t, uint8 a, uint8 x, uint8 y)
.export _sprite             ; void sprite(uint8 e, uint8 x, uint8 y)
.export _sprite_flip        ; void sprite_flip(uint8 e, uint8 x, uint8 y)

.export _nescopy            ; void nescopy(void* dst, const void* src, uint8 count)

.export _input_poll         ; void input_poll()

.import _main
.import popa ; cc65
.import popax ; cc65
.import popptr1 ; cc65
.import incsp3 ; cc65
.importzp tmp1, tmp2, tmp3, tmp4 ; cc65
.importzp ptr1, ptr2 ; cc65

; =======
; KBMTEST
; =======

.segment "TILES0"
.incbin "temp/bg.chr"
.segment "TILES1"
.incbin "temp/sp.chr"

.segment "RODATA"

sprite_data:
.incbin "temp/sprite.bin"
.include "temp/sprite.inc"

; read 4 bytes each from D3 D4 on 4016 and 4017
_input_poll:
	; strobe
	ldy #1
	sty $4016
	dey
	sty $4016
@poll_byte:
	.repeat 4, I
		ldy #8
		:
			lda $4016
			lsr
			lsr
			lsr
			lsr
			rol z:_input+0+I
			lsr
			rol z:_input+4+I
			lda $4017
			lsr
			lsr
			lsr
			lsr
			rol z:_input+8+I
			lsr
			rol z:_input+12+I
			dey
			bne :-
	.endrepeat
	rts

; ===
; RAM
; ===

CSTACK_SIZE = 128 ; cc65 internal C stack

.segment "ZEROPAGE"
ppu_ready:     .res 1 ; 0 = not ready, 1 = render on and send update, 2 = render off
nmi_count:     .res 1 ; increments every NMI
nmi_lock:      .res 1 ; prevents NMI re-entry (disables updates)
ppu_2000:      .res 1 ; PPU register settings (applied with ppu_send)
ppu_2001:      .res 1
ppu_2005:      .res 2
ppu_send_pos:  .res 1
oam_pos:       .res 1
_input:        .res 16 ; last result of input_poll
_input_new:    .res 2 ; new buttons pressed at last input_poll
_ascii_offset: .res 1 ; offset from CHR index to ASCII in ppu_string
_i:            .res 1 ; convenient index/temporary values
_j:            .res 1
_k:            .res 1
_l:            .res 1
_mx:           .res 2
_nx:           .res 2
_ox:           .res 2
_px:           .res 2

.segment "BSS"
cstack: .res CSTACK_SIZE ; cc65 internal C stack
_palette: .res 32

.segment "STACK"
_ppu_send: .res 72 ; bytes to send on next NMI when ppu_ready

.segment "OAM"
.align 256
_oam: .res 256

.segment "CODE"

; =========
; Rendering
; =========

_ppu_latch: ; void ppu_latch(uint16 addr)
	bit $2002
	stx $2006
	sta $2006
	rts

_ppu_latch_at: ; void ppu_latch_at(uint8 x, uint8 y)
	; X: .....6.. ...54321
	; Y: ....6.54 321.....
	; A = Y
	ldx #0
	stx ptr1+0
	lsr
	ror ptr1+0
	lsr
	ror ptr1+0
	pha
	lsr
	ror ptr1+0 ; ptr1+0 = Y 321.....
	and #%00000011
	sta ptr1+1
	pla
	and #%00001000
	ora ptr1+1
	ora #$20
	sta ptr1+1 ; ptr1+1 = Y ....6.54 | $20
	jsr popa ; A = X
	pha
	lsr
	lsr
	lsr
	and #%00000100
	ora ptr1+1
	tax ; X .....6.. | Y ....6... | $20
	pla
	and #%00011111
	ora ptr1+0 ; X ...54321 | Y 321.....
	jmp _ppu_latch

_ppu_write: ; void ppu_write(uint8 value);
	sta $2007
	rts

_ppu_load: ; void ppu_load(uint8* data, uint16 count)
	sta ptr2+0
	stx ptr2+1 ; ptr2 = count
	jsr popptr1 ; ptr1 = data
	ldy #0
@loop:
	; dec ptr2 (quit if 0)
	lda ptr2+0
	bne :+
		lda ptr2+1
		beq @rts
		dec ptr2+1
	:
	dec ptr2+0
	; load
	lda (ptr1), Y
	sta $2007
	; inc ptr1
	inc ptr1+0
	bne :+
		inc ptr1+1
	:
	jmp @loop
@rts:
	rts

_ppu_fill: ; void ppu_fill(uint8 value, uint16 count)
	sta ptr2+0
	stx ptr2+1
	jsr popa
@loop:
	; dec ptr2 (quit if 0)
	ldx ptr2+0
	bne :+
		ldx ptr2+1
		beq @rts
		dec ptr2+1
	:
	dec ptr2+0
	; fill
	sta $2007
	jmp @loop
@rts:
	rts

_ppu_string: ; void ppu_string(char* s)
	sta ptr1+0
	stx ptr1+1
	ldy #0
	:
		lda (ptr1), Y
		beq :+
		sec
		sbc _ascii_offset
		sta $2007
		iny
		bne :-
	:
	rts

_ppu_packet: ; void ppu_packet(uint16 addr, uint8* data, uint8 count)
	ldy ppu_send_pos
	sta _ppu_send+0, Y ; count
	sta tmp1
	jsr popptr1 ; ptr1 = data
	jsr popax ; X:A = addr
	ldy tmp1
	bne :+
		rts ; count of 0, no packet
	:
	ldy ppu_send_pos
	sta _ppu_send+2, Y ; addr low byte second
	txa
	sta _ppu_send+1, Y ; addr high byte first
	iny
	iny
	iny
	tya
	tax
	ldy #0
	:
		lda (ptr1), Y
		sta _ppu_send, X
		inx
		iny
		cpy tmp1
		bcc :-
	lda #0
	sta _ppu_send, X ; 0 marks end of packets
	stx ppu_send_pos
	rts

; void ppu_packet_apply()
_ppu_packet_apply = ppu_packet_apply

_ppu_ctrl_apply: ; void ppu_control_apply(uint8 v)
	sta $2000
_ppu_ctrl: ; void ppu_ctrl(uint8 v)
	sta ppu_2000
	rts

_ppu_mask_apply: ; void ppu_mask_apply(uint8 v)
	sta $2001
_ppu_mask: ; void ppu_mask(uint8 v)
	sta ppu_2001
	rts

_ppu_scroll: ; void ppu_scroll(uint16 x, uint16 y)
	sta ppu_2005+1
	txa
	asl
	sta tmp1
	jsr popax
	sta ppu_2005+0
	txa
	and #%00000001
	ora tmp1
	and #%00000011
	sta tmp1
	lda ppu_2000
	and #%11111100
	ora tmp1
	sta ppu_2000
	rts

_ppu_render_frame: ; void ppu_render_frame()
	lda #1
	sta ppu_ready
ppu_ready_wait:
	lda ppu_ready
	bne ppu_ready_wait
	rts

_ppu_wait_frame: ; void ppu_wait_frame()
	lda nmi_count
	:
		cmp nmi_count
		beq :-
	rts

_ppu_render_off: ; void ppu_render_off()
	lda #2
	sta ppu_ready
	jmp ppu_ready_wait

_ppu_nmi_off: ; void ppu_nmi_off()
	lda ppu_2000
	and #%01111111
	sta $2000
	rts

_ppu_nmi_on: ; void ppu_nmi_on()
	bit $2002
	:
		bit $2002
		bpl :-
	lda ppu_2000
	ora #%10000000
	sta ppu_2000
	sta $2000
	rts

; =======
; Sprites
; =======

_sprite_begin:
	lda #0
	sta oam_pos
	rts

_sprite_end:
	ldx oam_pos
	txa
	and #3
	beq :+
		rts ; 255 = completely full, treat any non-4 multiple as "full" error
	:
	lda #255
	:
		sta _oam+0, X
		inx
		inx
		inx
		inx
		bne :-
	dex
	stx oam_pos
	rts

_sprite_tile: ; void sprite_tile(uint8 t, uint8 a, uint8 x, uint8 y)
	ldx oam_pos
	cpx #255
	bne :+
		jmp incsp3
	:
	sta _oam+0, X ; X
	jsr popa
	sta _oam+3, X ; Y
	jsr popa
	sta _oam+2, X ; attribute
	jsr popa
	sta _oam+1, X ; tile
	inx
	inx
	inx
	inx
	bne :+
		dex ; 255 = mark as full
	:
	stx oam_pos
	rts

_sprite: ; void sprite(uint8 e, uint8 x, uint8 y)
	sta tmp2 ; Y
	jsr popa
	sta tmp1 ; X
	lda #0
	sta tmp3 ; X eor
	sta tmp4 ; A eor
sprite_common:
	jsr popa
	tax
	lda #<sprite_data
	clc
	adc sprite_data, X
	sta ptr1+0
	lda #>sprite_data
	adc sprite_data+SPRITE_COUNT, X
	sta ptr1+1
	; ptr1 = sprite data, (tmp1,tmp2) = (X,Y)
	ldx oam_pos
	ldy #0
@loop:
	cpx #255
	beq @end
	; attribute
	lda (ptr1), Y
	beq @end ; 0 marks end of sprite
	eor tmp4
	sta _oam+2, X
	; Y
	iny
	lda (ptr1), Y
	clc
	adc tmp2
	sta _oam+0, X
	; X
	iny
	lda (ptr1), Y
	eor tmp3
	clc
	adc tmp1
	sta _oam+3, X
	; tile
	iny
	lda (ptr1), Y
	sta _oam+1, X
	; loop
	iny
	inx
	inx
	inx
	inx
	bne @loop
	ldx #255 ; OAM is full
@end:
	stx oam_pos
	rts

_sprite_flip: ; void sprite_flip(uint8 e, uint8 x, uint8 y)
	sta tmp2 ; Y
	jsr popa
	sec
	sbc #(8-1)
	sta tmp1 ; X - (8-1)
	lda #$FF
	sta tmp3 ; X eor (X^$FF = -(X+1))
	lda #%01000000
	sta tmp4 ; A eor (horizontal flip)
	jmp sprite_common

; =======
; Utility
; =======

_nescopy: ; void nescopy(void* dst, void* src, uint8 count)
	sta tmp1
	jsr popptr1
	jsr popax
	sta ptr2+0
	stx ptr2+1
	ldy tmp1
	beq :++
:
	dey
	lda (ptr1), Y
	sta (ptr2), Y
	cpy #0
	bne :-
:
	rts

; ===
; NMI
; ===

nmi:
	inc nmi_count
	pha
	lda nmi_lock
	beq :+
		pla
		rti
	:
	lda #$FF
	sta nmi_lock ; prevent re-entry
	txa
	pha
	tya
	pha
	lda ppu_ready
	beq @post_send ; 0 = no update
	cmp #2 
	bcc @send ; 2 = render off
	lda ppu_2001
	and #%11100001
	sta $2001
	jmp @post_send
@send: ; 1 = send ppu data
	; OAM
	ldx #0
	stx $2003
	lda #>_oam
	sta $4014
	; palettes
	;ldx #0
	stx $2000 ; set horizontal increment
	bit $2002
	lda #>$3F00
	sta $2006
	;ldx #<$3F00
	stx $2006
	;ldx #0
	:
		lda _palette, X
		sta $2007
		inx
		cpx #32
		bcc :-
	; set $2000 to requested state
	lda ppu_2000
	sta $2000
	; upload _ppu_send
	jsr ppu_packet_apply
	; set scroll and mask
	lda ppu_2000
	sta $2000
	lda ppu_2005+0
	sta $2005
	lda ppu_2005+1
	sta $2005
	lda ppu_2001
	sta $2001
@post_send:
	lda #0
	sta ppu_ready
	sta nmi_lock
	pla
	tay
	pla
	tax
	pla
	rti

ppu_packet_apply: ; applies all packets stored in _ppu_send
	; move _ppu_send address to stack
	.assert (>_ppu_send = $01), error, "_ppu_send expected on stack page."
	tsx
	txa
	tay ; save stack position
	ldx #<(_ppu_send-1)
	txs
@send_packet:
	; first byte of packet = count
	pla
	beq @send_finish ; 0 = done
	tax
	pla
	sta $2006
	pla
	sta $2006
	:
		pla
		sta $2007
		dex
		bne :-
	beq @send_packet
@send_finish:
	; restore stack
	tya
	tax
	txs
	lda #0
	sta _ppu_send ; clear packet
	sta ppu_send_pos
	rts

; ===
; IRQ
; ===

irq:
	rti

; =====
; Reset
; =====

reset:
	sei       ; disable maskable interrupts
	lda #0
	sta $2000 ; disable non-maskable interrupt
	lda #0
	sta $2001 ; rendering off
	sta $4010 ; disable DMC IRQ
	sta $4015 ; disable APU sound
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; setup stack
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
	lda #$FF ; clear OAM
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

INES_MAPPER     = 0 ; NROM
INES_MIRROR     = 1 ; horizontal nametables
INES_PRG_16K    = 2 ; 32K
INES_CHR_8K     = 1 ; 8K
INES_BATTERY    = 0
INES2           = %00001000 ; NES 2.0 flag for bit 7
INES2_SUBMAPPER = 0
INES2_PRGRAM    = 0
INES2_PRGBAT    = 0
INES2_CHRRAM    = 0
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

.segment "VECTORS"
.word nmi
.word reset
.word irq

; end of file
