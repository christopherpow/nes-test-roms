; ram_retain
; Brad Smith, 2014
;
; tests the retention of RAM over reset
;
; up/down change page of RAM to view
; B/A fill page with ascending/descending values
;    will not change low 16 bytes of ZP page
;    will not change any bytes of stack page

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
gamepad: .res 1
page: .res 2

; RAM variables
.segment "BSS"

; CODE
.segment "CODE"

.macro stb val, addr
; - store immediate byte using A as temporary register
	lda #val
	sta addr
.endmacro

nmi:
	rti

irq:
	rti

reset:
	; standard startup
	sei       ; set interrupt flag (unnecessary, unless reset is called from code)
	cld       ; disable decimal mode
	ldx #$40
	stx $4017 ; disable APU IRQ
	ldx $ff
	txs       ; set up stack
	ldx #$00
	stx $2000 ; disable NMI
	stx $2001 ; disable render
	stx $4010 ; disable DPCM IRQ
	bit $2002 ; clear vblank flag
	; wait for vblank
	:
		bit $2002
		bpl :-
	; waste time instead of clearing memory
	lda #$00
	tax
	:
		.repeat 24
			nop
		.endrepeat
		inx
		bne :-
	; wait for second vblank
	:
		bit $2002
		bpl :-
	; PPU is now warmed up, NES is ready to go!
	;
	; set zero page variables, this is the only memory inizialization
	lda #0
	sta gamepad
	sta page+0
	sta page+1
	; clear the screen, set up default palette, etc.
	jsr reset_screen
	; main loop
main_loop:
	jsr draw
	; wait for vblank
	:
		bit $2002
		bpl :-
	; turn renering on
	lda #0
	sta $2005
	sta $2005
	lda #%00011110
	sta $2001
	:
		jsr read_gamepad
		lda gamepad
		beq :-
	lda gamepad
	cmp #%10000000
	bne :+
		jsr push_a
		jmp @pushed
	:
	cmp #%01000000
	bne :+
		jsr push_b
		jmp @pushed
	:
	cmp #%00001000
	bne :+
		jsr push_up
		jmp @pushed
	:
	cmp #%00000100
	bne :+
		jsr push_down
		jmp @pushed
	:
	@pushed:
	jmp main_loop

push_b:
	ldx #0
	ldy #0
	lda page+1
	cmp #1 ; don't write page 1
	bne :+
		jmp wait_release
	:
	cmp #0
	bne :+ ; skip 16 bytes of page 0
		.repeat 16
			iny
			inx
		.endrepeat
	:
		txa
		sta (page), Y
		inx
		iny
		bne :-
	jmp wait_release

push_a:
	ldx #$FF
	ldy #0
	lda page+1
	cmp #1 ; don't write page 1
	bne :+
		jmp wait_release
	:
	cmp #0
	bne :+ ; skip 16 bytes of page 0
		.repeat 16
			iny
			dex
		.endrepeat
	:
		txa
		sta (page), Y
		dex
		iny
		bne :-
	jmp wait_release

push_up:
	dec page+1
	lda page+1
	cmp #8
	bcc :+
		lda #7
		sta page+1
	:
	jmp wait_release

push_down:
	inc page+1
	lda page+1
	cmp #8
	bcc :+
		lda #0
		sta page+1
	:
	jmp wait_release

wait_release:
	:
		jsr read_gamepad
		lda gamepad
		bne :-
	rts

reset_screen:
	;
	; load palette
	;
	; latch PPU to $3F00
	lda $2002
	lda #$3F
	sta $2006
	lda #$00
	sta $2006
	; write default palette
	stb $0f, $2007
	stb $12, $2007
	stb $21, $2007
	stb $30, $2007
	stb $0f, $2007
	stb $13, $2007
	stb $23, $2007
	stb $33, $2007
	stb $0f, $2007
	stb $05, $2007
	stb $17, $2007
	stb $28, $2007
	stb $0f, $2007
	stb $1a, $2007
	stb $11, $2007
	stb $04, $2007
	stb $0f, $2007
	stb $16, $2007
	stb $26, $2007
	stb $36, $2007
	stb $0f, $2007
	stb $17, $2007
	stb $27, $2007
	stb $37, $2007
	stb $0f, $2007
	stb $19, $2007
	stb $29, $2007
	stb $39, $2007
	stb $0f, $2007
	stb $1c, $2007
	stb $2c, $2007
	stb $3c, $2007
	;
	; blank screen and attributes
	;
	; latch PPU to $2000
	lda $2002
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	; write 16 x 256 zeroes to screen
	ldy #16 ; high count
	ldx #0  ; low count
	lda #0
	:
		sta $2007
		dex
		bne :-
		dey
		bne :-
	; done
	rts

hex_table:
.byte '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'

draw_hex:
	pha
	; high nibble
	lsr
	lsr
	lsr
	lsr
	and #$F
	tax
	lda hex_table, X
	sta $2007
	; low nibble
	pla
	and #$F
	tax
	lda hex_table, X
	sta $2007
	rts

draw:
	; turn off rendering
	stb %00000000, $2001
	; latch to tile at 0,6
	lda $2002
	stb $20, $2006
	stb $C0, $2006
	; show page number and blank row
	lda #0
	.repeat 4
		sta $2007
	.endrepeat
	stb 'P', $2007
	stb 'A', $2007
	stb 'G', $2007
	stb 'E', $2007
	stb ':', $2007
	stb ' ', $2007
	lda page+1
	jsr draw_hex
	lda page+0
	jsr draw_hex
	lda #0
	.repeat 18+32
		sta $2007
	.endrepeat
	; display 256 byte page
	ldy #0
	:
		lda (page), Y
		jsr draw_hex
		iny
		bne :-
	; done
	rts

read_gamepad:
;
; - reads current controller state into gamepad variable
; - not safe if DPCM is active
; - not safe for Famicom expansion controllers
;
	; latch the current controller state
	lda #1
	sta $4016
	lda #0
	sta $4016
	; store low bit in gamepad to mark end of read
	lda #%0000001
	sta gamepad
	; read 8 bits from controller port
	:
		lda $4016
		ror
		rol gamepad
		bcc :-
	; gamepad now contains controller state
	rts

; end of file