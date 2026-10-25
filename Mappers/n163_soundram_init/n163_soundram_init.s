; n163_soundram_init
; displays contents of N163 sound RAM after power ON

; iNES header
.segment "HEADER"

INES_MAPPER = 19
INES_MIRROR = 0
INES_SRAM   = 0

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
fa: .res 1
fb: .res 1
ht: .res 1
row: .res 1
readback: .res 8

; RAM variables
.segment "BSS"

; STUB (fixed bank initializes mapper_)
.segment "STUB"
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
	; N163 setup
	ldx #0
	stx $5000
	stx $5800 ; IRQ disable
	.repeat 8, I ; CHR
	stx $8000+($800 * I)
	inx
	.endrepeat
	lda #$E0
	sta $C000 ; nametables
	sta $C800
	sta $D000
	sta $D800
	lda #$40
	sta $E000 ; PRG0 $8000 and disable sound
	lda #$01
	sta $E800 ; PRG1 $A000
	lda #$02
	sta $F000 ; PRG2 $C000
	jmp start ; ready to go

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

start:
	bit $2002
	:
		bit $2002
		bpl :-
	:
		bit $2002
		bpl :-
	jsr reset_screen
	;
	; dump to screen.
	;
	lda #$80
	sta $F800 ; incremental readback from start
	lda #4
	sta row
@row:
	jsr start_row
	ldx #0
	:
		lda $4800
		jsr printhex
		lda #' '
		sta $2007
		inx
		cpx #8
		bcc :-
	inc row
	lda row
	cmp #16+4
	bcc @row
	;
	; display
	;
	:
		bit $2002
		bpl :-
	lda #0
	sta $2005
	sta $2005
	lda #%00001010
	sta $2001
	:
	jmp :- ; wait forever

start_row: ; A = row, clobbers fa
	bit $2002
	sta fa
	lda #0
	.repeat 5
		asl fa
		rol
	.endrepeat
	eor #$20
	sta $2006
	lda fa
	clc
	adc #4
	sta $2006 ; row * 32 + column 4
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

printhex:
	pha
	stx ht
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
	ldx ht
	rts

; end of file
