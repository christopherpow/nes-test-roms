; n163_soundram
; tests write and read from N163 sound RAM
;
; Should print this:
;
; A7 89 30 B9 E9 A2 8B 2D ; "fibonacci" test pattern
; B8 E5 9D 82 1F A1 C0 61 ; (tests increment and wrap)
; 21 82 A3 25 C8 ED B5 A2 
; 57 F9 50 49 99 E2 7B 5D 
; D8 35 0D 42 4F 91 E0 71 
; 51 C2 13 D5 E8 BD A5 62 
; 07 69 70 D9 49 22 6B 8D 
; 38 39 3A 3B 3C 3D 3E 3F ; two rows in middle
; 40 41 42 43 44 45 46 47 ; to test non-incrementing writes
; B7 D9 90 69 F9 62 5B BD 
; 18 D5 ED C2 AF 71 20 91 
; B1 42 F3 35 28 5D 85 E2 
; 67 49 B0 F9 A9 A2 4B ED 
; 38 25 5D 82 DF 61 40 A1 
; E1 82 63 E5 48 2D 75 A2 ; not sure about the last two rows?
; 17 B9 D0 89 59 E2 3B 1D ; might be corrupted by sound unit
;
; B8 E5 9D 82 1F A1 C0 61 ; read back second line incrementally
;
; Not sure if the last two rows would be corrupted by the sound
; unit, but presumably they won't with sound disabled.

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
	; 1. fill N163 with fibbonacci pattern with auto-increment
	lda #$80
	sta $F800
	ldy #0
	sty fa
	lda #1
	sta fb
	:
		lda fa
		clc
		adc fb
		sta $4800
		tax
		lda fb
		sta fa
		stx fb
		iny
		bne :- ; writing 256 values should wrap after 128
	; 2. fill middle two rows with non-incrementing pattern
	ldy #$47
	:
		sty $F800
		sty $4800
		dey
		cpy #$38
		bcs :-
	; 3. read back second and third rows for verification
	ldx #$0F ; non-incremental
	ldy #7
	:
		stx $F800
		lda $4800
		sta readback, Y
		dex
		dey
		bpl :-
	;
	; tests done, output to screen.
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
	; non-incremental readback of second line
	lda #16+5
	jsr start_row
	ldy #0
	:
		lda readback, Y
		jsr printhex
		lda #' '
		sta $2007
		iny
		cpy #8
		bcc :-
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
