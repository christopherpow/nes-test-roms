.segment "ZEROPAGE"
nmi_ready:    .res 1
nmi_count:    .res 1
ptr:          .res 2
temp:         .res 1

.segment "STACK"
stack: .res 256

.segment "OAM"
oam: .res 256

.segment "CODE"

; UNROM bankswitching

bank:
.byte BANK ; define with -D

conflict_table:
.byte $00, $01, $02, $03, $04, $05, $06, $07

conflict_00:
.byte $00

conflict_FF:
.byte $FF

conflict_01:
.byte $01

conflict_02:
.byte $02

axrom_switch:
	tay
	sta conflict_table, Y
	rts

axrom_switch_conflict_00:
	sta conflict_00
	rts

axrom_switch_conflict_FF:
	sta conflict_FF
	rts

axrom_switch_conflict_01:
	sta conflict_01
	rts

axrom_switch_conflict_02:
	sta conflict_02
	rts

.segment "CODE"

.macro PPU_LATCH addr
	lda $2002
	lda #>addr
	sta $2006
	lda #<addr
	sta $2006
.endmacro

.macro PPU_LATCH_AT ax,ay
	PPU_LATCH ($2000+(ay*32)+ax)
.endmacro

.macro LOAD_PTR addr
	lda #<addr
	sta ptr+0
	lda #>addr
	sta ptr+1
.endmacro

.macro PPU_STRING ax,ay,str_addr
	PPU_LATCH_AT ax,ay
	LOAD_PTR str_addr
	jsr ppu_string
.endmacro

.macro PPU_DELAY delay
	lda #delay
	jsr ppu_delay
.endmacro

ppu_string:
	ldy #0
	:
		lda (ptr), Y
		beq :+
		sta $2007
		iny
		jmp :-
	:
	rts

ppu_hex:
	pha
	lsr
	lsr
	lsr
	lsr
	clc
	adc #$A0
	sta $2007
	pla
	and #$F
	clc
	adc #$A0
	sta $2007
	rts

ppu_vblank: ; wait for vblank
	lda nmi_count
	:
		cmp nmi_count
		beq :-
	rts

ppu_wait:
	jsr ppu_vblank
	lda #%00000000
	sta $2001
	rts

ppu_ready:
	lda #1
	sta nmi_ready
	jsr ppu_vblank
	; turn on rendering
	lda #%00011110
	sta $2001
	; set scroll
	lda $2002
	lda #0
	sta $2005
	sta $2005
	rts

ppu_delay: ; wait A frames
	tax
	:
		jsr ppu_vblank
		dex
		bne :-
	rts

; testing

.segment "DATA"

string_title: .asciiz "MAPPER 7 TEST"
string_start: .asciiz "        STARTUP BANK:"
string_aprg:  .asciiz "           PRG BANKS:"
string_abus0: .asciiz "            07 VS 00:"
string_abus1: .asciiz "            00 VS FF:"
string_abus2: .asciiz "            07 VS 01:"
string_abus3: .asciiz "            07 VS 02:"
string_abus:  .asciiz "       BUS CONFLICTS:"
string_sub:   .asciiz "  SUBMAPPER DETECTED:"

string_yes:   .asciiz "YES"
string_no:    .asciiz "NO"

string_and:   .asciiz "AND"
string_none:  .asciiz "NONE"
string_q:     .asciiz "?"

.segment "RAM"
result_start: .res 1
result_aprg:  .res 1
result_abus0: .res 1
result_abus1: .res 1
result_abus2: .res 1
result_abus3: .res 1
result_abus:  .res 1
result_sub:   .res 1

.segment "DATA"
tiles:
.incbin "test.chr"

.segment "CODE"
fill_chr_ram:
	PPU_LATCH $0000
	jsr @tiles_4k
	jsr @tiles_4k
	rts
@tiles_4k:
	lda #<tiles
	sta ptr+0
	lda #>tiles
	sta ptr+1
	ldx #16
	ldy #0
	:
		lda (ptr), Y
		sta $2007
		iny
		bne :-
		inc ptr+1
		dex
		bne :-
	rts

test_startup:
	lda bank
	sta result_start
	rts

test_aprg:
	ldx #0
	stx result_aprg
	:
		txa
		sta temp
		jsr axrom_switch
		lda bank
		cmp temp
		bne :+
		sta result_aprg
		inx
		bne :-
	:
	; finish result
	inc result_aprg
	lda #0
	jsr axrom_switch
	rts

test_abus:
	; 07 & 00
	lda #0
	jsr axrom_switch
	lda #7
	jsr axrom_switch_conflict_00
	lda bank
	sta result_abus0
	; 00 & FF
	lda #7
	jsr axrom_switch
	lda #0
	jsr axrom_switch_conflict_FF
	lda bank
	sta result_abus1
	; 07 & 01
	lda #0
	jsr axrom_switch
	lda #7
	jsr axrom_switch_conflict_01
	lda bank
	sta result_abus2
	; 07 & 02
	lda #0
	jsr axrom_switch
	lda #7
	jsr axrom_switch_conflict_02
	lda bank
	sta result_abus3
	; restore bank 0
	lda #0
	jsr axrom_switch
	; evaluate
	lda result_abus0 ; 07 & 00
	cmp #0
	bne @not_and
	lda result_abus1 ; 00 & FF
	cmp #0
	bne @not_and
	lda result_abus2 ; 07 & 01
	cmp #1
	bne @not_and
	lda result_abus3 ; 07 & 02
	cmp #2
	bne @not_and
	jmp @and
@not_and:
	lda result_abus0 ; 07
	cmp #7
	bne @not_none
	lda result_abus1 ; 00
	cmp #0
	bne @not_none
	lda result_abus2 ; 07
	cmp #7
	bne @not_none
	lda result_abus3 ; 07
	cmp #7
	bne @not_none
	jmp @none
@not_none:
	jmp @unknown
	; result
@none:
	; no bus conflicts
	lda #0
	sta result_abus
	rts
@and:
	; and bus conflicts
	lda #1
	sta result_abus
	rts
@unknown:
	; unknown result
	lda #2
	sta result_abus
	rts

test_sub:
	lda result_abus
	clc
	adc #1 ; 1 = no bus conflicts, 2 = AND bus conflicts, 3+ = unknown bus conflicts
	sta result_sub
	rts

print_yes_no:
	cmp #0
	beq :+
		LOAD_PTR string_yes
		jmp :++
	:
		LOAD_PTR string_no
	:
	jsr ppu_string
	rts

print_result_bus:
	cmp #0
	bne :+
		LOAD_PTR string_none
		jmp @print
	:
	cmp #1
	bne :+
		LOAD_PTR string_and
		jmp @print
	:
	LOAD_PTR string_q ; unknown result
@print:
	jsr ppu_string
	rts

testing:
	jsr test_startup
	; wait a few frames to give TV time to warm up
	PPU_DELAY 30
	; startup
	jsr ppu_wait
	PPU_STRING 4,6,string_start
	PPU_LATCH_AT 26,6
	lda result_start
	jsr ppu_hex
	jsr ppu_ready
	; AXROM prg
	jsr ppu_wait
	jsr test_aprg
	PPU_STRING 4,7,string_aprg
	PPU_LATCH_AT 26,7
	lda result_aprg
	jsr ppu_hex
	jsr ppu_ready
	; AXROM bus conflicts
	jsr ppu_wait
	jsr test_abus
	PPU_STRING 4,8,string_abus0
	PPU_LATCH_AT 26,8
	lda result_abus0
	jsr ppu_hex
	PPU_STRING 4,9,string_abus1
	PPU_LATCH_AT 26,9
	lda result_abus1
	jsr ppu_hex
	PPU_STRING 4,10,string_abus2
	PPU_LATCH_AT 26,10
	lda result_abus2
	jsr ppu_hex
	PPU_STRING 4,11,string_abus3
	PPU_LATCH_AT 26,11
	lda result_abus3
	jsr ppu_hex
	PPU_STRING 4,12,string_abus
	PPU_LATCH_AT 26,12
	lda result_abus
	jsr print_result_bus
	jsr ppu_ready
	; submapper
	jsr ppu_wait
	jsr test_sub
	PPU_STRING 4,13,string_sub
	PPU_LATCH_AT 26,13
	lda result_sub
	cmp #3
	bcc :+
		LOAD_PTR string_q
		jsr ppu_string
		jmp :++
	:
		jsr ppu_hex
	:
	jsr ppu_ready
	; end
	lda #7
	jsr axrom_switch
	:
	jmp :-

; main

irq:
	rti

nmi:
	pha
	txa
	pha
	tya
	pha
	lda nmi_ready
	beq :+
		; OAM DMA
		lda #00
		sta $2003
		lda #>oam
		sta $4014
		; end nmi
		lda #0
		sta nmi_ready
	:
	; nmi counter
	inc nmi_count
	pla
	tay
	pla
	tax
	pla
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
	; finished reset and warm-up
	; clear sprites
	lda #$FF
	ldx #0
	:
		sta oam, X
		inx
		bne :-
	; clear screen
	PPU_LATCH $2000
	lda #0
	ldx #0
	ldy #(4*4)
	:
		sta $2007
		inx
		bne :-
		dey
		bne :-
	; set a palette
	PPU_LATCH $3F00
	ldx #8
	:
		lda #$0F
		sta $2007
		lda #$13
		sta $2007
		lda #$23
		sta $2007
		lda #$30
		sta $2007
		dex
		bne :-
	; tiles to CHR RAM
	jsr fill_chr_ram
	; title
	PPU_STRING 4,4,string_title
	; turn on nmi
	lda #%10000000
	sta $2000
	; start testing
	jmp testing

.segment "VECTORS"
.word nmi
.word reset
.word irq
