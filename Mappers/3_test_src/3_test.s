
.segment "ZEROPAGE"
nmi_ready:    .res 1
nmi_count:    .res 1
ptr:          .res 2
temp:         .res 1

.segment "STACK"
stack: .res 256

.segment "OAM"
oam: .res 256

.segment "ALIGNED"

; CNROM bankswitching

conflict_table:
.byte $00, $01, $02, $03

conflict_00:
.byte $00

conflict_FF:
.byte $FF

conflict_01:
.byte $01

conflict_02:
.byte $02

cnrom_switch:
	tay
	sta conflict_table, Y
	rts

cnrom_switch_conflict_00:
	sta conflict_00
	rts

cnrom_switch_conflict_FF:
	sta conflict_FF
	rts

cnrom_switch_conflict_01:
	sta conflict_01
	rts

cnrom_switch_conflict_02:
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

string_title: .asciiz "MAPPER 3 TEST"
string_start: .asciiz "    STARTUP CHR BANK:"
string_wram:  .asciiz "             PRG RAM:"
string_cchr:  .asciiz "           CHR BANKS:"
string_cbus0: .asciiz "            03 VS 00:"
string_cbus1: .asciiz "            00 VS FF:"
string_cbus2: .asciiz "            03 VS 01:"
string_cbus3: .asciiz "            03 VS 02:"
string_cbus:  .asciiz "       BUS CONFLICTS:"
string_sub:   .asciiz "  SUBMAPPER DETECTED:"

string_yes:   .asciiz "YES"
string_no:    .asciiz "NO"

string_and:   .asciiz "AND"
string_none:  .asciiz "NONE"
string_q:     .asciiz "?"

.segment "RAM"
result_start: .res 1
result_wram:  .res 1
result_cchr:  .res 1
result_cbus0: .res 1
result_cbus1: .res 1
result_cbus2: .res 1
result_cbus3: .res 1
result_cbus:  .res 1
result_sub:   .res 1

.segment "CODE"

read_chr_bank:
	PPU_LATCH $0000
	lda $2007
	lda $2007
	rts

test_startup:
	jsr read_chr_bank
	sta result_start
	rts

test_wram:
	lda #$FF
	sta $6000
	lda #$00
	lda $6000
	cmp #$FF
	bne @fail
	lda #$00
	sta $6000
	lda #$FF
	lda $6000
	cmp #$00
	bne @fail
@pass:
	lda #1
	sta result_wram
	rts
@fail:
	lda #0
	sta result_wram
	rts

test_cchr:
	ldx #0
	stx result_cchr
	:
		txa
		sta temp
		jsr cnrom_switch
		jsr read_chr_bank
		cmp temp
		bne :+
		sta result_cchr
		inx
		bne :-
	:
	; finish result
	inc result_cchr
	lda #0
	jsr cnrom_switch
	rts

test_cbus:
	; 03 & 00
	lda #0
	jsr cnrom_switch
	lda #3
	jsr cnrom_switch_conflict_00
	jsr read_chr_bank
	sta result_cbus0
	; 00 & FF
	lda #3
	jsr cnrom_switch
	lda #0
	jsr cnrom_switch_conflict_FF
	jsr read_chr_bank
	sta result_cbus1
	; 03 & 01
	lda #0
	jsr cnrom_switch
	lda #3
	jsr cnrom_switch_conflict_01
	jsr read_chr_bank
	sta result_cbus2
	; 03 & 02
	lda #0
	jsr cnrom_switch
	lda #3
	jsr cnrom_switch_conflict_02
	jsr read_chr_bank
	sta result_cbus3
	; restore bank 0
	lda #0
	jsr cnrom_switch
	; evaluate
	lda result_cbus0 ; 03 & 00
	cmp #0
	bne @not_and
	lda result_cbus1 ; 00 & FF
	cmp #0
	bne @not_and
	lda result_cbus2 ; 03 & 01
	cmp #1
	bne @not_and
	lda result_cbus3 ; 03 & 02
	cmp #2
	bne @not_and
	jmp @and
@not_and:
	lda result_cbus0 ; 03
	cmp #3
	bne @not_none
	lda result_cbus1 ; 00
	cmp #0
	bne @not_none
	lda result_cbus2 ; 03
	cmp #3
	bne @not_none
	lda result_cbus3 ; 03
	cmp #3
	bne @not_none
	jmp @none
@not_none:
	jmp @unknown
	; result
@none:
	; no bus conflicts
	lda #0
	sta result_cbus
	rts
@and:
	; and bus conflicts
	lda #1
	sta result_cbus
	rts
@unknown:
	; unknown result
	lda #2
	sta result_cbus
	rts

test_sub:
	lda result_cbus
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
	; wram
	jsr ppu_wait
	jsr test_wram
	PPU_STRING 4,7,string_wram
	lda result_wram
	PPU_LATCH_AT 26,7
	jsr print_yes_no
	jsr ppu_ready
	; CNROM chr
	jsr ppu_wait
	jsr test_cchr
	PPU_STRING 4,8,string_cchr
	PPU_LATCH_AT 26,8
	lda result_cchr
	jsr ppu_hex
	jsr ppu_ready
	; CNROM bus conflicts
	jsr ppu_wait
	jsr test_cbus
	PPU_STRING 4,9,string_cbus0
	PPU_LATCH_AT 26,9
	lda result_cbus0
	jsr ppu_hex
	PPU_STRING 4,10,string_cbus1
	PPU_LATCH_AT 26,10
	lda result_cbus1
	jsr ppu_hex
	PPU_STRING 4,11,string_cbus2
	PPU_LATCH_AT 26,11
	lda result_cbus2
	jsr ppu_hex
	PPU_STRING 4,12,string_cbus3
	PPU_LATCH_AT 26,12
	lda result_cbus3
	jsr ppu_hex
	PPU_STRING 4,13,string_cbus
	PPU_LATCH_AT 26,13
	lda result_cbus
	jsr print_result_bus
	jsr ppu_ready
	; submapper
	jsr ppu_wait
	jsr test_sub
	PPU_STRING 4,14,string_sub
	PPU_LATCH_AT 26,14
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
