
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
bank_number:
.byte BANK

.segment "DATA"
tiles:
.byte $00 ; marks bank 0
.incbin "34_test.chr"
.byte $01 ; marks bank 1
.incbin "34_test.chr"

; BNROM bankswitching

conflict_table:
.byte $00, $01, $02, $03, $04, $05, $06, $07

conflict_00:
.byte $00

conflict_FF:
.byte $FF

bnrom_switch:
	tay
	sta conflict_table, Y
	rts

bnrom_switch_conflict_00:
	sta conflict_00
	rts

bnrom_switch_conflict_FF:
	sta conflict_FF
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

string_title: .asciiz "MAPPER 34 TEST"
string_start: .asciiz "        STARTUP BANK:"
string_vram:  .asciiz "             CHR RAM:"
string_wram:  .asciiz "             PRG RAM:"
string_bprg:  .asciiz "BNROM      PRG BANKS:"
string_bbus:  .asciiz "BNROM  BUS CONFLICTS:"
string_nprg:  .asciiz "NINA-001   PRG BANKS:"
string_nchr:  .asciiz "NINA-001   CHR BANKS:"
string_byes:  .asciiz "         BNROM VALID:"
string_nyes:  .asciiz "      NINA-001 VALID:"
string_sub:   .asciiz "  SUBMAPPER DETECTED:"

string_yes:   .asciiz "YES"
string_no:    .asciiz "NO"
string_q:     .asciiz "?"

.segment "RAM"
result_start: .res 1
result_vram:  .res 1
result_wram:  .res 1
result_bprg:  .res 1
result_bbus:  .res 1
result_nprg:  .res 1
result_nchr:  .res 1
result_sub:   .res 1

.segment "CODE"

test_startup:
	lda bank_number
	sta result_start
	rts

test_vram:
	PPU_LATCH $0000
	lda #$FF
	sta $2007
	PPU_LATCH $0000
	lda $2007
	lda $2007
	cmp #$FF
	bne @fail
	PPU_LATCH $0000
	lda #$00
	sta $2007
	PPU_LATCH $0000
	lda $2007
	lda $2007
	cmp #$00
	bne @fail
@pass:
	lda #1
	sta result_vram
	rts
@fail:
	lda #0
	sta result_vram
	rts

load_vram:
	lda #<tiles
	sta ptr+0
	lda #>tiles
	sta ptr+1
	ldx #(8*4)
	ldy #0
	PPU_LATCH $0000
	:
		lda (ptr), Y
		sta $2007
		iny
		bne :-
		dex
		beq :+
		inc ptr+1
		jmp :-
	:
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

test_bprg:
	ldx #0
	stx result_bprg
	:
		txa
		jsr bnrom_switch
		cmp bank_number
		bne :+
		sta result_bprg
		inx
		bne :-
	:
	; finish result
	inc result_bprg
	rts

test_bbus:
	lda #0
	jsr bnrom_switch
	lda #1
	jsr bnrom_switch_conflict_FF
	cmp bank_number
	bne @pass
	jsr bnrom_switch
	lda #0
	jsr bnrom_switch_conflict_FF
	cmp bank_number
	bne @pass
	jsr bnrom_switch
	lda #1
	jsr bnrom_switch_conflict_00
	cmp bank_number
	bne @pass
	jsr bnrom_switch
	lda #0
	jsr bnrom_switch_conflict_00
	cmp bank_number
	bne @pass
	jsr bnrom_switch
@fail:
	; no bus conflicts
	lda #0
	sta result_bbus
	rts
@pass:
	; bus conflicts
	lda #1
	sta result_bbus
	rts

test_nprg:
	ldx #0
	stx result_nprg
	:
		txa
		sta $7FFD
		cmp bank_number
		bne :+
		sta result_nprg
		inx
		bne :-
	:
	; finish result
	inc result_nprg
	rts

test_nchr:
	ldx #0
	stx result_nchr
	:
		; first byte of CHR identifies 4k bank
		txa
		sta temp
		sta $7FFE ; test low 4k bank
		PPU_LATCH $0000
		lda $2007
		lda $2007
		cmp temp
		bne :+
		sta $7FFF ; test high 4k bank
		PPU_LATCH $1000
		lda $2007
		lda $2007
		cmp temp
		bne :+
		sta result_nchr
		inx
		bne :-
	:
	; switch back to bank 0
	lda #0
	sta $7FFE
	sta $7FFF
	; finish result
	inc result_nchr
	rts

test_byes:
	ldx result_bprg
	dex
	bne @pass
@fail:
	lda #0
	rts
@pass:
	lda #1
	rts

test_nyes:
	ldx result_nprg
	dex
	beq @fail
	ldx result_nchr
	dex
	beq @fail
@pass:
	lda #1
	rts
@fail:
	lda #0
	rts

test_sub:
	ldx result_nprg
	dex
	beq @incomplete_nina
	ldx result_nchr
	dex
	beq @incomplete_nina
	ldx result_bprg
	dex
	bne @0
	jmp @1
@incomplete_nina:
	ldx result_nprg
	dex
	bne @3
	ldx result_nchr
	dex
	bne @3
	ldx result_bprg
	dex
	bne @2
	jmp @3
@0:
	; detected NINA PRG, NINA CHR, BNROM PRG (i.e. both submapper features at once)
	lda #0
	sta result_sub
	rts
@1:
	; detected NINA PRG, NINA CHR, but not BNROM PRG
	lda #1
	sta result_sub
	rts
@2:
	; detected BNROM PRG, but not NINA PRG, NINA CHR
	lda #2
	sta result_sub
	rts
@3:
	; detected strange combination
	lda #3
	sta result_sub
	rts

yes_no:
	cmp #0
	beq :+
		LOAD_PTR string_yes
		jmp :++
	:
		LOAD_PTR string_no
	:
	jsr ppu_string
	rts

testing:
	jsr test_startup
	jsr test_vram
	; initialize NINA-001 CHR banks
	lda #0
	sta $7FFE
	lda #1
	sta $7FFF
	; load VRAM if necessary
	lda result_vram
	beq :+
		jsr load_vram
	:
	; wait a few frames to give TV time to warm up
	PPU_DELAY 30
	; startup
	jsr ppu_wait
	PPU_STRING 4,6,string_start
	PPU_LATCH_AT 26,6
	lda result_start
	jsr ppu_hex
	jsr ppu_ready
	; vram
	jsr ppu_wait
	PPU_STRING 4,7,string_vram
	lda result_vram
	PPU_LATCH_AT 26,7
	jsr yes_no
	jsr ppu_ready
	; wram
	jsr test_wram
	jsr ppu_wait
	PPU_STRING 4,8,string_wram
	lda result_wram
	PPU_LATCH_AT 26,8
	jsr yes_no
	jsr ppu_ready
	; BNROM prg
	jsr test_bprg
	jsr ppu_wait
	PPU_STRING 4,9,string_bprg
	PPU_LATCH_AT 26,9
	lda result_bprg
	jsr ppu_hex
	jsr ppu_ready
	; BNROM bus conflicts
	jsr test_bbus
	jsr ppu_wait
	PPU_STRING 4,10,string_bbus
	lda result_bbus
	PPU_LATCH_AT 26,10
	jsr yes_no
	jsr ppu_ready
	; NINA-001 prg
	jsr test_nprg
	jsr ppu_wait
	PPU_STRING 4,11,string_nprg
	PPU_LATCH_AT 26,11
	lda result_nprg
	jsr ppu_hex
	jsr ppu_ready
	; NINA-001 chr
	jsr ppu_wait ; turn off rendering before test
	jsr test_nchr
	PPU_STRING 4,12,string_nchr
	PPU_LATCH_AT 26,12
	lda result_nchr
	jsr ppu_hex
	jsr ppu_ready
	; BNROM
	jsr ppu_wait
	PPU_STRING 4,14,string_byes
	PPU_LATCH_AT 26,14
	jsr test_byes
	jsr yes_no
	jsr ppu_ready
	; NINA-001
	jsr ppu_wait
	PPU_STRING 4,15,string_nyes
	PPU_LATCH_AT 26,15
	jsr test_nyes
	jsr yes_no
	jsr ppu_ready
	; submapper
	jsr test_sub
	jsr ppu_wait
	PPU_STRING 4,16,string_sub
	PPU_LATCH_AT 26,16
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
