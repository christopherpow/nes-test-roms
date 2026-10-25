; MMC5 ram size test
; Brad Smith, 2019-11-16
; http://rainwarrior.ca

; results:
;  1. startup PRG-RAM bank at $6000
;  2. whether each bank retained a previous save (result OR $80)
;  3. bank found for each of 128 values written to $5113
;
; If a bank fails to act as RAM it will show a result of $FF.
; For a 1MB

.segment "HEADER"

.import batt
.import ines2
.import ramsize
.import splitram

INES_MAPPER     = 5 ; MMC5
INES_MIRROR     = 1 ; horizontal nametables
INES_PRG_16K    = 1 ; 16K
INES_CHR_8K     = 1 ; 8K
INES_BATTERY    = <batt
INES2           = %00001000 * (<ines2) ; NES 2.0 flag for bit 7
INES2_SUBMAPPER = 0
INES2_PRGRAM    = (<(ramsize+6)) * (1-(<batt)) + (<splitram)
INES2_PRGBAT    = (<(ramsize+6)) * (<batt)
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

.segment "ZEROPAGE"
ptr: .res 2
start_result: .res 1

.segment "RAM"
result: .res 128

.segment "CODE"

; test that each address line works

.macro MAKE_SAVE addr, value
	lda #value
	sta addr
.endmacro

.macro CHECK_SAVE addr, value
	lda addr
	cmp #value
	bne @done
.endmacro

make_save:
	MAKE_SAVE $6001, $12
	MAKE_SAVE $6002, $34
	MAKE_SAVE $6004, $56
	MAKE_SAVE $6008, $78
	MAKE_SAVE $6010, $9A
	MAKE_SAVE $6020, $BC
	MAKE_SAVE $6040, $DE
	MAKE_SAVE $6080, $F0
	MAKE_SAVE $6100, $13
	MAKE_SAVE $6200, $57
	MAKE_SAVE $6400, $9B
	MAKE_SAVE $6800, $DF
	MAKE_SAVE $7000, $24
	rts

wipe_save:
	MAKE_SAVE $6001, $00
	MAKE_SAVE $6002, $00
	MAKE_SAVE $6004, $00
	MAKE_SAVE $6008, $00
	MAKE_SAVE $6010, $00
	MAKE_SAVE $6020, $00
	MAKE_SAVE $6040, $00
	MAKE_SAVE $6080, $00
	MAKE_SAVE $6100, $00
	MAKE_SAVE $6200, $00
	MAKE_SAVE $6400, $00
	MAKE_SAVE $6800, $00
	MAKE_SAVE $7000, $00
	rts

is_saved:
	CHECK_SAVE $6001, $12
	CHECK_SAVE $6002, $34
	CHECK_SAVE $6004, $56
	CHECK_SAVE $6008, $78
	CHECK_SAVE $6010, $9A
	CHECK_SAVE $6020, $BC
	CHECK_SAVE $6040, $DE
	CHECK_SAVE $6080, $F0
	CHECK_SAVE $6100, $13
	CHECK_SAVE $6200, $57
	CHECK_SAVE $6400, $9B
	CHECK_SAVE $6800, $DF
	CHECK_SAVE $7000, $24
@done:
	rts

is_wiped:
	CHECK_SAVE $6001, $00
	CHECK_SAVE $6002, $00
	CHECK_SAVE $6004, $00
	CHECK_SAVE $6008, $00
	CHECK_SAVE $6010, $00
	CHECK_SAVE $6020, $00
	CHECK_SAVE $6040, $00
	CHECK_SAVE $6080, $00
	CHECK_SAVE $6100, $00
	CHECK_SAVE $6200, $00
	CHECK_SAVE $6400, $00
	CHECK_SAVE $6800, $00
	CHECK_SAVE $7000, $00
@done:
	rts

; result display

.macro PPU_LATCH addr
	lda $2002
	lda #>addr
	sta $2006
	lda #<addr
	sta $2006
.endmacro

.macro PPU_XY ax,ay
	PPU_LATCH ($2000+(ay*32)+ax)
.endmacro

.macro PPU_STRING ax,ay,str_addr
	PPU_XY ax,ay
	lda #<str_addr
	sta ptr+0
	lda #>str_addr
	sta ptr+1
	jsr ppu_string
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

vsync:
	bit $2002
	:
		bit $2002
		bpl :-
	rts

; main

main:
	; first PPU wait
	jsr vsync
	; MMC5 PRG already initialized
	; MMC5 init continues
	lda #0
	sta $5101 ; CHR
	sta $5120
	sta $5121
	sta $5122
	sta $5123
	sta $5124
	sta $5125
	sta $5126
	sta $5127
	sta $5128
	sta $5129
	sta $512A
	sta $512B
	sta $5130
	lda #$02
	sta $5102 ; PRG RAM write enable
	lda #$01
	sta $5103
	lda #$00
	sta $5104 ; ExRAM
	lda #$44
	sta $5105 ; horizontal nametables
	lda #$00
	sta $5106 ; fill mode
	sta $5107
	sta $5200 ; vertical split
	sta $5201
	sta $5202
	sta $5203 ; IRQ
	sta $5204
	sta $5205 ; multiplier
	sta $5206
	lda #$FF
	sta $5207 ; misc MMC5A only
	lda #$00
	sta $5208
	sta $5209
	sta $520A
	; second PPU wait
	jsr vsync
	; check startup bank (if it was saved)
	lda #$FF
	sta start_result
	jsr is_saved
	bne :+
		lda $6000
		sta start_result
	:
	; mark startup bank (in case not saved)
	lda #$42
	sta $6003
	lda #$35
	sta $6005
	; check for previously saved data
	ldx #128
	@save_check_loop:
		dex
		txa
		ora #$80
		sta $5113
		jsr is_saved
		bne :+
			lda #$80
			jmp :++
		:
			lda #$00
		:
		sta result, X
		cpx #0
		bne @save_check_loop
	; set up all banks with a number
	ldx #128
	@bank_setup_loop:
		dex
		txa
		ora #$80
		sta $5113
		stx $6000
		cpx #0
		bne @bank_setup_loop
	; verify bank number
	ldx #128
	@bank_check_loop:
		dex
		txa
		ora #$80
		sta $5113
		jsr wipe_save
		jsr is_wiped
		bne @bad_bank
		jsr make_save
		jsr is_saved
		beq @good_bank
		@bad_bank:
			lda #$FF ; not working RAM
			jmp @next_bank_check
		@good_bank:
			lda $6000
		@next_bank_check:
		ora result, X
		sta result, X
		cpx #0
		bne @bank_check_loop
	; if startup bank is still unknown, check now
	lda start_result
	cmp #$FF
	bne @start_done
	ldx #0
	@start_check_loop:
		txa
		ora #$80
		sta $5113
		lda $6003
		cmp #$42
		bne :+
		lda $6005
		cmp #$35
		bne :+
			lda $6000
			sta start_result
			jmp @start_done
		:
		inx
		cpx #128
		bcc @start_check_loop
	@start_done:
	; reset to startup bank
	lda start_result
	ora #$80
	sta $5113
	; display results
	PPU_LATCH $2000
	lda #0
	tax
	tay
	:
		sta $2007
		inx
		bne :-
		iny
		cpy #16
		bcc :-
	PPU_LATCH $3F00
	ldx #0
	:
		lda #$0F
		sta $2007
		lda #$00
		sta $2007
		lda #$10
		sta $2007
		lda #$20
		sta $2007
		inx
		cpx #8
		bcc :-
	PPU_STRING 4,4,string_1
	PPU_STRING 4,6,string_2
	PPU_STRING 4,8,string_3
	lda start_result
	jsr ppu_hex
	PPU_XY 4,10
	ldx #0
	:
		lda result, X
		jsr ppu_hex
		lda #0
		sta $2007
		inx
		txa
		and #7
		bne :+
			lda #0
			sta $2007
			sta $2007
			sta $2007
			sta $2007
			sta $2007
			sta $2007
			sta $2007
			sta $2007
		:
		cpx #128
		bcc :--
	; turn on screen
	jsr vsync
	lda #$00
	bit $2002
	sta $2005
	sta $2005
	lda #%00001010
	sta $2001
	:
	jmp :-

string_1: .asciiz "MMC5 RAM SIZE TEST"
string_2: .asciiz "HIGH BIT = SAVE RETAINED"
string_3: .asciiz "STARTUP: "

.segment "FIXED"

nmi:
irq:
	rti

; startup stub

reset:
	sei
	cld
	ldx #$40
	sta $4017
	ldx #$FF
	txs
	ldx #$00
	stx $2000
	stx $2001
	stx $4010
	; MMC5 PRG init
	lda #$01
	sta $5100
	lda #$00
	;sta $5113 ; initialized later, test startup bank first
	sta $5114
	sta $5115
	sta $5116
	lda #$FF
	sta $5117
	jmp main

.segment "VECTORS"
.addr nmi
.addr reset
.addr irq

.segment "CHR"
.incbin "mmc5ramsize.chr"
.incbin "mmc5ramsize.chr"
