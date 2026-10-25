;;; public domain (lidnariq 2021)
;;; assemble with cl65 -t none -o pack2.nes pack2.asm

	.ZEROPAGE
outside:	.byt 0
indir:	.word 0
r1:	.byt 0
r2:	.byt 0

	.CODE
	.org $BFF0
	.include "constants.ca65.inc"
	.macpack generic

	.byt "NES",$1A,1,0,$70,0,0,0,0,0,0,0,0,0


reset:
	sei
	cld

	ldx #0	; reset x (0 on power-up, not reset)
	stx PPU_CTRL	; disable nmi
	stx PPU_MASK	; disable rendering
	stx APU_PERIOD_DMC	; disable DMC IRQ
	ldy #$40
	sty APU_MODE	; disable frame IRQ

	bit PPU_STATUS	; clear any recently-passed vblank
wvblank1:
	bit PPU_STATUS
	bpl wvblank1
wvblank2:
	bit PPU_STATUS
	bpl wvblank2

	ldy #$3F
	sty PPU_ADDR
	stx PPU_ADDR	; 0x3F00- palettes

	ldy #$1F	; write going down in memory
writingpalette:
	lda palette,y
	sta PPU_DATA	; write palette...
	dey
	bpl writingpalette

	jsr UploadChrFor218

	ldy #$20
	sty PPU_ADDR
	stx PPU_ADDR	; clear nametable
	ldy #0
	lda #4
clearnametable:
	sta PPU_DATA
	sta PPU_DATA
	sta PPU_DATA
	sta PPU_DATA
	dey
	bnz clearnametable

	lda #PPUCTRL::NMI_ENA|PPUCTRL::INC32
	sta PPU_CTRL

	;; enable NMI

	;; and wait forever
spin:
	jmp spin

nmi:
	ldx #0
	stx PPU_MASK

	inx
	stx JOY_1
	dex
	stx JOY_1

next:
	ldy #$20
	sty PPU_ADDR
	stx PPU_ADDR


	lda JOY_1
	sta r1
	lda JOY_1
	sta r2

	lda #0
	lsr r2
	rol
	lsr r1
	rol
	sta PPU_DATA

	lda #0
	lsr r2
	rol
	lsr r1
	rol
	sta PPU_DATA

	lda #0
	lsr r2
	rol
	lsr r1
	rol
	sta PPU_DATA

	lda #0
	lsr r2
	rol
	lsr r1
	rol
	sta PPU_DATA

	lda #0
	lsr r2
	rol
	lsr r1
	rol
	sta PPU_DATA

	lda #4
	sta PPU_DATA

	lda JOY_2
	sta r1
	lda JOY_2
	sta r2

	lda #0
	lsr r2
	rol
	lsr r1
	rol
	sta PPU_DATA

	lda #0
	lsr r2
	rol
	lsr r1
	rol
	sta PPU_DATA

	lda #0
	lsr r2
	rol
	lsr r1
	rol
	sta PPU_DATA

	lda #0
	lsr r2
	rol
	lsr r1
	rol
	sta PPU_DATA

	lda #0
	lsr r2
	rol
	lsr r1
	rol
	sta PPU_DATA

	inx
	cpx #32
	beq done
	jmp next
done:
	lda #0
	sta PPU_SCROLL
	sta PPU_SCROLL
	sta PPU_ADDR
	sta PPU_ADDR

	;; need to spin a little so that rendering starts at the right place
	ldx #28
tinydelay:
	dex
	bne tinydelay

	lda #PPUMASK::LEFT_BKGD_SHOW|PPUMASK::BKGD_ENA
	sta PPU_MASK	; enable rendering

	rti


UploadChrFor218:
	lda #0
	sta PPU_ADDR
	lda #0
	sta PPU_ADDR

	lda #8
	sta outside
repeat:
	lda #>chr
	sta indir+1
	lda #<chr
	sta indir
	ldx #64
	ldy #0
first:
	lda (indir),y
	sta PPU_DATA
	iny
	cpy #8
	bne first
	ldy #0
second:
	lda (indir),y
	sta PPU_DATA
	iny
	cpy #8
	bne second

	lda indir
	clc
	adc #8
	sta indir
	lda indir+1
	adc #0
	sta indir+1
	ldy #0
	dex
	bne first

	dec outside
	bne repeat
	rts

chr:
	.incbin "dots.1bp"


palette:
	.byt $21,0,0,$0f
	.byt $24,0,0,$0f
	.byt $27,0,0,$0f
	.byt $2A,0,0,$0f
	.byt $2d,0,0,$0f
	.byt $2d,0,0,$0f
	.byt $2d,0,0,$0f
	.byt $2d,0,0,$0f

vectors:
	.repeat ($fffa-*)
	.byte $FF
	.endrep
	.word nmi, reset, reset
