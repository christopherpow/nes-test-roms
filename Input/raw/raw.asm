;;; public domain (lidnariq 2013)
;;; assemble with xa65
	.byt "NES",$1a
	.byt 1	; 16KiB PRG
	.byt 1	; 8KiB CHR
	.byt $00,$08	; mapper 0x0, horz, no bat, no trainer; iNES2, notVS, not PC10
	.byt 0	; mapper 0x000, submapper 0
	.byt 0	; num 4MB prgrom banks, num 2MB chrrom banks
	.byt 0	; no prgram
	.byt 0	; no chrram
	.byt 2	; works equally well on NTSC and PAL
	.byt 0,0,0	; padding

#include "constants.inc"

#define ShiftAndDraw() .(:\
	tya:\
	ror:\
	tay:\
	lda #1:\
	adc #0:\
	sta PPU_DATA:\
	.)

	.text
*=$c000
	.dsb ($fe00 - *)
palette:
	.byt $30,$30,$30,$0f
	.byt $30,$30,$30,$0f
	.byt $30,$30,$30,$0f
	.byt $30,$30,$30,$0f
	.byt $30,$30,$30,$0f
	.byt $30,$30,$30,$0f
	.byt $30,$30,$30,$0f
	.byt $30,$30,$30,$0f

reset:
	sei
	cld
	ldx #0
	stx PPU_CTRL ; disable nmi
	stx PPU_MASK ; disable rendering
	stx APU_PERIOD_DMC ; disable DMC irq

	lda #APU_MODE_IRQ_DIS
	sta APU_MODE	; disable frame IRQ

	ldx #3	; wait 2-or-3 vblanks
wvblank:	
	bit PPU_STATUS
	bpl wvblank
	dex
	bne wvblank

	ldy #$3F
	sty PPU_ADDR
	stx PPU_ADDR	; 0x3F00- palettes
	
	ldy #31	; write going down in memory
writingpalette:	
	lda palette,y
	sta PPU_DATA	; write palette...
	dey
	bpl writingpalette

	ldy #$20
	sty PPU_ADDR
	stx PPU_ADDR	; clear nametable
	ldy #0
clearnametable: 
	stx PPU_DATA
	stx PPU_DATA
	stx PPU_DATA
	stx PPU_DATA
	dey
	bnz clearnametable
	
	lda #PPU_CTRL_NMI_ENA|PPU_CTRL_INC32
	sta PPU_CTRL

spin:
	jmp spin

nmi:
	ldx #0
	stx PPU_MASK

	ldy #1
	sty JOY_1
	dey
	sty JOY_1

next: 
	ldy #$20
	sty PPU_ADDR
	stx PPU_ADDR
	ldy JOY_1

	ShiftAndDraw()
	ShiftAndDraw()
	ShiftAndDraw()
	ShiftAndDraw()
	ShiftAndDraw()

	ldy #0
	sty PPU_DATA
	
	ldy JOY_2

	ShiftAndDraw()
	ShiftAndDraw()
	ShiftAndDraw()
	ShiftAndDraw()
	ShiftAndDraw()

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

	lda #PPU_MASK_LEFT_BKGD_SHOW|PPU_MASK_BKGD_ENA
	sta PPU_MASK	; enable rendering

	rti


	.dsb ($fffa - *)
Vectors:
	.word nmi
	.word reset
	.word reset

	.dsb 16
	.byt 0,0,0,$18,0,0,0,0
	.byt 0,0,0,$18,0,0,0,0

	.byt 0,$3c,$7e,$7e,$7e,$7e,$3c,0
	.byt 0,$3c,$7e,$7e,$7e,$7e,$3c,0
	.dsb (4096-48)
	.dsb 4096
