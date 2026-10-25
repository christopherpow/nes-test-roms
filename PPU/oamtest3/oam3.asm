	.zero
outside	.byt 0
oldbuttons	.byt 0
Buttons	.byt 0
indir	.word 0
count	.byt 0

	.bss
	*=$700
values 	.dsb 16
	
	.text
*=$F800
#include "constants.inc"
nmi:
	lda PPU_STATUS	; clear bit, we'll finish too fast
	
	ldx #JOY_1_LATCH	; 2 \\ begin
	stx JOY_1	; 4 ; strobe controller
	lda JOY_1	; 4 ; read other bits; won't clock register
	dex	; 2
	stx JOY_1	; 4
	and #$FC	; 2
	tax	; 2 ; now X = other bits

	cpx JOY_1	; 4
	ror	; 2
	cpx JOY_1	; 4
	ror	; 2
	cpx JOY_1	; 4
	ror	; 2
	cpx JOY_1	; 4
	ror	; 2
	cpx JOY_1	; 4
	ror	; 2
	cpx JOY_1	; 4
	ror	; 2
	cpx JOY_1	; 4
	ror	; 2
	cpx JOY_1	; 4
	ror	; 2 // end : 68 cycles
	sta Buttons	; 3

	lda Buttons
	cmp oldbuttons
	bne ContinueProcessing
	jmp donemode
ContinueProcessing
	sta oldbuttons
	
	lda #JOY_UP
	bit Buttons
	bnz NotUp
	lda #1
	bit count
	bnz LowNybbleUp
HighNybbleUp
	lda count
	lsr
	tax
	clc
	lda #16
	adc values,x
	sta values,x
	jmp NotUp
LowNybbleUp
	lda count
	lsr
	tax
	clc
	lda #1
	adc values,x
	sta values,x
NotUp

	lda #JOY_DN
	bit Buttons
	bnz NotDown
	lda #1
	bit count
	bnz LowNybbleDown
HighNybbleDown
	lda count
	lsr
	tax
	clc
	lda #240
	adc values,x
	sta values,x
	jmp NotDown
LowNybbleDown
	lda count
	lsr
	tax
	clc
	lda #255
	adc values,x
	sta values,x
NotDown

	lda #JOY_LT
	bit Buttons
	bnz NotLt
	dec count
	bpl NotLt
	lda #31
	sta count
NotLt
	
	lda #JOY_RT
	bit Buttons
	bnz NotRt
	inc count
	lda count
	cmp #32
	blt NotRt
	lda #0
	sta count
NotRt

	lda values
	bpl nocliplo
	lda #0
	sta values
nocliplo
	cmp #15
	blt nocliphi
	lda #14
	sta values
nocliphi
	
	sta outside
	inc outside
	
	ldy #0
	lda #>NameTable(0,15)
	sta PPU_ADDR
	lda #<NameTable(0,15)
	sta PPU_ADDR
	jmp SkipFirstOAM

NumbersLoop
	lda values,y
	sta PPU_OAM_DATA
SkipFirstOAM
	lda values,y
	lsr
	lsr
	lsr
	lsr
	tax
	lda hex,x
	sta PPU_DATA
	lda values,y
	and #15
	tax
	lda hex,x
	sta PPU_DATA

	iny
	dec outside
	bnz NumbersLoop

ClearLine	
	lda #'.'
	sta PPU_DATA

	iny
	cpy #32
	blt ClearLine
	

	lda #>NameTable(0,16)
	sta PPU_ADDR
	lda #<NameTable(0,16)
	sta PPU_ADDR

	ldx count
	inx
	ldy #32
ArrowLoop
	dex
	bz DrawMarker
	lda #' '
	sta PPU_DATA
	jmp DoneDraw
DrawMarker
	lda #'!'
	sta PPU_DATA
DoneDraw
	dey
	bnz ArrowLoop
	
donemode	

	lda #0
	sta PPU_SCROLL
	sta PPU_SCROLL

	rti

	
irq:
reset:
	sei
	cld

	ldx #0	; reset x (0 on power-up, not reset)
	;; PPU_CTRL_NMI_DIS
	stx PPU_CTRL	; disable nmi
	;; PPU_MASK_BKGD_DIS|PPU_MASK_SPR_DIS
	stx PPU_MASK	; disable rendering
	;; APU_DMC_IRQ_DIS
	stx APU_PERIOD_DMC	; disable DMC IRQ
	ldy #APU_MODE_IRQ_DIS
	sty APU_MODE	; disable frame IRQ

	bit PPU_STATUS	; clear any recently-passed vblank

	txa
	tay
clrmem:	sta 0,x	; clear memory
	sta $100,x
	sta $200,x
	sta $300,x
	sta $400,x
	sta $500,x
	sta $600,x
       	sta $700,x
	inx
	bne clrmem

	dex
	txs	; reset stack to $ff

	ldx #2	; wait 2 vblanks
wvblank:
	bit PPU_STATUS
	bpl wvblank
	dex
	bne wvblank

	ldy #$3F
	sty PPU_ADDR
	stx PPU_ADDR	; 0x3F00- palettes

	ldy #$1F	; write going down in memory
writingpalette:
	lda palette,y
	sta PPU_DATA	; write palette...
	dey
	bpl writingpalette

	
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
	bnz repeat

	ldx #4
	ldy #$C0
	lda #'.'
namin: 
	sta PPU_DATA
	dey
	bnz namin
	dex
	bnz namin

	ldy #$40
	lda #0
attrin: 
	sta PPU_DATA
	dey
	bnz attrin

	lda #7
	sta values
	
	;; and init video
	lda #PPU_MASK_LEFT_BKGD_SHOW|PPU_MASK_LEFT_SPR_SHOW|PPU_MASK_BKGD_ENA|PPU_MASK_SPR_ENA
	sta PPU_MASK
	lda #PPU_CTRL_SPR_LEFT|PPU_CTRL_BKGD_RIGHT|PPU_CTRL_NMI_ENA
	sta PPU_CTRL

	;; and wait forever
spin
	jmp spin

palette:
	.byt $21,0,0,PAL_BLACK
	.byt $24,0,0,PAL_BLACK
	.byt $27,0,0,PAL_BLACK
	.byt $2A,0,0,PAL_BLACK
	.byt $2d,0,0,PAL_BLACK
	.byt $2d,0,0,PAL_BLACK
	.byt $2d,0,0,PAL_BLACK
	.byt $2d,0,0,PAL_BLACK

hex:
	.asc "0123456789ABCDEF"
chr:
	.bin 0,512,"alnum.1bp"

vectors: 
	.dsb ($fffa-*)
	.word nmi
	.word reset
	.word irq
