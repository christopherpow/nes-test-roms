;;; public domain (lidnariq 2022)
;;; assemble with cl65 -t none -o vaus.nes vaus.asm

	.ZEROPAGE
outside:	.byt 0
indir:	.word 0
resF:	.word 0
resN:	.word 0

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

	lda #$ff
	ldx #0
oamshadowclear:	
	sta $200,x
	inx
	bnz oamshadowclear

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
	lda #' '
clearnametable:
	sta PPU_DATA
	sta PPU_DATA
	sta PPU_DATA
	sta PPU_DATA
	dey
	bnz clearnametable

	lda #PPUMASK::LEFT_BKGD_SHOW | PPUMASK::LEFT_SPR_SHOW | PPUMASK::BKGD_ENA | PPUMASK::SPR_ENA
	sta PPU_MASK

	;; enable NMI
	lda #PPUCTRL::NMI_ENA
	sta PPU_CTRL

	;; and wait forever
spin:
	jmp spin

nmi:
	bit PPU_STATUS
	ldx #8

vausreadf8:	
	lda $4017
	lsr a       ; nothing in 4017d0
	lsr a       ; famicom vaus in 4017d1
	rol resF
	lsr a       ; nothing in 4017d2
	lsr a       ; button in 4017d3
	lsr a       ; nes vaus in 4017d4
	rol resN

	dex
	bnz vausreadf8

	lda #$FF
	sta resF+1
	sta resN+1
	
	lda $4017
	lsr a
	lsr a
	rol resF
	rol resF+1
	lsr a
	lsr a
	lsr a
	rol resN
	rol resN+1

	lda resF
	eor #$FF
	sta resF
	lda resF+1
	eor #$FF
	sta resF+1
	lda resN
	eor #$FF
	sta resN
	lda resN+1
	eor #$FF
  	sta resN+1

	ldx #1      ; start next conversion
	stx $4016
	dex
	stx $4016

	lda #$21
	sta PPU_ADDR
	lda #$0c
	sta PPU_ADDR
	
	lda resF+1
	and #1
	tax
	lda hex,x
	sta PPU_DATA

	lda resF
	lsr
	lsr
	lsr
	lsr
	tax
	lda hex,x
	sta PPU_DATA

	lda resF
	and #15
	tax
	lda hex,x
	sta PPU_DATA

	lda #' '
	sta PPU_DATA
	sta PPU_DATA

	lda resN+1
	and #1
	tax
	lda hex,x
	sta PPU_DATA

	lda resN
	lsr
	lsr
	lsr
	lsr
	tax
	lda hex,x
	sta PPU_DATA

	lda resN
	and #15
	tax
	lda hex,x
	sta PPU_DATA

	lda #0
	sta PPU_SCROLL
	sta PPU_ADDR
	sta PPU_ADDR
	sta PPU_SCROLL

	clc
	lda #<cos512
	adc resF
	sta indir
	lda #>cos512
	adc resF+1
	sta indir+1

	ldy #0
	lda (indir),y
	sta $203

	clc
	lda #<sin512
	adc resF
	sta indir
	lda #>sin512
	adc resF+1
	sta indir+1

	ldy #0
	lda (indir),y
	sta $200

	clc
	lda #<cos512
	adc resN
	sta indir
	lda #>cos512
	adc resN+1
	sta indir+1

	ldy #0
	lda (indir),y
	sta $207

	clc
	lda #<sin512
	adc resN
	sta indir
	lda #>sin512
	adc resN+1
	sta indir+1

	ldy #0
	lda (indir),y
	sta $204

	lda #'O'
	sta $201
	lda #'X'
	sta $205

	lda #0
	sta $202
	lda #1
	sta $206

	lda #2
	sta PPU_OAM_DMA
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

cos512:	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,6,6,6,6,6,7,7,7,8,8,8,8,9,9,9,9,10,10,10,11,11,11,12,12,12,13,13,13,14,14,14,15,15,15,16,16,16,17,17,18,18,18,19,19,20,20,20,21,21,22,22,23,23,24,24,24,25,25,26,26,27,27,28,28,29,29,30,30,31,31,32,32,33,33,34,34,35,35,36,36,37,37,38,38,39,40,40,41,41,42,42,43,44,44,45,45,46,47,47,48,48,49,50,50,51,51,52,53,53,54,54,55,56,56,57,58,58,59,60,60,61,62,62,63,64,64,65,66,66,67,68,68,69,70,70,71,72,72,73,74,74,75,76,77,77,78,79,79,80,81,82,82,83,84,84,85,86,87,87,88,89,89,90,91,92,92,93,94,95,95,96,97,98,98,99,100,101,101,102,103,104,104,105,106,107,107,108,109,110,110,111,112,113,113,114,115,116,116,117,118,119,119,120,121,122,122,123,124,125,126,126,127,128,129,129,130,131,132,132,133,134,135,135,136,137,138,138,139,140,141,141,142,143,144,144,145,146,147,147,148,149,150,150,151,152,153,153,154,155,156,156,157,158,159,159,160,161,161,162,163,164,164,165,166,166,167,168,169,169,170,171,171,172,173,174,174,175,176,176,177,178,178,179,180,180,181,182,182,183,184,184,185,186,186,187,188,188,189,190,190,191,192,192,193,194,194,195,195,196,197,197,198,198,199,200,200,201,201,202,203,203,204,204,205,206,206,207,207,208,208,209,210,210,211,211,212,212,213,213,214,214,215,215,216,216,217,217,218,218,219,219,220,220,221,221,222,222,223,223,224,224,224,225,225,226,226,227,227,228,228,228,229,229,230,230,230,231,231,232,232,232,233,233,233,234,234,234,235,235,235,236,236,236,237,237,237,238,238,238,239,239,239,239,240,240,240,240,241,241,241,242,242,242,242,242,243,243,243,243,244,244,244,244,244,244,245,245,245,245,245,245,246,246,246,246,246,246,246,247,247,247,247,247,247,247,247,247,247,247,248,248,248,248,248,248,248,248,248,248,248,248,248,248
sin512:	.byte 210,209,208,207,206,206,205,204,203,203,202,201,200,200,199,198,197,197,196,195,194,194,193,192,191,191,190,189,188,188,187,186,185,185,184,183,182,182,181,180,179,179,178,177,176,176,175,174,174,173,172,171,171,170,169,168,168,167,166,166,165,164,163,163,162,161,161,160,159,159,158,157,156,156,155,154,154,153,152,152,151,150,150,149,148,148,147,146,146,145,144,144,143,143,142,141,141,140,139,139,138,137,137,136,136,135,134,134,133,133,132,131,131,130,130,129,129,128,127,127,126,126,125,125,124,123,123,122,122,121,121,120,120,119,119,118,118,117,117,116,116,115,115,114,114,113,113,112,112,111,111,110,110,109,109,109,108,108,107,107,106,106,106,105,105,104,104,104,103,103,102,102,102,101,101,101,100,100,99,99,99,98,98,98,97,97,97,96,96,96,96,95,95,95,94,94,94,94,93,93,93,92,92,92,92,92,91,91,91,91,90,90,90,90,90,89,89,89,89,89,89,88,88,88,88,88,88,87,87,87,87,87,87,87,87,87,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,87,87,87,87,87,87,87,87,87,88,88,88,88,88,88,89,89,89,89,89,89,90,90,90,90,90,91,91,91,91,92,92,92,92,92,93,93,93,94,94,94,94,95,95,95,96,96,96,96,97,97,97,98,98,98,99,99,99,100,100,101,101,101,102,102,102,103,103,104,104,104,105,105,106,106,106,107,107,108,108,109,109,109,110,110,111,111,112,112,113,113,114,114,115,115,116,116,117,117,118,118,119,119,120,120,121,121,122,122,123,123,124,125,125,126,126,127,127,128,129,129,130,130,131,131,132,133,133,134,134,135,136,136,137,137,138,139,139,140,141,141,142,143,143,144,144,145,146,146,147,148,148,149,150,150,151,152,152,153,154,154,155,156,156,157,158,159,159,160,161,161,162,163,163,164,165,166,166,167,168,168,169,170,171,171,172,173,174,174,175,176,176,177,178,179,179,180,181,182,182,183,184,185,185,186,187,188,188,189,190,191,191,192,193,194,194,195,196,197,197,198,199,200,200,201,202,203,203,204,205,206,206,207,208,209

chr:
	.incbin "alnum.1bp"

hex:	.byte "0123456789ABCDEF"

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
	.res ($fffa-*), $FF
	.word nmi, reset, reset
	
