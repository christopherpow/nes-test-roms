;;; public domain (lidnariq 2022)
;;; assemble with cl65 -t none -o vausadc.nes vausadc.asm

	.ZEROPAGE
outside:	.byt 0
indir:	.word 0
edges:	.byt 0
cur:	.byt 0
prev:	.byt 0
result:	.byt 0

	.setcpu "6502x"

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
	lda #' '
clearnametable:
	sta PPU_DATA
	sta PPU_DATA
	sta PPU_DATA
	sta PPU_DATA
	dey
	bnz clearnametable

	lda #PPUMASK::BKGD_ENA
	sta PPU_MASK

	;; enable NMI
	lda #PPUCTRL::NMI_ENA
	sta PPU_CTRL

	;; and wait forever
spin:
	jmp spin

nmi:
	lda #$21
	sta PPU_ADDR
	lda #$0F
	sta PPU_ADDR

	lda edges
	lsr
	lsr
	lsr
	lsr
	tax
	lda hex,x
	sta PPU_DATA

	lda edges
	and #15
	tax
	lda hex,x
	sta PPU_DATA

	lda #$21
	sta PPU_ADDR
	lda #$2F
	sta PPU_ADDR

	lda result
	eor #$ff
	lsr
	lsr
	lsr
	lsr
	tax
	lda hex,x
	sta PPU_DATA

	lda result
	eor #$ff
	and #15
	tax
	lda hex,x
	sta PPU_DATA
	
	lda #0
	sta PPU_SCROLL
	sta PPU_ADDR
	sta PPU_ADDR
	sta PPU_SCROLL

	sta edges

	ldx #6	; 6*1284-1 = 7703cy = 67.8 scanlines 2A03, 72.3 scanlines 2A07
	ldy #0
delay1:	
	dey	; 5cy/loop
	bnz delay1
	dex	; 1284cy/loop
	bnz delay1
	
	lda #PPUMASK::BKGD_ENA | PPUMASK::GREY
	sta PPU_MASK
	lda #PPUMASK::BKGD_ENA
	sta PPU_MASK
	
	ldx #1
	stx $4016
	dex
	stx $4016

	ldy #0

	.repeat 8
	lda $4017   ; 4 -- 13cy, how 2 faster?
	lsr         ; 2
	lsr         ; 2
	rol result  ; 5
	.endrep

	lda #2
	bit $4017
	bze lowloop
	bnz highloop

to_highloop:
	iny
	inx	; 2
highloop:	
	bit $4017	; 4 -- 11cy switch not taken
	bze to_lowloop	; 2 / 3
	iny	; 2
	bnz highloop	; 3
	;; termination
	jmp done

to_lowloop:		; -> 3 to here
	iny	; 2 -- 11cy when switch taken
	inx	; 2 
lowloop:
	bit $4017	; 4
	bnz to_highloop	; 2 / 3
	iny	; 2
	bnz lowloop	; 3
	;; termination
	jmp done

done:	
	lda #PPUMASK::BKGD_ENA | PPUMASK::GREY
	sta PPU_MASK
	lda #PPUMASK::BKGD_ENA
	sta PPU_MASK

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
	.incbin "alnum.1bp"

hex:	.byte "0123456789ABCDEF"

palette:
	.byt $21,0,0,$0f
	.byt $24,0,0,$0f
	.byt $27,0,0,$0f
	.byt $2A,0,0,$0f
	.byt $3d,0,0,$0f
	.byt $3d,0,0,$0f
	.byt $3d,0,0,$0f
	.byt $3d,0,0,$0f

vectors:
	.res ($fffa-*), $FF
	.word nmi, reset, reset
	
