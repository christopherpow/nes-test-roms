;;; cc0/public domain (lidnariq 2024)
;;; assemble with cl65 -t none -o shxing1.nes shxing1.asm
	.setcpu "6502x"
	

;;; shxing1
;;; test ROM for the shx instruction

;;; tables reproduced from No More Secrets:

;;;            buggy sax    extrabuggy  buggy sty   buggy stx   buggy sax
;;; mnemonic | SHA (zp),y | TAS abs,y | SHY abs,x | SHX abs,y | SHA abs,y
;;; opcode   |         93 |        9b |        9c |        9e |        9f
;;; value    |        A&X |       A&X |         Y |         X |       A&X
;;; cycle N  |          5 |         4 |         4 |         4 |         4

;;; Highbyte of address written to:
;;; Page not crossed: {H}
;;; Page crossed: {H+1} & value

;;; Value written:
;;; no RDY during cycle N: {H+1} & value
;;; RDY durign cycle N: value


;;; this test rom "just" validates the "when is address corrupted"

;;; shx and shy have four bytes of inputs: Y, X, H, and L
;;; the variously buggy SAX instructions have five (A,X,Y,H,L)

	.zeropage
indir:	.word 0
	
lo = $200
yi = $201
outside = $202
stub = $203
	
	.code
	.org $bff0
	.include "constants.ca65.inc"
	.macpack generic

	.byt "NES",$1A,1,0,$70,0,0,0,0,0,0,0,0,0

	.res 8192
	
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

	sty lo
	sty yi

	lda #$9e	; shx
	sta stub
	lda #3	; highbyte of address=3 so that addresses are either 3 or 0
	sta stub+2
	lda #$60	; rts
	sta stub+3

testloop:
	;; predict address of write-

	;; if lo+yi < 256, then address is correct
	lda lo
	clc
	adc yi
	bcc normal
pagewrap:
	tax
	lda #$ff
	sta 0,x	; with this setup it'll wrap to zero-page

	ldx #$FB	; X & 4 must be zero for this test
	lda lo
	sta stub+1
	ldy yi
	jsr stub

	lda lo
	clc
	adc yi
	tax
	lda 0,x
	bnz pagewrapfail
	jmp nexttest

normal:
	tax
	lda #$ff
	sta $300,x

	ldx #$FB	; the point being that X & (H+1) is zero
	lda lo
	sta stub+1
	ldy yi
	jsr stub

	lda lo
	clc
	adc yi
	tax
	lda $300,x
	bnz normalfail

nexttest:
	inc yi
	bnz testloop
	inc lo
	bnz testloop
	jmp printsuccess

pagewrapfail:
normalfail:	
	lda #$21
	sta PPU_ADDR
	lda #$06
	sta PPU_ADDR

	lda #'9'
	sta PPU_DATA
	lda #'E'
	sta PPU_DATA
	lda #' '
	sta PPU_DATA

	lda lo
	lsr
	lsr
	lsr
	lsr
	tax
	lda hex,x
	sta PPU_DATA
	lda lo
	and #$f
	tax
	lda hex,x
	sta PPU_DATA
	lda #' '
	sta PPU_DATA

	lda #'0'
	sta PPU_DATA
	lda #'3'
	sta PPU_DATA
	lda #' '
	sta PPU_DATA
	lda #' '
	sta PPU_DATA
	lda #'Y'
	sta PPU_DATA
	lda #'='
	sta PPU_DATA

	lda yi
	lsr
	lsr
	lsr
	lsr
	tax
	lda hex,x
	sta PPU_DATA
	lda yi
	and #$f
	tax
	lda hex,x
	sta PPU_DATA

	jmp hang

printsuccess:
	lda #$21
	sta PPU_ADDR
	lda #$0f
	sta PPU_ADDR

	lda #'O'
	sta PPU_DATA
	lda #'K'
	sta PPU_DATA
	
hang:	
	lda #0
	sta PPU_ADDR
	sta PPU_ADDR
	sta PPU_SCROLL
	sta PPU_SCROLL
wvblank3:	
	bit PPU_STATUS
	bpl wvblank3
	lda #PPUMASK::LEFT_BKGD_SHOW | PPUMASK::BKGD_ENA
	sta PPU_MASK

inf:	jmp inf

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
	.byt $20,0,0,$0f
	.byt $20,0,0,$0f
	.byt $20,0,0,$0f
	.byt $20,0,0,$0f

vectors:
	.res ($fffa-*), $FF
	.word reset, reset, reset
