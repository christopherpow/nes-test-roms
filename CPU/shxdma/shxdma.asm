;;; cc0/public domain (lidnariq 2024)
;;; assemble with cl65 -t none -o shxshy1.nes shxshy1.asm
	.setcpu "6502x"
	

;;; shxdma
;;; test ROM for the shx/shy instructions

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


;;; this test rom "just" validates the bit where toggling RDY (i.e. DPCMDMA) can fix the written value.

	.zeropage
indir:	.word 0
outside:	.byt 0
countNormal:	.word 0,0
countRdy:	.word 0,0
	
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

	lda #0
	sta countNormal
	sta countNormal+1
	sta countNormal+2
	sta countNormal+3
	sta countRdy
	sta countRdy+1
	sta countRdy+2
	sta countRdy+3
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

	;; draw header
	lda #$21
	sta PPU_ADDR
	lda #$E8
	sta PPU_ADDR

	lda #'N'
	sta PPU_DATA
	lda #'O'
	sta PPU_DATA
	lda #'R'
	sta PPU_DATA
	lda #'M'
	sta PPU_DATA
	lda #'A'
	sta PPU_DATA
	lda #'L'
	sta PPU_DATA

	lda #$21
	sta PPU_ADDR
	lda #$F2
	sta PPU_ADDR

	lda #'I'
	sta PPU_DATA
	lda #'N'
	sta PPU_DATA
	lda #'T'
	sta PPU_DATA
	lda #'E'
	sta PPU_DATA
	lda #'R'
	sta PPU_DATA
	lda #'R'
	sta PPU_DATA
	lda #'U'
	sta PPU_DATA
	lda #'P'
	sta PPU_DATA
	lda #'T'
	sta PPU_DATA
	lda #'E'
	sta PPU_DATA
	lda #'D'
	sta PPU_DATA
	
	;; initialize DPCM
	lda #APUDMC::LOOP|15 ; must set loop and not IRQ
	sta APU_PERIOD_DMC
	lda #(dpcmloop-$C000)>>6
	sta APU_DMC_OFFSET
	lda #255
	sta APU_DURATION_DMC ; 255x16+1 = 4081 bytes
	;; start DPCM
	lda #$10
	sta APU_STATUS

	;; start rendering and nmis
	lda #PPUMASK::LEFT_BKGD_SHOW | PPUMASK::BKGD_ENA
	sta PPU_MASK

	bit PPU_STATUS
	;; enable NMI
	lda #PPUCTRL::NMI_ENA | PPUCTRL::BKGD_RIGHT
	sta PPU_CTRL

testloop:
	lda #$80
	sta 0
	sta $100
	ldx #$FF
	ldy #0
	shx 0,y	; should write 1 (no RDY) or $FF (yes RDY)
	;; incorrect implementations will write to $0100

	lda 0
	eor #1
	bze normal
	eor #$fe
	bze rdy
	jmp error

normal:
	;; in this case, RDY did not interrupt SHX,
	;; and 0 was written to RAM

	ldx countNormal
	inx
	stx countNormal
	cpx #$10
	bne testloop
	lda #0
	sta countNormal

	ldx countNormal+1
	inx
	stx countNormal+1
	cpx #$10
	bne testloop
	lda #0
	sta countNormal+1

	ldx countNormal+2
	inx
	stx countNormal+2
	cpx #$10
	bne testloop
	lda #0
	sta countNormal+2

	ldx countNormal+3
	inx
	stx countNormal+3
	cpx #$10
	bne testloop
	lda #0
	sta countNormal+3
	jmp done

rdy:	
	;; in this case, RDY DID interrupt SHX,
	;; and $FE was written to RAM

	ldx countRdy
	inx
	stx countRdy
	cpx #$10
	bne testloop
	lda #0
	sta countRdy

	ldx countRdy+1
	inx
	stx countRdy+1
	cpx #$10
	bne testloop
	lda #0
	sta countRdy+1

	ldx countRdy+2
	inx
	stx countRdy+2
	cpx #$10
	bne testloop
	lda #0
	sta countRdy+2

	ldx countRdy+3
	inx
	stx countRdy+3
	cpx #$10
	bne testloopF
	lda #0
	sta countRdy+3
testloopF:	
	jmp testloop

error:
	lda #0
	sta APU_STATUS
	lda #0
	sta PPU_MASK
	lda #0
	sta PPU_CTRL
	
	;; 123456789a123456789b12345678
	;; Y=00 X=FE
	lda #$21
	sta PPU_ADDR
	lda #$0b
	sta PPU_ADDR

	lda #'Y'
	sta PPU_DATA
	lda #'='
	sta PPU_DATA
	lda #'0'
	sta PPU_DATA
	lda #'0'
	sta PPU_DATA
	lda #' '
	sta PPU_DATA
	lda #'X'
	sta PPU_DATA
	lda #'='
	sta PPU_DATA
	lda #'F'
	sta PPU_DATA
	lda #'F'
	sta PPU_DATA

	;; SHX 0000,Y
	lda #$21
	sta PPU_ADDR
	lda #$2b
	sta PPU_ADDR

	lda #'S'
	sta PPU_DATA
	lda #'H'
	sta PPU_DATA
	lda #'X'
	sta PPU_DATA
	lda #' '
	sta PPU_DATA
	lda #'0'
	sta PPU_DATA
	lda #'0'
	sta PPU_DATA
	lda #'0'
	sta PPU_DATA
	lda #'0'
	sta PPU_DATA
	lda #','
	sta PPU_DATA
	lda #'Y'
	sta PPU_DATA
	
	;; READ XX???
	lda #$21
	sta PPU_ADDR
	lda #$4b
	sta PPU_ADDR

	lda #'R'
	sta PPU_DATA
	lda #'E'
	sta PPU_DATA
	lda #'A'
	sta PPU_DATA
	lda #'D'
	sta PPU_DATA
	lda #' '
	sta PPU_DATA
	lda 0
	lsr
	lsr
	lsr
	lsr
	tax
	lda hex,x
	sta PPU_DATA
	lda 0
	and #15
	tax
	lda hex,x
	sta PPU_DATA
	lda #'?'
	sta PPU_DATA
	lda #'?'
	sta PPU_DATA
	lda #'?'
	sta PPU_DATA

	lda #$21
	sta PPU_ADDR
	lda #$6b
	sta PPU_ADDR

	;; common error: always (H+1)&X
	;; [0100]=XX
	lda #'['
	sta PPU_DATA
	lda #'0'
	sta PPU_DATA
	lda #'1'
	sta PPU_DATA
	lda #'0'
	sta PPU_DATA
	lda #'0'
	sta PPU_DATA
	lda #']'
	sta PPU_DATA
	lda #'='
	sta PPU_DATA
	lda $100
	lsr
	lsr
	lsr
	lsr
	tax
	lda hex,x
	sta PPU_DATA
	lda $100
	and #15
	tax
	lda hex,x
	sta PPU_DATA
	lda #'?'
	sta PPU_DATA

	lda #$22
	sta PPU_ADDR
	lda #$0a
	sta PPU_ADDR

	lda #'E'
	sta PPU_DATA
	lda #'X'
	sta PPU_DATA
	lda #'T'
	sta PPU_DATA
	lda #'R'
	sta PPU_DATA
	lda #'A'
	sta PPU_DATA
	ldx #' '
	stx PPU_DATA
	stx PPU_DATA
	lda #'F'
	sta PPU_DATA
	lda #'A'
	sta PPU_DATA
	lda #'I'
	sta PPU_DATA
	lda #'L'
	sta PPU_DATA
	stx PPU_DATA
	
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

done:
	lda #0
	sta APU_STATUS ; silence
	lda #0
	sta PPU_MASK

	clc
	lda countRdy
	adc countRdy+1
	adc countRdy+2
	adc countRdy+3
	bze fail
pass:

	lda #$22
	sta PPU_ADDR
	lda #$31
	sta PPU_ADDR

	lda #'?'
	sta PPU_DATA
	lda #'P'
	sta PPU_DATA
	lda #'A'
	sta PPU_DATA
	lda #'S'
	sta PPU_DATA
	lda #'S'
	sta PPU_DATA
	lda #'?'
	sta PPU_DATA
	jmp hang

fail:
	lda #$22
	sta PPU_ADDR
	lda #$32
	sta PPU_ADDR

	lda #'F'
	sta PPU_DATA
	lda #'A'
	sta PPU_DATA
	lda #'I'
	sta PPU_DATA
	lda #'L'
	sta PPU_DATA
	jmp hang
	
	
nmi:
	pha
	txa
	pha
	lda #$22
	sta PPU_ADDR
	lda #$0a
	sta PPU_ADDR

	ldx countNormal+3
	lda hex,x
	sta PPU_DATA
	ldx countNormal+2
	lda hex,x
	sta PPU_DATA
	ldx countNormal+1
	lda hex,x
	sta PPU_DATA
	ldx countNormal+0
	lda hex,x
	sta PPU_DATA

	lda #$22
	sta PPU_ADDR
	lda #$12
	sta PPU_ADDR

	ldx countRdy+3
	lda hex,x
	sta PPU_DATA
	ldx countRdy+2
	lda hex,x
	sta PPU_DATA
	ldx countRdy+1
	lda hex,x
	sta PPU_DATA
	ldx countRdy+0
	lda hex,x
	sta PPU_DATA

	lda #0
	sta PPU_ADDR
	sta PPU_ADDR
	sta PPU_SCROLL
	sta PPU_SCROLL
	
	pla
	tax
	pla
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
	.byt $20,0,0,$0f
	.byt $20,0,0,$0f
	.byt $20,0,0,$0f
	.byt $20,0,0,$0f

	.res ($f000-*), $FF
dpcmloop:
	.incbin "thud.dmc", 0,4081

vectors:
	.res ($fffa-*), $FF
	.word nmi, reset, reset
