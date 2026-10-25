


.segment "ZEROPAGE"
nmi_count: .res 1
current_bank: .res 1
timer: .res 1
ptr: .res 2



.segment "STACK"
stack:
.res 256

.segment "OAM"
oam:
.res 256

.segment "RAM"

.segment "DATA"

bank_tag:
.byte BANK_NUMBER

describe:
;        01234567890123456789012345678901
.byte "   BxROM 512k ROM test:         "
.byte "   You should see a repeating   "
.byte "   cycle counting 0-F below.    "
.byte 0

chr:
.incbin "test.chr"
.incbin "test.chr"

.segment "CODE"

irq:
	rti

nmi:
	pha
	txa
	pha
	tya
	pha
	; OAM DMA
	lda #00
	sta $2003
	lda #>oam
	sta $4014
	; nmi counter
	inc nmi_count
	; end nmi
	pla
	tay
	pla
	tax
	pla
	rti

reset:
    ; standard startup
    sei       ; set interrupt flag (unnecessary, unless reset is called from code)
    cld       ; disable decimal mode
    ldx #$40
    stx $4017 ; disable APU IRQ
    ldx $ff
    txs       ; set up stack
    ldx #$00
    stx $2000 ; disable NMI
    stx $2001 ; disable render
    stx $4010 ; disable DPCM IRQ
    bit $2002 ; clear vblank flag
    ; wait for vblank
:
    bit $2002
    bpl :-
    ; clear memory
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
    ; wait for second vblank
:
    bit $2002
    bpl :-
    ; PPU is now warmed up, NES is ready to go!
	jsr setup_screen
main_loop:
	inc timer
	lda timer
	cmp #30
	bcc :+
		lda #0
		sta timer
		inc current_bank
		lda current_bank
		and #15
		sta current_bank
	:
	lda current_bank
	jsr bankswitch
	lda bank_tag
	clc
	adc #$A0
	sta oam+1
	lda nmi_count
:
	cmp nmi_count
	beq :-
	jmp main_loop

setup_screen:
	; setup CHR
	lda $2002
	lda #0
	sta $2006
	sta $2006
	lda #<chr
	sta ptr+0
	lda #>chr
	sta ptr+1
	ldx #(4*8) ; 8k
	ldy #0
	:
		lda (ptr), Y
		sta $2007
		iny
		bne :-
		inc ptr+1
		dex
		bne :-
	; setup screen
	lda #0
	ldx #(4*2) ; 2k
	ldy #0
	:
		sta $2007
		iny
		bne :-
		dex
		bne :-
	; description
	lda$2002
	lda #$21 ; start at line 8
	sta $2006
	lda #$00
	sta $2006
	ldx #0
	:
		lda describe, X
		beq :+
		sta $2007
		inx
		jmp :-
	:
	; setup palette
	lda $2002
	lda #$3F
	sta $2006
	lda #$00
	sta $2006
	ldx #8
	:
		lda #$0F
		sta $2007
		lda #$12
		sta $2007
		lda #$21
		sta $2007
		lda #$30
		sta $2007
		dex
		bne :-
	; setup OAM
	lda #$FF
	ldx #0
	:
		sta oam, X
		dex
		bne :-
	; setup sprite 0
	lda #100
	sta oam+0 ; Y
	lda #0
	sta oam+1 ; tile
	lda #0
	sta oam+2 ; attribute
	lda #100
	sta oam+3 ; X
	; setup scroll
	lda #0
	sta $2005
	sta $2005
	; begin rendering
	lda #%00011110
	sta $2001
	; turn on NMI
	lda #%10000000
	sta $2000
	; done
	rts

.segment "FIXED"

bus_conflict: .byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
bankswitch:
	tay
	sta bus_conflict, Y
	rts

.segment "VECTORS"
.word nmi
.word reset
.word irq
