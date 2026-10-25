; ----------------------------------- NES 2.0 Header -----------------------------------

.byte $4E, $45, $53, $1A
.byte 1   ; number of 16K PRG-ROM banks
.byte 0   ; number of  8K CHR-ROM banks
.byte $00 ; flags 6 (NROM, horizontal mirroring)
.byte $08 ; flags 7 (NROM, NES 2.0 enabled)
.byte $00 ; NES 2.0 extended mapper information
.byte $00 ; NES 2.0 extended ROM size
.byte $00 ; NES 2.0 PRG-RAM size
.byte $07 ; NES 2.0 CHR-RAM size (8192, no battery)
.byte $00 ; NES 2.0 TV system
.byte $00 ; NES 2.0 Vs. System hardware
.byte 0,0 ; reserved

; ----------------------------------------- RAM ----------------------------------------

.zero

.org 0

controller1 .byte 0
controller2 .byte 0

OAM = $02 ; put whatever your OAM page is here

.text

; ----------------------------------------- PRG ----------------------------------------

.org $C000

.byte 0 ; DMC sample

reset:
	lda #0
	sta $2000 ; disable NMI
	sta $2001 ; disable rendering
	sta $4015 ; silence APU
	bit $2002
	lda #$0F
	jsr set_bg
	jsr set_bg

	lda #$4F  ; no IRQ, loop, rate F
	sta $4010
	lda #0
	sta $4012 ; address $C000
	sta $4013 ; length 1 byte
	lda #$10
	sta $4015 ; start sample

forever:
	lda #OAM
	sta $4014          ; ------ DMA ------
	ldx #1             ; even odd          <- strobe code must take an odd number of cycles total
	stx controller1    ; even odd even
	stx $4016          ; odd even odd even
	dex                ; odd even
	stx $4016          ; odd even odd even
 read_loop:
	lda $4017          ; odd even odd EVEN <- loop code must take an even number of cycles total
	and #3             ; odd even
	cmp #1             ; odd even
	rol controller2, x ; odd even odd even odd even (X = 0; waste 1 cycle and 0 bytes for alignment)
	lda $4016          ; odd even odd EVEN
	and #3             ; odd even
	cmp #1             ; odd even
	rol controller1    ; odd even odd even odd
	bcc read_loop      ; even odd [even]

	lda #1
	bit controller1
	bne glitch
	bit controller2
	beq forever
glitch:
	lda #$30
	jsr set_bg
	jmp forever


; wait for vblank and set BG colour to the value in A
;  clobbers X

set_bg:
	bit $2002
	bpl set_bg
	ldx #$3F
	stx $2006
	ldx #$00
	stx $2006
	sta $2007
	stx $2006
	stx $2006
	rts


.dsb $FFFA - *       ; pad to interrupt vectors

.word reset
.word reset
.word reset
