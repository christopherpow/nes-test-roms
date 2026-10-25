.byte $4E, $45, $53, $1A
.byte 1
.byte 0
.byte $00
.byte $08
.byte $00
.byte $00
.byte $00
.byte $00
.byte $02
.byte $00
.byte 0,0

* = $C000

reset:
	lda #0
	sta $2000
	sta $2001
	sta $4015
	sta $4001
	sta $4005

	ldx #$03
	stx $4015
	ldx #$BF
	stx $4000
	stx $4004

	sta $4002
	sta $4003
	sta $4006
	sta $4007
	jsr wait_4096

	ldx #$81
	stx $4002
	jsr wait_256
	stx $4006

	sta $4003
	jsr wait_1024
	jsr wait_256
	sta $4007

forever:
	jmp forever

wait_4096:
	jsr wait_2048
wait_2048:
	jsr wait_1024
wait_1024:
	jsr wait_512
wait_512:
	jsr wait_256
wait_256:
	jsr wait_128
wait_128:
	jsr wait_64
wait_64:
	jsr wait_32
wait_32:
	jsr wait_16
wait_16:
	nop
	nop
	rts

.dsb $FFFA - *
.word reset
.word reset
.word reset
