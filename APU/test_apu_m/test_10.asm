; TEST 01
; 2012 (c) JrezCorp Team
;
; ASM6502
;
	.list
	.inesprg 1 ; 1 PRG
	.ineschr 1 ; 1 CHR
	.inesmir 0 ; V-mirroring
	.inesmap 0 ; 0-map (NMAP)

; MEMORY MAP
;
; 0x0000 - 0x04FF - arrays (0x0000-0x00FF - zero page, 0x0100-0x01FF - stack, 0x0200-0x02FF - sprite memmory)
; 0x0500 - 0x07FF - variables
; 0x0800 - 0x1FFF - mirrors of 0x0000 - 0x07FF
; 0x2000 - 0x2007 - PPU registers
; 0x2008 - 0x3FFF - mirrors of 0x2000 - 0x2007
; 0x4000 - 0x401F - 2A03 (APU) registers
; 0x4020 - 0x5FFF - mapper registers (not used)
; 0x6000 - 0x7FFF - SRAM
; 0x8000 - 0xFFFF - PRG ROM

; VARIABLES AND ARRAYS

stack = $0100 ; (size = 256)
sprite_mem = $0200 ; (size = 256)
nmi_hit = $0500 ; (size = 1)

	.bank 0
	.org $8000

; PRG ROM

main:
; PPU is turned off
; setup palette
	lda #$3f
	sta $2006
	lda #0
	sta $2006
	lda #13
	sta $2007
	lda #32
	sta $2007
	lda #14
	sta $2007
	lda #14
	sta $2007
; clear attributes
	lda #$23
	sta $2006
	lda #$c0
	sta $2006
	ldx #64
	lda #0
main_loop0:
	sta $2007
	dex
	bne main_loop0
	lda #$21
	sta $2006
	lda #$2a
	sta $2006

; TEST
	jmp test

	.org $8100
test:
	lda #$40
	sta $4017
	lda #0
	sta $4017
	ldx #$39
	ldy #$18
loop:
	dex
	bne loop
	dey
	bne loop
	beq wait_0
wait_0:
	lda $4015
	bne wait_1
wait_1:
	lda #$40
	sta $4017
	lda #0
	sta $4002
	sta $4003
	lda #1
	sta $4015
	lda #0
	sta $4003
	lda #$80
	ldx #7
clear_lc_loop:
	sta $4017
	dex
	bne clear_lc_loop
	sta $4017
	stx $4017
	ldx #$38
	ldy #$18
clear_loop:
	dex
	bne clear_loop
	dey
	bne clear_loop
	beq wait_2
wait_2:
	nop
	nop
; Changing stx to sta here SHOULD NOT matter
	stx $4017
	nop
	ldx #$ff
	lda $4015
	and #1
	bne fail_loop
pass_loop:
	inx
	lda test_success, x
	sta $2007
	bne pass_loop
	beq test_end
fail_loop:
	inx
	lda test_error, x
	sta $2007
	bne fail_loop

test_end:
	jsr vwait
	lda #%00001010
	sta $2001
	clv
eloop:
	bvc eloop

; clear scroll
clear_scroll:
	lda #0
	sta $2006
	sta $2006
	rts

; wait for vblank starts
vwait_start:
	bit $2002
vwait_start_loop0:
	bit $2002
	bpl vwait_start_loop0
	rts

; wait for vblank ends and clear scroll
vwait_end:
	jsr vwait_end_wc
	jmp clear_scroll

; wait for vblank ends
vwait_end_wc:
	lda $2002
	bmi vwait_end_wc
	rts

; wait for a vblank
vwait:
	jsr vwait_wc
	jmp clear_scroll

; wait for a vblank (scroll not cleared)
vwait_wc:
	jsr vwait_start
	jmp vwait_end_wc

; entry point
start:
; turn off PPU
	lda #0
	sta $2000
	sta $2001
; disable interrupts
	sei
; reset stack pointer
	ldx $ff
	txs
; wait for two vblanks
	jsr vwait_wc
	jsr vwait_wc
; goto main
	jmp main

; non-maskable interrupt
nmi:
; save state
	php
	pha
	lda #1
	sta nmi_hit
	pla
	plp
	rti

; ignore any irq
irq:
	rti

; DATA

test_error:
	.db "TEST FAILED",0
test_success:
	.db "TEST PASSED",0

; POINTERS

	.bank 1
	.org $FFFA
	.dw nmi, start, irq

; CHR ROM

	.bank 2
	.org $0000
	.incbin "rom.chr"
