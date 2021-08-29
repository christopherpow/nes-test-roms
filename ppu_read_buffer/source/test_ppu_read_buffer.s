; This test tests the following diverse set of features:
;
; - CIRAM access (read and write) through $2007 (i.e. internal nametables)
; - For $2007 access, the 1-byte and 32-byte increment flag
; - Rudimentary PPU bus functions (tolerance: only $2003(W) into $2000(R) is tested)
; - CPU RAM mirroring ($0000-$07FF should be mirrored within $0000-$1FFF)
; - PPU I/O mirroring ($2000-$2007 should be mirrored within $2000-$3FFF)
; - Code execution from different pages of CPU RAM (including stack and zeropage)
; - Rudimentary sprite 0-hit flag function (tolerance: any shape, ~30 scanlines)
; - OAM (sprite memory) updates from CPU RAM (depends on RAM mirrors)
; - OAM (sprite memory) updates from ROM
; - OAM (sprite memory) updates from I/O space (depends on PPU open bus)
; - CHR-ROM reading through $2007
; - CNROM (iNES mapper 3) banked CHR-ROM access through $2007
;
; And much more. For details, see the attached readme.txt file.
;
; Expected output:
;
;  TEST:test_ppu_read_buffer      :)
;  -------------------------------
;  Testing basic PPU memory I/O.
;  Performing tests that combine
;  sprite 0 hit flag, $4014 DMA
;  and the RAM mirroring...
;  Graphical artifacts during
;  this test are OK and expected.
;  
;                Hit  No-Hit
;  Direct poke   OK   OK
;  DMA with ROM  OK   OK
;  DMA + PPU bus OK   OK
;  DMA with RAM  OK   OK
;  -------------------------------
;   This next test will take a while.
;   In order to distract you with
;   entertainment, art is provided.
;   Contemplate on the art while
;   the test is in progress.
;
;   
;  Passed




; Expected output, and explanation:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Written by Joel Yliluoma - http://iki.fi/bisqwit/

;CHR_RAM=0
;CART_WRAM=0
CUSTOM_MAPPER=3 ; CNROM
CUSTOM_HEADER=1
;CUSTOM_FOREVER=1
CUSTOM_NMI=1
CUSTOM_IRQ=1
.segment "HEADER"
.byte "NES",26
.byte 1,4 ; 16k prg, 3*8k chr
.byte CUSTOM_MAPPER*$10+$01 ; horizontal mirroring
.code

.segment "LIB"
.include "shell.inc"
.include "colors.inc"
.include "binarysearch.s"
.include "scanline_delay.s"

.segment "CODE"

zp_res	template_subst_ptr,2
zp_res	template_sourceptr,2
zp_res	template_targetptr,2
zp_res	template_bytecount

ppuio_addr    = template_sourceptr
ppuio_expect  = template_bytecount
ppuio_offset  = template_targetptr

zp_res	gfx_source_ppuptr,2
zp_res	gfx_bytecount
zp_res	spr0hit_fails_current

zp_res	joy_temp
zp_res	joy_cur
zp_res  scroll_y,2
zp_res  scroll_ydelta,2
zp_res	scroll_delays,8
zp_res  scroll_framecount,2

.segment "LIB"
SecretMessage: .byte "Secret Message. "
.include "bank_switch.s"
.include "testing2-begin.s"
.include "crash_proof.s"

.segment "DMA_TEST_MATERIAL"
SpriteDMA_test_pattern:
	.byte $0A,$0A,$8A,$8A

.segment "CODE"
main:
	jsr reset_tests
	
	; Ensure that a spurious NMI will not crash the ROM before
	; the NMI handler is actually loaded into RAM.
	setb nmi, $40    ; RTI
	set_vrom_page 0
	
	text_white
	print_ext_str "TEST:test_ppu_read_buffer",newline	;DO_DTE
	jsr print_spacer
	jsr save_console
	print_ext_str "Testing basic PPU memory I/O.",newline	;DO_DTE
	
	;jsr do_cool_graphics

.if 0
	text_color1
	print_str "COLOR1 TEXT",newline	;DO_DTE
	text_color2
	print_str "COLOR2 TEXT",newline	;DO_DTE
	text_white
	print_str "WHITE TEXT",newline	;DO_DTE
	
	jsr console_show
	jmp forever
.else

	jsr populate_ciram_slow
;jsr test_failing_barrier
	; leaves display hidden

	; First, verify that PPU memory I/O works as expected
	jsr test_one_byte_buffer
;jsr test_failing_barrier

	jsr test_nta_mirror
;jsr test_failing_barrier
	
	jsr test_sequential_memory_read
;jsr test_failing_barrier
	jsr test_sequential_memory_write
;jsr test_failing_barrier
	
	jsr test_ppu_open_bus
;jsr test_failing_barrier
	jsr test_ppu_read_buffer_open_bus
;jsr test_failing_barrier
	jsr test_ppu_read_buffer_open_bus_2
;jsr test_failing_barrier

	jsr test_chr_rom_read
;jsr test_failing_barrier
	jsr test_chr_rom_write
;jsr test_failing_barrier

	jsr test_one_byte_buffer_palette
;jsr test_failing_barrier
	jsr test_ppu_palette_wrap
;jsr test_failing_barrier
	jsr test_ppu_memory_14bit
;jsr test_failing_barrier
	jsr test_ppu_seq_read_wrap
;jsr test_failing_barrier

	jsr test_vaddr
;jsr test_failing_barrier
	;jsr test_failing_barrier
	
	jsr console_show
	set_ppuaddr $3F11
	lda #$03
	sta PPUDATA
	sta PPUDATA
	sta PPUDATA
	; ^ Make sprites visible

	jsr test_ram_mirroring
	jsr test_ppuio_mirroring
	
	;jsr test_failing_barrier

	jsr test_spr0hit_and_dma

	;jsr test_failing_barrier

	jsr do_cool_graphics
.endif

main_exit:
	jsr test_failing_barrier

	set_vrom_page 0
	text_white
	jsr console_show
	jsr wait_vbl_optional
	jmp tests_passed

print_spacer:
	ldx #29
:	lda #'-'	;DTE_CHARMAP
	jsr print_char
	dex
	bne :-
	jmp print_newline


normalize_wait_vbl:
	bit PPUSTATUS
	lda #0
	sta PPUADDR
	sta PPUADDR
	sta PPUSCROLL
	sta PPUSCROLL
	jsr wait_vbl_optional
	;lda #0
	sta PPUADDR
	sta PPUADDR
	rts

populate_ciram_slow:
	; Disable rendering
	jsr console_hide
	; Populate the entire $2D00..$2DFF region. (256 bytes)
	; $2D00..$2DFF:  addr+$80
	; Do it slowly,
	; in order to avoid bugs with those PPUs that do not
	; automatically update the taddr when $2007 is written to.
	ldx #$00
	ldy #$80
:	lda #$2D
	bit PPUSTATUS
	sta PPUADDR ; poke high 6 bits
	stx PPUADDR ; poke low  8 bits
	 sty PPUDATA
	iny
	inx
	bne :-
	rts

populate_ciram_quick:
	; Disable rendering
	jsr console_hide
	; Populate the entire $2C00..$2FFF region. (1024 bytes)
	; $2C00..$2CFF:  addr+$BB
	; $2D00..$2DFF:  addr+$A4
	; $2E00..$2EFF:  addr+$77
	; $2F00..$2FFF:  addr+$33
	; Do this fast, because it's used in test_ppuio_mirroring
	; which is run after sequential PPU I/O tests
	set_ppuaddr $2C00
	ldy #$BB
	jsr @fill256_quick
	ldy #$A4
	jsr @fill256_quick
	ldy #$77
	jsr @fill256_quick
	ldy #$33
	;jmp @fill256_quick
@fill256_quick:
	ldx #256 - 256/2
:	.repeat 2
	 sty PPUDATA
	 iny
	.endrepeat
	inx
	bne :-
	rts
	
	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_PPUMEMORYIO
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "PPU memory I/O does not work.",newline	;DO_DTE
	.byte "Possible areas of problem:",newline	;DO_DTE
	.byte "- PPU not implemented",newline	;DO_DTE
	.byte "- PPU memory writing ($2007)",newline	;DO_DTE
	.byte "- PPU memory reading ($2007)",newline	;DO_DTE
	.byte "- PPU memory area $2C00-$2FFF",0		;DO_DTE

	MakeTest TEST_ONEBYTEBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "Non-palette PPU memory reads",newline	;DO_DTE
	.byte "should have one-byte buffer.",0	;DO_DTE
.popseg
test_one_byte_buffer:
	begin_test TEST_PPUMEMORYIO

	set_ppuaddr $2D30
	ldy PPUDATA  ; $B0 should be placed in read-buffer
	set_ppuaddr $2D30
	; $B0 should be produced; $B0 should be placed in read-buffer
	fail_if_ppuread_y_not $B0

	begin_test TEST_ONEBYTEBUFFER
	set_ppuaddr $2D55
	; $B0 should be produced; $D5 should be placed in read-buffer
	ldy PPUDATA
	; Don't test the first read. It is a different topic, and will be
	; tested later in TEST_PPU_OPENBUS_FROM_WRITE2006_MUST_NOT_WRITETO_READBUFFER.

	; $D5 should be produced; $D6 should be placed in read-buffer
	fail_if_ppuread_y_not $D5
	; Don't do more reads; sequential reads are a different topic
	; and will be tested later in TEST_CIRAM_SEQ_READ_1.
	rts

	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_CIRAM_READ
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "CIRAM reading",$FF	;DO_DTE
	.word DOES_NOT_WORK

	MakeTest TEST_CIRAM_SEQ_READ_1
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Sequential CIRAM",$FF	;DO_DTE
	.word READING_1_NOTWORK

	MakeTest TEST_CIRAM_SEQ_READ_32
	.byte "Sequential CIRAM",$FF	;DO_DTE
	.word READING_32_NOTWORK

	MakeTest TEST_PALETTE_RAM_SEQ_READ_1
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Sequential PALETTE",$FF	;DO_DTE
	.word READING_1_NOTWORK

	MakeTest TEST_PALETTE_RAM_SEQ_READ_32
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Sequential PALETTE",$FF	;DO_DTE
	.word READING_32_NOTWORK

	MakeTest TEST_CHRROM_READ
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "CHR-ROM reading",$FF	;DO_DTE
	.word DOES_NOT_WORK

	MakeTest TEST_CHRROM_SEQ_READ_1
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Sequential CHR-ROM",$FF	;DO_DTE
	.word READING_1_NOTWORK

	MakeTest TEST_CHRROM_SEQ_READ_32
	.byte "Sequential CHR-ROM",$FF	;DO_DTE
	.word READING_32_NOTWORK

	READING_1_NOTWORK:
	.byte " reading",newline,"with  1",$FF	;DO_DTE
	.word BYTE_INCREMENT_DOES_NOT_WORK
	
	READING_32_NOTWORK:
	.byte " reading",newline,"with 32"	;DO_DTE
	BYTE_INCREMENT_DOES_NOT_WORK:
	.byte "-byte increment"	;DO_DTE
	DOES_NOT_WORK:
	.byte newline,"does not work.",0	;DO_DTE
.popseg
test_sequential_memory_read:
	check_other_test_failed TEST_PPUMEMORYIO
	bcs :-

	begin_test TEST_CIRAM_SEQ_READ_1

	lda #0 ; 1-byte increments
	sta PPUCTRL

	set_ppuaddr $2D55
	ldy PPUDATA  ; something should be produced; $D5 should be placed in read-buffer
	set_ppuaddr $2DC2
	; should produce $D5 (from read buffer); $42 will be placed in read buffer
	fail_if_ppuread_y_not $D5
	fail_if_ppuread_y_not $42
	fail_if_ppuread_y_not $43
	fail_if_ppuread_y_not $44
	
	begin_test TEST_CIRAM_SEQ_READ_32

	lda #4 ; 32-byte increments
	sta PPUCTRL

	set_ppuaddr $2D13
	; should produce $45 (from read buffer); $93 will be placed in read buffer
	fail_if_ppuread_y_not $45
	fail_if_ppuread_y_not $93
	fail_if_ppuread_y_not $B3
	fail_if_ppuread_y_not $D3

	lda #0 ; 1-byte increments
	sta PPUCTRL

:	rts

	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_CIRAM_SEQ_WRITE_1
	.byte "Sequential CIRAM writes",newline	;DO_DTE
	.byte "with  1",$FF	;DO_DTE
	.word BYTE_INCREMENT_DOES_NOT_WORK

	MakeTest TEST_CIRAM_SEQ_WRITE_32
	.byte "Sequential CIRAM writes",newline	;DO_DTE
	.byte "with 32",$FF	;DO_DTE
	.word BYTE_INCREMENT_DOES_NOT_WORK
.popseg

populate_ciram_short_very_quick:
	set_ppuaddr $2D33
	setb PPUDATA, $40
	setb PPUDATA, $A5
	setb PPUDATA, $3B
	setb PPUDATA, $77
	set_ppuaddr $2D33
	lda PPUDATA ; discard
	rts

test_sequential_memory_write:
	check_other_test_failed TEST_PPUMEMORYIO
	bcs :-

	lda #0 ; 1-byte increments
	sta PPUCTRL

	begin_test TEST_CIRAM_SEQ_WRITE_1
	jsr populate_ciram_short_very_quick
	fail_if_ppuread_y_not $40
	fail_if_ppuread_y_not $A5
	fail_if_ppuread_y_not $3B
	fail_if_ppuread_y_not $77

	begin_test TEST_CIRAM_SEQ_WRITE_32

	lda #4 ; 32-byte increments
	sta PPUCTRL
	set_ppuaddr $2D2B
	setb PPUDATA, $73 ; 2D2B
	setb PPUDATA, $A5 ; 2D4B
	setb PPUDATA, $93 ; 2D6B
	setb PPUDATA, $91 ; 2D8B
	setb PPUDATA, $44 ; 2DAB

	lda #0 ; 1-byte increments
	sta PPUCTRL

	set_ppuaddr $2D2B
	ldy PPUDATA ; discard; $73 is based in read buffer
	set_ppuaddr $2D6B
	fail_if_ppuread_y_not $73 ; $93 next
	set_ppuaddr $2DAB
	fail_if_ppuread_y_not $93 ; $44 next
	set_ppuaddr $2D4B
	fail_if_ppuread_y_not $44 ; $A5 next
	set_ppuaddr $2D8B
	fail_if_ppuread_y_not $A5 ; $91 next
	fail_if_ppuread_y_not $91 ; $91 next

	rts

	;;;;;;;;;;;;;;;;;;
.pushseg
	MakeTest TEST_NTA_MIRRORING_FAIL_1NTA
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "1-nametable setup",$FF	;DO_DTE
	.word TEST_NTA_MIRRORING_COMMON
	
	MakeTest TEST_NTA_MIRRORING_FAIL_4NTA
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Four-screen setup",$FF	;DO_DTE
	.word TEST_NTA_MIRRORING_COMMON
	
	MakeTest TEST_NTA_MIRRORING_FAIL_VERT
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Vertical mirroring"	;DO_DTE
	TEST_NTA_MIRRORING_COMMON:
	.byte " seems to",newline	;DO_DTE
	.byte "be active, even though this",newline	;DO_DTE
	.byte "ROM is explicitly configured",newline	;DO_DTE
	.byte "for horizontal mirroring.",0	;DO_DTE
.popseg
test_nta_mirror:
	; Write a byte into each of:
	;	$23FF	- $AA
	;	$27FF	- $BB
	;	$2BFF	- $CC
	;	$2FFF	- $DD
	; Then read each of them.
	; $DD-$DD-$DD-$DD: 1-nametable
	; $AA-$BB-$CC-$DD: 4-nametable
	; $BB-$BB-$DD-$DD: Vertical
	; $CC-$DD-$CC-$DD: Horizontal
	; Shortcut: We only read $23FF.
	set_ppuaddr $23FF
	setb PPUDATA, $AA
	set_ppuaddr $27FF
	setb PPUDATA, $BB
	set_ppuaddr $2BFF
	setb PPUDATA, $CC
	set_ppuaddr $2FFF
	setb PPUDATA, $DD
	
	@fails = gfx_bytecount
	setb @fails, 0
	
	set_ppuaddr $23FF
	check_other_test_failed TEST_ONEBYTEBUFFER
	bcs :+
	ldy PPUDATA
:	ldy PPUDATA
	begin_test TEST_NTA_MIRRORING_FAIL_1NTA
	cpy #$DD
	jsr @failIfEQ

	begin_test TEST_NTA_MIRRORING_FAIL_4NTA
	cpy #$AA
	jsr @failIfEQ

	begin_test TEST_NTA_MIRRORING_FAIL_VERT
	cpy #$BB
	jsr @failIfEQ

	begin_test TEST_CIRAM_READ
	; Only sign a CIRAM_READ failure, if the
	; byte read was none of these four choices.
	cpy #$CC
	beq :+
	lda @fails
	jsr @failIfEQ

:	; Reset the byte at $23FF to what console.s put
	; there, so the user won't notice anything amiss.
	set_ppuaddr $23FF
	setb PPUDATA, (console_palette_index * $55)
	; Note: console_palette_index must not be 0 here, or we will always fail.
@failIfEQ:
	beq :+
	rts
:	inc @fails
	jmp fail_current_test
	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_PPU_OPEN_BUS
	.byte "Any data that is transferred",newline	;DO_DTE
	.byte "through PPU I/O should linger",newline	;DO_DTE
	.byte "and be readable for a while",newline	;DO_DTE
	.byte "in any PPU register that does",newline	;DO_DTE
	.byte "not have a read function.",newline	;DO_DTE
	.byte "This is called «open bus».",newline	;DO_DTE
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "To minimally pass this test,",newline	;DO_DTE
	.byte "you need to at least provide",newline	;DO_DTE
	.byte "a bridge between $2003(W) and",newline	;DO_DTE
	.byte "$2000(R).",0	;DO_DTE

	MakeTest TEST_PPU_OPEN_BUS_SHORTCUT
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Reading a write-only PPU",newline	;DO_DTE
	.byte "register should not just give",newline	;DO_DTE
	.byte "the current value of SPRADDR.",newline	;DO_DTE
	.byte "That would be a too lazy",newline	;DO_DTE
	.byte "workaround for a failed test!",0	;DO_DTE
.popseg
test_ppu_open_bus:
	; Does $2000 give back what was written to $2003?
	begin_test TEST_PPU_OPEN_BUS

	jsr normalize_wait_vbl
	; completely random byte.
	lda #$B1
	sta SPRADDR
	lda #$E4    ; Some emulator may be confused and put this in wrong bus.
	lda PPUCTRL
	cmp #$B1
	jsr fail_current_test_ifNE ; fail if did not return last-written value

	; Hopefully $2000 does NOT just give back SPRADDR
	begin_test TEST_PPU_OPEN_BUS_SHORTCUT
	lda #$A6
	sta SPRADDR
	sta SPRDATA ; should increment SPRADDR a few times
	sta SPRDATA
	lda #$E4
	lda PPUCTRL
	cmp #$A8    ; should NOT return the updated SPRADDR
	jsr fail_current_test_ifEQ
	
	pha
	 begin_test TEST_PPU_OPEN_BUS
	pla
	cmp #$A6    ; instead, should return this
	jsr fail_current_test_ifNE
	rts

	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_PPU_OPENBUS_MUST_NOT_COPY_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "PPU memory read buffer is not",newline	;DO_DTE
	.byte "the open bus. Reading the bus",newline	;DO_DTE
	.byte "should repeat the last value",newline	;DO_DTE
	.byte "that was transferred, not",newline	;DO_DTE
	.byte "disclose the buffered byte.",0	;DO_DTE
.popseg
test_ppu_read_buffer_open_bus:
	check_other_test_failed TEST_PPU_OPEN_BUS
	bcc :+
	rts

:	begin_test TEST_PPU_OPENBUS_MUST_NOT_COPY_READBUFFER

	; It does not matter which values we receive. It can be anything.
	; Still, use the 2D00-2DFF region because at least we know
	; that we have _initialized_ it.
	set_ppuaddr $2D00
	ldx #0
:	ldy PPUDATA
	cpy PPUCTRL
	bne @failed
@continue_loop:
	inx
	bne :-
	rts
@failed:
	; If it failed, figure out whether it failed because the data
	; came from read buffer.
	lda PPUCTRL
	sta temp2	; open bus
	lda PPUDATA
	cmp temp2	; did the bus really disclose the next byte?
	bne @alternative_diagnosis
	jmp fail_current_test
@alternative_diagnosis = @continue_loop
	; Not nagging about this failure for now.


	;;;;;;;;;;;;;;;;;;;


.pushseg
	MakeTest TEST_PPU_OPENBUS_FROM_WRITE2000_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A write to $2000", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_WRITE2001_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A write to $2001", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_WRITE2002_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A write to $2002", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_WRITE2003_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A write to $2003", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_WRITE2004_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A write to $2004", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_WRITE2005_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A write to $2005", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_WRITE2006_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A write to $2006", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_WRITE2007_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A write to $2007", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_READ2000_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A read from $2000", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_READ2001_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A read from $2001", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_READ2002_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A read from $2002", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_READ2003_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A read from $2003", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_READ2004_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A read from $2004", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_READ2005_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A read from $2005", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_READ2006_MUST_NOT_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A read from $2006", $FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON

	TEST_PPU_OPENBUS_FROM_COMMON:
	;      0123456789ABCDEF0123456789ABC|
	.byte                  " must not",newline	;DO_DTE
	.byte "overwrite the $2007 read",newline	;DO_DTE
	.byte "buffer.",0	;DO_DTE

	MakeTest TEST_PPU_OPENBUS_INDEXED
	;      0123456789ABCDEF0123456789ABC|
	.byte "STA $2000,Y with Y=7 must"	;DO_DTE
	OPENBUS_DUMMYREAD_COMMON:
	.byte newline	;DO_DTE
	.byte "issue a dummy read to $2007.",0	;DO_DTE

	MakeTest TEST_PPU_OPENBUS_INDEXED2
	;      0123456789ABCDEF0123456789ABC|
	.byte "STA $1FF0,Y with Y=$17 mustn't",$FF	;DO_DTE
	.word OPENBUS_DUMMYREAD_COMMON

	MakeTest TEST_PPU_OPENBUS_FROM_READ_MIRROR_MUST_WRITETO_READBUFFER
	;      0123456789ABCDEF0123456789ABC|
	.byte "A read from a mirrored copy",newline	;DO_DTE
	.byte "of $2007 must act as if",newline	;DO_DTE
	.byte "$2007 was read, and update",newline	;DO_DTE
	.byte "the same read buffer.",0	;DO_DTE

	MakeTest TEST_PPU_READ_WITH_AND
	.byte "The AND",$FF	;DO_DTE
	.word TEST_PPU_READ_INS_COMMON
	
	MakeTest TEST_PPU_READ_WITH_ORA
	.byte "The ORA",$FF	;DO_DTE
	.word TEST_PPU_READ_INS_COMMON

	MakeTest TEST_PPU_READ_WITH_EOR
	.byte "The EOR",$FF	;DO_DTE
	.word TEST_PPU_READ_INS_COMMON
	
	MakeTest TEST_PPU_READ_WITH_CMP
	.byte "The CMP",$FF	;DO_DTE
	.word TEST_PPU_READ_INS_COMMON

	MakeTest TEST_PPU_READ_WITH_CPX
	.byte "The CPX",$FF	;DO_DTE
	.word TEST_PPU_READ_INS_COMMON
	
	MakeTest TEST_PPU_READ_WITH_CPY
	.byte "The CPY",$FF	;DO_DTE
	.word TEST_PPU_READ_INS_COMMON
	
	MakeTest TEST_PPU_READ_WITH_ADC
	.byte "The ADC",$FF	;DO_DTE
	.word TEST_PPU_READ_INS_COMMON
	
	MakeTest TEST_PPU_READ_WITH_SBC
	.byte "The SBC"	;DO_DTE
	TEST_PPU_READ_INS_COMMON:
	;      0123456789ABCDEF0123456789ABC|
	.byte        " instruction must be",newline	;DO_DTE
	.byte "usable for reading $2007",newline	;DO_DTE
	.byte "or any other I/O port.",0	;DO_DTE
.popseg

test_ppu_read_buffer_open_bus_2:
	jsr populate_ciram_quick
	
	seg_data "RODATA", { @specs: }

	; Do vice versa. Touch the bus, check the read buffer.
	.macro test_12 ppuaddr, dummyaddr, dummybyte, whichtest, t
		.local @x
		.if ppuaddr >= $2F00
		 @x = ((ppuaddr+$33) & $FF)
		.elseif ppuaddr >= $2E00
		 @x = ((ppuaddr+$77) & $FF)
		.elseif ppuaddr >= $2D00
		 @x = ((ppuaddr+$A4) & $FF)
		.else
		 @x = ((ppuaddr+$BB) & $FF)
		.endif
		.if dummybyte = @x
		 .error "ERROR in test_12write: dummybyte must not be the same as the value expected from PPU read buffer"
		.endif
		seg_data "RODATA", { .byte whichtest }
		seg_data "RODATA", { .dbyt ppuaddr }
		seg_data "RODATA", { .byte @x+t, dummybyte }
	.endmacro
	setw addr, @specs

	test_12 $2D33, $2000,$23, TEST_PPU_OPENBUS_FROM_WRITE2000_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2D1E, $2000,  0,  TEST_PPU_OPENBUS_FROM_READ2000_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2CE1, $2001,$C1, TEST_PPU_OPENBUS_FROM_WRITE2001_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2CAF, $2001,  0,  TEST_PPU_OPENBUS_FROM_READ2001_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2EFA, $2002,$00, TEST_PPU_OPENBUS_FROM_WRITE2002_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2E0A, $2002,  0,  TEST_PPU_OPENBUS_FROM_READ2002_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2CA0, $2003,$7A, TEST_PPU_OPENBUS_FROM_WRITE2003_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2C1B, $2003,  0,  TEST_PPU_OPENBUS_FROM_READ2003_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2DB1, $2004,$D0, TEST_PPU_OPENBUS_FROM_WRITE2004_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2D26, $2004,  0,  TEST_PPU_OPENBUS_FROM_READ2004_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2C62, $2005,$1F, TEST_PPU_OPENBUS_FROM_WRITE2005_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2C11, $2005,  0,  TEST_PPU_OPENBUS_FROM_READ2005_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2F11, $2006,$22, TEST_PPU_OPENBUS_FROM_WRITE2006_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2F33, $2006,  0,  TEST_PPU_OPENBUS_FROM_READ2006_MUST_NOT_WRITETO_READBUFFER,0
	test_12 $2E70, $2007,$EB, TEST_PPU_OPENBUS_FROM_WRITE2007_MUST_NOT_WRITETO_READBUFFER,0

	; See comment in @write12_sub for the meaning of these writes.
	setb nmi+8, $8D ; STA ABS
	setb nmi+10, $20 ; $200x
	setb nmi+11, $60 ; RTS

	ldy #0
:	jsr @write12_sub
	cpy #7
	beq :+
	jsr @read12_sub
	iny
	bne :-
:
	test_12 $2C34, $2007,$42, TEST_PPU_OPENBUS_INDEXED, 1 ; +1 to compensate for the dummy read
	jsr @write12_ind

	test_12 $2C02, $2007,$3A, TEST_PPU_OPENBUS_INDEXED2, 0
	jsr @write12_notind

	setb $2000,$00 ; Set PPUCTRL to sane value (we changed it earlier)

	begin_test TEST_PPU_OPENBUS_FROM_READ_MIRROR_MUST_WRITETO_READBUFFER
	;jsr wait_vbl_optional
	jsr populate_ciram_short_very_quick
	lda $33FF	; returns $40, $A5 is in buffer
	cmp #$40
	jsr fail_current_test_ifNE
	lda $2307	; returns $A5, $3B is in buffer
	cmp #$A5
	jsr fail_current_test_ifNE
	lda $2007	; returns $3B, $77 is in buffer
	cmp #$3B
	jsr fail_current_test_ifNE

	; Verify that I/O triggers in the emulator are not bound
	; to specific instructions

	jsr populate_ciram_short_very_quick
	begin_test TEST_PPU_READ_WITH_CMP
	lda #$40
	cmp $2007
	jsr fail_current_test_ifNE
	begin_test TEST_PPU_READ_WITH_EOR
	lda #$F0
	eor $2007
	cmp #$55 ; $F0 xor $A5
	jsr fail_current_test_ifNE
	begin_test TEST_PPU_READ_WITH_SBC
	lda #$3B+$3A
	sec
	sbc $2007
	cmp #$3A
	jsr fail_current_test_ifNE
	begin_test TEST_PPU_READ_WITH_ADC
	lda #(($3A-$77)&$FF)
	clc
	adc $2007 ;expect $77
	cmp #$3A
	jsr fail_current_test_ifNE

	jsr populate_ciram_short_very_quick
	begin_test TEST_PPU_READ_WITH_ORA
	lda #$8B
	ora $2007
	cmp #$CB ; $40 or $8B
	jsr fail_current_test_ifNE
	begin_test TEST_PPU_READ_WITH_AND
	lda #$27
	and $2007
	cmp #$25 ; $A5 and $27
	jsr fail_current_test_ifNE
	begin_test TEST_PPU_READ_WITH_CPX
	ldx #$3B
	cpx $2007
	jsr fail_current_test_ifNE
	begin_test TEST_PPU_READ_WITH_CPY
	ldy #$77
	cpy $2007
	jmp fail_current_test_ifNE


@write12_sub:
	;sta $2000,y cannot be used: It will do a dummy read.
	; So, construct the code in RAM instead.
	jsr @sub_configure
	sty nmi+9
	jsr nmi+8
:	cpx PPUDATA
	jmp fail_current_test_ifNE
@write12_ind:
	jsr @sub_configure
	sta $2000,y
	bne :-
@write12_notind:
	jsr @sub_configure
	ldy #$17
	sta $1FF0,y
	bne :-
@read12_sub:
	jsr @sub_configure
	lda $2000,y
	jmp :-
@sub_configure:
	tya
	pha
	 jsr @byte ; test number
	 jsr begin_test_
	 bit PPUSTATUS
	 jsr @byte ; dbyt ppuaddr hi
	 sta PPUADDR
	 jsr @byte ; dbyt ppuaddr lo
	 sta PPUADDR
	 jsr @byte ; expected byte
	 tax
	 jsr @byte ; byte to write
	pla
	tay
	bit PPUDATA
	rts
@byte:
	ldy #0
	lda (addr),y
	jmp inc_addr


	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_ONEBYTEBUFFER_PALETTE
	;      0123456789ABCDEF0123456789ABC|
	.byte "Palette reads from PPU should",newline	;DO_DTE
	.byte "not have one-byte buffer.",0	;DO_DTE

	MakeTest TEST_PALETTE_READS
	;      0123456789ABCDEF0123456789ABC|
	.byte "Palette reads from PPU do not",newline	;DO_DTE
	.byte "seem to be working at all.",0	;DO_DTE

	MakeTest TEST_PALETTE_READS_UNRELIABLE
	;      0123456789ABCDEF0123456789ABC|
	.byte "Palette reads  from PPU seem",newline	;DO_DTE
	.byte "to work randomly.",0	;DO_DTE

	MakeTest TEST_PALETTE_MIRRORS
	;      0123456789ABCDEF0123456789ABC|
	.byte "Palette indexes $3F1x should",newline	;DO_DTE
	.byte "be mirrors of $3F0x when",newline	;DO_DTE
	.byte "x is 0, 4, 8, or C.",0	;DO_DTE

	MakeTest TEST_PALETTE_UNIQUE
	;      0123456789ABCDEF0123456789ABC|
	.byte "It must be possible to store",newline	;DO_DTE
	.byte "unique data in each of $3F00,",newline	;DO_DTE
	.byte "$3F04, $3F08 and $3F0C.",0	;DO_DTE
.popseg
test_one_byte_buffer_palette:
	begin_test TEST_PALETTE_READS
	jsr console_hide

	.pushseg
	.segment "RODATA"
	@pal:
	;     0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
	.byte $1b,$3B,$16,$00,$05,$16,$18,$35,$39,$0A,$2D,$1B,$26,$1D,$36,$0E
	.byte $13,$29,$3A,$22,$00,$35,$07,$2D,$07,$36,$2B,$3C,$2F,$2E,$12,$06
	.popseg

	; Set a completely random (predefined) palette
	; In a conforming implementation, the writes to $3F00,$3F04 etc.
	; will be overwritten by writes to $3F10,$3F14 etc.
	; Set PPUADDR before every write so that this test does not depend
	; on proper implementation of automatic address-incrementing.
	bit PPUSTATUS
	ldx #0
:	lda #$3F
	sta PPUADDR
	stx PPUADDR
	lda @pal,x
	sta PPUDATA
	inx
	cpx #32
	bcc :-

	; Poke some data in $2F0F to ensure
	; that we aren't getting that mirrored
	set_ppuaddr $2F0F
	setb PPUDATA, $11 ; Some other value than $0E or $06.
	; Let's just hope it won't be mirrored
	; from $1F0F or something other.
	
	ldx #1 ; begin from 1 so that Y won't wrap to zero after X does
	ldy #0 ; number of successful reads 
	; Try this a few times.
:	 set_ppuaddr $3F0F
	 lda #$0E
	 cmp PPUDATA	; Should give this value right away
	 beq @ok
	 cmp PPUDATA	; Gives it on second read if uses a buffer
	 bne @nok
@ok:	 iny
@nok:	inx
	bne :-
	tya
	; Fail if it never worked.
	jsr fail_current_test_ifEQ

	; Fail another test, if the read did not always work
	begin_test TEST_PALETTE_READS_UNRELIABLE
	cpy #$FF
	jsr fail_current_test_ifNE

	begin_test TEST_ONEBYTEBUFFER_PALETTE
	set_ppuaddr $3F0D
	fail_if_ppuread_y_not $1D
	set_ppuaddr $3F1D
	fail_if_ppuread_y_not $2E
	set_ppuaddr $3F0E
	fail_if_ppuread_y_not $36
	
	begin_test TEST_PALETTE_RAM_SEQ_READ_1
	set_ppuaddr $3F19
	fail_if_ppuread_y_not $36
	fail_if_ppuread_y_not $2B
	fail_if_ppuread_y_not $3C

	begin_test TEST_PALETTE_MIRRORS
	; Data written to $3F1x should be read from $3F0x
	set_ppuaddr $3F00
	fail_if_ppuread_y_not $13
	; Data written to $3F0x should be read from $3F1x
	set_ppuaddr $3F00
	setb PPUDATA, $1B
	set_ppuaddr $3F10
	fail_if_ppuread_y_not $1B

	check_other_test_failed TEST_PALETTE_MIRRORS
	bcs :+

	lda #4 ; 32-byte increments
	sta PPUCTRL

	begin_test TEST_PALETTE_RAM_SEQ_READ_32
	set_ppuaddr $3F05
	fail_if_ppuread_y_not $16
	fail_if_ppuread_y_not $16
	fail_if_ppuread_y_not $16

	lda #0 ; 1-byte increments
	sta PPUCTRL

:	begin_test TEST_PALETTE_UNIQUE
	set_ppuaddr $3F04
	fail_if_ppuread_y_not $00
	set_ppuaddr $3F08
	fail_if_ppuread_y_not $07
	set_ppuaddr $3F0C
	fail_if_ppuread_y_not $2F

	check_other_test_failed TEST_PALETTE_UNIQUE
	bcs :+
	; If TEST_PALETTE_UNIQUE passed, go ahead and test the
	; 4,8,C mirrors as well. Otherwise it will be confusing.
	begin_test TEST_PALETTE_MIRRORS
	set_ppuaddr $3F14
	fail_if_ppuread_y_not $00
	set_ppuaddr $3F18
	fail_if_ppuread_y_not $07
	set_ppuaddr $3F1C
	fail_if_ppuread_y_not $2F
:	rts

	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_PPU_PALETTE_WRAP
	;      0123456789ABCDEF0123456789ABC|
	.byte "PPU addresses 3F00-3F1F",newline	;DO_DTE
	.byte "should be mirrored within",newline	;DO_DTE
	.byte "the whole 3F00-3FFF region,",newline	;DO_DTE
	.byte "for a total of 8 times.",0	;DO_DTE
.popseg

test_ppu_palette_wrap:
	check_other_test_failed TEST_PALETTE_READS
	bcc :+
	rts
	
:	begin_test TEST_PPU_PALETTE_WRAP
	jsr console_hide

	; Formula:
	;
	; Write X:byte1 to addr1
	; Write Y:byte2 to addr2 (mirror of addr1)
	; Write X:byte1 to addr3 (something other than addr1)
	; Read addr1, compare to Y
	@addr1 = template_subst_ptr+0
	@addr2 = template_subst_ptr+1
	@addr3 = template_sourceptr+0
	@random = template_sourceptr+1
	@has_buffer = template_bytecount

	; If the emulator has one-byte buffer for palette reads
	; (which it should not have), make this test functional
	; anyway.
	check_other_test_failed TEST_ONEBYTEBUFFER_PALETTE
	lda #0
	adc #0
	sta @has_buffer

	bit PPUSTATUS
	lda #0
@addr1_loop:
	sta @addr1
	sta @addr2
@addr2_loop:
	lda @addr2
	clc
	adc #$20
	cmp @addr1
	beq @addr2_loop_done
	sta @addr2
	eor #$0F
	sta @addr3
	jsr @test
	jmp @addr2_loop
@addr2_loop_done:
	lda @addr1
	clc
	adc #1
	bne @addr1_loop
	rts

@test:
	; Make two random bytes for X,Y (clamp them to 6 bits)
	ldx @random
	inc @random
	lda main,x ; A ROM memory page containing sufficiently random 256 bytes
	and #$3F
	tax
	and #$0F
	eor #$03 ; create a different value (and a dark one at that)
	tay

	jsr @set_addr1
	stx PPUDATA
	;
	lda @addr2
	jsr @set_addr
	sty PPUDATA ;should overwrite addr1 contents
	;
	lda @addr3
	jsr @set_addr
	stx PPUDATA ;write to unrelated address, in order to prevent
	;the test from passing just from using the last-written value
	;
	jsr @set_addr1
	lda @has_buffer
	beq :+
	; If the emulator has broken implementation for palette buffer,
	; do extra read before comparing
	lda PPUDATA
:	lda PPUDATA ; Should be immediately available for reading.
	and #$3F    ; Clear top two bits (they may be set because of open bus).
	sta temp    ; Because the point of this test is not to test the open bus,
	cpy temp    ; we use this slow algorithm rather than just clearing the
	;           ; bus with a "stx SPRADDR" at an earlier point.
	jmp fail_current_test_ifNE

;@fail:
;	zp_res @xxx1,2
;	sty @xxx1+0
;	sta @xxx1+1
;	 jsr console_show
;	 print_str "Fail 1="
;	 lda @addr1
;	 jsr print_hex
;	 print_str ",2="
;	 lda @addr2
;	 jsr print_hex
;	 print_str ",3="
;	 lda @addr3
;	 jsr print_hex
;	 print_str ";e:"
;	lda @xxx1+0
;	jsr print_hex
;	print_str ";r:"
;	lda @xxx1+1
;	jsr print_hex
;	
;	jsr print_newline
;	jsr wait_vbl
;	jsr console_hide
;	rts

@set_addr1:
	lda @addr1
@set_addr:
	bit PPUSTATUS
	pha
	 setb PPUADDR,$3F
	pla
	sta PPUADDR
	rts


	;;;;;;;;;;;;;;;;;;


.pushseg
	MakeTest TEST_PPU_MEMORY_14BIT_A
	;      0123456789ABCDEF0123456789ABC|
	.byte "Failed sub-test 1 of:"	;DO_DTE
	TEST_PPU_MEMORY_14BIT_COMMON:
	.byte newline	;DO_DTE
	.byte "The two MSB within the PPU",newline	;DO_DTE
	.byte "memory address should be",newline	;DO_DTE
	.byte "completely ignored in all",newline	;DO_DTE
	.byte "circumstances, effectively",newline	;DO_DTE
	.byte "mirroring the 0000-3FFF",newline	;DO_DTE
	.byte "address range within the",newline	;DO_DTE
	.byte "whole 0000-FFFF region,",newline	;DO_DTE
	.byte "for a total of 4 times.",0	;DO_DTE

	MakeTest TEST_PPU_MEMORY_14BIT_B
	;      0123456789ABCDEF0123456789ABC|
	.byte "Failed sub-test 2 of:",$FF	;DO_DTE
	.word TEST_PPU_MEMORY_14BIT_COMMON
.popseg
test_ppu_memory_14bit:
	check_other_test_failed TEST_PPUMEMORYIO
	bcs @rts
	check_other_test_failed TEST_PALETTE_READS
	bcc :+
@rts:	rts
:
	begin_test TEST_PPU_MEMORY_14BIT_A
	jsr console_hide

	@palette_has_buffer   = template_targetptr
	@reads_have_no_buffer = template_targetptr+1
	@reference  = template_sourceptr
	@offset     = template_sourceptr+1
	@addr       = addr

	; Because the point of this test is not to test the one-byte
	; buffer, check whether those tests failed and workaround
	; accordingly.
	check_other_test_failed TEST_ONEBYTEBUFFER_PALETTE
	lda #0
	sta @addr+0
	sta @addr+1
	adc #0
	sta @palette_has_buffer
	check_other_test_failed TEST_ONEBYTEBUFFER
	lda #0
	adc #0
	sta @reads_have_no_buffer

	; Verify that for each byte in 0000-3FFF range,
	; the exact same value can be read in +4000, +8000 and +C000.
@loop:
	; Read from @addr
	lda @addr+1
	jsr @read_at
	sta @reference
	lda #$40
	sta @offset
@loop_mirrors:
	; Read from @addr + @offset*$100
	; Which is, @addr + $4000
	;           @addr + $8000
	;       and @addr + $C000
	lda @addr+1
	ora @offset
	jsr @read_at
	; Should be the same as what was read from @addr
	cmp @reference
	jsr fail_current_test_ifNE
	lda @offset
	clc
	adc #$40
	sta @offset
	bne @loop_mirrors

	; Good. Go to next PPU addr. Skip in increments
	; of +57, so we cover the 0x4000 range in 288 loops,
	; and it won't take an intolerable amount of time.
	lda @addr+0
	clc
	adc #57
	sta @addr+0
	lda @addr+1
	adc #0
	sta @addr+1
	cmp #$40
	bcc @loop

	begin_test TEST_PPU_MEMORY_14BIT_B

	; Verify that a write to $7F03 updates the byte at $3F03.
	setb addr+0, 3
	ldx #$7F
	ldy #$3F
	jsr @read_write_test
	; Verify that a write to $3F03 updates the byte at $BF03.
	ldx #$3F
	ldy #$BF
	jsr @read_write_test
	; Verify that a write to $ADDD updates the byte at $6DDD.
	setb addr+0, $DD
	ldx #$AD
	ldy #$6D
	;jmp @read_write_test
@read_write_test:	
	txa
	jsr @setaddr
	jsr @read
	eor #$13
	sta @reference ; Change the byte
	;
	txa
	jsr @setaddr
	lda @reference
	sta PPUDATA    ; Write the changed byte
	;
	tya
	jsr @setaddr
	jsr @read
	cmp @reference ; Check that the write is shown in mirrored address
	jmp fail_current_test_ifNE
@read_at:
	jsr @setaddr
@read:	and #$3F
	cmp #$3F
	bcc @normal_read
@palette_read:
	lda @palette_has_buffer
	beq @one_read
	bne @two_reads
@normal_read:
	lda @reads_have_no_buffer
	bne @one_read
@two_reads:
	lda PPUDATA
@one_read:
	lda PPUDATA
	rts
@setaddr:
	sta PPUADDR
	pha
	 lda @addr+0
	 sta PPUADDR
	pla
	rts
	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_PPU_MIRROR_3000
	;      0123456789ABCDEF0123456789ABC|
	.byte "PPU memory range 3000-3EFE",newline	;DO_DTE
	.byte "should be a mirror of the",newline	;DO_DTE
	.byte "PPU memory range 2000-2EFE.",0		;DO_DTE

	MakeTest TEST_PPU_READ_3EFF
	.byte "Setting PPU address to 3EFF",newline	;DO_DTE
	.byte "and reading $2007 twice",newline		;DO_DTE
	.byte "should give the data at",newline		;DO_DTE
	.byte "$3F00, not the data at $2EFF.",0		;DO_DTE

	MakeTest TEST_PPU_MIRROR_2Fxx
	;      0123456789ABCDEF0123456789ABC|
	.byte "Reading PPU memory range 3Fxx",newline	;DO_DTE
	.byte "should put contents of 2Fxx",newline	;DO_DTE
	.byte "into the read buffer.",0			;DO_DTE

	MakeTest TEST_PPU_SEQ_READ_WRAP
	.byte "Setting PPU address to 3FFF",newline	;DO_DTE
	.byte "& reading $2007 thrice should",newline	;DO_DTE
	.byte "give the contents of $0000.",0		;DO_DTE

	MakeTest SEQ_READ_INTERNAL
	;      0123456789ABCDEF0123456789ABC|
	.byte "Unexpected: VROM contents at",newline	;DO_DTE
	.byte "$0000 and $1FFF read the same.",newline	;DO_DTE
	.byte "This should never happen in",newline	;DO_DTE
	.byte "this test ROM.",0	;DO_DTE
	
.popseg
test_ppu_seq_read_wrap:
	begin_test TEST_PPU_MIRROR_3000
	jsr console_hide

	ldy #0
	ldx #$20
 	bit PPUSTATUS
@loop:
	; Read whatever is at $2xxx
	stx PPUADDR
	sty PPUADDR
	jsr @read2 ; load $2xxx (whatever is there)
	pha
	 ; Read whatever is at $3xxx
	 txa
	 ora #$10
	 sta PPUADDR
	 sty PPUADDR
	 lda PPUDATA ; discard buffer
	pla
	cmp PPUDATA ; compare $3xxx
	bne @failed
	iny
	bne @loop
	inx
	cpx #$2F
	bcc @loop
	; Suspicious "ok!" $3EFF should have failed.
	begin_test TEST_PPU_READ_3EFF
	bcs @do_fail
@failed:
	; Don't fail the test if the reading address was
	; $2EFF and $3EFF. That particular address doesn't
	; work nicely.
	cpy #$FF
	bne @do_fail
	cpx #$2E
	beq @ok
@do_fail:
	jsr fail_current_test
@ok:
	begin_test TEST_PPU_READ_3EFF ; Mark this tested and passed

	;
	begin_test TEST_PPU_MIRROR_2Fxx
	ldy #0
	ldx #$2F
 	bit PPUSTATUS
@loop2:
	; Read whatever is at $2Fxx
	stx PPUADDR
	sty PPUADDR
	
	jsr @read2 ; load $2Fxx (whatever is there)
	pha
	 jsr @read3 ; Read PPUDATA a few times in order to put some dummy data in the read buffer.
	 lda PPUDATA
	 ; Read whatever is at $3xxx
	 txa
	 ora #$10
	 sta PPUADDR
	 sty PPUADDR
	 lda PPUDATA ; put byte in buffer, discard palette value
	 
	 ; Set PPUADDR to _some_ address that has a read buffer.
	 lda #0
	 sta PPUADDR
	 sta PPUADDR
	pla
	cmp PPUDATA ; compare $3xxx
	jsr fail_current_test_ifNE
	bne @failed
	iny
	bne @loop2

	begin_test TEST_PPU_SEQ_READ_WRAP

	; In order to avoid false positives, ensure
	; that all of the following locations contain
	; different bytes:
	;    $0000 - readonly
	;    $1FFF - readonly, so internal error if same content as $0000
	;    $2FFF
	;    $3F1F
	;    $3F00
	begin_test SEQ_READ_INTERNAL

	set_ppuaddr $0000
	jsr @read2
	sta temp
	
	set_ppuaddr $1FFF
	jsr @read2
	cmp temp
	jeq fail_current_test

	begin_test TEST_PPU_SEQ_READ_WRAP

	set_ppuaddr $2FFF
	lda temp
	and #$0F
	clc
	adc #$03
	tax
	set_ppuaddr $2FFF
	stx PPUDATA

	set_ppuaddr $3F1F
	lda temp
	and #$0F
	clc
	adc #$05
	tax
	set_ppuaddr $3F1F
	stx PPUDATA
	
	set_ppuaddr $3F00
	lda temp
	and #$0F
	clc
	adc #$06
	tax
	set_ppuaddr $3F00
	stx PPUDATA

	set_ppuaddr $3FFF
	jsr @read3
	cmp temp
	jmp fail_current_test_ifNE

@read3: lda PPUDATA
@read2: lda PPUDATA
@read1: lda PPUDATA
	rts


	;;;;;;;;;;;;;;;;;;


.pushseg
	MakeTest TEST_VADDR
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Relationship between $2005",newline	;DO_DTE
	.byte "and $2006 is not implemented",newline	;DO_DTE
	.byte "properly. Here is a guide.",newline	;DO_DTE
	.byte "It explains which registers",newline	;DO_DTE
	.byte "use which parts of the address.",newline	;DO_DTE
	.byte "Note that only the second",newline	;DO_DTE
	.byte "write to $2006 updates the",newline	;DO_DTE
	.byte "address really used by $2007.",newline	;DO_DTE
	.byte "FEDCBA9876543210ZYX: bit pos.",newline	;DO_DTE
	.byte "  ^^^^^^^^^^^^^^------ =$2007",newline	;DO_DTE
	.byte "zz543210-------------- $2006#1",newline	;DO_DTE
	.byte "        76543210------ $2006#2",newline	;DO_DTE
	.byte "           76543210--- $2005#1",newline	;DO_DTE
	.byte " 210--76543----------- $2005#2",newline	;DO_DTE
	.byte "    10---------------- $2000",0	;DO_DTE
	;        ||||||
	;        ||||||_ 0100
	;        |||||__ 0200
	;        ||||___ 0400
	;        |||____ 0800
	;        ||_____ 1000
	;        |______ 2000
	;

.popseg
test_vaddr:
	
	seg_data "RODATA", {@data:}
	.macro vaddr_set_test addr, expected_value
	    .pushseg
	        .segment "RODATA"
		.byte ((addr >> 10) & 3)	; $2000
		.byte ((addr & $1F) << 3)	; $2005.1
		.byte (((addr >> 12) & 7) | (((addr >> 5) & $1F) << 3)) ; $2005.2
		.byte ((addr & $FF))		; $2006.2
		.byte expected_value
	    .popseg
	.endmacro

	begin_test TEST_VADDR
	jsr populate_ciram_quick

	; Puts a smiley at the end of the title line.
	set_ppuaddr $205E
	@c1 = ':'-DTE_CHARSET_OFFSET	;DTE_CHARMAP
	; blank line
	@c2 = ')'-DTE_CHARSET_OFFSET	;DTE_CHARMAP
	setb PPUDATA, @c1+$80	; This byte will be used for verification below
	setb PPUDATA, @c2+$80
	set_ppuaddr $23C7
	@c3 = (console_palette_index * $15) + 2*$40
	setb PPUDATA, @c3
	
	set_vrom_page 0
	; CIRAM read - NTA 1
	vaddr_set_test $2C24, (($24+$BB)&$FF)
	vaddr_set_test $24B3, (($B3+$BB)&$FF)
	vaddr_set_test $2D24, (($24+$A4)&$FF)
	vaddr_set_test $25B3, (($B3+$A4)&$FF)
	vaddr_set_test $2E24, (($24+$77)&$FF)
	vaddr_set_test $26B3, (($B3+$77)&$FF)
	vaddr_set_test $2F24, (($24+$33)&$FF)
	vaddr_set_test $27B3, (($B3+$33)&$FF)
	; CIRAM read - NTA 0
	vaddr_set_test $205E, @c1+$80
	vaddr_set_test $285F, @c2+$80
	vaddr_set_test $2BC7, @c3
	; VROM read.
	vaddr_set_test $12D0, 'Q'	;page*3_B.chr
	vaddr_set_test $1660, 'U'	;page*3_B.chr
	vaddr_set_test $1810, 'D'	;page*3_B.chr
	vaddr_set_test $1EC1, '7'	;page*3_B.chr
	vaddr_set_test $00B4, 'f'	;ascii.chr
	seg_data "RODATA", {.byte $FF}

	setw addr, @data
	ldy #$FF
@more_test:
	jsr @byte
	bmi @no_more_test
	bit PPUSTATUS
	sta $2000	;$2000
	jsr @byte
	sta $2005	;$2005.1
	pha
	 jsr @byte
	 sta $2005	;$2005.2
	pla
	sta $2005	;$2005.1
	jsr @byte
	sta $2006	;$2006.2
	lda PPUDATA
	jsr @byte
	cmp PPUDATA
	jsr fail_current_test_ifNE
	jmp @more_test
@byte:
	iny
	lda (addr),y
@no_more_test:
	rts


	;;;;;;;;;;;;;;;;;;


.pushseg
	MakeTest TEST_RAM_MIRRORING
	.byte "CPU RAM at 0000-07FF should",newline	;DO_DTE
	.byte "be mirrored 4 times, in the",newline	;DO_DTE
	.byte "following address ranges:",newline	;DO_DTE
	.byte "- 0000-07FF",newline	;DO_DTE
	.byte "- 0800-0FFF",newline	;DO_DTE
	.byte "- 1000-17FF",newline	;DO_DTE
	.byte "- 1800-1FFF",0	;DO_DTE
.popseg
test_ram_mirroring:
	begin_test TEST_RAM_MIRRORING

	jsr normalize_wait_vbl

	@ptr1 = template_sourceptr
	@ptr2 = template_targetptr
	
	lda #7
	sta @ptr1+1
	lda #0
	sta @ptr1+0
	sta @ptr2+0
@loop_nextpage:
	lda @ptr1+1
	 ora #8
@loop_nextmirror:
	 sta @ptr2+1
@loop_256:
	 lda (@ptr1),y
	 cmp (@ptr2),y
	 jsr fail_current_test_ifNE
	 iny
	 bne @loop_256
	 
	 lda @ptr2+1
	 clc
	 adc #8
	 cmp #$20
	 bcc @loop_nextmirror
	dec @ptr1+1
	bpl @loop_nextpage
	rts

	;;;;;;;;;;;;;;;;;;


.pushseg
	MakeTest TEST_PPUIO_MIRRORING
	;      0123456789ABCDEF0123456789ABC|
	.byte "PPU I/O memory at 2000-2007",newline	;DO_DTE
	.byte "should be mirrored within the",newline	;DO_DTE
	.byte "whole 2000-3FFF region, for",newline	;DO_DTE
	.byte "a total of 1024 times.",0	;DO_DTE
.popseg
test_ppuio_mirroring:
	begin_test TEST_PPUIO_MIRRORING

	jsr populate_ciram_quick

	; Do test using $2000 and $2000
	ldx #$20
	lda #$00
	ldy #0
	jsr @ppudata_read_loop
	; Do test using $2B48 and $2BE0
	ldx #$2B
	lda #$48
	ldy #$48 ^ $E0
	jsr @ppudata_read_loop
	; Do test using $3100 and $31B0
	ldx #$31
	lda #$00
	ldy #$00 ^ $B0
	jsr @ppudata_read_loop
	; Do test using $3FF0 and $3F28
	ldx #$3F
	lda #$F0
	ldy #$F0 ^ $28
	jsr @ppudata_read_loop

	jmp console_show
@ppudata_read_loop:
	; Set PPU memory address using the I/O port at x*$100+a+(PPUADDR&7)
	sty temp
	stx addr+1
	and #$F7
	clc
	adc #(PPUADDR & 7)
	sta addr+0
	; Set VRAM address $2C00
	ldy #0
	lda #$2C
	sta (addr),y
	lda addr
	eor temp
	sta addr
	lda #$00
	sta (addr),y
	; We will do a total of 1024 reads.
	; This means that we will be reading the CIRAM
	; region of 2C00-2FFF.
	; For 20xx-27xx, $2Cxx.
	; For 28xx-2Fxx, $2Dxx.
	; For 30xx-37xx, $2Exx.
	; For 38xx-3Fxx, $2Fxx.
	seg_data "RODATA", { @offsets: .byte $BB,$A4,$77,$33 }
	lda PPUDATA
	setw ppuio_addr, PPUDATA
@test_more:
	ldy #0
	lda #7
	bit ppuio_addr+1
	bne @loop_32_reads
	lda ppuio_addr+1
	lsr
	lsr
	lsr ; 20..3F -> 04..07
	tax
	lda @offsets-4,x
	sta ppuio_expect
@loop_32_reads:
	lda (ppuio_addr),y
	cmp ppuio_expect
.if 0
	bne @failed
.else
	jsr fail_current_test_ifNE
.endif
	inc ppuio_expect
	tya
	 clc
	 adc #8
	tay
	bne @loop_32_reads

	lda ppuio_addr+1
	clc
	adc #1
	cmp #$30
	bcs @test_done
	sta ppuio_addr+1
	jmp @test_more

@test_done:
	rts
.if 0
@failed:
	pha
	 sty ppuio_offset
	 text_color1
	 print_ext_str "Failing address: $"	;DO_DTxE
	 lda ppuio_addr+1
	 jsr print_hex
	 lda ppuio_addr
	 clc
	 adc ppuio_offset
	 jsr print_hex
	 print_ext_str newline, "Expected $"	;DO_DTxE
	 lda ppuio_expect
	 jsr print_hex
	 print_ext_str ", got $"	;DO_DTxE
	pla
	jsr print_hex
	jsr print_newline
	jmp fail_current_test
.endif
	;;;;;;;;;;;;;;;;;;


; Perform sprite 0 hit test. A = 0 if expect no-hit, nonzero if expect hit.
spr0hit_test_helper:
	delay 40
	bit PPUSTATUS
	bvc @vbl_ok
	print_ext_str_flush_rts " VBL?"	;DO_DTE
@vbl_ok:
	cmp #0
	beq spr0_hit_test_helper_expect_no_hit
	;passthru
spr0_hit_test_helper_expect_hit:
	ldx #0
:	 delay_scanlines 3
	 bit PPUSTATUS
	 bvs @found
	inx
	cpx #100/3
	bcc :-
	text_white
	inc spr0hit_fails_current
	jsr fail_current_test
	print_ext_str " FAIL"	;DO_DTE
	jmp sec_rts_flush
@found:
	text_color1
	print_str " OK  "	;DO_DTE
	;txa
	;jsr print_dec_00_99
clc_rts_flush:
	jsr console_flush
	clc
	rts


spr0_hit_test_helper_expect_no_hit:
	ldx #0
:	 delay_scanlines 3
	 bit PPUSTATUS
	 bvs @fail
	inx
	inx
	inx
	cpx #100
	bcc :-
	text_color1
	print_ext_str " OK  "	;DO_DTE
	jmp clc_rts_flush
@fail:
	text_white
	inc spr0hit_fails_current
	jsr fail_current_test
	print_str " ??"	;DO_DTE
	txa
	jsr print_dec_00_99
sec_rts_flush:
	jsr console_flush
	sec
	rts

spr0hit_test_clear_hit_flag:
	txa
	pha
	 setb PPUCTRL, $28 ; 1-byte increments, sprites from $1000; big sprites
	 setb PPUMASK, $F*2 ;Show background graphics and sprites

	 jsr wait_vbl_optional
	 jsr clear_oam
	;.repeat $2, j
	;  .repeat $20, i
	;    setb SPRADDR, i*4 + j*$80
	;    stx SPRDATA
	;  .endrepeat
	;.endrepeat
	; setb SPRADDR, 0
	pla
	tax
	; This should make both sprite 0 hit flag and sprite overflow flag
	; to be cleared for next frame.
	;
	;setb PPUMASK, $F*2 ;Show background graphics and sprites
	jsr wait_vbl_optional
	jmp wait_vbl_optional ; one more frame in order to clear sprite0 hit flag


zp_byte spr0hit_readbuffertest_result

spr0hit_readbuffertest_begin:
	jsr populate_ciram_short_very_quick
	lda PPUDATA	; returns $40, $A5 is placed in buffer
	rts

spr0hit_readbuffertest_end:
	lda PPUDATA	; returns $A5 if read buffer was intact
	eor #$A5
	sta spr0hit_readbuffertest_result
	jmp console_apply_scroll ; Restore PPU address.
	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_SPHIT_AND_VBLANK
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Sprite 0 hit flag should not",newline	;DO_DTE
	.byte "read as set during vblank.",0	;DO_DTE
.popseg
test_spr0hit_and_vblank:
	begin_test TEST_SPHIT_AND_VBLANK

	jsr spr0hit_test_clear_hit_flag
	setb SPRADDR, $FF 
	setb SPRDATA, $0A
	lda $2000
	lda $2001
	lda $2002
	and #$20
	jsr fail_current_test_ifNE
	rts


	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_SPHIT_DIRECT
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Sprite 0 hit test by poking",newline	;DO_DTE
	.byte "data directly into $2003-4",newline	;DO_DTE
	.byte "^ Possible causes for failure:",newline	;DO_DTE
	.byte "- $2003/$2004 not implemented",newline	;DO_DTE
	.byte "- No sprite 0 hit tests",newline	;DO_DTE
	.byte "- Way too long vblank period",0	;DO_DTE
	
	MakeTest TEST_SPHIT_DIRECT_READBUFFER
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Sending 5 bytes of data into",newline	;DO_DTE
	.byte "$2003 and $2004",$FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON
.popseg
test_spr0hit_and_dma:
	begin_test TEST_SPHIT_DIRECT

	jsr normalize_wait_vbl
	;              0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|"
	text_white
	print_ext_str "Performing tests that combine",newline,"sprite 0 hit flag, $4014 DMA",newline,"and the RAM mirroring...",newline	;DO_DTE
	text_color1
	print_ext_str "Graphical artifacts during",newline,"this test are OK and expected.",newline,newline	;DO_DTE
	
	;0123456789ABCDEF0123456789ABC|
	;               Hit  No-Hit
	;Direct poke:   OK   OK
	;DMA with ROM:  
	;DMA + PPU BUS: 
	;DMA with 0000: OK   OK
	;               VBL? VBL?
	;               FAIL FAIL

	text_white
	print_ext_str_flush "              Hit   No-Hit",newline	;DO_DTE

	jsr test_spr0hit_and_vblank
	jsr test_spr0hit
	jsr test_spr0hit_and_vblank
	jsr test_dma_rom
	jsr test_spr0hit_and_vblank
	jsr test_dma_ppu_openbus
	jsr test_spr0hit_and_vblank
	jsr test_dma_mirroring
	jsr test_spr0hit_and_vblank
	
	jmp print_newline

test_spr0hit:
	begin_test TEST_SPHIT_DIRECT

	text_white
	print_ext_str_flush "Direct poke  "	;DO_DTE

	jsr spr0hit_test_clear_hit_flag
	jsr spr0hit_readbuffertest_begin
	; Upload this sprite-object that is known to cause sprite-0
	; hit within the first few frames after vblank.
	ldx #0
	stx SPRADDR
:	lda SpriteDMA_test_pattern,x
	sta SPRDATA
	inx
	cpx #4
	bcc :-
	setb SPRADDR, $00
	jsr spr0hit_readbuffertest_end
	
	; Expect hit
	lda #1
	jsr spr0hit_test_helper

	jsr spr0hit_test_clear_hit_flag

	; Expect no-hit
	lda #0
	jsr spr0hit_test_helper
	
	begin_test TEST_SPHIT_DIRECT_READBUFFER
	lda spr0hit_readbuffertest_result
	jsr fail_current_test_ifNE

	jmp print_newline


	;;;;;;;;;;;;;;;;;;


.pushseg	
	MakeTest TEST_SPHIT_DMA_ROM
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Sprite 0 hit test using DMA",newline	;DO_DTE
	.byte "($4014) using ROM as source",newline	;DO_DTE
	.byte "^ Possible causes for failure:",newline	;DO_DTE
	.byte "- $4014 DMA cannot read from",newline	;DO_DTE
	.byte "  anything other than RAM",0	;DO_DTE

	MakeTest TEST_SPHIT_DMA_READBUFFER
	.byte "Invoking a $4014 DMA with a",newline	;DO_DTE
	.byte "non-$20 value ",$FF	;DO_DTE
	.word TEST_PPU_OPENBUS_FROM_COMMON
.popseg
test_dma_rom:
	begin_test TEST_SPHIT_DMA_ROM
	;DRY-RUN

	text_white
	print_ext_str "DMA with ROM  "	;DO_DTE

	; Some emulators may have a buggy DMA implementation that triggers a crash.
	; Set up an NMI handler to rescue.
	jsr crash_proof_begin
	bcs @crashed

	; Place byte $F8 in the beginning of each RAM page
	; in order to ensure that the following DMA access will
	; not false-trigger due to using RAM rather than ROM.
	lda #$F8
	ldy #0
	ldx #7
	sty template_targetptr+0
:	stx template_targetptr+1
	sta (template_targetptr),y
	dex
	bpl :-

	ldx #>SpriteDMA_test_pattern
	ldy #1 ;expect hit
	jsr @half_test
	
	begin_test TEST_SPHIT_DMA_ROM ; Restore test name
	bcc :+
	jsr fail_current_test

:	ldx #>$C000 ; From filler.bin. Coincidentally consists
	; of content we know perfectly well what is in where,
	; and happens to not trigger sprite-0 hit.
	;
	ldy #0 ; expect no hit
	jsr @half_test
	jsr crash_proof_end

@crashed:
	jmp print_newline

@half_test:
	; Clear OAM first, to ensure the DMA will do its job
	jsr spr0hit_test_clear_hit_flag
	jsr spr0hit_readbuffertest_begin
	; Execute DMA
	stx $4014
	jsr spr0hit_readbuffertest_end
	tya
	jmp spr0hit_test_helper

spr0hit_readbuffertest_dma_conclude:
	begin_test TEST_SPHIT_DMA_READBUFFER
	lda spr0hit_readbuffertest_result
	jmp fail_current_test_ifNE
	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_SPHIT_DMA_PPU_BUS
	.byte "Sprite 0 hit test using DMA",newline	;DO_DTE
	.byte "($4014) using PPU I/O bus",newline	;DO_DTE
	.byte "as source",newline	;DO_DTE
	.byte "^ In this test, $4014 <- #$20.",newline	;DO_DTE
	.byte "  Possible causes for failure:",newline	;DO_DTE
	.byte "- DMA does not do proper reads",newline	;DO_DTE
	.byte "- PPU bus does not preserve",newline	;DO_DTE
	.byte "  last transferred values",newline	;DO_DTE
	.byte "- $2002 read returned a value",newline	;DO_DTE
	.byte "  that differs from expected",newline	;DO_DTE
	.byte "- $2004 read modifies the OAM",0	;DO_DTE
	
	MakeTest TEST_DMA_PPU_SIDEEFFECT
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Writing $20 into $4014 should",newline	;DO_DTE
	.byte "generate 32 reads into $2007",newline	;DO_DTE
	.byte "as a side-effect, each time",newline	;DO_DTE
	.byte "incrementing the PPU read",newline	;DO_DTE
	.byte "address.",0	;DO_DTE
.popseg
test_dma_ppu_openbus:
	check_other_test_failed TEST_PPUIO_MIRRORING
	bcc :+
	rts
	
:	@ppu_read_temp = scroll_y+0
	@read_count    = scroll_y+1

	begin_test TEST_SPHIT_DMA_PPU_BUS

	text_white
	print_ext_str_flush "DMA + PPU bus"	;DO_DTE

	; Some emulators may have a buggy DMA implementation that triggers a crash.
	; Set up an NMI handler to rescue.
	jsr crash_proof_begin
	bcs @crashed

	; Place $0A in PPU's open bus
	ldx #$0A
	ldy #1 ; expect hit
	jsr @half_test
	
	; EXPECTED SPRITE:
	;      From $2000:  0A (open bus)
	;      From $2001:  0A (open bus, again)
	;      From $2002:  8A (open bus + vblank flag; no sp0hit, no spoverflow)
	;      From $2003:  8A (open bus)
	;          If Vblank is unset, $8A will become $0A instead.

	; I.e. coordinates: $85,$05
	;        pattern:   $05  (tile #2 from $1000)
	;        attribute: $80 for Vertical flipping
	;                   $00 for Priority: On top of background
	;                   $01 for sprite palette #1

	; Place $F8 in PPU's open bus
	ldx #$F8
	ldy #0 ;expect no hit
	jsr @half_test
	jsr crash_proof_end

	begin_test TEST_DMA_PPU_SIDEEFFECT
	lda @read_count
	cmp #32
	beq @ok
	
	jsr print_newline
	;                    0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	print_ext_str_flush "Side-effect failure:",newline	;DO_DTE
	lda @read_count
	jsr print_dec
	print_ext_str " reads seen, 32 expected."	;DO_DTE
@crashed:
@ok:
	jmp print_newline

@half_test:
	txa
	pha
	
	 ; Clear OAM first, to ensure the DMA will do its job
	 jsr spr0hit_test_clear_hit_flag
	 ;
	 ; Fill some PPU memory with predictable data, so we
	 ; can also do TEST_DMA_PPU_SIDEEFFECT simultaneously.
	 set_ppuaddr ($2D00-$33)
	 jsr @ppu_fill_loop
	 ;
	 set_ppuaddr ($2E00-$33)
	 jsr @ppu_fill_loop
	 ;
	 set_ppuaddr $2D00
	 lda PPUDATA
	pla
	tax
	;
	; Execute DMA
	setb SPRADDR, $FF
	stx SPRDATA  ; SPRADDR wraps to $00; X is now in PPU's open bus
	;
	setb $4014, >$2000 ; Note: This will also do 32 reads from $2007 as a side effect.
	;
	ldx PPUDATA
	;
	jsr console_apply_scroll ; Restore PPU address because of that.
	;
	stx @ppu_read_temp
	;
	tya
	jsr spr0hit_test_helper
	;
	; Collect data for TEST_DMA_PPU_SIDEEFFECT
	lda @ppu_read_temp
	sec
	sbc #$33
	sta @read_count
	rts
@ppu_fill_loop:
	;; Total: 2 + (4+2+3)*256 - 1 cycles = 2305 cycles
	ldx #0		; 2
	: .repeat 4
	  stx PPUDATA	; 4
	  inx		; 2
	  .endrepeat
	  bne :-	; 3
	; Total:             2 + (6*4+3)*(256/4) - 1 = 1729 cycles
	; Without unrolling, 2 + (6*1+3)*(256/1) - 1 = 2305 cycles
	; Vblank region is about 2270 cycles.
	jsr console_apply_scroll
	jmp wait_vbl_optional
	;;;;;;;;;;;;;;;;;;


.pushseg
	MakeTest TEST_SPHIT_DMA_RAM
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "Sprite 0 hit test using DMA.",newline	;DO_DTE
	.byte "All internal RAM pages are",newline	;DO_DTE
	.byte "tested, including mirrored",newline	;DO_DTE
	.byte "addresses. Failing the test",newline	;DO_DTE
	.byte "may imply faulty mirroring.",0	;DO_DTE
.popseg
test_dma_mirroring:
	@spr0hit_fails = template_sourceptr

	begin_test TEST_SPHIT_DMA_RAM
	jsr save_console
	ldx #0
	stx @spr0hit_fails

@loop_more_memory:
	lda #0
	sta spr0hit_fails_current
	
	jsr my_restore_console

	text_white
	print_ext_str_flush "DMA with "	;DO_DTE
	txa
	jsr print_hex
	print_ext_str_flush "00 "	;DO_DTE

	txa
	pha
	 jsr crash_proof_begin
	 bcs @crashed
	pla
	pha
	 sta template_targetptr+1
	 lda #0
	 sta template_targetptr+0

	 bit PPUSTATUS
	 ldy #0
	@fill_loop:
	 lda SpriteDMA_test_pattern,y
	 sta (template_targetptr),y
	 iny
	 cpy #4
	 bcc @fill_loop

	 ldy #1 ; expect hit
	 jsr @half_test
	 begin_test TEST_SPHIT_DMA_RAM
	 bcc :+
	 jsr fail_current_test
	 inc @spr0hit_fails
	:

	 ldy #0
	 lda #$F8 ; hide sprite 0
	 sta (template_targetptr),y

	 ;ldy #0 ; expect no hit
	 jsr @half_test ; does DMA transfer again

@crashed:
	 begin_test TEST_SPHIT_DMA_RAM
	 bcc :+
	 jsr fail_current_test
	:
	 lda spr0hit_fails_current
	 beq @ok
	 
	 jsr print_newline
	 jsr save_console
@ok:	jsr crash_proof_end

	pla
	tax
	
	cpx #3+1
	bcs @dont_test_early_fails
	lda @spr0hit_fails
	cmp #3
	bcs @pointless

@dont_test_early_fails:
	inx
	cpx #$20
	bcc @loop_more_memory

	jsr my_restore_console
	text_white
	print_ext_str_flush_rts "DMA with RAM  ",newline	;DO_DTE

@half_test:
	jsr spr0hit_test_clear_hit_flag
	jsr spr0hit_readbuffertest_begin
	lda template_targetptr+1
	sta $4014 ; Execute DMA transfer
	jsr spr0hit_readbuffertest_end
	tya
	jsr spr0hit_test_helper
	jmp spr0hit_readbuffertest_dma_conclude

@pointless:
	print_ext_str newline,"Umm, do I need to go on?",newline ;DO_DTE
	rts

	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_CHRROM_READ_BANKED
	.byte "CHR ROM read through $2007",newline	;DO_DTE
	.byte "does not honor mapper 3",newline	;DO_DTE
	.byte "(CNROM) bank switching",0	;DO_DTE

	MakeTest TEST_CHRROM_READ_BANKED_BUFFER
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "The $2007 read buffer should",newline	;DO_DTE
	.byte "not retroactively react to",newline	;DO_DTE
	.byte "changes in VROM mapping.",newline	;DO_DTE
	.byte "When you read $2007, the",newline	;DO_DTE
	.byte "data is stored in a buffer",newline	;DO_DTE
	.byte "(«latch»), and the previous",newline	;DO_DTE
	.byte "content of the buffer is",newline	;DO_DTE
	.byte "returned. It is not a delayed",newline	;DO_DTE
	.byte "read request.",0	;DO_DTE
.popseg
test_chr_rom_read:
	check_other_test_failed TEST_PPUMEMORYIO
	bcc :+
	rts

:	
	begin_test TEST_CHRROM_READ
	jsr wait_vbl_optional

	lda #0 ; 1-byte increments
	sta PPUCTRL

	; Currently in bank 0.
	set_ppuaddr ($1FF0)
	ldy PPUDATA
	; This data is manually verified to match gfx.spr.page3_B.chr. It has no special meaning to it.
	ldx #0
:	lda PPUDATA
	cmp SecretMessage,x
	jsr fail_current_test_ifNE

	begin_test TEST_CHRROM_SEQ_READ_1
	inx
	cpx #16
	bcc :-


	lda #4 ; 32-byte increments
	sta PPUCTRL

	begin_test TEST_CHRROM_SEQ_READ_32
	
	; Currently in bank 0.
	set_ppuaddr ($1660)
	ldy PPUDATA
	; This data is manually verified to match gfx.spr.page3_B. It has no special meaning to it.
	fail_if_ppuread_y_not 'U'
	fail_if_ppuread_y_not 'U'
	fail_if_ppuread_y_not 'P'


	lda #0 ; 1-byte increments
	sta PPUCTRL


	begin_test TEST_CHRROM_READ_BANKED
	set_vrom_page 2
	set_ppuaddr $1F23

	ldy PPUDATA
	; This data is manually verified to match gfx.spr.page1.chr. It has no special meaning to it.
	fail_if_ppuread_y_not 'D'
	fail_if_ppuread_y_not 'U'
	fail_if_ppuread_y_not 'E'


	begin_test TEST_CHRROM_READ_BANKED_BUFFER
	set_ppuaddr $0FF0
	lda PPUDATA	; Puts 'S' in buffer from $0FF0
	set_vrom_page 0 ; In this VROM page, $0FF0 does not contain 'S'
	fail_if_ppuread_y_not 'S'

	rts


	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_CHRROM_WRITE
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "CHR ROM on  mapper 3  (CNROM  boards) must not be writable",0	;DO_DTE
.popseg
test_chr_rom_write:
	begin_test TEST_CHRROM_WRITE
	jsr wait_vbl_optional

	ldx #$03 ; $0300
	ldy #$00
	lda #$3E ; eor with $3E
	jsr @test_write
	ldx #$1C ; $1C3B
	ldy #$3B
	lda #$F2 ; eor with $F2
	jsr @test_write
	ldx #$07 ; $0777
	ldy #$77
	lda #$11 ; eor with $11
	;jmp @test_write
@test_write:
	sta temp2
	; Load whatever was in that address
	jsr @read
	sta temp
	; Modify the byte
	eor temp2
	jsr @setaddr
	sta PPUDATA
	; Load the byte again
	jsr @read
	cmp temp
	jmp fail_current_test_ifNE ; jsr + rts.
@setaddr:
	stx PPUADDR
	sty PPUADDR
	rts
@read:
	jsr @setaddr
	lda PPUDATA
	lda PPUDATA
	rts
	;;;;;;;;;;;;;;;;;;

.pushseg
	MakeTest TEST_BUFFER_DELAY_BLANK_1FRAME
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "The PPU read buffer should",newline	;DO_DTE
	.byte "survive 1 frame of idle",newline	;DO_DTE
	.byte "with rendering disabled.",0	;DO_DTE

	MakeTest TEST_BUFFER_DELAY_BLANK_2SECONDS
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "The PPU read buffer should",newline	;DO_DTE
	.byte "survive 2 seconds of idle",newline	;DO_DTE
	.byte "with rendering disabled.",0	;DO_DTE

	MakeTest TEST_BUFFER_DELAY_INTERNAL
	.byte "Unexpected: VROM contents at",newline	;DO_DTE
	.byte "$1Bxx did not match what was",newline	;DO_DTE
	.byte "hardcoded into the program.",0	;DO_DTE

	MakeTest TEST_BUFFER_DELAY_VISIBLE_1FRAME
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "The PPU read buffer should",newline	;DO_DTE
	.byte "survive 1 frame",$FF	;DO_DTE
	.word BUFFER_DELAY_VISIBLE_COMMON

	MakeTest TEST_BUFFER_DELAY_VISIBLE_1SECOND
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "The PPU read buffer should",newline	;DO_DTE
	.byte "survive 1 second",$FF	;DO_DTE
	.word BUFFER_DELAY_VISIBLE_COMMON

	MakeTest TEST_BUFFER_DELAY_VISIBLE_3SECONDS
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "The PPU read buffer should",newline	;DO_DTE
	.byte "survive 3 seconds",$FF	;DO_DTE
	.word BUFFER_DELAY_VISIBLE_COMMON

	MakeTest TEST_BUFFER_DELAY_VISIBLE_7SECONDS
	;      0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	.byte "The PPU read buffer should",newline	;DO_DTE
	.byte "survive 7 seconds"	;DO_DTE
	BUFFER_DELAY_VISIBLE_COMMON:
	.byte                  " of idle",newline	;DO_DTE
	.byte "with rendering enabled.",0	;DO_DTE

.segment "RODATA"
	test_read_buffer_fade_visible_delay_schedule:
		.byte TEST_BUFFER_DELAY_VISIBLE_1FRAME ; Which test
		.byte $DB	; Which data we expect
		.word $1B43	; Which address
		.word 1	; How many frames to sleep
		.byte TEST_BUFFER_DELAY_VISIBLE_1SECOND ; Which test
		.byte $68	; Which data we expect
		.word $1BE9	; Which address
		.word 60	; How many frames to sleep
		.byte TEST_BUFFER_DELAY_VISIBLE_3SECONDS ; Which test
		.byte $DB	; Which data we expect
		.word $1B43	; Which address
		.word 60*3	; How many frames to sleep
		.byte TEST_BUFFER_DELAY_VISIBLE_7SECONDS ; Which test
		.byte $68	; Which data we expect
		.word $1BE9	; Which address
		.word 60*7+1	; How many frames to sleep
		;All tests done
		.byte $00
.popseg


zp_byte beep_param
pre_beep_dual:
	setb beep_param, $93	; tempered volume, no envelope
	ldxa #($200 + 10*$800)	; period = $200, length=10
	ldy #4
	jsr beep
pre_beep:
	setb beep_param, $95	; tempered volume, no envelope
	ldxa #($142 + 10*$800)	; period = $142, length=10
	bne beep0
long_beep:
	setb beep_param, $8D	; env decay rate 13; not disabled, not looped; length counter enabled
	ldxa #($1C8 + 1*$800)	; period = $1C8, length=1
	bne beep0
final_beep:
	setb beep_param, $84	; env decay rate 4; not disabled, not looped; length counter enabled
	ldxa #($23F + 10*$800)	; period = $23F, length=10
beep0:	ldy #0
	;jmp beep
beep:	sta $4000+2,y
	lda beep_param
	sta $4000+0,y
	lda #0
	sta $4000+1,y ; no sweep
	txa
	sta $4000+3,y
	rts
pre_beep_length:
	jsr pre_beep
	delay_msec 110
	lda #$93	; almost silent
	sta $4000+0
	delay_msec 20
	rts


	;;;;;;;;;;;;;;;;;;


;wait_spr0hit:
;	; Wait for sprite0-hit flag
;:	bit PPUSTATUS
;	bvc :-
;	rts




my_restore_console:
	lda #13
	jsr write_text_out_raw ; put CR
	jmp restore_console


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


.include "testing2-end.s"

;;;;;;;;;;;;;;;;;;;;;
.include "graphics.s"
;;;;;;;;;;;;;;;;;;;;;


.segment "FILLER"
	.incbin "filler.bin"
