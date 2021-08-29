; All read-modify-write-instructions on 6502 have a particular glitch:
; They first write back the unmodified value; then, the modified value.
;
; This test requires that the PPU open bus behavior is accurately emulated.
; Cycle accuracy will not be tested.
;
;
; Expected output:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEST: cpu_dummy_writes_ppumem
; This program verifies that the
; CPU does 2x writes properly.
; Any read-modify-write opcode
; should first write the origi-
; nal value; then the calculated
; value exactly 1 cycle later.
; 
; Verifying open bus behavior.
;       W- W- WR W- W- W- W- WR
; 2000+ 0  1  2  3  4  5  6  7 
;   R0: 0- 0- 00 0- 0- 0- 0- 00
;   R1: 0- 0- 00 0- 0- 0- 0- 00
;   R3: 0- 0- 00 0- 0- 0- 0- 00
;   R5: 0- 0- 00 0- 0- 0- 0- 00
;   R6: 0- 0- 00 0- 0- 0- 0- 00
; OK; Verifying opcodes...
; 0E2E4E6ECEEE 1E3E5E7EDEFE 
; 0F2F4F6FCFEF 1F3F5F7FDFFF 
; 03234363C3E3 13335373D3F3 
; 1B3B5B7BDBFB              
; 
; Passed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Written by Joel Yliluoma - http://iki.fi/bisqwit/

.segment "LIB"
.include "shell.inc"
.include "colors.inc"
.include "crc_fast.s"
.segment "CODE"

;OFFICIAL_ONLY = 0

zp_res num_fails
zp_res num_fails_2, 2
zp_res open_bus_test_failed
zp_res opcode_ptr, 2
zp_res opcode_buffer, 8
zp_res temp_read_result
zp_res temp_ppu_ptr, 2
zp_res temp_open_bus
zp_res temp_x
zp_res x_position
zp_res ptr_2006, 2


.macro fail_if_y_not n, fail_label
	cpy #n
	bne fail_label
.endmacro


; Test open bus feature
;   puts A into $2000+X
;   reads  from $2000+Y
;   expects A
open_bus_test:
	sta $2000,x
	cmp $2000,y
	bne test_failed_finish
	rts

.macro do_open_bus_test test_no,name,poke_value,a_index,b_index
	set_test test_no,name
	lda #poke_value
	ldx #a_index
	ldy #b_index
	jsr open_bus_test
.endmacro
.macro do_open_bus_test1 poke_value,a_index,b_index
	lda #poke_value
	ldx #a_index
	ldy #b_index
	jsr open_bus_test
.endmacro

; Set PPU addr to A*0x100+X
; Preserved: A,X; gobbled: Y
set_vram_pos:
	ldy PPUSTATUS
	sta PPUADDR ; poke high 6 bits
	stx PPUADDR ; poke low  8 bits
	rts

test_failed_finish:
	; Re-enable screen
	jsr clear_vram_garbage
	jsr console_show
	jmp test_failed

main:   
	; Operations we will be doing are:
	;
	;  $2400->$27FF is nicely usable (secondary name table).
	;
	;      INC    ->$2526
	;      DEC    ->$2524
	;
	;      ASL    ->$254A
	;      LSR    ->$2512
	;      ROR    ->$2512 or $2592
	;      ROL    ->$254A or $254B
	;
	;      SLO    ->$252A
	;      SRE    ->$2512
	;      RRA    ->$2512 or $2592
	;      RLA    ->$254A or $254B
	;
	lda #0
	sta open_bus_test_failed

	jsr intro
	jsr init_random

	jsr populate_vram_slow
	; leaves display hidden

	; First, verify that PPU memory I/O works as expected
	;
	jsr test_one_byte_buffer
	jsr test_2005_writes
	jsr test_sequential_memory_read
	jsr test_sequential_memory_write
	jsr console_show

	jsr test_rom_write

	jsr test_open_bus_behavior
	
	; Then do the actual test!
	;
	jsr populate_vram_slow
	lda #$00
	sta $2000
	jsr test_dummy_writes
	;
	lda num_fails_2
	ora num_fails_2+1
	beq :+

	jsr wait_vbl
	text_color1
	ldx num_fails_2
	lda num_fails_2+1
	jsr print_dec16
	print_str " OPCODES FAILED THE TEST"
	text_white
	jmp test_failed_finish
	
:	; Now, when the dummy-write tests have been done (and passed),
	; verify whether we failed the open bus test earlier.
	; If it failed, do not give a clean exit status.
	lda open_bus_test_failed
	beq end_tests
	jsr wait_vbl
	print_str "Opcodes worked fine, but..."
	set_test 10,"Open bus behavior is wrong."
	jmp test_failed_finish

end_tests:
	jsr clear_vram_garbage
	jsr console_show
	jsr wait_vbl
	jmp tests_passed

.pushseg
 .segment "RODATA"
Opcodes:
	; Opcodes to test:
        ; * 0E (RMW) ASL abs    U 0F (RMW) SLO abs  (ASL+ORA)
        ; * 2E (RMW) ROL abs    U 2F (RMW) RLA abs  (ROL+AND)
        ; * 4E (RMW) LSR abs    U 4F (RMW) SRE abs  (LSR+EOR)
        ; * 6E (RMW) ROR abs    U 6F (RMW) RRA abs  (ROR+ADC)
        ; * CE (RMW) DEC abs    U CF (RMW) DCP abs  (DEC+CMP)
        ; * EE (RMW) INC abs    U EF (RMW) ISB abs  (INC+SBC)
        ;
        ; * 1E (RMW) ASL absx   U 1F (RMW) SLO absx  (ASL+ORA)  U 1B (RMW) SLO absy  (ASL+ORA)
        ; * 3E (RMW) ROL absx   U 3F (RMW) RLA absx  (ROL+AND)  U 3B (RMW) RLA absy  (ROL+AND)
        ; * 5E (RMW) LSR absx   U 5F (RMW) SRE absx  (LSR+EOR)  U 5B (RMW) SRE absy  (LSR+EOR)
        ; * 7E (RMW) ROR absx   U 7F (RMW) RRA absx  (ROR+ADC)  U 7B (RMW) RRA absy  (ROR+ADC)
        ; * DE (RMW) DEC absx   U DF (RMW) DCP absx  (DEC+CMP)  U DB (RMW) DCP absy  (DEC+CMP)
        ; * FE (RMW) INC absx   U FF (RMW) ISB absx  (INC+SBC)  U FB (RMW) ISB absy  (INC+SBC)
        ;
        ; K 12 (RMW) ASL ix     U 03 (RMW) SLO ix   (ASL+ORA)  U 13 (RMW) SLO iy  (ASL+ORA)
        ; K 32 (RMW) ROL ix     U 23 (RMW) RLA ix   (ROL+AND)  U 33 (RMW) RLA iy  (ROL+AND)
        ; K 52 (RMW) LSR ix     U 43 (RMW) SRE ix   (LSR+EOR)  U 53 (RMW) SRE iy  (LSR+EOR)
        ; K 72 (RMW) ROR ix     U 63 (RMW) RRA ix   (ROR+ADC)  U 73 (RMW) RRA iy  (ROR+ADC)
        ; K D2 (RMW) DEC ix     U C3 (RMW) DCP ix   (DEC+CMP)  U D3 (RMW) DCP iy  (DEC+CMP)
        ; K F2 (RMW) INC ix     U E3 (RMW) ISB ix   (INC+SBC)  U F3 (RMW) ISB iy  (INC+SBC)
        ;

	; Format: Opcode number ($00 for end, $01 for newline, $02 for space)
	;         Value for absolute address (appended to opcode) (BIG-ENDIAN)
	;         Value for X
	;         Value for PPU address (BIG_ENDIAN) -- this is used in order to avoid false positives.
	;         Value for open bus ($2005) -- this is the expected first byte
	;         Expected 2nd reads from $2007 with Carry Set, Carry Clear (two bytes)
	; Each opcode will be tried twice. Once with carry clear, once with carry set

        ;     opcode  ABS       X    PPUADDR  OpenBus   READ1  READ2
        ; OFFICIAL OPCODES
        .byte $0E,    $20,$06,  $00, $25,$FA, $25,      $CA,   $CA       ; ASL,254A
        .byte $2E,    $20,$06,  $00, $25,$FA, $25,      $CA,   $CB       ; ROL,254A or 254B
        .byte $4E,    $20,$06,  $00, $25,$FA, $25,      $92,   $92       ; LSR,2512
        .byte $6E,    $20,$06,  $00, $25,$FA, $25,      $92,   $12       ; ROR,2512 or $2592
        .byte $CE,    $20,$06,  $00, $25,$FA, $25,      $A4,   $A4       ; DEC,2524
	.byte $EE,    $20,$06,  $00, $25,$FA, $25,      $A6,   $A6       ; INC,2526 ---Expect: reads open bus, gets $25   (Vaddr = $25FA)
	;                                                                                      writes $2006     <-  $25   (taddr = $2525)
	;                                                                                      writes $2006     <-  $26   (Vaddr = $2526)
	;                                                                                      2*read $2007                               -> $A6
	.byte $02
	;
        .byte $1E,    $20,$05,  $01, $25,$FA, $26,      $0C,   $0C       ; ASL,$264C
        .byte $3E,    $20,$01,  $05, $25,$FA, $26,      $0C,   $0D       ; ROL,$264C or $264D
        .byte $5E,    $20,$04,  $02, $25,$FA, $26,      $D3,   $D3       ; LSR,$2613
        .byte $7E,    $20,$02,  $04, $25,$FA, $26,      $D3,   $53       ; ROR,$2613 or $2693
        .byte $DE,    $20,$04,  $02, $25,$FA, $26,      $E5,   $E5       ; DEC,$2625
	.byte $FE,    $20,$03,  $03, $25,$FA, $26,      $E7,   $E7       ; INC,$2627
	;
	.byte $01
	;
        ; UNOFFICIAL OPCODES
	;
        .byte $0F,    $20,$06,  $00, $25,$FA, $25,      $CA,   $CA       ; SLO,254A
        .byte $2F,    $20,$06,  $00, $25,$FA, $25,      $CA,   $CB       ; RLA,254A or 254B
        .byte $4F,    $20,$06,  $00, $25,$FA, $25,      $92,   $92       ; SRE,2512
        .byte $6F,    $20,$06,  $00, $25,$FA, $25,      $92,   $12       ; RRA,2512 or $2592
        .byte $CF,    $20,$06,  $00, $25,$FA, $25,      $A4,   $A4       ; DCP,2524
	.byte $EF,    $20,$06,  $00, $25,$FA, $25,      $A6,   $A6       ; ISB,2526
	;
	.byte $02
	;
        .byte $1F,    $20,$05,  $01, $25,$FA, $26,      $0C,   $0C       ; SLO,$264C
        .byte $3F,    $20,$01,  $05, $25,$FA, $26,      $0C,   $0D       ; RLA,$264C or $264D
        .byte $5F,    $20,$04,  $02, $25,$FA, $26,      $D3,   $D3       ; SRE,$2613
        .byte $7F,    $20,$02,  $04, $25,$FA, $26,      $D3,   $53       ; RRA,$2613 or $2693
        .byte $DF,    $20,$04,  $02, $25,$FA, $26,      $E5,   $E5       ; DCP,$2625
	.byte $FF,    $20,$03,  $03, $25,$FA, $26,      $E7,   $E7       ; ISB,$2627
	;
	.byte $01
	;
        ; UNOFFICIAL OPCODES
	;
        .byte $03,    $EA,<ptr_2006,  $00, $25,$FA, $25,      $CA,   $CA       ; SLO,254A
        .byte $23,    $EA,<ptr_2006,  $00, $25,$FA, $25,      $CA,   $CB       ; RLA,254A or 254B
        .byte $43,    $EA,<ptr_2006,  $00, $25,$FA, $25,      $92,   $92       ; SRE,2512
        .byte $63,    $EA,<ptr_2006,  $00, $25,$FA, $25,      $92,   $12       ; RRA,2512 or $2592
        .byte $C3,    $EA,<ptr_2006,  $00, $25,$FA, $25,      $A4,   $A4       ; DCP,2524
	.byte $E3,    $EA,<ptr_2006,  $00, $25,$FA, $25,      $A6,   $A6       ; ISB,2526
	;
	.byte $02
	;
        .byte $13,    $EA,<ptr_2006,  $00, $25,$FA, $26,      $0C,   $0C       ; SLO,$264C
        .byte $33,    $EA,<ptr_2006,  $00, $25,$FA, $26,      $0C,   $0D       ; RLA,$264C or $264D
        .byte $53,    $EA,<ptr_2006,  $00, $25,$FA, $26,      $D3,   $D3       ; SRE,$2613
        .byte $73,    $EA,<ptr_2006,  $00, $25,$FA, $26,      $D3,   $53       ; RRA,$2613 or $2693
        .byte $D3,    $EA,<ptr_2006,  $00, $25,$FA, $26,      $E5,   $E5       ; DCP,$2625
	.byte $F3,    $EA,<ptr_2006,  $00, $25,$FA, $26,      $E7,   $E7       ; ISB,$2627
	;
	.byte $01
	;
        ; UNOFFICIAL OPCODES
	;
        .byte $1B,    $20,$06,  $00, $25,$FA, $25,      $CA,   $CA       ; SLO,254A
        .byte $3B,    $20,$06,  $00, $25,$FA, $25,      $CA,   $CB       ; RLA,254A or 254B
        .byte $5B,    $20,$06,  $00, $25,$FA, $25,      $92,   $92       ; SRE,2512
        .byte $7B,    $20,$06,  $00, $25,$FA, $25,      $92,   $12       ; RRA,2512 or $2592
        .byte $DB,    $20,$06,  $00, $25,$FA, $25,      $A4,   $A4       ; DCP,2524
	.byte $FB,    $20,$06,  $00, $25,$FA, $25,      $A6,   $A6       ; ISB,2526
	;
	.byte $01
	;
	.byte $00

.popseg

test_dummy_writes:
	jsr console_show

	set_test 9,"Some opcodes failed the test."
	;        0123456789ABCDEF0123456789ABC|
        ;        0E2E4E6ECEEE 1E3E5E7EDEFE *00
        ;        0F2F4F6FCFEF 1F3F5F7FDFFF *00
        ;        03234363C3E3 13335373D3F3 *00
        ;        1B3B5B7BDBFB              *00
        ;
        text_white
        print_ext_str "Verifying opcodes...",newline
        setw opcode_ptr, Opcodes-1
        
 	lda #$60 ; "rts"
 	sta opcode_buffer+3
 	sta opcode_buffer+7
 	
 	lda #$F0
 	sta opcode_buffer+5
 	lda #$EA
 	sta opcode_buffer+6
 	
 	setw ptr_2006, $2006

        ldx #0 ; X coordinate
        stx num_fails
	stx num_fails_2
	stx num_fails_2+1
	
@opcode_loop:
	jsr next_opcode_byte
	beq @opcode_loop_end
	cmp #2
	beq @make_space
	bcc @make_newline

	sta opcode_buffer+0
	sta opcode_buffer+4
	jsr next_opcode_byte ; absolute address, hi byte
	sta opcode_buffer+2
	jsr next_opcode_byte ; absolute address, lo byte
	sta opcode_buffer+1
	jsr next_opcode_byte ; X
	sta temp_x
	jsr next_opcode_byte ; PPU address, hi
	sta temp_ppu_ptr+0
	jsr next_opcode_byte ; PPU address; lo
	sta temp_ppu_ptr+1
	jsr next_opcode_byte ; Value for open bus
	sta temp_open_bus

	stx x_position
	lda #0
	sta temp_read_result

	clc
	jsr @opcode_test_once
	sec
	jsr @opcode_test_once
	lda temp_read_result

	beq @didnt_fail
	; Did fail
	inc num_fails
	text_white
	lda opcode_buffer+0
	jsr print_hex
	bne @opcode_next

@opcode_loop_end:
	text_white
	rts

@make_newline:
	jmp @make_newline_s

@didnt_fail:
	text_color2
	lda opcode_buffer+0
	jsr print_hex
@opcode_next:
	ldx x_position
	inx
	inx
	bne @opcode_loop

@make_space:
	lda #' '
	jsr print_char
	inx
	jmp @opcode_loop

@make_newline_space_loop:
	lda #' '
	jsr print_char
	inx
@make_newline_s:
	cpx #26
	bcc @make_newline_space_loop
	; At end of line
	lda num_fails
	beq @no_fails_this_line
	clc
	adc num_fails_2
	sta num_fails_2
	lda #0
	adc num_fails_2+1
	sta num_fails_2+1
	lda num_fails
	pha
	 text_white
	 lda #'*'
	 jsr print_char
	pla
	jsr print_dec
@no_fails_this_line:
	jsr print_newline
	ldx #0
	stx num_fails
	jmp @opcode_loop

@opcode_test_once:
	; First, dry-execute the opcode just in case it crashes
	; the emulator (so the screen is left readable)
	php
	 ldy #0
	 jsr opcode_buffer+4
	 jsr console_hide
	plp

	; Then execute the actual opcode
	lda PPUSTATUS       ; reset low/high counter
	lda temp_ppu_ptr+0
	sta PPUADDR
	lda temp_ppu_ptr+1
	sta PPUADDR
	lda temp_open_bus
	sta $2005           ; writes to $2005 serve to put data into open bus
	sta $2005           ; writes twice, so the address toggle is not affected
	ldx temp_x
	ldy #0
	jsr opcode_buffer   ; Execute opcode

	lda PPUDATA         ; ignore this byte
	jsr next_opcode_byte
	eor PPUDATA
	ora temp_read_result
	sta temp_read_result

	jsr console_show_nowait
	jsr console_flush
	rts

next_opcode_byte:
	incw opcode_ptr
	ldy #0
	lda (opcode_ptr),y
	rts

test_one_byte_buffer:
	set_test 2,"Non-palette PPU memory reads  should have one-byte buffer"

	lda #$25
	ldx #$30
	jsr set_vram_pos
	ldy PPUDATA  ; $B0 should be placed in read-buffer
	ldx #$55
	jsr set_vram_pos
	ldy PPUDATA  ; $B0 should be produced; $D5 should be placed in read-buffer
	fail_if_y_not $B0, test_failed_finish2
	ldy PPUDATA  ; $D5 should be produced; $D6 should be placed in read-buffer
	fail_if_y_not $D5, test_failed_finish2
	rts

test_failed_finish2: ; Aux label to help overcome short branch length limitation.
	jmp test_failed_finish

test_2005_writes:
	;           0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	set_test 3,"A single write to $2005  must not change the address  used  by $2007  when  vblank is on."

	lda #$25
	ldx #$4B
	jsr set_vram_pos
	lda #$55
	sta $2005 ; set scrolling position
	; read from PPU memory
	lda $2007 ; discard first read
	ldy $2007 ; should be x ^ $80
	fail_if_y_not $CB, test_failed_finish2
	
	set_test 4,"Even two writes to $2005 must not change the address  used  by $2007  when  vblank is on."

	lda #$25
	ldx #$6A
	jsr set_vram_pos
	lda #$55
	sta $2005 ; set scrolling position
	lda #$8C
	sta $2005
	;
	; read from PPU memory
	lda $2007 ; discard first read
	ldy $2007 ; should be x ^ $80
	fail_if_y_not $EA, test_failed_finish2

	;           0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	set_test 5,"A single write to $2006  must not change the address  used  by $2007  when  vblank is on."

	lda #$25
	ldx #$93
	jsr set_vram_pos
	lda #$55
	sta $2006 ; set half of the address
	; read from PPU memory
	lda $2007 ; discard first read
	ldy $2007 ; should be x
	fail_if_y_not $13, test_failed_finish3

	;           0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	set_test 6,"A single write  to $2005 must change the address toggle for both $2005 and $2006."

	lda #$25
	ldx #$93
	jsr set_vram_pos ; taddr = $2593 (x fine = whatever)
	lda #$F0
	sta $2005 ; set half of the address (should set the toggle) -- taddr = $25DE
	; the next byte should set the toggle for also the VRAM
	lda #$DE ; alas, this completely overrides what was written to $2005.
	sta $2006
	; read from PPU memory
	lda $2007 ; discard first read
	ldy $2007 ; should be $DE ^ $80
	fail_if_y_not $5E, test_failed_finish3

	rts

test_failed_finish3: ; Aux label to help overcome short branch length limitation.
	jmp test_failed_finish


test_sequential_memory_read:
	set_test 7,"Sequential PPU memory read does not work"

	lda #$25
	ldx #$55
	jsr set_vram_pos
	ldy PPUDATA  ; something should be produced; $D5 should be placed in read-buffer
	lda #$25
	ldx #$C2
	jsr set_vram_pos
	ldy PPUDATA   ; should produce $D5 (from read buffer); $42 will be placed in read buffer
	fail_if_y_not $D5, test_failed_finish3
	ldy PPUDATA
	fail_if_y_not $42, test_failed_finish3
	ldy PPUDATA
	fail_if_y_not $43, test_failed_finish3
	ldy PPUDATA
	fail_if_y_not $44, test_failed_finish3
	ldy PPUDATA
	fail_if_y_not $45, test_failed_finish3
	rts



test_sequential_memory_write:
	set_test 8,"Sequential PPU memory write does not work"
	lda #$25
	ldx #$33
	jsr set_vram_pos
	lda #$40
	sta PPUDATA
	lda #$41
	sta PPUDATA
	lda #$42
	sta PPUDATA
	lda #$43
	sta PPUDATA
	lda #$44
	sta PPUDATA
	lda #$25
	ldx #$33
	jsr set_vram_pos
	ldy PPUDATA ; discard
	ldy PPUDATA
	fail_if_y_not $40, test_failed_finish4
	ldy PPUDATA
	fail_if_y_not $41, test_failed_finish4
	ldy PPUDATA
	fail_if_y_not $42, test_failed_finish4
	ldy PPUDATA
	fail_if_y_not $43, test_failed_finish4
	ldy PPUDATA
	fail_if_y_not $44, test_failed_finish4
	rts

test_failed_finish4: ; Aux label to help overcome short branch length limitation.
	jmp test_failed_finish


test_open_bus_behavior:
	; Extensively test open bus.
	;    $2000 is write only (writing updates open_bus, reading returns open_bus)
	;    $2001 is write only (writing updates open_bus, reading returns open_bus)
	;    $2002 is read only  (writing updates open_bus, reading UPDATES open_bus (but only for low 5 bits))
	;    $2003 is write only (writing updates open_bus, reading returns open_bus)
	;    $2004 is read-write (writing updates open_bus, however for %4==2, bitmask=11100011. Reading is UNRELIABLE.)
	;    $2005 is write only (writing updates open_bus, reading returns open_bus)
	;    $2006 is write only (writing updates open_bus, reading returns open_bus)
	;    $2007 is read-write (writing updates open_bus, reading UPDATES open_bus)
	;
	;    0123456789ABCDEF0123456789ABC|
	;          W- W- WR W- W- W- W- WR
	;          00 01 02 03 04 05 06 07
	;    R 00
	;    R 01
	;    R 03
	;    R 05
	;    R 06

	lda #0
	sta num_fails_2
	sta num_fails_2+1
	
	jsr console_show

	print_ext_str "Verifying open bus behavior.",newline
	text_color1
	print_ext_str "      W- W- WR W- W- W- W- WR",newline
	print_ext_str "2000+ 0  1  2  3  4  5  6  7 ",newline

	jsr reset_crc
	ldx #0
	jsr open_bus_test_line
	ldx #1
	jsr open_bus_test_line
	ldx #3
	jsr open_bus_test_line
	ldx #5
	jsr open_bus_test_line
	ldx #6
	jsr open_bus_test_line

	text_white
	is_crc $86FCF77E
	bne :+
	;             0123456789ABCDEF0123456789ABC|
	;print_ext_str "Open bus behavior OK.",newline
	print_ext_str "OK; "
	rts
:	jsr crc_off
	print_ext_str "FAIL! "
	ldx num_fails_2
	lda num_fails_2+1
	jsr print_dec16
	print_ext_str " ERRORS. CRC="
	jsr print_crc
	print_ext_str ".",newline

	; We will continue tests, but note for later use that
	; this test failed.
	lda #1
	sta open_bus_test_failed
	jmp crc_on


.pushseg
.segment "RODATA"
	; When writing or reading $2000+x, which bits of the result are shown in open bus?
open_bus_read_masks:
	.byte $FF,$FF,$1F,$FF, $FF,$FF,$FF,$FF

	; When writing to this port, which bits can we modify?
open_bus_write_and:
	.byte $FF&~$97,$FF&~$18,$FF,$FF, $7F,$7F,$FF,$FF
	;.byte $00,$00,$00,$00, $00,$00,$00,$00
open_bus_write_or:
	.byte $00,$00,$00,$00, $00,$00,$00,$00
.popseg


print_open_bus_fails:
	lda num_fails
	beq :+
	text_white ; nonzero value = fail, color = white
:	jsr update_crc
	jsr print_hex_nibble
	text_color2
	lda num_fails
	clc
	adc num_fails_2
	sta num_fails_2
	lda #0
	adc num_fails_2+1
	sta num_fails_2+1
	rts

.macro open_bus_write_test port_index
	.local loop
	; Should write to  $2000+port_index,
	; and test whether $2000+X gives out the same value when read.

	; Do several writes and count the number of failures.
.if port_index = 7
	lda #$24
	ldx #$00
	jsr set_vram_pos
.endif
	lda #' '
	jsr print_char
	lda #0
	sta num_fails
	ldy #15
loop:	 jsr next_random
	 and open_bus_write_and+port_index
	 ora open_bus_write_and+port_index
	 sta $2000+port_index
	 eor $2000,x
	 and open_bus_read_masks+port_index
	 beq :+
	 inc num_fails
:	dey
	bne loop
	jsr print_open_bus_fails
.endmacro

.macro open_bus_read_test  port_index
	.local loop
	; Should read from $2000+port_index,
	; and test whether $2000+X gives out the same value when read.
	lda #0
	sta num_fails
	ldy #15
loop:
	 lda $2000+port_index
	 eor $2000,x
	 and open_bus_read_masks+port_index
	 beq :+
	 inc num_fails
:	dey
	bne loop
	jsr print_open_bus_fails
.endmacro

.macro open_bus_skip_test
	; skipped in blue (default)
	lda #'-'
	jsr print_char
.endmacro
	

open_bus_test_line:
	; Test writing into each of $2000..$2007, and read from $2000+X.
	 text_color1
	 ldy #' '
	 tya
	 jsr print_char
	 jsr print_char
	 lda #'R'
	 jsr print_char
	 txa
	 jsr print_hex_nibble
	 lda #':'
	 jsr print_char
	 text_color2

	 jsr console_hide
	 ; Begin testing
	 ; Write to $2000, read from $2000+X
	 open_bus_write_test 0
	 open_bus_skip_test
	 open_bus_write_test 1
	 open_bus_skip_test
	 open_bus_write_test 2
	 open_bus_read_test  2
	 open_bus_write_test 3
	 open_bus_skip_test
	 open_bus_write_test 4
	 open_bus_skip_test
	 open_bus_write_test 5
	 open_bus_skip_test
	 open_bus_write_test 6
	 open_bus_skip_test
	 open_bus_write_test 7
	 open_bus_read_test  7

	; end test: clear garbage
	jsr console_show
	
	; redisplay screen, and print the line of results
	jsr print_newline
	rts

	.pushseg
	.segment "RODATA"
intro:	text_white
	print_str "TEST: cpu_dummy_writes_ppumem",newline
	text_color1
	jsr print_str_
	;       0123456789ABCDEF0123456789ABCD
	 .byte "This program verifies that the",newline
	 .byte "CPU does 2x writes properly.",newline
	 .byte "Any read-modify-write opcode",newline
	 .byte "should first write the origi-",newline
	 .byte "nal value; then the calculated",newline
	 .byte "value exactly 1 cycle later.",newline
	 .byte newline,0
	text_white
	rts
	.popseg

init_random:
	jsr init_crc_fast
	lda #1
	jsr update_crc_fast
	rts

next_random:
	lda #$55
	jmp update_crc_fast



populate_vram_slow:
	; Disable rendering
	jsr console_hide
	; Populate the $2500..$25FF with bytes: $80..$7F
	; These updates are done very slowly, in order to
	; avoid bugs with those PPUs that do not automatically
	; update the taddr when $2007 is written to.
	ldx #$00
:	lda #$25
	jsr set_vram_pos
	 txa
	 eor #$80
	 sta PPUDATA
	inx
	bne :-
	; Then, populate $2400..$24FF with $40..$BF
	; And        the $2600..$26FF with $C0..$3F
	; And        the $2700..$2BFF with $00..$FF
	; Since these will be used later in the tests,
	; they can be overwritten quicker.
	; 2400..24FF
	lda #$24
	ldx #$00
	jsr set_vram_pos
:	 txa
	 clc
	 adc #$40
	 sta PPUDATA
	inx
	bne :-
	; 2600..26FF
	lda #$26
	jsr set_vram_pos
:	 txa
	 clc
	 adc #$C0
	 sta PPUDATA
	inx
	bne :-
	; 2700..27FF
	lda #$27
	jsr set_vram_pos
:	stx PPUDATA
	inx
	bne :-
	rts

clear_vram_garbage:
	; Disable rendering
	jsr console_hide
;clear_vram_garbage_nohide:
	lda #0
	sta $2000
	sta $2001
	ldy $2002
	ldx #$00
	lda #$24
	jsr set_vram_pos
	ldy #$04
	lda #0
:	sta PPUDATA
	inx
	bne :- ; do 256 times
	dey
	bne :- ; do 1 times
	; pass through to reset_scrolling
;reset_scrolling:
	; reset scrolling position and vram write position
	ldy $2002
	;jsr console_show
	;
	ldx #0
	stx $2005
	lda console_scroll
	sta $2005
	txa
	jmp set_vram_pos
	rts

test_rom_write:
	set_test 11,"ROM should not be writable."
	lda #1
	sta test_return_0+1
	jsr test_return_0
	cmp #0
	bne :+
	rts
:	jmp test_failed_finish

test_return_0:
	lda #0
	rts
