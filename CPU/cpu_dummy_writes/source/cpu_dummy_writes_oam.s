; All read-modify-write-instructions on 6502 have a particular glitch:
; They first write back the unmodified value; then, the modified value.
; This test uses the PPU OAM write ($2004) autoincrement feature to
; verify that the CPU does that.
;
; Expected output:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEST: cpu_dummy_writes_oam
; This program verifies that the
; CPU does 2x writes properly.
; Any read-modify-write opcode
; should first write the origi-
; nal value; then the calculated
; value exactly 1 cycle later.
; 
; Requirement: OAM memory reads
; MUST be reliable. This is
; often the case on emulators,
; but NOT on the real NES.
; Nevertheless, this test can be
; used to see if the CPU in the
; emulator is built properly.
; 
; Testing OAM.  The screen will go blank for a moment now.
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

zp_res testflag02
zp_res pointer, 2
zp_res num_failed_oam_reads, 2
zp_res oam_read_test_failed

zp_res opcode_ptr, 2
zp_res opcode_buffer, 8
zp_res temp_x
zp_res temp_oam_data, 3
zp_res temp_read_result

zp_res x_position
zp_res ptr_2004, 2
zp_res num_fails
zp_res num_fails_2, 2


.macro test_at where, value
	setb SPRADDR,where
	lda SPRDATA
	cmp #value
.endmacro

test_failed_finish:
	; Re-enable screen
	jsr console_show
	jmp test_failed

main:   jsr intro

	lda #0
	sta oam_read_test_failed

	jsr oam_read_test
	text_white
	lda num_failed_oam_reads
	ora num_failed_oam_reads+1
	bne @first_test_not_ok

	print_str "OK; "
	jmp @first_test_ok
@first_test_not_ok:
	text_color1
	print_str "FAIL. "
	ldx num_failed_oam_reads
	lda num_failed_oam_reads+1
	jsr print_dec16
	print_str " FAILED READS.",newline
	; We will continue tests, but note for later use that
	; this test failed.
	lda #1
	sta oam_read_test_failed

@first_test_ok:	
	jsr test_rom_write
	jsr test_oam_access_functions
	
	; Then do the actual test!
	;
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
	print_str " OPCODES FAILED THE TEST",newline
	text_white
	lda oam_read_test_failed
	bne @opcode_test_failed_because_oam_problems
	jmp test_failed_finish
@opcode_test_failed_because_oam_problems:
	print_str "Probably because"
	jmp test_2_failed

:	; Now, when the dummy-write tests have been done (and passed),
	; verify whether we failed the open bus test earlier.
	; If it failed, do not give a clean exit status.
	lda oam_read_test_failed
	beq end_tests
	jsr wait_vbl
	print_str "Opcodes worked fine, but..."
	set_test 6,"OAM reads are unreliable."
	jmp test_failed_finish

end_tests:
	jsr console_show
	jsr wait_vbl
	jmp tests_passed




test_oam_access_functions:
	set_test 3,"Writes to OAM should automatically increment SPRADDR"

	jsr spr_reset

	; Poke OAM in automatically increasing address
	delay 2
	setb SPRADDR,$00  ; redundant
	delay 2
	setb SPRDATA,$01  ; put $01 in OAM[0]
	setb SPRDATA,$02  ; put $02 in OAM[1]
	;                              OAM[2] is kept as $E3
	
	; Verify that the second access was properly recorded
	ldx #$01
	ldy #$02         ; read from OAM[0] and OAM[1]
	delay 2
	setb SPRADDR,$00
	delay 2
	lda SPRDATA
	sta testflag02
	cpx testflag02
	bne test_3_failed
	delay 2
	setb SPRADDR,$01
	delay 2
	lda SPRDATA
	sta testflag02
	cpy testflag02
	bne test_3_failed

	;
	; Verify that OAMaddr does not automatically increment
	delay 2
	lda SPRDATA      ; should read from OAM[1], not OAM[2]
	pha
	 set_test 4,"Reads from OAM should not automatically increment SPRADDR"
	pla
	cmp #$02         ; expected result is $02
	bne test_3_or_4_failed
	rts
test_3_or_4_failed:
	jmp test_failed_finish
test_3_failed:
	lda oam_read_test_failed
	beq test_3_or_4_failed
	
	;          0123456789ABCDEF0123456789ABC|
	text_white
	print_str "A test for writes to OAM and",newline
	print_str "automatic SPRADDR increments",newline
	print_str "failed, but the test probably",newline
	print_str "was not meaningful in the",newline
	print_str "first place, because"
test_2_failed:
	text_white
	set_test 2,"OAM reading is too unreliable."
	jmp test_failed

spr_reset:
	; Disable rendering
	jsr wait_vbl
	; Poke OAM in explicitly set addresses

	setb SPRADDR,$00
	ldx #$55
	delay 2
	stx SPRDATA
	delay 2
	setb SPRADDR,$01
	ldy #$B7
	delay 2
	sty SPRDATA
	delay 2
	setb SPRADDR,$02
	delay 2
	setb SPRDATA,$E3
	delay 2
	setb SPRADDR,$00
	delay 2
	rts



oam_read_test:
	text_white
	;          0123456789ABCDEF0123456789ABC|
	print_str "Testing OAM.  The screen will go blank for a moment now.",newline
	
	jsr init_crc_fast
	; This test copied verbatim from oam_read
	;
	; Disable rendering
	jsr wait_vbl
	setb PPUCTRL,0
	setb PPUMASK,0
	
	ldx #0
	ldy #15
	stx num_failed_oam_reads

@loop:  ; Set address and read from OAM
	jsr next_random
	tax
	stx SPRADDR
	
	jsr next_random
	and #$E3

	; Set address and write complement
	stx SPRADDR
	sta SPRDATA

	pha
	 ; Write 7 extra bytes, necessary
	 ; to trigger failure in some cases.
	 jsr next_random
	 and #$E3
	 sta SPRDATA
	 jsr next_random
	 and #$E3
	 sta SPRDATA
	 jsr next_random
	 and #$E3
	 sta SPRDATA
	 ; Write in some unrelated OAM index
	 txa
	 clc
	 adc #$C7
	 sta SPRADDR
	 jsr next_random
	 and #$E3
	 sta SPRDATA
	 jsr next_random
	 and #$E3
	 sta SPRDATA
	 jsr next_random
	 and #$E3
	 sta SPRDATA
	 jsr next_random
	 and #$E3
	 sta SPRDATA
	
	 ; Set address and log whether read
	 ; back was correct
	stx SPRADDR
	; Load something into open bus
	lda $2007
	pla
	eor SPRDATA
	beq :+
	inc num_failed_oam_reads
	bne :+
	inc num_failed_oam_reads+1

:	inx
	bne @loop
	dey
	bne @loop
	
	.pushseg
	.segment "RODATA"
intro:	text_white
	print_str "TEST: cpu_dummy_writes_oam",newline
	text_color1
	jsr print_str_
	;       0123456789ABCDEF0123456789ABCD
	 .byte "This program verifies that the",newline
	 .byte "CPU does 2x writes properly.",newline
	 .byte "Any read-modify-write opcode",newline
	 .byte "should first write the origi-",newline
	 .byte "nal value; then the calculated",newline
	 .byte "value exactly 1 cycle later.",newline
	 .byte newline
	 .byte "Requirement: OAM memory reads",newline
	 .byte "MUST be reliable. This is",newline
	 .byte "often the case on emulators,",newline
	 .byte "but NOT on the real NES.",newline
	 .byte "Nevertheless, this test can be",newline
	 .byte "used to see if the CPU in the",newline
	 .byte "emulator is built properly.",newline
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
	;         Values for OAM[0],OAM[1],OAM[2] before test 
	;         Expected OAM[1] after opcode; with Carry Set, Carry Clear (two bytes)
	;           OAM[0] and OAM[2] are expected to be unmodified (and thus tested).
	; Each opcode will be tried twice. Once with carry clear, once with carry set
        ;     opcode  ABS             X    OAM0,1,2       READ1 READ2
        ; OFFICIAL OPCODES
        .byte $0E,    $20,$04,        $00, $25,$B7,$E3,   $4A, $4A   ; ASL,254A
        .byte $2E,    $20,$04,        $00, $25,$B7,$E3,   $4A, $4B   ; ROL,254A or 254B
        .byte $4E,    $20,$04,        $00, $25,$B7,$E3,   $12, $12   ; LSR,2512
        .byte $6E,    $20,$04,        $00, $25,$B7,$E3,   $12, $92   ; ROR,2512 or $2592
        .byte $CE,    $20,$04,        $00, $25,$B7,$E3,   $24, $24   ; DEC,2524
	.byte $EE,    $20,$04,        $00, $25,$B7,$E3,   $26, $26   ; INC,2526
	.byte $02
	;
        .byte $1E,    $20,$03,        $01, $26,$B7,$E3,   $4C, $4C   ; ASL,$264C
        .byte $3E,    $20,$00,        $04, $26,$B7,$E3,   $4C, $4D   ; ROL,$264C or $264D
        .byte $5E,    $20,$04,        $00, $26,$B7,$E3,   $13, $13   ; LSR,$2613
        .byte $7E,    $20,$02,        $02, $26,$B7,$E3,   $13, $93   ; ROR,$2613 or $2693
        .byte $DE,    $20,$04,        $00, $26,$B7,$E3,   $25, $25   ; DEC,$2625
	.byte $FE,    $20,$03,        $01, $26,$B7,$E3,   $27, $27   ; INC,$2627         
	;
	.byte $01
	;
        ; UNOFFICIAL OPCODES
	;
        .byte $0F,    $20,$04,        $00, $25,$B7,$E3,   $4A, $4A   ; SLO,254A
        .byte $2F,    $20,$04,        $00, $25,$B7,$E3,   $4A, $4B   ; RLA,254A or 254B
        .byte $4F,    $20,$04,        $00, $25,$B7,$E3,   $12, $12   ; SRE,2512
        .byte $6F,    $20,$04,        $00, $25,$B7,$E3,   $12, $92   ; RRA,2512 or $2592
        .byte $CF,    $20,$04,        $00, $25,$B7,$E3,   $24, $24   ; DCP,2524
	.byte $EF,    $20,$04,        $00, $25,$B7,$E3,   $26, $26   ; ISB,2526
	;
	.byte $02
	;
        .byte $1F,    $20,$03,        $01, $26,$B7,$E3,   $4C, $4C   ; SLO,$264C
        .byte $3F,    $20,$00,        $04, $26,$B7,$E3,   $4C, $4D   ; RLA,$264C or $264D
        .byte $5F,    $20,$04,        $00, $26,$B7,$E3,   $13, $13   ; SRE,$2613
        .byte $7F,    $20,$02,        $02, $26,$B7,$E3,   $13, $93   ; RRA,$2613 or $2693
        .byte $DF,    $20,$04,        $00, $26,$B7,$E3,   $25, $25   ; DCP,$2625
	.byte $FF,    $20,$03,        $01, $26,$B7,$E3,   $27, $27   ; ISB,$2627
	;
	.byte $01
	;
        ; UNOFFICIAL OPCODES
	;
        .byte $03,    $EA,<ptr_2004,  $00, $25,$B7,$E3,   $4A, $4A   ; SLO,254A
        .byte $23,    $EA,<ptr_2004,  $00, $25,$B7,$E3,   $4A, $4B   ; RLA,254A or 254B
        .byte $43,    $EA,<ptr_2004,  $00, $25,$B7,$E3,   $12, $12   ; SRE,2512
        .byte $63,    $EA,<ptr_2004,  $00, $25,$B7,$E3,   $12, $92   ; RRA,2512 or $2592
        .byte $C3,    $EA,<ptr_2004,  $00, $25,$B7,$E3,   $24, $24   ; DCP,2524
	.byte $E3,    $EA,<ptr_2004,  $00, $25,$B7,$E3,   $26, $26   ; ISB,2526           
	;
	.byte $02
	;
        .byte $13,    $EA,<ptr_2004,  $00, $26,$B7,$E3,   $4C, $4C   ; SLO,$264C
        .byte $33,    $EA,<ptr_2004,  $00, $26,$B7,$E3,   $4C, $4D   ; RLA,$264C or $264D
        .byte $53,    $EA,<ptr_2004,  $00, $26,$B7,$E3,   $13, $13   ; SRE,$2613
        .byte $73,    $EA,<ptr_2004,  $00, $26,$B7,$E3,   $13, $93   ; RRA,$2613 or $2693
        .byte $D3,    $EA,<ptr_2004,  $00, $26,$B7,$E3,   $25, $25   ; DCP,$2625
	.byte $F3,    $EA,<ptr_2004,  $00, $26,$B7,$E3,   $27, $27   ; ISB,$2627
	;
	.byte $01
	;
        ; UNOFFICIAL OPCODES
	;
        .byte $1B,    $20,$04,        $00, $25,$B7,$E3,   $4A, $4A   ; SLO,254A
        .byte $3B,    $20,$04,        $00, $25,$B7,$E3,   $4A, $4B   ; RLA,254A or 254B
        .byte $5B,    $20,$04,        $00, $25,$B7,$E3,   $12, $12   ; SRE,2512
        .byte $7B,    $20,$04,        $00, $25,$B7,$E3,   $12, $92   ; RRA,2512 or $2592
        .byte $DB,    $20,$04,        $00, $25,$B7,$E3,   $24, $24   ; DCP,2524
	.byte $FB,    $20,$04,        $00, $25,$B7,$E3,   $26, $26   ; ISB,2526
	;
	.byte $01
	;
	.byte $00
.popseg

test_dummy_writes:
	jsr console_show

	set_test 5,"Some opcodes failed the test."
	;        0123456789ABCDEF0123456789ABC|
        ;        0E2E4E6ECEEE 1E3E5E7EDEFE *00
        ;        0F2F4F6FCFEF 1F3F5F7FDFFF *00
        ;        03234363C3E3 13335373D3F3 *00
        ;        1B3B5B7BDBFB              *00
        ;
        text_white
        print_str "Verifying opcodes...",newline
        setw opcode_ptr, Opcodes-1
        
 	lda #$60 ; "rts"
 	sta opcode_buffer+3
 	sta opcode_buffer+7
 	
 	lda #$F0
 	sta opcode_buffer+5
 	lda #$EA
 	sta opcode_buffer+6
 	
 	setw ptr_2004, $2004

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
	jsr next_opcode_byte ; OAM[0]
	sta temp_oam_data+0
	jsr next_opcode_byte ; OAM[1]
	sta temp_oam_data+1
	jsr next_opcode_byte ; OAM[2]
	sta temp_oam_data+2

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
	ldx #0
	stx SPRADDR
	lda temp_oam_data+0
	sta SPRDATA
	lda temp_oam_data+1
	sta SPRDATA
	lda temp_oam_data+2
	sta SPRDATA
	; Put something dummy at [3]
	stx SPRDATA
	ldx #0
	stx SPRADDR

	ldx temp_x
	ldy #0
	jsr opcode_buffer   ; Execute opcode

	lda #3
	sta SPRADDR
	lda SPRDATA         ; Read OAM[3] (And ignore it) to clear any possible internal buffer
	; Load first byte, and expect it to be unmodified
	lda #0
	sta SPRADDR
	lda temp_oam_data+0
	eor SPRDATA
	ora temp_read_result
	sta temp_read_result
	; Load second byte, and expect it to be what the test dicatetes
	lda #1
	sta SPRADDR
	ldy #0
	jsr next_opcode_byte
	eor SPRDATA
	ora temp_read_result
	sta temp_read_result
	; Load third byte, and expect it to be unmodified
	lda #2
	sta SPRADDR
	lda temp_oam_data+2
	eor SPRDATA
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

test_rom_write:
	set_test 7,"ROM should not be writable."
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
