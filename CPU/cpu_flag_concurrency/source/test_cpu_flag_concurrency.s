; Expected output, and explanation:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Written by Joel Yliluoma - http://iki.fi/bisqwit/

.segment "LIB"
.include "shell.inc"
.include "colors.inc"
.include "sync_apu.s"
.segment "CODE"

zp_res	irq_temp
zp_res	nmi_temp
zp_res	irq_count
zp_res	nmi_count
zp_res	brk_count
zp_res	brk_issued
zp_res  brk_4015
zp_res	num_cycles_plan,2
zp_res	num_cycles_min,2
zp_res	num_cycles_max,2
zp_res	test_phase

zp_res  irq_attempt_offset
zp_res  add_temp,2
zp_res	n_repeats_seen

zp_res  flags_main_1
zp_res  flags_main_2
zp_res  flags_inside_nmi
zp_res  flags_inside_irq
zp_res  flags_inside_brk
zp_res  flags_stack_nmi
zp_res  flags_stack_irq
zp_res  flags_stack_brk
zp_res  flag_errors,2
zp_res  last_checksum,4

zp_res	temp_code_pointer
zp_res	brk_opcode

zp_res  console_save,4

.macro print_flag name,expect
	text_color2
	lda name
	and #$30
	cmp #expect
	beq :+
	text_color1
	inc flag_errors
	inc flag_errors+1
:	jsr print_hex
	text_white
.endmacro
.macro print_flags_2 hdr, name1,expect1, sep, name2,expect2, trail_f, trail_o
	.local @ok
	lda #0
	sta flag_errors+1
	my_print_str hdr
	print_flag name1,expect1
	my_print_str sep
	print_flag name2,expect2
	lda flag_errors+1
	beq @ok
	my_print_str trail_f
	lda #expect1
	jsr print_hex
	my_print_str sep
	lda #expect2
	jsr print_hex
	jmp :+
@ok:	my_print_str trail_o
:	my_print_str newline
.endmacro

.macro print_str_and_ret s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15
	print_str s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15
	rts
.endmacro
.macro my_print_str s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15
	.local @Addr
	jsr @Addr
	seg_data "RODATA",{@Addr: print_str_and_ret s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15}
.endmacro

test_failed_finish:
	; Re-enable screen
	jsr console_show
	text_white
	jmp test_failed

main:   
	jsr intro
	
	jsr verify_basic_flag_operations
	jsr verify_apu_irq_timing
	jsr verify_brk_and_irq_coincidence

	text_white
	jsr console_show
	jsr wait_vbl
	jmp tests_passed


verify_brk_and_irq_coincidence:
	; Now that we know _exactly_ how many cycles to consume
	; to have IRQ trigger on exactly the next cycle, go and
	; try to smash the IRQ together with a BRK instruction.
	text_color1
	print_str    "Invoking a BRK-IRQ collision",newline
	;             0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	text_white
	set_test 4,  "When an IRQ coincides with an BRK opcode,  the CPU  should  forget to run  the BRK opcode separately."

	lda #0
	sta irq_attempt_offset
	sta n_repeats_seen
	sta test_phase
	sta flag_errors
	sta flag_errors+1
	sta last_checksum+0
	sta last_checksum+1
	sta last_checksum+2
	sta last_checksum+3

	;          0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	;          Offs #INTs Flags   4015 B n
	;          00     2     $20,$30 $00  1
	print_str "Offs #INTs Flags   4015 B n"
	jsr save_console

@more_offsets:
	lda #0
	sta brk_count
	sta flags_stack_brk
	sta flags_inside_brk
	lda #$01
	sta brk_issued
	jsr sync_apu
	lda SNDCHN
	cli
	setb SNDMODE, 0 ; enable IRQ

	; Wait for another IRQ
	lda num_cycles_plan+0
	sec
	sbc #200
	sta add_temp+0
	lda num_cycles_plan+1
	sbc #0
	sta add_temp+1
	lda add_temp+0
	clc
	adc irq_attempt_offset
	tax
	lda add_temp+1
	adc #0
	jsr delay_256a_x_26_clocks

	php
	brk
	; The brk is actually a two-byte opcode. Just in case
	; someone's emulator does not do stuff properly, I made
	; the second byte a harmless NOP.
	nop
	; This weird opcode ($59) is an opcode that occurs
	; nowhere else in the code. It is checked at
	; the IRQ return address. Extra regular NOPs below
	; ensure that a broken CPU implementation will not
	; cause too much harm.
	eor $EAEA,y ;$59
	nop
	nop
	nop

	sei
	lda #$00
	sta brk_issued
	
	pla
	sta flags_inside_brk

	lda flags_stack_brk
	and #$30
	sta flags_stack_brk
	lda flags_inside_brk
	and #$30
	sta flags_inside_brk
	
	text_color2 ;default: assume OK

	ldx #'0'
	lda brk_opcode
	cmp #$59
	bne :+
	inx
:	stx brk_opcode

	jsr reset_crc
	lda brk_count
	 jsr update_crc_
	lda flags_stack_brk
	 jsr update_crc_
	lda flags_inside_brk
	 jsr update_crc_
	lda brk_4015
	 jsr update_crc_
	lda brk_opcode
	 jsr update_crc_

	setw addr,last_checksum
	jsr is_crc_
	beq @same_crc

	; CRC differs
	lda #0
	sta n_repeats_seen
	ldx #3
:	lda checksum,x
	eor #$FF
	sta last_checksum,x
	dex
	bpl :-
	jsr print_newline
	jsr save_console
	jmp @diff_crc

@same_crc:
	ldx #27
	jsr restore_console
@diff_crc:
	lda irq_attempt_offset
	jsr print_dec_100_255
	print_str "  "

	lda brk_count
	jsr print_dec
	print_str "     $"
	lda flags_stack_brk
	jsr print_hex
	print_str ",$"
	lda flags_inside_brk
	jsr print_hex
	print_str " $"
	lda brk_4015
	jsr print_hex
	print_str "  "
	lda brk_opcode
	jsr print_char
	lda #' '
	jsr print_char
	inc n_repeats_seen
	lda n_repeats_seen
	jsr print_dec
	
	jsr console_flush
	lda #13 ; CR
	jsr write_text_out

	inc irq_attempt_offset
	lda irq_attempt_offset
	cmp #250
	bcs :+
	jmp @more_offsets
:
	text_white
	
	; ANALYZE TEST RESULTS:
	;
	; BRK-IRQ conflict OK     IF  Seen #ints=1, B=0
	
	lda flag_errors
	beq :+
	jmp test_failed_finish
	
	rts
:	;my_print_str newline,"BRK-IRQ conflict OK",newline
	;                  0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	text_white
	print_str newline,"How does it look? Please",newline,"report to me the following",newline," information:",newline
	
	;          0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	;          - It took nnnnn cycles for      the APU IRQ to be signalled.
	print_str "- IRQ trigger timing: "
	text_color1
	 jsr print_plan
	text_white
	print_str newline
	print_str "- And the entire contents of",newline,"  the above table.",newline
	print_str "- It will be helpful to know",newline,"  if the values differ any",newline,"  when running the test",newline,"  multiple times.",newline
	text_color2
	print_str "THANK YOU FOR THE HELP.",newline
	text_color1
	print_str "-Joel Yliluoma (",'"',"Bisqwit",'"',")"
	lda #0
	jmp exit
	rts


verify_apu_irq_timing:
	text_color1
	;             0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	print_str    "Finding APU IRQ timings",newline
	set_test 3,  "APU timings differ from  what was expected"
	text_color2
	jsr save_console
	
	setw num_cycles_min, 25000 ; NTSC timing (29831), minus generous threshold
	setw num_cycles_max, 35000 ; PAL timing (33257), plus generous threshold
@more_loop:
	 ; plan = (min+max)/2. min and max are both 16-bit. The sum is 17-bit.
	 clc
	 lda num_cycles_min+0
	 adc num_cycles_max+0
	 tax
	 lda num_cycles_min+1
	 adc num_cycles_max+1
	 ror 
	 sta num_cycles_plan+1
	 txa
	 ror
	 sta num_cycles_plan+0
	 ; Check whether min==max
	 lda num_cycles_min+0
	 eor num_cycles_max+0
	 sta irq_attempt_offset
	 tax
	 lda num_cycles_min+1
	 eor num_cycles_max+1
	 bne :+
	 eor irq_attempt_offset
	 beq @done_loop
:
	 sei
	 setb SNDMODE, $40
	 setb irq_count, 0
	 lda #0
	 sta brk_issued
	 sta irq_count

	 ; Run one IRQ.
	 jsr sync_apu
	 setb SNDMODE, 0 ; Enable IRQ (but block them at CPU)
@event_done:
	 ; Wait for another IRQ
	 lda num_cycles_plan+0	; 3 cycles
	 sec			; 2 cycles
	 sbc #40		; 2 cycles
	 tax			; 2 cycles
	 lda num_cycles_plan+1	; 3 cycles
	 sbc #0			; 2 cycles
	 jsr delay_256a_x_26_clocks ; OVERHEAD: 3+2+2+2+3+2+26
	 ; Gets    29823
	 ; Expect: 29831
	 lda SNDCHN
	 and #$40
	 lsr ;$20
	 lsr ;$10
	 lsr ;$08
	 lsr ;$04
	 lsr ;$02
	 lsr ;$01
	 sta brk_4015
	 ldx #0
	 jsr restore_console
	 lda brk_4015
	 jsr print_digit
	 my_print_str "; "
	 jsr @disp_cycles_minmax
	 lda #13 ; CR
	 jsr write_text_out

	 ; irq occurred(1)? max=A. Otherwise, min=A+1
	 lda brk_4015
	 bne @need_smaller
@need_larger:
	 ; min = plan+1
	 lda num_cycles_plan+0
	 clc
	 adc #1
	 sta num_cycles_min+0
	 lda num_cycles_plan+1
	 adc #0
	 sta num_cycles_min+1
	 jmp @more_loop
@need_smaller:
	 ; max = plan
	 lda num_cycles_plan+0
	 sta num_cycles_max+0
	 lda num_cycles_plan+1
	 sta num_cycles_max+1
	 jmp @more_loop
@done_loop:
	jsr restore_console

	setb SNDMODE, $40 ;disable IRQ
	;print_str newline
	lda num_cycles_plan+0
	cmp #<29823
	bne :+
	lda num_cycles_plan+1
	cmp #>29823
	bne :+

	text_white
	my_print_str "OK, "
	text_color2
	jsr @disp_cycles_minmax
	jmp print_newline
:	text_color1
	;             0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	my_print_str "Warning: Timings do not match the reference. Expected 29823.Got "
	text_white
	jsr print_plan
	text_color1
	my_print_str ". Trying anyway.",newline
	rts
@disp_cycles_minmax:
	 lda num_cycles_min+1
	 ldx num_cycles_min+0
	 jsr print_dec16
	 my_print_str "<="
	 text_white
	 jsr print_plan
	 text_color2
	 my_print_str "<"
	 lda num_cycles_max+1
	 ldx num_cycles_max+0
	 jsr print_dec16
	 jmp console_flush
print_plan:
	 lda num_cycles_plan+1
	 ldx num_cycles_plan+0
	 jmp print_dec16

verify_basic_flag_operations:
	text_color1
	;             0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	my_print_str "Verifying that basic CPU flag operations work properly",newline

	; Collect flags.
	lda #$FF
	pha
	plp
	php
	pla
	sta flags_main_2

	lda #0
	pha
	plp
	php
	pla
	sta flags_main_1

	text_color2

	; Run one NMI.
	lda #0
	sta brk_issued
	sta nmi_count
	sta irq_count
	setb PPUCTRL, $80
	jsr save_console
	my_print_str "Waiting for NMI..."
:	lda nmi_count
	beq :-	
	setb PPUCTRL, $00
	
	ldx #18
	jsr restore_console
	my_print_str "Waiting for IRQ..."

	; Run one IRQ.
	setb SNDMODE, $00
	cli
:	lda irq_count
	beq :-	
	sei
	setb SNDMODE, $40

	ldx #18
	jsr restore_console
	my_print_str "Issuing BRK..."

	lda #1
	sta brk_issued
	nop
	brk
	nop
	nop
	lda #0
	sta brk_issued

	ldx #14
	jsr restore_console

	text_white
	;           0123456789ABCDEF0123456789ABC|01234
	set_test 2,"Flag  bits  are   not handled properly"
	lda #0
	sta flag_errors

	print_flags_2 "DFL:",flags_main_1,    $30, "; ", flags_main_2,    $30, "   EXPECT: ", "   OK   "
	print_flags_2 "NMI:",flags_stack_nmi, $20, "->", flags_inside_nmi,$30, "   EXPECT: ", "   OK   "
	print_flags_2 "IRQ:",flags_stack_irq, $20, "->", flags_inside_irq,$30, "   EXPECT: ", "   OK   "
	print_flags_2 "BRK:",flags_stack_brk, $30, "->", flags_inside_brk,$30, "   EXPECT: ", "   OK   "

	my_print_str newline
	lda flag_errors
	beq :+
	text_color1
	jsr print_dec
	my_print_str " ERROR(S)"
	jmp test_failed_finish
:	rts






	.pushseg
	.segment "RODATA"
intro:	text_white
	print_str "TEST:test_cpu_flag_concurrency"
	text_color1
	jsr print_str_
	;       0123456789ABCDEF0123456789ABCD
	 .byte newline,0
	text_white
	rts
	.popseg





nmi:
	php
	sta nmi_temp
	pla
	sta flags_inside_nmi
	pla
	sta flags_stack_nmi
	pha
	 inc nmi_count
	lda nmi_temp
	rti

irq:
	php
	sta irq_temp
	lda SNDCHN
	sta brk_4015
	lda brk_issued
	bne @got_brk
	pla ; my flags
	sta flags_inside_irq
	pla ; caller's flags
	sta flags_stack_irq
	pha ; push the flags again
	 inc irq_count
	lda irq_temp
	rti
@got_brk:
	; Check IRQ return address
	txa
	pha
	 ; Inspect stack
	 tsx
	 lda $0104,x
	 sta temp_code_pointer+0
	 lda $0105,x
	 sta temp_code_pointer+1
	 ldx #0
	 lda (temp_code_pointer,x)
	 sta brk_opcode
	pla
	tax

	pla ; my flags
	sta flags_inside_brk
	pla ; caller's flags
	sta flags_stack_brk
	pha ; push the flags again
	inc brk_count
	lda irq_temp
	rti

save_console: ; Preserves A,X,Y
	pha
	 lda console_pos
	 sta console_save+0
	 lda console_scroll
	 sta console_save+1
	 lda text_out_addr+0
	 sta console_save+2
	 lda text_out_addr+1
	 sta console_save+3
	pla
	rts

restore_console: ; X = number of backspaces to put in console. Preserves A,Y
	pha
	 lda console_save+0
	 sta console_pos
	 lda console_save+1
	 sta console_scroll
	 lda console_save+2
	 sta text_out_addr+0
	 lda console_save+3
	 sta text_out_addr+1
:	 txa
	 beq :+
	 dex
	 lda #8
	 jsr write_text_out
	 jmp :-
:	pla
	rts
