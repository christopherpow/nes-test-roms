.macro fail_if_y_not n
	cpy #n
	jsr fail_current_test_ifNE
.endmacro
.macro fail_if_ppuread_y_not n
	ldy #n
	cpy PPUDATA
	jsr fail_current_test_ifNE
.endmacro



.macro MakeTest name1
        .local @Target
        .segment "POINTERS"
        name1 = (*-TestNames)/2 + 2
        .word @Target
        .segment "RODATA"
        @Target:
.endmacro

seg_data "POINTERS", {TestNames:}


.pushseg
 .segment "TEST_RESULTS": zeropage
 first_failed_test_code: .res 1
 test_summary:

 .segment "LIB"
reset_tests:
	lda #0
	ldx #NUM_TESTS
:	dex
	sta test_summary,x
	bne :-
	stx test_code
	stx first_failed_test_code
	
	lda #1
	sta test_summary+NUM_TESTS
	rts

.macro begin_test code
	lda #code
	jsr begin_test_ ; MUST NOT CHANGE CARRY FLAG
.endmacro

.macro check_other_test_failed code
	pha
	 lda test_summary + 1 + (code-2)
	 cmp #2
	pla
.endmacro

begin_test_: ; MUST NOT CHANGE CARRY FLAG
	php
	sta test_code
	txa
	pha
	 ldx test_code
	 lda test_summary-1,x
	 bne :+
	 inc test_summary-1,x
:	 txa
	 asl
	 tax
	 lda TestNames+0-4,x
	 sta test_name+0
	 lda TestNames+1-4,x
	 sta test_name+1
	pla
	tax
	plp
	rts

fail_current_test_ifEQ:
	beq fail_current_test
	rts
fail_current_test_ifNE:
	bne fail_current_test
	rts
fail_current_test_ifCC:
	bcc fail_current_test
	rts
fail_current_test_ifCS:
	bcs fail_current_test
	rts

fail_current_test:
	pha
	txa
	pha
	 ldx test_code
	 bmi @test_not_available
	 cpx #2
	 bcc @no_test_to_fail
	 lda test_summary-1,x
	 cmp #1
	 bcc @test_not_running
	 bne @test_already_failed

	 inc test_summary-1,x
	 inc test_summary
	 
	 lda first_failed_test_code
	 bne :+
	 stx first_failed_test_code
:	 

	@no_test_to_fail:

	 setb SNDCHN,   3 ; Enable APU square channel 0

	 setb $4000+0, $0F ; env decay rate 15; not disabled, not looped; length counter enabled
	 setb $4000+1, $00 ; no sweep
	 setb $4000+2, $C8 ; period lo
	 setb $4000+3, 1*8+3 ; period hi=3, length counter=30
	 
	 ;jsr wait_vbl
	 ;print_str "Test #"
	 ;lda test_code
	 ;jsr print_dec
	 ;print_str " failed",newline

	@test_already_failed:
	@test_not_available:
	@test_not_running:

	pla
	tax
	pla
	rts

test_failing_barrier:
	lda test_summary
	bne some_tests_failed
	;print_str "No fails so far",newline	;DO_DxE
	rts
some_tests_failed:
	set_vrom_page 0
	text_white
	jsr console_show

	jsr print_spacer
	print_ext_str_flush "Failed tests:"	;DO_DTE
	text_color1

	setb test_summary, 0

	ldx #NUM_TESTS
:	txa
	pha
	 lda test_summary-1,x
	 cmp #1
	 bcc @wasnt_tested
	 beq @didnt_fail
	  jsr begin_test_
	  jsr print_space
	  jsr print_dec
	  jmp @didnt_fail
@wasnt_tested:
	  inc test_summary	;Set a flag indicating some tests were not tested
@didnt_fail:
	 pla
	 tax
	dex
	cpx #2
	bcs :-
	jsr print_newline

	lda test_summary
	beq @no_untested_tests
	; Print list of not-tested tests if the flag was set
	text_white
	print_ext_str "Untested:"	;DO_DTE
	text_color1
	ldx #2
:	lda test_summary-1,x
	bne @was_tested
	 
	 jsr print_space
	@end_range:
	 txa
	 pha
	  jsr print_dec
	 pla
	 tax
	 
	 cpx #NUM_TESTS-2
	 bcs @not_three

	 lda test_summary-1+0,x
	 bne @not_three
	 lda test_summary-1+1,x
	 bne @not_three
	 ; Three or more were untested
	 lda #'-'	;DTE_CHARMAP
	 jsr print_char
	 
	@range_loop:
	 lda test_summary-1+1,x
	 bne @end_range
	 inx
	 bne @range_loop

@not_three:

@was_tested:
	inx
	cpx #NUM_TESTS
	bcc :-
	jsr print_newline
@no_untested_tests:
	text_white
	;jsr print_spacer
	;print_ext_str "The first failed test was:",newline
	jsr print_spacer
	lda first_failed_test_code
	jsr begin_test_
	; Re-enable screen
	set_vrom_page 0
	jsr console_show
	text_white
	jmp test_failed
 
.popseg

