.pushseg
.segment "LIB"

crash_proof_begin:
	setb nmi+3, $A2 ; LDX #imm
	pla
	sta nmi+8
	pla
	sta nmi+9
	tsx
	stx nmi+4
	setb nmi+5, $9A ; TXS
	setb nmi+6, $38 ; SEC
	setb nmi+7, $4C ; JMP abs
	lda nmi+9
	pha
	lda nmi+8
	pha
	inc nmi+8
	bne @no_pagewrap
	inc nmi+9
@no_pagewrap:
	lda #10 ; Tolerate 10 NMIs (frames) before deeming we've crashed
	sta nmi_count
	setb nmi+0, $4C ; JMP abs
	setb nmi+1, <@rescue
	setb nmi+2, >@rescue
	setb PPUCTRL, $A8 ; Enable NMI
	clc
	; This routine may return once or twice, like setjmp.
	; Carry clear = normal exit. Carry set = crash-recovery exit.
	rts
@rescue:
	dec nmi_count
	bmi @crashed
	rti
@crashed:
nmi_crash_recovery:
	jsr crash_proof_end
	text_white
	print_ext_str_flush " CRASH"	;DO_DTE
	inc spr0hit_fails_current
	jsr fail_current_test
	jmp nmi+3

crash_proof_end:
	setb PPUCTRL, $28 ; Disable NMI
	setb nmi+0, $40 ; RTI
	rts

.segment "LIB"

zp_res	irq_temp_a
zp_res	irq_temp_x
zp_res	irq_temp_y
zp_res	irq_temp_s
zp_res	irq_temp_p
zp_res	irq_temp_pc
irq_temp_reg = irq_temp_a

irq:	; We do not expect IRQs.
	; Check what is going on.
	sta irq_temp_a ; A
	stx irq_temp_x ; X
	sty irq_temp_y ; Y
	pla
	sta irq_temp_p ; P
	pla
	sta irq_temp_pc+0 ; pc lo
	pla
	sta irq_temp_pc+1 ; pc hi
	tsx
	stx irq_temp_s ; S
	lda irq_temp_p
	and #$10
	bne @got_spurious_brk
	; Wasn't BRK. Was IRQ.
	text_white
	;jsr console_show
	;set_vrom_page 0
	print_ext_str " IRQ?",newline,"Did not expect an IRQ"	;DO_DTE
	jmp @exit_show_registers
@got_spurious_brk:
	; Was crash-proof active?
	lda nmi+7
	cmp #$4C+100
	bne @nope
	ldx irq_temp_reg+3
	jmp nmi_crash_recovery
@nope:
	; Decrement the return PC by 2 to get the location of BRK instruction
	dec irq_temp_pc+0
	bne :+
	dec irq_temp_pc+1
:	dec irq_temp_pc+0
	bne :+
	dec irq_temp_pc+1
:	;
	;
	text_white
	;jsr console_show
	;set_vrom_page 0
	;                                    0123456789ABCDEF0123456789ABC|0123456789ABCDEF0123456789ABC|
	print_ext_str " BRK?",newline,"BRK executed where should not"	;DO_DTE
@exit_show_registers:
	@delta = $80
	.pushseg
	 .segment "RODATA"
	 @script:
	 .byte newline	;DTE_CHARMAP
	 .byte "PC="	;DTE_CHARMAP
	 .byte @delta+1+irq_temp_pc+1-irq_temp_reg
	 .byte @delta+1+irq_temp_pc+0-irq_temp_reg
	 .byte ",AXYSP="	;DTE_CHARMAP
	 .byte @delta+1+irq_temp_a-irq_temp_reg
	 .byte "/"	;DTE_CHARMAP
	 .byte @delta+1+irq_temp_x-irq_temp_reg
	 .byte "/"	;DTE_CHARMAP
	 .byte @delta+1+irq_temp_y-irq_temp_reg
	 .byte "/"	;DTE_CHARMAP
	 .byte @delta+1+irq_temp_s-irq_temp_reg
	 .byte "/"	;DTE_CHARMAP
	 .byte @delta+1+irq_temp_p-irq_temp_reg
	 .byte newline	;DTE_CHARMAP
	 .byte "Test failed.",newline	;DTE_CHARMAP
	 .byte @delta
	.popseg
	ldy #$FF
	ldx #'!'	;DTE_CHARMAP
@l:	jsr print_char_x
:	iny
	ldx @script,y
	cpx #@delta
	beq @done
	bcc @l
	lda irq_temp_reg-@delta-1,x
	jsr print_hex
	bcs :-
@done:	jsr print_newline
	jsr fail_current_test
	jmp main_exit

.popseg
