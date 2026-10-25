; Text output as expanding zero-terminated string at text_out_base

; The final exit result byte is written here
final_result = $6000

; Text output is written here as an expanding
; zero-terminated string
text_out_base = $6004

zp_res text_out_temp
zp_res text_out_addr,2

init_text_out:
	ldx #0
	
	; Put valid data first
	setb text_out_base,0
	
	lda #$80
	jsr set_final_result
	
	; Now fill in signature that tells emulator there's
	; useful data there
	setb text_out_base-3,$DE
	setb text_out_base-2,$B0
	setb text_out_base-1,$61
	
	ldx #>text_out_base
	stx text_out_addr+1
	setb text_out_addr,<text_out_base
	rts


; Sets final result byte in memory
set_final_result:
	sta final_result
	rts


; Writes character to text output
; In: A=Character to write
; Preserved: A, X, Y
write_text_out:
.if DTE_CHARSET_OFFSET = $20
write_text_out_raw:
.endif
	sty text_out_temp
	
	; Write new terminator FIRST, then new char before it,
	; in case emulator looks at string in middle of this routine.
	pha
.if DTE_CHARSET_OFFSET <> $20
	 tay
	 lda DTE_CHARSET-DTE_CHARSET_OFFSET,y
	 jsr write_text_out_work_raw
	pla
	pha
	 cmp #(DTE_CHARSET_OFFSET+DTE_CHARSET2_BEGIN)
	 bcc @dont_do
	 tay
	 lda DTE_CHARSET2-DTE_CHARSET_OFFSET-DTE_CHARSET2_BEGIN,y
	 bne @do
@dont_do:
	pla
	bne write_text_out_exit
@do:
.endif
write_text_out_common:
	 jsr write_text_out_work_raw
	pla
write_text_out_exit:
	ldy text_out_temp
	rts

write_text_out_work_raw:
	 pha
	  ldy #1
	  lda #0
	  sta (text_out_addr),y
	  dey
	 pla
	 sta (text_out_addr),y
	 inc text_out_addr+0
	 beq :+
	 rts
:	 inc text_out_addr+1
	 rts

.if DTE_CHARSET_OFFSET <> $20
; Write raw ascii character without reverse ROM character set translation
write_text_out_raw:
	sty text_out_temp
	pha
	 jmp write_text_out_common
.endif
