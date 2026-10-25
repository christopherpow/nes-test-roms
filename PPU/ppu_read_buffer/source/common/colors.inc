;color1 = ($60-DTE_CHARSET_OFFSET)&$FF
;color2 = ($C0-DTE_CHARSET_OFFSET)&$FF
;color3 = ($100-DTE_CHARSET_OFFSET)&$FF

color1 = ($80-DTE_CHARSET_OFFSET)&$FF
color2 = ($80-DTE_CHARSET_OFFSET)&$FF
color3 = ($100-DTE_CHARSET_OFFSET)&$FF

; $E0 means, ' ' becomes 0, '!' becomes 1 etc.


zp_res color_ptr,2

.pushseg
.segment "RODATA"
Color1Esc: .byte 27, "[0;33m", 0
Color2Esc: .byte 27, "[1;34m", 0
Color3Esc: .byte 27, "[0;37m", 0
.segment "LIB"

TextColor1:
	pha
	setw color_ptr, Color1Esc
	lda #color1
	bne ColorPrint_Sub
TextColor2:
	pha
	setw color_ptr, Color2Esc
	lda #color2
	bne ColorPrint_Sub
TextColor3:
	pha
	setw color_ptr, Color3Esc
	lda #color3
ColorPrint_Sub:
	sta text_color
	tya
	pha
	 ldy #0
@loop:
	 lda (color_ptr),y
	 beq :+
	  jsr write_text_out_raw
	  incw color_ptr
	  bne @loop
:	pla
	tay
	pla
	rts
	


.popseg



.macro text_color1
	jsr TextColor1
.endmacro
.macro text_color2
	jsr TextColor2
.endmacro
.macro text_white
	jsr TextColor3
.endmacro
