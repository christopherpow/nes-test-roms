zp_res	nmi_count
zp_res	nmi_ready

	; Generates the load and store templates
	; for using A*$100 as a buffer.
load_transfer_templates:
	sta template_subst_ptr+1
	lda #$00
	sta template_subst_ptr+0

	setw template_targetptr, load_cool_graphics
	setw template_sourceptr, loader_template_begin
	setb template_bytecount, (loader_template_end-loader_template_begin+3)&~3
	jsr load_template

	setw template_targetptr, transfer_cool_graphics
	setw template_sourceptr, writer_template_begin
	setb template_bytecount, (writer_template_end-writer_template_begin+3)&~3
	;jmp load_template
load_template:
	ldy #0
	ldx template_subst_ptr+1
@loop:	lda (template_sourceptr),y
	cmp #$FF
	beq @patch
@maybe_wrap:
	sta (template_targetptr),y
	iny
	cpy template_bytecount
	bne @loop
	rts
@patch:	lda template_subst_ptr+0
	sta (template_targetptr),y
	iny
	txa
	jmp @maybe_wrap

.pushseg
.segment "TEMPLATE1"
writer_template_begin:
	lda #0
	sta gfx_source_ppuptr+0
	sta gfx_source_ppuptr+1
writer_template_skip_sourceptr:
	sty gfx_bytecount
	lda PPUSTATUS ; reset flip-flop
:	 jsr load_cool_graphics
	 lda addr+1
	 sta PPUADDR ; poke high 6 bits
	 lda addr+0
	 sta PPUADDR ; poke low  8 bits
	 ldy #0
:	 .repeat 8
	  lda $FFFF,y
	  sta PPUDATA
	  iny
	 .endrepeat
	 cpy gfx_bytecount
	 bne :-
	 inc gfx_source_ppuptr+1
	 inc addr   +1
	dex
	bne :--
	rts
writer_template_end:


.segment "TEMPLATE2"
loader_template_begin:
	; Load Y bytes of data from PPU address "gfx_source_ppuptr" into oam_buffer.
	lda gfx_source_ppuptr+1
	sta PPUADDR ; poke high 6 bits
	lda gfx_source_ppuptr+0
	sta PPUADDR ; poke low  8 bits
	lda PPUDATA
	ldy #0
:	.repeat 4
	 lda PPUDATA
	 sta $FFFF,y
	 iny
	.endrepeat
	cpy gfx_bytecount
	bne :-
	rts

loader_template_end:


.popseg

do_cool_graphics:
	; Disable the rendering while transferring data from/to PPU memory.
	jsr console_hide

	; Just for the sake of being peculiar, all our graphics data is in VROM.
	; Not only that, the code that loads the data from VROM is also in VROM!
	; And not only that, the code that shows the data is there too.
	
	; load_transfer_templates:
	;   Copies loader_template and writer_template into RAM.
	;   Patches the oam_buffer offset into the code.
	; load_cool_graphics:
	;   This function is in RAM. It loads data from VROM into buffer.
	; transfer_cool_graphics:
	;   This function is in RAM. It writes data from buffer into PPU memory.

	; Load our graphics bootstrap code from VROM
	set_vrom_page 1
	setw gfx_source_ppuptr, page2_prefix_size
	
	@size = $500 ;show_cool_graphics_size + $100
	ldx #>@size

        ldy #0
:       setb gfx_bytecount,0
        txa
	bne :+
	lda #<@size
	beq @rts
        sta gfx_bytecount
:	txa
	pha
         tya
         pha
          clc
          adc #>show_cool_graphics-1	;-1 because we also load OAM template (gfx.spr.bin)
          jsr load_transfer_templates
          jsr load_cool_graphics
          inc gfx_source_ppuptr+1
 	 pla ;y
 	 tay
 	 iny
 	pla ;x
 	tax
        dex
        bpl :--
@rts:	
	; Jump into the bootstrap code.
	jmp show_cool_graphics ; skip gfx.spr.bin


;;;;;;;;;;;;;;;;;;;;;


.segment "CHARS_P2"
    @begin:
        .incbin "gfx.nta.bin"
        .incbin "gfx.attr.bin"

	; The following begins at $0400.
	.incbin "gfx.spr2.chr"

	; The following begins at $0400+sprite_data_size.
        .incbin "gfx.pal.bin"
    @end:
	page2_prefix_size = @end-@begin

	.incbin "gfx.spr.bin"

.segment "CHARS_P2"
	; THIS CODE IS STORED IN VROM. (PAGE 1, OFFSET $0520)
show_cool_graphics_template_begin:
	; Because this code actually resides in VROM, and later in system
	; RAM, we must use this c_DELTA factor in any and all absolute
	; addresses (jmp,jsr etc) within this code referring to itself.
	c_DELTA  = show_cool_graphics_template_begin - show_cool_graphics

	lda #1
	sta console_palettes_disabled

	text_white
	jsr print_spacer
    	jsr print_str_
	;      0123456789ABCDEF0123456789ABCD
	.byte " This next test will take a while.",newline	;DO_DTE
	.byte " In order to distract you with",newline	;DO_DTE
	.byte " entertainment, art is provided.",newline	;DO_DTE
	.byte " Contemplate on the art while",newline	;DO_DTE
	.byte " the test is in progress.",newline	;DO_DTE
	.byte newline,newline,newline	;DO_DTE
	.byte 0	;DO_DTE

	jsr c_test_read_buffer_fade_blank_delay - c_DELTA

	lda #0
	sta PPUCTRL
	sta PPUMASK

	;set_vrom_page 1
	lda #>oam_buffer
	jsr load_transfer_templates

	; Transfer $400 bytes of data to $2C00 (second nta regadless of mirroring)
	setw addr, $2C00
	ldx #4
	ldy #0
	jsr transfer_cool_graphics

	; Transfer $20 bytes of data to $3FE0
	setw addr, $3FE0
	setw gfx_source_ppuptr, (page2_prefix_size-$20)
	ldx #1
	ldy #$20
	jsr transfer_cool_graphics + (writer_template_skip_sourceptr-writer_template_begin)

	; Initialize scrolling
	lda #230
	sta scroll_framecount
	lda #1
	sta scroll_y
	lda #0
	sta scroll_y+1
	sta scroll_ydelta
	sta scroll_ydelta+1
	sta scroll_framecount+1
	sta nmi_ready

	jsr console_show
	set_vrom_page 0

	jsr c_test_read_buffer_fade_visible_delay_init - c_DELTA

	lda #$00
	sta nmi_count
	sta PPUSCROLL
	lda #1
	sta PPUSCROLL

	; Load $100 bytes of actual sprite data to oam_buffer
	ldx #0
:	lda CoolGraphicsSpr, x
	sta oam_buffer,x
	inx
	bne :-

	jsr wait_vbl_optional
	lda #$A0    ;Select background graphics page 0 & nametable 0
	sta PPUCTRL ; & 16pix sprites & enable NMI
	lda #($F * 2)
	sta PPUMASK ;Show background graphics and sprites

c_main_loop:
	lda #0
c_end_flag_loc = *-1
	bne @end_loop

	jsr c_update_scrolling    - c_DELTA  - $800
	jsr c_update_sprites      - c_DELTA - $1000
	jsr c_update_split_points - c_DELTA - $1800
	jsr c_read_joypad         - c_DELTA - $1800

	; All set. Wait for NMI
	inc nmi_ready
:	lda nmi_count
	beq :-
	dec nmi_count
	dec nmi_ready
	beq c_main_loop

@end_loop:
	lda #0
	sta PPUCTRL 	; disable NMI
	sta console_palettes_disabled
	jsr console_hide
	set_vrom_page 0
	rts

c_nmi_handler:
	; That's right, our NMI handler is in RAM.
	lda nmi_ready
	beq @skip_nmi_reentry
	lda nmi_count
	bne @skip_nmi_reentry

	sta bus_conflict_antidote+0 ; VROM page 0
	lda PPUSTATUS

	jsr c_test_read_buffer_fade_visible_delay_callback - c_DELTA

	ldx scroll_y
	ldy #0

	sty PPUSCROLL	;0
	stx PPUSCROLL	;scroll_y

	sty SPRADDR	;sprite 0
	lda #>oam_buffer
	sta $4014

	lda #$A0    ;Select background graphics page 1 & nametable 0
	sta PPUCTRL ; & 16pix sprites & enable NMI

 	ldy #($F*2)
	lda #2
	bit joy_cur
	beq :+
	ldy #($F*2)-16
:	sty PPUMASK

	jsr c_perform_splits - c_DELTA - $1000

	inc nmi_count
@skip_nmi_reentry:
	rti


c_update_sprites:
	lda scroll_y
	clc
	adc #17
	sta scroll_delays
	
	ldy #$F8
	ldx #$E0
c_sprloop:
	.repeat 8,n
	lda CoolGraphicsSpr -n*$20,x
	sec
	sbc scroll_delays
	bcc :+
	tya
:	sta oam_buffer -n*$20 ,x
	.endrepeat

	inx
	inx
	inx
	inx
	bne c_sprloop
	rts

c_perform_splits:
	; SECTION 0 (BEFORE GRAPHICS):
	; 	0.$0000 - BACKGROUND: TEXT.
	;	0.$1000 - unused

	lda scroll_delays+0
	beq :+
	ldx #50
	jsr delay_n_scanlines_ntsc

	 set_vrom_page 2
	 lda #$B1    ;Select background graphics page 1 & nametable 0
	 sta PPUCTRL ; & 16pix sprites & enable NMI
	; SECTION 1:
	; 	2.$0000 - SPRITES.
	;	2.$1000 - BACKGROUND
	
	lda scroll_delays+4
	beq :+
	ldx #50
	jsr delay_n_scanlines_ntsc

	 set_vrom_page 1
	; SECTION 2:
	; 	1.$0000 - SPRITES.   code unused
	;	1.$1000 - BACKGROUND
	
	lda scroll_delays+5
	beq :+
	ldx #20
	jsr delay_n_scanlines_ntsc

	 set_vrom_page 0
	; SECTION 3:
	; 	0.$0000 - unused
	;	0.$1000 - SPRITES & BACKGROUND

	lda scroll_delays+6
	beq :+
	ldx #20
	jsr delay_n_scanlines_ntsc

	 set_vrom_page 2
	 lda #$A1    ;Select background graphics page 0 & nametable 0
	 sta PPUCTRL ; & 16pix sprites & enable NMI
	; SECTION 4:
	; 	2.$0000 - SPRITES & BACKGROUND
	;	2.$1000 - unused

:	rts


c_update_split_points:
	c_MAXY=242
	c_OFFSET=255

	lda #255
	sta scroll_delays+0
	sta scroll_delays+1
	sta scroll_delays+2
	sta scroll_delays+3
	
	ldx scroll_y
	dex
	stx scroll_delays+6

	.macro sub16 v
	 lda #<(v)
	 sec
	 sbc scroll_delays+6
	 tax
	 lda #>(v)
	 sbc #0
	.endmacro

	sub16 c_OFFSET+(0)
	bne @skip0
	stx scroll_delays+0

	sub16 c_OFFSET+(64)
	bne @skip0
	stx scroll_delays+1

	sub16 c_OFFSET+(128)
	bne @skip1
	stx scroll_delays+2
	
	sub16 c_OFFSET+(184)
	bne @skip2
	stx scroll_delays+3

@skip2:
@skip1:
@skip0:
	lda #0
	sta scroll_delays+4
	sta scroll_delays+5
	sta scroll_delays+6

	lda scroll_delays+0
	tay
	cmp #c_MAXY
	bcc :+
	ldy #0
:	sty scroll_delays+0
	;
	lda scroll_delays+1
	cmp #c_MAXY
	bcs :+
	sec
	sbc scroll_delays+0
	sta scroll_delays+4
	;
	lda scroll_delays+2
	cmp #c_MAXY
	bcs :+
	sec
	sbc scroll_delays+1
	sta scroll_delays+5
	;
	lda scroll_delays+3
	cmp #c_MAXY
	bcs :+
	sec
	sbc scroll_delays+2
	sta scroll_delays+6
:
	rts

c_update_scrolling:
	lda scroll_framecount+1
	cmp #2
	beq c_scroll_finish
	bcs c_scroll_updated
	;
	inc scroll_framecount
	bne :+
	inc scroll_framecount+1
:	lda scroll_y+1
	clc
	adc scroll_ydelta
	sta scroll_y+1
	lda scroll_y
	adc scroll_ydelta+1
	sta scroll_y
	lda scroll_y
	cmp #$A0
	bcs c_past_end
	 lsr
	 lsr
	 lsr
	 lsr
	 tay
	 lda scroll_ydelta
	 clc
	 adc c_scroll_ydeltatable - c_DELTA - $1000,y
	 sta scroll_ydelta
	 lda scroll_ydelta+1
	 adc #0
	 sta scroll_ydelta+1
	 jmp c_scroll_updated - c_DELTA
c_past_end:
	 lda scroll_ydelta
	 sec
	 sbc #15
	 sta scroll_ydelta
	 lda scroll_ydelta+1
	 sbc #0
	 sta scroll_ydelta+1
	 jmp c_scroll_updated - c_DELTA
c_scroll_finish:
	 inc scroll_framecount+1
	 lda #238
	 sta scroll_y
c_scroll_updated:
	rts
c_scroll_ydeltatable:
	.byt 1,1,1,1,3, 6,10,15,20,24

c_read_joypad:
	ldy #1
	sty JOY1
	dey
	sty JOY1
	ldy #8
:	lda JOY1
	sta joy_temp
	lsr
	ora joy_temp
	lsr
	ror joy_cur
	dey
	bne :-
	rts


c_test_read_buffer_fade_blank_delay:
	; Because the screen will be blank for 2 seconds,
	; make some kind of sound effect to show that
	; it is somehow expected.
	
	setb SNDCHN, 3 ; Enable APU square channels 0,1
	
	jsr pre_beep_dual

	jsr populate_ciram_quick
	; Console is left hidden and rendering disabled
	set_ppuaddr $2E13
	lda PPUDATA
	
	begin_test TEST_BUFFER_DELAY_BLANK_2SECONDS

	jsr pre_beep_length
	jsr pre_beep_length
	jsr pre_beep_length
	jsr long_beep
	delay (CLOCK_RATE*161)/100
	fail_if_ppuread_y_not $8A ; $77+$13

	jsr final_beep
	begin_test TEST_BUFFER_DELAY_BLANK_1FRAME
	delay 40000 ; 1-2 frames
	lda PPUDATA
	cmp #$8B
	jmp fail_current_test_ifNE



zp_res test_position
zp_res test_ptr, 2
zp_res remaining_delay, 2

c_test_read_buffer_fade_visible_delay_callback:
	bit test_position
	bmi @elapse_delay
	.macro test_set__addr_and_load
		lda (test_ptr),y
		sta PPUADDR
		dey ;2
		lda (test_ptr),y
		sta PPUADDR
		lda PPUDATA
	.endmacro
@choose_next_test:
	begin_test TEST_BUFFER_DELAY_INTERNAL
	;bit PPUSTATUS
	; Check if we are in end of list of tests
	ldy #0
	lda (test_ptr),y
	beq @all_done
	;
	; Set flag that indicates a test is in progress
	dec test_position
	;
	; Load test specifications
	ldy #5
	lda (test_ptr),y
	sta remaining_delay+1
	dey ;4
	lda (test_ptr),y
	sta remaining_delay+0
	dey ;3
	test_set__addr_and_load
	lda PPUDATA
	ldy #1
	cmp (test_ptr),y
	jsr fail_current_test_ifNE
	ldy #3
	test_set__addr_and_load
	ldy #0
	lda (test_ptr),y
	jmp begin_test_
@all_done:
	setb cool_graphics_end_flag, 1
@done:	rts

@elapse_delay:
	lda remaining_delay+0
	sec
	sbc #1
	sta remaining_delay+0
	lda remaining_delay+1
	sbc #0
	sta remaining_delay+1
	ora remaining_delay+0
	bne @done
@read_result:
	jsr final_beep

	ldy #3
	test_set__addr_and_load
	dey ;1
	cmp (test_ptr),y
	jsr fail_current_test_ifNE
	; Reset flag, to indicate no test is in progress
	inc test_position
	; Jump to next test
	lda test_ptr
	clc
	adc #6
	sta test_ptr
	lda test_ptr+1
	adc #0
	sta test_ptr+1
	jmp @choose_next_test - c_DELTA

c_test_read_buffer_fade_visible_delay_init:
	setb test_position, 0
	sta remaining_delay+0
	sta remaining_delay+1
	setw test_ptr, test_read_buffer_fade_visible_delay_schedule
	rts


cool_graphics_end_flag = c_end_flag_loc - c_DELTA
nmi                    = c_nmi_handler - c_DELTA

show_cool_graphics_size = * - show_cool_graphics_template_begin




	
.segment "CHARS_GFX3a"
	.incbin "gfx.spr.page3_A.chr"
.segment "CHARS_GFX3b"
	.incbin "gfx.spr.page3_B.chr"

	
.segment "CHARS_SPR1"
	.incbin "gfx.spr1.chr"
.segment "CHARS_GFX1"
	.incbin "gfx.page1.chr"
.segment "CHARS_GFX2"
	.incbin "gfx.page2.chr"


.segment "CHARS_6C00"
	.incbin "gfx.6c00.bin"

.segment "CHARS_P4"
	.incbin "gfx.bank4.bin"

.segment "STACK"
 transfer_cool_graphics: .res (writer_template_end-writer_template_begin+3)&~3
 ; This function is stored in $100-$15F. Though the memory area
 ; is reserved for stack, we expect the stack to require
 ; considerably less space.
 ; There's no real REASON for it to be stored in this contrived
 ; manner other than to provide some unusual code for an emulator
 ; author to test with, and occassional fun for anyone who chooses
 ; to try to reverse engineer an assembler trace.

.segment "ZEROPAGE"
 ; Load X times Y bytes of data from PPU address $0000, store
 ; to PPU address "addr". Use temporary buffer as configured by
 ; load_transfer_templates.
 load_cool_graphics:     .res ((loader_template_end-loader_template_begin)+3)&~3
 ; This function is stored in ZEROPAGE. We don't generally
 ; use that much of zeropage room, so it is expected to be
 ; available. As a side effect, we get a linker error if
 ; the alignment did not work properly, which is good.
 ; The disclaimer about real reason applies somewhat, though.
 ; We could easily fit both of these functions in BSS.


.segment "OAM_BUF"
 
 ; OAM cache. Sprite data is calculated into this buffer,
 ; and uploaded to the PPU using the $4014 DMA.
 .align $100
 oam_buffer: .res $100

.segment "CODE_BUF"
 
 .align $100
 CoolGraphicsSpr:
 .res $100

 ; This buffer is for code that runs in the RAM.
 ; It is initialized by loading the code from VROM.
 ; The real reason disclaimer is heavily leaned on here.
 show_cool_graphics_buf:
 .res show_cool_graphics_size

 show_cool_graphics = show_cool_graphics_buf+$1800
