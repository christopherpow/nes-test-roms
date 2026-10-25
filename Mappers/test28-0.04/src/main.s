;
; Simple sprite demo for NES
; Copyright 2011, 2018 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in any source
; code copies.  This file is offered as-is, without any warranty.
;

.include "nes.inc"
.include "global.inc"
.export OAM
.exportzp das_keys, das_timer

.segment "ZEROPAGE"
oam_used:      .res 1  ; starts at 0
cur_keys:      .res 2
new_keys:      .res 2
das_keys:      .res 2
das_timer:     .res 2

; Game variables
reg00_value: .res 1
reg01_value: .res 1
reg80_value: .res 1
reg81_value: .res 1

putsdst = $02
TOPLEFTADDR = $2062

.segment "VECTORS"
  ; see VECTORS0 in wrongbanks.s for explanation
  .byt $00, $FF, $01, $01
  .addr nmi, reset, irq

.segment "CODE"
;;
; This NMI handler is good enough for a simple "has NMI occurred?"
; vblank-detect loop.  But sometimes there are things that you always
; want to happen every frame, even if the game logic takes far longer
; than usual.  These might include music or a scroll split.  In these
; cases, you'll need to put more logic into the NMI handler.
.proc nmi
  inc retraces
  rti
.endproc

; A null IRQ handler that just does RTI is useful to add breakpoints
; that survive a recompile.  Set your debugging emulator to trap on
; reads of $FFFE, and then you can BRK $00 whenever you need to add
; a breakpoint.
;
; But sometimes you'll want a non-null IRQ handler.
; On NROM, the IRQ handler is mostly used for the DMC IRQ, which was
; designed for gapless playback of sampled sounds but can also be
; (ab)used as a crude timer for a scroll split (e.g. status bar).
.proc irq
  rti
.endproc

.proc reset
  ; The very first thing to do when powering on is to put all sources
  ; of interrupts into a known state.
  sei             ; Disable interrupts
  ldx #$00
  stx PPUCTRL     ; Disable NMI and set VRAM increment to 32
  stx PPUMASK     ; Disable rendering
  stx $4010       ; Disable DMC IRQ
  dex             ; Subtracting 1 from $00 gives $FF, which is a
  txs             ; quick way to set the stack pointer to $01FF
  bit PPUSTATUS   ; Acknowledge stray vblank NMI across reset
  bit SNDCHN      ; Acknowledge DMC IRQ
  lda #$40
  sta P2          ; Disable APU Frame IRQ
  lda #$0F
  sta SNDCHN      ; Disable DMC playback, initialize other channels

vwait1:
  bit PPUSTATUS   ; It takes one full frame for the PPU to become
  bpl vwait1      ; stable.  Wait for the first frame's vblank.
  cld

  ; Do NOT clear zero page here; we're storing a signature
  ; value at $FC-$FE to distinguish warm from cold boots.
  ; But we can load the interbank code.
  lda #<__LOWCODE_LOAD__
  sta 0
  lda #>__LOWCODE_LOAD__
  sta 1
  lda #<__LOWCODE_RUN__
  sta 2
  lda #>__LOWCODE_RUN__
  sta 3
  ldx #<(65536 - __LOWCODE_SIZE__)
  lda #>(65536 - __LOWCODE_SIZE__)
  sta 5
  ldy #0
lowcodeloop:
  lda (0),y
  sta (2),y
  iny
  bne :+
  inc 1
  inc 3
:
  inx
  bne lowcodeloop
  inc 5
  bne lowcodeloop
  ldx #0
  jsr ppu_clear_oam

vwait2:
  bit PPUSTATUS  ; After the second vblank, we know the PPU has
  bpl vwait2     ; fully stabilized.
  
  ; after this point all vblank waiting should be by NMI or by
  ; sprite 0 (PPUSTATUS bit 6), not by PPUSTATUS bit 7.

  ; Load palette while in real vblank (not forced blank)
  ; to hide the rainbow streak
  lda #$3F
  sta PPUADDR
  ldx #$00
  stx PPUADDR
palloop:
  lda initial_palette,x
  sta PPUDATA
  inx
  cpx #32
  bcc palloop
  lda #0
  sta OAMADDR
  ldx #>OAM
  stx OAM_DMA 
  sta reg00_value
  sta reg01_value
  sta reg80_value
  sta reg81_value

  ; At this point, the PPU is awake, so we can at least display an
  ; error message if things go haywire.  Determine whether some basic
  ; mapper writes will make the rest of the test inaccessible.
  ; Because the 6502 has no well-defined exception mechanism, we have
  ; to use careful "look before you leap" (LBYL) coding instead of
  ; the Python idiom "easier to ask forgiveness than permission"
  ; (EAFP).
  jsr try_returning

  ; Test for bus conflicts, or cases where the ROM interferes with
  ; the value that the CPU is writing.
  jsr verify_bus_conflict
  lda test_result
  bne at1_failure_1
no_bus_conflicts:

  ; Detect how much CHR RAM is present and make sure that CHR RAM
  ; banking doesn't interfere with PRG ROM banking.
  ; 5: Writing to CHR bank register swaps out PRG ROM
  ; 6: Values read back don't match the expected mirroring pattern
  ; for 8K, 16K, or 32K
  jsr detect_chr_ram_size
  beq known_chr_ram_size
at1_failure_1:
  jmp at1_failure
known_chr_ram_size:

  ; set_chr_bank_x calls try_returning as a side effect, and if
  ; the mapper is implemented correctly, try_returning left us in
  ; horizontal mirroring mode.  Both horizontal and vertical
  ; mirroring have two distinct nametables at $2000 and $2C00.
  ; To distinguish the two nametables, write $00 to one and $01
  ; to the other.  The rest of the tests will expect these.
  jsr load_mirror_sentinels
  sta PPUADDR
  sty PPUADDR  ; seek to $2000
  bit PPUDATA  ; dummy read
  lda PPUDATA  ; read $2000
  beq not_stuck_in_1_screen
  ldy #<msg_mirror_stuck
  lda #>msg_mirror_stuck
  bne puts_and_freeze
not_stuck_in_1_screen:

  ; Awake PPU and usable mirroring.  We can call puts_16 and
  ; puts_multiline_16 now to display error messages.

  ; The first test is whether the user was too hasty in pressing
  ; the reset button.  First tell a cold boot from a warm boot.
  lda sig
  cmp #<sigvalue
  bne is_cold_boot
  lda sig+1
  cmp #>sigvalue
  bne is_cold_boot
  lda test_in_progress
  bne is_hasty
  ldy #<msg_warm_boot_last
  lda #>msg_warm_boot_last
  bne puts_and_freeze

is_hasty:
  ldy #<msg_not_so_fast
  lda #>msg_not_so_fast
  bne puts_and_freeze

puts_and_freeze:
  pha
  tya
  pha
  jsr load_font_16
  pla
  tay
  pla
  ldx #$21
  stx 3
  ldx #$02
  stx 2
  jsr puts_multiline_16
freeze:
  ldx #0
  ldy #0
  lda #VBLANK_NMI
  sec
  jsr ppu_screen_on
forever:
  jmp forever

is_cold_boot:

  ; Yay.  We get to begin a new test.  Write the signature to
  ; signal that the test is in progress.
  lda #<sigvalue
  sta sig
  lda #>sigvalue
  sta sig+1
  lda #1
  sta test_in_progress
  
  ; Now load the font used for interactive tests, clear the screen,
  ; and reload the mirroring sentinels.
  jsr load_font_16
  jsr load_mirror_sentinels

.endproc
.proc interactive_test
  lda #$20
  sta 3
  lda #$62
  sta 2
  ldy #<msg_interactive_test
  lda #>msg_interactive_test
  jsr puts_multiline_16

  lda #>msg_mode_outer_bank
  sta 1
  lda #<msg_mode_outer_bank
  sta 0
  lda #$21
  ldx #$42
  jsr puts_16

new_regvalues:
  lda #VBLANK_NMI
  sta PPUCTRL
  asl a
  sta PPUMASK
  lda #<reg80_value
  sta 4
  lda #>reg80_value
  sta 5
  lda #$21
  ldx #$48
  ldy #1
  jsr hexdump8
  lda #<reg81_value
  sta 4
  lda #>reg81_value
  sta 5
  lda #$21
  ldx #$59
  ldy #1
  jsr hexdump8
  
  ldx reg80_value
  ldy reg81_value
  jsr read_mode_x_bank_y
  jsr write_out80_outC0
  jsr verify_bank80c0
  sta failure_details+0
  stx failure_details+1
  .if 0
    jsr verify_mirror_ignores_bit3210
    stx failure_details+2
  .else
    jsr get_mirror_type
    sta failure_details+2
  .endif
  
  ; if A is nonzero, it's the bank that had a problem
  ; (80, C0), and X 
  lda #<failure_details
  sta 4
  lda #>failure_details
  sta 5
  sta $0F
  lda #$23
  ldx #$2B
  ldy #3
  jsr hexdump8
  sta $0E
  sta $0F

loop:
  lda retraces
:
  cmp retraces
  beq :-
  lda #VBLANK_NMI|BG_0000
  ldx #0
  ldy #0
  clc
  jsr ppu_screen_on
  jsr read_pads
  ldx #0
  jsr autorepeat
  lda new_keys
  and #KEY_START|KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT
  beq loop
  
  lsr a
  bcc notRight
  inc reg80_value
  lda reg80_value
  and #$3F
  sta reg80_value
  jmp new_regvalues
notRight:
  
  lsr a
  bcc notLeft
  dec reg80_value
  lda reg80_value
  and #$3F
  sta reg80_value
  jmp new_regvalues
notLeft:
  
  lsr a
  bcc notDown
  dec reg81_value
  lda reg81_value
  and #$3F
  sta reg81_value
  jmp new_regvalues
notDown:
  
  lsr a
  bcc notUp
  inc reg81_value
  lda reg81_value
  and #$3F
  sta reg81_value
  jmp new_regvalues
notUp:
  ; then it must have been start. Go on to the automatic test.
.endproc
.proc auto_tests
  lda retraces
:
  cmp retraces
  beq :-
  lda #0    ; hide tiles, also nametable and attribute fill value
  sta PPUMASK
  tay       ; attribute fill value
  ldx #$20  ; nametable address
  jsr ppu_clear_nt
  ldx #0
  jsr set_chr_bank_x
  jsr setup_oam_for_test
  lda #>msg_auto_test_1
  ldy #<msg_auto_test_1
  ldx #$21
  stx 3
  ldx #$02
  stx 2
  jsr puts_multiline_16
  jsr run_auto_test_1
  bne at1_failure

  lda #0    ; hide tiles, also nametable and attribute fill value
  sta PPUMASK
  tay       ; attribute fill value
  ldx #$20  ; nametable address
  jsr ppu_clear_nt
  jsr run_auto_test_2
  bne at1_failure
  jsr run_auto_test_3
  bne at1_failure

  lda #0
  sta PPUMASK
  tay       ; attribute fill value
  ldx #$20  ; nametable address
  jsr ppu_clear_nt

  ; Write message and freeze in bank 0
  
  jsr load_font_16
  lda #>msg_one_last_thing
  ldy #<msg_one_last_thing
  ldx #$21
  stx 3
  ldx #$02
  stx 2
  jsr puts_multiline_16
freeze:
  ldx #0
  ldy #0
  lda #VBLANK_NMI
  sec
  jsr ppu_screen_on
  lda #0
  sta test_in_progress
  jmp freeze_in_bank_0

  jmp reset::puts_and_freeze

.if 0
  ldx #0
  jsr set_chr_bank_x
  jsr write_out80_outC0
  ldy #<failure_details
  lda #>failure_details
  sty 4
  sta 5
  lda #$23
  ldx #$02
  ldy #8
  jsr hexdump8
  jmp reset::freeze
.endif
.endproc
.proc at1_failure
  ; Reload the font in case a test screwed up the CHR bank,
  ; clear the screen, and display the error message.
  pha
  lda #0
  sta PPUMASK
  jsr load_font_16
  pla
  cmp #5
  bcs dont_write_mode_outer
  pha

  ; Write outer bank and address where it failed
  lda #>msg_mode_outer_bank
  sta 1
  lda #<msg_mode_outer_bank
  sta 0
  lda #$23
  ldx #$22
  jsr puts_16
  lda #<reg80_value
  sta 4
  lda #>reg80_value
  sta 5
  lda #$23
  ldx #$28
  ldy #1
  jsr hexdump8
  lda #<reg81_value
  sta 4
  lda #>reg81_value
  sta 5
  lda #$23
  ldx #$39
  ldy #1
  jsr hexdump8
  pla

dont_write_mode_outer:
  asl a
  adc #<-2
  tax

  ; Write the start of the failure message
  lda msg_auto_test_failures+1,x
  ldy msg_auto_test_failures,x
  ldx #$20
  stx 3
  ldx #$82
  stx 2
  jsr puts_multiline_16
  lda #0
  sta $0F
detailsloop:

  ; Move to the next error description and finish if empty
  sec
  tya
  adc 0
  sta 0
  ldy #0
  tya
  adc 1
  sta 1
  lda (0),y
  beq detailsdone

  lda 1  ; Save next error description because hexdump8
  pha    ; will lose it
  lda 0
  pha
  lda #failure_details
  clc
  adc $0F
  inc $0F
  sta 4
  lda #$00
  sta 5
  lda 2
  and #$E0
  ora #$18
  tax
  lda 3
  ldy #1
  jsr hexdump8
  pla
  sta 0
  pla
  sta 1

  lda 2
  and #$E0
  ora #$04
  tax
  lda 3
  jsr puts_16
  lda 2
  clc
  adc #64
  sta 2
  bcc :+
  inc 3
:
  jmp detailsloop
detailsdone:

  lda #VBLANK_NMI
  sta PPUCTRL
  lda retraces
:
  cmp retraces
  beq :-
  jmp reset::freeze
.endproc

.proc write_out80_outC0
  ldy #<out80
  lda #>out80
  sty 4
  sta 5
  lda #$21
  ldx #$E7
  ldy #8
  jsr hexdump8
  ldy #<out80+8
  lda #>out80+8
  sty 4
  sta 5
  lda #$22
  ldx #$27
  ldy #8
  jsr hexdump8
  ldy #<outC0
  lda #>outC0
  sty 4
  sta 5
  lda #$22
  ldx #$A7
  ldy #8
  jsr hexdump8
  ldy #<outC0+8
  lda #>outC0+8
  sty 4
  sta 5
  lda #$22
  ldx #$E7
  ldy #8
  jsr hexdump8
  rts
.endproc

; TEXT OUTPUT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; Loads an 8x16 pixel 2bpp ASCII font into $40-$FF, clears the rest
; of the pattern table, and clears both nametables.
.proc load_font_16
  ; Load the font
  lda #<font16_chr
  sta 0
  lda #>font16_chr
  sta 1
  lda #$04
  sta PPUADDR
  ldy #$00
  sty PPUADDR
  ldx #>(font16_chr_end - font16_chr)
fontcopyloop:
  lda (0),y
  sta PPUDATA
  iny
  bne fontcopyloop
  inc 1
  dex
  bne fontcopyloop

  ldx #$20  ; nametable address
  lda #$00  ; nametable fill value
  tay       ; attribute fill value
  jsr ppu_clear_nt
  ldx #$2C
  lda #$00
  tay
  jsr ppu_clear_nt

  ; Also clear the unused part of the pattern table (tiles $00-$3F),
  ; which is the same size as a nametable.
  ldx #$00  ; pattern table address
  txa
  tay
  jmp ppu_clear_nt
.endproc

;;
; Writes the string at (AAYY) to lines starting at 2 and 3.
; At finish, 0 and 1 points to start of last line, and Y is the
; length of the last line.
.proc puts_multiline_16
srclo = 0
srchi = 1
dstlo = 2
dsthi = 3
  sta srchi
  sty srclo
lineloop:
  lda dsthi
  ldx dstlo
  jsr puts_16
  lda dstlo
  clc
  adc #64
  sta dstlo
  bcc :+
  inc dsthi
:
  lda (srclo),y
  beq done
  tya
  sec
  adc srclo
  sta srclo
  bcc lineloop
  inc srchi
  bcs lineloop
done:
  rts
.endproc

;;
; Writes the string at (0) to the nametable at AAXX.
; Does not write to memory.
.proc puts_16
  sta PPUADDR
  stx PPUADDR
  pha
  txa
  pha
  ldy #0
copyloop1:
  lda (0),y
  cmp #' '
  bcc after_copyloop1
  asl a
  sta PPUDATA
  iny
  bne copyloop1
after_copyloop1:
  
  pla
  clc
  adc #32
  tax
  pla
  adc #0
  sta PPUADDR
  stx PPUADDR
  ldy #0
copyloop2:
  lda (0),y
  cmp #' '
  bcc after_copyloop2
  rol a
  sta PPUDATA
  iny
  bne copyloop2
after_copyloop2:
  rts
.endproc

;;
; Writes the Y (1-8) bytes at (4) in spaced hex to the nametable at AAXX.
; Modifies $0002-$000B.
.proc hexdump8
strlo = 0
strhi = 1
dstlo = 2
dsthi = 3
hexsrc = 4
hexoffset = 6
hexbuf = 7
bytesleft = 11
  stx dstlo
  sta dsthi
  sty bytesleft
  lda #0
  sta hexbuf+2
  sta strhi
  sta hexoffset
  lda #<hexbuf
  sta 0
  lda #32
  sta hexbuf+3
  ldy hexoffset
loop:
  lda (hexsrc),y
  lsr a
  lsr a
  lsr a
  lsr a
  jsr hdig
  sta hexbuf+0
  lda (hexsrc),y
  iny
  sty hexoffset
  and #$0F
  jsr hdig
  sta hexbuf+1
  
  lda dstlo
  tax
  clc
  adc #3
  sta dstlo
  lda dsthi
  jsr puts_16
  ldy hexoffset
  dec bytesleft
  bne loop
  rts
hdig:
  cmp #10
  bcc :+
  adc #'A'-'0'-11
:
  adc #'0'
  rts
.endproc

.segment "RODATA"
font16_chr:
  .incbin "obj/nes/font16.chr"
font16_chr_end:

initial_palette:
  .byt $0F,$1a,$2a,$3a,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F
  .byt $0F,$1a,$2a,$3a,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F

; Error messages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Messages if it thinks there's a bus conflict
;      "[                          ]"
msg_mirror_stuck:
  .byt "Fail!",LF
  .byt "Stuck in 1-screen mirroring.",LF
  .byt "Writing reg80=$FF should",LF
  .byt "enable horizontal mirroring.",0
msg_not_so_fast:
  .byt "Not So Fast!",LF
  .byt "Please turn the power off,",LF
  .byt "and next time don't press",LF
  .byt "Reset until asked.",0
msg_warm_boot_last:
  .byt "Hmmm...",LF
  .byt "CPU reset changes to last",LF
  .byt "bank.  Not sure if want.",LF
  .byt "Please turn the power off.",0
msg_one_last_thing:
  .byt "One last thing:",LF
  .byt "Please press the Reset",LF
  .byt "Button on the Control Deck.",LF
  .byt "(Yeah, like Genesis X-Men.)",0

msg_interactive_test:
  .byt "INL-ROM Action 53 Mapper",LF
  .byt "Interactive behavior test",LF
  .byt $7F," 2012, 2018 Damian Yerrick",LF
  .byt LF,LF
  .byt "16K banks at $8000-$BFFF",LF
  .byt "0-7:",LF
  .byt "8-F:",LF
  .byt "16K banks at $C000-$FFFF",LF
  .byt "0-7:",LF
  .byt "8-F:",LF
  .byt "Details:",0  
  
msg_mode_outer_bank:
  .byt "Mode $     Outer bank $",0

msg_auto_test_1:
  .byt "Automatic test in progress",LF,LF
  .byt "Testing PRG bank switching",LF
  .byt "for all combinations of",LF
  .byt "mode, outer bank, and inner",LF
  .byt "bank.  Give it a minute.",0

msg_auto_test_failures:
  ; 1-10:
  .addr msg_at1_not_ignore_d3d0, msg_at1_wrong_mirroring
  .addr msg_at1_not_ignore_d4, msg_at1_wrong_prgbank
  .addr msg_at2_chr_overwrites_last, msg_at2_chr_wrong_read
  .addr msg_knownval_wrong, msg_bus_conflict_and
  .addr msg_knownval_wrong, msg_bus_conflict_or
  ; 11-13:
  .addr msg_at2_1sm_mode, msg_at2_chr_d4
  .addr msg_at2_chr_overwrites_inner
msg_at1_not_ignore_d3d0:
  .byt "Fail!",LF
  .byt "Nametable mirroring should",LF
  .byt "ignore inner bank bits 3-0.",0
  .byt "Nametable:",0
  .byt "Inner bank:",0,0
msg_at1_wrong_mirroring:
  .byt "Fail!",LF
  .byt "Nametable mirroring is",LF
  .byt "wrong for this mode.",LF
  .byt "CC: vertical  F0: horizontal",LF
  .byt "E4: D4 selects V/H",LF
  .byt "00/FF: 1-screen ignoring D4",LF
  .byt "AA: 1-screen D4 controlled",0
  .byt "Actual:",0
  .byt "Expected:",0,0
msg_at1_not_ignore_d4:
  .byt "Fail!",LF
  .byt "PRG bank switching should",LF
  .byt "ignore inner bank bit 4.",0
  .byt "Bank base (80/C0):",0
  .byt "Inner bank D3-D0:",0,0
msg_at1_wrong_prgbank:
  .byt "Fail!",LF
  .byt "Wrong 16K PRG bank for this",LF
  .byt "combination of mode, outer",LF
  .byt "bank, and inner bank.",0
  .byt "Address (80/C0):",0
  .byt "Inner bank:",0
  .byt "Actual 16K bank:",0
  .byt "Expected 16K bank:",0,0
msg_at2_chr_overwrites_last:
  .byt "Fail!",LF
  .byt "Setting the CHR bank somehow",LF
  .byt "changes the PRG ROM bank",LF
  .byt "in $C000.",0,0
msg_at2_chr_overwrites_inner:
  .byt "Fail!",LF
  .byt "Setting the CHR bank somehow",LF
  .byt "changes the PRG ROM bank",LF
  .byt "in $8000.",0
  .byt "Written:",0
  .byt "Read back:",0,0
msg_at2_chr_wrong_read:
  .byt "Fail!",LF
  .byt "Data read back from $1010",LF
  .byt "in CHR RAM did not match",LF
  .byt "what was written.",LF
  .byt "Expected",LF
  .byt "Bank 8K 16K 32K    Actual",0
  .byt   "0  10  10  10",0
  .byt   "1  10  11  11",0
  .byt   "2  10  10  12",0
  .byt   "3  10  11  13",0,0
msg_knownval_wrong:
  .byt "Fail!",LF
  .byt "$FFF6 and $FFF7 should have",LF
  .byt "$00 and $FF.  They don't.",LF
  .byt "A13 problem?",0,0
msg_bus_conflict_and:
  .byt "Fail!",LF
  .byt "Writing $FF over a $00 value",LF
  .byt "in ROM did not swap in the",LF
  .byt "last bank.  Bus conflict?",0,0
msg_bus_conflict_or:
  .byt "Fail!",LF
  .byt "Writing $00 over a $FF value",LF
  .byt "in ROM did not swap in the",LF
  .byt "first bank.  Bus conflict?",0,0
msg_at2_1sm_mode:
  .byt "Fail!",LF
  .byt "Page in 1-screen mirroring",LF
  .byt "should use the last value",LF
  .byt "written to $00 D4, $01 D4,",LF
  .byt "or $80 D0.",0
  .byt "Actual:",0
  .byt "Expected:",0,0
msg_at2_chr_d4:
  .byt "Fail!",LF
  .byt "Nametable mirroring using",LF
  .byt "reg00 D4 is wrong for this",LF
  .byt "mode.",0
  .byt "Actual:",0
  .byt "Expected:",0
  .byt "Mode:",0,0
