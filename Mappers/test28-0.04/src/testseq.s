
.include "nes.inc"
.include "global.inc"

AT1_SHOW_CPU_USAGE = 0

.segment "CODE"

; Test phase 1: PRG bank and basic nametable mirroring ;;;;;;;;;;;;;;

;;
; Determines what 16 KiB PRG ROM bank ought to be mapped into a given
; CPU address with a given set of $80, $81, $00 values.
; @param A $80 value
; @param X $01 value
; @param Y $81 value
; @param C CPU A14 value: clear for $8000-$BFFF, set for $C000-$FFFF
; @return PRG bank number in A, Y unchanged, X changed
.proc calc_prg_bank
bank_mode = 0
outer_bank = 1
current_bank = 2
  stx current_bank
  sty outer_bank
  rol outer_bank
  lsr a  ; discard mirroring bits
  lsr a
  sta bank_mode

  ; If the mode is UxROM (10 = mapper 180, 11 = mapper 2), and bit 0
  ; of the mode matches CPU A14, then the read is within the fixed
  ; bank.  For such reads, the mapper acts in 32K (NROM) mode.
  and #$02
  beq not_unrom
  lda outer_bank
  eor bank_mode
  ; If bit 0 of the eor result is false, there is a match, so
  ; fall through to the not-UNROM code.
  and #$01
  bne have_current_bank
  sta bank_mode

not_unrom:
  ; In 32K switched modes (NROM, CNROM, BNROM, AOROM),
  ; shift CPU A14 into the current bank
  lda outer_bank
  lsr a
  rol current_bank
  
have_current_bank:
  lda bank_mode
  lsr a
  lsr a
  and #$03
  tax
  lda current_bank
  eor outer_bank
  and bank_size_masks,x
  eor outer_bank
  rts
.endproc

.segment "RODATA"
bank_size_masks: .byt $01, $03, $07, $0F

.segment "CODE"
.if 0
;;
; Who tests the tests?
; Run a few predefined values.
; The tests were last correlated to docs/calc_prg_bank.py
; on 2012-10-14.
.proc sanity_self_test
current_bank_counter = 3
  ldx #15

loop:
  stx current_bank_counter
  lda #$30
  ldy #$2a
  clc
  jsr calc_prg_bank
  ldx current_bank_counter
  sta out80,x
  lda #$30
  ldy #$2a
  sec
  jsr calc_prg_bank
  ldx current_bank_counter
  sta outC0,x
  dex
  bpl loop
  rts
.endproc
.endif

;;
; Sets up the OAM and nametable for a sprite 0 hit 16 lines above
; the bottom of the screen.
.proc setup_oam_for_test

  ; Prepare three sprites: sprite 0 and three progress bars
  lda #240-8-16-1
  sta OAM+0
  lda #240-8-16-3
  sta OAM+4
  lda #240-8-16-5
  sta OAM+8
  lda #('_' << 1) | 1
  sta OAM+1
  sta OAM+5
  sta OAM+9
  lda #1
  sta OAM+2
  lsr a
  sta OAM+6
  sta OAM+10
  lda #3
  sta OAM+3
  lda #0
  sta OAM+7
  sta OAM+11
  ldx #12
  jsr ppu_clear_oam

  ; Write something for sprite 0 to collide with
  lda #$23
  sta PPUADDR
  lda #$60
  sta PPUADDR
  lda #('_' << 1) | 1
  sta PPUDATA
  ldx #0
  stx OAMADDR
  lda #>OAM
  sta OAM_DMA
  rts
.endproc

current_bank_mode = reg80_value
current_outer_bank = reg81_value

.proc run_auto_test_1
  lda #VBLANK_NMI
  ldx #0
  ldy #0
  stx reg80_value
  stx reg81_value
  sec
  jsr ppu_screen_on

okay_stable:
  lda reg81_value
  asl a
  adc reg81_value
  adc #16
  sta OAM+7
  lda reg80_value
  asl a
  adc reg80_value
  adc #16
  sta OAM+11
  jsr at1_iteration
  bne at1_failure
  
  inc reg81_value
  lda last_bank_number
  lsr a
  and reg81_value
  sta reg81_value
  bne okay_stable

  inc reg80_value
  lda reg80_value
  and #$3F
  sta reg80_value
  bne okay_stable
at1_failure:
  rts
.endproc

.segment "ZEROPAGE"
failure_details: .res 8

.segment "CODE"
;;
; Waits for sprite 0 hit and then runs a test of writes for a
; particular value of $80 (mode) and $81 (outer bank) and all 32
; values of $01 (inner bank and 1-screen page).
;
; It returns an error code in A, and it writes up to 8
; bytes of details to failure_details.  This table explains all
; error types.
; Type 00: Success
; Type 01: NT mirroring doesn't ignore D3-D0
;   0. Nametable where detected
;   1. Inner bank where detected
; Type 02: NT mirroring doesn't match expected value for current mode
;   0. Actual 8-bit value for this mode
;   1. Value from mirror_type_expect for this mode
; Type 03: PRG bank doesn't ignore D4
;   0. A15-A8 of base of wrong PRG bank (either $80 or $C0)
;   1. D3-D0 of inner bank for which $0x doesn't match $1x
; Type 04: PRG bank doesn't match expected value for current mode
;   0. A15-A8 of base of wrong PRG bank (either $80 or $C0)
;   1. D3-D0 of inner bank for which mode doesn't match
;   2. Actual bank number
;   3. Expected bank number

.proc at1_iteration
  ldx reg80_value
  ldy reg81_value
s0wait0:
  bit PPUSTATUS
  bvs s0wait0
s0wait1:
  bit PPUSTATUS
  bvc s0wait1

.if ::AT1_SHOW_CPU_USAGE
  lda #LIGHTGRAY
  sta PPUMASK
  lsr a
.else
  lda #0
.endif
  sta PPUMASK
  jsr read_mode_x_bank_y

  ; We have just long enough left in vblank to squeeze in an OAM DMA
  ; to notify the user of progress.
  ldy #0
  sty OAMADDR
  lda #>OAM
  sta OAM_DMA

  ; turn rendering back on for the next frame,
  ; so that sprite 0 triggers again
  lda #VBLANK_NMI
  sta PPUCTRL
  sty PPUSCROLL
  sty PPUSCROLL
  lda #BG_ON|OBJ_ON
  sta PPUMASK

  ; Now we have several thousand cycles to interpret the
  ; values read back from the ROM banks and nametables.
  jsr verify_mirror_ignores_bit3210
  beq ok01
  txa
  and #$1F
  sta failure_details+1
  txa
  and #$60
  lsr a
  lsr a
  lsr a
  ora #$20
  sta failure_details+1
  lda #ATFAIL_PRG2MIRROR
  rts
ok01:

.if ::AT1_SHOW_CPU_USAGE
  lda #BG_ON|OBJ_ON|LIGHTGRAY
  sta PPUMASK
.endif
  jsr get_mirror_type
  sta failure_details+0
  lda #$03
  and current_bank_mode
  tax
  lda mirror_type_expect,x
  cmp failure_details+0
  beq ok02
  sta failure_details+1
  lda #ATFAIL_WRONG_MIRROR
  rts
ok02:

.if ::AT1_SHOW_CPU_USAGE
  lda #BG_ON|OBJ_ON
  sta PPUMASK
.endif
  jsr verify_prg_bank_ignores_D4
  beq ok03
  sta failure_details+0
  stx failure_details+1
  lda #ATFAIL_MIRROR2PRG
  rts
ok03:

.if ::AT1_SHOW_CPU_USAGE
  lda #BG_ON|OBJ_ON|LIGHTGRAY
  sta PPUMASK
.endif
  jsr verify_bank80c0
  beq ok04
  sta failure_details+0
  stx failure_details+1
  and #$40
  lsr a
  ora failure_details+1
  tax
  lda out80,x
  sta failure_details+2
  lda failure_details+0
  cmp #$C0
  lda reg80_value
  ldy reg81_value
  ldx failure_details+1
  jsr calc_prg_bank
  sta failure_details+3
  lda #ATFAIL_WRONG_PRG
  rts
ok04:

.if ::AT1_SHOW_CPU_USAGE
  lda #BG_ON|OBJ_ON
  sta PPUMASK
.endif
  lda #0
  rts
.endproc

;;
; Test 01: Verifies that bits 0-3 of current_bank (reg $01)
; don't affect nametable mirroring.
; @return X = 0 for success, >0 for failure
.proc verify_mirror_ignores_bit3210
  ldx #127
loop:
  txa
  and #$70
  tay
  lda out20,x
  eor out20,y
  and #$10
  bne done
  dex
  bne loop
done:
  rts
.endproc

;;
; Test 02: Get the mirroring type in A.
; 7654 3210
; |||| |||+- $01.D4 = 0, $2000
; |||| ||+-- $01.D4 = 1, $2000
; |||| |+--- $01.D4 = 0, $2400
; |||| +---- $01.D4 = 1, $2400
; |||+------ $01.D4 = 0, $2800
; ||+------- $01.D4 = 1, $2800
; |+-------- $01.D4 = 0, $2C00
; +--------- $01.D4 = 1, $2C00
; 00: 1-screen page 0 regardless of D4
; FF: 1-screen page 1 regardless of D4
; AA: 1-screen controlled by D4
; CC: Vertical mirroring
; F0: Horizontal mirroring
.proc get_mirror_type
  lda #$00
  tax
  pha
loop:
  lda out20,x
  lsr a
  pla
  ror a
  pha
  txa
  clc
  adc #16
  tax
  bpl loop
  pla
  rts
.endproc

;;
; Loads $00 into $2000 and $01 into $2C00.
; @return A=$20, Y=$00
.proc load_mirror_sentinels
  lda #$20
  ldy #$00
  sta PPUADDR
  sty PPUADDR  ; seek to $2000
  sty PPUDATA  ; write $00
  ldx #$2C
  stx PPUADDR
  sty PPUADDR  ; seek to $2C00
  ldx #1
  stx PPUDATA  ; write $01
  rts
.endproc

.segment "RODATA"
mirror_type_expect:
  .byt %10101010 ; 1sc
  .byt %10101010 ; 1sc
  .byt %11001100 ; vertical
  .byt %11110000 ; horizontal

.segment "CODE"

;;
; Test 03: Verifies that bits 0-3 of current_bank (reg $01)
; don't affect nametable mirroring.
; @return for success: A=X=0
;         for failure: A=high byte of bank base ($80 or $C0)
;         and X=inner bank that mismatched ($01-$0F)
.proc verify_prg_bank_ignores_D4
  ldx #15
loop:
  lda out80,x
  cmp out80+16,x
  beq ok80
  lda #$80
  jmp done
ok80:
  lda out80,x
  cmp out80+16,x
  beq okC0
  lda #$C0
  jmp done
okC0:
  dex
  bne loop
  lda #0
done:
  rts
.endproc

;;
; Test 04: Compares the PRG bank values read back from the mapper
; to the expected values for the current bank mode and outer bank.
; @return A: 0 if success; $80 if failed in $8000; 
;            $C0 if failed in $C000
;         X: one bank value on which it mismatched
;         Y: Expected value there
.proc verify_bank80c0
curbank = 3
  ldx #15
loop:
  stx curbank
  lda current_bank_mode
  ldy current_outer_bank
  clc
  jsr calc_prg_bank
  and last_bank_number
  ldx curbank
  cmp out80,x
  beq ok80
  tay
  lda #$80
  jmp done
ok80:
  lda current_bank_mode
  ldy current_outer_bank
  sec
  jsr calc_prg_bank
  and last_bank_number
  ldx curbank
  cmp outC0,x
  beq okC0
  tay
  lda #$C0
  jmp done
okC0:
  dex
  bpl loop
  lda #0
done:
  rts
.endproc

; Test phase 2: CHR bank and mirroring edge cases ;;;;;;;;;;;;;;;;;;;

.segment "ZEROPAGE"
chr_ram_last_bank: .res 1
.segment "CODE"
.proc load_chr_ram_sentinels
  lda #0
  sta PPUMASK
  ldx #3
load_loop:
  jsr set_chr_bank_x
  beq :+
  lda #ATFAIL_PRG_CHANGED
  rts
:
  lda #$10
  sta PPUADDR
  sta PPUADDR
  txa
  ora #$10
  sta PPUDATA
  dex
  bpl load_loop
  lda #0
  rts
.endproc

;;
; This is run to set up the second auto-test to detect the effective
; size of CHR RAM.
; Failure type 5 (no details): Writing to a CHR banking register
; changes the last bank of PRG ROM.
; Failure type 6: What is read back does not match the expected value
; for 8K, 16K, or 32K CHR RAM.
;   0-3. Value in bank 0-3
.proc detect_chr_ram_size
  jsr load_chr_ram_sentinels
  bne knownrts
  ldx #3
readbackloop:
  jsr set_chr_bank_x
  beq :+
  lda #ATFAIL_PRG_CHANGED
knownrts:
  rts
:
  lda #$10
  sta PPUADDR
  sta PPUADDR
  lda PPUDATA
  lda PPUDATA
  sta failure_details,x
  dex
  bpl readbackloop
  
  ; First must be $10
  lda failure_details+0
  cmp #$10
  beq pass0
fail_readback:
  lda #ATFAIL_BAD_CHR_SIZE
  rts
pass0:
  ; Last must be $10, $11, or $13
  eor failure_details+3
  cmp #2
  beq fail_readback
  cmp #4
  bcs fail_readback
  sta chr_ram_last_bank
  tax
  sta failure_details+7
  lda expected_bank_1_by_size,x
  cmp failure_details+1
  bne fail_readback
  lda expected_bank_2_by_size,x
  eor failure_details+2
  bne fail_readback
  
  ; So at this point we know CHR is either 8K, 16K, or 32K.
  
  lda #0
  rts
.endproc

.segment "RODATA"
;                              8K  16K  xxx  32K
expected_bank_1_by_size: .byt $10, $11, $A5, $11
expected_bank_2_by_size: .byt $10, $10, $A5, $12

.segment "CODE"

.proc run_auto_test_2
  jsr try_returning  ; ensure horizontal mirroring
  jsr load_mirror_sentinels
  jsr load_chr_ram_sentinels
  lda #0

  ; Make sure 1-screen mirroring modes $00 and $01 don't have
  ; individual D4s, reg00 and reg01 don't have individual D4s,
  ; and mode overrides D4 if mode is written last.  Do this by
  ; writing one 1-screen mode to reg80, writing the opposite page
  ; to both reg00 and reg01, writing the original mode back to
  ; reg80, and reading the mirroring sentinels.

  ; Set mode=$3C, reg00=$10, reg01=$10, mode=$3C, then fail if
  ; mirroring is not 1-screen page 0.
  lda #0
  jsr opposing_1sm_test
  bne known_rts_1
  jsr get_mirror_type_once
  cmp #$00
  beq pass_1s0_mode
  ldx #$00
have_fd1:
  sta failure_details+0
  stx failure_details+1
  lda #ATFAIL_1SM_MODE
known_rts_1:
  rts
pass_1s0_mode:

  ; Set mode=$3D, reg00=$00, reg01=$00, mode=$3D, then fail if
  ; mirroring is not 1-screen page 1.
  lda #1
  jsr opposing_1sm_test
  bne known_rts_1
  jsr get_mirror_type_once
  cmp #$FF
  beq pass_1s1_mode
  ldx #$FF
  bne have_fd1
pass_1s1_mode:
  
  ; Make sure reg00 D4 affects mirroring in 1-screen but not H/V.
  ; For each mode $3C through $3F, for each reg00 in $00 and $10,
  ; write mode and reg00 and read back mirroring.
  jsr at2_reg00_d4_test
  bne known_rts_1
  
  ; Make sure CHR bank does not affect inner bank.
  ; Write mode $3F, then for each reg00 in $00-$0F, read back the
  ; inner bank.
  jsr at2_test_chr_to_inner
  bne known_rts_1
  
  lda #0
  rts
.endproc

;;
; Write a 1-screen mirroring mode to mode, write the opposite to CHR
; bank and inner bank, and write it to mode again.
.proc opposing_1sm_test
  and #$01
  ora #$3C
  sta reg80_value
  tax
  lda #$80
  jsr safe_set_a_to_x
  bne bankfail
  txa
  ldx #$10
  and #$01
  eor #$3D
  beq :+
  ldx #$00  ; X = opposite
:
  lda #$01
  jsr safe_set_a_to_x
  bne bankfail
  jsr set_chr_bank_x
  bne bankfail
  lda #$80
  ldx reg80_value
  jsr safe_set_a_to_x
  bne bankfail
  rts

bankfail:
  lda #ATFAIL_PRG_CHANGED
  rts
.endproc

;;
; Read the mirror type directly from VRAM,
; not once for each D4.
.proc get_mirror_type_once
  lda #0
  sta test_result
  bit PPUSTATUS
  ldx #$20
  ldy #$00
loop:
  stx PPUADDR
  sty PPUADDR
  lda PPUDATA
  lda PPUDATA
  lsr a
  ror test_result
  ror test_result
  txa
  clc
  adc #4
  tax
  cmp #$30
  bcc loop
  lda test_result
  asl a
  ora test_result
  rts
.endproc

.proc at2_reg00_d4_test
  ldx #$3C
reg00_d4_loop:
  stx reg80_value
  lda #$80
  jsr safe_set_a_to_x
  beq :+
bankfail:
  lda #ATFAIL_PRG_CHANGED
  rts
:
  ldx #$00
  jsr set_chr_bank_x
  bne bankfail
  jsr get_mirror_type_once
  and #$55
  sta failure_details+0
  ldx #$10
  jsr set_chr_bank_x
  bne bankfail
  jsr get_mirror_type_once
  and #$AA
  ora failure_details+0
  sta failure_details+0
  
  lda reg80_value
  and #$03
  tax
  lda mirror_type_expect,x
  cmp failure_details+0
  beq correct_d4
  sta failure_details+1
  lda reg80_value
  sta failure_details+2
  lda #ATFAIL_CHR_D4
  rts
correct_d4:
  ldx reg80_value
  inx
  cpx #$40
  bcc reg00_d4_loop
  lda #0
  rts
.endproc

.proc at2_test_chr_to_inner
  lda #$81
  ldx #$3F
  jsr safe_set_a_to_x  
  bne bankfail
  lda #$80
  ldx #$3F
  jsr safe_set_a_to_x  
  bne bankfail
  
  ldx #$0F
loop:
  stx failure_details+0
  ; Set inner bank X
  lda #$01
  jsr safe_set_a_to_x  
  bne bankfail

  ; Set CHR to opposite
  txa
  eor #$0F
  tax
  jsr set_chr_bank_x  
  bne bankfail
  txa
  eor #$0F
  tax
  
  ; Read back inner bank
  txa
  eor $BFF8
  and $FFF8
  and #$0F
  beq correct_inner_bank
  lda $BFF8
  sta failure_details+1
  lda #ATFAIL_INNER_CHANGED
  rts

correct_inner_bank:
  dex
  bpl loop
  lda #0
  rts
bankfail:
  lda #ATFAIL_PRG_CHANGED
  rts
.endproc

.proc run_auto_test_3
  ldx #$0F
innerloop:
  stx reg01_value
  jsr inner_outer_mode
  
  ldy #0
loop16k:
  ldx #$1F
outerloop:
  stx reg81_value
  lda #$03
modeloop:
  sta reg80_value
  sty failure_details+7
  cpy #32          ; C = 8000 or C000
  lda reg80_value  ; A = mode
  ldx reg01_value  ; X = inner bank
  ldy reg81_value  ; Y = outer bank
  jsr calc_prg_bank
  and last_bank_number
  ldy failure_details+7
  cmp out80,y
  bne failed
  iny
  lda reg80_value
  clc
  adc #4
  cmp #$40
  bcc modeloop
  ldx reg81_value
  inx
  cpx #$21
  bcc outerloop
  cpy #64
  bcc loop16k
  lda #0
  rts
failed:
  sta failure_details+3
  lda out80,y
  sta failure_details+2
  lda reg01_value
  sta failure_details+1
  lda failure_details+7
  asl a
  and #$40
  ora #$80
  sta failure_details+0
  lda #ATFAIL_WRONG_PRG
  rts
.endproc
