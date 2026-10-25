.include "nes.inc"
.include "global.inc"
.import wrongbanks_init_vram, wrongbanks_ay_and_freeze

test_result = $FB
is_nonfirst_bank = $BFF8
is_last_bank = $FFF9

.segment "LOWCODE"

.proc set_chr_bank_x
  lda #$00
.endproc
;;
; Writes X to reg A by writing A to $5000 and X to $8000.
; If the last bank is still in $C000, A = 0 and Z flag is true (EQ).
; If the writes caused another bank to become mapped into $C000,
; A = nonzero and Z flag is cleared (NE), and $FF is written
; to reg $80 and $81.
; X is unchanged, and Y is unchanged if the write was successful.
.proc safe_set_a_to_x
  sta $5000
  stx known00
  lda is_last_bank
  beq fail
  lda #0
  rts
fail:
  jmp try_returning
.endproc

;;
; Verifies that only the value written by the CPU, not the value in
; ROM, affects the written register values.
; @return in test_result:
; 0 success
; 1 known00 does not contain 0
; 3 knownFF does not contain $FF
; 2 bus conflict acts as OR
; 4 bus conflict acts as AND
.proc verify_bus_conflict
  ; Ensure 32K mode
  ldx #$80
  stx regsel
  lda known00
  sta known00
  inx
  stx regsel

  ; Try writing FF to known 00
  lda known00
  beq known00_ok
  ; fail: expected 0.
  lda #ATFAIL_KNOWN_00
  jmp done
known00_ok:

  eor #$FF
  sta known00
  lda is_last_bank
  bne ok2
  ; fail: bus conflict acts as AND on at least some bits.
  lda #ATFAIL_BUS_CONFLICT_AND
  jmp done
ok2:

  ; Try writing 00 to known FF
  lda knownFF
  eor #$FF
  beq ok3
  ; fail: expected 0.
  lda #ATFAIL_KNOWN_FF
  jmp done

ok3:
  sta known00
  lda is_nonfirst_bank
  beq ok4
  ; fail: bus conflict acts as OR on at least some bits.
  lda #ATFAIL_BUS_CONFLICT_OR
  jmp done

ok4:
  lda #0
done:
  sta test_result
  jmp try_returning
.endproc

.proc freeze_in_bank_0
  ldy #$80  ; Mode: 32K UNROM #180, horizontal mirroring
  sty $5000
  ldx #$0B
  stx $8000
  lda #$01  ; Inner bank: 0
  sta $5000
  lsr a
  sta $8000
  iny       ; Outer bank: 0
  sty $5000
  sta $8000
forever:
  jmp forever  
.endproc

.proc inner_outer_mode
  lda #$01
  sta $5000
  stx $8000
  ldx #$1F
  ldy #0
outer_loop:
  stx reg81_value
  lda #$81
  sta $5000
  stx $8000
  lda #$80
  sta $5000
  ldx #0
mode_loop:
  txa
  asl a
  asl a
  ora #$03
  sta $8000
  lda $BFF8
  sta out80,y
  lda $FFF8
  sta outC0,y
  iny
  inx
  cpx #$10
  bcc mode_loop
  ldx reg81_value
  inx
  cpx #$21
  bcc outer_loop
  jmp try_returning
.endproc

;;
; Fills out80, outC0, out20, out24, out28, and out2C with the result
; from each inner bank from $00 to $1F.
; Modifies no other memory.
; @param X bank mode
; @param Y outer bank
.proc read_mode_x_bank_y
  lda #$80
  sta regsel
  stx $8000
  lda #$81
  sta regsel
  sty $8000
  ldy #$01
  sty regsel
  dey
  ldx #$1F

; readback
loop:
  stx $FFF8  ; 20 cycles for CPU readbacks
  lda $BFF8
  sta out80,x
  lda $FFF8
  sta outC0,x
  lda #$20  ; 22 cycles for each PPU readback
  sta PPUADDR
  sty PPUADDR
  lda PPUDATA
  lda PPUDATA
  sta out20,x
  lda #$24
  sta PPUADDR
  sty PPUADDR
  lda PPUDATA
  lda PPUDATA
  sta out24,x
  lda #$28
  sta PPUADDR
  sty PPUADDR
  lda PPUDATA
  lda PPUDATA
  sta out28,x
  lda #$2C
  sta PPUADDR
  sty PPUADDR
  lda PPUDATA
  lda PPUDATA
  sta out2C,x
  dex  ; and 5 cycles for looping
  bpl loop

  ; that's a total of 113 cycles per byte
  ; so it'll need rendering turned off at least 12 lines early
  ; let's make it 16 to make room for things
.endproc
.proc try_returning
  ; Return to last bank
  lda #$FF
  ldy #$80
  sty regsel
  sta knownFF
  iny
  sty regsel
  sta knownFF
  lda is_last_bank
  beq failed_to_return
  rts
.endproc

;;
; So one of the tests failed to return to the last bank.  In this
; case, we can use the text output capability of wrongbanks.
.proc failed_to_return
dstlo = 0
dsthi = 1
srclo = 2
char_half = 3
  jsr wrongbanks_init_vram
  lda #>failed_to_return_msg
  ldy #<failed_to_return_msg
  jmp wrongbanks_ay_and_freeze
.endproc

failed_to_return_msg:
  .byt "FAIL!",LF
  .byt "COULD NOT RETURN TO LAST",LF
  .byt "PRG BANK AFTER TEST #"
failed_to_return_testid:
  .byt "XXXX.",LF
  .byt "PLEASE TURN THE POWER OFF.",0


