
; bit 3: buttons 2, 1, 5, 9, 6, 10, 11, 7
; bit 4: buttons 4, 3, 12, 8, X, X, X, X

.export read_powerpad, powerpad_bit_to_button
.exportzp cur_d3, cur_d4, prev_d3, prev_d4

.zeropage
cur_d3: .res 1
cur_d4: .res 1
prev_d3: .res 1
prev_d4: .res 1

.rodata
powerpad_bit_to_button:
  .byte 1, 0, 4, 8, 5, 9, 10, 6  ; D3 button numbers minus 1
  .byte 3, 2, 11, 7  ; D4 button numbers minus 1

.code
.proc read_powerpad
read_d3 = $00
read_d4 = $01
  ldx #1
  stx $4016
  stx read_d3
  stx read_d4
  dex
  stx $4016
  loop:
    ldx $4017
    txa
    and #$08
    cmp #$01
    rol read_d3
    txa
    and #$10
    cmp #$01
    rol read_d4
    bcc loop
  lda cur_d3
  sta prev_d3
  lda read_d3
  sta cur_d3
  lda cur_d4
  sta prev_d4
  lda read_d4
  and #$F0
  sta cur_d4
  rts
.endproc
