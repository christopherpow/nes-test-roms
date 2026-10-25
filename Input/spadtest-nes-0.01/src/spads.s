;
; Super NES controller reading code for NES (not DPCM safe)
; by Damian Yerrick
;

.p02
.exportzp cur_keys, cur_axlr, new_keys, new_axlr
.export read_spads

.segment "ZEROPAGE"
cur_keys: .res 2
cur_axlr: .res 2
new_keys: .res 2
new_axlr: .res 2

.segment "CODE"
.proc read_spads

  ; Save the previous state of which buttons were NOT pressed
  ldx #3
savelastloop:
  lda cur_keys,x
  eor #$FF
  sta new_keys,x
  dex
  bpl savelastloop

  ; Actually read 
  lda #1
  sta $4016
  sta cur_axlr+1  ; This 1 bit will get rotated out of player 2's
  lsr a           ; cur_axlr through cur_keys to carry.  When it
  sta cur_keys+1  ; arrives, we're done reading the controllers.
  sta $4016
readloop:
  lda $4016
  and #$03
  cmp #$01
  lsr a
  rol cur_axlr+0
  rol cur_keys+0
  lda $4017
  and #$03
  cmp #$01
  lsr a
  rol cur_axlr+1
  rol cur_keys+1
  bcc readloop

  ; Calculate new presses
  ldx #3
calcnewloop:
  lda cur_keys,x
  and new_keys,x
  sta new_keys,x
  dex
  bpl calcnewloop

  rts
.endproc
