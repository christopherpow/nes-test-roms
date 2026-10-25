.importzp nmis
.export tell_ly

.code
.align 32

;;
; Waits for a button to be pressed on controller 1.
;
; This is analogous to the zapkernels in Zap Ruder.  It closely
; resembles a zapkernel for the Vs. System's serial light gun, except
; it's intended to cover the entire field instead of just the visible
; portion.  This means it needs to loop slowly enough to run fewer
; than 256 iterations per frame.  (Incidentally, a zapkernel for
; two Vs. guns would also have to be a 2-line kernel.)
; There are 341/3*312 = 35464 cycles per frame on Dendy.  (PAL NES
; and NTSC NES have fewer.)  This means the checking loop needs at
; least ceil(35464/256) = 139 cycles.
;
; @return A: button pressed; Y: relative time within frame of press
.proc tell_ly
cur_keys = $00
lastnmis = $01
  ldy #0
  lda nmis
  sta lastnmis

chkloop:
  ; Strobe controller and increase Y, clearing if frame changed:
  ; 26 cycles
  lda #$01
  sta $4016
  sta cur_keys  ; once the $01 bit is shifted out, it's finished
  iny
  lsr a
  sta $4016
  lda nmis
  cmp lastnmis
  beq :+
    sta lastnmis
    ldy #0
  :

  ; Read bits from controller and repeat if nonzero:
  ; 114 cycles
  bitloop:
    lda $4016
    and #$03
    cmp #$01
    rol cur_keys
    bcc bitloop
  beq chkloop
  .assert >(*) = >(chkloop), error, "tell_ly crosses page boundary"
  lda cur_keys
  rts
.endproc
