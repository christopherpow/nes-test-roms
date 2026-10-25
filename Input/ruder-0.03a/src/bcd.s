;
; Binary to decimal conversion for 8-bit numbers
; Copyright 2010 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.export bcd8bit

.macro bcd8bit_iter value
  .local skip
  cmp value
  bcc skip
  sbc value
skip:
  rol highDigits
.endmacro

;;
; Converts a decimal number to two or three BCD digits
; in no more than 84 cycles.
; @param a the number to change
; @return a: low digit; 0: upper digits as nibbles
.proc bcd8bit
highDigits = 0
  asl highDigits  ; clear out two bits
  asl highDigits  ; conversion will fill the rest of them
  
  ; Each iteration takes 11 if subtraction occurs or 10 if not.
  ; But if 80 is subtracted, 40 and 20 aren't, and if 200 is
  ; subtracted, 80 is not, and at least one of 40 and 20 is not.
  ; So this part takes up to 6*11-2 cycles.
  bcd8bit_iter #200
  bcd8bit_iter #100
  bcd8bit_iter #80
  bcd8bit_iter #40
  bcd8bit_iter #20
  bcd8bit_iter #10
  rts
.endproc

