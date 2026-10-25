;
; math.s
; 8-bit multiply and divide
;
; Copyright (c) 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;

;
; The NES CPU has no FPU, nor does it have a multiplier or divider
; for integer math.  So we have to implement these in software.
; Here are subroutines to compute 8x8=16-bit product, a fractional
; quotient in 0.8 fixed point, 2-argument arctangent, and rectangular
; to polar coordinate conversion.  Also included are lookup tables of
; sine and cosine for angles expressed in units of 1/32 of a turn
; from due right, where cos(0) = cos(32) = sin(8) = 1.0.
; 
; Further information:
; http://en.wikipedia.org/wiki/Fixed-point_arithmetic
; http://en.wikipedia.org/wiki/Binary_multiplier
; http://en.wikipedia.org/wiki/Boxing_the_compass
; http://en.wikipedia.org/wiki/Binary_scaling#Binary_angles
;

.include "tennis.inc"
.segment "CODE"

;;
; Multiplies two 8-bit factors to produce a 16-bit product
; in about 153 cycles.
; @param A one factor
; @param Y another factor
; @return high 8 bits in A; low 8 bits in $0000
;         Y and $0001 are trashed; X is untouched
.proc mul8
factor2 = 1
prodlo = 0

  ; Factor 1 is stored in the lower bits of prodlo; the low byte of
  ; the product is stored in the upper bits.
  lsr a  ; prime the carry bit for the loop
  sta prodlo
  sty factor2
  lda #0
  ldy #8
loop:
  ; At the start of the loop, one bit of prodlo has already been
  ; shifted out into the carry.
  bcc noadd
  clc
  adc factor2
noadd:
  ror a
  ror prodlo  ; pull another bit out for the next iteration
  dey         ; inc/dec don't modify carry; only shifts and adds do
  bne loop
  rts
.endproc

;;
; Computes 256*a/y.  Useful for finding slopes.
; 0 and 1 are trashed.
.proc getSlope1
quotient = 0
divisor = 1

  sty divisor
  ldy #1  ; when this gets ROL'd eight times, the loop ends
  sty quotient
loop:
  asl a
  bcs alreadyGreater
  cmp divisor
  bcc nosub
alreadyGreater:
  sbc divisor
  sec  ; without this, results using alreadyGreater are wrong
       ; thx to http://6502org.wikidot.com/software-math-intdiv
       ; for helping solve this
nosub:
  rol quotient
  bcc loop
  lda quotient
  rts
.endproc


.segment "RODATA"

; Tangents of angles between the ordinary angles, used by getAngle.
; you can make trig tables even in windows calculator
; (90/16*1)t*256= 25
tantable:
  .byt 25, 78, 137, 210

; Accurate sin/cos table used by measureFromSilo.
; These are indexed by angle in quadrant 1, and scaled by 256.
; (90*7/8)s*256=
sine256Q1:
  .byt 0, 50, 98, 142, 181, 213, 237, 251
cosine256Q1:
  .byt  0, 251, 237, 213, 181, 142, 98, 50
