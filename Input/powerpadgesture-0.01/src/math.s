.export divaxbyy, diva2by1

;;
; Computes (256*a+x)/y, provided that a < y.
; 0-2 are trashed.
; out: remainder in A; quotient in 0
.proc divaxbyy
quotient = 0
divisor = 1
dividendlo = 2

  stx dividendlo
  sty divisor
fix2by1:
  ldy #1  ; when this gets ROL'd eight times, the loop ends
  sty quotient
loop:
  asl dividendlo
  rol a
  bcs alreadyGreater
  cmp divisor
  bcc nosub
alreadyGreater:
  sbc divisor
  sec
nosub:
  rol quotient
  bcc loop
  rts
.endproc

;;
; Alternate entry point to divaxbyy:
; in: dividend high in A, low in $02,
;     divisor in $01 which never modified
; out: quotient in 0, remainder in A
diva2by1 = divaxbyy::fix2by1
