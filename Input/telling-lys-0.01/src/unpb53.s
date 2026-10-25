;
; PB53 unpacker for 6502 systems
; Copyright 2012-2016 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"
.export unpb53_some, unpb53_xtiles
.export PB53_outbuf
.exportzp ciSrc, ciDst, ciBufStart, ciBufEnd
.importzp nmis

.segment "ZEROPAGE"
ciSrc: .res 2
ciDst: .res 2
ciBufStart: .res 1
ciBufEnd: .res 1
PB53_outbuf = $0100

.segment "CODE"
.proc unpb53_some
ctrlbyte = 0
bytesLeft = 1
  ldx ciBufStart
loop:
  ldy #0
  lda (ciSrc),y
  inc ciSrc
  bne :+
  inc ciSrc+1
:
  cmp #$82
  bcc twoPlanes
  beq copyLastTile
  cmp #$84
  bcs solidColor

  ; at this point we're copying from the first stream to this one
  ; assuming that we're decoding two streams in parallel and the
  ; first stream's decompression buffer is PB53_outbuf[0:ciBufStart]
  txa
  sec
  sbc ciBufStart
  tay
copyTile_ytox:
  lda #16
  sta bytesLeft
prevStreamLoop:
  lda PB53_outbuf,y
  sta PB53_outbuf,x
  inx
  iny
  dec bytesLeft
  bne prevStreamLoop
tileDone:
  cpx ciBufEnd
  bcc loop
  rts

copyLastTile:
  txa
  cmp ciBufStart
  bne notAtStart
  lda ciBufEnd
notAtStart:
  sec
  sbc #16
  tay
  jmp copyTile_ytox

solidColor:
  pha
  jsr solidPlane
  pla
  lsr a
  jsr solidPlane
  jmp tileDone
  
twoPlanes:
  jsr onePlane
  ldy #0
  lda (ciSrc),y
  inc ciSrc
  bne :+
  inc ciSrc+1
:
  cmp #$82
  bcs copyPlane0to1
  jsr onePlane
  jmp tileDone

copyPlane0to1:
  ldy #8
  and #$01
  beq noInvertPlane0
  lda #$FF
noInvertPlane0:
  sta ctrlbyte
copyPlaneLoop:
  lda a:PB53_outbuf-8,x
  eor ctrlbyte
  sta PB53_outbuf,x
  inx
  dey
  bne copyPlaneLoop
  jmp tileDone

onePlane:
  ora #$00
  bpl pb8Plane
solidPlane:
  ldy #8
  and #$01
  beq solidLoop
  lda #$FF
solidLoop:
  sta PB53_outbuf,x
  inx
  dey
  bne solidLoop
  rts

pb8Plane:
  sec
  rol a
  sta ctrlbyte
  lda #$00
pb8loop:

  ; at this point:
  ; A: previous byte in this plane
  ; C = 0: copy byte from bitstream
  ; C = 1: repeat previous byte
  bcs noNewByte
  lda (ciSrc),y
  iny
noNewByte:
  sta PB53_outbuf,x
  inx
  asl ctrlbyte
  bne pb8loop
  clc
  tya
  adc ciSrc
  sta ciSrc
  bcc :+
  inc ciSrc+1
:
  rts
.endproc

unpb53_tiles_left = $0002

.proc unpb53_xtiles
  stx unpb53_tiles_left
.endproc
.proc unpb53_tiles
  ldx #0
  stx ciBufStart
  ldx #16
  stx ciBufEnd
loop:
  jsr unpb53_some
  ldx #0
copyloop:
  lda PB53_outbuf,x
  sta $2007
  inx
  cpx #16
  bcc copyloop
  dec unpb53_tiles_left
  bne loop
  rts
.endproc
