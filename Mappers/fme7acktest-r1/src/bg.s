.include "nes.inc"
.include "global.inc"
.segment "CODE"
.proc cls_puts_multiline
  pha
  tya
  pha

  lda #VBLANK_NMI
  sta PPUCTRL
  asl a
  sta PPUMASK

  ; Start by clearing the first nametable
  ldx #$20
  lda #$40
  ldy #$00
  jsr ppu_clear_nt

  lda #$20
  sta $03
  lda #$62
  sta $02
  pla
  tay
  pla
  ; fall through to puts_multiline_16
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

