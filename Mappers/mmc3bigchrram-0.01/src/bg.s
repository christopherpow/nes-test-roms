.include "nes.inc"
.include "global.inc"

.segment "CODE"
.proc draw_title_bg
  ; Start by clearing the first nametable
  ldx #$20
  lda #$40
  ldy #$00
  jsr ppu_clear_nt

  ; set attribute for orange "mmc3" at top
  lda #$23
  sta PPUADDR
  lda #$C8
  sta PPUADDR
  lda #$AA
  ldx #16
:
  sta PPUDATA
  dex
  bne :-

  ; Load the logo tiles
  jsr load_font
  ldy #cp_mmc3logo
  jsr copyjob 
  
  ; Load the map
  ldx #6
  stx $8000
  lda #<.bank(titlestrips)
  sta $8001
  ldx #0
striploop:
  lda titlestrips,x
  beq stripdone
  lsr a
  lsr a
  tay
  lda titlestrips,x
  inx
  and #$03
  ora #$20
  sta PPUADDR
  lda titlestrips,x
  inx
  sta PPUADDR
  lda titlestrips,x
  inx
  stx $00
  tax
striptileloop:
  stx PPUDATA
  inx
  dey
  bne striptileloop
  ldx $00
  bne striploop
stripdone:

  lda #$22
  sta 3
  lda #$24
  sta 2
  lda #>title_msg
  ldy #<title_msg
  jsr puts_multiline_16

  rts
.endproc

.segment "BANK1"

.macro TITLESTRIP xt, yt, tile, length
  .dbyt ((length) * 1024) | ((yt) * 32) | (xt)
  .byte tile
.endmacro

titlestrips:
  TITLESTRIP  6,  6, $80,  6  ; m
  TITLESTRIP  6,  7, $90,  6
  TITLESTRIP  6,  8, $A0,  6
  TITLESTRIP  6,  9, $B0,  6
  TITLESTRIP 12,  6, $80, 14  ; mc3
  TITLESTRIP 12,  7, $90, 14
  TITLESTRIP 12,  8, $A0, 14
  TITLESTRIP 12,  9, $B0, 14
  TITLESTRIP 22,  4, $8E,  2  ; top of 3
  TITLESTRIP 22,  5, $9E,  2
  TITLESTRIP 24,  4, $AE,  2
  TITLESTRIP 24,  5, $BE,  2
  TITLESTRIP  2, 12, $C0,  3  ; big
  TITLESTRIP  2, 13, $D0,  5
  TITLESTRIP  2, 14, $E0,  5
  TITLESTRIP  5, 15, $C3,  2
  TITLESTRIP  8, 12, $C5,  6  ; CHR
  TITLESTRIP  8, 13, $D5,  6
  TITLESTRIP  8, 14, $E5,  6
  TITLESTRIP 15, 12, $C9,  7  ; RAM
  TITLESTRIP 15, 13, $D9,  7
  TITLESTRIP 15, 14, $E9,  7
  TITLESTRIP 23, 12, $F0,  1  ; test
  TITLESTRIP 28, 12, $F1,  1
  TITLESTRIP 23, 13, $F2,  7
  TITLESTRIP 23, 14, $F9,  7
  .byte $00

title_msg:
  .byte "To test NES 2.0 support,",10
  .byte "press the Start Button.",10,10,10
  .byte "@ 2016 Damian Yerrick",0

; CHR RAM loading ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "CODE"

.proc copytovram
src = $00
  sty src+0
  sta src+1
.endproc
.proc copytovram0
src = copytovram::src
  ldy #0
loop:
  lda (src),y
  sta PPUDATA
  iny
  bne loop
  inc src+1
  dex
  bne loop
  rts
.endproc

;;
; Seeks to a VRAM address.  Addresses in $0000-$3FFF are always
; accessed using window 0 ($0000-$07FF), and addresses in $1000 are
; always accessed using windows 2-3 ($1000-$17FF).  This accommodates
; a simplification in the HH86 subset of the MMC3 mapper.
; @param A high byte of address ($00-$7F)
.proc vram_seek_to_page
  pha
  and #%01111000
  lsr a
  lsr a
  cmp #$10
  bcc havefinalwritepair
  ldx #2
  stx $8000
  sta $8001
  inx
  ora #$01
havefinalwritepair:
  stx $8000
  sta $8001

  ; Set PPU destination address
  pla
  and #$07
  bcc :+
    ora #$10
  :
  sta PPUADDR
  ldx #0
  stx PPUADDR
  rts
.endproc

.proc load_font
  ldy #cp_fizztertiny
.endproc
.proc copyjob
src = 0
srcbank = 2
dst = 3
length = 4

  ldx #0
:
  lda copyjobsbase+0,y
  sta $00,x
  inx
  iny
  cpx #5
  bcc :-

  ; Set the source bank in PRG ROM
  ldx #6
  stx $8000
  lda srcbank
  sta $8001

loop:
  ; Set MMC3 CHR window.
  ldx #0
  lda dst
  jsr vram_seek_to_page

  ; we want to copy min(8 - (dst & $07), length) 256-byte blocks
  lda dst
  and #$07
  eor #$FF
  sec
  adc #8  ; A = 8 - (dst & $07)
  cmp length
  bcc :+
    lda length
    clc
  :
  pha
  tax
  adc dst
  sta dst
  jsr copytovram0
  pla
  eor #$FF
  sec
  adc length
  sta length
  bne loop
  rts
.endproc

.macro COPYJOB name, srclabel, length, dstaddr
name = * - copyjobsbase
  .addr srclabel
  .byte <.bank(srclabel)
  .byte >dstaddr
  .byte >(length)
.endmacro
.segment "RODATA"
copyjobsbase:
  COPYJOB cp_fizztertiny, fizztertiny_chr, 3072, $0400
  COPYJOB cp_mmc3logo,    mmc3logo_chr,    2048, $1800

.segment "BANK0"
fizztertiny_chr: .incbin "obj/nes/fizztertiny16.chr"
mmc3logo_chr: .incbin "obj/nes/mmc3logo.chr"

.segment "CODE"
.proc mmc3_load_first_8_banks
  ldx #5
:
  lda firstbanks,x
  stx $8000
  sta $8001
  dex
  bpl :-
  rts
.endproc

.segment "RODATA"
firstbanks: .byte 0, 2, 4, 5, 6, 7

; Text writing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;
; Clears the screen and write the zero-terminated string at (AAYY),
; which must not exceed 12 lines.
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

