;
; Code that gets loaded into all banks but the last in the
; INL-ROM multi-discrete mapper
;
.include "nes.inc"
.exportzp sig, retraces, test_in_progress
.export sigvalue
.export wrongbanks_init_vram, wrongbanks_ay_and_freeze

sig = $00FC
test_in_progress = sig+2
retraces = $00FF
LF = $0A
sigvalue = $F01E  ; as in "foie gras"

.segment "INESHDR"
  .byt "NES",$1A
  .byt 2, 0  ; PRG ROM size
  .byt $C0, $10  ; mapper 28 was assigned in October 2012

.segment "VECTORS0"
  ; $FFF6: constant 0 (used for bus conflict testing)
  ; $FFF7: constant $FF (used for bus conflict testing)
  ; $FFF8: Current bank
  ; $FFF9: Nonzero if this is the last bank
  .byt $00, $FF, $00, $00
  .addr nmi, reset, irq

.segment "CODE0"
.proc reset
  sei
  cld
  ldx #$00
  stx PPUCTRL
  stx PPUMASK
  dex
  txs
  bit $2002
  dex
  
.if 0
  lda #>sigvalue
  sta sig+1
  lda #0
  sta test_in_progress
  lda #<sigvalue
  sta sig+0
.endif

  ; X is $FE by now, so wait twice for vertical blank to make sure
  ; that the PPU has stabilized
@vblankwait1:  
  bit $2002
  bpl @vblankwait1
  inx
  bne @vblankwait1

  ; Load palette and font and clear nametable
  jsr wrongbanks_init_vram

  ; If "in progress", write "not so fast".
  lda sig
  cmp #<sigvalue
  bne sig_does_not_match
  lda sig+1
  cmp #>sigvalue
  bne sig_does_not_match
  lda test_in_progress
  beq sig_matches
  
  ; At this point, the sig matches, and AY is not done
  lda #>not_so_fast_msg
  ldy #<not_so_fast_msg
  jmp wrongbanks_ay_and_freeze

sig_does_not_match:
  ; HIDDEN FEATURE: hold A on controller 2
  ; to skip power-up bank number test
  ; (useful for testing in emulators that don't support #28)
  lda #1
  sta $4016
  lsr a
  sta $4016
  lda $4017
  lsr a
  bcc notA
  ldx #7
codecopyloop:
  lda secretcode,x
  sta $80,x
  dex
  bpl codecopyloop
  jmp $0080
notA:
  lda #>not_last_bank_msg
  ldy #<not_last_bank_msg
  jmp wrongbanks_ay_and_freeze

sig_matches:
  lda $FFF8  ; current bank number; if it's 0 we win
  beq match_bank_0
  lda #>reset_changes_msg
  ldy #<reset_changes_msg
  jmp wrongbanks_ay_and_freeze

match_bank_0:
  lda #>pass_msg
  ldy #<pass_msg
  jmp wrongbanks_ay_and_freeze
.endproc

.proc wrongbanks_ay_and_freeze
  ldx #$21
  stx 3
  ldx #$02
  stx 2
  jsr wrongbanks_puts_multiline
  lda #$00
  sta PPUSCROLL
  sta PPUSCROLL
  lda #VBLANK_NMI
  sta PPUCTRL
  lda #BG_ON
  sta PPUMASK

forever:
  jmp forever
.endproc

.proc wrongbanks_init_vram
  ; Set the palette
  lda #$3F
  ldx #$00
  sta PPUADDR
  stx PPUADDR
  sta PPUDATA
  lda #$2A
  sta PPUDATA

  ; Clear CHR RAM
  stx PPUADDR
  stx PPUADDR
  txa
  ldy #$10
chrclrloop:
  sta PPUDATA
  inx
  bne chrclrloop
  dey
  bne chrclrloop
  
  ; Clear nametable
  lda #$20
  sta PPUADDR
  stx PPUADDR
  ldx #$F0
ntclrloop:
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  sta PPUDATA
  dex
  bne ntclrloop
  ldy #64
attrclrloop:
  stx PPUDATA
  dey
  bne attrclrloop

  ; load font for error message
fontsrc = 0
fontdstlo = 2
  lda #VRAM_DOWN
  sta PPUCTRL
  lda #<romfont
  sta fontsrc
  lda #>romfont
  sta fontsrc+1
  tya
streamloop:
  ; at this point, A is the low order bit of the destination address
  ldy #$00
  sty PPUADDR
  sta fontdstlo
  sta PPUADDR
streambyteloop:
  lda (fontsrc),y
  sta PPUDATA
  iny
  cpy #32
  bcc streambyteloop
  tya
  clc
  adc fontsrc
  sta fontsrc
  bcc :+
  inc fontsrc+1
:
  lda fontdstlo
  clc
  adc #16
  cmp #32
  bcc streamloop
  sbc #31
  cmp #5
  bcc streamloop

  lda #VBLANK_NMI  ; reset writing direction
  sta PPUCTRL
  rts
.endproc

;;
; @param A high byte of string
; @param Y low byte of string
.proc wrongbanks_puts_multiline
srcLo = $00
srcHi = $01
dstLo = $02
dstHi = $03

  sta srcHi
  ldx #0
  stx srcLo  ; keep the low byte in Y

newline:
  lda dstHi
  sta PPUADDR
  lda dstLo
  sta PPUADDR
  clc
  adc #32
  sta dstLo
  bcc :+
  inc dstHi
:
  ldx #0
charloop:
  lda (srcLo),y
  beq done
  iny
  bne :+
  inc srcHi
:
  cmp #LF
  beq newline
  and #$3F
  sta PPUDATA
  bpl charloop
done:
  rts
.endproc


nmi:
  inc retraces
irq:
  rti
  
secretcode:
  lda #$FF
  sta $FFF7
  jmp ($FFFC)
  
.segment "RODATA0"
romfont:
  .incbin "obj/nes/font8x5.bin"
hello_world:
  .byt "HELLO WORLD",0
not_so_fast_msg:
  .byt "NOT SO FAST!",LF
  .byt "PLEASE TURN THE POWER OFF,",LF
  .byt "AND NEXT TIME DON'T PRESS",LF
  .byt "RESET UNTIL ASKED.",0
not_last_bank_msg:
  .byt "FAIL!",LF
  .byt "DOES NOT POWER ON WITH",LF
  .byt "LAST BANK IN $C000-$FFFF.",LF
  .byt "PLEASE TURN THE POWER OFF.",0
reset_changes_msg:
  .byt "FAIL!",LF
  .byt "CPU RESET CHANGES BANK.",LF
  .byt "PLEASE TURN THE POWER OFF.",0
pass_msg:
  .byt "PASS!",LF
  .byt "RESET LEAVES BANK UNCHANGED.",LF
  .byt "PLEASE TURN THE POWER OFF.",0

