.segment "AUX" ; prg $c000

.incbin "testtable"
testtable0 = $c000
testtable1 = $c400

.segment "FIXED" ; prg $e000



nmihit = $7ff ; set by NMI handler
failedatest = $7fe ; true if any test failed
failedb003 = $7fd ; value of b003 reg when a test failed
failedbank = $7fc ; what PPU 1K bank (0-15) failed

; current test
fail_expected = $7f9 ; what was expected to be read back from the ppu
fail_got = $7f8 ; what was actually read back from the ppu
; first failed test
failedexpected = $7f7
failedgot = $7f6


currentbank = $7fb ; current PPU 1K bank
currentb003 = $7fa ; current b003 value



addrtmp = $10
valtmp = $20
strtmp = $400


txt_good:
  .asciiz "All Tests Passed!"

txt_bad:
  .asciiz "Test Failed:"

  
donumber:
  ; a = PPUADDR high
  ; x = PPUADDR low
  ; valtmp = 1 byte value, interpreted as hex string, to print
  pha
  lda valtmp
  lsr
  lsr
  lsr
  lsr
  clc
  adc #$e0
  sta strtmp + 0
  lda valtmp
  and #$0f
  adc #$e0
  sta strtmp + 1
  lda #.lobyte(strtmp)
  sta addrtmp
  lda #.hibyte(strtmp)
  sta addrtmp + 1
  lda #$00
  sta strtmp + 2
  pla
  jsr dotext
  rts

dotext:
  ; a = PPUADDR high
  ; x = PPUADDR low
  ; tmp, tmp+1 = pointer to asciiz string
  ldy #$00
  sta $2006
  stx $2006
@reloop:
  lda (addrtmp), y
  beq @out
  sta $2007
  iny
  jmp @reloop
@out:
  rts

.macro drawnumber scx, scy
  lda #($20 | scy >> 6)
  ldx #(scx >> 3 | scy << 2 & $e0)
  jsr donumber
.endmacro
  
.macro drawtext addr, scx, scy
  lda #.lobyte(addr)
  sta addrtmp
  lda #.hibyte(addr)
  sta addrtmp + 1
  lda #($20 | scy >> 6)
  ldx #(scx >> 3 | scy << 2 & $e0)
  jsr dotext
.endmacro

.macro drawtextpos scx, scy
  lda #($20 | scy >> 6)
  ldx #(scx >> 3 | scy << 2 & $e0)
  jsr dotext
.endmacro

  
onirq:
  rti

onnmi:
  lda #$01
  sta nmihit
  rti
  
waitfornmi:
  lda #$00
  sta nmihit
@redo:
  lda nmihit
  beq @redo
  rts
  
mypalette:
  .byte $0F,$00,$10,$20,$0F,$11,$21,$31,$0F,$15,$25,$35,$0F,$1B,$2B,$3B

.if .defined(vrc24)
  .define chrregtest0 chrregtest0_dracula
  .define chrregtest1 chrregtest1_dracula
  .define chrregdisp chrregdisp_dracula
.else
  .define chrregtest0 chrregtest0_esper
  .define chrregtest1 chrregtest1_esper
  .define chrregdisp chrregdisp_esper
.endif
  
chrregtest0_dracula: ; fill chr regs for first test
  lda #$00
  sta $d000
  lda #$02
  sta $d001
  lda #$04
  sta $d002
  lda #$06
  sta $d003
  lda #$08
  sta $e000
  lda #$0a
  sta $e001
  lda #$0c
  sta $e002
  lda #$0e
  sta $e003
  rts
  
chrregtest1_dracula: ; fill chr regs for second test
  lda #$01
  sta $d000
  lda #$03
  sta $d001
  lda #$05
  sta $d002
  lda #$07
  sta $d003
  lda #$09
  sta $e000
  lda #$0b
  sta $e001
  lda #$0d
  sta $e002
  lda #$0f
  sta $e003
  rts

chrregdisp_dracula: ; fill chr regs for final display
  lda #$10
  sta $d000
  lda #$11
  sta $d001
  lda #$12
  sta $d002
  lda #$13
  sta $d003
  lda #$14
  sta $e000
  lda #$15
  sta $e001
  lda #$16
  sta $e002
  lda #$17
  sta $e003
  rts

chrregtest0_esper: ; fill chr regs for first test
  lda #$00
  sta $d000
  lda #$02
  sta $d002
  lda #$04
  sta $d001
  lda #$06
  sta $d003
  lda #$08
  sta $e000
  lda #$0a
  sta $e002
  lda #$0c
  sta $e001
  lda #$0e
  sta $e003
  rts
  
chrregtest1_esper: ; fill chr regs for second test
  lda #$01
  sta $d000
  lda #$03
  sta $d002
  lda #$05
  sta $d001
  lda #$07
  sta $d003
  lda #$09
  sta $e000
  lda #$0b
  sta $e002
  lda #$0d
  sta $e001
  lda #$0f
  sta $e003
  rts

chrregdisp_esper: ; fill chr regs for final display
  lda #$10
  sta $d000
  lda #$11
  sta $d002
  lda #$12
  sta $d001
  lda #$13
  sta $d003
  lda #$14
  sta $e000
  lda #$15
  sta $e002
  lda #$16
  sta $e001
  lda #$17
  sta $e003
  rts
  
runtest: ;run the ppu test
  ; addrtemp - address of the test table (1KiB)
  lda #$00
  sta currentb003
@nextb003:
  lda currentb003 ; $chr reg
  sta $b003 ; set current banking value
  lda #$00
  sta currentbank ; restart bank here
@nextchr:  
  ; fetch the correct result
  ldy #$00
  lda (addrtmp), y
  ; run the test
  pha
  lda currentbank
  asl
  asl ; convert bank number to ppu address high byte
  tax ; x now contains the bank number.
  pla
  
  jsr pputest

  ; did we succeed?
  cmp #$01
  beq @donext
  
  ; uh oh, failure
  lda failedatest
  bne @donext ; only record the first failure
  lda #$01
  sta failedatest
  lda currentb003
  sta failedb003
  lda currentbank
  sta failedbank
  lda fail_expected
  sta failedexpected
  lda fail_got
  sta failedgot
  
@donext:
  ; increment the result table
  inc addrtmp
  bne @inccounters
  inc addrtmp + 1

@inccounters:
  ; increment the counters
  inc currentbank
  lda #$10
  cmp currentbank
  bne @nextchr
  inc currentb003
  lda #$40
  cmp currentb003
  bne @nextb003
  
  rts
  
  
ppufill: ;fill 1k of PPU with given byte value
  ;X - top byte of PPU address to start at
  ;A - byte value to fill
  stx $2006 ; high byte
  ldx #$00
  stx $2006 ; low byte
  ldy #$04 ; 4*256 bytes
@loop:
  sta $2007
  dex
  bne @loop
  dey
  bne @loop
  rts
  
pputest: ;test 512B of PPU with given byte value
  ;X - top byte of PPU address to start at
  ;A - byte value to test
  ;return:
  ;A - 1 if passed, 0 if failed
  stx $2006 ; high byte
  ldx #$00
  stx $2006 ; low byte
  ldy #$02 ; 2*256 bytes - avoid reading the palette in the last bank
  sta valtmp ; save cmp value to ram
  lda $2007 ; dummy read
@loop:
  lda $2007
  cmp valtmp
  bne @failed
  dex
  bne @loop
  dey
  bne @loop
@succeeded:
  lda #$01
  rts
@failed:
  sta fail_got
  lda valtmp
  sta fail_expected
  lda #$00
  rts
  
  
onreset:
  sei
  cld
  lda #$00
  sta $2000 ; disable NMI (theoretically)
  sta $4015 ; disable all audio channels
  sta $4010 ; disable dmc interrupt
  
  ldx #$ff
  txs       ; set stack
  stx $4017 ; disable frame interrupt
  
  ; vrc6 init
  sta $8000 ; set 16K prg at $8000
  sta $c000 ; set 8K prg at $C000
  sta $f001 ; disable irq
  sta $f002 ; disable irq
  stx $9003 ; disable external audio
  
  
  ldx #$03
@waitforppu: ; wait 3 frames for the PPU to warm up
  lda $2002
  and #$80
  beq @waitforppu
  dex
  bne @waitforppu
  
  ; in our first usable vblank
  lda #$00 ; reset everything PPU related
  sta $2000
  sta $2001
  sta $2005
  sta $2005
  
  ; store palettes
  ldy #$00
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
@paletteloop:  
  lda mypalette,y
  sta $2007
  iny
  cpy #$10
  bne @paletteloop

; clear nametables to their test patterns
  ldx #$20
  stx $b003 ; standard vrc6 V mirror mode
  
  lda #$80
  jsr ppufill ; first nametable filled with $80
  ldx #$24
  lda #$81
  jsr ppufill ; second nametable filled with $81

; clear ram to 00
  ldy #$00
  lda #$00
@ramclear:
  sta $0000,y
  sta $0100,y
  sta $0200,y
  sta $0300,y
  sta $0400,y
  sta $0500,y
  sta $0600,y
  sta $0700,y
  iny
  bne @ramclear

; begin testing
  
  jsr chrregtest0
  lda #.lobyte(testtable0)
  sta addrtmp
  lda #.hibyte(testtable0)
  sta addrtmp + 1
  jsr runtest
  
  jsr chrregtest1
  lda #.lobyte(testtable1)
  sta addrtmp
  lda #.hibyte(testtable1)
  sta addrtmp + 1
  jsr runtest

  ; fail or not, now show results
  ldx #$20
  stx $b003 ; standard vrc6 V mirror mode
  jsr chrregdisp ; load correct chr rom regs
  
  lda #$00
  jsr ppufill ; first nametable filled with $00
  
  lda failedatest
  cmp #$01
  beq @failure
@success:
  drawtext txt_good, 32, 32
  jmp @finish
@failure:  
  drawtext txt_bad, 32, 32

  lda failedb003
  sta valtmp
  drawnumber 32, 40
  lda failedbank
  sta valtmp
  drawnumber 56, 40
  lda failedexpected
  sta valtmp
  drawnumber 80, 40
  lda failedgot
  sta valtmp
  drawnumber 104, 40

@finish:

  lda #$80 ;enable NMI
  sta $2000

  jsr waitfornmi
  
  lda #$0e ; enable rendering
  sta $2001

  lda #$00 ; reset scroll
  sta $2005
  sta $2005

@mainloop:  
  jsr waitfornmi
  jmp @mainloop

  
.segment "VECTORS"
  .WORD onnmi
  .WORD onreset
  .WORD onirq
