.include "src/nes.h"
.include "src/ram.h"
.import __RAMCODE_RUN__, __RAMCODE_LOAD__, __RAMCODE_SIZE__

; iNES header
.segment "INESHDR"
  .byt "NES",$1A
  .byt 32  ; PRG ROM is sixteen banks, each 2 * 16384 bytes
  .byt 0   ; no CHR ROM; uses 8 KiB CHR RAM instead (if we used
           ; CHR ROM we'd be on the NINA board instead of BxROM)
  .byt $21 ; v mirroring, no battery, no trainer; mapper $x2
  .byt $20 ; classic header format; mapper $2x

; Reset vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Each one has the Barbie stub and a unique identifier.

.macro resetstub banknumber
  .byt banknumber
  sei
  ldx #0
  stx $FFF3
  jmp reset
  .addr nmi, $FFF1, irq
.endmacro

.segment "VECTORS0"
resetstub 0
.segment "VECTORS1"
resetstub 1
.segment "VECTORS2"
resetstub 2
.segment "VECTORS3"
resetstub 3
.segment "VECTORS4"
resetstub 4
.segment "VECTORS5"
resetstub 5
.segment "VECTORS6"
resetstub 6
.segment "VECTORS7"
resetstub 7
.segment "VECTORS8"
resetstub 8
.segment "VECTORS9"
resetstub 9
.segment "VECTORS10"
resetstub 10
.segment "VECTORS11"
resetstub 11
.segment "VECTORS12"
resetstub 12
.segment "VECTORS13"
resetstub 13
.segment "VECTORS14"
resetstub 14
.segment "VECTORS15"
resetstub 15


; Now the testing program begins. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "ZEROPAGE"
nmis: .res 1

; Strings to be built up by the test code
; (one byte longer than actual number of results to include the
; NUL terminator byte)
found_banks: .res 17
found_nts: .res 9

.segment "CODE"
.proc irq
  rti
.endproc

.proc nmi
  inc nmis
  rti
.endproc

.proc reset
  ldx #$00
  stx PPUCTRL
  stx PPUMASK
  stx $4010      ; disable DMC IRQ
  cld
  dex
  txs
  bit PPUSTATUS
  bit $4015      ; ack DMC IRQ
  lda #$40
  sta $4017      ; disable frame IRQ
  lda #$0F
  sta $4015      ; enable all channels but DMC
vwait1:
  bit PPUSTATUS
  bpl vwait1
  
  ; While waiting nearly 30000 cycles for the PPU to stabilize,
  ; clear zero page and OAM and then copy the RAM-based code to RAM.
  ldy #0
  ldx #0
  lda #$F0
clear_zp:
  sty $00,x
  sta OAM,x
  inx
  bne clear_zp
  
  lda #<__RAMCODE_LOAD__
  sta 0
  lda #>__RAMCODE_LOAD__
  sta 1
  lda #<__RAMCODE_RUN__
  sta 2
  lda #>__RAMCODE_RUN__
  sta 3
  ldx #<(65536 - __RAMCODE_SIZE__)
  lda #>(65536 - __RAMCODE_SIZE__)
  sta 5
  ldy #0
copyramcodeloop:
  lda (0),y
  sta (2),y
  iny
  bne :+
  inc 1
  inc 3
:
  inx
  bne copyramcodeloop
  inc 5
  bne copyramcodeloop

vwait2:
  bit PPUSTATUS
  bpl vwait2

  ; Shortest possible way to set the first four colors to grayscale
  ; assumes Y is 0 on entry
  ldx #$3F
  stx PPUADDR
  ldy #0
  sty PPUADDR
  stx PPUDATA
  sty PPUDATA
  lda #$10
  sta PPUDATA
  asl a
  sta PPUDATA

  ; first pattern table is $0400 of zeroes, then the font
  tya
  sty PPUADDR
  sty PPUADDR
clrvram1:
  .repeat 4
    sta PPUDATA
  .endrepeat
  iny
  bne clrvram1

  lda #<font16_chr
  sta 0
  lda #>font16_chr
  sta 1
  ldx #>(96*32)  ; 96 tiles, 32 bytes each
loadfontloop:  
  lda (0),y
  sta PPUDATA
  iny
  bne loadfontloop
  inc 1
  dex
  bne loadfontloop
  
  ldx #$24
  stx PPUADDR
  sty PPUADDR
  tya
clrvram2:
  .repeat 8
    sta PPUDATA
  .endrepeat
  iny
  bne clrvram2
  
  ; Now the PPU has been initialized.  We're ready to do tests.
  jsr testReadingAllBanks
  jsr testNametableReadback

  ; Write the results of the tests to the screen
  lda #0
  sta 2
putsmap_loop:
  ldy 2
  lda putsmap+2,y
  sta 0
  lda putsmap+3,y
  sta 1
  lda putsmap+1,y
  ldx putsmap,y
  jsr puts
  lda 2
  clc
  adc #4
  sta 2
  cmp #putsmap_end - putsmap
  bcc putsmap_loop

  lda #VBLANK_NMI
  sta PPUCTRL
loop:
  lda nmis
:
  cmp nmis
  beq :-
  lda PPUSTATUS
  lda #VBLANK_NMI
  sta PPUCTRL
  lda #0
  sta PPUSCROLL
  sta PPUSCROLL
  lda #BG_ON
  sta PPUMASK
  jmp loop

.endproc

.proc testNametableReadback
  lda #7
  sta 0
wrloop:
  jsr selNametableA
  lda 0
  ora #$30
  sta PPUDATA
  dec 0
  bpl wrloop

  lda #7
  sta 0
rdloop:
  jsr selNametableA
  lda PPUDATA
  lda PPUDATA
  ldx 0
  sta found_nts,x
  dec 0
  bpl rdloop
  rts
.endproc

.proc selNametableA
  lda 0
  asl a
  asl a
  pha
  and #$10
  tay
  sta zerothrough15,y
  pla
  and #$0C
  ora #$20
  sta PPUADDR
  lda #0
  sta PPUADDR
  rts
.endproc

;;
; Writes the string at (0) to the nametable at AAXX.
; Does not write to memory.
.proc puts
  sta PPUADDR
  stx PPUADDR
  pha
  txa
  pha
  ldy #0
copyloop1:
  lda (0),y
  beq after_copyloop1
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
  beq after_copyloop2
  sec
  rol a
  sta PPUDATA
  iny
  bne copyloop2
after_copyloop2:
  rts
.endproc

.segment "RAMCODE"
.proc setPRGBank
  ; Changing banks on a discrete mapper with bus conflicts and no
  ; fixed bank is a 2-step process.  First change to the main bank
  ; to bring zerothrough15 into view, then change by writing an
  ; identical value to zerothrough15.
  ldy #0
  sty $FFF3
  tay
  sta zerothrough15,y
  rts
.endproc

;;
; Post condition: setPRGBank 0
.proc testReadingAllBanks
  lda #15
  sta 0
test_reading_all_banks:
  lda 0
  jsr setPRGBank
  ldx 0
  lda $FFF0

  ; convert to HEX ASCII
  ora #$30
  cmp #$3A
  bcc :+
  adc #6
:
  sta found_banks,x
  dec 0
  bpl test_reading_all_banks
  rts
.endproc

.segment "RODATA"
font16_chr:
  .incbin "obj/nes/font16.chr"
zerothrough15:
  .repeat 17,I
    .byt I
  .endrepeat

putsmap:
  .addr $2062, credit1
  .addr $20A2, credit2
  .addr $20E2, credit3
  .addr $2142, result1head
  .addr $2184, found_banks
  .addr $21C2, result1foot
  .addr $2222, result2head
  .addr $2264, found_nts
  .addr $22A2, result2foot
putsmap_end:
credit1:        .byt "BxROM/AxROM Function Tester",0
credit2:        .byt "PD 2011 Damian Yerrick",0
credit3:        .byt "No rights reserved",0
result1head:    .byt "Accessible PRG banks:",0
result1foot:    .byt "if not 0-F then truncated!",0
result2head:    .byt "Accessible nametables:",0
result2foot:    .byt "01:horz; 02:vert; 04:1-scrn",0
