;
; FME-7 big RAM test for NES
; Copyright 2015 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;

.include "nes.inc"
.include "global.inc"

OAM = $0200
MAX_WRAM_BANKS = 16

.segment "ZEROPAGE"
nmis:          .res 1
cur_keys:      .res 2
new_keys:      .res 2

wram_banknums: .res MAX_WRAM_BANKS  ; bank numbers read back
has_wram:      .res 1   ; 0 for has WRAM, >=$80 for nope

.segment "CODE"
;;
; This NMI handler is good enough for a simple "has NMI occurred?"
; vblank-detect loop.
.proc nmi_handler
  inc nmis
  rti
.endproc
.proc irq_handler
  rti
.endproc

.proc main
  jsr load_main_palette
  jsr check_for_wram
  sta has_wram
  bne disp_results  ; If no RAM is present, don't run more tests
  jsr check_bank_numbers

disp_results:
  lda #>results_msg
  ldy #<results_msg
  jsr cls_puts_multiline
  jsr display_results
  jsr press_A
  lda #>page1_msg
  ldy #<page1_msg
  jsr cls_puts_multiline
  jsr press_A
  lda #>page2_msg
  ldy #<page2_msg
  jsr cls_puts_multiline
  jsr press_A
  lda #>page3_msg
  ldy #<page3_msg
  jsr cls_puts_multiline
  jsr press_A
  jmp disp_results
.endproc
  
.proc press_A

  ; Wait for a vertical blank
  lda nmis
vw3:
  cmp nmis
  beq vw3
  
  ; Turn the screen on
  ldx #0
  ldy #0
  lda #VBLANK_NMI|BG_0000|OBJ_1000
  clc
  jsr ppu_screen_on

  ; Does player want to quit?
  jsr read_pads
  lda new_keys
  and #KEY_A|KEY_START
  beq press_A
  rts
.endproc

.proc load_main_palette
  ; seek to the start of palette memory ($3F00-$3F1F)
  ldx #$3F
  stx PPUADDR
  ldx #$00
  stx PPUADDR
copypalloop:
  lda initial_palette,x
  sta PPUDATA
  inx
  cpx #initial_palette_end-initial_palette
  bcc copypalloop
  rts
.endproc

.proc display_results
  ; set destination PPU address
  ldx #$21
  stx $03
  inx
  stx $02
  lda has_wram
  beq yes_has_wram
  lda #>wram_don_exits_msg
  ldy #<wram_don_exits_msg
  jmp puts_multiline_16
  
yes_has_wram:
  lda #>wram_found_msg
  ldy #<wram_found_msg
  jsr puts_multiline_16
  
  lda #>wram_banknums
  sta $05
  lda #<wram_banknums
  sta $04
  lda #$21
  ldx #$66
  ldy #8
  jsr hexdump8

  lda #>(wram_banknums+8)
  sta $05
  lda #<(wram_banknums+8)
  sta $04
  lda #$21
  ldx #$A6
  ldy #8
  jsr hexdump8

  rts
.endproc

.segment "RODATA"
initial_palette:
  .byt $17,$27,$20,$37
initial_palette_end:

results_msg:
  .byte "FME-7 big work RAM test",10
  .byte "Copr. 2015 Damian Yerrick",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte " Press Start for explanation",0

page1_msg:
  .byte "As of March 2015, NES",10
  .byte "emulators disagree on how",10
  .byte "much RAM at $6000 a game",10
  .byte "using Sunsoft's FME-7 mapper",10
  .byte "IC can see.",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "[1/3]",0

page2_msg:
  .byte "FME-7 register 8 sets both",10
  .byte "the PRG ROM bank at $6000",10
  .byte "and whether $6000 is ROM or",10
  .byte "RAM.  It's been speculated",10
  .byte "that even if this register",10
  .byte "is set to RAM, the FME-7",10
  .byte "will still drive the PRG ROM",10
  .byte "bank output lines based on",10
  .byte "the selected bank number.",10
  .byte "",10
  .byte "",10
  .byte "[2/3]",0

page3_msg:
  .byte "If this is true, the 6264",10
  .byte "(8Kx8 SRAM) on the NES-BTR",10
  .byte "or JSROM PCB can be replaced",10
  .byte "with a 62256 (32Kx8 SRAM),",10
  .byte "and an NES program can use",10
  .byte "all four 8K banks.  This",10
  .byte "would provide ample space",10
  .byte "for a Z-machine or BASIC",10
  .byte "interpreter.",10
  .byte "",10
  .byte "",10
  .byte "[3/3]Press Start for results",0

wram_don_exits_msg:
  .byte "No WRAM found at $6000-$7FFF",0

wram_found_msg:
  .byte "WRAM banks at $6000",10
  .byte "C0:",10
  .byte "C8:",10
  .byte "",10
  .byte "(Each distinct number above",10
  .byte "means 8 KiB of RAM.)",10
  .byte "",0

; Include the CHR ROM data
.segment "CHR"
  .res $0400  ; bank 0: empty
  .incbin "obj/nes/fizztertiny16.chr"  ; banks 1-3: font
  .res $1000  ; banks 4-7: empty

; RAM tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

; 7654 3210  PRG Bank 0 (FME-7 Register $08)
; ERBB BBBB
; ||++-++++- Value on PRG A18-A13 when reading ROM at $6000
; ||         (Only FME-7 has A18; 5B has PSG output on that pin instead)
; |+-------- RAM/ROM select (0: ROM; 1: RAM)
; +--------- WRAM +CE (0: disable; 1: enable)
;            00-3F: ROM; 40-7F: open bus; C0-FF: RAM
;            This ROM does not currently test open bus behavior.

;;
; Ensures that the board contains WRAM.
.proc check_for_wram
addrlo = $00
addrhi = $01

  ; Select first 8K of WRAM
  lda #$08
  sta $8000
  lda #$C0
  sta $A000
  asl a
  asl a    ; A = 0, carry set

  ; Fill first 8K of WRAM ($6000-$7FFF) with a 9-byte pattern
  ldx #$60
  stx addrhi
  tay
  sty addrlo
fillpatloop:
  rol a
  sta ($00),y
  iny
  bne fillpatloop
  inc addrhi       ; Once address reaches $8000, RAM is full
  bpl fillpatloop

  ; Verify that this pattern can be read back
  stx addrhi
  tya
  sec
checkpatloop:
  rol a
  tax
  eor ($00),y
  bne failure
  txa
  iny
  bne checkpatloop
  inc addrhi       ; Once address reaches $8000, RAM is full
  bpl checkpatloop
  lda #$00
  clc
  rts

failure:
  lda #$FF
  sec
  rts
.endproc

.proc check_bank_numbers

  ; Fill all 16 banks with a bank tag
  ldx #$08
  stx $8000
  ldx #MAX_WRAM_BANKS - 1
fillbanknumsloop:
  txa        ; switch to RAM bank X
  ora #$C0
  sta $A000
  sta $6900  ; stash bank number OR $C0
  dex
  bpl fillbanknumsloop

  ; Copy bank tags out to work RAM
  ldx #MAX_WRAM_BANKS - 1
checkbanknumsloop:
  txa        ; switch to RAM bank X
  ora #$C0
  sta $A000
  lda $6900  ; fetch bank number
  sta wram_banknums,x
  dex
  bpl checkbanknumsloop
  rts
.endproc
