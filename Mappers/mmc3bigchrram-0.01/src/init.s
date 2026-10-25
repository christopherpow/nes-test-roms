.include "nes.inc"
.include "global.inc"

.segment "RESETCODE"
.proc reset_handler
  ; The very first thing to do when powering on is to put all sources
  ; of interrupts into a known state.
  sei             ; Disable interrupts
  ldx #$00
  stx PPUCTRL     ; Disable NMI and set VRAM increment to 32
  stx PPUMASK     ; Disable rendering
  stx $4010       ; Disable DMC IRQ
  dex             ; Subtracting 1 from $00 gives $FF, which is a
  txs             ; quick way to set the stack pointer to $01FF
  bit PPUSTATUS   ; Acknowledge stray vblank NMI across reset
  lda #$40
  sta P2          ; Disable APU Frame IRQ
  lda #$0F
  sta SNDCHN      ; Disable DMC playback, initialize other channels
  bit SNDCHN      ; Acknowledge DMC IRQ
  sta $E000       ; Disable MMC3 PIT IRQ

vwait1:
  bit PPUSTATUS   ; It takes one full frame for the PPU to become
  bpl vwait1      ; stable.  Wait for the first frame's vblank.

  ; Over the next 29700 cycles, get the rest of the chipset into a
  ; known state.  Start by turning off BCD addition for compatibility
  ; with poorly made famiclones.
  cld
  
  ; Set up initial CHR banks
  ldx #7
mmc3init:
  stx $8000
  lda mmc3init_values,x
  sta $8001
  dex
  bpl mmc3init

  ; Clear zero page
  inx
  txa
clear_zp:
  sta $00,x
  inx
  bne clear_zp
  stx $A000      ; Set vertical mirroring

vwait2:
  bit PPUSTATUS  ; After the second vblank, we know the PPU has
  bpl vwait2     ; fully stabilized.
  
  ; From here on out, vblank waiting is done with NMI
  jmp main
.endproc

mmc3init_values:
  .byte 0, 2, 4, 5, 6, 7  ; initial CHR banks
  .byte 0, 1              ; initial PRG banks

