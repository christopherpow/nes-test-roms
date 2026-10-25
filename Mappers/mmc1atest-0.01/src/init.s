.include "nes.inc"
.include "global.inc"

.segment "CODE"
.proc reset_handler
  ; After powering on, put all interrupt sources into a known state.
  sei             ; Disable interrupts
  ldx #$00
  stx PPUCTRL     ; Disable NMI and set VRAM increment to 32
  stx PPUMASK     ; Disable rendering
  stx $4010       ; Disable DMC IRQ
  dex             ; Subtracting 1 from $00 gives $FF, which is a
  txs             ; quick way to set the stack pointer to $01FF
  bit PPUSTATUS   ; Acknowledge stray vblank NMI across reset
  bit SNDCHN      ; Acknowledge DMC IRQ
  lda #$40
  sta P2          ; Disable APU Frame IRQ
  lda #$0F
  sta SNDCHN      ; Disable DMC playback, initialize other channels

vwait1:
  bit PPUSTATUS   ; It takes one full frame for the PPU to become
  bpl vwait1      ; stable.  Wait for the first frame's vblank.

  ; Burn 29700 cycles until the second frame's vblank by setting
  ; up the rest of the chipset.
  ; First turn off BCD for famiclone compatibility
  cld

  ; Clear the zero page
  ldx #0
  txa
clear_zp:
  sta $00,x
  inx
  bne clear_zp

  ; Initialize MMC1 CHR bank
  inx  ; A=0, X=1
  sta $8000  ; mirroring doesn't matter
  sta $8000
  stx $8000  ; begin in fixed $C000
  stx $8000
  sta $8000  ; and 8 KiB CHR switching mode
  sta $A000  ; and CHR bank 0
  sta $A000
  sta $A000
  sta $A000
  sta $A000

vwait2:
  bit PPUSTATUS  ; After the second vblank, we know the PPU has
  bpl vwait2     ; fully stabilized.  Use NMI from here on out.
  jmp main
.endproc

