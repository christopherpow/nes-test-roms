;
; Simple sprite demo for NES
; Copyright 2011-2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;

.include "nes.inc"
.include "global.inc"

OAM = $0200

.segment "ZEROPAGE"
nmis:          .res 1
oam_used:      .res 1  ; starts at 0
cur_keys:      .res 2
new_keys:      .res 2

.segment "BSS"
irq_handler:   .res 3
RTI_INSTRUCTION = $40
JMP_INSTRUCTION = $4C

.segment "CODE"
;;
; This NMI handler is good enough for a simple "has NMI occurred?"
; vblank-detect loop.  But sometimes there are things that you always
; want to happen every frame, even if the game logic takes far longer
; than usual.  These might include music or a scroll split.  In these
; cases, you'll need to put more logic into the NMI handler.
.proc nmi_handler
  inc nmis
  rti
.endproc

.proc main
  jsr title_screen
  
  sei
  lda #1
  ldy #32
  jsr randomize_vram
  lda #1
  ldy #32
  jsr check_randomized_vram
  beq ok_8k
  
  ; Serious problem if 8K doesn't verify
  ldx #$3F
  stx PPUADDR
  ldx #$00
  stx PPUADDR
  ldx #$06
  stx PPUDATA
  ldx #$26
  stx PPUDATA
  ldy #<fail_8k_msg
  lda #>fail_8k_msg
  jmp have_msg

ok_8k:
  lda #1
  ldy #128
  jsr randomize_vram
  lda #1
  ldy #128
  jsr check_randomized_vram
  beq ok_32k
  ldy #<fail_32k_msg
  lda #>fail_32k_msg
  jmp have_msg

ok_32k:
  ldy #<ok_32k_msg
  lda #>ok_32k_msg
have_msg:
  jsr cls_puts_multiline
  jsr load_font
  jsr mmc3_load_first_8_banks
gloop:
  jsr read_pads
  lda nmis
:
  cmp nmis
  beq :-
  lda #VBLANK_NMI
  ldx #0
  ldy #0
  clc
  jsr ppu_screen_on
  lda new_keys
  and #KEY_START|KEY_A
  beq gloop
  jmp main
.endproc

.proc title_screen

  ; Assume we're still in vblank.  Copy the palette now, during
  ; actual vblank, to avoid a rainbow streak.
  lda #VBLANK_NMI
  sta PPUCTRL
  asl a
  sta PPUMASK
  jsr load_main_palette

  ; While in forced blank we have full access to VRAM.
  ; Load the nametable (background map).
  jsr draw_title_bg
  
  sei
  lda #JMP_INSTRUCTION
  sta irq_handler
  lda #<title_irq_handler
  sta irq_handler+1
  lda #>title_irq_handler
  sta irq_handler+2
  sta $E000
  jsr ppu_clear_oam
  lda #VBLANK_NMI
  sta PPUCTRL
  
titleloop:

  ; No real game logic here
  jsr read_pads

  ; Wait for a vertical blank
  ; and set the scroll registers to display it.
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

  ; Set up IRQ and CHR bank
  ldx #0
  stx $8000
  stx $8001
  inx
  stx $8000
  lda #6
  sta $8001
  lda #128
  sta $C000
  sta $C001
  sta $E001
  cli

  lda new_keys
  and #KEY_A|KEY_START
  beq titleloop
  rts
.endproc

.proc title_irq_handler
  pha
  lda #$02
  sta $8001
  sta $E000
  pla
  rti
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
  cpx #32
  bcc copypalloop
  rts
.endproc

;;
; Fills with the Rijndael finite field
; (polynomial $11B, generator $03).
; @param A starting value
; @param Y number of 256-byte units to check (usually 32 or 128)
.proc randomize_vram
value = $00
dst = $01
limit = $02
  sta value
  sty limit
  lda #VBLANK_NMI
  sta PPUCTRL
  asl a
  sta dst
  sta PPUMASK
pageloop:
  lda dst
  jsr vram_seek_to_page
  ldy #0
  lda value
byteloop:
  asl a
  eor value
  bcc :+
    eor #$1B
  :
  sta value
  sta PPUDATA
  iny
  bne byteloop
  inc dst
  lda dst
  cmp limit
  bne pageloop
  rts
.endproc

.proc check_randomized_vram
value = $00
dst = $01
limit = $02
  sta value
  sty limit
  lda #VBLANK_NMI
  sta PPUCTRL
  asl a
  sta dst
  sta PPUMASK
pageloop:
  lda dst
  jsr vram_seek_to_page
  ldy PPUDATA
  ldy #0
  lda value
byteloop:
  asl a
  eor value
  bcc :+
    eor #$1B
  :
  sta value
  cmp PPUDATA
  bne bail
  iny
  bne byteloop
  inc dst
  lda dst
  cmp limit
  bne pageloop
bail:
  rts
.endproc

.segment "RODATA"
initial_palette:
  .byt $02,$22,$20,$0F, $02,$16,$16,$16, $02,$17,$27,$0F, $02,$16,$16,$16
  .byt $02,$16,$16,$16, $02,$16,$16,$16, $02,$16,$16,$16, $02,$16,$16,$16

.segment "RODATA"
fail_8k_msg:
  .byte "The first 8 KiB of CHR RAM",10
  .byte "failed to verify.",10
  .byte "Try cleaning the connector",10
  .byte "using rubbing alcohol on a",10
  .byte "cotton swab.",0
fail_32k_msg:
  .byte "This system supports 8 KiB",10
  .byte "CHR RAM but not 32 KiB.",10
  .byte "Bug the author of this",10
  .byte "emulator or mapper pack to",10
  .byte "add support for the NES 2.0",10
  .byte "header, or use a WIP",10
  .byte "version of FCEUX.",0
ok_32k_msg:
  .byte "Congratulations!",10
  .byte "This system supports 32 KiB",10
  .byte "CHR RAM.",0

