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
irqs:          .res 1
cur_keys:      .res 2
new_keys:      .res 2

test_step:     .res 1
test_regnum:   .res 1
test_value:    .res 1
test_results:  .res 8


.segment "CODE"
;;
; This NMI handler is good enough for a simple "has NMI occurred?"
; vblank-detect loop.
.proc nmi_handler
  inc nmis
  rti
.endproc

.proc main
  jsr load_main_palette
  jsr run_irq_tests

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
  lda #7
  sta test_step
  lda #>test_results
  sta $05
loop:

  lda #<test_results
  clc
  adc test_step
  sta $04

  ; Calculate the VRAM address of the test result:
  ; $2132 + $40 * line number
  ldy test_step
  tya
  lsr a
  sta $02
  lda #0
  ror a
  lsr $02
  ror a
  adc #$32
  tax
  lda $02
  adc #$21

  ; Decide how to draw it
  cpy #2
  bcc is_byte

  ; 2-7: Write "No IRQ", "Acked", "No ack"
  pha
  lda test_results,y
  cmp #2
  bcc :+
  lda #2
:
  tay
  lda ack_msgslo,y
  sta 0
  lda ack_msgshi,y
  sta 1
  pla
  jsr puts_16
  jmp next_step

  ; and write a byte to vram
is_byte:
  ldy #1
  jsr hexdump8
next_step:
  dec test_step
  bpl loop
  rts
.endproc

.segment "RODATA"
initial_palette:
  .byt $17,$27,$20,$37
initial_palette_end:

results_msg:
  .byte "FME-7 IRQ acknowledge test",10
  .byte "Copr. 2015 Damian Yerrick",10
  .byte "",10
  .byte "Basic support",10
  .byte "To be announced",10
  .byte "Write $0D=$00",10
  .byte "Write $0D=$01",10
  .byte "Write $0D=$80",10
  .byte "Write $0D=$81",10
  .byte "Write $0E=$FF",10
  .byte "Write $0F=$FF",10
  .byte " Press Start for explanation",0

page1_msg:
  .byte "As of February 2015, NES",10
  .byte "emulators disagree on what",10
  .byte "value to write to register",10
  .byte "$0D to acknowledge an",10
  .byte "interrupt request (IRQ) on",10
  .byte "Sunsoft's FME-7 mapper IC.",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "",10
  .byte "[1/3]",0

page2_msg:
  .byte "NESdev Wiki says clearing",10
  .byte "bit 0 (enable interrupts)",10
  .byte "will acknowledge the IRQ.",10
  .byte "This is consistent with the",10
  .byte "ack behavior of many other",10
  .byte "IRQ sources on the NES.",10
  .byte "",10
  .byte "FCEUX source code says",10
  .byte "writing any value to $0D,",10
  .byte "$0E, or $0F will work.",10
  .byte "",10
  .byte "[2/3]",0

page3_msg:
  .byte "Nestopia and Mednafen, on",10
  .byte "the other hand, won't",10
  .byte "acknowledge the IRQ unless",10
  .byte "the program stops the",10
  .byte "counter using bit 7 and",10
  .byte "restarts it.  This would",10
  .byte "change the usable periods",10
  .byte "for multiple IRQs in a",10
  .byte "frame from 256*n cycles",10
  .byte "to 256*n+6 cycles.",10
  .byte "",10
  .byte "[3/3]Press Start for results",0

ack_msgslo: .lobytes ack0_msg, ack1_msg, ack2_msg
ack_msgshi: .hibytes ack0_msg, ack1_msg, ack2_msg
ack0_msg:   .byte "No IRQ",0
ack1_msg:   .byte "Acked",0
ack2_msg:   .byte "No ack",0


; Include the CHR ROM data
.segment "CHR"
  .res $0400  ; bank 0: empty
  .incbin "obj/nes/fizztertiny16.chr"  ; banks 1-3: font
  .res $1000  ; banks 4-7: empty

; IRQ handler: This actually performs the tests ;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"
.proc run_irq_tests 
  ; Basic test: Does it generate any IRQs in a 5-frame period?
  lda #VBLANK_NMI
  sta PPUCTRL
  lda #0
  sta nmis
:
  cmp nmis
  beq :-
  
  ldx #$0D
  lda #$00
  stx $8000
  sta $A000
  sta test_regnum
  sta test_value
  inx
  lda #$81
  stx $8000
  sta $A000
  inx
  stx $8000
  sta $A000
  ldx #$0D
  stx $8000
  sta $A000

  cli  ; Allow interrupts through
  lda #5
:
  cmp nmis
  bcs :-

  ; Save whether stuff was acknowledged
  sei
  lda irqs
  sta test_results+0

  ldx #2
irqstepsloop:
  stx test_step
  
  ; Set up the IRQ
  lda tests2to7_regnum-2,x
  sta test_regnum
  lda tests2to7_value-2,x
  sta test_value
  ldx #$0D
  lda #$00  ; stop counting, stop irq
  sta irqs
  stx $8000
  sta $A000
  inx
  lda #$01
  stx $8000
  sta $A000
  inx
  stx $8000
  sta $A000
  ldx #$0D
  lda #$81  ; start counting, start irq
  stx $8000
  sta $A000
  cli

  ; And wait a frame for IRQs to happen
  lda nmis
:
  cmp nmis
  beq :-
  sei
  ldx test_step
  lda irqs
  sta test_results,x
  inx
  cpx #8
  bcc irqstepsloop
  rts
.endproc

.segment "RODATA"
tests2to7_regnum: .byte $0D, $0D, $0D, $0D, $0E, $0F
tests2to7_value:  .byte $00, $01, $80, $81, $FF, $FF

.segment "CODE"
.proc irq_handler
  pha
  lda irqs
  bne is_second_irq

  ; First IRQ in a test: Make the write sequence.  If it
  ; acknowledges IRQ, we're fine.
  lda test_regnum
  sta $8000
  lda test_value
  sta $A000
  jmp irqend

is_second_irq:
  ; If a second IRQ happens, the test produced no acknowledgment.
  ; In this case shut off all IRQs.
  lda #$0D
  sta $8000
  lda #$00
  sta $A000

irqend:
  inc irqs
  pla
  rti
.endproc
