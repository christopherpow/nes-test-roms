.import nmi_handler, reset_handler, irq_handler
.export init_fme7

.segment "INESHDR"
  .byt "NES",$1A  ; magic signature
  .byt 2          ; PRG ROM size in 16384 byte units
  .byt 1          ; CHR ROM size in 8192 byte units
  .byt $50        ; mirroring type and mapper number lower nibble
  .byt $40        ; mapper number upper nibble

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "CODE"
.proc init_fme7
  ldx #15
:
  stx $8000
  lda initial_fme7_banks,x
  sta $A000
  dex
  bpl :-
  rts
.endproc


.segment "RODATA"
initial_fme7_banks:
  .byte 0, 1, 2, 3, 4, 5, 6, 7  ; CHR banks
  .byte 3, 0, 1, 2  ; PRG banks
  .byte $00  ; mirroring
  .byte $00, $FF, $FF  ; IRQ
