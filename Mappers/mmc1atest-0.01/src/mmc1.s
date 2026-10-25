;
; MMC1A test header
; Copyright 2021 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;

.import nmi_handler, reset_handler, irq_handler

.segment "STUB"
.scope
ffd0:
  .byte $00  ; bank number, to be replaced later
resetstub_entry:
  sei
  ldx #$FF
  stx resetstub_entry+2
  jmp reset_handler

  .res ffd0+$2A-*  ; skip the SSS header space
  .assert * = $FFFA, error, "SSS header skip is malformed"
  .addr nmi_handler, resetstub_entry, irq_handler
.endscope
