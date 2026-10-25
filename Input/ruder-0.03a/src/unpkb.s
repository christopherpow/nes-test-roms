;
; unpkb.s - RLE unpacking
;
; Copyright (c) 2008 Damian Yerrick
; 
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;

;;; Version history
;
;   2008 Dec: Optimized to better use the Y register, on a
;             suggestion by blargg
;   2003 Oct: Ported for use with ca65 assembler
;   2000:     Initial release, for x816 assembler


.export PKB_unpackblk, PKB_unpack
.exportzp PKB_source, PKB_len


;;; Configuration
;
; Set PKB_outport to whatever data port you want PackBits to use.
; (Slightly more modification is necessary for memory-to-memory
; unpacking.)
;
PKB_outport = $2007         ;NES PPU data register

;
; Set PKB_source to the address in direct page (i.e. zero page)
; where the pointer to packed data is stored.
;
PKB_source  = $00
PKB_len     = $02

.segment "CODE"

;
; PKB_unpackblk
; Unpack PackBits() encoded data from memory at (PKB_source)
; to a character device such as the NES PPU data register.
;
; This entry point assumes a 16-bit length word in network
; byte order before the data.
PKB_unpackblk:

  ldy #0
  lda (PKB_source),y
  inc PKB_source
  bne :+
  inc PKB_source+1
:
  sta PKB_len+1
  lda (PKB_source),y
  inc PKB_source
  bne :+
  inc PKB_source+1
:
  sta PKB_len

; This entry point assumes a 16-bit length word in host byte order
; at PKB_len.
PKB_unpack:
  lda PKB_len
  beq :+
  inc PKB_len+1   ;trick to allow easier 16-bit decrement
:
  ; optimization suggested by blargg is to hold the low byte
  ; in Y instead of using constant Y=0; saves 3 cycles per
  ; compressed byte
  ldy PKB_source
  lda #0
  sta PKB_source

@PKB_loop:
  lda (PKB_source),y
  bmi @PKB_run

  iny  ; got a literal string
  bne :+
  inc PKB_source+1
:
  tax
  inx
@PKB_strloop:
  lda (PKB_source),y
  iny
  bne :+
  inc PKB_source+1
:
  sta PKB_outport
  dec PKB_len
  bne :+
  dec PKB_len+1
  beq @PKB_rts
:
  dex
  bne @PKB_strloop
  beq @PKB_loop

@PKB_rts:
  sty PKB_source  ; restore the address after the compressed data
  rts

@PKB_run:
  iny  ; got a run
  bne :+
  inc PKB_source+1
:
  tax
  dex
  lda (PKB_source),y  ; get the run contents
  iny
  bne @PKB_runloop
  inc PKB_source+1
@PKB_runloop:
  sta PKB_outport
  dec PKB_len
  bne :+
  dec PKB_len+1
  beq @PKB_rts
:
  inx
  bne @PKB_runloop
  beq @PKB_loop
