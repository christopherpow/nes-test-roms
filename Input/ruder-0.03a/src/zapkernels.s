;
; Zapper reading kernels (NTSC)
; Copyright 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "nes.inc"

; $4017.D4: Trigger switch (1: pressed)
; $4017.D3: Light detector (0: bright)
;
; There are three working kernels:
; NTSC single player (X, Y) kernel
; NTSC 2-player (Y) kernel
; NTSC (Yon, Yoff) kernel
;
; PAL kernels are left as an exercise for the reader:
; PAL single player (X, Y) kernel
; PAL 2-player (Y) kernel
; PAL (Yon, Yoff) kernel

.export zapkernel_yonoff_ntsc, zapkernel_yon2p_ntsc, zapkernel_xyon_ntsc

.align 256
.proc zapkernel_yonoff_ntsc
off_lines = 0
on_lines = 1
subcycle = 2
DEBUG_THIS = 0
  lda #0
  sta off_lines
  sta on_lines
  sta subcycle

; Wait for photosensor to turn ON
lineloop_on:
  ; 8
  lda #$08
  and $4017
  beq hit_on

  ; 72
  jsr waste_12
  jsr waste_12
  jsr waste_12
  jsr waste_12
  jsr waste_12
  jsr waste_12

  ; 11
  lda off_lines
  and #LIGHTGRAY
  ora #BG_ON|OBJ_ON
.if DEBUG_THIS
  sta PPUMASK
.else
  bit $0100
.endif

  ; 12.67
  clc
  lda subcycle
  adc #$AA
  sta subcycle
  bcs :+
:

  ; 10
  inc off_lines
  dey
  bne lineloop_on
  jmp bail

; Wait for photosensor to turn ON
lineloop_off:
  ; 8
  lda #$08
  and $4017
  bne hit_off

hit_on:
  ; 72
  jsr waste_12
  jsr waste_12
  jsr waste_12
  jsr waste_12
  jsr waste_12
  jsr waste_12

  ; 11
  lda off_lines
  and #LIGHTGRAY
  ora #BG_ON|OBJ_ON
.if DEBUG_THIS
  sta PPUMASK
.else
  bit $0100
.endif

  ; 12.67
  clc
  lda subcycle
  adc #$AA
  sta subcycle
  bcs :+
:

  ; 10
  inc on_lines
  dey
  bne lineloop_off

hit_off:
bail:
waste_12:
  rts
.endproc

.proc zapkernel_yon2p_ntsc
off_lines1 = 0
off_lines2 = 1
subcycle = 2
mask_1 = 3
mask_2 = 4
DEBUG_THIS = 0
  lda #0
  sta off_lines1
  sta off_lines2
  sta subcycle
  lda #$08
  sta mask_1
  sta mask_2

lineloop_on:
  ; 20
  lda mask_1
  and $4016
  sta mask_1
  cmp #1
  lda #0
  adc off_lines1
  sta off_lines1

  ; 20
  lda mask_2
  and $4017
  sta mask_2
  cmp #1
  lda #0
  adc off_lines2
  sta off_lines2
  
  ; 44
  jsr waste_12
  jsr waste_12
  jsr waste_12
  bit $0100
  bit $0100

  ; 12
.if DEBUG_THIS
  tya
  tya
  and #LIGHTGRAY
  ora #BG_ON|OBJ_ON
  sta PPUMASK
.else
  jsr waste_12
.endif

  ; 12.67
  clc
  lda subcycle
  adc #$AA
  sta subcycle
  bcs :+
:

  ; 5
  dey
  bne lineloop_on
  jmp bail

hit_off:
bail:
waste_12:
  rts
.endproc


; Ideally, the jsr should begin 10 cycles before the start of
; rendering, so place sprite 0 wisely.
; @return X: horizontal position; Y: distance from bottom
.align 256
.proc zapkernel_xyon_ntsc
DEBUG_THIS = 0

  ldx #0
  lda #$08
 
lineloop:
  ; 84
  bit $4017
  beq bail0
  bit $4017
  beq bail1
  bit $4017
  beq bail2
  bit $4017
  beq bail3
  bit $4017
  beq bail4
  bit $4017
  beq bail5
  bit $4017
  beq bail6
.if DEBUG_THIS
  lda #BG_ON|OBJ_ON|TINT_R|TINT_G
  sta PPUMASK
  lda #BG_ON|OBJ_ON
  sta PPUMASK
.else
  bit $4017
  beq bail7
  bit $4017
  beq bail8
.endif
  bit $4017
  beq bail9
  bit $4017
  beq bail10
  bit $4017
  beq bail11
  bit $4017
  beq bail12
  bit $4017
  beq bail13

  ; 14
  clc
  bit $4017
  beq bail14
  txa
  adc #$AA
  tax

  ; 10.67
  lda #$08
  bit $4017
  beq bail15
  bcs :+
:

  ; 5
  dey
  bne lineloop

bail0:
  ldx #0
  rts
bail1:
  ldx #1
  rts
bail2:
  ldx #2
  rts
bail3:
  ldx #3
  rts
bail4:
  ldx #4
  rts
bail5:
  ldx #5
  rts
bail6:
  ldx #6
  rts
bail7:
  ldx #7
  rts
bail8:
  ldx #8
  rts
bail9:
  ldx #9
  rts
bail10:
  ldx #10
  rts
bail11:
  ldx #11
  rts
bail12:
  ldx #12
  rts
bail13:
  ldx #13
  rts
bail14:
  ldx #14
  rts
bail15:
  ldx #15
  rts
.endproc
