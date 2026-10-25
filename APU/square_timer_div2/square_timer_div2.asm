; Shows that the square timer's period is (raw + 1) * 2, not raw + 1
; with a divide-by-two before the duty sequencer.
;
; I also ran the square at highest pitch ($4002 = 8), quickly
; wrote 255 to $4002 then 8 to $4002 again. This would cause the
; timer to be loaded once with the higher value. Looking in a sound
; editor, the momentary flat section was 512 clocks, not 256 clocks.
;
; correct.wav     correct output with divide-by-two before timer
; div2_after.wav  emulator result with divide-by-two after timer

      .include "prefix_apu.a"

reset:
      lda   #250
      jsr   delay_msec
      
      lda   #1          ; enable
      sta   $4015
      lda   #$bf
      sta   $4000
      lda   #$0f
      sta   $4001
      lda   #$00        ; timer period = 2
      sta   $4002
      lda   #$00
      sta   $4003
      
      lda   #100
      jsr   delay_msec
      
      lda   #0
      sta   <0
      
; If divide-by-two is after timer, timer reload never
; falls between A and B, so frequency is just $4002=255
;
;                            A B (see below)
;128|256|256|256|256|256|256|256|256|256 ...
;  0                         1792 clocks
      
; If divide-by-two is before timer, every fourth timer reload
; falls between A and B, so frequency is equivalent to $4002=223
; (512*3 + 256 = 1792, (1792 / 4) / 2 - 1 = 223)
;
;                            A B (see below)
;  256|  512  |  512  |  512  |256|  512  |  512 ...
;  0                         1792 clocks
      
loop:                   ; 1792 clocks per iteration
                        ; 512 + 512 + 512 + 256 = 1792
      ldy   #5          ; 1772 delay
      lda   #69         
      jsr   delay_ya0
                        ; Timer reloaded with 512 three times during delay
      
      ldx   #127        ; 20 clocks
      ldy   #255
      stx   $4002       ; A in diagram above
                        ; Timer is reloaded with 256 here
      sty   $4002       ; B in diagram above
      dec   <0
      bne   loop
      
      lda   #223        ; play square with $4003 = 223
      sta   $4002
      lda   #250
      jsr   delay_msec
      
      lda   #255        ; play square with $4003 = 255
      sta   $4002
      lda   #250
      jsr   delay_msec
      
      lda   #0
      sta   $4002
      jmp   forever
      
