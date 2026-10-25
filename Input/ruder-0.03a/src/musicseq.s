;
; Music sequence data for Zap Ruder
; Copyright 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
; Translation: Go ahead and make your ReMixes, but credit me.

.include "pentlyseq.inc"

.segment "RODATA"

; Sound effect map for Zap Ruder
;  0 MIRV splitting into three ordinary missiles
;  1 Snare drum (triangle part)
;  2 Kick drum (triangle part)
;  3 Hi-hat
;  4 Hi-hat lo (ball bounces off paddle)
;  5 Snare drum (noise part)
;  6 Kick drum (noise part)
pently_sfx_table:
  .addr mirv_split_snd
  .byt 0, 18
  .addr snare2_snd
  .byt 8, 2
  .addr kick2_snd
  .byt 8, 4
  .addr hihat_snd
  .byt 12, 2
  .addr hihatlo_snd
  .byt 12, 4

  .addr snare_snd
  .byt 12, 7
  .addr kick_snd
  .byt 12, 3
  .addr openhat_snd
  .byt 14, 10

; alternating duty/volume and pitch bytes

mirv_split_snd:
  .byt $4F, $24, $44, $24
  .byt $4F, $29, $44, $29
  .byt $4F, $2E, $44, $2E
  .byt $44, $24, $42, $24
  .byt $44, $29, $42, $29
  .byt $44, $2E, $42, $2E
  .byt $42, $24, $41, $24
  .byt $42, $29, $41, $29
  .byt $42, $2E, $41, $2E
snare2_snd:
  .byt $8F, $26, $8F, $25
kick2_snd:
  .byt $8F, $1F, $8F, $1B, $8F, $18, $82, $15
hihat_snd:
  .byt $06, $03, $04, $82
hihatlo_snd:
  .byt $06, $07, $04, $86, $02, $07, $01, $86
snare_snd:
  .byt $0A, $05, $08, $84, $06, $04
  .byt $04, $84, $03, $04, $02, $04, $01, $04
kick_snd:
  .byt $08,$04,$08,$0E,$04,$0E
  .byt $05,$0E,$04,$0E,$03,$0E,$02,$0E,$01,$0E
openhat_snd:
  .byt $07,$03, $06,$83, $05,$03, $05,$83, $04,$03, $04,$83
  .byt $03,$03, $03,$83, $02,$03, $02,$83, $01,$03, $01,$03

; Each drum consists of one or two sound effects.
drumSFX:
  .byt  6,  2
  .byt  1,  5
  .byt  3,$FF
  .byt  7,$FF
KICK  = 0*8
SNARE = 1*8
CLHAT = 2*8
OHAT = 3*8

instrumentTable:
  ; first byte: initial duty (0/4/8/c) and volume (1-F)
  ; second byte: volume decrease every 16 frames
  ; third byte:
  ; bit 7: cut note if half a row remains
  .byt $88, 0, $80, 0  ; bass
  .byt $48, 4, $00, 0  ; substitute for ohit until arpeggio supported
  .byt $87, 6, $00, 0  ; xylo short
  .byt $86,10, $00, 0  ; axe #1
  .byt $47,10, $00, 0  ; axe #2
  .byt $09,10, $00, 0  ; axe #3
  .byt $82, 3, $00, 0  ; axe #1 echo
  .byt $42, 3, $00, 0  ; axe #2 echo
  .byt $02, 2, $00, 0  ; axe #3 echo
  .byt $82, 5, $00, 0  ; axe echo-128

pently_songs:
  .addr title_conductor
  .addr win_point_conductor
  .addr win_game_conductor
  .addr axe_conductor

musicPatternTable:
  ; patterns 0: engine test
  .addr mario1drums
  ; 1-3: tennis win point
  .addr win_point_drums, win_point_tri, win_point_sq
  ; 4-6: tennis win game - fanfare
  .addr win_game_fanfare, win_game_fanfare_joinin, win_game_fanfare_drums
  ; 7-9: axe backbeat
  .addr axe_drums, axe_sq1, axe_tiestream

;____________________________________________________________________
; title theme

title_conductor:
  setTempo 400
  playPatNoise 0, 0, 0
  waitRows 32
  dalSegno

mario1drums:
  .byt KICK, CLHAT, CLHAT, CLHAT, SNARE, CLHAT, CLHAT, CLHAT
  .byt KICK, KICK, KICK, CLHAT, SNARE, CLHAT, CLHAT, CLHAT
  .byt KICK, CLHAT, CLHAT, CLHAT, SNARE, CLHAT, CLHAT, KICK
  .byt CLHAT, KICK, CLHAT, KICK, SNARE, CLHAT, SNARE, SNARE
  .byt 255

;____________________________________________________________________
; tennis: win point

win_point_conductor:
  setTempo 600
  playPatNoise 1, 0, 0
  playPatTri 2, 15, 0
  playPatSq1 3, 27, 1
  playPatSq2 3, 22, 0
  waitRows 25
  setTempo 0
  waitRows 1

win_point_drums:
  .byt SNARE|D_D8, SNARE|D_D8, SNARE|D_4, KICK, KICK, SNARE|D_8, KICK, KICK
  .byt SNARE|D_D8, SNARE|D_D8, SNARE|D_4, REST|D_1
  .byt 255

win_point_tri:
  .byt N_B|D_D8, N_DH|D_D8, N_EH|D_D8, REST, N_EH, N_EH, N_EH, N_EH, N_EH, N_EH
  .byt N_EH|D_D8, N_FH|D_D8, N_GH|D_D8, REST|D_1
  .byt 255

win_point_sq:
  .byt N_B|D_D8, N_DH|D_D8, N_EH|D_D8, REST|D_D8, REST|D_4
  .byt N_EH|D_D8, N_FH|D_D8, N_GH|D_D8, REST|D_1

;____________________________________________________________________
; tennis: win game

win_game_conductor:
  setTempo 896
  playPatSq1 4, 37, 1
  playPatNoise 6, 0, 0
  waitRows 4
  playPatTri 5, 25, 0
  playPatSq2 5, 29, 1
  waitRows 20
  fine

win_game_fanfare:
  .byt N_C|D_8, N_C|D_8
win_game_fanfare_joinin:
  .byt N_F|D_4, REST|D_8, N_C|D_8, N_F|D_2, REST|D_1, 255
win_game_fanfare_drums:
  .byt CLHAT|D_8, CLHAT|D_8, SNARE|D_D4, CLHAT, CLHAT, SNARE|D_2, REST|D_1, 255

;____________________________________________________________________
; Axe

axe_conductor:
  setTempo 515
  playPatNoise 7, 0, 0
  playPatSq1 8, 2, 2
  playPatSq2 9, 0, 2
  segno
  waitRows 32
  playPatSq1 8, 5, 2
  waitRows 32
  playPatSq1 8, 0, 2
  waitRows 32
  playPatSq1 8, 2, 2
  waitRows 32
  dalSegno

axe_drums:
  .byt KICK|D_8, OHAT|D_8, SNARE, CLHAT, OHAT|D_8, 255
axe_sq1:
  .byt REST|D_8, N_D|D_8, 255
axe_tiestream:
  .byt N_TIE|D_1, 255

