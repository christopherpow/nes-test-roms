;
; Video output for tennis game
; Copyright 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;
.include "tennis.inc"
.import ppu_staxp32ym1

JOIN_DELAY = 300
LED_TILE_BASE = $A0
NET_TILE_BASE = $9E

TIP_WIDTH = 12
tipBuffer = $0100
digitsBuffer = $0140

.proc tennis_title
join_msg_to_draw = game_state

  lda #VBLANK_NMI
  sta PPUCTRL
  sta join_msg_to_draw
  ldx #$00
  stx PPUMASK
  stx player_joined
  stx player_joined+1
  stx state_timer_hi
  stx whatNeedsDrawn
  stx whatToBlit

  ; copy palette
  lda #$3F
  sta PPUADDR
  stx PPUADDR
copypalloop:
  lda tennis_title_palette,x
  sta PPUDATA
  inx
  cpx #$20
  bcc copypalloop
  
  stx PPUADDR  ; set VRAM address to $2000 for title loading
  ldx #$00
  stx PPUADDR
oamload:
  lda tennis_title_oam,x
  sta OAM,x
  inx
  cpx #tennis_title_oam_end-tennis_title_oam
  bcc oamload
  jsr ppu_clear_oam

  lda #<tennis_title_pkb
  sta 0
  lda #>tennis_title_pkb
  sta 1
  jsr PKB_unpackblk
loop:
  lda nmis
:
  cmp nmis
  beq :-

  jsr draw_player_join_msg

  ldx #0
  stx OAMADDR
  lda #>OAM
  sta OAM_DMA
  ldy #0
  lda #VBLANK_NMI|BG_0000|OBJ_1000
  sec
  jsr ppu_screen_on
  jsr read_pads
  jsr pently_update
  
  ldx #1
handle_join:
  lda new_keys,x
  and #KEY_A|KEY_START
  beq no_join_button
  lda player_joined,x
  bne done
  lda #1
  sta player_joined,x
  lda player_joined+0
  and player_joined+1
  bne done
  stx join_msg_to_draw
  lda #<-JOIN_DELAY
  sta state_timer
  lda #>-JOIN_DELAY
  sta state_timer_hi
no_join_button:
  dex
  bpl handle_join

  ; count down join time
  bit state_timer_hi
  bpl no_join_time_countdown
  inc state_timer
  bne no_join_time_countdown
  inc state_timer_hi
  beq done

no_join_time_countdown:
  lda new_keys+0
  ora new_keys+1
  and #KEY_B
  beq loop

done:
  rts

draw_player_join_msg:
  ldx join_msg_to_draw
  bmi no_draw_player_join_msg
  lda #$23
  sta PPUADDR
  lda portrait_line1,x
  sta PPUADDR
  ldy #8
  txa
  beq charloop1
  ldx #$08
charloop1:
  lda character_names,x
  beq charloop1_done
  sta PPUDATA
  inx
  dey
  bne charloop1
charloop1_done:

  ldx join_msg_to_draw
  lda #$23
  sta PPUADDR
  lda portrait_line2,x
  sta PPUADDR
  ldx #0
charloop2:
  lda msg_ready,x
  sta PPUDATA
  inx
  cpx #6
  bcc charloop2
  lda #$FF
  sta join_msg_to_draw
no_draw_player_join_msg:
  rts
.endproc

; IN-GAME GRAPHICS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; Loads the table background.
.proc tennis_load_bg
  ; copy palette
  lda #$3F
  sta PPUADDR
  stx PPUADDR
copypalloop:
  lda tennis_palette,x
  sta PPUDATA
  inx
  cpx #32
  bcc copypalloop

  lda #VBLANK_NMI
  sta PPUCTRL
  lda #$20
  sta PPUADDR
  lda #$00
  sta PPUADDR
  ldy #1
  jsr ppu_sta32y
  lda #95
  ldy #1
  jsr ppu_sta32y
  lda #2
  ldy #24
  jsr ppu_sta32y
  lda #0
  ldy #6
  jsr ppu_sta32y
  
  ; portraits
  ldx #1
each_portrait:
  ldy #$23
  sty PPUADDR
  lda portrait_addr,x
  sta PPUADDR
  lda #$AA
  sta PPUDATA
  eor #$01
  sta PPUDATA
  lda player_joined,x
  bne not_ai
  ldy #$00
ainameloop:
  lda msg_aiplayer,y
  beq not_ai
  sta PPUDATA
  iny
  bne ainameloop  
not_ai:

  ldy #$23
  sty PPUADDR
  lda portrait_addr,x
  ora #$20
  sta PPUADDR
  lda #$BA
  sta PPUDATA
  eor #$01
  sta PPUDATA

  ; copy the character's name
  ldy #$08
  sty 0
  cpx #$00
  bne charnameloop
  ldy #$00
charnameloop:
  lda character_names,y
  sta PPUDATA
  iny
  dec 0
  bne charnameloop  

  dex
  bpl each_portrait
  
  ; attributes for portraits
  lda #$23
  sta PPUADDR
  lda #$F0
  sta PPUADDR
  ldx #0
loop2:
  lda status_attrs,x
  sta PPUDATA
  inx
  cpx #8
  bcc loop2

  lda #VBLANK_NMI|VRAM_DOWN
  sta PPUCTRL
  lda #1
  sta 0
netcols:
  lda #$20
  sta PPUADDR
  lda #$4F
  clc
  adc 0
  sta PPUADDR
  lda #NET_TILE_BASE
  ora 0
  ldx #24
  ldy #1
  jsr ppu_staxp32ym1
  dec 0
  bpl netcols
  rts
.endproc

.proc tennis_draw_sprites

  ; Place sprite 0
  lda #7
  sta OAM
  lda #4
  sta OAM+1
  lda #%00000000
  sta OAM+2
  lda #20
  sta OAM+3
  ldx #4
  stx oam_used

  ; Draw both players' paddles
  ldy paddle_yhi+0
  ldx #PADDLE_1P_X
  lda #PADDLE_LENGTH|%00000000
  jsr draw_paddle
  ldy paddle_yhi+1
  ldx #PADDLE_2P_X
  lda #PADDLE_LENGTH|%01000000
  jsr draw_paddle

  ; The ball disappears in any state numbered at least as high as
  ; STATE_WIN_POINT.
  lda game_state
  cmp #STATE_WIN_POINT
  bcs not_drawing_ball  
  lda #BALL_RADIUS|%10000000
  ldx ball_xhi
  ldy ball_yhi
  jsr draw_ball
not_drawing_ball:

.if 0
  ; debug draw one y coord
  ldy paddle_ypred+0
  ldx #8
  lda #0
  jsr draw_y_arrow_sprite
.endif

  ldx oam_used
  jmp ppu_clear_oam
.endproc

.proc tennis_draw_score_digits
  lda player_score+0
  jsr bcd8bit
  ora #LED_TILE_BASE
  sta digitsBuffer+1
  lda 0
  beq score1_is_zero
  ora #LED_TILE_BASE
score1_is_zero:
  sta digitsBuffer+0

  lda player_score+1
  jsr bcd8bit
  ora #LED_TILE_BASE
  sta digitsBuffer+3
  lda 0
  beq score2_is_zero
  ora #LED_TILE_BASE
score2_is_zero:
  sta digitsBuffer+2
  lda whatNeedsDrawn
  and #<~DRAW_SCORE
  sta whatNeedsDrawn
  lda whatToBlit
  ora #DRAW_SCORE
  sta whatToBlit
  rts
.endproc

.proc tennis_blit_score_digits
  lda #VBLANK_NMI|VRAM_DOWN
  sta PPUCTRL
  ldx #3
loop:
  lda #$23
  sta PPUADDR
  lda digitlocs,x
  sta PPUADDR
  lda digitsBuffer,x
  sta PPUDATA
  beq iszero
  ora #$10
iszero:
  sta PPUDATA
  dex
  bpl loop
  lda whatToBlit
  and #<~DRAW_SCORE
  sta whatToBlit
  rts

.pushseg
.segment "RODATA"
digitlocs:
  .byt $4C, $4D, $5C, $5D
.popseg
.endproc

.proc tennis_hide_tip
  lda #$02
  jsr tennis_init_tip
  ldx #NET_TILE_BASE
  .repeat 4,I
    stx tipBuffer+(TIP_WIDTH/2)-1+I*TIP_WIDTH
  .endrepeat
  inx
  .repeat 4,I
    stx tipBuffer+(TIP_WIDTH/2)+I*TIP_WIDTH
  .endrepeat
  rts
.endproc

.proc tennis_init_tip
  ldy #4*TIP_WIDTH-1
  :
    sta tipBuffer,y
    dey
    bpl :-
  lda whatNeedsDrawn
  and #<~DRAW_TIP
  sta whatNeedsDrawn
  lda whatToBlit
  ora #DRAW_TIP
  sta whatToBlit
  rts
.endproc

.proc tennis_blank_tip
  lda #' '
  jsr tennis_init_tip
  ldy #6
  sty tipBuffer+0
  iny
  sty tipBuffer+TIP_WIDTH-1
  iny
  sty tipBuffer+3*TIP_WIDTH
  iny
  sty tipBuffer+3*TIP_WIDTH+TIP_WIDTH-1
  rts
.endproc

;;
; Copies the name of player X at point Y in the tip buffer.
; trashes 0, 1; leaves X unchanged
.proc copy_player_name_to_tip
  stx 0
  lda #8
  sta 1
  cpx #0
  beq copyloop
  ldx #8
copyloop:
  lda character_names,x
  beq done
  sta tipBuffer,y
  iny
  inx
  dec 1
  bne copyloop
done:
  ldx 0
  rts
.endproc

.proc tennis_tip_point_player_x
  jsr tennis_blank_tip
  ldy #TIP_WIDTH*2 + TIP_WIDTH/2 - 3
  jsr copy_player_name_to_tip
  ldy #5-1
copywordpt:
  lda pointMsg,y
  sta tipBuffer+TIP_WIDTH + TIP_WIDTH/2 - 3,y
  dey
  bpl copywordpt
  rts
.pushseg
.segment "RODATA"
pointMsg: .byt "POINT"
.popseg
.endproc

.proc tennis_tip_game_point_player_x
  jsr tennis_blank_tip
  ldy #TIP_WIDTH*2 + TIP_WIDTH/2 - 3
  jsr copy_player_name_to_tip
  ldy #10-1
copywordpt:
  lda pointMsg,y
  sta tipBuffer+TIP_WIDTH + TIP_WIDTH/2 - 5,y
  dey
  bpl copywordpt
  rts
.pushseg
.segment "RODATA"
pointMsg: .byt "GAME POINT"
.popseg
.endproc

.proc tennis_tip_winner_player_x
  jsr tennis_blank_tip
  ldy #TIP_WIDTH + TIP_WIDTH/2 - 3
  jsr copy_player_name_to_tip
  ldy #5-1
copywordpt:
  lda winsMsg,y
  sta tipBuffer+TIP_WIDTH*2 + TIP_WIDTH/2 - 3,y
  dey
  bpl copywordpt
  rts
.pushseg
.segment "RODATA"
winsMsg: .byt "WINS!"
.popseg
.endproc

.proc tennis_tip_serve
  jsr tennis_blank_tip
  lda serve_turn
  lsr a
  tax
  ldy #TIP_WIDTH + TIP_WIDTH/2 - 3
  jsr copy_player_name_to_tip
  ldy #8-1
copywordpt:
  lda serveMsg,y
  sta tipBuffer+TIP_WIDTH*2 + TIP_WIDTH/2 - 4,y
  dey
  bpl copywordpt
  rts
.pushseg
.segment "RODATA"
serveMsg: .byt "TO SERVE"
.popseg
.endproc

.proc tennis_blit_tip
  ldx #0
  lda #$90 - TIP_WIDTH / 2
  sta 0
lineloop:
  lda #$21
  sta PPUADDR
  lda 0
  sta PPUADDR
  clc
  adc #32
  sta 0
  .repeat ::TIP_WIDTH, I
    lda tipBuffer+I,x
    sta PPUDATA
  .endrepeat
  txa
  adc #TIP_WIDTH
  tax
  cpx #TIP_WIDTH*4
  bcc lineloop
  lda whatToBlit
  and #<~DRAW_TIP
  sta whatToBlit
  rts
.endproc

.proc tennis_blit_something
  lda whatToBlit
  lsr a
  bcc :+
  jmp tennis_blit_score_digits
:
  lsr a
  bcc :+
  jmp tennis_blit_tip
:
  rts
.endproc

.segment "RODATA"
tennis_title_pkb:  .incbin "src/tennis_title.pkb"
tennis_title_palette:
  ; 0: background; 1-3: unused
  .byt $2A,$10,$16,$0F, $2A,$10,$16,$0F, $2A,$10,$16,$0F, $2A,$10,$16,$0F
  ; 4: Podge's paddle; 5: Daffle's paddle; 6: ball; 7: unused
  .byt $2A,$1A,$00,$0F, $2A,$18,$16,$00, $2A,$3A,$30,$00, $2A,$00,$00,$00
tennis_title_oam:
  .byt $1F,$0C,$02,$60
  .byt $27,$1C,$02,$60
  .byt $2F,$0D,$00,$94
  .byt $37,$1D,$00,$94
tennis_title_oam_end:

tennis_palette:
  ; 0: Background; 1: Score LEDs; 2; Podge's face; 3: Daffle's face
  .byt $0F,$00,$2A,$10, $0F,$16,$26,$00, $0F,$02,$10,$27, $0F,$08,$16,$27
  ; 4: Podge's paddle; 5: Daffle's paddle; 6: ball; 7: unused
  .byt $0F,$1A,$00,$00, $0F,$18,$16,$00, $0F,$3A,$30,$00, $0F,$00,$00,$00
status_attrs: .byt $80,$00,$00,$10,$C0,$00,$00,$10
portrait_addr:  .byt $42,$52
portrait_line1: .byt $44,$54
portrait_line2: .byt $64,$74
character_names:
  .byt "PODGE", 0, 0, 0
  .byt "DAFFLE", 0, 0
msg_ready: .byt "READY!"
msg_aiplayer: .byt "(NES)",0
