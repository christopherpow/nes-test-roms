	;; This source is released to the public domain
	;;  (or under creative commons-zero, if necessary)
	;;  with the hope that it is useful

	;; Made by lidnariq, 2013-2014

.include "constants.inc"
	.macpack generic
	.macpack longbranch

.segment "HEADER"
	.byte "NES", $1a
	.byte 1     ; 16KiB PRG (actually 8KiB)
	.byte 2     ; 16KiB CHR (actually 1KiB)
	.byte $38   ; 4: mapper 99  &9=8:4scr &4:no trainer &2:no bat
	.byte $69   ; 4: mapper 99  &C=8:nes2.0 &3=1:vs
	.byte 0     ; 4: mapper 99  4:submapper 0
	.byte 0     ; > 4MB PRG, > 2MB CHR
	.byte 0     ; no PRG RAM (also try 5)
	.byte 0     ; no CHR RAM
	.byte 0     ; NTSC (VS system)
	.byte 5     ; same PPU as Vs SMB
	.byte 0,0   ; reserved

.segment "VECTORS"
	.word NMI
	.word Reset
	.word Reset
	
.segment "CHR"
	.res 4096,0
	.incbin "vs-smb-font.chr"
	;; rest of the padding is handled by the linker script
	
.ZEROPAGE
Watchdogtimer: .dword 0
WatchdogSignatureRAM: .res 16
Cointimer: .word 0
SavedCoins: .res 1
OnTime: .res 1
OffTime: .res 1
Joy1Cache: .res 1
Joy2Cache: .res 1
NTaddrLow: .res 1	            ; scratch variable
NMI_count: .res 1
PrintStringPtr: .word 0
CoinX: .res 1
CoinY: .res 1
SelTest: .res 1
PalSel: .res 1
RC2C05: .res 1
iter: .res 1

	;; for convenience, we're using the same character font
	;; that comes with VS SMB, which is, in ROM,
	;; "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	;; color0 color1 color2 color3 "-x" edge "!" ? ? blob 
	;;  there's more stuff after that, but we don't care.

	;; the main stroke of the font is color 1
	;; it comes with a drop shadow that is color 3.

	;; pursuant to US caselaw, bitmap fonts are /not
	;;  copyrightable/, and thus including *just*
	;;  the font itself is a non-infringing use.

.CODE

	;; To adapt this to any other Mapper 99 VS system game,
	;;  you need only update the following 5 things:
	
	;; This could be 4 or 0. For Vs SMB it's 0.
Mapper99PageWithFont = 0
	;; This could be PPUCTRL::BKGD_RIGHT or PPUCTRL::BKGD_LEFT. For Vs SMB it's RIGHT.
PpuPageWithFont = PPUCTRL::BKGD_RIGHT

	;; This table is a lookup table to convert a byte
	;; containing an ascii character into a byte specifying
	;; a tile in CHRROM
.proc AsciiToNative
	.res 32,36	; control characters
	.byte 36,43,36,36,36,36,36,36
	.byte 36,36,46,36,36,40,36,36	; minimal punctuation (-, !)
	.byte 0,1,2,3,4,5,6,7,8,9	; 0-9
	.byte 46		; : (blob)
	.res 6,36	; no more punctuation
	.byte 10,11,12,13,14,15,16,17,18,19,20,21,22
	.byte 23,24,25,26,27,28,29,30,31,32,33,34,35
	.res 6,36	; even more no punctuation
	.byte 10,11,12,13,14,15,16,17,18,19,20,21,22
	.byte 23,24,25,26,27,28,29,30,31,32,41,34,35
	.res 5,36
.endproc

	;; This table is a lookup table to convert a
	;; hexadecimal value into a byte specifying a tile in
	;; CHRROM
.proc HexToNative
	.byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
.endproc

	;; This only specifies which PPU we start off assuming
	;; the palette should be.
	;; 0 - 2c03 (2c02, 2c07)
	;; 1,2,3,4 - 2c04-000[1,2,3,4]
	;; 2c05 is automatically detected and this value is ignored
.proc DefaultPPU
	.byte 4
.endproc

;;; END OF THINGS TO ADJUST FOR DIFFERENT CHR ROM SETS

	;; this should be exactly 16 bytes wide for convenience
.proc PalettesData
	;;     0123456789abc
	.byte "    RC2C03   ",$01,$0f,$28
	.byte " RP2C04-0001 ",$20,$2f,$36
	.byte " RP2C04-0002 ",$1a,$00,$3b
	.byte " RP2C04-0003 ",$22,$2e,$23
	.byte " RP2C04-0004 ",$0b,$1d,$0d
.endproc
	
;;; bg, col1, col2, col3 = A,X,X,Y
.proc FlushPalette
	sta PPU_DATA
	stx PPU_DATA
	stx PPU_DATA
	sty PPU_DATA
	sta PPU_DATA
	stx PPU_DATA
	stx PPU_DATA
	sty PPU_DATA
	sta PPU_DATA
	stx PPU_DATA
	stx PPU_DATA
	sty PPU_DATA
	sta PPU_DATA
	stx PPU_DATA
	stx PPU_DATA
	sty PPU_DATA
	rts
.endproc

	;; A = idx of palette to flush
	;; (no bounds checking!)
.proc UpdatePalette
	bit RC2C05
	bmi @isa2c05
	asl
	asl
	asl
	asl
	pha
	jsr WaitForNMI
	ldx #9
	ldy #20
	jsr gotoXY
	pla
	tax
	lda #13
	sta iter
@nextname:
	lda PalettesData,x
	tay
	lda AsciiToNative,y
	sta PPU_DATA
	inx
	dec iter
	bne @nextname

	lda #$3f
	sta PPU_ADDR
	sty PPU_ADDR

	lda PalettesData,x
	pha
	ldy PalettesData+1,x
	lda PalettesData+2,x
	tax
	pla
	jsr FlushPalette
	rts

@isa2c05:
	jsr WaitForNMI
	lda #$3f
	sta PPU_ADDR
	lda #0
	sta PPU_ADDR

	lda PalettesData+13 ; assume 2c05 palette is close enough to same as 2c03
	ldy PalettesData+14
	ldx PalettesData+15
	jsr FlushPalette
	
	ldx #9
	ldy #20
	jsr gotoXY
	ldy #13
	ldx #0
	jsr PrintString
	.byte "    RC2C05   $"
	rts
.endproc

;;; -----------------------------
	;; use as JSR PrintString / .byte "string$"
	;; YOU MUST HAVE ENABLENMI FIRST.
	;; trashes A, Y, PrintStringPtr
.proc PrintString
	jsr DisableVideo
	pla
	sta PrintStringPtr
	pla
	sta PrintStringPtr+1
	ldy #1      ; rts +1s, so we need to +1 also
@nextChar:
	lda (PrintStringPtr),y
	cmp #'$'
	beq @donePrintString
	tax
	lda AsciiToNative, x
	sta PPU_DATA
	iny
	bne @nextChar
	;; for speed reasons ... won't print more than 255 characters
	dey
	sec
@donePrintString:
	tya
	adc PrintStringPtr
	sta PrintStringPtr
	lda #0
	adc PrintStringPtr+1
	sta PrintStringPtr+1
	jsr WaitForNMI
	lda #0
	bit PPU_STATUS
	sta PPU_SCROLL
	sta PPU_SCROLL
	jsr EnableVideo
	jmp (PrintStringPtr)
.endproc

;;; -------------------------
;;; self-explanatory
.proc Print_Y_as_hex
	tya
	lsr
	lsr
	lsr
	lsr
	tax
	lda HexToNative, x
	sta PPU_DATA
	tya
	and #15
	tax
	lda HexToNative, x
	sta PPU_DATA
	rts
.endproc
.proc Print_X_as_hex
	txa
	lsr
	lsr
	lsr
	lsr
	tay
	lda HexToNative, y
	sta PPU_DATA
	txa
	and #15
	tay
	lda HexToNative, y
	sta PPU_DATA
	rts
.endproc
.proc Print_A_as_hex
	pha
	lsr
	lsr
	lsr
	lsr
	tax
	lda HexToNative, x
	sta PPU_DATA
	pla
	and #15
	tax
	lda HexToNative, x
	sta PPU_DATA
	rts
.endproc


;;; - Switches to inc32, writes X bytes, all " ", except for the Yth one "*",
;;;   switches back to inc1
.proc DoDots
	lda #PPUCTRL::INC32 | PPUCTRL::NMI_ENA | PpuPageWithFont
	bit RC2C05
	bmi @is2c05
	sta PPU_CTRL
	bpl @not2c05
@is2c05:
	sta PPU_MASK
@not2c05:

	inx         ; off-by-1
	iny
@nextcell:
	lda AsciiToNative+' '
	dey
	bne @noreload
	lda AsciiToNative+'*'
@noreload:
	sta PPU_DATA
	dex
	bne @nextcell

	jmp EnableNMI
.endproc

;;; ----------------------
;;; Measures the amount of time it takes for the watchdog to kick in
.proc TimeWatchdog
	jsr AddSignature
	jsr WaitForNMI
	ldx #10
	ldy #14
	jsr gotoXY
	jsr PrintString
	.byte "Please wait$"
	lda #0
	sta PPU_SCROLL
	sta PPU_SCROLL
	
	jsr DisableNMI
	sta Watchdogtimer
	sta Watchdogtimer+1
	lda #$F0
	sta Watchdogtimer+2
	bit JOY_2	; last time we'll read from watchdog_reset until hopefully it kicks in
@TWL:
	inc Watchdogtimer	;5
	bne @TWL	;3 -> 2047
	inc Watchdogtimer+1	;5
	bne @TWL	;3 -> 526079
	inc Watchdogtimer+2	;5
	bne @TWL	;3 -> 8417231
	;; here we timed out.
	;; it's been roughly 8.5Mcycles, or 4.7sec
	;; if it hasn't kicked in yet, it probably won't.

	;; enable nmi
	jsr EnableNMI
	jsr WaitForNMI
	ldx #0
	ldy #14
	jsr gotoXY
	jsr PrintString
	;; ....0123456789a123456789b123456789c1
	.byte "Watchdog did not occur          "
	.byte "   press a button to continue   $"

	jsr RmSignature
	jsr WaitKey

	rts
.endproc

;;; PPU needs reinit, CPU registers all trashed, but RAM should be all OK here.
;;; we're actually called (jsr) from the reset vector, which will finish init when we return.
.proc WatchdogFinished
	jsr ClearNT
	ldx #0
	ldy #14
	jsr gotoXY
	jsr EnableNMI
	jsr PrintString
	;; ....0123456789a123456789b123456789c1
	.byte " Watchdog occured after $"
	lda Watchdogtimer+2
	and #15
	jsr Print_A_as_hex
	ldx Watchdogtimer+1
	jsr Print_X_as_hex
	ldx Watchdogtimer
	jsr Print_X_as_hex
	ldx #0
	ldy #15
	jsr gotoXY
	jsr PrintString
	.byte "   press a button to continue   $"

	jsr EnableVideo

	jsr WaitKey
	jsr RmSignature
	jmp Main    ; we won't be here without having initialized everything on the previous boot
.endproc

;;; -------------------
;;; WaitForNMI trashes A and X
.proc WaitForNMI
	ldx #0
	stx NMI_count
@wait:
	lda NMI_count
	beq @wait
	stx NMI_count
	rts
.endproc

;;; -------------------
.proc NMI
	inc NMI_count
	rti
.endproc
;;; -------------------
.proc EnableNMI
	bit RC2C05
	bmi @is2c05
	lda #PPUCTRL::NMI_ENA | PpuPageWithFont
	sta PPU_CTRL
	rts
@is2c05:
	lda #PPUCTRL::NMI_ENA | PpuPageWithFont
	sta PPU_MASK
	rts
.endproc
;;; -------------------
.proc DisableNMI
	bit RC2C05
	bmi @is2c05
	lda #PpuPageWithFont
	sta PPU_CTRL
	rts
@is2c05:
	lda #PpuPageWithFont
	sta PPU_MASK
.endproc
;;; -------------------
.proc EnableVideo
	bit RC2C05
	bmi @is2c05
	lda #PPUMASK::LEFT_BKGD_SHOW | PPUMASK::BKGD_ENA
	sta PPU_MASK
	rts
@is2c05:
	lda #PPUMASK::LEFT_BKGD_SHOW | PPUMASK::BKGD_ENA
	sta PPU_CTRL
	rts
.endproc
;;; -------------------
.proc DisableVideo
	bit RC2C05
	bmi @is2c05
	lda #0
	sta PPU_MASK
	rts
@is2c05:
	lda #0
	sta PPU_CTRL
	rts
.endproc

;;; ------------------------
;;; gotoXY
.proc gotoXY
	;; we assume the input is sane.
	;; we also pretend the nametables are 1 wide and 4 high
	lda PPU_STATUS
	txa
	asl
	asl
	asl
	sta NTaddrLow
	tya
	lsr
	ror NTaddrLow
	lsr
	ror NTaddrLow
	lsr
	ror NTaddrLow
	ora #$20
	sta PPU_ADDR
	lda NTaddrLow
	sta PPU_ADDR
	rts
.endproc

;;; -----------------------
;;; Measure the amount of time it takes for a coin to fall through
.proc TimeCoindrop
	jsr WaitForNMI
	ldx #10
	ldy #6
	jsr gotoXY
	jsr PrintString
	;; ....0123456789a123456789b123456789c1
	.byte           "Insert coins          "
	.byte " Elapsed times will print below "
	.byte "         Press A to quit$"

	lda #1
	sta CoinX
	lda #10
	sta CoinY

	lda #Mapper99PageWithFont|3
	sta JOY_1

	;; wait for user to release A button
@waitArelease:
	lda #1
	bit JOY_2
	bne @waitArelease
	
@WaitCoinStart:
	lda JOY_1	; coin drop
	and #$60	; coin mask
	sta SavedCoins
	bne @GoCount

	;; see if user wants to quit
	lda JOY_2	; keepalive
	and #1	; check A button
	beq @WaitCoinStart
	rts         ; hit A = quit

@GoCount:
	lda #$60	; coin mask
	ldx #0
	ldy #0

@TimingLoop:
	bit JOY_1	; 4
	beq @Done	; 2
	inx	; 2
	bne @TimingLoop	; 3 usually -> 2815
	bit JOY_2	; 4 (added to prevent watchdog)
	iny	; 2
	bne @TimingLoop	; 3 usually -> 722943
@TimedOut:
	jsr WaitForNMI
	ldx CoinX
	ldy CoinY
	jsr gotoXY
	jsr PrintString
	;; ....123456789a123456789b1234567
	.byte "timeout$"
	jmp @incCoincursor
	rts

@Done:
	stx Cointimer
	sty Cointimer+1
	
	jsr WaitForNMI
	ldx CoinX
	ldy CoinY
	jsr gotoXY
	lda SavedCoins
	lsr
	lsr
	lsr
	lsr
	lsr
	tax
	lda HexToNative, x
	sta PPU_DATA
	lda AsciiToNative+'-'     ; '-'
	sta PPU_DATA
	ldx Cointimer+1
	jsr Print_X_as_hex
	ldx Cointimer
	jsr Print_X_as_hex
	lda AsciiToNative+' '
	sta PPU_DATA ; clean up over a historical "timeout"
	;; value in YYXX specifies approximately time / 6us

@incCoincursor:
	lda CoinX
	clc
	adc #8
	sta CoinX
	lda #$E0
	bit CoinX
	beq @NoIncY
	lda CoinX
	and #$1F
	sta CoinX
	inc CoinY

	lda CoinY
	cmp #18
	bne @NoWrapY
	lda #10
	sta CoinY
@NoIncY:
@NoWrapY:
	lda #0
	sta PPU_ADDR
	sta PPU_ADDR
	sta PPU_SCROLL
	sta PPU_SCROLL

	;; A value of $FFFF would mean 721919 cycles elapsed, or
	;;  approximately half a second

	jmp @WaitCoinStart
.endproc

;;; -----------------------------
;;; Test to see if reading from a mirror of the counter works
.proc TestCounterMirror
	jsr WaitForNMI
	lda $5FFF
	jsr WaitForNMI
	jsr WaitForNMI
	jsr WaitForNMI
	lda $5020
	jsr WaitForNMI
	jsr WaitForNMI
	jsr WaitForNMI
	lda $5FFF
	jsr WaitForNMI
	jsr WaitForNMI
	jsr WaitForNMI
	lda $5020
	jsr WaitForNMI
	ldx #6
	ldy #14
	jsr gotoXY
	jsr PrintString
	;; ....0123456789a123456789b123456789c1
	.byte       "Coin counter should       "
	.byte "     have incremented by 2      "
	.byte "               A button returns$"
	jsr WaitForNMI
	jsr WaitForNMI
	jsr WaitKey
	rts
.endproc

;;; ------------------------------
;;; Counts twice, using the durations in OnTime and OffTime. 0 means 256
.proc TestCounterTiming
	jsr WaitForNMI
	ldx #0
	ldy #12
	jsr gotoXY
	jsr PrintString
	;; ....0123456789a123456789b123456789c1
	.byte "  HEXADECIMAL times are in ms   "
	.byte "     Joystick changes times     "
	.byte " A increments coin counter by 2 "
	.byte "            B quits             "
	.byte "  On time              Off time$"
@redraw:
	jsr WaitForNMI
	ldx #4
	ldy #17
	jsr gotoXY
	ldx OnTime
	jsr Print_X_as_hex
	ldx #26
	ldy #17
	jsr gotoXY
	ldx OffTime
	jsr Print_X_as_hex
	lda #0
	sta PPU_SCROLL
	sta PPU_SCROLL

@waitNoInput:
	jsr loadJoy
	lda Joy1Cache
	bne @waitNoInput

	;; wait for input-
@waitInput:
	jsr loadJoy
	lda Joy1Cache
	beq @waitInput

	;; Test for up/down/button
	lda #JOYBUT::UP
	bit Joy1Cache
	bne @UpPressed

	lda #JOYBUT::DN
	bit Joy1Cache
	bne @DnPressed

	lda #JOYBUT::LT
	bit Joy1Cache
	bne @LtPressed

	lda #JOYBUT::RT
	bit Joy1Cache
	bne @RtPressed

	lda #JOYBUT::ABUT
	bit Joy1Cache
	bne @AButPressed

	lda #JOYBUT::BBUT
	bit Joy1Cache
	beq @waitNoInput
	rts

	jmp @waitNoInput
@UpPressed:
	inc OnTime
	jmp @redraw
@DnPressed:
	dec OnTime
	jmp @redraw
@LtPressed:
	dec OffTime
	jmp @redraw
@RtPressed:
	inc OffTime
	jmp @redraw
@AButPressed:
	jsr DoTestCounterTiming
	jmp @redraw
.endproc

.proc DoTestCounterTiming
	ldy OnTime
	lda #1
	sta $4020
nextOn1: 
	jsr Delay1785cy
	dey
	bne nextOn1

	ldy OffTime
	lda #0
	sta $4020
nextOff1: 
	jsr Delay1785cy
	dey
	bne nextOff1

	ldy OnTime
	lda #1
	sta $4020
nextOn2: 
	jsr Delay1785cy
	dey
	bne nextOn2

	ldy OffTime
	lda #0
	sta $4020
nextOff2: 
	jsr Delay1785cy
	dey
	bne nextOff2

	rts
.endproc

;;; ---------------------------------------
;;; Tests for presence of the $6000-$7fff RAM
.proc TestSHM
	jsr ClearNT
	jsr WaitForNMI
	ldx #1
	ldy #6
	jsr gotoXY
	jsr PrintString
	;; ...0123456789a123456789b123456789c1
	.byte "Shared memory tests-$"
	jsr DisableVideo

	lda #0
	sta $6000
	sta $6800	; clean up any fingerprints we left lying around
	sta JOY_1	; should disable RAM if we're the master
	; don't have to write the correct value to JOY_1.2 because we'll fix it right afterwards.
	
	jsr WaitForNMI
	ldx #2
	ldy #7
	jsr gotoXY
	lda #24
	sta $6000
	lda $6000
	cmp #24
	beq @RAMnotDisabled

	jsr PrintString
	;; ..0123456789a123456789b123456789c1
	.byte "* 4016x1 clr- SHM disabled$"
	jmp @RAMdisabled
@RAMnotDisabled:
	jsr PrintString
	;; ....0123456789a123456789b123456789c1
	.byte "* 4016x1 clr- SHM not disabled$"
@RAMdisabled:
	jsr DisableVideo
	
	lda #Mapper99PageWithFont|2	; should enable RAM if we're the master
	sta JOY_1

	jsr WaitForNMI
	ldx #2
	ldy #9
	jsr gotoXY
	lda #15
	sta $6000
	lda $6000
	cmp #15
	bne @RAMnotEnabled

	jsr PrintString
	;; ..0123456789a123456789b123456789c1
	.byte "* 4016x1 set- SHM enabled$"
	jsr DisableVideo

	ldx #2
	ldy #11
	jsr gotoXY
	ldx $6800
	cpx #15
	bne @RAMnotMirrored

	jsr PrintString
	;; ..0123456789a123456789b123456789c1
	.byte "* 2KB SHM mirrored$"
	jmp @RAMenabled
@RAMnotMirrored:
	jsr PrintString
	;; ..0123456789a123456789b123456789c1
	.byte "* 2KB SHM not mirrored$"
	jmp @RAMenabled
@RAMnotEnabled:
	jsr PrintString
	;; ..0123456789a123456789b123456789c1
	.byte "* 4016x1 set- SHM not enabled$"
@RAMenabled:
	jsr DisableVideo

	ldx #1
	ldy #14
	jsr gotoXY
	jsr PrintString
	.byte "press a button to continue$"

	jsr WaitKey
	rts
.endproc

;;; --------------------------------------
;;; load joypads
.proc loadJoy
	ldx #3
	stx JOY_1
	dex
	stx JOY_1

	ldx #8

@nextjoyread:
	lda JOY_1
	lsr
	ror Joy2Cache

	lda JOY_2
	lsr
	ror Joy1Cache

	dex
	bne @nextjoyread
	rts
.endproc
;;; ---------------------------------------
.proc WaitKey
	;; waits for no button, then for button, then for no button
@WaitNoKey1:
	jsr loadJoy
	lda #$0f
	bit Joy1Cache
	bne @WaitNoKey1
@WaitKey2:
	jsr loadJoy
	lda #$0f
	bit Joy1Cache
	beq @WaitKey2
@WaitNoKey3:
	jsr loadJoy
	lda #$0f
	bit Joy1Cache
	bne @WaitNoKey3

	rts
.endproc

;;; --------------------------------------
.proc AddSignature
	ldx #15
@adding:
	lda WatchdogSignatureROM,x
	sta WatchdogSignatureRAM,x
	dex
	bpl @adding

	rts
.endproc

.proc RmSignature
	ldx #15
	lda #0
@clearing:
	sta WatchdogSignatureRAM,x
	dex
	bpl @clearing

	rts
.endproc

.proc CheckSignature
	ldx #0
@NextByte:
	lda WatchdogSignatureRAM,x
	cmp WatchdogSignatureROM,x
	bne @NoSignature
	inx
	cpx #16
	bne @NextByte

	lda #1
	rts

@NoSignature:
	lda #0
	rts
.endproc

.proc WatchdogSignatureROM
	.byte "watchdog testing"
.endproc

;;; fatal error of some sort pertaining to the ppu
;;; make an obnoxious sound using the noise channel
.proc Fatal
	ldy #APUSTATUS::NSE_ENA
	sty APU_STATUS

@forever:
	sty APU_ATTRIB_NSE
	sty APU_DURATION_NSE
	sty APU_PERIOD_NSE
	lda #0
@wait:
	jsr Delay1785cy
	adc #1
	bne @wait

	iny
	jmp @forever
.endproc
	
;;; Clears all NTs on VS system. Also disables video
.proc ClearNT
	jsr DisableVideo
	bit PPU_STATUS
	lda #$20
	sta PPU_ADDR
	ldx #0
	sta PPU_ADDR
	lda #36
	ldy #16
	jsr FillNT
	rts
.endproc

;;; Writes A to PPU Y*256-X times
.proc FillNT
@inner:
	sta PPU_DATA
	inx
	bne @inner
	dey
	bne @inner
	rts
.endproc	
	
;;; ----------------------------------
.proc Reset
	;;  universal resets:
	sei
	cld

	ldx #0
	stx PPU_CTRL
	stx PPU_MASK
	stx APU_PERIOD_DMC
	ldy #$40
	sty APU_MODE

	bit PPU_STATUS
	ldx #2	; wait 2 vblanks
	stx JOY_1	; should enable SHM RAM if we're master
@wvblank:
	bit PPU_STATUS
	bpl @wvblank
	dex
	bne @wvblank

	;; check for signature
	jsr CheckSignature
	beq @clearingmemory
	jsr WatchdogFinished ; signature present: First display watchdog time

@clearingmemory:
	;; clear memory
	lda #0
	tax
@clrmem:	sta 0,x	; clear memory
	sta $100,x
	sta $200,x
	sta $300,x
	sta $400,x
	sta $500,x
	sta $600,x
	sta $700,x
	sta $6000,x ; shared memory
	sta $6100,x
	sta $6200,x
	sta $6300,x
	sta $6400,x
	sta $6500,x
	sta $6600,x
	sta $6700,x
	inx
	bne @clrmem

	dex
	txs

	;; detect 2C05 by - waiting for 2002.7, enabling nmi in
	;; 2000.7 waiting for 2002.7, and seeing if the nmi ;;
	;; flag has been set.
	bit PPU_STATUS
	jsr EnableNMI
@test2c05: 
	bit PPU_STATUS
	bpl @test2c05

	lda NMI_count
	bne @not2c05

	;; if it hasn't been, 2001 and 2000 must be
	;; flipped. double-check by setting 2001.7 and making
	;; sure we get an nmi then.
	sta PPU_CTRL
	lda #$80
	sta PPU_MASK
	bit PPU_STATUS
@test2c05p2:
	bit PPU_STATUS
	bpl @test2c05p2

	lda NMI_count
	bne @is2c05

	;; Here we didn't get NMIs either way. Something is all broken.
	;; ... what do we do?

	;; I guess we play a sad melody?
	jmp Fatal

@is2c05:
	lda #$80
	sta RC2C05
@not2c05:
	lda #0      ; now that we're done with that experiment ...
	sta PPU_MASK
	sta PPU_CTRL
	
	lda DefaultPPU
	sta PalSel

	jmp Main
.endproc

;;; 1790Cy is really close to 1ms. This routine, when inside a "dey; bne -" loop will achieve that.
;;; Trashes X.
.proc Delay1785cy	            ; 6
	ldx #253    ; 8
@delay:
	nop         ; 10
	dex         ; 12
	bne @delay  ; 15 -> 1779
	rts         ; 1785
.endproc
	
;;; ---------------------------------------
;;; Main menu
;;;  - Time Watchdog
;;;  - Time Coin drop
;;;  - Test Counter mirror
;;;  - Test Counter timing
;;;  -- specify on time in ms
;;;  -- specify off time in ms
;;;  - Test shared memory bank

;;; --------------------------------------
.proc Main
	lda #0
	sta SelTest
	lda #50
	sta OnTime
	sta OffTime
	
	jsr EnableNMI
	jsr ClearNT

	lda PalSel
	jsr UpdatePalette

	ldx #5
	ldy #17
	jsr gotoXY
	jsr PrintString
	;; ....0123456789a123456789b1
	.byte "2002 readback is $"
	jsr DisableVideo
	lda #0
	sta PPU_STATUS
	lda PPU_STATUS
	jsr Print_A_as_hex
	lda AsciiToNative+' '
	sta PPU_DATA
	lda #$ff
	sta PPU_STATUS
	lda PPU_STATUS
	jsr Print_A_as_hex

	ldx #5
	ldy #18
	jsr gotoXY
	jsr PrintString
	;; ....0123456789a123456789b
	.byte "dipswitches are $"
	jsr DisableVideo
	lda JOY_1
	and #$9A
	jsr Print_A_as_hex
	lda AsciiToNative+' '
	sta PPU_DATA
	lda JOY_2
	and #$fe
	jsr Print_A_as_hex

	ldx #1
	ldy #27
	jsr gotoXY
	jsr PrintString
	.byte "build "
	.incbin "assembletime"
	jsr DisableVideo

	ldx #4
	ldy #6
	jsr gotoXY
	jsr PrintString
	;; ....0123456789a123456789b123456789c1
	.byte     "Up and down changes test    "
	.byte "        A to select test$"
	jsr DisableVideo
	ldx #3
	ldy #9
	jsr gotoXY
	jsr PrintString
	.byte    "Test Watchdog timeout        "
	.byte "   Test coindrop duration       "
	.byte "   Test coin counter mirrors    "
	.byte "   Test VS shared memory        "
	.byte "   Test Coin counter speed$"
	bit RC2C05
	bmi @is2c05
	
	jsr DisableVideo
	ldx #4
	ldy #19
	jsr gotoXY
	jsr PrintString
	.byte "Left and right change PPU$"

@is2c05:
@Redraw:
	jsr WaitForNMI
	ldx #1
	ldy #9
	jsr gotoXY
	ldx #5
	ldy SelTest
	jsr DoDots
	lda #0
	sta PPU_SCROLL
	sta PPU_SCROLL

@waitNoInput:
	jsr loadJoy
	lda Joy1Cache
	bne @waitNoInput

	;; wait for input-
@waitInput:
	;; lda #$60
	;; bit JOY_1
	;; beq @noCoins
	;; jsr ClearNT
	;; jsr TimeCoindrop ; just so that it works without input

@noCoins:
	jsr loadJoy
	lda Joy1Cache
	beq @waitInput

	;; Test for up/down/button
	lda #JOYBUT::UP
	bit Joy1Cache
	bne @UpPressed

	lda #JOYBUT::DN
	bit Joy1Cache
	bne @DnPressed

	lda #JOYBUT::LT
	bit Joy1Cache
	bne @LtPressed

	lda #JOYBUT::RT
	bit Joy1Cache
	bne @RtPressed

	lda #JOYBUT::ALLBUT
	bit Joy1Cache
	bne @ButPressed

	jmp @waitNoInput

@UpPressed:
	dec SelTest
	jpl @Redraw

	lda #4
	sta SelTest
	jmp @Redraw

@DnPressed:
	inc SelTest
	lda SelTest
	cmp #5
	jcc @Redraw

	lda #0
	sta SelTest
	jmp @Redraw

@LtPressed:
	dec PalSel
	bpl @ReloadPalette

	lda #4
	sta PalSel
@ReloadPalette:
	lda PalSel
	jsr UpdatePalette
	jmp @Redraw

@RtPressed:
	inc PalSel
	lda PalSel
	cmp #5
	bcc @ReloadPalette

	lda #0
	sta PalSel
	jsr UpdatePalette
	jmp @Redraw

@ButPressed:
	jsr ClearNT
	lda SelTest
	cmp #0
	beq @SelWatchdog
	cmp #1
	beq @SelCoin
	cmp #2
	beq @SelMirror
	cmp #3
	beq @SelSHM
;;; option- test timing requirements
	jsr TestCounterTiming
	jmp Main
@SelWatchdog:
	jsr TimeWatchdog
	jmp Main
@SelCoin:
	jsr TimeCoindrop
	jmp Main
@SelMirror:
	jsr TestCounterMirror
	jmp Main
@SelSHM:
	jsr TestSHM
	jmp Main
.endproc
