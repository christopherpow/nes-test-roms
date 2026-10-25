; To assemble: snarfblasm.exe famicom_microphone_test_battery.asm
;
; Records about 1.5 seconds of 1-bit audio at 43.653 kHz using the Famicom's
; joypad 2 microphone and plays it back on demand. Recordings are saved in
; PRG-RAM, which isn't initialized before first use.
;
; License: This program and source code may be freely distributed, modified,
; and/or used in any way and for any purpose.


PPU_CONTROL := $2000
  kPpuControlScrollXHigh              := %00000001
  kPpuControlScrollYHigh              := %00000010
  kPpuControlVramIncrementRow         := %00000100
  kPpuControlSpriteAddress1000        := %00001000
  kPpuControlBgAddress1000            := %00010000
  kPpuControl8x16Sprites              := %00100000
  kPpuControlSlave                    := %01000000
  kPpuControlEnableNmi                := %10000000
PPU_MASK    := $2001
  kPpuMaskGreyscale                   := %00000001
  kPpuMaskEnableLeftmostColumnBg      := %00000010
  kPpuMaskEnableLeftmostColumnSprites := %00000100
  kPpuMaskEnableBg                    := %00001000
  kPpuMaskEnableSprites               := %00010000
  kPpuMaskEmphasizeRed                := %00100000
  kPpuMaskEmphasizeGreen              := %01000000
  kPpuMaskEmphasizeBlue               := %10000000
PPU_STATUS  := $2002
OAM_ADDRESS := $2003
OAM_DATA    := $2004
PPU_SCROLL  := $2005
PPU_ADDRESS := $2006
PPU_DATA    := $2007

DMC_FREQUENCY := $4010
DMC_RAW       := $4011
DMC_ADDRESS   := $4012
DMC_LENGTH    := $4013
OAM_DMA       := $4014
SOUND_CONTROL := $4015
JOYPAD1       := $4016
  kButtonRight  := $01
  kButtonLeft   := $02
  kButtonDown   := $04
  kButtonUp     := $08
  kButtonStart  := $10
  kButtonSelect := $20
  kButtonB      := $40
  kButtonA      := $80
JOYPAD2       := $4017
FRAME_COUNTER := $4017

has_recorded              := $F0
joypad1_down              := $F4
joypad2_down              := $F5
joypad1_press             := $F6
joypad2_press             := $F7
ppu_mask_value            := $F8


.ORG $0000

    ; iNES Header.
    .db $4E,$45,$53,$1A,$02,$01,$02,$00,$00,$00,$70,$00,$00,$00,$00,$00


.ORG  $6010
.BASE $E000

  Start:
    ; Disable everything.
    SEI
    LDA #0
    STA PPU_CONTROL
    STA PPU_MASK
    STA SOUND_CONTROL
    LDA #$40
    STA FRAME_COUNTER
    CLD

    ; Wait for the PPU to initialize.
    BIT PPU_STATUS
   -
    BIT PPU_STATUS
    BPL -
   -
    BIT PPU_STATUS
    BPL -

    ; Initialize our state.
    LDA #$00
    STA DMC_RAW

    JSR SetUpBackground
    JSR SetUpPalettes

    LDA #$00
    STA has_recorded
    STA joypad1_down
    STA joypad2_down
    STA joypad1_press
    STA joypad2_press

    LDA #(kPpuMaskEnableBg | kPpuMaskEnableLeftmostColumnBg)
    STA ppu_mask_value

    ; Enable interrupts and wait for vblank.
    LDA #kPpuControlEnableNmi
    STA PPU_CONTROL

    JMP $


.dsb $E100 - $, $00

  HandleNmi:
    PHA
    TXA
    PHA
    TYA
    PHA

    LDA ppu_mask_value
    STA PPU_MASK

    ; Write the 'stopped' status symbol.
    BIT PPU_STATUS
    LDA #$21
    STA $2006
    LDA #$AA
    STA $2006

    LDA #$49
    STA $2007

    LDA #$00
    STA PPU_ADDRESS
    STA PPU_ADDRESS

    JSR ReadJoypads

    ; If joypad 1 A is pressed and we've recorded audio, play it back.
    BIT joypad1_press
    BPL +
    ; LDA has_recorded
    ; BEQ +

    LDA #$4A
    JSR PrepForAudio
    JSR PlaySample
    JMP HandleNmi_Resume

   +
    ; If joypad 2 A is pressed, record audio.
    BIT joypad2_press
    BPL HandleNmi_Done

    LDA #$4B
    JSR PrepForAudio
    JSR RecordSample
    JMP HandleNmi_Resume

   HandleNmi_Resume:
    ; Enable the NMI again so we don't hang.
    LDA #kPpuControlEnableNmi
    STA PPU_CONTROL

   HandleNmi_Done:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTI


  PrepForAudio:
    ; Write the status symbol.
    BIT PPU_STATUS
    LDX #$21
    STX $2006
    LDX #$AA
    STX $2006

    STA $2007

    ; Disable NMIs; we can't have interrupts fire during the audio handling because it'll mess up timing.
    LDA #$00
    STA PPU_CONTROL

    STA PPU_ADDRESS
    STA PPU_ADDRESS

    RTS


.dsb $E200 - $, $00

  ; 41 cycles per bit.
  PlaySample:
    ; Start at the beginning of WRAM.
    LDA #$60
    STA $01
    LDA #$00
    STA $00
    TAY

    ; Play audio. We're going to read a byte, shift bits out of it to the left until we've handled all 8
    ; (signaled by a sentinel leaving the byte and making it 0), and increment Y and possibly the page.
    JMP PlaySample_Start

   PlaySample_Done:
    ; We're at 22 cycles. We need to burn cycles before the final write that returns us to 0.
    NOP
    NOP
    NOP
    NOP
    NOP
    BIT $00
    ; 35 cycles.

    LDA #$00
    STA DMC_RAW
    ; 41 cycles.

    RTS

   -
    BIT $00               ; 3
    JMP PlaySample_Start  ; 3

   PlaySample_Loop_Byte:
    ; Go to the next byte. If we're on the same page, burn cycles.
    INY                   ; 2
    BNE -                 ; 2/3

    ; Advance the page. Bail if we're past the end of PRG-RAM.
    INC $01               ; 5
    BMI PlaySample_Done   ; 2/3
    ; 21 cycles total.

   PlaySample_Start:
    ; Load the byte, prep the top bit, and put a sentinel in the bottom bit.
    LDA ($00),Y  ; 5
    SEC          ; 2
    ROL          ; 2
    ; 30 cycles total.

   PlaySample_Loop_Bit:
    ; Choose a value based on the bit.
    BCS +        ; 3/2
    LDX #$00     ;   2
    JMP ++       ;   3
   +
    LDX #$7F     ; 2
    NOP          ; 2
   ++
    ; 37 cycles total.

    STX DMC_RAW  ; 4
    ; 41 cycles total. -----------


    ; (Start the cycle count here. All paths need to take 41 cycles.)

    ; Margin cycles.
    NOP
    BIT $00
    ; 5 cycles total.

    ; Get the next bit. If shifting makes A zero, we've hit the sentinel and need the next byte.
    ASL                       ; 2
    BEQ PlaySample_Loop_Byte  ; 2/3
    ; 9 cycles.

    ; C contains the next bit to play.
    NOP                       ; 2
    NOP                       ; 2
    NOP                       ; 2
    NOP                       ; 2
    NOP                       ; 2

    NOP                       ; 2
    NOP                       ; 2
    NOP                       ; 2
    NOP                       ; 2
    JMP PlaySample_Loop_Bit   ; 3


.dsb $E300 - $, $00

  ; 41 cycles per bit.
  RecordSample:
    ; Start at the beginning of WRAM.
    LDA #$01
    STA has_recorded

    LDA #$60
    STA $01
    LDA #$00
    STA $00
    TAY

    ; Record audio. We're going to read bits into a byte until we've done 8 (signaled by a sentinel entering C), write
    ; the byte, and increment Y and possibly the page.
   RecordSample_Loop_Byte:
    ; 32 cycles total.
    LDA #$01   ; 2
    STA $02    ; 3
    ; 37 cycles total.

   RecordSample_Loop_Read:
    LDA $4016  ; 4
    ; 41 cycles total. --------

    ; (Start the cycle count here. All paths need to take 41 cycles.)

    ; Shift the microphone bit into $02. If C becomes set, $02 has been filled.
    AND #$04                           ; 2
    CMP #$01                           ; 2
    ROL $02                            ; 5
    BCC RecordSample_Loop_WaitForNext  ; 2/3
    ; 11 cycles total.

    LDA $02                            ; 3
    STA ($00),Y                        ; 6
    INY                                ; 2
    BNE +                              ; 2/3
    ; 24 cycles total

    INC $01                            ; 5
    BPL RecordSample_Loop_Byte         ; 2/3

    RTS

   +
    ; 25 cycles total.
    NOP
    NOP
    JMP RecordSample_Loop_Byte
    ; 32 cycles total.

   RecordSample_Loop_WaitForNext:
    ; 12 cycles total.
    NOP
    NOP
    NOP
    NOP
    NOP
    ; 22 cycles total

    NOP
    NOP
    NOP
    NOP
    NOP
    ; 32 cycles total.

    NOP
    JMP RecordSample_Loop_Read
    ; 37 cycles total.


  ReadJoypads:
    ; Strobe the joypads and set a loop-end sentinel.
    LDX #$01
    STX $01
    STX JOYPAD1
    DEX
    STX JOYPAD1

    ; Read the buttons until encountering the sentinel.
   -
    ; Check both bottom bits to support Famicom expansion port controllers.
    LDA JOYPAD1
    AND #$03
    CMP #$01
    LSR
    ROL $00

    LDA JOYPAD2
    LSR
    ROL $01
    BCC -

    ; Get the newly-pressed buttons.
    LDX $00
    TXA
    EOR joypad1_down
    AND $00
    STA joypad1_press
    STX joypad1_down

    LDX $01
    TXA
    EOR joypad2_down
    AND $01
    STA joypad2_press
    STX joypad2_down

    RTS


  SetUpBackground:
    ; Clear nametable 0.
    LDA PPU_STATUS
    LDA #$20
    STA PPU_ADDRESS
    LDA #$00
    STA PPU_ADDRESS

    LDA #$FF
    LDY #$00
   -
    STA PPU_DATA
    STA PPU_DATA
    STA PPU_DATA
    STA PPU_DATA
    INY
    BNE -

    ; Write our text to the screen.
    LDX #$00
   SetUpBackground_SequenceLoop:
    ; If the byte is negative, the data stream is finished.
    LDA dPpuSequence,X
    BMI SetUpBackground_Done

    ; Write the address.
    STA PPU_ADDRESS
    INX

    LDA dPpuSequence,X
    STA PPU_ADDRESS
    INX

    ; Get the length.
    LDA dPpuSequence,X
    TAY
    INX

    ; Write the data.
   -
    LDA dPpuSequence,X
    STA PPU_DATA
    INX

    DEY
    BNE -
    BEQ SetUpBackground_SequenceLoop

   SetUpBackground_Done:
    RTS


  ; VRAM write strings. Format is PPU address, length, data. Struct is $FF terminated and cannot be more than 256 bytes
  ; long.
  dPpuSequence:
    ; ||..,....,....,....,....,....,||
    ;   Records 1.5 seconds of 1-bit
    ;   43,653 Hz audio from the
    ;   Famicom microphone and plays
    ;   it back.
    ;
    ;   Controls:
    ;     Joypad 2 A: Record
    ;     Joypad 1 A: Play
    ;
    ;   Status:
    .db $20,$62, 28,  $1B,$28,$26,$32,$35,$27,$36,$FF,$01,$45,$05,$FF,$36,$28,$26,$32,$31,$27,$36,$FF,$32,$29,$FF,$01,$48,$25,$2C,$37
    .db $20,$82, 24,  $04,$03,$47,$06,$05,$03,$FF,$11,$3D,$FF,$24,$38,$27,$2C,$32,$FF,$29,$35,$32,$30,$FF,$37,$2B,$28
    .db $20,$A2, 28,  $0F,$24,$30,$2C,$26,$32,$30,$FF,$30,$2C,$26,$35,$32,$33,$2B,$32,$31,$28,$FF,$24,$31,$27,$FF,$33,$2F,$24,$3C,$36
    .db $20,$C2, 8,   $2C,$37,$FF,$25,$24,$26,$2E,$45

    .db $21,$02, 9,   $0C,$32,$31,$37,$35,$32,$2F,$36,$46
    .db $21,$24, 18,  $13,$32,$3C,$33,$24,$27,$FF,$02,$FF,$0A,$46,$FF,$1B,$28,$26,$32,$35,$27
    .db $21,$44, 16,  $13,$32,$3C,$33,$24,$27,$FF,$01,$FF,$0A,$46,$FF,$19,$2F,$24,$3C

    .db $21,$A2, 7,   $1C,$37,$24,$37,$38,$36,$46

    .db $FF  ; (Terminate)
   dPpuSequence_End:


  .IF dPpuSequence_End - dPpuSequence > 256
  .ERROR dPpuSequence is too long.
  .ENDIF


  SetUpPalettes:
    LDA PPU_STATUS
    LDA #$3F
    STA PPU_ADDRESS
    LDA #$00
    STA PPU_ADDRESS

    LDX #$20
    LDY #$00
   -
    LDA dPaletteData,Y
    STA PPU_DATA
    INY
    DEX
    BNE -

    RTS

  dPaletteData:
    .db $0F,$30,$16,$0F,$0F,$30,$16,$0F,$0F,$30,$16,$0F,$0F,$30,$16,$0F
    .db $0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F


.dsb $FFFA - $, $00

  .dw HandleNmi
  .dw Start
  .dw Start

  .INCBIN "chr.dat"