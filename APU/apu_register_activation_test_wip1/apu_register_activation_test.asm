; To assemble: snarfblasm.exe palette_read_test.asm

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


joypad1_press         := $F4
joypad1_down          := $F5
joypad2_press         := $F6
joypad2_down          := $F7
palette_base          := $F8
ppu_mask_value        := $F9
ppu_open_bus_value    := $FA
bit_test              := $FB


sprite_region := $0200

kOamDmaTarget := $40

.ORG $0000

    ; iNES Header.
    .db $4E,$45,$53,$1A,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


; Play a 17 byte sample. Does DMA every 428 cycles.
; Read from APU open bus until value reflects DMA.
; Then prime the PPU and time a jump to $3FFE with A = #$40.
; DMAs should be 432 cycles apart.


.ORG  $0010
.BASE $C000

  .db $40,$40

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

    LDA #$4E
    STA DMC_FREQUENCY

    LDX #$00
    STX DMC_ADDRESS
    STX DMC_LENGTH
    STX DMC_RAW
   -
    LDA #$00
    STA $00,X
    PHA
    STA $0200,X
    STA $0300,X
    STA $0400,X
    STA $0500,X
    STA $0600,X
    STA $0700,X
    INX
    BNE -

    DEX
    TXS

    LDA #kPpuMaskEnableBg | kPpuMaskEnableLeftmostColumnBg
    STA ppu_mask_value

   -
    BIT PPU_STATUS
    BPL -

    JSR SetUpPalettesFinal
    JSR ClearNametable

    JMP SyncDmcDma
    ; JSR WriteOamDump

   ; -
    ; BIT PPU_STATUS
    ; BPL -

    ; LDA #$00
    ; STA PPU_SCROLL
    ; STA PPU_SCROLL

    ; LDA #0
    ; STA PPU_CONTROL

    ; LDA ppu_mask_value
    ; STA PPU_MASK

    ; JMP $



.dsb $D000 - $, $00
  SyncDmcDma:
    LDA #$10
    STA SOUND_CONTROL
    NOP
    NOP

   SyncDmcDma_Loop:
   -
    LDA $5000
    AND #$10
    BNE -
   .IF >$ != >SyncDmcDma_Loop
   .ERROR Branch should not cross page boundary.
   .ENDIF

    ; Now we have executed 9 cycles since the DMA initiated.
    ; Burn 511 cycles.
    LDY #51
   -
    NOP
    BIT $00
    DEY
    BNE -

    ; 520. Spend 36.
    LDX #$22
    STX PPU_ADDRESS
    LDY #$00
    STY PPU_ADDRESS
    LDA #$14
    STA PPU_DATA
    STX PPU_ADDRESS
    STY PPU_ADDRESS
    LDA PPU_DATA

    LDA #$8D
    STA PPU_STATUS
    
    ; 556. Spend 13.
    LDA #>ReadOam
    PHA
    LDA #<ReadOam
    PHA
    PHP

    ; 569. Spend 7.
    LDA #$40
    JMP $3FFE


.dsb $D100 - $, $00
  ReadOam:
    ; On the first pass, we have a ton of time before OAM should decay.
    LDX #$00
    STX SOUND_CONTROL
   --
    LDY #8
   -
    LDA OAM_DATA
    STA $0700,X
    STA OAM_DATA
    INX
    BEQ +
    DEY
    BNE -
    JSR RefreshOam
    JMP --
    
   +
    JSR WriteOamDump

    LDA #$00
    STA PPU_SCROLL
    STA PPU_SCROLL
    STA PPU_CONTROL
   -
    BIT PPU_STATUS
    BPL -
    LDA #kPpuMaskEnableBg | kPpuMaskEnableLeftmostColumnBg
    STA PPU_MASK
    
    JMP $
    


  RefreshOam:
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20

    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20

    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20

    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20

    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20

    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20

    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20

    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20
    .db $AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20,$AD,$04,$20,$8D,$04,$20

    RTS


  WriteOamDump:
    LDA #$20
    STA PPU_ADDRESS
    LDA #$E0
    STA PPU_ADDRESS

    LDY #$00
   -
    LDA $0700,Y

    TAX
    LSR
    LSR
    LSR
    LSR
    STA PPU_DATA
    TXA
    AND #$0F
    STA PPU_DATA

    INY
    BNE -

    RTS


  ClearNametable:
    BIT PPU_STATUS
    LDA #$20
    STA PPU_ADDRESS
    LDA #$00
    STA PPU_ADDRESS

    LDX #$FD
    LDY #120
   -
    LDA #$FE
    STA PPU_DATA
    STX PPU_DATA
    LDA #$FC
    STA PPU_DATA
    STX PPU_DATA
    LDA #$FC
    STA PPU_DATA
    STX PPU_DATA
    LDA #$FC
    STA PPU_DATA
    STX PPU_DATA
    DEY
    BNE -

    LDY #64
    LDA #%01110111
   -
    STA PPU_DATA
    DEY
    BNE -

    RTS


  SetUpPalettesFinal:
    LDA PPU_STATUS
    LDA #$3F
    STA PPU_ADDRESS
    LDA #$00
    STA PPU_ADDRESS

    LDX #$20
    LDY #$00
   -
    LDA dPaletteDataFinal,Y
    STA PPU_DATA
    INY
    DEX
    BNE -

    RTS

  dPaletteDataFinal:
    .db $0F,$00,$30,$30,$0F,$10,$30,$30,$0F,$20,$30,$30,$0F,$30,$30,$30
    .db $0F,$30,$30,$30,$0F,$30,$30,$30,$0F,$30,$30,$30,$0F,$30,$30,$30


.dsb $FFFA - $, $00

  .dw Start
  .dw Start
  .dw ReadOam

  .INCBIN "chr.dat"