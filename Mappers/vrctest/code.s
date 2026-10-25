.include "vrctest.inc"
.include "branch.inc"
; ------------------------------------------------------------- IMPORTS
.import Pin21Lookup, ATLookup, ScanlineIRQTable, CycleIRQTable
.import PaletteData, BoardMapperTable, SubmapperTable, LegacyMapperTable
.import TextPtrTable
.importzp TotalBoards

; ------------------------------------------------------------ ZEROPAGE
.zeropage
; shadow attribute table
attrtable:  .res 64

; buffer for various things
buffer:     .res 20

; space for leaf subroutine local variables
leaflocals: .res 4

; pointer for inline subroutine arguments
argptr:     .res 2

; text related
textptr:    .res 2
texty:      .res 1
textx:      .res 1
textcolor:  .res 1
textlength: .res 1
oampos:     .res 1

; probed register offsets
vrcreg2:    .res 1
vrcreg1:    .res 1
vrcreg3:    .res 1

; irq testing related
irqtime:    .res 1

; variables displayed via string lookup must reside at >= $80
.segment "ZEROPAGE80": zeropage

; probing results
pin3:       .res 1
pin4:       .res 1
pin21:      .res 1
asictype:   .res 1

; VRC4 features
onescreen:  .res 1
swapc000:   .res 1
scnlnirq:   .res 1
cycleirq:   .res 1

; identification
pcbtype:    .res 1

shadowoam = $0200

; ------------------------------------------------------------MAIN CODE
.code
ResetVector:
    sei
    ldx #$ff
    txs
    stx $4017
    inx
    stx $2000
    stx $2001
    jsr WaitVBlank
    stx $4010
    stx $4015
    ldy #0
; initialize zero page and important VRC registers
:       tya
        sta 0,x
        sta $9000,x ; nametable mirroring
        sta $b000,x ; first two CHR ROM pages
        sta $f000,x ; IRQ related registers
        lda #$f0
        sta shadowoam,x
        inx
    bne :-
    jsr WaitVBlank
; initialize nametable
    tax ; x is now $f0
    lda #$20
    sta $2006
    sty $2006
    lda #' '
:       sta $2007
        sta $2007
        sta $2007
        sta $2007
        dex
    bne :-
    jsr PrintText
    textyx 0, 5
    headingtext "VRC2/VRC4 TEST PROGRAM"
    textend
    jsr PrintText
    textyx 3, 8
    headingtext "PIN CONNECTIONS:"
    textend
; ---------------------------------------------------------------------
; probe whether pin 21 is connected
    ldx #15
:       stx $b000
        lda #$03
        jsr ReadCHRROM
        sta buffer,x
        dex
    bpl :-
    jsr CompareTable
    .addr buffer
    .byte 16, 2
; pin 21 connected to CHR ROM A10 (most PCBs)
    .byte 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15
; pin 21 n/c, CHR ROM address lines shifted (PCB 351618 = mapper 22)
    .byte 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7
    bcs :+
        jmp ProbeFailed
:   tax
    lda Pin21Lookup,x
    sta pin21
; fall through
; ---------------------------------------------------------------------
.proc ProbePins3and4
; probe which address lines connect to pin 3 and 4
regdata = leaflocals
color3  = buffer
length3 = buffer+1
buffer3 = buffer+2
color4  = buffer+10
length4 = buffer+11
buffer4 = buffer+12

    inx
    stx regdata ; write 1 if pin21 connected, 2 if pin21 n/c
    ldx #8
    stx length3
    stx length4
    lda #'-'
:       sta buffer3-1,x
        sta buffer4-1,x
        dex
    bne :-
    stx color3
    stx color4
    ldx #7
    lda #1
probeloop:
        tay
        lda regdata
        sta $b000,y
; check the bank mapped at PPU $07FF
; if it's bank 1, we wrote to $B002
; otherwise, it should be bank 0
        lda #$07
        jsr ReadCHRROM
        beq notpin3
        cmp #1
        beq ispin3
        jmp ProbeFailed
notpin3:
; check the bank mapped at PPU $03FF
; if it's bank 16, we wrote to $B001
; otherwise, we wrote to $B000 so it should be bank 1
        lda #$03
        jsr ReadCHRROM
        cmp #1
        beq neither
        cmp #16
        beq ispin4
        jmp ProbeFailed
ispin3:
        txa
        eor #7
        sta buffer3,x
        tya
        sta vrcreg2
        ora pin3
        sta pin3
        inc color3
        bne neither ; always taken
ispin4:
        txa
        eor #7
        sta buffer4,x
        tya
        sta vrcreg1
        ora pin4
        sta pin4
        inc color4
neither:
        lda #0
        sta $b000,y
        dex
        tya
        asl
    bne probeloop

    lda vrcreg1
    ora vrcreg2
    sta vrcreg3

; set color based on number of connected address bits
;  0: probing failed, no further tests possible
;  1: green (PCB)
; 2~: amber (legacy emulation)
    lda #color4-color3
colorloop:
        tax
        ldy color3,x
        bne :+
            jmp ProbeFailed
:       lda #Color::green
        cpy #1
        beq :+
            lda #Color::amber
:       sta color3,x
;       sec ; carry is already set
        txa
        sbc #color4-color3
    bpl colorloop
    jsr PrintText
    textyx 5, 3
    resulttext "PIN 3 (VRC A1):", color3
    resulttext "PIN 4 (VRC A0):", color4
    resulttext "PIN 21 (CHR A10):", pin21
    textend
.endproc
; fall through
; ---------------------------------------------------------------------
; probe nametable mirroring
    ldx #3
    ldy #$ff
:       lda ATLookup,x
        sta $2006
        sty $2006
        stx $2007
        dex
    bpl :-
    ldx #$3
:       stx $9000
        lda #$23
        jsr ReadCHRROM
        sta buffer,x
        lda #$27
        jsr ReadCHRROM
        sta buffer+4,x
        lda #$2b
        jsr ReadCHRROM
        sta buffer+8,x
        lda #$2f
        jsr ReadCHRROM
        sta buffer+12,x
        dex
    bpl :-
    jsr CompareTable
    .addr buffer
    .byte 16, 2
; vertical/horizontal only (VRC2)
;  $9000: 0 1 2 3
    .byte 0,0,0,0   ; $2000
    .byte 1,0,1,0   ; $2400
    .byte 0,1,0,1   ; $2800
    .byte 1,1,1,1   ; $2C00
; one-screen supported (VRC4)
;  $9000: 0 1 2 3
    .byte 0,0,0,1   ; $2000
    .byte 1,0,0,1   ; $2400
    .byte 0,1,0,1   ; $2800
    .byte 1,1,0,1   ; $2C00
    sta onescreen
; ---------------------------------------------------------------------
; probe PRG bank switching
    ldy #0
    sty $8000
    iny
    sty $a000
    lda #2
    ldx vrcreg2
    sta $9000,x
    dey
    sty $8000
    lda $9fff
    sta buffer
    lda $bfff
    sta buffer+1
    lda $dfff
    sta buffer+2
    tya
    sta $9000,x
    lda $9fff
    sta buffer+3
    lda $bfff
    sta buffer+4
    lda $dfff
    sta buffer+5
    jsr CompareTable
    .addr buffer
    .byte 6, 2
; only $8000 and $a000 switchable (VRC2)
    .byte $fc,$fd,$fe,$fc,$fd,$fe
; can switch $c000 and $a000 instead (VRC4)
    .byte $fe,$fd,$fc,$fc,$fd,$fe
    sta swapc000
; ---------------------------------------------------------------------
; probe IRQ functions
    ldx #<ScanlineIRQTable
    ldy #>ScanlineIRQTable
    lda #2
    jsr IRQTest
    sta scnlnirq
    ldx #<CycleIRQTable
    ldy #>CycleIRQTable
    lda #6
    jsr IRQTest
    sta cycleirq
; ---------------------------------------------------------------------
    jsr PrintText
    textyx 11, 9
    headingtext "VRC4 FEATURES:"
    textend
; fall through
; ---------------------------------------------------------------------
.proc IdentifyASIC
threshold = leaflocals

    lda #Result::vrc2
    sta asictype
; if all features are 0 then it's VRC2
    lda onescreen
    ora swapc000
    ora scnlnirq
    ora cycleirq
    beq isvrc2
; if onescreen = 1, swapc000 = 1, and any IRQ test passed, it's VRC4
    ldy #Result::yes
    cpy onescreen
    bne notvrc4
    cpy swapc000
    bne notvrc4
    lda scnlnirq
    ora cycleirq
    beq notvrc4
    inc asictype
    bne makered ; always taken
notvrc4:
    lda #Result::unknown
    sta asictype
    iny ; y is now Result::yes+1
makered:
; if VRC4, make any "no" results red
; if neither, make both "no" and "yes" red
    sty threshold
    ldx #cycleirq-onescreen
loop:
        lda onescreen,x
        cmp threshold
        bcs :+
            adc #Result::redno-Result::no
            sta onescreen,x
:       dex
    bpl loop
isvrc2:
    jsr PrintText
    textyx 13, 3
    resulttext "ONE-SCREEN MIRRORING:", onescreen
    resulttext "FIX 8000,SWITCH C000:", swapc000
    resulttext "PSEUDO-SCANLINE IRQ:", scnlnirq
    resulttext "CYCLE COUNTER IRQ:", cycleirq
    textend
.endproc
; fall through
; ---------------------------------------------------------------------
.proc IdentifyPCB
mapcolor  = buffer
maplength = buffer+1
mapnumber = buffer+2
subcolor  = buffer+4
sublength = buffer+5
subnumber = buffer+6
    ldx #0
    stx subnumber
    inx
    stx sublength
    inx
    stx mapnumber ; all our mapper numbers start with '2'
    stx maplength
    jsr CompareTable
    .addr pin3
    .byte 4, TotalBoards
;         pin3 pin4 pin21        asictype
:   .byte $04, $02, Result::yes, Result::vrc4 ; mapper 21 sub 1
    .byte $80, $40, Result::yes, Result::vrc4 ; mapper 21 sub 2
    .byte $01, $02, Result::nc,  Result::vrc2 ; mapper 22
    .byte $02, $01, Result::yes, Result::vrc4 ; mapper 23 sub 1
    .byte $08, $04, Result::yes, Result::vrc4 ; mapper 23 sub 2
    .byte $02, $01, Result::yes, Result::vrc2 ; mapper 23 sub 3
    .byte $01, $02, Result::yes, Result::vrc4 ; mapper 25 sub 1
    .byte $04, $08, Result::yes, Result::vrc4 ; mapper 25 sub 2
    .byte $01, $02, Result::yes, Result::vrc2 ; mapper 25 sub 3
    .assert * - :- = 4 * TotalBoards, error, "Inconsistent board list length"
    bcc notboard
    tax
;   sec ; carry is already set
    adc #Result::board-1
    sta pcbtype
    lda BoardMapperTable,x
    sta mapnumber+1
    lda SubmapperTable,x
    sta subnumber
    lda #Color::green
    bne done ; always taken

notboard:
    jsr CompareTable
    .addr pin3
    .byte 3, 5
    .byte $84, $42, Result::yes ; mapper 21
    .byte $01, $02, Result::nc  ; mapper 22
    .byte $0a, $05, Result::yes ; mapper 23
    .byte $2a, $15, Result::yes ; mapper 23 (FCEUX)
    .byte $05, $0a, Result::yes ; mapper 25
    bcs :+
        lda #'?'
        sta mapnumber
        sta mapnumber+1
        bcc bad ; always taken
:   tax
    lda LegacyMapperTable,x
    sta mapnumber+1
    lda asictype
    cmp #Result::vrc4
    beq :+
        cmp #Result::vrc2
        bne bad
:   lda #Result::board+TotalBoards
    sta pcbtype
    lda #Color::amber
    bne done ; always taken
bad:
    lda #Result::unknown
    sta pcbtype
    lda #Color::red
done:
    sta mapcolor
    sta subcolor
    jsr PrintText
    textyx 18, 9
    headingtext "IDENTIFICATION:"
    textend
    jsr PrintText
    textyx 20, 7
    resulttext "ASIC:", asictype
    resulttext "PCB:",  pcbtype
    resulttext "MAPPER:", mapcolor
    textend
    lda subnumber
    beq :+
        jsr PrintText
        textyx 23, 7
        resulttext "SUBMAPPER:", subcolor
        textend
:   ldy #BGColor::good
.endproc
; fall through
; ------------------------------------------------------------- AllDone
AllDone:
; send attribute table to PPU
    lda #$23
    sta $2006
    ldx #$c0
    stx $2006
:       lda <(attrtable-$c0),x
        sta $2007
        inx
    bne :-
; send sprite list to PPU
    stx $2003
    lda #>shadowoam
    jsr WaitVBlank
    sta $4014
; send palette to PPU
    lda #$3f
    sta $2006
    stx $2006
:       sty $2007
.repeat 3
        lda PaletteData,x
        sta $2007
        inx
.endrep
        cpx #3*8
    bcc :-
; set scroll position and turn on screen
    ldx #$00
    stx $2000
    lda #$1e
    sta $2001
    stx $2005
    lda #240-screenvpos
    sta $2005
forever:
    jmp forever

ProbeFailed:
    jsr PrintText
    textyx 10, 5
    resulttext "MAPPER PROBING FAILED!", 0
    textend
    ldy #BGColor::bad
    jmp AllDone
; ---------------------------------------------------- END OF MAIN CODE

; ---------------------------------------------------------- WaitVBlank
WaitVBlank:
    bit $2002
:       bit $2002
    bpl :-
    rts

; ---------------------------------------------------------- ReadCHRROM
ReadCHRROM:
    sta $2006
    lda #$ff
    sta $2006
    lda $2007
    lda $2007
    rts

; ------------------------------------------------------------- IRQTest
.proc IRQTest
vector  = leaflocals
irqmode = leaflocals+2
result  = leaflocals+3

    stx argptr
    sty argptr+1
    sta irqmode
    lda #Result::yes
    sta result
    lda #>SlideStart
    sta vector+1
loop:
    ldy #0
    sty irqtime
    lda (argptr),y
    beq done
    sta $f000
    lsr
    lsr
    lsr
    lsr
    ldx vrcreg1
    sta $f000,x
    iny
    lda (argptr),y
    tax
    iny
    lda (argptr),y
    sta vector
    sec ; add y+1 to ptr
    tya
    adc argptr
    sta argptr
    bcc :+
        inc argptr+1
:   lda irqmode
    ldy vrcreg2
    sta $f000,y
; now counting cycles until IRQ
    ldy #Result::early  ; 2
    cli                 ; 2
    jmp (vector)        ; 5

SlideStart:
        cmp #$c9        ; 0~2
        cmp #$c5        ; 0~2
SlideEnd:
    nop                 ; 2
:       dex             ; 2*x
    tbne :-             ; 3*x-1
    iny
; IRQ should fire between these two INYs
    iny
; give some time for the IRQ to fire late
:       inx
    bpl :-
    sei
    lda irqtime
    beq abort
    cmp #Result::early+1
    beq loop
        sta result
    bne loop ; always taken

done:
    lda result
abort:
    rts

.assert >SlideEnd = >SlideStart, error, "IRQ clock slide crosses pages"
.export SlideEnd
.endproc

IRQVector:
    pha
    txa
    pha
    sty irqtime
    ldx vrcreg3
    sta $f000,x
    pla
    tax
    pla
NMIVector:
    rti

; -------------------------------------------------------- CompareTable
.proc CompareTable
ramptr   = leaflocals
columns  = leaflocals+2
rows     = leaflocals+3
argbytes = rows+1-ramptr

    pla
    sta argptr
    pla
    sta argptr+1
    ldy #argbytes
:       lda (argptr),y
        sta ramptr-1,y
        dey
    bne :-
    clc
    lda #argbytes+1
    adc argptr
    sta argptr
    bcc :+
        inc argptr+1
:   ldx rows
rowloop:
    ldy #0
:       lda (argptr),y
        cmp (ramptr),y
        bne nomatch
        iny
        cpy columns
    bcc :-

; found a match! result = (rows - x)
; move argptr past remaining table rows
rowsleft = ramptr ; don't need ramptr any more
    stx rowsleft
skiprowloop:
        clc
        lda columns
        adc argptr
        sta argptr
        bcc :+
            inc argptr+1
:       dex
    bne skiprowloop
    sec
    lda rows
    sbc rowsleft
;   sec ; carry is already set
    jmp (argptr) ; return

nomatch:
    clc
    lda columns
    adc argptr
    sta argptr
    bcc :+
        inc argptr+1
:   dex
    bne rowloop

; none of the rows matched
    lda rows
    clc
    jmp (argptr) ; return
.endproc

; ----------------------------------------------------------- PrintText
.proc PrintText

    pla
    sta argptr
    pla
    sta argptr+1
    ldy #1
    lda (argptr),y
    sta texty
    iny
    lda (argptr),y
    sta textx
    iny
lineloop:
    lda (argptr),y
    beq done
    sta textlength
    sec ; add y+1 to ptr
    tya
    adc argptr
    sta textptr
    lda #0
    sta textcolor
    adc argptr+1
    sta textptr+1
    jsr BGTextCore
; advance argptr to end of string
    clc
    lda textlength
    adc textptr
    sta argptr
    lda #0
    tay
    adc textptr+1
    sta argptr+1
    lda (argptr),y
    bmi stringlookup
    beq novariable
; $01-$7F: variable is string buffer in zero page
        sta textptr
        sty textptr+1
        bpl nolookup
; $80-$FF: variable is ROM string index
stringlookup:
        tax
        lda 0,x
        asl
        tax
        lda TextPtrTable,x
        sta textptr
        lda TextPtrTable+1,x
        sta textptr+1
nolookup:
    lda (textptr),y
    sta textcolor
    iny
; right justify (textx = 32 - textx - textlength)
    lda textx
    pha
    lda (textptr),y
    tax
    eor #$ff
    sec
    adc #32
;   sec ; carry is already set
    sbc textx
    sta textx
;   sec ; add y+1 to ptr; carry is already set
    tya
    adc textptr
    sta textptr
    bcc :+
        inc textptr+1
:   stx textlength
    cpx #9
    bcs printvartobg
    jsr SpriteTextCore
nextline:
    pla
    sta textx
novariable:
    inc texty
    ldy #1
    bne lineloop ; always taken

done:
    sec ; add y+1 to ptr
    tya
    adc argptr
    sta argptr
    bcc :+
        inc argptr+1
:   jmp (argptr) ; return

printvartobg:
    jsr BGTextCore
    jmp nextline
.endproc

; ---------------------------------------------------------- BGTextCore
.proc BGTextCore
temp        = leaflocals
attrymask   = leaflocals+1
attrxmask   = leaflocals+2

    lda texty
.repeat 3
    lsr
    ror temp
.endrep
    ora #$20
    sta $2006
    lda temp
    and #$e0
    ora textx
    sta $2006
; write text to nametable
    ldy #0
:       lda (textptr),y
        sta $2007
        iny
        cpy textlength
    bcc :-
; for white text, nothing more to do
    lda textcolor
    beq done
; for color text, update the attribute table
; set vertical attribute mask
    lda #$0f
    bit temp ; bit 6 of temp is bit 1 of texty
    bvc :+
        lda #$f0
:   sta attrymask
; let x = byte position in attribute table: (y >> 2) << 3 | x >> 2
    lda texty
    asl
    and #$38
    sta temp
    lda textx
    lsr
    lsr
    ora temp
    tax
; let y = attribute quarter-cells spanned by text:
; length / 2, plus 1 if either end (or both) is unaligned
    lda textlength
    lsr
    tay
    lda #1
    bcs odd
    bit textx
    beq even
odd:
        iny
even:
    asl ; a is now 2
    bit textx
    beq nofirsthalf
; do the first quarter-cell
    lda #$cc
    jsr modifycell
    beq done
attrloop:
    inx
nofirsthalf:
    dey
    beq lasthalf
; do a full half-cell
    lda #$ff
    jsr modifycell
    bne attrloop
done:
    rts

; do the last quarter-cell
lasthalf:
    lda #$33
; fall through
modifycell:
; (src ^ dst) & mask ^ dst == src & mask | dst & ~mask
    sta attrxmask
    lda attrtable,x
    eor textcolor
    and attrymask
    and attrxmask
    eor attrtable,x
    sta attrtable,x
    dey
    rts
.endproc

; ------------------------------------------------------ SpriteTextCore
.proc SpriteTextCore
spritey = leaflocals
spritex = leaflocals+1
attr    = leaflocals+2

    lda texty
    asl
    asl
    asl
    adc #screenvpos-1
    sta spritey
    lda textx
    asl
    asl
    asl
    sta spritex
    lda textcolor
    and #3
    sta attr
    ldx oampos
    ldy #0
loop:
    lda spritex
    sta shadowoam+3,x
;   clc ; carry is already clear
    adc #8
    sta spritex
    lda (textptr),y
    cmp #' '
    beq skipspace
        sta shadowoam+1,x
        lda spritey
        sta shadowoam,x
        inx
        inx
        lda attr
        sta shadowoam,x
        inx
        inx
skipspace:
    iny
    cpy textlength
    bcc loop

; done
    stx oampos
    rts
.endproc

; ------------------------------------------------------------- VECTORS
.segment "VECTORS"
.addr NMIVector, ResetVector, IRQVector
