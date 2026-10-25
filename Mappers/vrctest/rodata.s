.include "vrctest.inc"

.rodata
; ----------------------------------------------------- IRQ TIMING DATA
.import SlideEnd

; "less accurate emus" means emulators that check IRQ/NMI between
; instructions (e.g. FCEU), rather than on the last cycle of each
; instruction as a real 6502 does

; adjust = 12 ; ok on less accurate emus, late on h/w?
adjust = 11   ; ok on less accurate emus and h/w?
; adjust = 10 ; early on less accurate emus, ok on h/w?

.macro irqtiming cyc
.assert (cyc - adjust) >= 5, error, "irqtiming: too few cycles"
.byte (cyc - adjust) / 5
.byte <SlideEnd - (cyc - adjust) .mod 5
.endmacro

ScanlineIRQTable:
.repeat 11, row
    .byte 255 - row
    cycles .set (343 + row * 341) / 3
    irqtiming cycles
.endrep
.byte 0

mincycles = adjust + 5
CycleIRQTable:
.repeat 11, row
    .byte 256 - (row + mincycles)
    cycles .set row + mincycles
    irqtiming cycles
.endrep
.byte 0
; ---------------------------------------------------------------- DATA
Pin21Lookup:
.byte Result::yes, Result::nc

ATLookup:
.byte $23, $27, $2b, $2f

PaletteData:
.repeat 2
.byte $0f, $00, $20 ; white
.byte $0f, $16, $26 ; red
.byte $0f, $17, $27 ; amber
.byte $0f, $19, $29 ; green
.endrep

BoardMapperTable:
.byte 1, 1
.byte 2
.byte 3, 3, 3
.byte 5, 5, 5

SubmapperTable:
.byte 1, 2
.byte 0
.byte 1, 2, 3
.byte 1, 2, 3

LegacyMapperTable:
.byte 1, 2, 3, 3, 5
; ---------------------------------------------------------------- TEXT
TextPtrTable:
.addr TextNo
.addr TextYes
.addr TextUnknown
.addr TextEarly
.addr TextNC
.addr TextLate
.addr TextRedNo
.addr TextRedYes
.addr TextVRC2
.addr TextVRC4
BoardNames:
.addr Text352398    ; mapper 21 sub 1 (VRC4; Wai Wai World 2)
.addr Text352889    ; mapper 21 sub 2 (VRC4; Ganbare Goemon Gaiden 2)
.addr Text351618    ; mapper 22 (VRC2; Ganbare Pennant Race, Twinbee 3)
.addr TextWorldHero ; mapper 23 sub 1 (HKO "World Hero"--is it really VRC4?)
.addr Text352396    ; mapper 23 sub 2 (VRC4; four games)
.addr Text350926Etc ; mapper 23 sub 3 (VRC2; six games)
.addr Text351406    ; mapper 25 sub 1 (VRC4; three games)
.addr Text352400    ; mapper 25 sub 2 (VRC4; two TMNT games)
.addr Text351948    ; mapper 25 sub 3 (VRC2; Ganbare Goemon Gaiden)
BoardNamesEnd:
.addr TextLegacyEmu

TextNo:
colortext green, "NO"
TextYes:
colortext green, "YES"
TextUnknown:
colortext red,   "???"
TextEarly:
colortext amber, "EARLY"
TextNC:
colortext green, "N/C"
TextLate:
colortext amber, "LATE"
TextRedNo:
colortext red,   "NO"
TextRedYes:
colortext red,   "YES"
TextVRC2:
colortext green, "VRC2"
TextVRC4:
colortext green, "VRC4"
Text352398:
colortext green, "352398"
Text352889:
colortext green, "352889"
Text351618:
colortext green, "351618"
TextWorldHero:
colortext green, "WORLD HERO"
Text352396:
colortext green, "352396"
Text350926Etc:
colortext green, "350926,ETC."
Text351406:
colortext green, "351406"
Text352400:
colortext green, "352400"
Text351948:
colortext green, "351948"
TextLegacyEmu:
colortext amber, "LEGACY EMU"
; ------------------------------------------------------------- EXPORTS
.export Pin21Lookup, ATLookup, ScanlineIRQTable, CycleIRQTable
.export PaletteData, BoardMapperTable, SubmapperTable, LegacyMapperTable
.export TextPtrTable

.exportzp TotalBoards = <(LegacyMapperTable - SubmapperTable)

.assert (SubmapperTable - BoardMapperTable) = TotalBoards, error, "Inconsistent board list length"
.assert (BoardNamesEnd - BoardNames) = TotalBoards * 2, error, "Inconsistent board list length"

; ------------------------------------------------------------- CHR ROM
.segment "CHARS"
.repeat 32, chrbank
    .incbin "font.chr"
    .repeat 1024-832
        .byte chrbank
    .endrep
.endrep
