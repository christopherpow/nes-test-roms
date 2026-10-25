.segment "HEADER"
.import MAPPER, SUBMAPPER, BRAM, WRAM, NES2

; set battery bit if we have any battery ram
battery = 2 & (BRAM >> 2 | BRAM >> 1 | BRAM | BRAM << 1)

; mask out NES 2.0 fields if we're not making a NES 2.0 header
nes2mask = (NES2 >> 3) * 255

.byte "NES", $1a            ; fourcc
.byte 2, 4                  ; prgrom and chrrom size
.lobytes MAPPER << 4 | battery
.lobytes MAPPER & $f0 | NES2
.lobytes nes2mask & (SUBMAPPER << 4)
.byte 0                     ; upper bits of prgrom and chrrom size
.lobytes nes2mask & (BRAM << 4 | WRAM)
