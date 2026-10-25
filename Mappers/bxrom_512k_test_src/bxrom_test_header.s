; iNES header
.segment "HEADER"

INES_MAPPER = 34 ; BNROM
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte 32 ; 32 * 16k = 8 * 32k banks = 512k PRG (oversized BNROM)
.byte 0  ; 8k CHR-RAM
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding
