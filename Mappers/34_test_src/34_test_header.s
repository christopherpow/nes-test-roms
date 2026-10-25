; iNES header
.segment "HEADER"

; DEFINE
;   PRG_BANKS
;   CHR_BANKS
;   SUBMAPPER
;   WRAM

INES_MAPPER = 34
INES_MIRROR = 0 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF
INES_20     = %00001000 ; NES 2.0 flag for bit 7

.if CHR_BANKS = 0
	VRAM = 7
.else
	VRAM = 0
.endif

.byte 'N', 'E', 'S', $1A ; ID
.byte PRG_BANKS ; 16k PRG banks
.byte CHR_BANKS ; 8k CHR-ROM banks
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000) | INES_20
.byte (SUBMAPPER << 4) ; NES 2.0 ex mapper
.byte $0 ; NES 2.0 ex ROM size
.byte WRAM ; NES 2.0 ex RAM size
.byte VRAM ; NES 2.0 ex VRAM size
.byte $02 ; NES 2.0 ex TV system (NTSC/PAL)
.byte $00 ; NES 2.0 ex VS
.byte $0, $0 ; padding
