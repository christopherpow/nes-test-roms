	.export _write8000, _writeA000, _writeC000, _writeE000
	.export _cputc

.segment "HEADER"
MAPPER = 1
SUBMAPPER = 5

	.byte "NES", $1a	; iNES header identifier
	.byte 2	; PRG ROM - 32KiB = 2*16
	.byte 1	; CHR ROM - 8KiB = 1*8
	.byte ((MAPPER&15)<<4)	; 4:mapper 0  &9=1:vertical &4:noTrainer &2:noBatt
	.byte ((MAPPER&240))|$08	; 4:mapper 00  &C=8:NES2 &2:notVS &1:notPC10
	.byte ((MAPPER&3840)>>8)|(SUBMAPPER<<4)	; 4:mapper 000 4:submapper 0
	.byte $00	; > 4MB PRG, > 2MB CHR
	.byte $00	; no NV nor volatile PRG RAM
	.byte $00	; no NV CHRRAM, no volatile CHRRAM
	.byte 0	; NTSC preferred
	.byte 0	; nothing for VS
	.byte 0,0	; padding

.CODE

.proc _write8000
	sta $8000
	lsr
	sta $8000
	lsr
	sta $8000
	lsr
	sta $8000
	lsr
	sta $8000
	rts
.endproc

.proc _writeA000
	sta $A000
	lsr
	sta $A000
	lsr
	sta $A000
	lsr
	sta $A000
	lsr
	sta $A000
	rts
.endproc

.proc _writeC000
	sta $C000
	lsr
	sta $C000
	lsr
	sta $C000
	lsr
	sta $C000
	lsr
	sta $C000
	rts
.endproc

.proc _writeE000
	sta $E000
	lsr
	sta $E000
	lsr
	sta $E000
	lsr
	sta $E000
	lsr
	sta $E000
	rts
.endproc

.proc _cputc
	sta $2007
	rts
.endproc
	
.segment "CONTAINS0"
	.byt 0
.segment "CONTAINS1"
	.byt 1
	
.segment "CHR"
	.incbin "pearl_ascii.chr"

