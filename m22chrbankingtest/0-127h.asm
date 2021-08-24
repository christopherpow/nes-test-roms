.segment "HEADER"
MAPPER = 22
SUBMAPPER = 0	

	.byte "NES", $1a	; iNES header identifier
	.byte 1	; PRG ROM - 16KiB = 1*16
	.byte 16	; CHR ROM - 128KiB = 16*8
	.byte ((MAPPER&15)<<4)	; 4:mapper 0  &9=1:vertical &4:noTrainer &2:noBatt
	.byte ((MAPPER&240))|$08	; 4:mapper 00  &C=8:NES2 &2:notVS &1:notPC10
	.byte ((MAPPER&3840)>>8)|(SUBMAPPER<<4)	; 4:mapper 000 4:submapper 0
	.byte $00	; > 4MB PRG, > 2MB CHR
	.byte $00	; no NV nor volatile PRG RAM
	.byte $00	; no NV CHRRAM, no volatile CHRRAM
	.byte 0	; NTSC preferred
	.byte 0	; nothing for VS
	.byte 0,0	; padding

.RODATA
	
.segment "CHR"
	.incbin "0-127.chr"
