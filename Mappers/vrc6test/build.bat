@echo off
ca65 raw.asm -D vrc24
ld65 -C mem.cfg raw.o
copy /B ines_mapper24_16Kprg_32Kchr+rom.bin+chr0+chr1 vrc6test24.nes
ca65 raw.asm
ld65 -C mem.cfg raw.o
copy /B ines_mapper26_16Kprg_32Kchr+rom.bin+chr0+chr1 vrc6test26.nes
