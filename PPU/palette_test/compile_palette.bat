del temp\palette.o
del temp\palette_pal.o
del temp\palette.nes
del temp\palette_pal.nes
cc65\bin\ca65 palette.s -D NTSC -g -o temp\palette.o
cc65\bin\ca65 palette.s -D PAL -g -o temp\palette_pal.o
cc65\bin\ld65 -C nrom.cfg -o temp\palette.nes temp\palette.o -m temp\palette_map.txt -Ln temp\palette_labels.txt --dbgfile temp\palette.dbg
cc65\bin\ld65 -C nrom.cfg -o temp\palette_pal.nes temp\palette_pal.o -m temp\palette_pal_map.txt -Ln temp\palette_pal_labels.txt --dbgfile temp\palette_pal.dbg
python palette_symbols.py
@pause