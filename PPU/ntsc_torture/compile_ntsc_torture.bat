del temp\ntsc_torture.o
del temp\ntsc_torture.nes
cc65\bin\ca65 ntsc_torture.s -g -o temp\ntsc_torture.o
cc65\bin\ld65 -C nrom.cfg -o temp\ntsc_torture.nes temp\ntsc_torture.o -m temp\ntsc_torture_map.txt -Ln temp\ntsc_torture_labels.txt
@pause