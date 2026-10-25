del ram_retain.o
del ram_retain.nes
cc65\bin\ca65 ram_retain.s
cc65\bin\ld65 -C nrom.cfg -o ram_retain.nes ram_retain.o -m ram_retain.txt
@pause