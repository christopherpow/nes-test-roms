del n163_soundram_init.o
del n163_soundram_init.nes
cc65\bin\ca65 n163_soundram_init.s -g
cc65\bin\ld65 -C simple32.cfg -o n163_soundram_init.nes n163_soundram_init.o -m n163_soundram_init.txt --dbgfile n163_soundram_init.dbg
@pause