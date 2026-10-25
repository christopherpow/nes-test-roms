del n163_soundram.o
del n163_soundram.nes
cc65\bin\ca65 n163_soundram.s -g
cc65\bin\ld65 -C simple32.cfg -o n163_soundram.nes n163_soundram.o -m n163_soundram.txt --dbgfile n163_soundram.dbg
@pause