cl65 -t nes -c --create-dep 0-127.c.d -Cl -I ../../knes/knes -Oris --listing 0-127.c.lst -o 0-127.o 0-127.c
cl65 -t nes -c --create-dep 0-127h.asm.d -Cl -I ../../knes/knes -Oris --listing 0-127h.asm.lst -o 0-127h.o 0-127h.asm
cl65 -t none --mapfile 0-127.map -Wl --dbgfile,0-127.nes.dbg -o 0-127.nes -C 0-127.cfg 0-127h.o 0-127.o ../../knes/knes/knes.lib
