all:	../cpu_dummy_writes_oam.nes \
	../cpu_dummy_writes_ppumem.nes

../cpu_dummy_writes_oam.nes: cpu_dummy_writes_oam.o
	ld65  -o $@ $^ -v -C nes.cfg

cpu_dummy_writes_oam.o: cpu_dummy_writes_oam.s
	ca65 --smart -o $@ $^ -t nes -v -I common

#####
../cpu_dummy_writes_ppumem.nes: cpu_dummy_writes_ppumem.o
	ld65  -o $@ $^ -v -C nes.cfg

cpu_dummy_writes_ppumem.o: cpu_dummy_writes_ppumem.s
	ca65 --smart -o $@ $^ -t nes -v -I common

#####
clean:
	rm -f cpu_dummy_writes_oam.o ../cpu_dummy_writes_oam.nes
	rm -f cpu_dummy_writes_ppumem.o ../cpu_dummy_writes_ppumem.nes
