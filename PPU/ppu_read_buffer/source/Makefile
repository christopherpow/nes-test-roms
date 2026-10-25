all:	../test_ppu_read_buffer.nes

COMMON=\
		common/binarysearch.s \
		common/build_rom.s \
		common/colors.inc \
		common/console.s \
		common/crc.s \
		common/delay.s \
		common/macros.inc \
		common/neshw.inc \
		common/ppu.s \
		common/print.s \
		common/scanline_delay.s \
		common/shell.inc \
		common/shell.s \
		common/testing.s \
		common/text_out.s
GFX=\
		gfx.spr1.chr \
		gfx.spr2.chr \
		gfx.spr.bin \
		gfx.nta.bin \
		gfx.attr.bin \
		gfx.pal.bin \
		gfx.page1.chr \
		gfx.page2.chr \
		gfx.spr.page3_A.chr \
		gfx.spr.page3_B.chr \
		gfx.bank4.bin \
		gfx.6c00.bin

LISTS=\
		gfx.mixed.dat \
		gfx.1.lst \
		gfx.2.lst \
		gfx.3.lst \
		gfx.4.lst

PREPROCESSED=asm.tmp

#####
OBJS=test_ppu_read_buffer.o
../test_ppu_read_buffer.nes: $(OBJS) nes.cfg
	ld65 -vm -o "$@" $(OBJS) -v -C nes.cfg -m nes.map

test_ppu_read_buffer.o: \
		$(PREPROCESSED) \
		$(GFX) \
		filler.bin
	ca65 --cpu 6502x -g --smart -o "$@" "$<" -t nes -v -I common

$(PREPROCESSED): \
		test_ppu_read_buffer.s \
		graphics.s \
		testing2-begin.s \
		testing2-end.s \
		bank_switch.s \
		crash_proof.s \
		$(COMMON) \
		dte_translate.php inc/dte_reader.php
	php -q dte_translate.php "$<" "$@"  common/ascii.chr


#####
clean:
	rm -f test_ppu_read_buffer.o ../test_ppu_read_buffer.nes
	rm -f $(GFX) $(LISTS)
	rm -f filler.bin

#gfx/source.png: /WWW/kala/The\ Dolomites,\ Alps,\ Italy.jpg
#	convert "$<" -resize 256x224'!' -sharpen 1 "$@"

gfx/source.png: gfx/thomaskinkade50.jpg
	convert "$<" -resize 256x224'!' -sharpen 1 "$@"

#gfx.mixed.dat: gfx/source.png gfx/quantize.php gfx/nes_palette.php
	#php -q gfx/quantize.php "$<" gfx/mixed.png "$@" 1

gfx.mixed.dat: gfx/source.png gfx/quantize_with_sprites
	gfx/quantize_with_sprites "$<" gfx/mixed.png "$@" 1
	
gfx/quantize_with_sprites: gfx/quantize_with_sprites.cc
	g++ -o $@  -Wall -W -Ofast -std=c++0x -g -lgd -fopenmp $^


gfx.1.lst: gfx.mixed.dat gfx/reduce.php
	php -q gfx/reduce.php "$<"   0  64 gfx.1.lst 256  gfx.page1.chr  0 gfx.spr1.chr 0

gfx.2.lst: gfx.mixed.dat gfx/reduce.php
	php -q gfx/reduce.php "$<"  64 128 gfx.2.lst 256  gfx.page2.chr  0 gfx.spr2.chr 64

gfx.3.lst: gfx.mixed.dat gfx/reduce.php
	php -q gfx/reduce.php "$<" 128 184 gfx.3.lst 256  gfx.spr.page3_B.chr 0 '' 1

gfx.4.lst: gfx.mixed.dat gfx/reduce.php
	php -q gfx/reduce.php "$<" 184 240 gfx.4.lst 256  gfx.spr.page3_A.chr 44 '' 44

# Deal with multiple outputs that aren't handled well by GNU make
gfx.page1.chr gfx.spr1.chr: gfx.1.lst
	- @if test -f "$@" ; then :; else rm -f "$<"; $(MAKE) "$<"; fi
gfx.page2.chr gfx.spr2.chr: gfx.2.lst
	- @if test -f "$@" ; then :; else rm -f "$<"; $(MAKE) "$<"; fi
gfx.spr.page3_A.chr: gfx.3.lst
	- @if test -f "$@" ; then :; else rm -f "$<"; $(MAKE) "$<"; fi
gfx.spr.page3_B.chr: gfx.4.lst
	- @if test -f "$@" ; then :; else rm -f "$<"; $(MAKE) "$<"; fi

gfx.pal.bin: $(LISTS) gfx/encode_image.php
	cat $(LISTS) | \
	php -q gfx/encode_image.php gfx/sample.png \
		gfx.spr.bin \
		gfx.nta.bin \
		gfx.attr.bin \
		gfx.pal.bin

# Deal with multiple outputs that aren't handled well by GNU make
gfx.nta.bin gfx.attr.bin gfx.spr.bin: gfx.pal.bin
	- @if test -f "$@" ; then :; else rm -f "$<"; $(MAKE) "$<"; fi

gfx.6c00.bin: filler/help.asm filler/help2.asm
	nasm -Ifiller/ -O9  filler/help.asm -o $@
	nasm -Ifiller/ -O9  filler/help2.asm -o filler.tmp
	tr '\0-\177' '\200-\377' < filler.tmp >> $@

filler.bin: filler/part1.asm filler/apacker-filler
	nasm -Ifiller/ -O9 "$<" -o filler.tmp
	filler/apacker-filler < filler.tmp > "$@.tmp"
	php -r "print str_pad('',5900,chr(255));" > "$@"
	dd if="$@.tmp" conv=notrunc of="$@"
	rm -f "$@.tmp"

filler/apacker-filler: filler/apacker-filler.cc
	g++ -o $@ $<  -Wall -W -Ofast -std=c++0x -fopenmp -fomit-frame-pointer -g

common/ascii_1.chr: common/make_ascii.php $(PREPROCESSED)
	php -q "$<" 0 1 3  $(PREPROCESSED) > "$@"
common/ascii_w.chr: common/make_ascii.php $(PREPROCESSED)
	php -q "$<" 0 2 3  $(PREPROCESSED) > "$@"
