AS65 = ca65
LD65 = ld65
CC = gcc
ifdef COMSPEC
EMU := start
DOTEXE=.exe
else
EMU := gnome-open
DOTEXE=
endif
CFLAGS = -std=gnu99 -Wall -DNDEBUG -O
CFLAGS65 = 
objdir = obj/nes
srcdir = src
imgdir = tilesets

objlist = main 
objlistntsc = $(foreach o,$(objlist),$(objdir)/$(o).o)

.PHONY: run dist zip all2bts

run: bntest.nes
	$(EMU) $<

all2bts: $(foreach o,$(2btlist),$(objdir)/$(o).2bt)

# Actually this depends on every single file in zip.in, but currently
# we use changes to bntest.nes as a heuristic for when something was
# changed.  Limitation: it won't see changes to docs or tools.
dist: zip
zip: bntest.zip
bntest.zip: zip.in bntest.nes makefile
	zip -9 -u $@ -@ < $<

map.txt bntest.nes: nes.ini $(objlistntsc)
	$(LD65) -C $^ -m map.txt -o bntest.nes

$(objdir)/%.o: $(srcdir)/%.s $(srcdir)/nes.h $(srcdir)/ram.h
	$(AS65) $(CFLAGS65) $< -o $@

$(objdir)/%.o: $(objdir)/%.s
	$(AS65) $(CFLAGS65) $< -o $@

$(objdir)/main.o: $(objdir)/font16.chr

$(objdir)/%16.chr: tilesets/%16.png
	python tools/pilbmp2nes.py -H 16 $< $@

$(objdir)/%.chr: tilesets/%.png
	python tools/pilbmp2nes.py $< $@

