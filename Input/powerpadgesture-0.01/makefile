#!/usr/bin/make -f

title := powerpadgesture
version := 0.01
objlist := init nrom main bg player powerpad ppuclear math

srcdir := src
objdir := obj/nes
imgdir := tilesets

PY :=
SAVTOOL := $(PY) tools/savtool.py
PNGTOCHR := $(PY) tools/pilbmp2nes.py
EMU := Mesen

# convenience targets ###############################################

.PHONY: run all dist zip clean

run: $(title).nes
	$(EMU) $<
all: $(title).nes
dist: zip
zip: $(title)-$(version).zip
clean:
	-rm $(objdir)/*.chr $(objdir)/*.o $(objdir)/*.sav $(objdir)/*.nam
	-rm $(objdir)/*.sentinel $(title).dbg

# graphics ##########################################################

$(objdir)/%.chr: $(objdir)/%.png
	$(SAVTOOL) $< $@
$(objdir)/%.nam: $(objdir)/%.sav
	$(SAVTOOL) $< $@
$(objdir)/%.u.chr: $(objdir)/%.sav
	$(SAVTOOL) $< $@
$(objdir)/%.chr: $(imgdir)/%.png
	$(PNGTOCHR) --planes "0;1" $< $@
$(objdir)/bg.sav: $(imgdir)/bg.png
	$(SAVTOOL) --palette 090F1210090F16100909090909090909 $< $@

# assembly ##########################################################

$(objdir)/%.o: $(srcdir)/%.s $(srcdir)/nes.inc $(srcdir)/global.inc
	ca65 -g -o $@ $<

# extra includes ####################################################

$(objdir)/main.o: $(objdir)/bg.u.chr $(objdir)/spritegfx.chr
$(objdir)/bg.o: $(objdir)/bg.nam

# linking ###########################################################

$(objdir)/$(title).nes.sentinel: \
  nrom128.cfg $(foreach o,$(objlist),$(objdir)/$(o).o)
	ld65 -o $(title).nes -m map.txt --dbgfile $(title).dbg -C $^
map.txt $(title).dbg $(title).nes: $(objdir)/$(title).nes.sentinel
	test -f $@

# packaging #########################################################

$(objdir)/index.txt: makefile
	echo "files created during build go here" > $@

$(title)-$(version).zip: zip.in \
  $(objdir)/index.txt README.md CHANGES.txt $(title).nes
	zip -9u $@ -@ < $<
