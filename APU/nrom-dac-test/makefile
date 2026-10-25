#!/usr/bin/make -f
#
# Makefile for NES game
# Copyright 2011-2014 Damian Yerrick
# Modification by Persune 2022
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty
# provided the copyright notice and this notice are preserved.
# This file is offered as-is, without any warranty.
#

# These are used in the title of the NES program and the zip file.
title = $(outdir)/nrom-dac-test
version = 0.0.2

# Space-separated list of assembly language files that make up the
# PRG ROM.  If it gets too long for one line, you can add a backslash
# (the \ character) at the end of the line and continue on the next.
objlist = header init main nmi irq graphics

AS65 = ca65
LD65 = ld65
CFLAGS65 = -g
LDFLAGS65 = -v
MAPPERCFG = nrom_128.cfg
objdir = obj
srcdir = src
imgdir = gfx
outdir = output

EMU := mesen
DEBUGEMU := mesen
# other options for EMU are start (Windows) or gnome-open (GNOME)

# Occasionally, you need to make "build tools", or programs that run
# on a PC that convert, compress, or otherwise translate PC data
# files into the format that the NES program expects.  Some people
# write their build tools in C or C++; others prefer to write them in
# Perl, PHP, or Python.  This program doesn't use any C build tools,
# but if yours does, it might include definitions of variables that
# Make uses to call a C compiler.
CC = gcc
CFLAGS = -std=gnu99 -Wall -DNDEBUG -O

# Windows needs .exe suffixed to the names of executables; UNIX does
# not.  COMSPEC will be set to the name of the shell on Windows and
# not defined on UNIX.  Also the Windows Python installer puts
# py.exe in the path, but not python3.exe, which confuses MSYS Make.
ifeq ($(OS), Windows_NT)
DOTEXE:=.exe
PY:=py
else
DOTEXE:=
PY:=
endif

.PHONY: run debug all dist zip clean

run: $(title).nes
	$(EMU) $<
debug: $(title).nes
	$(DEBUGEMU) $<

all: $(title).nes

# Rule to create or update the distribution zipfile by adding all
# files listed in zip.in.  Actually the zipfile depends on every
# single file in zip.in, but currently we use changes to the compiled
# program, makefile, and README as a heuristic for when something was
# changed.  It won't see changes to docs or tools, but usually when
# docs changes, README also changes, and when tools changes, the
# makefile changes.
dist: zip
zip: $(title)-$(version).zip
$(title)-$(version).zip: zip.in $(title).nes README.md CHANGES.txt $(objdir)/index.txt
	zip -9 -u $@ -@ < $<

# Build zip.in from the list of files in the Git tree
zip.in:
	git ls-files | grep -e "^[^.]" > $@
	echo $(title).nes >> $@
	echo zip.in >> $@

$(objdir)/index.txt: makefile
	echo Files produced by build tools go here > $@

# make sure that files actually exist before deleting them
clean:
	if [ "$(wildcard $(objdir)/*)" ]; then rm -v $(objdir)/*; fi
	if [ "$(wildcard $(outdir)/*)" ]; then rm -v $(outdir)/*; fi

# Rules for PRG ROM

objlisto = $(foreach o,$(objlist),$(objdir)/$(o).o)

$(outdir)/map.txt $(title).nes: $(MAPPERCFG) $(objlisto)
	$(LD65) $(LDFLAGS65) --dbgfile $(title).dbg -o $(title).nes -m $(outdir)/map.txt -C $^

$(objdir)/%.o: $(srcdir)/%.asm $(srcdir)/defines.inc $(srcdir)/global.inc
	$(AS65) $(CFLAGS65) $< -o $@

$(objdir)/%.o: $(objdir)/%.asm
	$(AS65) $(CFLAGS65) $< -o $@

# Files that depend on .incbin'd files
$(objdir)/header.o: $(objdir)/tiles.chr

# Rules for CHR ROM

$(title).chr: $(objdir)/tiles.chr
	cat $^ > $@

# figure out how to compile .png into .chr later
$(objdir)/tiles.chr: $(imgdir)/tiles.chr
	cp $< $@

# $(objdir)/%.chr: $(imgdir)/%.png
	# $(PY) tools/pilbmp2nes.py $< $@

# $(objdir)/%16.chr: $(imgdir)/%.png
	# $(PY) tools/pilbmp2nes.py -H 16 $< $@
