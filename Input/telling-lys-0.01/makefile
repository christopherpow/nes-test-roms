#!/usr/bin/make -f
#
# Makefile for NES game
# Copyright 2011-2014 Damian Yerrick
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty
# provided the copyright notice and this notice are preserved.
# This file is offered as-is, without any warranty.
#

# These are used in the title of the NES program and the zip file.
title = telling-lys
version = 0.01

# Space-separated list of assembly language files that make up the
# PRG ROM.  If it gets too long for one line, you can add a backslash
# (the \ character) at the end of the line and continue on the next.
objlist = \
  nrom telling init main ppuclear unpb53


AS65 = ca65
LD65 = ld65
CFLAGS65 = 
objdir = obj/nes
srcdir = src
imgdir = tilesets

#EMU := "/C/Program Files/Nintendulator/Nintendulator.exe"
EMU := fceux
DEBUGEMU := ~/.wine/drive_c/Program\ Files\ \(x86\)/FCEUX/fceux.exe
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
ifdef COMSPEC
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

# Packaging
dist: zip
zip: $(title)-$(version).zip

# The zipfile depends on every file in zip.in, but as a shortcut,
# mention only files on which the ROM doesn't itself depend.
$(title)-$(version).zip: zip.in $(title).nes README.md CHANGES.txt $(objdir)/index.txt
	$(PY) tools/zipup.py $< $(title)-$(version) -o $@
	-advzip -z3 $@

# Build zip.in from the list of files in the Git tree
# Enable if actually using Git
#zip.in:
#	git ls-files | grep -e "^[^.]" > $@
#	echo zip.in >> $@

$(objdir)/index.txt: makefile
	echo Files produced by build tools go here, but caulk goes where? > $@

clean:
	-rm $(objdir)/*.o $(objdir)/*.s $(objdir)/*.chr $(objdir)/*.nam
	-rm $(objdir)/*.pb53	

# Rules for PRG ROM

objlistntsc = $(foreach o,$(objlist),$(objdir)/$(o).o)

map.txt $(title).nes: nrom128.cfg $(objlistntsc)
	$(LD65) -o $(title).nes -m map.txt -C $^

$(objdir)/%.o: $(srcdir)/%.s $(srcdir)/nes.inc $(srcdir)/global.inc
	$(AS65) $(CFLAGS65) $< -o $@

$(objdir)/%.o: $(objdir)/%.s
	$(AS65) $(CFLAGS65) $< -o $@

# Files that depend on .incbin'd files
$(objdir)/main.o: \
  $(objdir)/text.u.chr.pb53 $(objdir)/text.nam.pb53 \
  $(objdir)/convo.u.chr.pb53 $(objdir)/convo.nam.pb53 \
  $(objdir)/pass.u.chr.pb53 $(objdir)/pass.nam.pb53

# This is an example of how to call a lookup table generator at
# build time.  mktables.py itself is not included because the demo
# has no music engine, but it's available online at
# http://wiki.nesdev.com/w/index.php/APU_period_table
$(objdir)/ntscPeriods.s: tools/mktables.py
	$< period $@

# Rules for CHR ROM

$(title).chr: $(objdir)/bggfx.chr $(objdir)/spritegfx.chr
	cat $^ > $@

$(objdir)/%.chr: $(imgdir)/%.png
	$(PY) tools/pilbmp2nes.py --planes "0;1" $< $@

$(objdir)/%16.chr: $(imgdir)/%.png
	$(PY) tools/pilbmp2nes.py -H 16 $< $@

$(objdir)/%.u.chr: $(objdir)/%.chr
	$(PY) tools/uniq.py $< $@ $(<:.chr=.nam)

# workaround for Make's multi-output parallelism failure
$(objdir)/%.nam: $(objdir)/%.u.chr
	touch $@

%.pb53: tools/pb53.py %
	$(PY) $^ --raw $@
