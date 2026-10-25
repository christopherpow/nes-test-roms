#!/usr/bin/make -f
#
# Makefile for NES game
# Copyright 2011 Damian Yerrick
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty
# provided the copyright notice and this notice are preserved.
# This file is offered as-is, without any warranty.
#

# These are used in the title of the NES program and the zip file.
title = test28
version = 0.04

# Space-separated list of .s files that make up PRG ROM
objlist = main testseq pads ppuclear interbank wrongbanks


AS65 = ca65
LD65 = ld65
CFLAGS65 = -DUSE_DAS=1
objdir = obj/nes
srcdir = src
imgdir = tilesets

#EMU := "/C/Program Files/Nintendulator/Nintendulator.exe"
EMU := fceux
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
# not defined on UNIX.
ifdef COMSPEC
DOTEXE:=.exe
PY:=py -3
else
DOTEXE:=
PY:=python3
endif

.PHONY: run dist zip

run: $(title).nes
	$(EMU) $<

# Rule to create or update the distribution zipfile by adding all
# files listed in zip.in.  Actually the zipfile depends on every
# single file in zip.in, but currently we use changes to the compiled
# program, makefile, and README as a heuristic for when something was
# changed.  It won't see changes to docs or tools, but usually when
# docs changes, README also changes, and when tools changes, the
# makefile changes.
dist: zip
zip: $(title)-$(version).zip
$(title)-$(version).zip: zip.in $(title).nes $(title)-8Mbit.nes README.md CHANGES.txt $(objdir)/index.txt
	zip -9 -u $@ -@ < $<

%.nes: tools/bankdupe.py %.prg
	$^ $@ 32
%-8Mbit.nes: tools/bankdupe.py %.prg
	$^ $@ 64

$(objdir)/index.txt: makefile
	echo Files produced by build tools go here, but caulk goes where? > $@

# Rules for PRG ROM

objlistntsc = $(foreach o,$(objlist),$(objdir)/$(o).o)

map.txt $(title).prg: test28master.x $(objlistntsc)
	$(LD65) -o $(title).prg -C $^ -m map.txt

$(objdir)/%.o: $(srcdir)/%.s $(srcdir)/nes.inc $(srcdir)/global.inc
	$(AS65) $(CFLAGS65) $< -o $@

$(objdir)/%.o: $(objdir)/%.s
	$(AS65) $(CFLAGS65) $< -o $@

# Files that depend on .incbin'd files
$(objdir)/main.o: $(objdir)/font16.chr
$(objdir)/wrongbanks.o: $(objdir)/font8x5.bin

# This is an example of how to call a lookup table generator at
# build time.  mktables.py itself is not included because the demo
# has no music engine, but it's available online at
# http://wiki.nesdev.com/w/index.php/APU_period_table
$(objdir)/ntscPeriods.s: tools/mktables.py
	$< period $@

# Rules for CHR ROM

$(title).chr: $(objdir)/bggfx.chr $(objdir)/spritegfx.chr
	cat $^ > $@

$(objdir)/font8x5.bin: $(imgdir)/font8x5.png
	$(PY) tools/cvt8x5.py $< $@

$(objdir)/%.chr: $(imgdir)/%.png
	$(PY) tools/pilbmp2nes.py $< $@

$(objdir)/%16.chr: $(imgdir)/%.png
	$(PY) tools/pilbmp2nes.py -H 16 $< $@


