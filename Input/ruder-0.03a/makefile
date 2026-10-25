#!/usr/bin/make -f
#
# Makefile for Zapper tech demo for NES
# Copyright 2011-2019 Damian Yerrick
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty
# provided the copyright notice and this notice are preserved.
# This file is offered as-is, without any warranty.
#

# These are used in the title of the NES program and the zip file.
title = ruder
version = 0.03a

# Space-separated list of assembly language files that make up the
# PRG ROM.  If it gets too long for one line, you can add a backslash
# (the \ character) at the end of the line and continue on the next.
objlist := zapkernels main ppuclear title menu testpatterns \
           drawball kinematics tennis tennisgfx axe \
           pentlysound pentlymusic musicseq ntscPeriods \
           bcd math pads unpkb

objlistnsf := nsfshell pentlysound pentlymusic musicseq ntscPeriods

AS65 = ca65
LD65 = ld65
CFLAGS65 := -DZAPPER_TO_A_BUTTON=1 -DPENTLY_USE_PAL_ADJUST=0 \
  -DPENTLY_USE_ROW_CALLBACK=1
objdir = obj/nes
srcdir = src
imgdir = tilesets

#EMU := "/C/Program Files/Nintendulator/Nintendulator.exe"
#EMU := fceux --input1 GamePad.0 --input2 Zapper.0
#EMU := mednafen -nes.pal 0 -nes.input.port1 gamepad -nes.input.port2 zapper
EMU := Mesen.exe


# other options for EMU are start (Windows) or gnome-open (GNOME)

# Occasionally, you need to make "build tools", or programs that run
# on a PC that convert, compress, or otherwise translate PC data
# files into the format that the NES program expects.  Some people
# write their build tools in C or C++; others prefer to write them in
# Perl, PHP, or Python.  This program doesn't use any C build tools,
# but if yours does, it might include definitions of variables that
# Make uses to call a C compiler.
CC := gcc
CFLAGS := -std=gnu99 -Wall -DNDEBUG -O

# Windows needs .exe suffixed to the names of executables; UNIX does
# not.  COMSPEC will be set to the name of the shell on Windows and
# not defined on UNIX.
ifdef COMSPEC
DOTEXE:=.exe
PY:=py
else
DOTEXE:=
PY:=python3
endif

.PHONY: run debug all dist zip clean

run: $(title).nes
	$(EMU) $<

# packaging

# Actually this depends on every file in zip.in, but currently we use
# changes to the ROM, makefile, and README as a heuristic for when
# something was changed.  Limitation: it won't see changes to docs or
# tools unless there is a corresponding makefile change.
all: $(title).nes $(title).nsf
dist: zip
zip: $(title)-$(version).zip
$(title)-$(version).zip: zip.in $(title).nes $(title).nsf \
  README.md CHANGES.txt $(objdir)/index.txt
	zip -9 -u $@ -@ < $<

# Build zip.in from the list of files in the Git tree
zip.in:
	git ls-files | grep -e "^[^.]" > $@
	echo zip.in >> $@
	echo $(title).nes >> $@
	echo $(title).nsf >> $@

$(objdir)/index.txt: makefile
	echo Files produced by build tools go here, but caulk goes where? > $@

# Rules for PRG ROM

objlistntsc := $(foreach o,$(objlist),$(objdir)/$(o).o)
objlistnsf := $(foreach o,$(objlistnsf),$(objdir)/$(o).o)

map.txt $(title).nes: nrom128.cfg $(objlistntsc)
	$(LD65) -C $^ -m map.txt -o $(title).nes

nsfmap.txt $(title).nsf: nsf.cfg $(objlistnsf)
	$(LD65) -C $^ -m nsfmap.txt -o $(title).nsf

$(objdir)/%.o: $(srcdir)/%.s $(srcdir)/nes.inc $(srcdir)/global.inc
	$(AS65) $(CFLAGS65) $< -o $@

$(objdir)/%.o: $(objdir)/%.s
	$(AS65) $(CFLAGS65) $< -o $@

# Files that depend on .incbin'd files or on other headers
$(objdir)/main.o: $(objdir)/bggfx.chr $(objdir)/spritegfx.chr

$(objdir)/testpatterns.o: $(srcdir)/ballbg.pkb $(srcdir)/fullbright.pkb \
    $(srcdir)/hlines.pkb $(srcdir)/menu.pkb $(srcdir)/vlines.pkb \
    $(srcdir)/pulltrigger.pkb

$(objdir)/tennis.o: $(srcdir)/tennis.inc
$(objdir)/tennisgfx.o: $(srcdir)/tennis.inc $(srcdir)/tennis_title.pkb 
$(objdir)/math.o: $(srcdir)/tennis.inc
$(objdir)/kinematics.o: $(srcdir)/tennis.inc
$(objdir)/title.o: $(srcdir)/title.pkb

# Generate lookup tables at build time
$(objdir)/ntscPeriods.s: tools/mktables.py
	$(PY) $< period $@

# Rules for CHR ROM

$(objdir)/%.chr: $(imgdir)/%.png
	$(PY) tools/pilbmp2nes.py $< $@

$(objdir)/%16.chr: $(imgdir)/%.png
	$(PY) tools/pilbmp2nes.py -H 16 $< $@
