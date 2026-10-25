Big RAM test
============
As of March 2015, NES emulators disagree on how much RAM at $6000 a
game using Sunsoft's FME-7 mapper IC can see.

FME-7 register 8 sets both the PRG ROM bank at $6000 and whether
$6000 is ROM or RAM.  It's been speculated that even if this register
is set to RAM, the FME-7 will still drive the PRG ROM bank output
lines based on the selected bank number.  If this is true, the 6264
(8Kx8 SRAM) on the NES-BTR or JSROM PCB can be replaced with a 62256
(32Kx8 SRAM), and an NES program can use all four 8K banks.  This
would provide ample space for a Z-machine or BASIC interpreter.

Run this test on a _Batman: Return of the Joker_ or other FME-7
board with PRG ROM, CHR ROM, and WRAM.  I have no experience
modding carts myself, but these signals should be relevant for
rewiring a cart to use a 62256:

* FME-7 pin 36: PRG A13 output
* 62256 pin 26: A13 input (+CE on 6264; you'll need to cut this) 
* FME-7 pin 34: PRG A14 output
* 62256 pin 1: A14 input (not connected on 6264)

Legal
-----
The test is distributed under the following terms:

    Copyright 2015 Damian Yerrick
    Copying and distribution of this file, with or without
    modification, are permitted in any medium without royalty
    provided the copyright notice and this notice are preserved.
    This file is offered as-is, without any warranty.
