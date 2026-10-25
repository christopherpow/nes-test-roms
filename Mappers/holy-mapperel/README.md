Holy Mapperel
=============
An NES cartridge PCB manufacturing test by Damian Yerrick

This demo detects what mapper it's running on through mirroring,
tests how big of a ROM it was written on, verifies that the mapper
is mostly working, and displays the result.

The name
--------
Paul Molloy of infiniteneslives.com is selling INL-ROM circuit
boards for making NES game cartridges.  These boards are
configured at manufacturing time to support the mapper circuits
required by several games.  Among them are *Holy Diver* by IREM and
*Batman: Return of the Joker* by Sunsoft.  Putting those titles
together would produce something Burt Ward as Dick "Robin" Grayson
might say: "Holy diver, Batman!"

But the Warners probably wouldn't like that.  A label in Warner Music
Group publishes the Dio album after which *Holy Diver* was named, and
DC Comics (a unit of Warner Bros.) publishes comics starring Batman.
In 2010, DC forced NIS America to remove a Batman allusion from the
title of the PSP game now known as *What Did I Do To Deserve This,
My Lord?*.  So instead, I'm avoiding references to either Warner
company's works in the title.

Building
--------
First install Python 3, Pillow, cc65, GNU Make, and GNU Coreutils.
The README file for [nrom-template] describes how to set it up.

The makefile performs four steps:

1. Convert the system font from PNG format to an NES-friendly format
2. Assemble all source code files
3. Link source code files into mapperel-master.nes
4. Generate ROMs with several sizes and mappers

Because several mappers don't guarantee what bank will be visible
at power on, ROM generation repeats several parts of the program:

* Every 4 KiB has the Morse code crash handler.
* Every 16 KiB has a trampoline that attempts to change a discrete
  logic mapper or MMC1 to the last bank.
* Every 256 KiB has the entire test, in order to make testing of
  SUROM easier.

Once you have test ROMs of the appropriate size, you can use any
PRG/CHR splitting program.  The contents of all PRG ROMs of a
given size are identical no matter the mapper, as are all CHR
ROMs.  They differ only by header.  So go ahead and burn an EPROM
for each size and plug it into your socketed boards.  However, you
can't chop off the end of a larger ROM and use that on a smaller
ROM, as it will be missing the special tag that identifies it as
the last bank of the ROM.  This is intentional in order to prevent
builds with disconnected upper address lines from silently working.

[nrom-template]: https://github.com/pinobatch/nrom-template/

Supported boards
----------------
* 001: SxROM (Bionic Commando, Final Fantasy, Journey to Silius)
* 002: UNROM, UOROM (7432) (Battle Kid, Contra, DuckTales)
* 004: TxROM (Super Mario Bros. 2-3, Mega Man 3-6)
* 007: AxROM (Battletoads, Jeopardy!)
* 009: PNROM (Punch-Out!!)
* 010: FJROM, FKROM (Famicom Wars, Fire Emblem)
* 011: Color Dreams (Crystal Mines, Spiritual Warfare)
* 028: INL-ROM (A53) (Action 53 volumes 1 and 2)
* 034: BNROM (Deadly Towers, Haunted: Halloween '85)
* 066: NROM, CNROM, GNROM (Balloon Fight, Pipe Dream, Gumshoe)
* 069: JLROM, JSROM (Batman: Return of the Joker, Gimmick!, Hebereke)
* 078.3: IF-12 (Holy Diver)
* 118 TxSROM (NES Play Action Football, Wanderers from Ys)
* 180: UNROM (7408) (Crazy Climber)

The tests
---------
The tests look at bank tags, which are increasing numbers spaced at
regular intervals in memory that let the program know what bank has
been switched into a particular address window.  Bank tags are placed
at offset 4088 ($FF8) of every 4 KiB bank of PRG ROM and at offset
508 ($1FC) of every 2 KiB bank of CHR ROM or RAM.

First it copies test code into RAM that detects the mapper by how
it responds to writes to the supported boards' nametable mirroring
ports.  Then, because a few mappers react the same way, such as all
boards with fixed mirroring, it narrows the field through very basic
bank switching tests.  This needs to be done in RAM because 32K bank
switching may cause the startup ROM bank to be switched out.

After the mapper number is determined, it copies a driver for that
mapper into RAM.  The driver is responsible for changing 8K CHR banks
and WRAM banks and doing detailed tests.

### CHR test

The CHR test starts by asking the driver to switch the first 8K of
CHR memory into PPU $0000-$1FFF.  If data written to $0000 in CHR
space does not change, it's CHR ROM.  Otherwise, it's CHR RAM, and
the test writes bank tags in CHR RAM banks 31 down to 0.  It reads
bank tags from the last CHR bank to verify that they're internally
consistent.  If so, it knows the size of CHR memory.

If CHR ROM is present, the test coompares the font in $0000-$03FF to
a copy of the font in PRG ROM to ensure that CHR ROM can be read.
Then it verifies all bank tags and saves the result for later.

If it's CHR RAM, it writes a pseudorandom pattern to all of CHR RAM,
reads it back, and saves whether it matched for later.  Then it
repeats the test seven more times with the pattern shifted by a byte
each time.  While this is going on, it buzzes the speaker to let the
user know it hasn't frozen.  Finally, it puts the bank tags back and
loads the small font into CHR RAM.

### WRAM test 

Work RAM (WRAM), or PRG RAM, is RAM on the cartridge at CPU
$6000-$7FFF.  First it looks for the string `SAVEDATA` at $6100 and
sets a flag if it was found.  Then the same test is done as for CHR
RAM:  determine how much, then test each byte.  Finally, `SAVEDATA`
is written back.  This way, the user can test for battery backup by
running the test, powering off, and running the test again.
Pressing the Reset button will cause an incorrect result.

### Detailed test

Some mappers are more flexible than others in how they map PRG and
CHR banks into windows within the CPU and PPU address space.  This
test steps through all the PRG and CHR bank numbers in each window
with various combinations of banking modes.  It also checks whether
WRAM can be disabled for power-off protection (on mappers that claim
to support this) and whether the IRQ works roughly as expected.
This is no substitute for an exhaustive mapper-specific test, but
it should help determine whether the chips are soldered properly.

Morse codes
-----------
Sometimes a test fails hard enough that the framework isn't even sure
that it can display the results on screen.  When a test fails this
hard, your NES will beep Morse code at you.  Find a ham and fix your
soldering.

* `WB` (`.__ _...`):  
  Wrong bank at startup.  INL's versions of the ASIC mappers
  guarantee that the LAST 4 KiB of the cart is swapped into
  $F000-$FFFF at power on.  Discrete mappers should be OK as well,
  as there's a stub every 16K.
* `MIR` (`__ .. ._.`):  
  The nametable mirroring for this mapper doesn't match any of the
  supported mappers.  Check PA13-PA10, /PA13, CIRAM A10, and CIRAM
  enable, and don't try running the 78.3 test on an emulator that
  does not support NES 2.0 format.
* `SU` (`... .._`):  
  Attempt to switch to second half of SUROM (4 Mbit MMC1) failed.
* `LB` (`._.. _...`):  
  Mapper detection did not leave the last 16K of PRG ROM swapped in.
* `RB` (`._. _...`):  
  Attempt to return to the last bank after a test failed.
* `CBT` (`_._. _... _`):  
  CHR bank tags are not internally consistent.
* `FON` (`.._. ___ _.`):  
  Font in CHR memory does not match that in PRG ROM.  The CHR ROM or
  CHR RAM is bad.
* `DRV` (`_.. ._. ..._`):  
  The mapper was detected, but no PRG and CHR test driver for the
  mapper exists.
* `SMS` (`... __ ...`):  
  Your mobile phone has received a text message.

The mapper 34 test may freeze in emulators that do not support the
NES 2.0 format.  They make both the BNROM registers and the NINA-001
registers available at the same time, when the current best practice
is to switch between the two based on CHR size.

Displayed result
----------------
After the buzzing ends, the following variables are valid:

* `cur_mapper`: iNES mapper number of the detected mapper
* `last_prg_bank`: Size of PRG ROM in 4096 byte units, minus one
* `is_chrrom`: Zero if CHR RAM is present; nonzero if CHR ROM is
  present
* `last_chr_bank`: Size of CHR ROM or CHR RAM in 8192 byte units,
  minus one
* `chr_test_result`: Zero if bank tags were read correctly from CHR
  ROM or if all bytes of CHR RAM correctly hold values.
* `has_wram`: Nonzero if $6000-$7FFF is RAM.
* `has_savedata`: Nonzero if `SAVEDATA` was in $6100 of the first
  WRAM bank at startup
* `last_wram_bank`: WRAM size in 8192 byte units, minus one;
  unspecified if `has_wram` is zero
* `wram_test_result`: Zero if all bytes of WRAM correctly hold
  values; unspecified if `has_wram` is zero
* `driver_prg_result`: Detailed WRAM and PRG ROM test results
* `driver_chr_result` Detailed IRQ and CHR test results

All this information is displayed on the screen.  The detailed code
is 4 digits: WRAM, PRG ROM, IRQ, and CHR ROM/RAM.  Zero is normal;
anything else reflects something unexpected.

* Mapper 001 `1xxx`: $E000 bit 4 does not disable WRAM
* Mapper 001 `4xxx`: $A000 bit 4 does not disable WRAM (SNROM), or
  $A000 bit 4 disables WRAM (all but SNROM)
* Mapper 004 `2xxx`: Read-only mode not present

In iNES format environments that don't support NES 2.0, such as FCEUX
and PowerPak, the MMC3 test will return a warning about lack of write
protection on WRAM.  These environments don't implement this feature
because not implementing it gets the two *StarTropics* games, which
use MMC6, to run with the MMC3 driver.

Some INL-ROM products implement only a subset of the entire mapper as
a cost-saving measure.  For example, MMC3 may lack WRAM protection or
may fix C and P values.  Detailed codes will reflect unimplemented
features.  If a subset changes mirroring behavior, the mapper may not
be recognized in the first place.

For the convenience of users testing large numbers of boards without
looking at the screen, the result is also beeped through the speaker
with one note per nibble so that an attentive ear can pick up any
deviation from the intended melody.

    0: Bb       4: F        8: Mid C    C: G
    1: Low C    5: G        9: D        D: A
    2: D        6: A        A: E        E: B
    3: Eb       7: Bb       B: F        F: High C

There are three groups of nibbles:

1. Tweet, mapper number (2)
2. Tweet, PRG ROM size in 32768 byte units (2), PRG RAM size in
   8192 byte units (2), buzz if PRG RAM defective, ding if battery,
   PRG RAM detailed result (1), PRG ROM detailed result (1)
3. Tweet, CHR size in 8192 byte units minus 1 (2), CHR RAM or
   ROM (1), buzz if CHR RAM or ROM defective, IRQ detailed
   result (1), CHR detailed result (1)

Limits
------
Not all mappers are supported.

NROM and CNROM are detected as mapper 66, but the board name should
be correct if their memory size matches a known configuration.

The IRQ test measures only gross behavior of the interval timer of
the MMC3 or FME-7.  It is intended to measure continuity of the IRQ
pin during board assembly, not as a substitute for detailed tests of
exact timing of a CPLD reimplementation of a particular mapper.

The "Flash ID" results mean nothing.  They are reserved for
future expansion.  Pull requests are appreciated.

Legal
-----
Copyright 2013-2017 Damian Yerrick

Available under zlib License.

Not sponsored or endorsed by DC Comics, IREM, Nintendo, or Sunsoft.
