MMC1A test
==========

This program tests an obscure behavior of MMC1A, an early version of
a support chip found in NES games.  It shows that setting bit 4 of
the program bank register ($E000) causes bit 3 to bypass the fixed
bank logic and go straight to the ROM.

Background
----------
MMC1 is a memory mapping integrated circuit found in about 30 percent
of licensed games for the Nintendo Entertainment System.  Its four
5-bit registers control what parts of memory it makes available to
the console.  The high bit of the program bank register ($E000 D4) is
known to control different behavior on the MMC1A revision compared to
later revisions.  On the later MMC1B, setting D4 to 1 prevents the
MMC1 from asserting the battery RAM's chip select, whereas on MMC1A,
D4 has no effect on the battery RAM's chip select.

Only two games, both exclusive to Japan (where the NES is called
Family Computer or Famicom for short), rely on MMC1A's behavior of
not disabling battery RAM.  One is SOFEL's _The Money Game_, whose
sequel was released elsewhere as _Wall Street Kid_.  The other is
Shinsei's _Tatakae!! Ramenman: The Explosive Choujin 102 Gei_, an
adaptation of a manga by Yudetamago.  The preservation community
has assigned [iNES mapper 155] to these games.

At the start of 2022, it was pointed out that these games
deliberately set D4 using the instruction `ORA #$10`.  This hinted
that D4 may actually exist in the program bank register and have some
other effect on the mapper's behavior.  So PinoBatch (the author of
this test) wrote an NES program to step through all possible bank
number settings, displaying what banks they select, and asked KMLbay
to run it on a donor cartridge with an MMC1A.  It turned out that
when D4 is on, D3 (the second highest bit) controls the PRG ROM A17
pin regardless of the CPU address.  It could have been a debugging
feature or planning ahead for large PRG RAM.

[iNES mapper 155]: https://wiki.nesdev.org/w/index.php?title=INES_Mapper_155

What MMC1A does
---------------
Difference in $E000 behavior between MMC1A and MMC1B can be observed
only with some values of $8000.

Control ($8000): `CPPMM`

- `P=0, P=1`: 32K mode (no fixed bank; CPU A14 controls PRG ROM A14);  
  `P=2`: 16K mode, $8000-$BFFF fixed (CPU A14 low selects bank 0);  
  `P=3`: 16K mode, $C000-$FFFF fixed (CPU A14 high selects bank 15)
- `C` and `M`: Related to the PPU bus, unrelated to this test

PRG bank select ($E000): `RPPPP`

- `P`: Select PRG ROM A17-A14 when not in the fixed bank
- `R`: Effect depends on MMC1 revision.  MMC1B disables PRG RAM while
  `R=1`.  On MMC1A, `R=0` means the fixed bank affects PRG A17-A14
  output, and when `R=1`, the fixed bank affects PRG A16-A14 output,
  and D3 alone controls PRG A17.

The test method
---------------
Two ROMs are included: `mmc1a.nes` with 256 KiB PRG ROM and 8 KiB
CHR ROM, and `mmc1a-sn.nes` with the same PRG ROM and CHR RAM.
All sixteen banks of PRG ROM, each 16 KiB, contain the same code and
data except for one byte, which stores the number of that bank.
The code does the following:

1. Reset MMC1
2. Copy font to CHR RAM in case the included CHR ROM is not installed
   (so that the same program works on CHR ROM and CHR RAM boards)
3. Clear the screen and write the title heading at the top
4. Write all combinations of 0, 4, 8, C in $8000 and 00-1F in $E000,
   read which pair of PRG ROM banks is switched in for each case,
   and print the results

Results from an MMC1A:

    80:08
     00  00 01 02 03 04 05 06 07
     08  08 09 0a 0b 0c 0d 0e 0f
     10  00 01 02 03 04 05 06 07
     18  88 89 8a 8b 8c 8d 8e 8f  [differs from MMC1B]
    80:0C
     00  0f 1f 2f 3f 4f 5f 6f 7f
     08  8f 9f af bf cf df ef ff
     10  07 17 27 37 47 57 67 77  [differs from MMC1B]
     18  8f 9f af bf cf df ef ff

MMC1B lacks this bypassing behavior.  When the test is run on
MMC1B, the 10 and 18 rows should match the corresponding 00 and
08 rows.  The behavior of AX5904, a third-party clone of MMC1,
has yet to be tested.

How to build it
---------------
Install Python 3, Pillow, cc65, and Make, then at a terminal type

    make all

Legal
---------------
Copyright 2022 Damian Yerrick  
License: zlib
