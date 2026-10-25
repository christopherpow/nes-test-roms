Action 53 mapper comprehensive test
===================================

The Action 53 mapper is designed for making multicarts of NROM,
CNROM, UNROM, BNROM, and AOROM games.

Reset points other than the last bank
-------------------------------------
The first test is that the ROM boots in the last bank, and the last
test is that reset does not change the bank number.  So all reset
entry points except the last bank do the following:

1. Init the NES.
2. Load 8x8 mono font and palette.
3. Clear nametable.
4. If the warm boot signature is "in progress", write "not so fast"
   and freeze.
5. If the warm boot signature is not "complete", write "fail: not
   last bank" and freeze.
6. If the bank at $C000 is not 0, write "fail: reset changes bank"
   and freeze.
7. Write "pass" and freeze.

== Reset in the last bank ==

The boot process first does some basic sanity checks on the
environment, enough to ensure that the interactive PRG ROM test
returns meaningful results.

1. Init the NES.  While waiting for the PPU to spin up, load the RAM
   module into $0400.
2. Load the palette.
3. Check for bus conflicts.
4. Detect the size of CHR RAM.  If the test fails, write an error
   message and freeze.
5. Write mirroring sentinels to VRAM: $00 to $2000 and $01 to $2C00.
   These tile numbers both refer to blank tiles.  If the write to
   $2C00 affected $2000, then we're stuck in 1-screen mirroring, in
   which case write msg_mirror_stuck and freeze.
6. If the warm boot signature is "in progress", write msg_not_so_fast
   and freeze.
7. If the warm boot signature is "complete", write msg_warm_boot_last
   and freeze.
8. Set the warm boot signature.

Once all these tests pass, load the font, clear both nametables and
the part of the pattern table not covered by the font, and write the
mirroring sentinels again.

After this it proceeds to an interactive test phase.  Use the Control
Pad to set the mode ($80) and outer bank ($81), and the program reads
back the PRG bank and mirroring values for each value of the inner
bank ($01).  Lines 1 and 2 show what banks were switched into $8000,
and lines 3 and 4 show $C000.  The last line has 3 bytes: which
window ($80 or $C0) doesn't match specification (or $00 if match),
which outer bank ($00 to $0F) doesn't match specification if any,
and a byte representing the current nametable mirroring mode.

Press Start to run automated tests.  When a test fails, it writes
up to eight bytes to failure_details and returns a failure code.
An error message and descriptions of each failure_details byte are
associated with each failure code.

If the automated tests all return 0 for success, it enters the final
test.  Write msg_one_last_thing, load the first bank into $C000
(mode $08, outer $00, inner $00), set the signature to "complete",
and freeze, waiting for a reset.

When writing any error message, load the font again first in case
the CHR bank changed.

Detect CHR RAM size
-------------------
1. Using set_chr_bank_x, load $10-$13 into $1010 of all four CHR RAM
   banks.  Start with $13 in bank 3 then go down to $10 in bank 0, so
   that the $10 is most recent.  If setting the CHR bank swaps the
   last PRG bank out, fail.
2. Using set_chr_bank_x, read these values back out and calculate the
   CHR RAM size based on the following rules:

    * If $10,$10,$10,$10: CHR RAM size is 8 KiB.
    * If $10,$11,$10,$11: CHR RAM size is 16 KiB.
    * If $10,$11,$12,$13: CHR RAM size is 32 KiB.
    * Otherwise, fail.

Automated PRG bank test
-----------------------

Press Start to run automated tests.  The first iterates through
all 131072 combinations of mode, outer bank, and inner bank.  It
calls a function in the RAM module that writes the registers, reads
back what PRG banks are loaded into $8000 and $C000, and reads back
what CHR banks are loaded into $2000, $2400, $2800, and $2C00.  Then
it calculates, based on the mapper's specification, what is expected
for the same combinations.  This takes one frame for each combination
of mode and outer bank, to test all 32 current bank values.

A Famicom or NTSC NES finishes this phase in up to 17 seconds on a
512 KiB cart (outer banks $00-$0F) or 68 seconds on a maximum 2 MiB
cart (outer banks $00-$3F).  PAL takes 20% longer.

== Automated CHR bank test ==

A shorter automated test covers everything to do with register $00.
It clocks through the last bank, setting inner bank, then outer bank,
then mirroring.  These tests are performed using safe_set_a_to_x, in
mode $3C-$3F and the last outer bank.

1. Reload the CHR bank sentinels, as in detecting CHR RAM size.
2. Make sure mirroring mode $00 and $01 don't have individual D4s,
   that reg00 and reg01 don't have individual D4s, and mode overrides
   D4 if mode is written last.  Set mode=$3C, reg00=$10, reg01=$10,
   mode=$3C, then fail if mirroring is not 1-screen page 0.
   Repeat with $3D, $00, $00, $3D, and page 1.
3. Make sure reg00 D4 affects mirroring in 1-screen but not H/V.
   For each mode $3C through $3F, for each reg00 in $00 and $10,
   write mode and reg00 and read back mirroring.
4. Make sure CHR bank does not affect inner bank or mirroring.
   Write mode $3F, then for each reg00 in $00-$0F, read back the
   inner bank and mirroring.

The final automated test is 

== The RAM module ==

The RAM module is loaded into $0400-$07FF and actually performs all
actions that may result in PRG ROM bank switching.

try_returning ensures that the last bank is loaded into $C000-$FFFF
so that tests can continue.

1. Write $FF to registers $80 and $81.  This ensures horizontal
   mirroring.
2. If the bank at $C000 is the last bank, return A = 1.
3. Clear the screen and turn the background red in case the CHR bank
   changed to make the font disappear.
4. Write failed_to_return_msg and freeze.  Because the code for
   banks other than the last bank is loaded, we can use that to
   display the message.

verify_bus_conflict switches to 32K (oversize BNROM) mode and runs
four tests:

1. Make sure $FFF6 contains 0, or fail with ATFAIL_KNOWN_00.
2. Write $FF to $FFF6 and make sure the last bank was selected, or
   fail with ATFAIL_BUS_CONFLICT_AND.
3. Make sure $FFF6 contains $FF, or fail with ATFAIL_KNOWN_FF.
4. Write $00 to $FFF7 and make sure the first bank was selected, or
   fail with ATFAIL_BUS_CONFLICT_OR.
5. Tail call try_returning.

read_mode_x_bank_y is the heart of automated test 1, and it does the
following:

1. Write the chosen mode to $80 and the chosen outer bank to $81.
2. For inner bank from $00 to $1F:

    1. Write the inner bank to $01.
    2. Read the bank in $8000 and write it to out80[X].
    3. Read the bank in $C000 and write it to outC0[X].
    4. Read the nametable in $2000 and write it to out20[X].
    5. Read the nametable in $2400 and write it to out24[X].
    6. Read the nametable in $2800 and write it to out28[X].
    7. Read the nametable in $2C00 and write it to out2C[X].

3. Tail call try_returning.

inner_outer_mode is similar to read_mode_x_bank_y but does not read nametables and writes the registers in a different order, to make sure that the bank behavior does not depend on the order in which bank numbers and mode are written:

1. Write the chosen inner bank to $01.
2. Set Y to 0.
3. For outer bank from $1F to $20:
     Write the outer bank to $81.
     For mode from $03 to $3F by 4:
       Write the mode to $80.
       Read the bank in $8000 and write it to out80[Y].
       Read the bank in $C000 and write it to outC0[Y].
       Add 1 to Y.
4. Tail call try_returning.

safe_set_a_to_x writes a mapper register and tells whether this affected the CHR RAM bank.

1. Set mapper register A to value X.
2. If the last bank is still visible in $C000-$FFFF, return A = 0.
3. Tail call try_returning.  If this happens, other writes took effect.

set_chr_bank_x is a convenience alias for safe_set_a_to_x that always sets register 0.

freeze_in_bank_0 sets the mode to $0B (32K UNROM #180, horizontal mirroring) and the outer and inner banks to 0, so that the first bank is mapped into $8000 and $C000.

Emulating the mapper
--------------------
To determine what bank ought to be mapped into a given address with a
given set of $80 (mode), $81 (outer bank), $01 (current bank) values:

1. Get the bank size and bank mode from reg $80 and the current bank
   from $81.
2. If the bank mode is 2 or 3, and bit 0 of the bank mode matches bit
   14 of the address, set the bank size and bank mode to 0.
3. If the bank mode is 0 or 1, shift the current bank to the left by
   1 and set bit 0 to bit 14 of the address.
4. Replace the bits that are 0 in the size_masks for the bank size
   with bits from the outer bank shifted left by 1. The size_masks
   are $01, $03, $07, $0F.

The reference implementations are calc_prg_bank in src/testseq.s and
docs/calc_prg_bank.py.

Legal
-----
The program and manual are distributed under these terms:

Copyright 2012, 2018 Damian Yerrick
Copying and distribution of this file, with or without
modification, are permitted in any medium without royalty provided
the copyright notice and this notice are preserved in all source
code copies.  This file is offered as-is, without any warranty.
