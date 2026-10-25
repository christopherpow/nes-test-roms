

01-basics
---------
Tests basic operation of sprite overflow flag (bit 5 of $2002).

2) Should set flag when 9 sprites are on scanline
3) Reading $2002 shouldn't clear flag
4) Shouldn't clear flag at beginning of VBL
5) Should clear flag at end of VBL
6) Shouldn't set flag when $2001=$00
7) Should set normally when $2001=$08
8) Should set normally when $2001=$10


02-details
----------
Tests details of sprite overflow flag

2) Should set flag even when sprites are under left clip
3) Disabling rendering shouldn't clear flag
4) Should clear flag at the end of VBL even when $2001=0
5) Should set flag even when sprite Y coordinates are 239
6) Shouldn't set flag when sprite Y coordinates are 240 (off screen)
7) Shouldn't set flag when sprite Y coordinates are 255 (off screen)
8) Should set flag regardless of which sprites are involved
9) Shouldn't set flag when all scanlines have 7 or fewer sprites
10) Double-height sprites aren't handled properly


03-timing
---------
Tests sprite overflow flag timing to PPU clock accuracy

2) PPU VBL timing is wrong
3) PPU VBL timing is wrong
3) Flag cleared too early at end of VBL
4) Flag cleared too late at end of VBL
5) Flag set too early for first scanline
6) Flag set too late for first scanline
7) Horizontal positions shouldn't affect timing
8) Set too early for last sprites on first scanline
9) Set too late for last sprites on first scanline
10) Set too early for last scanline
11) Set too late for last scanline
12) Set too early when 9th sprite # is way after 8th
13) Set too late when 9th sprite # is way after 8th
14) Overflow on second scanline occurs too early
15) Overflow on second scanline occurs too late


04-obscure
----------
Tests the pathological behavior when 8 sprites are on a scanline
and the one just after the 8th is not on the scanline. After that,
the PPU interprets different bytes of each following sprite as
its Y coordinate. 1 2 3 4 5 6 7 8 9 10 11 12 13 14: If 1-8 are
on the same scanline, 9 isn't, then the second byte of 10, the
third byte of 11, fourth byte of 12, first byte of 13, second byte
of 14, etc. are treated as those sprites' Y coordinates for the
purpose of setting the overflow flag. This search continues until
all sprites have been scanned or one of the (erroneously interpreted)
Y coordinates places the sprite within the scanline.

2) Checks that second byte of sprite #10 is treated as its Y 
3) Checks that third byte of sprite #11 is treated as its Y 
4) Checks that fourth byte of sprite #12 is treated as its Y 
5) Checks that first byte of sprite #13 is treated as its Y 
6) Checks that second byte of sprite #14 is treated as its Y 
7) Checks that search stops at the last (64th) sprite
8) Same as test #2 but using a different range of sprites


05-emulator
-----------
Tests things that an optimized emulator is likely get wrong

2) Didn't set flag when $2002 wasn't read during frame
3) Disabling rendering didn't recalculate flag time
4) Changing sprite RAM didn't recalculate flag time
5) Changing sprite height didn't recalculate time

Multi-tests
-----------
The NES/NSF builds in the main directory consist of multiple sub-tests.
When run, they list the subtests as they are run. The final result code
refers to the first sub-test that failed. For more information about any
failed subtests, run them individually from rom_singles/ and
nsf_singles/.


Flashes, clicks, other glitches
-------------------------------
If a test prints "passed", it passed, even if there were some flashes or
odd sounds. Only a test which prints "done" at the end requires that you
watch/listen while it runs in order to determine whether it passed. Such
tests involve things which the CPU cannot directly test.


Alternate output
----------------
Tests generally print information on screen, but also report the final
result audibly, and output text to memory, in case the PPU doesn't work
or there isn't one, as in an NSF or a NES emulator early in development.

After the tests are done, the final result is reported as a series of
beeps (see below). For NSF builds, any important diagnostic bytes are
also reported as beeps, before the final result.


Output at $6000
---------------
All text output is written starting at $6004, with a zero-byte
terminator at the end. As more text is written, the terminator is moved
forward, so an emulator can print the current text at any time.

The test status is written to $6000. $80 means the test is running, $81
means the test needs the reset button pressed, but delayed by at least
100 msec from now. $00-$7F means the test has completed and given that
result code.

To allow an emulator to know when one of these tests is running and the
data at $6000+ is valid, as opposed to some other NES program, $DE $B0
$G1 is written to $6001-$6003.


Audible output
--------------
A byte is reported as a series of tones. The code is in binary, with a
low tone for 0 and a high tone for 1, and with leading zeroes skipped.
The first tone is always a zero. A final code of 0 means passed, 1 means
failure, and 2 or higher indicates a specific reason. See the source
code of the test for more information about the meaning of a test code.
They are found after the set_test macro. For example, the cause of test
code 3 would be found in a line containing set_test 3. Examples:

	Tones         Binary  Decimal  Meaning
	- - - - - - - - - - - - - - - - - - - - 
	low              0      0      passed
	low high        01      1      failed
	low high low   010      2      error 2


NSF versions
------------
Many NSF-based tests require that the NSF player either not interrupt
the init routine with the play routine, or if it does, not interrupt the
play routine again if it hasn't returned yet. This is because many tests
need to run for a while without returning.

NSF versions also make periodic clicks to prevent the NSF player from
thinking the track is silent and thus ending the track before it's done
testing.

-- 
Shay Green <gblargg@gmail.com>
