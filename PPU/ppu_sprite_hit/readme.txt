

01-basics
---------
Tests basic sprite 0 hit behavior (nothing timing related).

2) Flag isn't working at all
3) Should hit even when completely behind background
4) Should miss when background rendering is off
5) Should miss when sprite rendering is off
6) Should miss when all rendering is off
7) All-transparent sprite should miss
8) Only low two palette index bits are relevant
9) Any non-zero palette index should hit with any other
10) Should miss when background is all transparent
11) Should always miss other sprites


02-alignment
------------
Tests alignment of sprite hit with background.
Places a solid background tile in the middle of the screen and
places the sprite on all four edges both overlapping and
non-overlapping.

2) Basic sprite-background alignment is way off
3) Sprite should miss left side of bg tile
4) Sprite should hit left side of bg tile
5) Sprite should miss right side of bg tile
6) Sprite should hit right side of bg tile
7) Sprite should miss top of bg tile
8) Sprite should hit top of bg tile
9) Sprite should miss bottom of bg tile
10) Sprite should hit bottom of bg tile


03-corners
----------
Tests sprite 0 hit using a sprite with a single pixel set,
for each of the four corners.

2) Lower-right pixel should hit
3) Lower-left pixel should hit
4) Upper-right pixel should hit
5) Upper-left pixel should hit


04-flip
-------
Tests sprite 0 hit for single pixel sprite and background.

2) Horizontal flipping doesn't work
3) Vertical flipping doesn't work
4) Horizontal + Vertical flipping doesn't work


05-left_clip
------------
Tests sprite 0 hit with regard to clipping of left 8 pixels of screen.

2) Should miss when entirely in left-edge clipping
3) Left-edge clipping occurs when $2001 is not $1E
4) Left-edge clipping is off when $2001 = $1E
5) Left-edge clipping should block hits only when X = 0
6) Should miss; sprite pixel covered by left-edge clip
7) Should hit; sprite pixel outside left-edge clip
8) Should hit; sprite pixel outside left-edge clip


06-right_edge
-------------
Tests sprite 0 hit with regard to column 255 (ignored) and off
right edge of screen.

2) Should always miss when X = 255
3) Should hit; sprite has pixels < 255
4) Should miss; sprite pixel is at 255
5) Should hit; sprite pixel is at 254
6) Should also hit; sprite pixel is at 254


07-screen_bottom
----------------
Tests sprite 0 hit with regard to bottom of screen.

2) Should always miss when Y >= 239
3) Can hit when Y < 239
4) Should always miss when Y = 255
5) Should hit; sprite pixel is at 238
6) Should miss; sprite pixel is at 239
7) Should hit; sprite pixel is at 238


08-double_height
----------------
Tests basic sprite 0 hit double-height operation.

2) Lower sprite tile should miss bottom of bg tile
3) Lower sprite tile should hit bottom of bg tile
3) Lower sprite tile should miss top of bg tile
4) Lower sprite tile should hit top of bg tile


09-timing
---------
Tests sprite 0 hit timing to PPU clock accuracy.

2) PPU VBL timing is wrong
3) Flag set too soon for upper-left corner
4) Flag set too late for upper-left corner
5) Flag set too soon for upper-right corner
6) Flag set too late for upper-right corner
7) Flag set too soon for lower-left corner
8) Flag set too late for lower-left corner
9) Flag cleared too soon at end of VBL
10) Flag cleared too late at end of VBL


10-timing_order
---------------
Ensures that hit time is based on position on screen, and unaffected
by which pixel of sprite is being hit (upper-left, lower-right, etc.)

2) Upper-left corner
3) Upper-right corner
4) Lower-left corner
5) Lower-right corner
6) Hit time shouldn't be based on pixels under left clip
7) Hit time shouldn't be based on pixels at X=255
8) Hit time shouldn't be based on pixels off right edge

Multi-tests
-----------
The NES/NSF builds in the main directory consist of multiple sub-tests.
When run, they list the subtests as they are run. The final result code
refers to the first sub-test that failed. For more information about any
failed subtests, run them individually from rom_singles/ and
nsf_singles/.


Multi-tests
-----------
The NES/NSF builds in the main directory consist of multiple sub-tests.
When run, they list the subtests as they are run. The final result code
refers to the first sub-test that failed. For more information about any
failed subtests, run them individually from rom_singles/ and
nsf_singles/.


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
