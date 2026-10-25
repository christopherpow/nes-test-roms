# nes-audio-tests

Test ROMs and NSFs for NES and Famicom expansion audio. This collection is for testing various details of audio, and verifying emulator implementation. Pre-built ROMs can be found in the build/ folder.

Relative volume tests:
- **db_apu** - Full volume APU square vs. APU triangle.
- **db_vrc6** - Full volume APU square vs. full volume VRC6 square. Hotswap.
- **db_vrc7** - Full volume APU square vs. full volume VRC7 pseudo-square (2:1 modulator at 50%, full feedback). Hotswap.
- **db_fds** - Full volume APU square vs. full volume FDS square. Hotswap.
- **db_mmc5** - Full volume APU square vs. full volume MMC5 square. Hotswap.
- **db_n163** - Full volume APU square vs. full volume N163 square, 1 channel mode. Hotswap.
- **db_5b** - Full volume APU square vs. volume 12 5B square. Hotswap.

  I am collecting results from these volume tests for a survey. Information is available here:
  - https://forums.nesdev.com/viewtopic.php?t=17741

Hotswap test ROMs are to be loaded on a suitable dev cart. On reset they copy their code to RAM and begin executing there. A buzz will be played through the DMC channel to indicate it is ready, and it will wait ~4 seconds for you to pull out the cartridge, then insert an appropriate expansion audio cartridge. Another buzz will indicate the code is still running before the test begins. After completing the cart will repeat the test. (Hotswapping frequently causes a crash, so it may take multiple attempts.) NROM versions of the hotswap ROMs may be used if the dev cart does not support the original mapper.

Some people are uncomfortable with the idea of hotswapping cartridges. It has been safe in my experience, doing it hundreds of times, but I cannot guarantee it's a 100% safe procedure. All I can say is that it's been worth the risk for me to be able to test these things. Do not hotswap your cartridges if you don't accept this risk.


Other tests: (these are not survey tests, merely part of my personal ongoing investigations)
- **patch_vrc7** - Comparison of prospective VRC7 built-in patch set against the actual set. ([reference recording](http://rainwarrior.ca/projects/nes/patch_vrc7_nukeykt.flac))
- **clip_vrc7** - Demonstration of clipping in the VRC7 amplifier. ([commentary](https://forums.nesdev.org/viewtopic.php?p=224897#p224897))
- **clip_5b** - Demonstration of nonlinearity in the 5B amplifier. ([commentary](https://forums.nesdev.org/viewtopic.php?p=225012#p225012))
- **noise_vrc7** - White noise to characterize the VRC7 filters.
- **noise_5b** - White noise to characterize the 5B filters, other frequency tests.
- **sweep_5b** - Frequency sweep and noise to characterize the 5B filters. (Good for APU too. [commentary](https://forums.nesdev.org/viewtopic.php?p=225699#p225699))
- **envelope_5b** - Frequency and phase reset verification for 5B envelope, other frequency verification.
- **phase_5b** - Tone phase behaviour for 5B.
- **nsf_init_y** - 8 bit beep readout of Y register value passed to INIT.
- **tri_silence** - Tests various ways of silencing the triangle, clarifies interaction between $4008/400B and linear counter.
- **test_vrc7** - Examines properties of the VRC7 "test" register $0F.
- **test_n163_longwave** - Tests long period values of N163 often neglected by emulators.
- **dac_square** - Tests the linearity of the square channel DAC. ([commentary](https://forums.nesdev.org/viewtopic.php?t=24288))
- **dac_tnd0,1,2,3** - Tests the linearity of the triangle/noise/DMC channel DAC. ([commentary](https://forums.nesdev.org/viewtopic.php?t=24288))

NSF2 tests: (to test NSF players for [NSF2](https://wiki.nesdev.com/w/index.php/NSF2) support)
- **nsf2_init_play** - The non-returning INIT and NMI-driven PLAY feature.
- **nsf2_init_no_play** - The non-returning INIT and suppressed PLAY feature.
- **nsf2_irq** - The IRQ feature.
- **nsf2_saw_song** - DMC IRQ saw wave test [by Blargg](http://blargg.8bitalley.com/misc/nes-saw/)


Notes:
- swap.s - common code for hotswap tests
- [lfo_vrc7.s](https://github.com/bbbradsmith/nes-audio-tests/tree/c5051051cb0c50edfa799e55747f14189a2628d9) - retracted test using VRC7 chip reset, which did reset tremolo LFO but not vibrato. Test register at $0F can do this better, see test_vrc7.s instead.
- NESDev Forums: [Famicom expansion hardware recordings](https://forums.nesdev.org/viewtopic.php?t=8639) - old thread with some hardware recordings still available.

Building:
- Get CC65 and put it in the cc65/ folder. Link: http://cc65.github.io/cc65/
- Use one of the batch files to compile.

License:
- These files may be freely redistributed and modified for any purpose. Credit to the original author and/or a link to the original source would be appreciated, but is not required.
