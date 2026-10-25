# NROM DAC Test

## About

This program utilizes the [CPU test mode](https://www.nesdev.org/wiki/CPU_Test_Mode) to iterate through all possible DAC combinations of the five internal sound channels, grouped by their pin outputs.
This requires a special hardware setup to enable directly writing triangle phase, isolate pins 1 and 2 to get a clear voltage reading, as well as reading/writing the test registers of the APU at $4018-401A.

## Use

Ensure that pin 30 is tied high before use. A good way to check is if the counters on screen are counting up.

Measure the isolated voltage outputs of pins 1 and 2 at each iteration step.

## Compiling

Dependencies (these must be available in path):
- CA65
- LD65
- Mesen

Run the makefile in a suitable bash environment. For more detailed instructions, refer to Pinobatch's [NROM template](https://github.com/pinobatch/nrom-template). If no bash environment is available, run the make.bat file.

## Credits

- Initial template: yoeynsf
- Inspiration: plgDavid, lidnariq
- Programming help: lidnariq, Kasumi, Fiskbit
- NROM template: Pinobatch
- I-CHR: Kasumi
- NEXXT: FrankenGraphics
- Art: yoeynsf
- General assistance: the NESDev Discord server

Thanks so much for your wonderful help!!
