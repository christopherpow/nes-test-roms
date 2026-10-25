HelpString:

db 'TEST_PPU_READ_BUFFER -- Secret Reference Emulation Mode',13,10
db 'Commandline options:',13,10
db '  /D',9,'Do disassembly trace (note: does not set /T automatically)',13,10
db '  /T',9,'Text only: Do not do graphics mode',13,10
db '  /P',9,'Palette only: No NTSC simulation, no borders, no correct aspect ratio',13,10
db '  /M',9,'Mode-X only: Do not use VESA to provide NTSC in RGB32 without dithering',13,10
db '  /H,/?',9,'This help',13,10
db 10
db "The assembler trace, and the test ROM's console output, will",13,10
db 'both be output to STDOUT. Suggest redirecting with "> file.txt".',13,10
db 10
db 'I.e. like this: program /D /T > trace.log', 13,10
db 10
db 'If you do not redirect the output, there will be minor artifacts on the',13,10
db 'screen while the test runs; this is because the text is rendered on the',13,10
db 'graphical screen, but the BIOS does not support the Mode-X graphics used.',13,10
db 10
db 'Thank you for using this self-testing NES emulator.',13,10
db 'Written in April 2012 by Joel Yliluoma - http://iki.fi/bisqwit/',13,10
db 10
db 'P.S. Load any sub-40k NROM/CNROM/GxROM cart with :filename',13,10
db 0
