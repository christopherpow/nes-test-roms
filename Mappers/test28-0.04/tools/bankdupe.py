#!/usr/bin/env python3
import sys

# 28 once emulators support it; 2 for testing
# can also use 7 or 34 if bank size is 2
mapper = 28
# 0: 00  1: 01
#    11     01
mirroring = 0

def main(argv=None):
    argv = argv or sys.argv
    infilename = argv[1]
    outfilename = argv[2]
    size = int(argv[3])
    if size & (size - 1) != 0:
        print("bankdupe.py: size %d is not a power of two"
              % (size,), file=sys.stderr)
        sys.exit(1)
    with open(infilename, 'rb') as infp:
        infp.read(16)
        bank0 = bytearray(infp.read(16384))
        bank1 = bytearray(infp.read(16384))
    header = bytearray(b'NES\x1A')
    header.extend([size, 0,
                   ((mapper & 0x0F) << 4) | mirroring,
                   mapper & 0xF0])
    header.extend([0] * 8)
    assert len(header) == 16

    with open(outfilename, 'wb') as outfp:
        outfp.write(header)
        for i in range(size - 1):
            bank0[0x3FF8] = i
            outfp.write(bank0)
        bank1[0x3FF8] = size - 1
        outfp.write(bank1)

if __name__=='__main__':
    main()
##    main([sys.argv[0], '../test28.prg', '../test28.nes'])
