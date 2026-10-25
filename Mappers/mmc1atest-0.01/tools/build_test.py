#!/usr/bin/env python3
import os, sys, argparse

size_to_fff4 = {
    8192: 0, 16384: 1, 32768: 2, 65536: 0, 131072: 3, 262144: 4, 524288: 5
}
INES_MAPPER_MMC1A = 155

def open_rb_and_read(filename):
    with open(filename, "rb") as infp:
        return infp.read()

def parse_rom_title(title):
    btitle = title.encode("ascii")
    if not 2 <= len(btitle) <= 16:
        raise ValueError("length out of range 2-16" % title)
    return btitle

def parse_argv(argv):
    p = argparse.ArgumentParser()
    p.add_argument("prgrom", type=open_rb_and_read,
                   help="PRG ROM data (16 KiB) to be duplicated")
    p.add_argument("chrrom", nargs='?', type=open_rb_and_read,
                   help="CHR ROM data")
    p.add_argument("-t", "--title", type=parse_rom_title,
                   help="internal title (2 to 16 ASCII characters)")
    p.add_argument("-o", "--output", default="-",
                   help="write iNES ROM file (default: stdout)")
    parsed = p.parse_args(argv[1:])
    if parsed.output == '-':
        if sys.stdout.isatty():
            p.error("cannot write ROM to a terminal")
        try:
            sys.stdout.buffer.writelines
        except AttributeError:
            p.error("cannot write a list of banks to writelines")
    return parsed

def main(argv=None):
    args = parse_argv(argv or sys.argv)

    NUM_BANKS = 16
    mapper = INES_MAPPER_MMC1A
    prgrom = bytearray(args.prgrom)
    if len(prgrom) != 0x4000:
        raise ValueError("PRG ROM is %d bytes; expected 16384" % len(prgrom))
    fff4 = size_to_fff4[NUM_BANKS * len(prgrom)] << 4

    chrrom = args.chrrom
    if chrrom is not None:
        chrsz = max(0x2000, len(chrrom))
        if chrsz > 0x20000:
            raise ValueError("CHR ROM is %d bytes; expected 1 to 131072"
                             % chrsz)
        if chrsz & (chrsz - 1): chrsz = 1 << sz.bit_length()
        if len(chrrom) < chrsz:
            chrrom += b'\xFF' * (chrsz - len(chrrom))
        chrsum = sum(chrrom) & 0xFFFF
        fff4 |= size_to_fff4[chrsz]
    else:
        chrsum = 0
        fff4 |= 8  # 8 KiB CHR RAM

    # Set the SSS header for FamicomBox
    if args.title is not None:
        prgrom[0x3FF0 - len(args.title):0x3FF0] = args.title
        prgrom[0x3FF7] = len(args.title) - 1
    prgrom[0x3FF0:0x3FF2] = 0, 0  # rm old checksum before recalculating
    prgrom[0x3FF2:0x3FF4] = chrsum >> 8, chrsum & 0xFF
    prgrom[0x3FF4] = fff4
    prgrom[0x3FF5] = 4  # Generic MMC
    prgrom[0x3FF6] = 1  # ASCII
    prgrom[0x3FF8] = 0x33  # Other publisher
    prgrom[0x3FF9] = -sum(prgrom[0x3FF2:0x3FF9]) & 0xFF

    ines = bytearray(b"NES\x1A" + bytes(12))
    ines[4] = NUM_BANKS
    if chrrom: ines[5] = len(chrrom) // 8192
    ines[6] = (mapper & 0x0F) << 4
    ines[7] = mapper & 0xF0
    
    banks = [ines]
    for banknum in range(NUM_BANKS):
        bank = bytearray(prgrom)
        bank[0x3FD0] = banknum  # Set the "current bank" for the test
        prgsum = sum(bank) & 0xFFFF  # Fix up the bank's SSS header
        bank[0x3FF0:0x3FF2] = prgsum >> 8, prgsum & 0xFF
        banks.append(bank)
    if chrrom: banks.append(chrrom)

    if args.output == '-':
        sys.stdout.buffer.writelines(banks)
    else:
        with open(args.output, "wb") as outfp:
            outfp.writelines(banks)

if __name__=='__main__':
    if 'idlelib' in sys.modules:
        import shlex
        main(shlex.split("""
./build_test.py -t "MMC1A TEST" ../mmc1atest.prg ../obj/nes/bggfx.chr
-o test.nes
"""))
    else:
        main()

