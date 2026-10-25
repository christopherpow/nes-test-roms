#!/usr/bin/env python3
#
# RLE compression tool for Action 53
# Copyright 2012-2015 Damian Yerrick
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty
# provided the copyright notice and this notice are preserved.
# This file is offered as-is, without any warranty.
#
"""

The compression format deals with 16-byte chunks considered as 8x8
pixel tiles with two bit planes.  Possible formats are as follows:

* Ordinary tile: two blocks of 8 bytes
* 2-bit tile (colors 0 and 1 or 2 and 3):
  a block of 8 bytes, then 8 bytes of $00 or $FF
* 2-bit tile (colors 0 and 2 or 1 and 3):
  8 bytes of $00 or $FF, then a block of 8 bytes
* 2-bit tile (colors 0 and 3 or 1 and 2):
  a block of 8 bytes, then the same block copied or complemented
* Solid color tile: 8 bytes of $00 or $FF then 8 bytes of $00 or $FF
* Repeat tile: Repeat previous 16 bytes
* Repeat tile from previous pattern table: Repeat 16 bytes starting
  4096 bytes back

"""

def pb8_oneplane(planedata, topValue=None):
    ctile = bytearray([0])
    lastc = topValue
    flag = 0
    for c in planedata:
        flag = flag << 1
        if c == lastc:
            flag = flag | 1
        else:
            ctile.append(c)
            lastc = c
    ctile[0] = flag
    return ctile

def pb53(chrdata, segsize=4096, copyprev=True):
    """Compress tile data with PB53.

chrdata -- byte string
segsize -- length of each segment
copyprev -- if enabled, allow tiles to reference the same numbered
tile in the previous segment

Return (pb53data, seekpoints)
pb53 is the compressed data
seekpoints is an array of indices of the start of the compressed data
for each segment after the first

"""
    out = bytearray()
    seekpoints = []

    for i in range(0, len(chrdata), 16):
        is_first_in_seg = i % segsize == 0
        if i > 0 and is_first_in_seg:
            seekpoints.append(len(out))
        tile = chrdata[i:i + 16]
        if len(tile) < 16:
            tile = bytearray(tile)
            tile.extend(bytearray(16 - len(tile)))

        # Solid color tiles: $84-$87
        solid0 = (tile[0] in (0x00, 0xFF)
                  and all(x == tile[0] for x in tile[1:8]))
        solid1 = (tile[8] in (0x00, 0xFF)
                  and all(x == tile[8] for x in tile[9:16]))
        if solid0 and solid1:
            ctrlbyte = 0x84 | (0x02 & tile[8]) | (0x01 & tile[0])
            out.append(ctrlbyte)
            continue

        # Duplicate previous tile in same segment: $82
        if not is_first_in_seg and chrdata[i - 16:i] == tile:
            out.append(0x82)
            continue

        # Duplicate tile from previous segment: $83
        if (copyprev and i >= segsize
            and chrdata[i - segsize:i - segsize + 16] == tile):
            out.append(0x83)
            continue

        # Encode first plane
        if solid0:
            ctrlbyte = 0x80 | (0x01 & tile[0])
            out.append(ctrlbyte)
        else:
            pb = pb8_oneplane(tile[0:8])
            out.extend(pb)

        # Encode second plane
        if solid1:
            ctrlbyte = 0x80 | (0x01 & tile[8])
            out.append(ctrlbyte)
        elif tile[0:8] == tile[8:16]:
            # Colors 0 and 3
            ctrlbyte = 0x82
            out.append(ctrlbyte)
        elif all(a ^ b == 0xFF for (a, b) in zip(tile[0:8], tile[8:16])):
            # Colors 1 and 2
            ctrlbyte = 0x83
            out.append(ctrlbyte)
        else:
            pb = pb8_oneplane(tile[8:16])
            out.extend(pb)
    return (out, seekpoints)

def unpb53plane(ctrlbyte, it):
    if ctrlbyte >= 0x80:
        # Solid plane
        p0data = 0xFF if (ctrlbyte & 0x01) else 0x00
        return ([p0data] * 8)
    p0data = [next(it)]
    while len(p0data) < 8:
        ctrlbyte = ctrlbyte << 1
        p0data.append(p0data[-1] if ctrlbyte & 0x80 else next(it))
    return p0data
    
def unpb53(data, numTiles=None, segsize=4096):
    out = bytearray()
    it = iter(data)
    for ctrlbyte in it:
        if numTiles is not None and len(out) >= numTiles * 16:
            break

        if ctrlbyte >= 0x84 and ctrlbyte <= 0x87:
            # Solid color tiles
            p0data = 0xFF if (ctrlbyte & 0x01) else 0x00
            out.extend([p0data] * 8)
            p1data = 0xFF if (ctrlbyte & 0x02) else 0x00
            out.extend([p1data] * 8)
            continue

        if ctrlbyte == 0x82:
            # Repeat previous tile
            out.extend(out[-16:])
            continue

        if ctrlbyte == 0x83:
            # Repeat corresponding tile from other bank
            out.extend(out[-segsize:-segsize + 16])
            continue

        # Decode each plane
        out.extend(unpb53plane(ctrlbyte, it))
        ctrlbyte = next(it)
        if ctrlbyte in (0x82, 0x83):
            # 2-color plane, colors 0/3 or 1/2
            xorbyte = 0xFF if (ctrlbyte & 0x01) else 0x00
            out.extend(c ^ xorbyte for c in out[-8:])
        else:
            out.extend(unpb53plane(ctrlbyte, it))

    return out

roms = [
    '../../my_games/Concentration Room 0.02.nes',
    '../roms/Zooming_Secretary.nes',
    '../../compomenu/roms/fhbg.nes',
    '../../compomenu/roms/lan.nes',
    '../../compomenu/roms/mower.nes',
    '../../compomenu/roms/slappin.nes',
    '../../compomenu/roms/thwaite.nes',
    '../../ruder/ruder.nes',
]

def test():
    import ines
    for filename in roms:
        rom = ines.load_ines(filename)
        (data, seekpoints) = pb53(rom['chr'])
        print("%s: compressed %s to %s\nseekpoints: %s"
              % (filename, len(rom['chr']), len(data), seekpoints))
        unpacked = unpb53(data)
        if unpacked != rom['chr']:
            diffs = [i if a != b else None
                     for (i, (a, b)) in enumerate(zip(rom['chr'], unpacked))]
            print("unpacked different starting at", diffs[0])
        else:
            print("unpack ok")
  
def parse_argv(argv):
    from optparse import OptionParser
    parser = OptionParser(usage="usage: %prog [options] [[-i] INFILE [[-o] OUTFILE]]")
    parser.add_option("-d", "--unpack", dest="unpacking",
                      help="unpack instead of packing",
                      action="store_true", default=False)
    parser.add_option("--block-size", dest="blocksize",
                      help="compress SIZE 16-byte units per segment (default 256)", metavar="SIZE",
                      type="int", default=256)
    parser.add_option("--no-prev", dest="copyprev",
                      help="don't use references to previous block",
                      action="store_false", default=True)
    parser.add_option("--raw", dest="withHeader",
                      help="don't write 2-byte length and segment seek points",
                      action="store_false", default=True)
    parser.add_option("-i", "--input", dest="infilename",
                      help="read input from INFILE", metavar="INFILE")
    parser.add_option("-o", "--output", dest="outfilename",
                      help="write output to OUTFILE", metavar="OUTFILE")
    (options, args) = parser.parse_args(argv[1:])

    # Fill unfilled roles with positional arguments
    argsreader = iter(args)
    infilename = options.infilename
    if infilename is None:
        try:
            infilename = next(argsreader)
        except StopIteration:
            infilename = '-'
    if infilename == '-' and options.unpacking:
        import sys
        if sys.stdin.isatty():
            raise ValueError('cannot decompress from terminal')

    outfilename = options.outfilename
    if outfilename is None:
        try:
            outfilename = next(argsreader)
        except StopIteration:
            outfilename = '-'
    if outfilename == '-' and not options.unpacking:
        import sys
        if sys.stdout.isatty():
            raise ValueError('cannot compress to terminal')

    return (infilename, outfilename, options.unpacking,
            options.blocksize, options.copyprev, options.withHeader)

argvTestingMode = True

def main(argv=None):
    import sys
    if argv is None:
        argv = sys.argv
        if (argvTestingMode and len(argv) < 2
            and sys.stdin.isatty() and sys.stdout.isatty()):
            argv.extend(input('args:').split())
    try:
        (infilename, outfilename, unpacking,
         blocksize, copyprev, withHeader) = parse_argv(argv)
    except Exception as e:
        import sys
        sys.stderr.write("%s: %s\n" % (argv[0], str(e)))
        sys.exit(1)

    # Read input file
    infp = None
    try:
        if infilename != '-':
            infp = open(infilename, 'rb')
        else:
            infp = sys.stdin
        data = infp.read()
    finally:
        if infp and infilename != '-':
            infp.close()
        del infilename, infp

    if unpacking:

        # Decompress input file
        if withHeader:
            numTiles = data[0] * 256 + data[1]
            startOffset = -((-numTiles) // 256) * 2
        else:
            numTiles = None
            startOffset = 0
        outdata = unpb53(data[startOffset:], blocksize * 16, copyprev)
    else:

        # Compress input file
        (outdata, seekpoints) = pb53(data, blocksize * 16, copyprev)
        if withHeader:

            # The .pb53 header is the unpacked length in 16-byte units,
            # 16-bit big endian, followed by 16-bit seek points
            sz = (len(data) // 16) % 0x10000
            headerwords = [sz]
            headerwords.extend(seekpoints)
            header = b"".join(bytes([(sz >> 8) & 0xFF, sz & 0xFF])
                              for sz in headerwords)
            outdata = header + outdata
    
    # Read input file
    outfp = None
    try:
        if outfilename != '-':
            outfp = open(outfilename, 'wb')
        else:
            outfp = sys.stdout
        outfp.write(outdata)
    finally:
        if outfp and outfilename != '-':
            outfp.close()

if __name__=='__main__':
    main()
##    test()
