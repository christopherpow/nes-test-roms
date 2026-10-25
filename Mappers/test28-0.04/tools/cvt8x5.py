#!/usr/bin/env python3
"""
Converts 8x5 font to interleaved ROM data for quick expander
by Damian Yerrick
license: zlib
"""

from __future__ import with_statement, division
from PIL import Image
from array import array
import sys

def main(argv=None):
    argv = argv or sys.argv
    im = Image.open(argv[1])
    w = im.size[0] // 8
    data = bytearray( im.getdata())
    data = bytearray(sum(0x80 >> i if d else 0
                         for i, d in enumerate(data[i:i + 8]))
                     for i in range(0, len(data), 8))
    idxs = [b
            for i in range(0, 5 * w, w)
            for j in range(i, i + 2)
            for k in range(j, len(data), 5 * w)
            for b in range(k, k + w, 2)]
    data = bytearray(data[b] for b in idxs)
    with open(argv[2], "wb") as outfp:
        outfp.write(data)

if __name__=="__main__":
    main()
