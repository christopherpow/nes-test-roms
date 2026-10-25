#!/usr/bin/env python3
#
# Nametable compression for cut scenes
# Copyright 2011-2015 Damian Yerrick
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty
# provided the copyright notice and this notice are preserved.
# This file is offered as-is, without any warranty.
#
from __future__ import with_statement

def histo(it):
    """Count occurrences of each distinct element in an iterable."""
    out = {}
    for el in it:
        out.setdefault(el, 0)
        out[el] += 1
    return out

def dedupe_chr(chrdata):
    seen_chrdata = {}
    nt = []
    for tile in chrdata:
        seen_chrdata.setdefault(tile, len(seen_chrdata))
        nt.append(seen_chrdata[tile])
    seen_chrdata = sorted(seen_chrdata.items(), key=lambda x: x[1])
    seen_chrdata = [row[0] for row in seen_chrdata]
    return (seen_chrdata, nt)

def compress_nt(ntdata):
    from sys import stderr as red
    from bitbuilder import BitBuilder, log2

    runcounts = {}  # used for determining backref
    base = 0
    runs = []
    greatest = -1
    while base < len(ntdata):

        # measure the run of new tiles (greatest+i+1)
        # starting at t
        i = 0
        imax = min(128, len(ntdata) - base)
        while (i < imax
               and (greatest + i + 1) % 256 == ntdata[base + i]):
            i += 1
        if i > 0:
            greatest += i
            base += i
            runs.append((-1, i))
            continue

        # measure the +0 run starting at t
        i = 1
        imax = min(128, len(ntdata) - base)
        while (i < imax
               and ntdata[base] == ntdata[base + i]):
            i += 1

        # we use the same number of bits for a backreference
        # that are in greatest
        runs.append((ntdata[base], i, log2(greatest) + 1))
        runcounts.setdefault(ntdata[base], 0)
        runcounts[ntdata[base]] += 1
        base += i
    runcounts = sorted(runcounts.items(), key=lambda x: -x[1])
    most_common_backref = runcounts[0][0] if len(runcounts) else 0

    out = BitBuilder()
    out.append(most_common_backref, 8)
    for row in runs:
        idx, runlength = row[:2]
        out.appendGamma(runlength - 1)
        if idx < 0:
            out.append(2, 2)
        elif idx == most_common_backref:
            out.append(3, 2)
        else:
            nbits = row[2]
            if idx >= 1 << nbits:
                print("index FAIL! %d can't fit in %d bits" % (idx, nbits),
                      file=red)
            out.append(idx, nbits + 1)
    return str(out)
