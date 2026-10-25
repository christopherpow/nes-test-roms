#!/usr/bin/env python3
"""

Convert anything to unique tiles

"""
import sys
import argparse

def parse_argv(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("INFILE")
    parser.add_argument("TILEFILE")
    parser.add_argument("MAPFILE")
    parser.add_argument("--block-size", type=int, default=16,
                        help="size of each tile (NES, GB: 16; SMS, MD, SNES: 32)")
    parser.add_argument("--map-add", type=int, default=0,
                        help="add this to all map entries")
    args = parser.parse_args(argv[1:])
    return args

def uniq(it):
    tiles = []
    tile2id = {}
    tilemap = []
    for tile in it:
        if tile not in tiles:
            tile2id[tile] = len(tiles)
            tiles.append(tile)
        tilemap.append(tile2id[tile])
    return tiles, tilemap

def main(argv=None):
    args = parse_argv(argv or sys.argv)
    with open(args.INFILE, "rb") as infp:
        data = infp.read()
    block_size = args.block_size
    data = [data[i:i + block_size] for i in range(0, len(data), block_size)]
    tiles, tilemap = uniq(data)
    if len(tiles) > 256:
        print("%s: too many tiles (%d)"
              % (args.INFILE, len(tiles)), file=sys.stderr)
        sys.exit(1)
    map_add = args.map_add
    tilemap = bytes((map_add + b) & 0xFF for b in tilemap)
    with open(args.TILEFILE, "wb") as outfp:
        outfp.writelines(tiles)
    with open(args.MAPFILE, "wb") as outfp:
        outfp.write(tilemap)

if __name__=='__main__':
    main()
##    main(["uniq.py", "testbg.chr", "woods.chr", "testbg.map"])
