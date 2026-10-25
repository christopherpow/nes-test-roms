#!/usr/bin/env python3
import sys
import os
import argparse
import zipfile
import tarfile

def make_zipfile(outname, filenames, prefix):
    with zipfile.ZipFile(outname, "w", zipfile.ZIP_DEFLATED) as z:
        for filename in filenames:
            z.write(filename, prefix+filename)

def make_tarfile(outname, filenames, prefix, mode="w"):
    with tarfile.open(outname, "w", zipfile.ZIP_DEFLATED) as z:
        for filename in filenames:
            z.add(filename, prefix+filename)

def make_tarfile_gz(outname, filenames, prefix):
    return make_tarfile(outname, filenames, prefix, mode="w:gz")

def make_tarfile_bz2(outname, filenames, foldername):
    return make_tarfile(outname, filenames, prefix, mode="w:bz2")

def make_tarfile_xz(outname, filenames, foldername):
    return make_tarfile(outname, filenames, prefix, mode="w:xz")

formathandlers = [
    (".zip", make_zipfile),
    (".tar", make_tarfile),
    (".tgz", make_tarfile_gz),
    (".tar.gz", make_tarfile_gz),
    (".tbz", make_tarfile_bz2),
    (".tar.bz2", make_tarfile_bz2),
    (".txz", make_tarfile_xz),
    (".tar.xz", make_tarfile_xz),
]

tophelptext = """
Make a zip or tar archive containing specified files without a tar bomb.
"""
bottomhelptext = """

Supported output formats: """+", ".join(x[0] for x in formathandlers)

def parse_argv(argv):
    p = argparse.ArgumentParser(
        description=tophelptext, epilog=bottomhelptext
    )
    p.add_argument("filelist",
                   help="name of file containing newline-separated relative "
                   "paths to files to include, or - for standard input")
    p.add_argument("foldername",
                   help="name of folder in archive (e.g. hello-1.2.5)")
    p.add_argument("-o", "--output",
                   help="path of archive (default: foldername + .zip)")
    return p.parse_args(argv[1:])

def get_writerfunc(outname):
    outbaselower = os.path.basename(outname).lower()
    for ext, writerfunc in formathandlers:
        if outbaselower.endswith(ext):
            return writerfunc
    raise KeyError(os.path.splitext(outbaselower)[1])

def main(argv=None):
    args = parse_argv(argv or sys.argv)
    if args.filelist == '-':
        filenames = set(sys.stdin)
    else:
        with open(args.filelist, "r") as infp:
            filenames = set(infp)
    filenames = set(x.strip() for x in filenames)
    filenames = sorted(x for x in filenames if x)

    outname = args.output or args.foldername + ".zip"
    writerfunc = get_writerfunc(outname)
    writerfunc(outname, filenames, args.foldername+"/")

if __name__=='__main__':
    main()
