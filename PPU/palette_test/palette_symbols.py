import sys
assert sys.version_info[0] >= 3, "Python 3 required."

from collections import OrderedDict

def label_to_nl(label_file, nl_file, range_min, range_max):
    labels = []
    try:
        of = open(label_file, "rt")
        labels = of.readlines()
    except IOError:
        print("skipped: "+label_file)
        return
    labs = {}
    sout = ""
    for line in labels:
        words = line.split()
        if (words[0] == "al"):
            adr = int(words[1], 16)
            sym = words[2]
            sym = sym.lstrip('.')
            if (sym[0] == '_' and sym[1] == '_'):
                continue # skip compiler internals
            if (adr >= range_min and adr <= range_max):
                if (adr in labs):
                    # multiple symbol
                    text = labs[adr]
                    textsplit = text.split()
                    if (sym not in textsplit):
                        text = text + " " + sym
                        labs[adr] = text
                else:
                    labs[adr] = sym
    for (adr, sym) in labs.items():
        sout += ("$%04X#%s#\n" % (adr, sym))
    open(nl_file, "wt").write(sout)
    print("debug symbols: " + nl_file)
    
if __name__ == "__main__":
    label_to_nl("temp\\palette_labels.txt", "temp\\palette.nes.ram.nl", 0x0000, 0x7FF)
    label_to_nl("temp\\palette_labels.txt", "temp\\palette.nes.0.nl", 0x8000, 0xBFFF)
    label_to_nl("temp\\palette_labels.txt", "temp\\palette.nes.1.nl", 0xC000, 0xFFFF)
    label_to_nl("temp\\palette_pal_labels.txt", "temp\\palette_pal.nes.ram.nl", 0x0000, 0x7FF)
    label_to_nl("temp\\palette_pal_labels.txt", "temp\\palette_pal.nes.0.nl", 0x8000, 0xBFFF)
    label_to_nl("temp\\palette_pal_labels.txt", "temp\\palette_pal.nes.1.nl", 0xC000, 0xFFFF)
