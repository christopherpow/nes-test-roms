#!/usr/bin/env python

bank_size_masks = [(2<<i)-1 for i in range(4)]

def calc_prg_bank(address, bank_mode, outer_bank, current_bank):
    """Calculate the 16K bank on A20-A14 for a given address."""
    cpu_a14 = (address >> 14) & 0x01
    outer_bank = outer_bank << 1
    bank_mode >>= 2  # discard mirroring bits
    if (bank_mode ^ cpu_a14) & 0x03 == 0x02:  # in UNROM fixed bank?
        bank_mode = 0  # if so, treat as NROM
    if (bank_mode & 0x02) == 0:  # in 32K bank mode?
        current_bank = (current_bank << 1) | cpu_a14
    bank_size_mask = bank_size_masks[(bank_mode >> 2) & 3]
    return ((current_bank ^ outer_bank) & bank_size_mask) ^ outer_bank

def test_with_bank_mode_size(bank_mode, outer_bank):
    print "mode $%02x, outer bank $%02x" % (bank_mode, outer_bank)
    out80 = [calc_prg_bank(0x8000, bank_mode, outer_bank, current_bank)
         for current_bank in range(16)]
    print "$8000 banks:", " ".join("%02x" % i for i in out80)
    outC0 = [calc_prg_bank(0xC000, bank_mode, outer_bank, current_bank)
             for current_bank in range(16)]
    print "$C000 banks:", " ".join("%02x" % i for i in outC0)

for outer_bank in (0x00, 0x3C, 0x3F):
    test_with_bank_mode_size(0x28, outer_bank)
for outer_bank in (0x00, 0x03, 0x3F):
    test_with_bank_mode_size(0x2C, outer_bank)
for bank_mode in (0x00, 0x08, 0x0C, 0x10, 0x18, 0x1C,
                  0x20, 0x28, 0x2C, 0x30, 0x38, 0x3C):
    test_with_bank_mode_size(bank_mode, 0x2A)

test_with_bank_mode_size(0x08, 0x00)
