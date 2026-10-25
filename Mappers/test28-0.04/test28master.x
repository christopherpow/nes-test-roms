#
# Linker script for test28
# Copyright 2010 Damian Yerrick
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty
# provided the copyright notice and this notice are preserved.
# This file is offered as-is, without any warranty.
#
MEMORY {
  ZP:     start = $10, size = $E0, type = rw;
  # use first $10 zeropage locations as locals and last as fixed globals
  HEADER: start = 0, size = $0010, type = ro, file = %O, fill=yes, fillval=$00;
  RAM:    start = $0300, size = $0100, type = rw;
  MODULE:    start = $0400, size = $0400, type = rw;
  ROM0:    start = $C000, size = $4000, type = ro, file = %O, fill=yes, fillval=$FF;
  ROM7:    start = $C000, size = $4000, type = ro, file = %O, fill=yes, fillval=$FF;
}

SEGMENTS {
  INESHDR:  load = HEADER, type = ro, align = $10;
  ZEROPAGE: load = ZP, type = zp;
  BSS:      load = RAM, type = bss, define = yes;
  CODE0:    load = ROM0, type = ro, align = $100;
  RODATA0:  load = ROM0, type = ro, align = $100;
  VECTORS0: load = ROM0, type = ro, start = $FFF6;
  LOWCODE:  load = ROM7, run = MODULE, define = yes, type = rw, align = $100, optional=yes;
  CODE:     load = ROM7, type = ro, align = $100;
  RODATA:   load = ROM7, type = ro, align = $100;
  VECTORS:  load = ROM7, type = ro, start = $FFF6;
}

FILES {
  %O: format = bin;
}

