CL65=cl65
KNESPATH=../../knes/knes
TARGETBASE=serom

default: $(TARGETBASE).nes

%.o: %.asm
	$(CL65) -t nes -c --create-dep $<.d -Cl -I $(KNESPATH) -Oris --listing $<.lst -o $@ $<

%.o: %.c
	$(CL65) -t nes -c --create-dep $<.d -Cl -I $(KNESPATH) -Oris --listing $<.lst -o $@ $<

ta tb tc t0 t1: $(TARGETBASE)_ctl.o $(TARGETBASE).o $(KNESPATH)/knes.lib
	$(CL65) -t none --mapfile $(TARGETBASE).map -Wl --dbgfile,$(TARGETBASE).nes.dbg -o /dev/null -C $(TARGETBASE).cfg $^

# Surely there is some way to duplicate a segment using just cl65... maybe?
$(TARGETBASE).nes: ta tb tc t0 t1
	cat ta t0 tb t1 tb tc > $(TARGETBASE).nes

clean:
	rm -f $(TARGETBASE)_ctl.o $(TARGETBASE).o $(TARGETBASE).nes ta t0 tb t1 tc *.lst *.d *.dbg *.map
