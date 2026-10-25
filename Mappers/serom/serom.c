#include <knes.h>
#include <conio.h>

void write8000(byte ctrl);
void writeA000(byte chr1);
void writeC000(byte chr2);
void writeE000(byte prg);

#define NameTable(x,y) ((x&31)|((y&31)<<5)|0x2000|((x&32)<<5)|((y&32)<<6))

byte fail;

void main (void) {
	fail=0;
	PPU.ctrl = BASE_NT0|ADDRINC_1|SPR_CHR0|BG_CHR0|SPR_8X8|NMI_ON;

	// Upload minimal palette
	PPU_ADDR(0x3F00);
	PPU.data = 0xF;
	PPU.data = 0x01;
	PPU.data = 0x23;
	PPU.data = 0x30;

	writeA000(0);
	writeC000(0);

	/* set 32KiB mode, check both locations */
	write8000(0); //8KBCHR, 32KBPRG, 1scA
	// Also set up CHR and NT while we're at it

	PPU_ADDR(0x2060);
	// ......0123456789a123456789b123456789c1
	cprintf("Submapper 5 has been allocated  "
	        " for the SEROM, SHROM, and      "
	        " SH1ROM PCBs, which do not      "
	        " support PRG banking at all.    "
	        "A few games rely on not having  "
	        " to set any registers pertaining"
	        " to PRG banking.");

	PPU_ADDR(0x2160);
	cprintf("see also: http://forums.nesdev  "
	        " .com/viewtopic.php?t=8955");
	
	PPU_ADDR(0x2200);
	cprintf("32K mode- %d,%d",_M(0x8000),_M(0xC000));
	if (_M(0x8000) == 0 && _M(0xC000) == 1) {
		PPU_ADDR(0x2222);
		cprintf("- good");
	} else {
		fail++;
		PPU_ADDR(0x2222);
		cprintf("- what?");
	}

	/* set UNROM mode, attempting to set bank 1 for both */
	write8000(_B(01100));
	writeE000(1);
	PPU_ADDR(0x2240);
	cprintf("'16+16F' mode- %d,%d",_M(0x8000),_M(0xC000));
	if (_M(0x8000) == 1 && _M(0xC000) == 1) {
		fail++;
		PPU_ADDR(0x2262);
		cprintf("- plain MMC1 emulation");
	} else if (_M(0x8000) == 0 && _M(0xC000) == 1) {
		PPU_ADDR(0x2262);
		cprintf("- SEROM");
	} else {
		fail++;
		PPU_ADDR(0x2262);
		cprintf("- what?");
	}
	
	/* set m180 mode, attempting to set bank 0 for both */
	write8000(_B(01000));
	writeE000(0);
	PPU_ADDR(0x2280);
	cprintf("'16F+16' mode- %d,%d",_M(0x8000),_M(0xC000));
	if (_M(0x8000) == 0 && _M(0xC000) == 0) {
		fail++;
		PPU_ADDR(0x22A2);
		cprintf("- plain MMC1 emulation");
	} else if (_M(0x8000) == 0 && _M(0xC000) == 1) {
		PPU_ADDR(0x22A2);
		cprintf("- SEROM");
	} else {
		fail++;
		PPU_ADDR(0x22A2);
		cprintf("- what?");
	}

	PPU_ADDR(0x2300);
	if (fail == 0) {
		cprintf("All tests passed");
	} else {
		cprintf("%d tests failed",fail);
	}

	/* enable video */
	wait_vblank();
	PPU.mask = GRAYSCL_OFF|BGCLIP_OFF|SPRCLIP_OFF|BGREND_ON|SPRREND_OFF|
		TINTRED_OFF|TINTGRN_OFF|TINTBLU_OFF;
	PPU.ctrl = BASE_NT0|ADDRINC_1|SPR_CHR0|BG_CHR0|SPR_8X8|NMI_ON;
	PPU_SCROLL(0,0);
	PPU_ADDR(0);
	while (1);
}
