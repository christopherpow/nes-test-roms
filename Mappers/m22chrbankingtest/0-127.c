#include <knes.h>

#define NameTable(x,y) ((x&31)|((y&31)<<5)|0x2000|((x&32)<<5)|((y&32)<<6))

int ii, jj, tt;

void main (void) {
	PPU.ctrl = BASE_NT0|ADDRINC_1|SPR_CHR0|BG_CHR0|SPR_8X8|NMI_ON;

	// Upload minimal palette
	PPU_ADDR(0x3F00);
	PPU.data = 0xF;
	PPU.data = 0x01;
	PPU.data = 0x23;
	PPU.data = 0x30;

	PPU.ctrl = BASE_NT0|ADDRINC_32|SPR_CHR0|BG_CHR0|SPR_8X8|NMI_ON;

	PPU_ADDR(NameTable(14,7));
	for (ii=0x40;ii<0x50;ii++)
		PPU.data = ii;
	PPU_ADDR(NameTable(15,7));
	for (ii=0x50;ii<0x60;ii++)
		PPU.data = ii;
	PPU_ADDR(NameTable(16,7));
	for (ii=0x60;ii<0x70;ii++)
		PPU.data = ii;
	PPU_ADDR(NameTable(17,7));
	for (ii=0x70;ii<0x80;ii++)
		PPU.data = ii;

	PPU_SCROLL(0,0);
	PPU_ADDR(0);

	#if 1
	/* Correct version */
	while(1) {
		for (jj=0;jj<16;jj++) {
			for (ii=0;ii<16;ii++) {
				wait_vblank();
				PPU.mask = GRAYSCL_OFF|BGCLIP_OFF|SPRCLIP_OFF|BGREND_ON|SPRREND_OFF|
					((ii & 7) << 5); // violate abstraction for visibility
				_M(0xB001) = ii;
				_M(0xB003) = jj;
				for (tt=0;tt<7;tt++) wait_vblank();
			}
		}
	}
	#endif

#if 0
	/* Nestopia/puNES compatible version */
	while(1) {
		for (jj=0;jj<16;jj+=2) {
			for (ii=0;ii<32;ii++) {
				wait_vblank();
				PPU.mask = GRAYSCL_OFF|BGCLIP_OFF|SPRCLIP_OFF|BGREND_ON|SPRREND_OFF|
					((ii & 7) << 5); // violate abstraction for visibility
				_M(0xB001) = ii;
				_M(0xB003) = jj;
				for (tt=0;tt<7;tt++) wait_vblank();
			}
		}
	}
#endif
}
