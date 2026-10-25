//
// mict.c
// Famicom Microphone Test, by Brad Smith 2019
// http://rainwarrior.ca
//

// for debugging performance
//#define PROFILE() ppu_profile(0x41);
#define PROFILE() {}

typedef unsigned char      uint8;
typedef   signed char      sint8;
typedef unsigned short int uint16;
typedef   signed short int sint16;
typedef unsigned long  int uint32;
typedef   signed long  int sint32;

extern uint8* ptr;
extern uint8 i, j, k, l;
extern uint16 ix, jx, kx, lx;
extern uint32 eix, ejx, ekx, elx;
#pragma zpsym("ptr")
#pragma zpsym("i")
#pragma zpsym("j")
#pragma zpsym("k")
#pragma zpsym("l")
#pragma zpsym("ix")
#pragma zpsym("jx")
#pragma zpsym("kx")
#pragma zpsym("lx")
#pragma zpsym("eix")
#pragma zpsym("ejx")
#pragma zpsym("ekx")
#pragma zpsym("elx")

extern uint8 sprite_chr[];
#define SPRITE_CHR_SIZE (16 * 2 * 16)

extern uint8 read_4016();

extern void ppu_latch(uint16 addr); // write address to PPU latch
extern void ppu_direction(uint8 vertical); // set write increment direction
extern void ppu_write(uint8 value); // write value to $2007
extern void ppu_load(uint16 count); // uploads bytes from ptr to $2007 (clobbers ptr)
extern void ppu_fill(uint8 value, uint16 count); // uploads single value to $2007
extern void ppu_ctrl(uint8 v); // $2000, only bits 4-6 count (tile pages, sprite height), applies at next post
extern void ppu_mask(uint8 v); // $2001, applies at next post
extern void ppu_scroll_x(uint16 x);
extern void ppu_scroll_y(uint16 y);
extern void ppu_post(uint8 mode); // waits for next frame and posts PPU update
extern void ppu_profile(uint8 emphasis); // immediate $2001 write, OR with current mask (use bit 0 for greyscale)

// POST_OFF     turn off rendering
// POST_NONE    turn on, no other updates
// POST_UPDATE  turn on, palette, send
// POST_DOUBLE  turn on, palette, send 64 bytes across 2 nametables
#define POST_OFF    1
#define POST_NONE   2
#define POST_UPDATE 3
#define POST_DOUBLE 4

extern uint16 ppu_send_addr;
extern uint8 ppu_send_count;
extern uint8 ppu_send[64];

extern uint8 palette[32];
extern uint8 oam[256];

void cls() // erase nametables
{
	ppu_latch(0x2000);
	ppu_fill(0xFF,0x1000);
}

uint8 out_column;
uint8 out_row;
uint8 out_or;

void test()
{
	cls();
	
	for (i=0; i<32; i+=4)
	{
		palette[i+0] = 0x0F;
		palette[i+1] = 0x21;
		palette[i+2] = 0x10;
		palette[i+3] = 0x27;
	}

	while (1)
	{
		ppu_send_addr = 0x2000 + (8 + out_column) + ((7 + out_row) * 32);
		ppu_send_count = 1;
		ppu_send[0] = ((read_4016() & 0x04) >> 2) | out_or;

		++out_column;
		if (out_column >= 16)
		{
			out_column = 0;
			++out_row;
			if (out_row >= 16)
			{
				out_row = 0;
				out_or ^= 0x10; // flip output colour
			}
		}

		PROFILE();
		ppu_post(POST_UPDATE);
	}
}

void main()
{
	ppu_latch(0x0000);
	ppu_fill(0x00,8*1024);

	ptr = sprite_chr;
	ppu_latch(0x0000);
	ppu_load(SPRITE_CHR_SIZE);

	ppu_ctrl(0x00);

	ppu_scroll_x(0);
	ppu_scroll_y(0);
	test();

	return;
}
