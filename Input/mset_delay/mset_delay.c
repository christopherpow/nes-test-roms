//
// mset_delay.c
// Hyperkin Mouse read delay test, by Brad Smith 2019
//
// http://rainwarrior.ca
//

#define MOUSE_BIT_DELAY 3
#define MOUSE_DELAY_MIN (11 + MOUSE_BIT_DELAY)
#define MOUSE_DELAY_MAX 50
// make sure these match mset_delay.s

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

extern uint8 gamepad;
extern uint8 mouse_delay;
extern uint8 mouse0;
extern uint8 mouse1;
extern uint8 mouse2;
extern uint8 mouse3;
extern uint8 mouse4;
extern sint8 mouse_y;
extern sint8 mouse_x;
#pragma zpsym("gamepad")
#pragma zpsym("mouse_delay")
#pragma zpsym("mouse0")
#pragma zpsym("mouse1")
#pragma zpsym("mouse2")
#pragma zpsym("mouse3")
#pragma zpsym("mouse4")
#pragma zpsym("mouse_y")
#pragma zpsym("mouse_x")

extern uint8 sprite_chr[];
#define SPRITE_CHR_SIZE (16 * 3 * 16)

extern uint8 prng(); // 8-bit random value
extern void mouse_sense(); // cycles sensitivity setting (doesn't work on Hyperkin clone)
extern void input_setup();
extern void input_poll();
extern void sound_play(uint8* addr); // play a sound effect

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

#define PAD_A       0x80
#define PAD_B       0x40
#define PAD_SELECT  0x20
#define PAD_START   0x10
#define PAD_UP      0x08
#define PAD_DOWN    0x04
#define PAD_LEFT    0x02
#define PAD_RIGHT   0x01
#define MOUSE_L     0x40
#define MOUSE_R     0x80

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

sint8 mxl;
sint8 mxh;
sint8 myl;
sint8 myh;

void mlh_reset()
{
	mxl = 0;
	mxh = 0;
	myl = 0;
	myh = 0;
}

uint8 line = 0;
uint8 oam_pos = 0;

void add_sprite(uint8 x, uint8 y, uint8 tile)
{
	oam[oam_pos+0] = y-1;
	oam[oam_pos+1] = tile;
	oam[oam_pos+2] = 0;
	oam[oam_pos+3] = x;
	oam_pos += 4;
}

void add_hex_sprite(uint8 x, uint8 y, uint8 value)
{
	add_sprite(x,y,value>>4);
	add_sprite(x+8,y,value&0xF);
}

void add_dec_sprite(uint8 x, uint8 y, sint8 value)
{
	uint8 sign = (value < 0) ? 1 : 0;
	if (sign) value = 0 - value;

	/*
	// this was too slow
	add_sprite(x,y,value%10);
	value /= 10;
	x -= 8;
	while (value > 0)
	{
		add_sprite(x,y,value%10);
		value /= 10;
		x -= 8;
	}
	*/

	// this was fast enough
	i = (uint8)value;
	j = 0;
	if (i >= 100)
	{
		j=1;
		i-=100;
	}
	add_sprite(x,y,i%10); x -= 8;
	if (i >= 10 || j > 0)
	{
		i /= 10;
		add_sprite(x,y,i); x -= 8;
	}
	if (j > 0)
	{
		add_sprite(x,y,j); x -= 8;
	}
	
	if (sign) add_sprite(x,y,0x10);
}

uint8 mouse_last;
uint8 gamepad_last;
uint8 mouse_new;
uint8 gamepad_new;

void new_poll()
{
	input_poll();
	mouse_new = mouse1 & (mouse1 ^ mouse_last);
	mouse_last = mouse1;
	gamepad_new = gamepad & (gamepad ^ gamepad_last);
	gamepad_last = gamepad;
}

void test()
{
	cls();
	mlh_reset();
	
	for (i=0; i<32; i+=4)
	{
		palette[i+0] = 0x0F;
		palette[i+1] = 0x00;
		palette[i+2] = 0x10;
		palette[i+3] = 0x30;
	}
	palette[12+3] = 0x00; // grey background text

	while (1)
	{
		oam_pos = 0;

		add_sprite(125+mouse_x,117+mouse_y,0x27);
		add_sprite(125,117,0x24);
	
		add_sprite((2+10)*8,(2+line)*8,0x11);
		ppu_send_addr = 0x2000 + 2 + ((2+line) * 32);
		ppu_send_count = 10;
		ppu_send[0] = mouse0 >> 4;
		ppu_send[1] = mouse0 & 0xF;
		ppu_send[2] = mouse1 >> 4;
		ppu_send[3] = mouse1 & 0xF;
		ppu_send[4] = mouse2 >> 4;
		ppu_send[5] = mouse2 & 0xF;
		ppu_send[6] = mouse3 >> 4;
		ppu_send[7] = mouse3 & 0xF;
		ppu_send[8] = mouse4 >> 4;
		ppu_send[9] = mouse4 & 0xF;
		++line; if (line >= 24) line = 0;
		
		add_hex_sprite(20*8,2*8,mouse0);
		add_hex_sprite(22*8,3*8,mouse1);
		add_hex_sprite(24*8,3*8,mouse2);
		add_hex_sprite(26*8,3*8,mouse3);
		add_hex_sprite(28*8,2*8,mouse4);
		add_dec_sprite(20*8,4*8,mouse_delay);
		add_sprite(22*8,4*8,(mouse1&0x30)>>4);

		if (mouse_x < mxl) mxl = mouse_x;
		if (mouse_x > mxh) mxh = mouse_x;
		if (mouse_y < myl) myl = mouse_y;
		if (mouse_y > myh) myh = mouse_y;
		add_dec_sprite(29*8,5*8,mxl);
		add_dec_sprite(29*8,6*8,mxh);
		add_dec_sprite(29*8,8*8,myl);
		add_dec_sprite(29*8,9*8,myh);
		
		while (oam_pos != 0)
		{
			oam[oam_pos+0] = 0xFF;
			oam_pos += 4;
		}

		PROFILE();
		ppu_post(POST_UPDATE);
		new_poll();

		if (gamepad_new & PAD_B)
		{
			mlh_reset();
		}
		
		if (gamepad_new & PAD_A)
		{
			mouse_sense();
		}
		
		if (gamepad_new & PAD_DOWN)
		{
			--mouse_delay;
			if (mouse_delay < MOUSE_DELAY_MIN) mouse_delay = MOUSE_DELAY_MIN;
		}
		
		if (gamepad_new & PAD_UP)
		{
			++mouse_delay;
			if (mouse_delay > MOUSE_DELAY_MAX) mouse_delay = MOUSE_DELAY_MAX;
		}
	}
}

void main()
{
	input_setup();
	mouse_last = mouse1;
	gamepad_last = gamepad;

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
