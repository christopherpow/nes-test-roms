//
// kbmtest.c
// Keyboard Mouse Host test by Brad Smith 2024
// http://rainwarrior.ca
//

#include "temp/sprite.h" // sprite enums

//
// NES library
//

typedef unsigned char      uint8;
typedef   signed char      sint8;
typedef unsigned short int uint16;
typedef   signed short int sint16;
typedef unsigned long  int uint32;
typedef   signed long  int sint32;

extern uint8 nmi_count;    // every NMI increments this value
extern uint8 input[2];     // gamepad inputs
extern uint8 input_new[2]; // new buttons pressed at last poll
extern uint8 ascii_offset; // offset from CHR index to ASCII in ppu_string
extern uint8 i,j,k,l;      // convenient zeropage temporaries
extern uint16 mx,nx,ox,px; // 16-bit ZP temporaries
#pragma zpsym("nmi_count")
#pragma zpsym("input")
#pragma zpsym("input_new")
#pragma zpsym("ascii_offset")
#pragma zpsym("i")
#pragma zpsym("j")
#pragma zpsym("k")
#pragma zpsym("l")
#pragma zpsym("mx")
#pragma zpsym("nx")
#pragma zpsym("ox")
#pragma zpsym("px")

extern uint8 palette[32];  // NES render palette
extern uint8 ppu_send[72]; // data packets uploaded to PPU (see ppu_packet)
extern uint8 oam[256];     // sprite OAM data

// PPU updates while rendering is off
extern void ppu_latch(uint16 addr); // sets PPU write address
extern void ppu_latch_at(uint8 x, uint8 y); // latches a nametable tile (x>32=$2400, y>32=$2800, etc.)
extern void ppu_write(uint8 value); // writes one byte at latched address
extern void ppu_load(const uint8* data, uint16 count); // writes block of data
extern void ppu_fill(uint8 value, uint16 count); // writes repeating byte
extern void ppu_string(const char* s); // writes a string (set ascii_offset before using)

// PPU updates while rendering is on, during vblank
extern void ppu_packet(uint16 addr, const uint8* data, uint8 count);
extern void ppu_packet_apply(); // immediately apply packets while rendering is off
// Internal packet format, stored in ppu_send:
//   uint8: count (number of bytes to update), 0 = end of packets
//   uint16: address (PPU address for data)
//   uint8 * count: bytes of data

// PPU controls
extern void ppu_ctrl(uint8 v); // new value for $2000 at next render
extern void ppu_control_apply(uint8 v); // set immediately
extern void ppu_mask(uint8 v); // $2001 at next render
extern void ppu_mask_apply(uint8 v);
extern void ppu_scroll(uint16 x, uint16 y); // scroll at next render (note 240<=y<256 is "between" nametables)
extern void ppu_render_frame(); // send updates to renderer and wait for next frame
extern void ppu_wait_frame(); // wait 1 frame without applying updates
extern void ppu_render_off(); // turn renderer off
extern void ppu_nmi_off(); // disable NMI
extern void ppu_nmi_on();

// Sprites
extern void sprite_begin(); // begin a new sprite list
extern void sprite_end(); // conclude the sprite list
extern void sprite_tile(uint8 t, uint8 a, uint8 x, uint8 y); // add a single tile
extern void sprite(uint8 e, uint8 x, uint8 y); // add sprite "e" at x/y
extern void sprite_flip(uint8 e, uint8 x, uint8 y); // sprite with horizontal flip

void nescopy(void* dst, const void* src, uint8 count); // copies bytes (memcpy replacement), but count is <256

extern void input_poll(); // poll the keyboard/mouse into input array

#define PAD_A       0x80
#define PAD_B       0x40
#define PAD_SELECT  0x20
#define PAD_START   0x10
#define PAD_U       0x08
#define PAD_D       0x04
#define PAD_L       0x02
#define PAD_R       0x01

#define NMTADDR(x_,y_) (0x2000+(x_)+((y_)*32))

//
// KBM Test
//

// palettes
const uint8 pal[32] = {
	0x0F, 0x00, 0x10, 0x3A, // dark grey, grey, pale green
	0x0F, 0x17, 0x10, 0x2C, // orange, grey, cyan
	0x0F, 0x01, 0x11, 0x21, // (filler blue)
	0x0F, 0x02, 0x12, 0x22, // (filler purple)
	0x0F, 0x26, 0x2A, 0x30, // red, green, white
	0x0F, 0x05, 0x15, 0x25, // (filler magenta)
	0x0F, 0x0B, 0x1B, 0x2B, // (filler blue-green)
	0x0F, 0x08, 0x18, 0x28, // (filler yellow)
};

uint8 mousex = 128;
uint8 mousey = 120;
uint8 mousew = 0;

#define KEYXMIN 4
#define KEYXMAX 28
#define KEYYMIN 10
#define KEYYMAX 26
uint8 keyx = KEYXMIN;
uint8 keyy = KEYYMIN;

const uint8 HEX[16] = { 0x27,0x1E,0x1F,0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x04,0x05,0x06,0x07,0x08,0x09 };
uint8 hextemp[4];
void update_hex8(uint16 addr, uint8 v)
{
	hextemp[0] = HEX[(v>> 4)     ];
	hextemp[1] = HEX[(v    )&0x0F];
	ppu_packet(addr,hextemp,2);
}
void update_hex16(uint16 addr, uint16 v)
{
	hextemp[0] = HEX[(v>>12)     ];
	hextemp[1] = HEX[(v>> 8)&0x0F];
	hextemp[2] = HEX[(v>> 4)&0x0F];
	hextemp[3] = HEX[(v    )&0x0F];
	ppu_packet(addr,hextemp,4);
}
void update_char(uint16 addr, uint8 v)
{
	v -= ascii_offset;
	ppu_packet(addr,&v,1);
}

// main loop
void main()
{
	ascii_offset = 'A'-0x04; // 'A' maps to $04
	nescopy(palette,pal,32);

	ppu_latch(0x2000); ppu_fill(0, 1024);
	ppu_latch_at(4,4); ppu_string("KBMTEST");
	ppu_latch_at(4,6); ppu_string("PORT");
	ppu_latch_at(4,7); ppu_string("MSE");
	ppu_latch_at(4,8); ppu_string("KEY");
	// secondary colours for key log
	ppu_latch(0x23D0); ppu_fill(0x50,0x08);
	ppu_latch(0x23D8); ppu_fill(0x55,0x28);

	while(1)
	{
		input_poll();
		sprite_begin();

		// l = port
		l = 0xFF;
		if      ((input[ 4] & 0x07) == 0x06) l = 0; // $4016
		else if ((input[12] & 0x07) == 0x06) l = 8; // $4017
		update_hex16(NMTADDR(9,6),(l<128)?(0x4016+(l/8)):0);
		
		if (l>=128) // no device, clear the input
		{
			l = 0;
			for (i=0;i<8;++i) input[i] = 0;
		}

		if ((input[4+l] & 0x20)) // mouse connected
		{
			if (input[4+l] & 0x08) // relative motion
			{
				static sint8 dx,dy;
				static sint16 nx,ny;
				dx = input[5+l];
				dy = input[6+l];
				nx = (sint16)mousex + dx;
				ny = (sint16)mousey + dy;
				if (nx < 0) nx = 0;
				if (nx > 255) nx = 255;
				if (ny < 0) ny = 0;
				if (ny > 240) ny = 240;
				mousex = (uint8)nx;
				mousey = (uint8)ny;
			}
			else // absolute motion
			{
				mousex = input[5+l];
				mousey = input[6+l];
			}
			
			i = input[7+l] & 0x78; // mouse wheel
			if (i)
			{
				i >>= 3;
				if (i & 0x08) i |= 0xF0; // extend sign
				mousew += i;
			}

			update_char(NMTADDR( 9,7),(input[4+l] & 0x80)?'L':('L'+0x80));
			update_char(NMTADDR(10,7),(input[7+l] & 0x80)?'M':('M'+0x80));
			update_char(NMTADDR(11,7),(input[4+l] & 0x40)?'R':('R'+0x80));
			update_hex8(NMTADDR(13,7),mousex);
			update_hex8(NMTADDR(16,7),mousey);
			update_hex8(NMTADDR(19,7),mousew);
			sprite(SPRITE_mouse,mousex,mousey);
		}

		if ((input[4+l] & 0x10)) // keyboard connected
		{
			for (i=0; i<4; ++i)
			{
				j = input[0+i+l];
				if (!j) break;
				ppu_packet(NMTADDR(keyx,keyy),&j,1);
				++keyx;
				if (keyx >= KEYXMAX)
				{
					keyx = KEYXMIN;
					++keyy;
					if (keyy >= KEYYMAX) keyy = KEYYMIN;
				}
			}
			j = 0;
			ppu_packet(NMTADDR(keyx,keyy),&j,1); // blank tile to mark next press
		}

		sprite((input[4+l] & 0x20) ? SPRITE_on : SPRITE_off,7*8,7*8); // mouse connected
		sprite((input[4+l] & 0x10) ? SPRITE_on : SPRITE_off,7*8,8*8); // keyboard connected

		sprite_end();
		ppu_render_frame();
	}
}
