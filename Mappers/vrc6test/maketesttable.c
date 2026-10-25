#include <stdio.h>
#include <stdlib.h>

/*
128 tests:
chr banks 00,02,04,06,08,0a,0c,0e
  test 64 different b003 values
chr banks 01,03,05,07,09,0b,0d,0f
  test 64 different b003 values
  
chr banks 00:0f are byte filled with their bank number.
NTA is byte filled with $80
NTB is byte filled with $81
the two nametables were filled using one of the 'standard' VRC6 modes
that commercial games used, so they're probably right

each test has 16 bytes that it expects to read throughout each of the 16 PPU 1K banks,
excepting the palette area, of course
*/

char bankvals0[] = {0, 2, 4, 6, 8, 10, 12, 14};
char bankvals1[] = {1, 3, 5, 7, 9, 11, 13, 15};

char ptables[] =
		{
			0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,
			0x80,0xc0,0x81,0xc1,0x82,0xc2,0x83,0xc3,
			0x00,0x01,0x02,0x03,0x84,0xc4,0x85,0xc5,	
		};

char getbankval(int banknum, char *banks, int b003)
{
	if (banknum < 8) // pattern tables
	{
		int ptidx = b003 & 3;
		if (ptidx == 3) ptidx--;
		char pt = ptables[ptidx * 8 + banknum];
		char bankval = banks[pt & 7];

		if (b003 & 0x20) // replace A10
		{
			if (pt & 0x80) // this is a bank that gets a replacement
			{
				bankval &= 0xfe;
				if (pt & 0x40)
					bankval |= 1;
			}
		}
		return bankval;
	}
	else // name tables
	{
		banknum &= 3;
		int bankreg;
		switch (b003 & 7)
		{
			case 0:
			case 6:
			case 7: // h-mirror, 6677
				bankreg = banknum >> 1 | 6;
				break;
			case 2:
			case 3:
			case 4: // v-mirror, 6767
				bankreg = banknum | 6;
				break;
			case 1:
			case 5: // 4-screen, 4567
				bankreg = banknum | 4;
				break;
		}
		char bankval = banks[bankreg];
		if (b003 & 0x20) // replace A10
		{
			switch (b003 & 0x0f)
			{
				case 0:
				case 7: // v mirror
					bankval &= 0xfe;
					bankval |= banknum & 1;
					break;
				case 3:
				case 4: // h-mirror
					bankval &= 0xfe;
					bankval |= banknum >> 1;
					break;
				case 8:
				case 15: // 1sca
					bankval &= 0xfe;
					break;
				case 11:
				case 12: // 1scb
					bankval |= 1;
					break;
			}
		}
		if (!(b003 & 0x10)) // nametables are from chrram
		{
			bankval &= 1;
			bankval |= 0x80; // we mark the two NTram banks as 0x80, 0x81
		}
		return bankval;
	}
}

void runtest(char *banks, int b003, FILE *f)
{
	char c;
	int banknum;
	for (banknum = 0; banknum < 16; banknum++)
	{
		c = getbankval(banknum, banks, b003);
		fwrite(&c, 1, 1, f);
	}
}

void runalltests(char *banks, FILE *f)
{
	int b003;
	for (b003 = 0; b003 < 64; b003++)
	{
		runtest(banks, b003, f);
	}
}

int main(void)
{
	FILE *f = fopen("testtable", "wb");
	runalltests(bankvals0, f);
	runalltests(bankvals1, f);
	
	fclose(f);
	return 0;
}
