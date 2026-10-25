#include <stdio.h>
#include <stdlib.h>

int main(void)
{
	int i;
	char bank;
	FILE *f = fopen("chr0", "wb");
	for (bank = 0; bank < 16; bank++)
	{
		for (i = 0; i < 1024; i++)
		{
			fwrite(&bank, 1, 1, f);
		}
	}
	fclose(f);
	return 0;
}
