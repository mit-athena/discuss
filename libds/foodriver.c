#include <stdio.h>
#include "tfile.h"

char bfr[2048];
tfile bar;

main()
{
	int code;
	int count;    
	FILE *foo = fopen("/etc/termcap", "r");
	extern tfile pager_tfile();
	init_tfpager();
	
	bar = pager_tfile();

	topen(bar, 0, &code);
	while (1) {
		if (!(count=fread(bfr, 1, 2048, foo))) break;
		twrite(bar, bfr, count, &code);
		if (code) break;
	}
}
