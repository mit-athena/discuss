/*
 * Pager: Routines to create a "more" running out of a particular file
 * descriptor.
 */
#include <stdio.h>

int pager_create() 
{
	int filedes[2];
	int i;
	if(pipe(filedes) != 0) return(-1);

	switch(fork()) {
	case -1:
		return(-1);
	case 0:
		/*
		 * Child; dup read half to 0, close all but 0, 1, and 2
		 */
		(void) dup2(filedes[0], 0);
		for (i=3; i<32; i++)
			(void) close(i);
		(void) execlp("more", "more", (char *) NULL);
		exit(1);
	default:
		/*
		 * Parent:  close "read" side of pipe, return
		 * "write" side.
		 */
		(void) close(filedes[0]);
		return(filedes[1]);
	}
}
