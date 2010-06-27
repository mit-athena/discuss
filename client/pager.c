/*
 * Pager: Routines to create a pager running out of a particular file
 * descriptor.
 */
#ifndef lint
static char *rcsid_discuss_c = "$Id: pager.c,v 1.4 1999-02-08 14:46:52 danw Exp $";
#endif /* lint */

#include <stdio.h>

int pager_create() 
{
	int filedes[2];
	int i;
	const char *pager;
	if (!(pager = getenv("PAGER")))
		pager = PAGER;

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
		(void) execlp("/bin/sh", "/bin/sh", "-c", pager, (char *) NULL);
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
