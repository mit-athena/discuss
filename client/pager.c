/*
 * Pager: Routines to create a "more" running out of a particular file
 * descriptor.
 */
#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/pager.c,v 1.2 1987-04-10 23:50:21 srz Exp $";
#endif lint

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
