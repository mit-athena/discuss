/*
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tfile.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tfile.c,v 1.3 1987-07-18 00:01:19 srz Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 * tfile.c -- a new implementation of tfile's.
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.2  87/04/11  00:06:21  srz
 * Added RCS junk
 * 
 *
 */
#ifndef lint
static char *rcsid_tfile_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tfile.c,v 1.3 1987-07-18 00:01:19 srz Exp $";
#endif lint

#include <stdio.h>
#include <errno.h>
#include "../include/tfile.h"
#include <sys/types.h>
#include <sys/stat.h>

#define min(A, B) ((A) < (B) ? (A) : (B))
#define NIL 0
#define SUCCESS 1

extern char *malloc ();
extern int errno;

tfile tcreate (tfs, infop, info, proc)
int tfs;
char *infop;
int info;
int (*proc)();
{
     tfile tf;

     tf = (tfile) malloc (sizeof (struct trecord));
     if (tf ==  NIL)
	  return (NIL);

     tf -> proc = proc;
     tf -> size = tfs;
     tf -> info = info;
     tf -> infop = infop;

     return (tf);
}

/*
 *
 * tfsize (tf)  -- return size of a file.
 *
 */
int tfsize (tf)
tfile tf;
{
     return (tf -> size);
}

topen(tf,mode,result)
tfile tf;
char *mode;
int *result;
{
     if (tf == NIL || tf -> proc == NIL) {
	  *result = EINVAL;
	  return (-1);
     }

     return ((*(tf -> proc)) (TFOPEN, &(tf -> infop), &(tf -> info), mode, 0, result));

}

tclose(tf,result)
tfile tf;
int *result;
{
     if (tf == NIL || tf -> proc == NIL) {
	  *result = EINVAL;
	  return (-1);
     }

     return ((*(tf -> proc)) (TFCLOSE, &(tf -> infop), &(tf -> info), 0, 0, result));
}

int tread(tf,bufp,wanted,result)
tfile tf;
char *bufp;
int wanted;
int *result;
{
     if (tf == NIL || tf -> proc == NIL) {
	  *result = EINVAL;
	  return (-1);
     }

     return ((*(tf -> proc)) (TFREAD, &(tf -> infop), &(tf -> info), bufp, wanted,result));
}

int twrite(tf,bufp,wanted,result)
tfile tf;
char *bufp;
int wanted;
int *result;
{
     if (tf == NIL || tf -> proc == NIL) {
	  *result = EINVAL;
	  return (-1);
     }

     return ((*(tf -> proc)) (TFWRITE, &(tf -> infop), &(tf -> info), bufp, wanted, result));
}

int tcontrol(tf,op,cinfop,result)
tfile tf;
int op;
char *cinfop;
int *result;
{
     if (tf == NIL || tf -> proc == NIL) {
	  *result = EINVAL;
	  return (-1);
     }

     return ((*(tf -> proc)) (TFCONTROL, &(tf -> infop), &(tf -> info), cinfop, op, result));
}

int tdestroy (tf)
tfile tf;
{
     int dummy;

     if (tf == NIL) {
	  return (-1);
     }

     if (tf -> proc != NIL)
	  (void) (*(tf -> proc)) (TFDESTROY, &(tf -> infop), &(tf -> info), 0, 0, &dummy);
     tf -> proc = NIL;
     free (tf);

     return(0);
}
