/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 *	$Id: tfile.c,v 1.6 1999-01-22 23:10:01 ghudson Exp $
 *
 * tfile.c -- a new implementation of tfile's.
 *
 *
 */
#ifndef lint
static char rcsid_tfile_c[] =
    "$Id: tfile.c,v 1.6 1999-01-22 23:10:01 ghudson Exp $";
#endif lint

#include <stdio.h>
#include <errno.h>
#include <discuss/tfile.h>
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

     tf = (tfile) malloc (sizeof (struct _tfile));
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
