/*
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tunix.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tunix.c,v 1.2 1987-04-11 00:06:29 srz Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 * tunix.c -- procedures to have tfiles work from unix files.
 *
 *	$Log: not supported by cvs2svn $
 *
 */
#ifndef lint
static char *rcsid_tunix_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tunix.c,v 1.2 1987-04-11 00:06:29 srz Exp $";
#endif lint

#define min(A, B) ((A) < (B) ? (A) : (B))
#define NIL 0

#define SUCCESS 1

#include <stdio.h>
#include "../include/tfile.h"
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>


extern int errno;

/*
 *
 * tunix () -- This is the handler procedure, that handles tfile requests.
 *
 */
int tunix (op, infop, info, argp, argn, result)
int op, *info, argn, *result;
char **infop, *argp;
{
     int numread,numwrite;

     *result = 0;		/* optimist */

     switch (op) {
     case TFOPEN:		/* argp is pointer to modes */
          return (0);

     case TFCLOSE:
          return (0);

     case TFREAD:
	  numread = read (*info, argp, argn);
	  if (numread < 0) {
	       *result = errno;
	       return (-1);
	  }
	  return(numread);

     case TFWRITE:
	  numwrite = write (*info, argp, argn);
	  if (numwrite < 0) {
	       *result = errno;
	       return (-1);
	  }
	  return(numwrite);

     case TFDESTROY:
	  return (0);

     default:
	  *result = EBADF;
	  return (-1);
     }
}

/*
 *
 * unix_tfile (tfs, us)
 *
 */
tfile unix_tfile (desc)
int desc;
{
     struct stat buf;

     if (fstat (desc, &buf) < 0)
	  return (NIL);

     return (tcreate (buf.st_size, 0, desc, tunix));
}
