/*
 *
 * tunix.c -- procedures to have tfiles work from unix files.
 *
 */

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
