/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 *	$Id: tunix.c,v 1.9 2006-03-10 07:11:38 ghudson Exp $
 *
 * tunix.c -- procedures to have tfiles work from unix files.
 *
 *
 */
#ifndef lint
static char rcsid_tunix_c[] =
    "$Id: tunix.c,v 1.9 2006-03-10 07:11:38 ghudson Exp $";
#endif /* lint */

#define min(A, B) ((A) < (B) ? (A) : (B))
#define NIL 0

#define SUCCESS 1

#include <stdio.h>
#include <stdlib.h>
#include <discuss/tfile.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#define NL '\n'

struct tunix_state {
     char last_char;
};

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
     struct tunix_state *ts;

     *result = 0;		/* optimist */
     ts = (struct tunix_state *) *infop;

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
	  /* save last character written to file, so we can force NL */
	  ts -> last_char = argp [numwrite-1];
	  return(numwrite);

     case TFDESTROY:
	  free (*infop);
	  return (0);

     case TFCONTROL:
	  if (argn == TFC_FORCE_NL) {			/* force a NL at this point */
	       if (ts -> last_char != NL) {
		    ts -> last_char = NL;
		    write (*info, &(ts -> last_char), 1);
	       }
	  }
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
     struct tunix_state *ts;

     if (fstat (desc, &buf) < 0)
	  return (NIL);

     ts = (struct tunix_state *) malloc (sizeof (struct tunix_state));
     ts -> last_char = 0;

     return (tcreate ((int) buf.st_size, (char *) ts, desc, tunix));
}
