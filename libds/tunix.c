/*
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tunix.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tunix.c,v 1.4 1989-01-04 20:47:00 raeburn Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 * tunix.c -- procedures to have tfiles work from unix files.
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.3  87/07/18  00:01:38  srz
 * Added control operation to tfile's.  First control operation:  FORCE_NL,
 * which forces a NL onto a tfile if it isn't already there.
 * 
 * Revision 1.2  87/04/11  00:06:29  srz
 * Added RCS junk
 * 
 *
 */
#ifndef lint
static char rcsid_tunix_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tunix.c,v 1.4 1989-01-04 20:47:00 raeburn Exp $";
#endif lint

#define min(A, B) ((A) < (B) ? (A) : (B))
#define NIL 0

#define SUCCESS 1

#include <stdio.h>
#include <discuss/tfile.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#define NL '\n'

char *malloc();

extern int errno;

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

     return (tcreate (buf.st_size, (char *) ts, desc, tunix));
}
