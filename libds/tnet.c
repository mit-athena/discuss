/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tnet.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tnet.c,v 1.5 1989-06-03 00:22:24 srz Exp $
 *
 * tnet.c -- procedures to have tfiles go over the net.
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.4  89/01/04  20:49:11  raeburn
 * fixed include refs
 * 
 * Revision 1.3  87/07/17  03:11:26  spook
 * *** empty log message ***
 * 
 * Revision 1.2  87/04/11  00:06:25  srz
 * Added RCS junk
 * 
 *
 */
#ifndef lint
static char rcsid_tnet_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tnet.c,v 1.5 1989-06-03 00:22:24 srz Exp $";
#endif lint

#define min(A, B) ((A) < (B) ? (A) : (B))
#define NIL 0

#define SUCCESS 1
#define TFILE_BLK 500

#include <stdio.h>
#include "usp.h"
#include <discuss/tfile.h>
#include <errno.h>


extern int errno;

/*
 *
 * tnet () -- This is the handler procedure, that handles tfile requests.
 *
 */
int tnet (op, infop, info, argp, argn, result)
int op, *info, argn, *result;
char **infop, *argp;
{
     USPStream *us;
     USPCardinal bt;
     unsigned numread;

     *result = 0;		/* optimist */
     us = (USPStream *) *infop;

     switch (op) {
     case TFOPEN:		/* argp is pointer to modes */
	  if (*argp == 'r') {	/* reading */
	       if (USP_rcv_blk(us, &bt) != SUCCESS) {
		    *result = errno;
		    return (-1);
	       }
	       if (bt != TFILE_BLK) {
		    *result = EBADF;
		    return (-1);
	       }
	       *info = 1;
	  } else {
	       if (USP_begin_block (us, TFILE_BLK) != SUCCESS) {
		    *result = errno;
		    return (-1);
	       }
	       *info = -1;
	  }	
          return (0);

     case TFCLOSE:
	  if (*info > 0)
	       USP_flush_block(us);
	  else
	       USP_end_block(us);
          return (0);

     case TFREAD:
	  if (USP_get_byte_block(us, argp, argn, &numread) != SUCCESS) {
	       *result = errno;
	       return (-1);
	  }
	  return(numread);

     case TFWRITE:
	  if (USP_put_byte_block(us, argp, argn) != SUCCESS) {
	       *result = errno;
	       return (-1);
	  }
	  return (argn);

     case TFDESTROY:
	  return (0);

     default:
	  *result = EBADF;
	  return (-1);
     }
}

/*
 *
 * net_tfile (tfs, us)
 *
 */
tfile net_tfile (tfs, us)
int tfs;
USPStream *us;
{
     return (tcreate (tfs, (char *)us, 0, tnet));
}
