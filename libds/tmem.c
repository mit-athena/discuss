/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tmem.c,v $
 *	$Author: miki $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tmem.c,v 1.4 1994-03-25 16:45:04 miki Exp $
 *
 *	tfile module for ``memory'' tfiles. 
 * 
 *	$Log: not supported by cvs2svn $
 * Revision 1.3  89/06/03  00:22:16  srz
 * Added standard copyright notice.
 * 
 * Revision 1.2  89/06/03  00:13:07  srz
 * Ken's changes.
 * 
 * Revision 1.1  88/03/11  00:05:45  wesommer
 * Initial revision
 * 
 */

#ifndef lint
static char rcsid_tmem_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/tmem.c,v 1.4 1994-03-25 16:45:04 miki Exp $";
#endif lint

#include <errno.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <discuss/tfile.h>

#define min(x,y) ((x)<(y)?(x):(y))

enum iovdir { SCATTER, GATHER };
	
static int iovmove (direct, iovpp, buf, len)
	register enum iovdir direct;
	struct iovec **iovpp;
	register char *buf;
	register int len;	
{
	register int moved = 0;
	register int count;
	register struct iovec *iovp;
	
	iovp = *iovpp;
	while (len > 0) {
		count = min (iovp->iov_len, len);
		if (direct == SCATTER) 
#ifdef POSIX
			memmove (iovp->iov_base, buf, count);
#else
			bcopy (buf, iovp->iov_base, count);
#endif
		else
#ifdef POSIX
			memmove (buf, iovp->iov_base,  count);
#else
			bcopy (iovp->iov_base, buf, count);
#endif
		len -= count;
		moved += count;
		buf += count;
		iovp->iov_base += count;
		iovp->iov_len -= count;
		if (iovp->iov_len == 0) {
			iovp ++;
			if (iovp->iov_len == 0)
				break;
		}
	}
	*iovpp = iovp;
	return moved;
}

static int tmem(op, infop, info, argp, argn, result)
	int op, argn;
	register int *result, *info;
	char **infop, *argp;
{
	*result = 0;		/* optimist */
	switch (op) {
	case TFOPEN:
	case TFCLOSE:
		return 0;
	case TFREAD:
		return iovmove (GATHER, (struct iovec **)infop,
				argp, argn);
	case TFWRITE:
		return iovmove (SCATTER, (struct iovec **)infop,
				argp, argn);
	case TFDESTROY:
		if (*(char **)info)
			free (*(char **)info);
		return 0;
	default:
		*result = EINVAL;
		return -1;
	}
}

tfile mem_tfile (buffer, length)
	char *buffer;
	int length;
{
	register struct iovec *ts =
		(struct iovec *) malloc (2 * sizeof (struct iovec));
	ts[0].iov_base = buffer;
	ts[0].iov_len = length;
	ts[1].iov_len = 0;
	return tcreate (length, (char *) ts, (int)ts, tmem);
}

tfile memv_tfile (vec)
	register struct iovec *vec;
{
	register int i, len;
	for (i=0, len = 0; vec[i].iov_len; i++)
		len += vec[i].iov_len;
		
	return tcreate (len, (char *) vec, 0, tmem);
}
