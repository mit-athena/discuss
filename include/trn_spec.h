/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/trn_spec.h,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/trn_spec.h,v 1.2 1989-06-03 00:07:26 srz Exp $
 *
 *	Transaction specifiers, and things which use them.
 */

/*
 * Flags:
 */
#define TSPEC_ONLY_ONE (0x0001) /* If set, only one message */
#define TSPEC_DEFAULT_ALL (0x0002) /* If empty, default to all
				    * messages */
#define TSPEC_DEFAULT_CUR (0x0004) /* If empty, default to current
				    * message */

typedef struct _tgen {
	trn_nums (*nextfn)();	/* Generator function */
	trn_info *tinfo;	/* Transaction info   */
	mtg_info *minfo;	/* Meeting info       */
	int	flags;		/* Flags 	      */
	struct _tgen *next;	/* Next one */
} trn_gen;


