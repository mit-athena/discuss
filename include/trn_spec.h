/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Id: trn_spec.h,v 1.3 1999-01-22 23:09:48 ghudson Exp $
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


