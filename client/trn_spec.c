/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/trn_spec.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/trn_spec.c,v 1.3 1991-09-04 11:29:35 lwvanels Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 *	Parse a command line containing transaction specifiers and 
 * 	"standard" message selection control arguments.
 *
 *	parse_trans_spec "compiles" the command line arguments into 
 *	a "generator" data structure.  
 *
 *	The generator strucuture contains things like a trn_info for each
 *	generated transaction.
 *
 * 	It is indended that the application will call "tg_next_trn" repeatedly
 *	until it returns a non-zero error code.  "tg_next_trn" returns 
 *	DSC_NO_MORE when there are no more transactions in a series, and
 *	silently swallows things like deleted transactions (unless the 
 *	-deleted option was given...)
 *
 *	It is intended that this will be the place to do things like 
 *	matching subjects based on a regexp, implementing "-by_chain", etc,
 *	although some significant rearrangement of the FSMs will be needed
 * 	to do that.
 *
 *	If this were LISP, I would wind up turning tg_next_trn into something
 *	which called eval..  but it isn't.  Sorry.
 *
 * 	$Log: not supported by cvs2svn $
 * Revision 1.2  87/07/08  19:06:42  wesommer
 * Another intermediate version.
 * 
 * Revision 1.1  87/04/10  23:42:37  srz
 * Initial revision
 *
 */

#ifndef lint
static char *rcsid_trn_spec_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/trn_spec.c,v 1.3 1991-09-04 11:29:35 lwvanels Exp $";
#endif lint

#include "interface.h"
#include "globals.h"
#include "trn_spec.h"
#include "discuss_err.h"
#include "dsc_et.h"
#include <stdio.h>
#include <strings.h>
#include <ctype.h>

#define max(a,b) ((a)<(b)?(b):(a))

/*
 * Table of legal transaction specs.  Entries must be sorted in ASCII
 * order.
 */

static struct trn_ent {	
	char *name;
	int first, second;
} trn_table[] = {
	{ ".", 		TRSPEC_CUR|TRSPEC_REM, 0 },
	{ "cur", 	TRSPEC_CUR|TRSPEC_REM, 0 },
	{ "n",	 	TRSPEC_NEXT|TRSPEC_REM, 0 },
	{ "next", 	TRSPEC_NEXT|TRSPEC_REM, 0 },
	{ "nr",		TRSPEC_NREF|TRSPEC_REM, 0 },
	{ "nref",	TRSPEC_NREF|TRSPEC_REM, 0 },
	{ "p", 		TRSPEC_PREV|TRSPEC_REM, 0 },
	{ "pr",		TRSPEC_PREF|TRSPEC_REM, 0 },
	{ "pref",	TRSPEC_PREF|TRSPEC_REM, 0 },
	{ "prev", 	TRSPEC_PREV|TRSPEC_REM, 0 },
	{ "all",	TRSPEC_FIRST|TRSPEC_REM, TRSPEC_NEXT },
	{ "aref",	TRSPEC_FREF|TRSPEC_REM, TRSPEC_NREF },
	{ "first",	TRSPEC_FIRST|TRSPEC_REM, 0 },
	{ "last",	TRSPEC_LAST|TRSPEC_REM, 0},
	{ "$",		TRSPEC_LAST|TRSPEC_REM, 0},
	{ "new",	TRSPEC_FUNS|TRSPEC_REM, TRSPEC_NEXT },
};
int n_trn_table = (sizeof(trn_table)/sizeof(struct trn_ent));
	
static struct swtch {
	char *name;
	int	set;
} switches[] = {
	{ "-deleted", 	TRSPEC_DEL },
};

int n_switches = (sizeof(switches)/sizeof(struct swtch));

/*
 * Parse a transaction specifier.
 * Returns a "generator" for the transactions specified.
 * Modifies (reallocs) argv to contain only the arguments it does 
 * not understand.
 *
 * Returns a standard error code.
 */
int
parse_trans_spec(argv_p, argc_p, environ, dfault, gen)
	char ***argv_p;	/* Pointer to array of (char *) */
	int *argc_p;	/* Arg count (modified) */
	struct _dsc_pub *environ;
	char *dfault;
	trans_gen **gen; /* Return */
{
	char **argv = *argv_p;
	char **nargv;
	int nargc = 0;
	int argc = *argc_p;
	register char **argp = argv; 	/* induction variable */
	int code = 0;		/* return error code */
	trans_gen *tg;
	
	if (!environ->attending)
		return DISC_NO_MTG;
	
	nargv = (char **)malloc(0);
	tg = (trans_gen *)malloc(sizeof(*tg));
	
	tg->flags = 0;
	tg->minfo = &environ->m_info;
	tg->nbp = &environ->nb;
	tg->te_last = tg->te = (trnspec_entry *)tg;
	tg->current = environ->current;
	tg->highest_seen = environ->highest_seen;
	
	--argc;
	++argp; 
	if (argc == 0) process_an_arg(tg, dfault);

	for ( ; argc; --argc, ++argp) {
		if(process_an_arg(tg, *argp)) {
			nargv = (char **) realloc(nargv, nargc + 1 * (sizeof(char *)));
			nargv[nargc] = *argp;
			nargc++;
		} else free(*argp);
		*argp = NULL;
	}
	free (*argv_p);
	
	*argv_p = nargv;
	*argc_p = nargc;
	*gen = tg;
	
	dsc_get_mtg_info(tg->nbp, tg->minfo, &code);
	if (code) return code;
	if (environ->current == 0) return 0;
	dsc_get_trn_info(tg->nbp, environ->current, &tg->tinfo, &code);
	return code;
}
int
process_an_arg(tg, arg)
	trans_gen *tg;
	char *arg;
{
	int i, val, retval;
	char *cp;

	if ((cp = index(arg, ',')) ||
	    (cp = index(arg, ':'))) {
	        *cp = '\0';
		retval = process_an_arg(tg, arg);
		if (!retval) {
			retval = process_an_arg(tg, cp+1);
			if (!retval) {
				tg->te_last->flags &= ~TRSPEC_REM;
				tg->te_last->flags |= TRSPEC_UNTIL;
			}
		}
		*cp = ',';
		return 0;
	}

	{
		register char c;
		int val = 0;
		trnspec_entry *new_te;
		for (cp = arg; c = *cp; cp++) {
			if (!isdigit(c)) goto not_number;
			val *= 10;
			val += (c - '0');
		}
		add_new_te(tg, TRSPEC_NUM|TRSPEC_REM, val);
		return 0;
        }

not_number:
	for (i = 0; i < n_trn_table; i++) {
		if(!strcmp(arg, trn_table[i].name)) {
			add_new_te(tg, trn_table[i].first, 0);
			if (trn_table[i].second) {
				add_new_te(tg, trn_table[i].second, 0);
			}
			return 0;
		}
	}
	for (i = 0; i < n_switches; i++) {
		if(!strcmp(arg, switches[i].name)) {
			tg->flags |= switches[i].set;
			return 0;
		}
	}
	return 1;
} 

int
tg_next_trn(tg)
	trans_gen *tg;
{
	register trnspec_entry *te;
	int deleted, remove, until, flags, trn_num, unseen;
	int code = 0;
	
	/* Come up with next xcn number */
	if(tg->te == (trnspec_entry *)tg) return DSC_NO_MORE;
	
	te=tg->te;
	
	trn_num = tg_next_trn_num(tg, te);

	if (trn_num == 0) return DSC_NO_MORE;

	if (trn_num != tg->current)
		dsc_get_trn_info(tg->nbp, trn_num, &tg->tinfo, &code);

	if (tg->flags&TRSPEC_DEL) {
		if (code != DELETED_TRN) {
/* arg! */
			deleted = -1;
			flags = TRSPEC_NEXT;
			goto again;
/* end arg! */
		} else code = 0;
	}

	if (code == 0) {
		tg->current = trn_num;
	}

	if(te->flags&TRSPEC_REM) {
		remque(te);
		te_free(te);
	}

	if (code) tg->current = 0;
	return code;
} 
	
add_new_te(tg, flags, num)
    	trans_gen *tg;
     	int flags;
	int num;
{
	trnspec_entry *new_te = (trnspec_entry *)malloc(sizeof(*new_te));
	new_te->flags = flags;
	new_te->num = num;
	insque(new_te, tg->te_last);
}

/*
 * A Lisp programmer would call this function a special case of eval..
 */

int tg_next_trn_num(tg, te)
	trans_gen *tg;
	trnspec_entry *te;
{
	int trn_num;

	switch (te->flags&TRSPEC_KINDS) {
	case TRSPEC_UNTIL:
		trn_num = tg_next_trn_num(tg, te->down);
		if (trn_num >= tg->current) return 0;
		/* fall through into next... */

	case TRSPEC_NEXT:
		if (tg->flags&TRSPEC_DEL) return tg->current + 1;
		else return tg->tinfo.next;
		
	case TRSPEC_PREV:
		if (tg->flags&TRSPEC_DEL) return tg->tinfo.current - 1;
		else return tg->tinfo.prev;
		
	case TRSPEC_NREF:
		return tg->tinfo.nref;
		
	case TRSPEC_PREF:
		return tg->tinfo.pref;

	case TRSPEC_FREF:
		return tg->tinfo.fref;
		
	case TRSPEC_LREF:
		return tg->tinfo.lref;

	case TRSPEC_FIRST:
		return tg->minfo->first;

	case TRSPEC_LAST:
		return tg->minfo->last;

	case TRSPEC_CUR:
		return te->tinfo.current;

	case TRSPEC_NUM:
		return te->num;

	case TRSPEC_FUNS:	
		return tg->highest_seen + 1;

	default:
		abort();
	}
}