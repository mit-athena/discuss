/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/trn_spec.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/trn_spec.c,v 1.2 1987-07-08 19:06:42 wesommer Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 *	A new routine to parse transaction specifiers.  This one 
 *	has a slightly different interface from the previous one, and
 *	was inspired by forum_trans_specs_.pl1 in the Multics forum 
 * 	system, with slight variations.
 * 	
 *	This is not working code yet.
 *
 * 	$Log: not supported by cvs2svn $
 * Revision 1.1  87/04/10  23:42:37  srz
 * Initial revision
 * 
 */

#ifndef lint
static char *rcsid_trn_spec_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/trn_spec.c,v 1.2 1987-07-08 19:06:42 wesommer Exp $";
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
};
int n_trn_table = (sizeof(trn_table)/sizeof(struct trn_ent));
	
static struct swtch {
	char *name;
	int	set;
} switches[] = {
	{ "-reverse",   TRSPEC_REVERSE  },
	{ "-rev",	TRSPEC_REVERSE  },
	{ "-deleted", 	TRSPEC_DEL },
	{ "-u",		TRSPEC_UNSEEN },
	{ "-uns",	TRSPEC_UNSEEN },
	{ "-unseen",	TRSPEC_UNSEEN }
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
	int code;
	
	/* Come up with next xcn number */
	if(tg->te == (trnspec_entry *)tg) return DSC_NO_MORE;
	
	te=tg->te;
	
	deleted = tg->flags&TRSPEC_DEL;
	unseen = tg->flags&TRSPEC_UNSEEN;

	remove = te->flags&TRSPEC_REM;
	until = te->flags&TRSPEC_UNTIL;
	flags = te->flags&TRSPEC_KINDS;

again:
	switch (flags) {
	case TRSPEC_NEXT:
		if (deleted) trn_num = tg->current + 1;
		else trn_num = tg->tinfo.next;
		if (unseen) trn_num = max(tg->highest_seen+1, trn_num);
		break;
		
	case TRSPEC_PREV:
		if (deleted) trn_num = tg->tinfo.current - 1;
		else trn_num = tg->tinfo.prev;
		break;
		
	case TRSPEC_NREF:
		trn_num = tg->tinfo.nref;
		break;
		
	case TRSPEC_PREF:
		trn_num = tg->tinfo.pref;
		break;

	case TRSPEC_FREF:
		trn_num = tg->tinfo.fref;
		break;
		
	case TRSPEC_LREF:
		trn_num = tg->tinfo.lref;
		break;

	case TRSPEC_FIRST:
		trn_num = tg->minfo->first;
		break;

	case TRSPEC_LAST:
		trn_num = tg->minfo->last;
		break;

	case TRSPEC_CUR:
		goto gotit;

	case TRSPEC_NUM:
		trn_num = te->num;
		break;

	case TRSPEC_FUNS:		/* First unseen - this is broken*/
		trn_num = tg->highest_seen + 1;
		break;

	default:
		abort();
	}

	if (trn_num == 0) return DSC_NO_MORE;

	if (until) {
		if (tg->current < trn_num) {
			until = 0;
			flags = TRSPEC_NEXT;
			goto again;
		}
	}

	dsc_get_trn_info(tg->nbp, trn_num, &tg->tinfo, &code);

	if (code == DELETED_TRN) {
		if (deleted != TRSPEC_DEL) {
			deleted = -1;
			flags = TRSPEC_NEXT;
			goto again;
		}
		tg->current = 0;
	}
	if (code == 0) {
		tg->current = trn_num;
	}
	if (unseen && trn_num <= tg->highest_seen) 
		goto again;

gotit:
	if(remove) {
		remque(te);
		free(te);
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
}