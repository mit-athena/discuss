/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/trn_spec.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/trn_spec.c,v 1.1 1987-04-10 23:42:37 srz Exp $
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
 */

#ifndef lint
static char *rcsid_trn_spec_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/trn_spec.c,v 1.1 1987-04-10 23:42:37 srz Exp $";
#endif lint

#include "trn_spec.h"

/*
 * Table of legal transaction specs.  Entries must be sorted in ASCII
 * order.
 */

static struct {	
	char *name;
	int (*function)();
	int set, modify;
} trn_table[] = {
	{ ".", 		simple, TRSPEC_CUR, 0 },
	{ "cur", 	simple, TRSPEC_CUR, 0 },
	{ "n",	 	simple, TRSPEC_NEXT, 0 },
	{ "next", 	simple, TRSPEC_NEXT, 0 },
	{ "nr",		simple, TRSPEC_NREF, 0 },
	{ "nref",	simple, TRSPEC_NREF, 0 },
	{ "p", 		simple, TRSPEC_PREV, 0 },
	{ "pr",		simple, TRSPEC_PREF, 0 },
	{ "pref",	simple, TRSPEC_PREF, 0 },
	{ "prev", 	simple, TRSPEC_PREV, 0 }
}

static struct {
	char *s;
	int	set;
} switches[] = {
	{ "-reverse",   TRSPEC_REVERSE, 0 },
	{ "-rev",	TRSPEC_REVERSE, 0 },
	{ "-idl", }
}








/*
 * Parse a transaction specifier.
 * Returns a "generator" for the transactions specified.
 * Modifies (reallocs) argv to contain only the arguments it does 
 * not understand.
 *
 * Returns a standard error code.
 */
int
parse_trans_spec(argv_p, argc_p, env, flags, gen)
	char ***argv_p;	/* Pointer to array of (char *) */
	int *argc_p;	/* Arg count (modified) */
	struct _dsc_pub env; /* Environment (current mtg, trn, etc) */
	int flags;	/* Flags modifying what the parser does */
	trans_gen *gen; /* Return */
{
	char **argv = *argv_p;
	char **nargv = (char **)malloc(0);
	int nargc = 0;
	int argc = *argc_p;
	register char **nargp;
	register char **argp; 	/* induction variable */
	int code = 0;		/* return error code */

	for ( ; argc; --argc, ++argp) {
		register char *arg = *argp;
		
	}


out:
	*argv_p = nargv;
	*argc_p = nargc;

	return code;
}


}