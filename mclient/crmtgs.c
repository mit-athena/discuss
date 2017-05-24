/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Id: crmtgs.c,v 1.7 1999-04-12 16:47:06 ghudson Exp $
 *
 *	Fill out a .meetings file with the primary name of all the
 *	meetings in it.  This requires that the meeting be accessible
 *	at this time; however, this program may be run several times
 *	to get it right..
 *
 */

#ifndef lint
static char rcsid_crmtgs_c[] =
    "$Id: crmtgs.c,v 1.7 1999-04-12 16:47:06 ghudson Exp $";
#endif /* lint */

#include <stdlib.h>
#include <stdio.h>
#include <discuss/discuss.h>

extern char *getenv ();

main(argc, argv)
	int argc;
	char **argv;
{
	int n_matches, code;
	register name_blk *nbp;
	name_blk *set;
	int i;
	mtg_info info;
	char **aliasv;
	int naliases;
	
#if defined(__APPLE__) && defined(__MACH__)
	add_error_table(&et_dsc_error_table);
#else
	initialize_dsc_error_table();
#endif

	dsc_expand_mtg_set(NULL, "*", &set, &n_matches, &code);

	if (code)
		punt_code("Can't expand meeting set", code);

	if (!n_matches)
		punt("No meetings?");

	for (nbp = set; nbp < set + n_matches; ++nbp) {
		dsc_get_mtg_info(nbp, &info, &code);
		if (code) {
			printf("%s: %s\n", nbp->aliases[0],
			       error_message(code));
			continue;
		}
		if (strcmp(nbp->aliases[0], info.long_name)) {
			printf("Adding long_name %s to %s\n",
			       info.long_name, nbp->aliases[0]);

			/*
			 * Beware of dragons and fenceposts in the
			 * following code.
			 */
			aliasv = nbp->aliases;
			while (*aliasv++) continue;
			naliases = aliasv - nbp->aliases;
			nbp->aliases = (char **) realloc(nbp->aliases,
				++naliases * sizeof(nbp->aliases));
			aliasv = nbp->aliases + naliases;
			while(naliases--) {
				*aliasv = aliasv[-1];
				--aliasv;
			}
			if (aliasv != nbp->aliases) abort();
			*aliasv = info.long_name;
		} else printf("Long name %s already present\n", 
			      info.long_name);
	}
	dsc_update_mtg_set(NULL, set, n_matches, &code);
	if (code)
		punt_code("update_mtg_set failed", code);
}

punt(string)
	char *string;
{
	puts(string);
	exit(1);
}

punt_code(string, code)
	char *string;
	int code;
{
	printf("%s: %s", string, error_message(code));
	exit(1);
} 

	
