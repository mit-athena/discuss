/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/lsm.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/lsm.c,v 1.8 1987-03-22 04:38:12 spook Exp $
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.7  87/01/06  23:47:21  rfrench
 * Added "new" field to lsm.
 * 
 * Revision 1.6  87/01/05  11:08:42  srz
 * Massaged header.
 * 
 * Revision 1.5  87/01/04  23:13:41  rfrench
 * First major change to lsm command:
 * Alphabetizes by long name and lists all local names together.
 * 
 */

#ifndef lint
static char *rcsid_lsm_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/lsm.c,v 1.8 1987-03-22 04:38:12 spook Exp $";
#endif lint


#include <strings.h>
#include <stdio.h>
#include "types.h"
#include "interface.h"
#include "globals.h"

extern char *malloc(),*ctime(), *error_message();

static int print_header, long_output;
static char *auser_id;

static
do_mtg(mtg_name)
	char *mtg_name;
{
#ifdef	notdef
	struct _dsc_pub *lng_name,lng_temp;
#endif
	name_blk *set,set_temp;
	register name_blk *nbp;
	int n_matches, i, j, code;
	bool updated,this_mtg;
	char last_host[140], last_path[140];

	dsc_expand_mtg_set(auser_id, mtg_name, &set, &n_matches, &code);
	if (!n_matches)
		return (0);

#ifdef	notdef
	lng_name = (struct _dsc_pub *) calloc (n_matches,
					       sizeof(struct _dsc_pub));
	for (i = 0; i < n_matches; i++) {
		for (j=i;j<i;j++)
			if (!strcmp(set[i].hostname,set[j].hostname) && !strcmp(set[i].pathname, set[j].pathname)) {
				lng_name[i] = lng_name[j];
				break;
		if (i == j && long_output) {
			dsc_get_mtg_info(&set[i], &lng_name[i].m_info, &code);
			if (code)
				fprintf(stderr,
					"Error checking meeting %s: %s\n",
					set[i].aliases[0],
					error_message(code));
			bcopy(&set[i], &lng_name[i].nb, sizeof(name_blk));
			}
		}
	}
#endif

	/* Yech yech bubble sort -- I'll optimize it later */
	if (n_matches > 1) {
		for (i=0;i<n_matches;i++)
			for (j=0;j<n_matches-1;j++) {
#ifdef	notdef
				if (strcmp(lng_name[j].m_info.long_name,
					   lng_name[j+1].m_info.long_name) > 0) {
						   lng_temp = lng_name[j];
						   lng_name[j] = lng_name[j+1];
						   lng_name[j+1] = lng_temp;
						   set_temp = set[j];
						   set[j] = set[j+1];
						   set[j+1] = set_temp;
					   }
#endif
				if (strcmp(set[j].aliases[0], set[j+1].aliases[0]) > 0) {
					bcopy(&set[j], &set_temp,
					      sizeof(name_blk));
					bcopy(&set[j+1], &set[j],
					      sizeof(name_blk));
					bcopy(&set_temp, &set[j+1],
					      sizeof(name_blk));
				}
			}
	}
	last_host[0] = '\0';
	last_path[0] = '\0';
	for (i = 0; i < n_matches; i++) {
		if (print_header) {
			printf("%-7s %-30s   %-30s  %s\n",
			       " Flags","Meeting ID","Short name",
#ifdef notdef
			       "New"
#else
			       ""
#endif
			       );
			print_header = 0;
		}
		nbp = &set[i];
		/* Test to see if we are attending this meeting */
		if (dsc_public.attending && !strcmp(dsc_public.host, nbp ->hostname) && !strcmp(dsc_public.path, nbp->pathname)) {
		     updated = (dsc_public.highest_seen < dsc_public.m_info.last);
		     this_mtg = TRUE;
		} else if (nbp->date_attended) {
		     this_mtg = FALSE;
		     dsc_updated_mtg(nbp, &updated, &code);
		     if (code) {
			  fprintf(stderr, "Error checking meeting %s: %s\n",
				  nbp -> aliases[0], error_message(code));
			  continue;
		     }
		} else {
			this_mtg = FALSE;
			updated = 0;
			code = 0;
		}
		if (code) {
			fprintf(stderr, "Error checking meeting %s: %s\n",
				nbp -> aliases[0], error_message(code));
			continue;
		}
		if (!strcmp(last_host,nbp->hostname) && !strcmp(last_path, nbp->pathname))
			printf("        %-30s   %s\n","",nbp->aliases[0]);
		else {
			printf(" %c      %-30s   %*s",updated?'c':' ',
#ifdef	notdef
			       lng_name[i].m_info.long_name,
#else
			       nbp->aliases[0],
#endif
			       updated?-30:0,
			       (nbp->aliases[1] ? nbp->aliases[1] : ""));
#ifdef	notdef
			if (updated)
				printf(" (%04d)",lng_name[i].m_info.last -
				       lng_name[i].nb.last);
#endif
			printf("\n");
			strcpy(last_host,nbp->hostname);
			strcpy(last_path,nbp->pathname);
		}
	}
	return(0);
}

list_meetings (argc, argv)
	int argc;
	char **argv;
{
	int have_names = 0;
	int i, *used;
	char *auser;

	used = (int *)calloc(argc, sizeof(int));
	long_output = 0;	/* make dependent on arguments later */
	auser = "";
	print_header = 1;

	for (i = 1; i < argc; i++) {
		if (!strcmp("-user", argv[i])) {
			if (i == argc - 1) {
				fprintf(stderr,
					"Missing argument for -user\n");
				free((char *)used);
				return;
			}
			if (auser[0] != '\0') {
			     fprintf(stderr,
				     "Only one of -user, -public allowed\n");
			     goto punt;
			}
			used[i] = 1;
			auser = argv[++i];
			used[i] = 1;
		}
		else if (!strcmp(argv[i],"-public")) {
		     if (auser[0] != '\0') {
			  fprintf(stderr,
				  "Only one of -user, -public allowed\n");
			  goto punt;
		     }
		     auser = "discuss";
		     used[i] = 1;
		}
		else if (*argv[i] == '-') {
			fprintf(stderr,
				"Unknown control argument %s\n",
				argv[i]);
			free((char *)used);
			return;
		}
		else {
			have_names = 1;
		}
	}

	auser_id = malloc(128);
	if (*auser)
		(void) strcpy(auser_id, auser);
	else {
		register char *at;
		(void) strcpy(auser_id, user_id);
	}

	if (!have_names) {
		do_mtg("*");
	}
	else for (i = 1; i < argc; i++) {
		if (!used[i])
			do_mtg(argv[i]);
	}

punt:
	free((char *)used);
}
