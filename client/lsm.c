/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/lsm.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/lsm.c,v 1.24 1996-09-08 20:31:20 ghudson Exp $
 *
 */

#ifndef lint
static char rcsid_lsm_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/lsm.c,v 1.24 1996-09-08 20:31:20 ghudson Exp $";
#endif lint

#include <string.h>
#include <stdio.h>
#include <discuss/discuss.h>
#include "globals.h"
#include <errno.h>
#include <ss/ss.h>

extern char *malloc(), *calloc(), *ctime(), *error_message();
extern void flag_interrupts(), dont_flag_interrupts();


int print_header, long_output;
static char last_host[140], last_path[140];
static char *auser_id;
static int fast;

/* this function should probably be void */
int do_line(nbp, code, updated)
	register name_blk *nbp;
	int code, updated;
{
	/*
	 * Output format:  8-char flags field (including trailing
	 * spaces), followed by a ", "-separated list of meetings
	 * names, and newline.
	 *
	 * If an error occurred, 22+ characters of the first meeting
	 * name are printed (padded with whitespace), followed by a
	 * parenthesized error message.
	 */
	char **namep;
	if (print_header) {
	        char *fmt = "%-7s %-22s   %-22s\n";
		last_host[0] = '\0';
		last_path[0] = '\0';
		printf (" Flags  Meeting\n");
		printf (" -----  -------\n");
		print_header = 0;
	}
	if (code) {
		printf("        %-22s (%s %s%s)\n", nbp->aliases[0],
		       error_message (code),
		       (code == ECONNREFUSED) ? "by "
		       : ((code == ETIMEDOUT) ? "with " : ""),
		       nbp->hostname);
		return 0;
	}
	if(!strcmp(last_host,nbp->hostname)&&!strcmp(last_path,nbp->pathname))
	    updated = 0;
	printf (" %c      %s", updated ? 'c' : ' ', nbp->aliases[0]);
	for (namep = &nbp->aliases[1]; *namep; namep++)
	    printf (", %s", *namep);
	printf ("\n");
	strcpy(last_host,nbp->hostname);
	strcpy(last_path,nbp->pathname);
	return 0;
}

static
do_mtg(mtg_name)
	char *mtg_name;
{
	name_blk *set;
	register name_blk *nbp;
	int n_matches, i, code;
	bool updated;

	dsc_expand_mtg_set(auser_id, mtg_name, &set, &n_matches, &code);

	if (code) {
		ss_perror(sci_idx, code, "");
		return(0);
	}

	if (!n_matches)
		return (0);

	for (i = 0; i < n_matches; i++) {
	        if (interrupt)
		        break;

		nbp = &set[i];

		if (fast) {
		     updated = 0;
		} else {
		     /* Test to see if we are attending this meeting */
		     if (dsc_public.attending 
		     && !strcmp(dsc_public.host, nbp->hostname) 
		     && !strcmp(dsc_public.path, nbp->pathname)) {
			  updated = (dsc_public.highest_seen 
				     < dsc_public.m_info.last);
		     } else {
			  dsc_updated_mtg(nbp, &updated, &code);
			  if (interrupt)
			       break;
			  if (code == NO_SUCH_TRN) {	/* Meeting lost trns */
			       updated = TRUE;
			       code = 0;
			  }
		     }
		}
		do_line(nbp, code, updated);
	}
	dsc_destroy_mtg_set(set, n_matches);
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
	fast = 0;
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
		else if (!strcmp(argv[i],"-f") || !strcmp(argv[i], "-fast") || !strcmp(argv[i], "-brief") || !strcmp(argv[i], "-bf"))
		     fast = 1;
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
		(void) strcpy(auser_id, user_id);
	}

	flag_interrupts();
	if (!have_names) {
		do_mtg("*");
	}
	else for (i = 1; i < argc; i++) {
		if (!used[i])
			do_mtg(argv[i]);
	}
	dont_flag_interrupts();

punt:
	free(auser_id);
	free((char *)used);
}
