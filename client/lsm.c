/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/lsm.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/lsm.c,v 1.5 1987-01-04 23:13:41 rfrench Exp $
 *
 *	$Log: not supported by cvs2svn $
 */

#ifndef lint
static char *rcsid_lsm_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/lsm.c,v 1.5 1987-01-04 23:13:41 rfrench Exp $";
#endif lint


#include <strings.h>
#include <stdio.h>
#include "types.h"
#include "interface.h"
#include "globals.h"

extern char *malloc(),*ctime();

static char *user, *realm;
static int print_header;

static
do_mtg(mtg_name)
	char *mtg_name;
{
	struct _dsc_pub *lng_name,lng_temp;
	name_blk *set,set_temp;
	register name_blk *nbp;
	int n_matches, i, j, code;
	bool updated,this_mtg;
	char last_id[140];

	expand_mtg_set(realm, user, mtg_name, &set, &n_matches);
	if (!n_matches)
		return (0);
	lng_name = (struct _dsc_pub *)malloc(n_matches * sizeof(struct _dsc_pub));
	for (i = 0; i < n_matches; i++) {
		for (j=i;j<i;j++)
			if (!strcmp(set[i].unique_id,set[j].unique_id)) {
				lng_name[i] = lng_name[j];
				break;
			}
		if (i == j) {
			dsc_get_mtg_info(set[i].unique_id,&lng_name[i].m_info,&code);
			if (code)
				fprintf(stderr, "Error checking meeting %s: %s\n",
					set[i].unique_id, error_message(code));
		}
	}
/* Yech yech bubble sort -- I'll optimize it later */
	if (n_matches > 1) {
		for (i=0;i<n_matches;i++)
			for (j=0;j<n_matches-1;j++) {
				if (strcmp(lng_name[j].m_info.long_name,
					   lng_name[j+1].m_info.long_name) > 0) {
						   lng_temp = lng_name[j];
						   lng_name[j] = lng_name[j+1];
						   lng_name[j+1] = lng_temp;
						   set_temp = set[j];
						   set[j] = set[j+1];
						   set[j+1] = set_temp;
					   }
			}
	}
	last_id[0] = '\0';
	for (i = 0; i < n_matches; i++) {
		if (print_header) {
			printf("%-7.7s %-30.30s %s\n",
			       " Flags",
			       "Meeting ID",
			       "Short name(s)");
			print_header = 0;
		}
		nbp = &set[i];
		/* Test to see if we are attending this meeting */
		if (dsc_public.attending && !strcmp(dsc_public.mtg_uid, nbp -> unique_id)) {
		     updated = (dsc_public.highest_seen < dsc_public.m_info.last);
		     this_mtg = TRUE;
		} else {
		     this_mtg = FALSE;
		     dsc_updated_mtg(nbp -> unique_id,
				     nbp -> date_attended,
				     nbp -> last,
				     &updated,
				     &code);
		     if (code) {
			  fprintf(stderr, "Error checking meeting %s: %s\n",
				  nbp -> unique_id, error_message(code));
			  continue;
		     }
		}
		if (code) {
			fprintf(stderr, "Error checking meeting %s: %s\n",
				nbp -> unique_id, error_message(code));
			continue;
		}
		if (!strcmp(last_id,nbp->unique_id))
			printf("        %-30.30s   %s\n","",nbp->mtg_name);
		else {
			printf("%c       %-30.30s   %s\n",updated?'c':' ',
			       lng_name[i].m_info.long_name,
			       nbp->mtg_name);
			strcpy(last_id,nbp->unique_id);
		}
	}
}

/*			    nbp -> last,
			    this_mtg ? "NOW ATTENDING\n" 
			             : (nbp -> date_attended == 0) ? "\n"
			    			: ctime(&(nbp -> date_attended);
*/

list_meetings (argc, argv)
	int argc;
	char **argv;
{
	int have_names = 0;
	int i, *used;

	used = (int *)malloc(sizeof(int)*argc);
	bzero(used, sizeof(int)*argc); /* bletch */
	user = "";
	realm = user;
	print_header = 1;

	for (i = 1; i < argc; i++) {
		if (!strcmp("-user", argv[i])) {
			if (i == argc - 1) {
				fprintf(stderr,
					"Missing argument for -user\n");
				free(used);
				return;
			}
			if (user[0] != '\0') {
			     fprintf(stderr,
				     "Only one of -user, -public allowed\n");
			     goto punt;
			}
			used[i] = 1;
			user = argv[++i];
			used[i] = 1;
		}
		else if (!strcmp(argv[i],"-public")) {
		     if (user[0] != '\0') {
			  fprintf(stderr,
				  "Only one of -user, -public allowed\n");
			  goto punt;
		     }
		     user = "discuss";
		     used[i] = 1;
		}
		else if (*argv[i] == '-') {
			fprintf(stderr,
				"Unknown control argument %s\n",
				argv[i]);
			free(used);
			return;
		}
		else {
			have_names = 1;
		}
	}

	if (!have_names) {
		do_mtg("*");
	}
	else for (i = 1; i < argc; i++) {
		if (!used[i])
			do_mtg(argv[i]);
	}

punt:
	free(used);
}
