#include <strings.h>
#include <stdio.h>
#include "../include/types.h"
#include "../include/interface.h"
#include "globals.h"

extern char *malloc(),*ctime();

static char *user, *realm;
static int print_header;

static
do_mtg(mtg_name)
	char *mtg_name;
{
	name_blk *set;
	register name_blk *nbp;
	int n_matches, i, code;
	bool updated,this_mtg;

	expand_mtg_set(realm, user, mtg_name, &set, &n_matches);
	for (i = 0; i < n_matches; i++) {
		if (print_header) {
			printf("%-7.7s %-30.30s %-10.10s  %-24.24s\n",
			       "Changed",
			       "Meeting ID",
			       " Last seen ",
			       "Date attended");
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
		if (updated)
		     printf("   *    %-30.30s   [%04d]    %s",
			    nbp -> mtg_name,
			    nbp -> last,
			    this_mtg ? "NOW ATTENDING\n" 
			             : (nbp -> date_attended == 0) ? "\n"
			    			: ctime(&(nbp -> date_attended)));
		else
		     printf("        %-30.30s    *END*    %s",
			    nbp -> mtg_name,
			    this_mtg ? "NOW ATTENDING\n" 
			             : (nbp -> date_attended == 0) ? "\n"
			    			: ctime(&(nbp -> date_attended)));
	   }
   }

list_meetings (sci_idx, argc, argv)
	int sci_idx, argc;
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
