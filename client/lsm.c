#include <strings.h>
#include <stdio.h>
#include "../include/types.h"
#include "../include/dsname.h"

extern char *malloc();

static char *user, *realm;
static int print_header;

static
do_mtg(mtg_name)
	char *mtg_name;
{
	name_blk *set;
	register name_blk *nbp;
	int n_matches, i, code;
	bool updated;

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
		updated_mtg(nbp -> unique_id,
			    nbp -> date_attended,
			    nbp -> last,
			    &updated,
			    &code);
/*		if (code) {
			fprintf(stderr, "Error checking meeting %s: %s\n",
				nbp -> unique_id, error_message(code));
		}
		else {
*/
			printf("%-7.7s %-30.30s   [%04d]    %s",
			       updated ? "   *   " : "",
			       nbp -> mtg_name,
			       nbp -> last,
			       ctime(&(nbp -> date_attended)));
/*		}*/
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
			used[i] = 1;
			user = argv[++i];
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
}
