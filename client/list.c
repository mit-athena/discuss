/*
 *
 * List request for DISCUSS
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v 1.19 1989-01-29 17:08:38 srz Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v $
 * $Locker:  $
 *
 * Copyright (C) 1986, 1988 by the MIT Student Information Processing Board
 *
 */
#ifndef lint
static char rcsid_discuss_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v 1.19 1989-01-29 17:08:38 srz Exp $";
#endif lint

#include <stdio.h>
#include <string.h>
#include <sys/param.h>		/* for MIN() */
#include "ss.h"
#include "config.h"
#include <discuss/discuss.h>
#include "globals.h"

char *ctime(), *malloc(), *local_realm(), *error_message(), *short_time();
static trn_info2 t_info;
static list_it(),delete_it(),retrieve_it();
static int performed;		/* true if trn was acted upon */
static int barred;		/* true if access was denied sometime */
static int only_initial;
static int long_subjects;
static int setting;		/* Whether we are turning flag on or off */

void map_trns();

static list_it(i)
	int i;
{
	char newtime[26], nlines[10];
	char *cp;
	int max_len;
	int code;

	dsc_get_trn_info2(&dsc_public.nb, i, &t_info, &code);
	if (code == DELETED_TRN) {
		code = 0;
		goto punt;
		/* should check -idl flag */
	}
	else if (code == NO_ACCESS) {
	        code = 0;
	        barred = TRUE;
		goto punt;
	}
	else if (code != 0) {
		ss_perror(sci_idx, code,
			  "Can't read transaction info");
		goto punt;
	}

	if (t_info.pref && only_initial) {
		code = 0;
		goto punt;
	}

	if (!performed) {
	    performed = TRUE;
	    dsc_public.current = i; /* current = first */
	}

	strcpy(newtime, short_time(&t_info.date_entered));
	/*
	 * If author ends with current realm, punt the realm.
	 */
	if ((cp=index(t_info.author, '@')) != NULL)
		if (!strcmp(cp+1, local_realm()))
			*cp = '\0';

	(void) sprintf (nlines, "(%d)", t_info.num_lines);
	(void) sprintf (buffer, " [%04d]%s%c",
			t_info.current,
			((t_info.flags & TRN_FLAG1) != 0) ? "F" : "",
			(t_info.current == dsc_public.current) ? '*' : ' ');
	(void) strncat (buffer, "     ",
			MIN (5, 13-strlen (buffer)) - strlen (nlines));

	if (strlen(t_info.author) > 15)
		(void) strcpy(&t_info.author[12], "...");

	(void) sprintf (buffer + strlen (buffer), "%s %s %-15s ",
			nlines, newtime, t_info.author);
	max_len = 80 - strlen (buffer);

	if (!long_subjects && strlen (t_info.subject) > max_len)
	    (void) strcpy (&t_info.subject [max_len - 3], "...");

	(void) printf ("%s%s\n", buffer, t_info.subject);

 punt:
	(void) free (t_info.author);
	(void) free (t_info.subject);
	return(code);
}

list (argc, argv, sci_idx)
	int argc, sci_idx;
	char **argv;
{
	char **ap, **ap2, **nargv;
	int ac;
	long_subjects = 0;

	for (ap = argv; *ap; ap++)
	    ;
	nargv = (char **) calloc (ap - argv + 1, sizeof (char *));
	ac = 0;
	for (ap = argv, ap2 = nargv; *ap; ap++) {
	    if (!strcmp (*ap, "-long_subjects") || !strcmp (*ap, "-lsj"))
		long_subjects = 1;
	    else
		*ap2++ = *ap, ac++;
	}
	*ap2 = (char *) NULL;
	map_trns(ac, nargv, "all", list_it);
	free (ap2);
	return;
}

static delete_it (i)
    int i;
{
     int code;

     if (only_initial) {
	     ss_perror(sci_idx, 0, "flag '-initial' not accepted");
	     return 1;
     }
     dsc_delete_trn(&dsc_public.nb, i, &code);
     if (code == NO_ACCESS) {
	  barred = TRUE;
     } else if (code == 0) {
	  performed = TRUE;
     } else if (code != DELETED_TRN) {
	  (void) fprintf(stderr, "Error deleting transaction %d: %s\n",
			 i, error_message(code));
	  if (code != EXPUNGED_TRN)
	       return(code);				/* stop now */
     }

     return(0);
}


del_trans(argc, argv)
	int argc;
	char **argv;
{
	map_trns(argc, argv, "current", delete_it);
	dsc_public.current = 0;
	return;
}

static retrieve_it (i)
    int i;
{
     int code;

     if (only_initial) {
	     ss_perror(sci_idx, 0, "flag '-initial' not accepted");
	     return 1;
     }
     dsc_retrieve_trn(&dsc_public.nb, i, &code);
     if (code == NO_ACCESS) {
	  barred = TRUE;
     } else if (code == 0) {
	  performed = TRUE;
	  dsc_public.current = i;
     } else if (code != TRN_NOT_DELETED) {
	  (void) fprintf(stderr, "Error retrieving transaction %d: %s\n",
			 i, error_message(code));
	  return(code);
     }
     return (0);
}



ret_trans(argc, argv)
	int argc;
	char **argv;
{
	map_trns(argc, argv, "current", retrieve_it);
	return;
}

void map_trns(argc, argv, defalt, proc)
	int argc;
	char **argv;
	char *defalt;
	int (*proc)();
{
	int i, code;
	selection_list *trn_list;

	if (!dsc_public.attending) {
		ss_perror(sci_idx, 0, "No current meeting.\n");
		return;
	}
	dsc_get_mtg_info(&dsc_public.nb,
			 &dsc_public.m_info, &code);
	if (code != 0) {
		(void) ss_perror(sci_idx, code, "Can't get meeting info");
		return;
	}

	dsc_get_trn_info (&dsc_public.nb, dsc_public.current,
			 &t_info, &code);
	if (code != 0)
		t_info.current = 0;
	else {
	     free(t_info.subject);
	     t_info.subject = NULL;
	     free(t_info.author);
	     t_info.author = NULL;
	}

	only_initial = 0;
	trn_list = (selection_list *)NULL;
	for (i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-initial"))
			only_initial = 1;
		else {
			trn_list = trn_select(&t_info, argv[i],
					      trn_list, &code);
			if (code) {
			    sprintf (buffer, "``%s''", argv[i]);
			    ss_perror(sci_idx, code, buffer);
			    sl_free(trn_list);
			    return;
			}
		}
	}
	if (trn_list == (selection_list *)NULL) {
		trn_list = trn_select(&t_info, defalt,
				      (selection_list *)NULL, &code);
		if (code) {
			ss_perror(sci_idx, code, defalt);
			free(trn_list);
			return;
		}
	}

	performed = FALSE;
	barred = FALSE;

	(void) sl_map(proc, trn_list);
	if (!performed)
	     ss_perror(sci_idx, barred ? NO_ACCESS : DISC_NO_TRN, "");
}

static flag_it(i)
int i;
{
     int code;

     dsc_get_trn_info2(&dsc_public.nb, i, &t_info, &code);
     if (code == DELETED_TRN) {
	  code = 0;
	  goto punt;
     } else if (code == NO_ACCESS) {
	  code = 0;
	  barred = TRUE;
	  goto punt;
     } else if (code != 0) {
	  ss_perror(sci_idx, code,
		    "Can't read transaction info");
	  goto punt;
     }

     if (setting)
	  t_info.flags |= TRN_FLAG1;
     else
	  t_info.flags &= ~TRN_FLAG1;

     dsc_set_trn_flags(&dsc_public.nb, i, t_info.flags, &code);
     if (code == NO_ACCESS) {
	  barred = TRUE;
     } else if (code == 0) {
	  performed = TRUE;
     } else if (code != DELETED_TRN) {
	  (void) fprintf(stderr, "Error setting flags for transaction %d: %s\n",
			 i, error_message(code));
	  goto punt;
     }
     code = 0;

punt:
     (void) free (t_info.author);
     (void) free (t_info.subject);
     return(code);
}

int set_flag(argc, argv)
int argc;
char **argv;
{
     
     if (argc < 2) {
	  goto usage;
     }

     if (!strcmp(argv[1], "on"))
	  setting = TRUE;
     else if (!strcmp(argv[1], "off"))
	  setting = FALSE;
     else goto usage;

     argc--,argv++;
     map_trns(argc, argv, "current", flag_it);
     return;

usage:
     (void) fprintf(stderr, "Usage: set flag [on|off] [trn_specs]\n");
     return;
}
