/*
 *
 * List request for DISCUSS
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v 1.24 1989-05-02 18:34:23 raeburn Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v $
 * $Locker:  $
 *
 * Copyright (C) 1986, 1988 by the MIT Student Information Processing Board
 *
 */
#ifndef lint
static char rcsid_discuss_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v 1.24 1989-05-02 18:34:23 raeburn Exp $";
#endif lint

#include <stdio.h>
#include <string.h>
#include <sys/param.h>		/* for MIN() */
#include <ss/ss.h>
#include "config.h"
#include <discuss/discuss.h>
#include "globals.h"

char *ctime(), *malloc(), *local_realm(), *error_message(), *short_time();
static trn_info2 t_info;
static list_it(),delete_it(),retrieve_it();
static int performed;		/* true if trn was acted upon */
static int barred;		/* true if access was denied sometime */
static int only_initial;	/* kludges... */
static int long_subjects;
static int flag_set, flag_reset;
static int setting;		/* Whether we are turning flag on or off */

void map_trns();

static list_it(t_infop, codep)
trn_info2 *t_infop;
int *codep;
{
	char newtime[26], nlines[10];
	char *cp;
	int max_len;

	if (*codep == DELETED_TRN) {
		*codep = 0;
		goto punt;
		/* should check -idl flag */
	}
	else if (*codep == NO_ACCESS) {
	        *codep = 0;
	        barred = TRUE;
		goto punt;
	}
	else if (*codep != 0) {
		ss_perror(sci_idx, *codep,
			  "Can't read transaction info");
		goto punt;
	}

	*codep = 0;
	if (t_infop->pref && only_initial) {
		goto punt;
	}

	if (flag_set && !flag_reset && (t_infop->flags & TRN_FLAG1) == 0)
		goto punt;
	if (!flag_set && flag_reset && (t_infop->flags & TRN_FLAG1) != 0)
		goto punt;

	if (!performed) {
	    performed = TRUE;
	    dsc_public.current = t_infop->current; /* current = first */
	}

	strcpy(newtime, short_time(&t_infop->date_entered));
	/*
	 * If author ends with current realm, punt the realm.
	 */
	if ((cp=index(t_infop->author, '@')) != NULL)
		if (!strcmp(cp+1, local_realm()))
			*cp = '\0';

	(void) sprintf (nlines, "(%d)", t_infop->num_lines);
	(void) sprintf (buffer, " [%04d]%s%c",
			t_infop->current,
			((t_infop->flags & TRN_FLAG1) != 0) ? "F" : "",
			(t_infop->current == dsc_public.current) ? '*' : ' ');
	(void) strncat (buffer, "     ",
			MIN (5, 13-strlen (buffer)) - strlen (nlines));

	if (strlen(t_infop->author) > 15)
		(void) strcpy(&t_infop->author[12], "...");

	(void) sprintf (buffer + strlen (buffer), "%s %s %-15s ",
			nlines, newtime, t_infop->author);
	max_len = 80 - strlen (buffer);

	if (!long_subjects && strlen (t_infop->subject) > max_len)
	    (void) strcpy (&t_infop->subject [max_len - 3], "...");

	(void) printf ("%s%s\n", buffer, t_infop->subject);

punt:
	return;
}

list (argc, argv, sci_idx)
	int argc, sci_idx;
	char **argv;
{
	char **ap, **ap2, **nargv;
	int ac;

	long_subjects = flag_set = flag_reset = 0;

	for (ap = argv; *ap; ap++)
	    ;
	nargv = (char **) calloc (ap - argv + 1, sizeof (char *));
	ac = 0;
	for (ap = argv, ap2 = nargv; *ap; ap++) {
	    if (!strcmp (*ap, "-long_subjects") || !strcmp (*ap, "-lsj"))
		long_subjects = 1;
	    else if (!strcmp (*ap, "-flag_set"))
		flag_set = 1;
	    else if (!strcmp (*ap, "-flag_reset"))
		flag_reset = 1;
	    else
		*ap2++ = *ap, ac++;
	}
	*ap2 = (char *) NULL;
	map_trns(ac, nargv, "all", list_it, FALSE);
	free (nargv);
	return;
}

static delete_it (t_infop, codep)
trn_info2 *t_infop;
int *codep;
{
     if (only_initial) {
	  ss_perror(sci_idx, 0, "flag '-initial' not accepted");
	  *codep = 1;
	  return;
     }

     if (*codep == DELETED_TRN) {		/* Already deleted, done */
	  *codep = 0;
	  return;
     }

     dsc_delete_trn(&dsc_public.nb, t_infop->current, codep);
     if (*codep == NO_ACCESS) {
	  barred = TRUE;
     } else if (*codep == 0) {
	  performed = TRUE;
     } else {
	  (void) fprintf(stderr, "Error deleting transaction %d: %s\n",
			 t_infop->current, error_message(*codep));
	  if (*codep != EXPUNGED_TRN)
	       return;					/* stop now */
     }

     *codep = 0;
     return;
}


del_trans(argc, argv)
	int argc;
	char **argv;
{
	map_trns(argc, argv, "current", delete_it, FALSE);
	dsc_public.current = 0;
	return;
}

static retrieve_it (t_infop, codep)
trn_info2 *t_infop;
int *codep;
{
     if (only_initial) {
	  ss_perror(sci_idx, 0, "flag '-initial' not accepted");
	  *codep = 1;
	  return;
     }

     if (*codep == 0) {				/* Transaction exists */
	  return;
     }

     dsc_retrieve_trn(&dsc_public.nb, t_infop->current, codep);
     if (*codep == NO_ACCESS) {
	  barred = TRUE;
     } else if (*codep == 0) {
	  performed = TRUE;
	  dsc_public.current = t_infop->current;
     } else {
	  (void) fprintf(stderr, "Error retrieving transaction %d: %s\n",
			 t_infop->current, error_message(*codep));
	  return;
     }
     *codep = 0;
     return;
}

ret_trans(argc, argv)
	int argc;
	char **argv;
{
	map_trns(argc, argv, "current", retrieve_it, TRUE);
	return;
}

void map_trns(argc, argv, defalt, proc, include_deleted)
	int argc;
	char **argv;
	char *defalt;
	int (*proc)();
	int include_deleted;     
{
	int i, code;
	selection_list *trn_list;

	if (!dsc_public.attending) {
		ss_perror(sci_idx, 0, "No current meeting.\n");
		return;
	}
	dsc_destroy_mtg_info(&dsc_public.m_info);
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

	dsc_destroy_trn_info(&t_info);		/* Get rid of dynamic stuff */

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
			sl_free(trn_list);
			return;
		}
	}

	performed = FALSE;
	barred = FALSE;

	(void) sl_map(proc, trn_list, include_deleted);
	sl_free(trn_list);
	if (!performed)
	     ss_perror(sci_idx, barred ? NO_ACCESS : DISC_NO_TRN, "");
}

static flag_it(t_infop, codep)
trn_info2 *t_infop;
int *codep;
{
     if (*codep == DELETED_TRN) {
	  *codep = 0;
	  return;
     } else if (*codep == NO_ACCESS) {
	  *codep = 0;
	  barred = TRUE;
	  return;
     } else if (*codep != 0) {
	  ss_perror(sci_idx, *codep,
		    "Can't read transaction info");
	  return;
     }

     if (setting)
	  t_infop->flags |= TRN_FLAG1;
     else
	  t_infop->flags &= ~TRN_FLAG1;

     dsc_set_trn_flags(&dsc_public.nb, t_infop->current, t_infop->flags, codep);
     if (*codep == NO_ACCESS) {
	  barred = TRUE;
     } else if (*codep == 0) {
	  performed = TRUE;
     } else {
	  (void) fprintf(stderr, "Error setting flags for transaction %d: %s\n",
			 t_infop->current, error_message(*codep));
	  return;
     }
     *codep = 0;

     return;
}

int switch_cmd(argc, argv)
int argc;
char **argv;
{
     
     if (argc < 2) {
	  goto usage;
     }

     if (!strcmp(argv[0], "switch_on") || !strcmp(argv[0], "swn"))
	  setting = TRUE;
     else
	  setting = FALSE;

     if (strcmp(argv[1], "flag"))
	  goto usage;
     argc--,argv++;
     map_trns(argc, argv, "current", flag_it, FALSE);
     return;

usage:
     (void) fprintf(stderr, "Usage: %s flag [trn_specs]\n",argv[0]);
     return;
}
