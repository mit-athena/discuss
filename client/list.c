/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * List request for DISCUSS
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v 1.29 1991-08-07 09:04:27 lwvanels Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v $
 * $Locker:  $
 *
 */
#ifndef lint
static char rcsid_discuss_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v 1.29 1991-08-07 09:04:27 lwvanels Exp $";
#endif lint

#include <stdio.h>
#include <string.h>
#include <sys/param.h>		/* for MIN() */
#include <ss/ss.h>
#include "config.h"
#include <discuss/discuss.h>
#include "globals.h"

char *ctime(), *malloc(), *local_realm(), *error_message(), *short_time();
static trn_info3 t_info;
static list_it(),delete_it(),retrieve_it();
static int performed;		/* true if trn was acted upon */
static int barred;		/* true if access was denied sometime */
static int long_subjects;
static int setting;		/* Whether we are turning flag on or off */

void map_trns();

static list_it(t_infop, codep)
trn_info3 *t_infop;
int *codep;
{
	char newtime[26], nlines[10];
	char *cp,*author;
	int len;

	if (*codep == NO_ACCESS) {
	        *codep = 0;
	        barred = TRUE;
		goto punt;
	}
	else if ((*codep != 0) && (*codep != DELETED_TRN)) {
		ss_perror(sci_idx, *codep,
			  "Can't read transaction info");
		goto punt;
	}

	*codep = 0;

	if (!performed) {
	    performed = TRUE;
	    dsc_public.current = t_infop->current; /* current = first */
	}

	strcpy(newtime, short_time(&t_infop->date_entered));
	if (t_infop-> signature != NULL && *(t_infop->signature) != '\0')
	     author = t_infop -> signature;
	else
	     author = t_infop -> author;

	/*
	 * If author/signature ends with current realm, punt the realm.
	 */
	if ((cp=index(author, '@')) != NULL)
		if (!strcmp(cp+1, local_realm()))
			*cp = '\0';

	(void) sprintf (nlines, "(%d)", t_infop->num_lines);
	(void) sprintf (buffer, " [%04d]%s%c",
			t_infop->current,
			((t_infop->flags & TRN_FLAG1) != 0) ? "F" : "",
			(t_infop->current == dsc_public.current) ? '*' : ' ');
	if ((len = MIN(5, 13-strlen (buffer)) - strlen (nlines)) > 0)
		(void) strncat (buffer, "     ", len);

	if (strlen(author) > 15)
		(void) strcpy(&author[12], "...");

	(void) sprintf (buffer + strlen (buffer), "%s %s %-15s ",
			nlines, newtime, author);
	len = 79 - strlen (buffer);

	if (!long_subjects && strlen (t_infop->subject) > len)
	    (void) strcpy (&t_infop->subject [len - 3], "...");

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
	map_trns(ac, nargv, "all", list_it, FALSE);
	free (nargv);
	return;
}

static delete_it (t_infop, codep)
trn_info3 *t_infop;
int *codep;
{
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
trn_info3 *t_infop;
int *codep;
{
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

void map_trns(argc, argv, defalt, proc, filter_flags)
	int argc;
	char **argv;
	char *defalt;
	int (*proc)();
	int filter_flags;     
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

	trn_list = (selection_list *)NULL;
	for (i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-initial"))
		  filter_flags |= filter_ONLY_INITIAL;
		else if (!strcmp (argv[i], "-terminal"))
		  filter_flags |= filter_ONLY_TERMINAL;
		else if ((!strcmp (argv[i], "-include_deleted")) ||
			 (!strcmp (argv[i], "-idl")))
		  filter_flags |= filter_INCLUDE_DELETED;
		else if ((!strcmp (argv[i], "-only_deleted")) ||
			 (!strcmp (argv[i], "-odl")))
		  filter_flags |= filter_ONLY_DELETED;		    
		else if ((!strcmp (argv[i], "-flag_set")) ||
			 (!strcmp (argv[i], "-flag")))
		  filter_flags |= filter_FLAG_SET;		    
		else if (!strcmp (argv[i], "-flag_reset"))
		  filter_flags |= filter_FLAG_RESET;		    
		/* someday we'll have abbrevs */
		else if (!strcmp (argv[i], "-no_terminal"))
		  filter_flags &= ~filter_ONLY_TERMINAL;
		else if ((!strcmp (argv[i], "-no_include_deleted")) ||
			 (!strcmp (argv[i], "-nidl")))
		  filter_flags &= ~filter_INCLUDE_DELETED;
		else if ((!strcmp (argv[i], "-no_only_deleted")) ||
			 (!strcmp (argv[i], "-nodl")))
		  filter_flags &= ~filter_ONLY_DELETED;		    
		else if ((!strcmp (argv[i], "-no_flag_set")) ||
			 (!strcmp (argv[i], "-no_flag")))
		  filter_flags &= ~filter_FLAG_SET;		    
		else if (!strcmp (argv[i], "-no_flag_reset"))
		  filter_flags &= ~filter_FLAG_RESET;		    
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

	(void) sl_map(proc, trn_list, filter_flags);
	sl_free(trn_list);
	if (!performed)
	     ss_perror(sci_idx, barred ? NO_ACCESS : DISC_NO_TRN, "");
}

static flag_it(t_infop, codep)
trn_info3 *t_infop;
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
