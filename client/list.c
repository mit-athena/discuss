/*
 *
 * List request for DISCUSS
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v 1.16 1987-11-09 23:45:54 raeburn Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v $
 * $Locker:  $
 *
 * Copyright (C) 1986 by the MIT Student Information Processing Board
 *
 */
#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v 1.16 1987-11-09 23:45:54 raeburn Exp $";
#endif lint

#include <stdio.h>
#include <strings.h>
#include "discuss_err.h"
#include "ss.h"
#include "tfile.h"
#include "interface.h"
#include "config.h"
#include "dsc_et.h"
#include "globals.h"

char *ctime(), *malloc(), *local_realm(), *error_message(), *short_time();
static int	time_now, time_sixmonthsago, time_plusthreemonths;
static trn_info t_info;
static list_it(),delete_it(),retrieve_it();
static int performed;		/* true if trn was acted upon */
static int barred;		/* true if access was denied sometime */
static int only_initial;

void map_trns();

static
list_it(i)
	int i;
{
	char newtime[26];
	char *cp;
	int code;

	dsc_get_trn_info(&dsc_public.nb, i, &t_info, &code);
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
			  "Can't read trn info");
		goto punt;
	}

	if (t_info.pref && only_initial) {
		code = 0;
		goto punt;
	}

	if (!performed) {
		performed = TRUE;
		dsc_public.current = i;		/* current = first */
	}

	strcpy(newtime, short_time(&t_info.date_entered));
	/*
	 * If author ends with current realm, punt the realm.
	 */
	if ((cp=index(t_info.author, '@')) != NULL)
		if (!strcmp(cp+1, local_realm()))
			*cp = '\0';
	
	if (strlen(t_info.author) > 15) {
		(void) strcpy(&t_info.author[12], "...");
	}
	(void) sprintf(buffer, "(%d)", t_info.num_lines);
	if (strlen(t_info.subject) > 35) {
	     (void) strcpy(&t_info.subject[32], "...");
	}
	(void) printf(" [%04d]%c%5s %s %-15s %-20s\n",
		      t_info.current,
		      (t_info.current == dsc_public.current)?'*':' ',
		      buffer,
		      newtime,
		      t_info.author,
		      t_info.subject);
 punt:
	(void) free (t_info.author);
	(void) free (t_info.subject);
	return(code);
}

list(argc, argv)
	int argc;
	char **argv;
{
	(void) time(&time_now); 
	time_sixmonthsago = time_now - 6*30*24*60*60; 
	time_plusthreemonths = time_now + 6*30*24*60*60;

	map_trns(argc, argv, "all", list_it);
	return;
}

static 
delete_it(i)
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

static
retrieve_it(i)
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

void
map_trns(argc, argv, defalt, proc)
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

	dsc_get_trn_info(&dsc_public.nb, dsc_public.current,
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
				ss_perror(sci_idx, code, argv[i]);
				sl_free(trn_list);
				return;
			}
		}
	}
	if (trn_list == (selection_list *)NULL) {
		trn_list = trn_select(&t_info, defalt,
				      (selection_list *)NULL, &code);
		if (code) {
			ss_perror(sci_idx, code, "");
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
