/*
 *
 * List request for DISCUSS
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v 1.7 1986-10-29 10:26:34 srz Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/list.c,v $
 * $Locker:  $
 *
 * Copyright (C) 1986 by the MIT Student Information Processing Board
 *
 * $Log: not supported by cvs2svn $
 * Revision 1.6  86/10/19  10:00:05  spook
 * Changed to use dsc_ routines; eliminate refs to rpc.
 * 
 * Revision 1.5  86/10/14  22:59:06  spook
 * Checking meeting info on each request.
 * 
 * Revision 1.4  86/09/22  06:18:43  spook
 * changed selected-list manipulation
 * 
 * Revision 1.3  86/09/10  18:57:27  wesommer
 * Made to work with kerberos; meeting names are now longer.
 * ./
 * 
 * Revision 1.2  86/08/23  21:43:09  spook
 * moved timecheck for list into list module
 * 
 * Revision 1.1  86/08/22  00:23:56  spook
 * Initial revision
 * 
 *
 */

#include <stdio.h>
#include <strings.h>
#include "../include/ss.h"
#include "../include/tfile.h"
#include "../include/interface.h"
#include "../include/config.h"
#include "../include/dsc_et.h"
#include "globals.h"

char *ctime(), *malloc();
static int	time_now, time_sixmonthsago, time_plusthreemonths;
static int idl, sci_idx;
static trn_info t_info;
static list_it(),delete_it(),retrieve_it();
static int performed;				/* true if trn was acted upon */
static int barred;				/* true if access was denied
						   sometime */

static
list_it(i)
	int i;
{
	char newtime[26];
	char *cp;
	error_code code;

	dsc_get_trn_info(dsc_public.mtg_uid, i, &t_info, &code);
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
	else {						/* success */
	     if (!performed) {
		  performed = TRUE;
		  dsc_public.current = i;		/* current = first */
	     }
	}
	cp = ctime(&t_info.date_entered);
	if((t_info.date_entered < time_sixmonthsago) ||
	   (t_info.date_entered > time_plusthreemonths))
		(void) sprintf(newtime, "%-7.7s %-4.4s",
			       cp+4, cp+20);
	else
		(void) sprintf(newtime, "%-12.12s", cp+4);
	/*
	 * If author ends with current realm, punt the realm.
	 */
	if ((cp=index(t_info.author, '@')) != NULL)
		if (!strcmp(cp+1, REALM))
			*cp = '\0';
	
	if (strlen(t_info.author) > 15) {
		(void) strcpy(&t_info.author[12], "...");
	}
	(void) sprintf(buffer, "(%d)", t_info.num_lines);
	if (strlen(t_info.subject) > 36) {
		(void) strcpy(&t_info.subject[33], "...");
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

list(sci_arg, argc, argv)
	int sci_arg;
	int argc;
	char **argv;
{
	(void) time(&time_now); 
	time_sixmonthsago = time_now - 6*30*24*60*60; 
	time_plusthreemonths = time_now + 6*30*24*60*60;

	map_trns(sci_arg, argc, argv, "all", list_it);
	return;
}

static 
delete_it(i)
int i;
{
     int code;

     dsc_delete_trn(dsc_public.mtg_uid, i, &code);
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


del_trans(sci_arg, argc, argv)
	int sci_arg;
	int argc;
	char **argv;
{
	map_trns(sci_arg, argc, argv, "current", delete_it);
	dsc_public.current = 0;
	return;
}

static
retrieve_it(i)
int i;
{
     int code;

     dsc_retrieve_trn(dsc_public.mtg_uid, i, &code);
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



ret_trans(sci_arg, argc, argv)
	int sci_arg;
	int argc;
	char **argv;
{
	map_trns(sci_arg, argc, argv, "current", retrieve_it);
	return;
}

map_trns(sci_arg, argc, argv, defalt, proc)
	int sci_arg;
	int argc;
	char **argv;
	char *defalt;
	void (*proc)();
{
	int txn_no;
	int code;
	int i;
	selection_list *trn_list;

	sci_idx = sci_arg;

	if (!dsc_public.attending) {
		ss_perror(sci_idx, 0, "No current meeting.\n");
		return;
	}
	dsc_get_mtg_info(dsc_public.mtg_uid, &dsc_public.m_info, &code);
	if (code != 0) {
		(void) ss_perror(sci_idx, code, "Can't get meeting info");
		return;
	}
	txn_no = dsc_public.m_info.first;

	dsc_get_trn_info(dsc_public.mtg_uid, dsc_public.current, &t_info, &code);
	if (code != 0)
		t_info.current = 0;
	else {
	     free(t_info.subject);
	     t_info.subject = NULL;
	     free(t_info.author);
	     t_info.author = NULL;
	}

	if (argc == 1) {
		trn_list = trn_select(&t_info, defalt,
				      (selection_list *)NULL, &code);
		if (code) {
			ss_perror(sci_idx, code, "");
			free(trn_list);
			return;
		}
	}
	else {
		trn_list = (selection_list *)NULL;
		while (argv++, argc-- > 1) {
			trn_list = trn_select(&t_info, *argv,
					      trn_list, &code);
			if (code != 0) {
				ss_perror(sci_idx, code, *argv);
				sl_free(trn_list);
				return;
			}
		}
	}

	performed = FALSE;
	barred = FALSE;

	sl_map(proc, trn_list);
	if (!performed)
	     if (barred)
		  ss_perror(sci_idx, NO_ACCESS, "");
	     else
		  (void) fprintf(stderr, "%s: No transactions selected\n", ss_name(sci_idx));
}

