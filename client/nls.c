/*
 *
 * List request for DISCUSS
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/nls.c,v 1.4 1996-09-19 22:28:28 ghudson Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/nls.c,v $
 * $Locker:  $
 *
 * Copyright (C) 1986 by the MIT Student Information Processing Board
 *
 */
#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/nls.c,v 1.4 1996-09-19 22:28:28 ghudson Exp $";
#endif lint

#include <stdio.h>
#include <string.h>
#include "discuss_err.h"
#include <ss/ss.h>
#include "tfile.h"
#include "interface.h"
#include "config.h"
#include "dsc_et.h"
#include "globals.h"
#include "trn_spec.h"

char *ctime(), *malloc(), *local_realm(), *error_message(), *short_time();
static int	time_now, time_sixmonthsago, time_plusthreemonths;
static list_it(),delete_it(),retrieve_it();
static int performed;		/* true if trn was acted upon */
static int barred;		/* true if access was denied sometime */

void map_trns();

static
list_it(ti)
	trn_info *ti;
{
	char newtime[26];
	char *cp;
	int code;

	strcpy(newtime, short_time(&ti->date_entered));
	/*
	 * If author ends with current realm, punt the realm.
	 */
	if ((cp=strchr(ti->author, '@')) != NULL)
		if (!strcmp(cp+1, local_realm()))
			*cp = '\0';
	
	if (strlen(ti->author) > 15) {
		(void) strcpy(&ti->author[12], "...");
	}
	(void) sprintf(buffer, "(%d)", ti->num_lines);
	if (strlen(ti->subject) > 35) {
		(void) strcpy(&ti->subject[32], "...");
	}
	(void) printf(" [%04d]%c%5s %s %-15s %-20s\n",
		      ti->current,
		      (ti->current == dsc_public.current)?'*':' ',
		      buffer,
		      newtime,
		      ti->author,
		      ti->subject);
 punt:
	(void) free (ti->author);
	(void) free (ti->subject);
}

nlist(argc, argv)
	int argc;
	char **argv;
{
	trans_gen *tg;
	int code;
	extern int interrupt;
	
	(void) time(&time_now); 
	time_sixmonthsago = time_now - 6*30*24*60*60; 
	time_plusthreemonths = time_now + 6*30*24*60*60;

	if (code = parse_trans_spec(&argv, &argc, &dsc_public, "all", &tg)) {
		ss_perror(sci_idx, code, "before listing transactions");
		goto punt;
	}
	if (argc) {
		int i;
		printf("Remaining args:\n");
		for (i = 0; i < argc; i++) {
			printf("\"%s\" ", argv[i]);
		}
		printf("\n");
	}
			
	flag_interrupts();
	
	if ((code = tg_next_trn(tg)) == 0) {
#ifdef notdef
		dsc_public.current = tg->current;
#endif notdef
		list_it(&tg->tinfo);
		while (!interrupt && (code = tg_next_trn(tg)) == 0) {
			if (interrupt) break;
			list_it(&tg->tinfo);
		}
	} 
	if (code && code != DSC_NO_MORE) 
		ss_perror(sci_idx, code, "while listing transactions");

punt:
	dont_flag_interrupts();
	return;
}

