/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/rn.c,v $
 *	$Author: srz $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/rn.c,v 1.2 1987-11-07 02:50:38 srz Exp $
 *
 *	Copyright (C) 1987 by the Massachusetts Institute of Technology
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.1  87/10/24  19:48:02  srz
 * Initial revision
 * 
 */

#ifndef lint
static char *rcsid_update_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/rn.c,v 1.2 1987-11-07 02:50:38 srz Exp $";
#endif lint

#include "types.h"
#include "interface.h"
#include "globals.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/ioctl.h>

static changed_meetings()
{
	int code, n_matches, i;
	name_blk *set, *nbp;

	dsc_expand_mtg_set(user_id, "*", &set, &n_matches, &code);
	for (i = 0; i < n_matches; i++) {
		nbp = &set[i];
		if (nbp->status & DSC_ST_CHANGED) {
			free(set);
			return 1;
		}
	}
	free(set);
	return 0;
}

rn(argc, argv, ss_idx)
	int argc;
	char **argv;
        int ss_idx;
{
	int code = 0;
	int cmd;

	printf("Checking meetings...\n");
	fflush(stdout);

	ss_execute_line(ss_idx, "ckm", &code);
	if (code != 0) goto punt;

	flag_interrupts();
	printf("\n");

	if (!changed_meetings())
	     return;

	cmd = more_break("Hit space to go to next meeting: ", " qn?");
	if (interrupt)
	     goto done;
	printf("\n");
	switch(cmd) {
	case 'q':
	     goto done;
	case ' ':
	case 'n':
	     break;
	case '?':
	     printf("List of possible responses:\n\n");
	     printf("<space>,n\tNext meeting\n");
	     printf("q\t\tQuit from read_new\n");
	     printf("?\t\tShow this list\n\n");
	     break;
	}
	ss_execute_line(ss_idx, "nm", &code);
	if (code != 0) goto punt;

	while (1) {			/* we get out when changed_meetings is false */
	        if (interrupt)
		     break;
		
		while (unseen_transactions()) {
		        if (interrupt)
			     break;

			cmd = more_break("Hit space for next transaction: ", " qnptr?");
			if (interrupt)
			     break;
		        printf("\n");
			switch (cmd) {
			case 'q':
				goto punt;
			case ' ':
			case 'n':
				ss_execute_line(ss_idx, "next", &code);
				if (code != 0) goto punt;
				break;
			case 'p':
				ss_execute_line(ss_idx, "prev", &code);
				if (code != 0) goto punt;
				break;
			case 'r':
				ss_execute_line(ss_idx, "reply", &code);
				if (code != 0) goto punt;
				break;
			case 't':
				ss_execute_line(ss_idx, "talk", &code);
				if (code != 0) goto punt;
				break;
			case '?':
				printf("List of possible responses:\n\n");
				printf("<space>,n\tNext transaction\n");
				printf("p\t\tPrevious transaction\n");
				printf("q\t\tQuit from read_new\n");
				printf("r\t\tReply to current transaction\n");
				printf("t\t\tEnter a new transaction\n");
				printf("?\t\tShow this list\n\n");
				break;
			}
		}

		if (!changed_meetings())
		     break;

		cmd = more_break("Hit space to go to next meeting: ", " qn?ptr");
		if (interrupt)
		     break;
		printf("\n");
		switch(cmd) {
		case 'q':
		     goto punt;
		case ' ':
		case 'n':
		     ss_execute_line(ss_idx, "nm", &code);
		     if (code != 0) goto punt;
		     break;
		case 'p':
		     ss_execute_line(ss_idx, "prev", &code);
		     if (code != 0) goto punt;
		     break;
		case 'r':
		     ss_execute_line(ss_idx, "reply", &code);
		     if (code != 0) goto punt;
		     break;
		case 't':
		     ss_execute_line(ss_idx, "talk", &code);
		     if (code != 0) goto punt;
		     break;
		case '?':
		     printf("List of possible responses:\n\n");
		     printf("<space>,n\tNext meeting\n");
		     printf("p\t\tPrevious transaction\n");
		     printf("q\t\tQuit from read_new\n");
		     printf("r\t\tReply to current transaction\n");
		     printf("t\t\tEnter a new transaction\n");
		     printf("?\t\tShow this list\n\n");
		     break;
		}
	}

done:
	return;

punt:
	dont_flag_interrupts();
	ss_perror(ss_idx, code, 0);
}

/*
 * Flames to /dev/null
 */

more_break(prompt, cmds)
	char *prompt;
	char *cmds;
{
	struct sgttyb tty, ntty;
	int arg;
	char buf[1];

	arg = FREAD;				/* Flush pending input */
	ioctl(0, TIOCFLUSH, &arg);
	ioctl(0, TIOCGETP, &tty);		/* Get parameters.. */
	ntty = tty;
	ntty.sg_flags |= CBREAK;
	ntty.sg_flags &= ~ECHO;
	ioctl(0, TIOCSETP, &ntty);		/* go to cbreak, ~echo */
	write(1, prompt, strlen(prompt));
	for (;;)  {
		if (read(0, buf, 1) != 1) {
			buf[0] = 'q';
			break;
		}
		if (index(cmds, buf[0]))
			break;
		write(1, "\7", 1);
	} 
	ioctl(0, TIOCSETP, &tty);
	write(1, "\n", 1);
	return buf[0];
}

static
unseen_transactions()
{
     return (dsc_public.current < dsc_public.m_info.last);
}
