/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/rn.c,v $
 *	$Author: srz $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/rn.c,v 1.1 1987-10-24 19:48:02 srz Exp $
 *
 *	Copyright (C) 1987 by the Massachusetts Institute of Technology
 *
 *	$Log: not supported by cvs2svn $
 */

#ifndef lint
static char *rcsid_update_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/rn.c,v 1.1 1987-10-24 19:48:02 srz Exp $";
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
	while(changed_meetings()) {
	        if (interrupt)
		     break;
		cmd = more_break("Hit space to go to next meeting: ", " q");
	        if (interrupt)
		     break;
	        printf("\n");
		switch(cmd) {
		case 'q': 
			goto punt;

		case ' ':
		case 'n':
			break;
		}
		ss_execute_line(ss_idx, "nm", &code);
		if (code != 0) goto punt;
		
		while (unseen_transactions()) {
		        if (interrupt)
			     break;

			cmd = more_break("Hit space for next transaction: ", " q");
			if (interrupt)
			     break;
		        printf("\n");
			switch (cmd) {
			case 'q':
				goto punt;
			case ' ':
			case 'n':
				break;
			}
			ss_execute_line(ss_idx, "next", &code);
			if (code != 0) goto punt;
		}
	}
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
     return (dsc_public.highest_seen < dsc_public.m_info.last);
}
