/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/sequencer.c,v $
 *	$Author: probe $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/sequencer.c,v 1.1 1991-07-05 00:48:53 probe Exp $
 *
 *	Copyright (C) 1987 by the Massachusetts Institute of Technology
 *
 *	$Log: not supported by cvs2svn $
 */

#ifndef lint
static char *rcsid_sequencer_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/sequencer.c,v 1.1 1991-07-05 00:48:53 probe Exp $";
#endif lint

#include "types.h"
#include "interface.h"
#include "globals.h"
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

static unseen_transactions()
{
	return 1;		/* XXX */
}
sequencer(argc, argv, ss_idx)
	int argc;
	char **argv;
        int ss_idx;
{
	int code;
	int cmd;
	ss_execute_line(ss_idx, "ckm", &code);
	if (code != 0) goto punt;

	while(changed_meetings()) {
		cmd = more_break("Hit space to go to next meeting: ", " q");
		switch(cmd) {
		case 'q': 
			return;

		case ' ':
		case 'n':
			break;
		}
		ss_execute_line(ss_idx, "nm", &code);
		if (code != 0) goto punt;
		
		while (unseen_transactions()) {
			cmd = more_break("Hit space for next transaction: ", " q");
			switch (cmd) {
			case 'q':
				return;
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
	ntty.sg_flags |= ~ECHO;
	ioctl(0, TIOCSETP, &ntty);		/* go to cbreak, ~echo */
	write(1, prompt, strlen(prompt));
	for (;;)  {
		if (read(0, buf, 1) != 1) {
			buf[0] = 'q';
			break;
		}
		if (index(cmds, buf[0]))
			break;
		buf[0]='\7';
		write(1, buf, 1);
	} 
	ioctl(0, TIOCSETP, &tty);
	return buf[0];
}

