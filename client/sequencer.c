/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/sequencer.c,v $
 *	$Author: ghudson $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/sequencer.c,v 1.4 1996-09-19 22:28:30 ghudson Exp $
 *
 *	Copyright (C) 1987 by the Massachusetts Institute of Technology
 *
 *	$Log: not supported by cvs2svn $
 *	Revision 1.3  1993/03/07 06:22:02  probe
 *	ss_execute_line should not be called with read-only strings.
 *
 * Revision 1.2  91/07/22  01:29:03  probe
 * POSIX integration
 * 
 * Revision 1.1  91/07/05  00:48:53  probe
 * Initial revision
 * 
 */

#ifndef lint
static char *rcsid_sequencer_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/sequencer.c,v 1.4 1996-09-19 22:28:30 ghudson Exp $";
#endif lint

#include "types.h"
#include "interface.h"
#include "globals.h"
#include <sys/types.h>
#include <sys/file.h>
#include <sys/ioctl.h>

static char ss_buf[512];

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
	strcpy(ss_buf, "ckm");
	ss_execute_line(ss_idx, ss_buf, &code);
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
		strcpy(ss_buf, "nm");
		ss_execute_line(ss_idx, ss_buf, &code);
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
			strcpy(ss_buf, "next");
			ss_execute_line(ss_idx, ss_buf, &code);
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
	int arg;
	char buf[1];
#ifndef POSIX
	struct sgttyb tty, ntty;

	arg = FREAD;				/* Flush pending input */
	ioctl(0, TIOCFLUSH, &arg);
	ioctl(0, TIOCGETP, &tty);		/* Get parameters.. */
	ntty = tty;
	ntty.sg_flags |= CBREAK;
	ntty.sg_flags &= ~ECHO;
	ioctl(0, TIOCSETP, &ntty);		/* go to cbreak, ~echo */
#else
	struct termios tty, ntty;

	(void) tcflush(0, TCIFLUSH);
	(void) tcgetattr(0, &tty);
	ntty = tty;
	ntty.c_cc[VMIN] = 1;
	ntty.c_cc[VTIME] = 0;
        ntty.c_iflag &= ~(ICRNL);
        ntty.c_lflag &= ~(ICANON|ISIG|ECHO);
	(void) tcsetattr(0, TCSANOW, &ntty);
#endif
	write(1, prompt, strlen(prompt));
	for (;;)  {
		if (read(0, buf, 1) != 1) {
			buf[0] = 'q';
			break;
		}
		if (strchr(cmds, buf[0]))
			break;
		write(1, "\7", 1);
	}
#ifdef POSIX
	(void) tcsetattr(0, TCSANOW, &tty);
#else
	ioctl(0, TIOCSETP, &tty);
#endif
	write(1, "\n", 1);
	return buf[0];
}
