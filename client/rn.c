/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/rn.c,v $
 *	$Author: probe $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/rn.c,v 1.10 1991-07-22 01:28:26 probe Exp $
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.9  89/06/02  23:38:39  srz
 * Added standard copyright notice.
 * 
 * Revision 1.8  89/05/19  16:58:18  srz
 * Declared static functions in advance.
 * 
 * Revision 1.7  89/05/08  02:47:31  srz
 * jik's fix to stop printing twice.
 * 
 * Revision 1.6  89/01/05  01:58:44  raeburn
 * replaced included header files with <discuss/discuss.h>
 * 
 * Revision 1.5  88/04/21  16:04:46  srz
 * Added ^R to redisplay current transaction (courtesy of jik)
 * 
 * Revision 1.4  88/04/20  16:34:04  srz
 * Added catchup, loop for '?' on first prompt.
 * 
 * Revision 1.3  88/01/15  23:11:33  srz
 * Fixed bug where new meetings caused problems for "next"
 * 
 * Revision 1.2  87/11/07  02:50:38  srz
 * Added new commands ('r', 't', 'p', '?'), and fixed bug that Mark reported
 * about trying to reprint the same transaction over and over again when
 * quitting out of 'more'.
 * 
 * Revision 1.1  87/10/24  19:48:02  srz
 * Initial revision
 * 
 */

#ifndef lint
static char rcsid_update_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/rn.c,v 1.10 1991-07-22 01:28:26 probe Exp $";
#endif /* lint */

#include <discuss/discuss.h>
#include "globals.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/ioctl.h>

#ifdef POSIX
#include <termios.h>
#endif

static unseen_transactions();

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

	for (;;) {
	     cmd = more_break("Hit space to go to next meeting: ", " qn?");
	     if (interrupt)
		  goto done;
	     printf("\n");
	     switch(cmd) {
	     case 'q':
		  goto done;
	     case ' ':
	     case 'n':
		  goto first_meeting;
	     case '?':
		  printf("List of possible responses:\n\n");
		  printf("<space>,n\tNext meeting\n");
		  printf("q\t\tQuit from read_new\n");
		  printf("?\t\tShow this list\n\n");
		  break;
	     }
	}
first_meeting:
	ss_execute_line(ss_idx, "nm", &code);
	if (code != 0) goto punt;

	while (1) {			/* we get out when changed_meetings is false */
	        if (interrupt)
		     break;
		
		while (unseen_transactions()) {
		        if (interrupt)
			     break;

			cmd = more_break("Hit space for next transaction: ", " qcnp\022tr?");
			if (interrupt)
			     break;
		        printf("\n");
			switch (cmd) {
			case 'q':
				goto done;
			case ' ':
			case 'n':
				if (dsc_public.current == 0)
					ss_execute_line(ss_idx, "pr first", &code);
				else
					ss_execute_line(ss_idx, "next", &code);
				if (code != 0) goto punt;
				break;
			case 'c':
				catchup(0,0);
				break;
			case 'p':
				ss_execute_line(ss_idx, "prev", &code);
				if (code != 0) goto punt;
				break;
			case '\022':
				ss_execute_line(ss_idx, "pr", &code);
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
				printf("c\t\tCatch up on transactions in meeting\n");
				printf("p\t\tPrevious transaction\n");
				printf("^R\t\tReview current transaction\n");
				printf("q\t\tQuit from read_new\n");
				printf("r\t\tReply to current transaction\n");
				printf("t\t\tEnter a new transaction\n");
				printf("?\t\tShow this list\n\n");
				break;
			}
		}

		if (!changed_meetings())
		     break;

		cmd = more_break("Hit space to go to next meeting: ", " qn?ptr\022");
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
                case '\022':
		     ss_execute_line(ss_idx, "pr", &code);
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
		     printf("^R\t\tReview the current transaction\n");
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
		if (index(cmds, buf[0]))
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

static
unseen_transactions()
{
     return (dsc_public.current < dsc_public.m_info.last);
}
