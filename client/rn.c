/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *	$Id: rn.c,v 1.15 1999-01-22 23:09:33 ghudson Exp $
 *
 */

#ifndef lint
static char rcsid_update_c[] =
    "$Id: rn.c,v 1.15 1999-01-22 23:09:33 ghudson Exp $";
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

rn(argc, argv, ss_idx)
	int argc;
	char **argv;
        int ss_idx;
{
	int code = 0;
	int cmd;

	printf("Checking meetings...\n");
	fflush(stdout);

	strcpy(ss_buf, "ckm");
	ss_execute_line(ss_idx, ss_buf, &code);
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
	strcpy(ss_buf, "nm");
	ss_execute_line(ss_idx, ss_buf, &code);
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
				    strcpy(ss_buf, "pr first");
				else
				    strcpy(ss_buf, "next");
				ss_execute_line(ss_idx, ss_buf, &code);
				if (code != 0) goto punt;
				break;
			case 'c':
				catchup(0,0);
				break;
			case 'p':
				strcpy(ss_buf, "prev");
				ss_execute_line(ss_idx, ss_buf, &code);
				if (code != 0) goto punt;
				break;
			case '\022':
				strcpy(ss_buf, "pr");
				ss_execute_line(ss_idx, ss_buf, &code);
				if (code != 0) goto punt;
				break;
			case 'r':
				strcpy(ss_buf, "reply");
				ss_execute_line(ss_idx, ss_buf, &code);
				if (code != 0) goto punt;
				break;
			case 't':
				strcpy(ss_buf, "talk");
				ss_execute_line(ss_idx, ss_buf, &code);
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
		     strcpy(ss_buf, "nm");
		     ss_execute_line(ss_idx, ss_buf, &code);
		     if (code != 0) goto punt;
		     break;
		case 'p':
		     strcpy(ss_buf, "prev");
		     ss_execute_line(ss_idx, ss_buf, &code);
		     if (code != 0) goto punt;
		     break;
                case '\022':
		     strcpy(ss_buf, "pr");
		     ss_execute_line(ss_idx, ss_buf, &code);
		     if (code != 0) goto punt;
		     break;
		case 'r':
		     strcpy(ss_buf, "reply");
		     ss_execute_line(ss_idx, ss_buf, &code);
		     if (code != 0) goto punt;
		     break;
		case 't':
		     strcpy(ss_buf, "talk");
		     ss_execute_line(ss_idx, ss_buf, &code);
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

static
unseen_transactions()
{
     return (dsc_public.current < dsc_public.m_info.last);
}
