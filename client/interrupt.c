/*
 *
 *    Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    Developed by the MIT Student Information Processing Board (SIPB).
 *    For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 * Routines for dealing with ^C while running program.
 *
 * Stan picked the names, not me..
 */
#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/interrupt.c,v 1.5 1994-03-25 16:32:14 miki Exp $";
#endif lint

#include <signal.h>
#ifdef POSIX
struct sigaction act, oact;
#endif

int interrupt = 0;
#ifndef POSIX
static int (*old_interrupt_handler)() = SIG_DFL;
#endif
static void
interrupt_handler(dummy)
int dummy;
{
	interrupt = 1;
}

void
flag_interrupts()
{
	interrupt = 0;
#ifdef POSIX
       sigemptyset(&act.sa_mask);
       act.sa_flags = 0;
       act.sa_handler= (void (*)()) interrupt_handler;
       (void) sigaction(SIGINT, &act, &oact);
#else
	old_interrupt_handler = signal (SIGINT, interrupt_handler);
#endif
}

void
dont_flag_interrupts()
{
#ifdef POSIX
        (void) sigaction (SIGINT, &oact, (struct sigaction *)0);
#else
	(void) signal (SIGINT, old_interrupt_handler);
#endif
}
