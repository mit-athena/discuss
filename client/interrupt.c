/*
 * Routines for dealing with ^C while running program.
 *
 * Stan picked the names, not me..
 */
#ifndef lint
static char *rcsid_discuss_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/interrupt.c,v 1.2 1987-04-10 23:49:22 srz Exp $";
#endif lint

#include <signal.h>

int interrupt = 0;
static int (*old_interrupt_handler)() = SIG_DFL;

static void
interrupt_handler()
{
	interrupt = 1;
}

void
flag_interrupts()
{
	interrupt = 0;
	old_interrupt_handler = signal (SIGINT, interrupt_handler);
}

void
dont_flag_interrupts()
{
	(void) signal (SIGINT, old_interrupt_handler);
}
