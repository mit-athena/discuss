/*
 * Routines for dealing with ^C while running program.
 *
 * Stan picked the names, not me..
 */

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
