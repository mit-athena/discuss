/*
 *
 * disserve.c -- Simple top level program for test.
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/server/disserve.c,v 1.2 1988-10-08 01:38:16 raeburn Exp $
 * $Log: not supported by cvs2svn $
 */

#include <stdio.h>
#include <syslog.h>
extern char *error_message();

int main (argc,argv)
    int argc;
    char **argv;
{
    int code;

#ifndef SUBPROC
    openlog ("discuss", LOG_PID, LOG_DAEMON);
#endif
    init_rpc("discuss",&code);
    if (code) {
#ifndef SUBPROC
	syslog (LOG_ERR, "RPC initialization failed: %s",
		error_message (code));
#else
	fprintf(stderr, "%s\n", error_message(code));
#endif
	exit(1);
    }
    while (!code)
	recvit (&code);
    return 0;
}
