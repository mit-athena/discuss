/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * disserve.c -- Simple top level program for test.
 *
 * $Id: disserve.c,v 1.5 1999-01-22 23:10:17 ghudson Exp $
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
#ifdef LOG_DAEMON
    openlog ("discuss", LOG_PID, LOG_DAEMON);
#else
    openlog ("discuss", LOG_PID);
#endif
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
