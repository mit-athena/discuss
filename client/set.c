/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * set request for Discuss
 *
 * $Id: set.c,v 1.6 1999-01-22 23:09:34 ghudson Exp $
 *
 */

#ifndef lint
static char rcsid_discuss_c[] =
    "$Id: set.c,v 1.6 1999-01-22 23:09:34 ghudson Exp $";
#endif /* lint */

#include <stdio.h>
#include <string.h>

int set_seen();

static struct set_req {
     char *name;				/* Name of request */
     int (*routine)();				/* Routine to call */
} sr[] = {"seen", set_seen};

#define NUM_SET_REQUESTS (sizeof (sr) / sizeof (struct set_req))

set_cmd(argc, argv)
     int argc;
     char **argv;
{
     int code,i;

     if (argc == 1)
	  goto usage;

     for (i = 0; i < NUM_SET_REQUESTS; i++) {
	  if (!strcmp (argv[1], sr[i].name)) {
	       (*(sr[i].routine))(--argc,++argv);
	       return;
	  }
     }

     printf("Possible set requests are:\n");
     for (i = 0; i < NUM_SET_REQUESTS; i++) {
	  printf("set %s\n", sr[i].name);
     }

     return;

usage:
     fprintf(stderr, "Usage:  %s <option>\n", argv[0]);
}
