/*
 *
 * set request for Discuss
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/set.c,v 1.3 1989-01-29 17:08:26 srz Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/set.c,v $
 * $Locker:  $
 *
 * Copyright (C) 1988 by the MIT Student Information Processing Board
 *
 */

#ifndef lint
static char rcsid_discuss_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/set.c,v 1.3 1989-01-29 17:08:26 srz Exp $";
#endif /* lint */

#include <stdio.h>
#include <string.h>

int set_seen();
int set_flag();

static struct set_req {
     char *name;				/* Name of request */
     int (*routine)();				/* Routine to call */
} sr[] = {"seen", set_seen,
	  "flag", set_flag};

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
