/*
 *
 *      Copyright (C) 1989 by the Massachusetts Institute of Technology
 *      Developed by the MIT Student Information Processing Board (SIPB).
 *      For copying information, see the file mit-copyright.h in this release.
 *
 */

#ifndef lint
#ifndef SABER
static char *RCSid = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/dsgrep/regerror.c,v 1.1 1991-05-22 11:25:16 lwvanels Exp $";
#endif
#endif

#include <stdio.h>
void regerror(msg)
     char *msg;
{
  fprintf(stderr,"dsgrep: Error in regular expression %s\n",msg);
  exit(1);
}
