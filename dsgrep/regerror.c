/*
 *
 *      Copyright (C) 1989 by the Massachusetts Institute of Technology
 *      Developed by the MIT Student Information Processing Board (SIPB).
 *      For copying information, see the file mit-copyright.h in this release.
 *
 */

#ifndef lint
#ifndef SABER
static char *RCSid = "$Id: regerror.c,v 1.2 1999-01-22 23:09:40 ghudson Exp $";
#endif
#endif

#include <stdio.h>
#include <stdlib.h>
void regerror(msg)
     char *msg;
{
  fprintf(stderr,"dsgrep: Error in regular expression %s\n",msg);
  exit(1);
}
