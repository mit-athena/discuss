/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/* 
 * 
 * Created by: Mark W. Eichin <eichin@athena.mit.edu>
 * $Id: do_quote.c,v 1.4 2006-03-10 07:11:37 ghudson Exp $
 *
 */
#ifndef lint
static char rcsid_do_quote_c[] = "$Id: do_quote.c,v 1.4 2006-03-10 07:11:37 ghudson Exp $";
#endif /* lint */

#include <stdlib.h>

/*
 *
 * do_quote ()  -- requote a string so that Lisp won't barf on it.
 *  		   Basically requotes '"' and '\'.  Returns a malloc'd
 *		   copy of the original string.
 *
 *		   Warning:  This routine frees its argument, so the
 *		   canonical way of calling it is:  frep = do_quote(frep).
 *
 */

char *do_quote(s)
     char *s;
{
  char *ret, *t;

  t = ret = malloc(2*strlen(s)+1);

  while(*s) {
    switch(*s) {
    case '"':
    case '\\':
      *(t++) = '\\';
    default:
      *(t++) = *(s++);
    }
  }
  *t = '\0';
  
  return(ret);
}

    

