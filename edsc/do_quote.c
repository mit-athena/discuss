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
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/do_quote.c,v $
 * $Author: srz $
 *
 */
#ifndef lint
static char rcsid_do_quote_c[] = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/edsc/do_quote.c,v 1.1 1989-06-02 23:43:18 srz Exp $";
#endif lint

char *malloc();
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

    

