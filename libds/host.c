/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 *	$Id: host.c,v 1.7 1999-01-22 23:09:58 ghudson Exp $
 *
 * host.c () -- Program to return the default hostname, internet style.  
 *		Placed here because UNIX is too brain-damaged to have this
 *		routine (gethostname is doesn't return unique names.  This
 *		caches the result.
 *
 */
#ifndef lint
static char *rcsid_host_c = "$Id: host.c,v 1.7 1999-01-22 23:09:58 ghudson Exp $";
#endif lint

#include <string.h>
#include <netdb.h>
#include <ctype.h>

char *
local_host_name ()
{
     static char myhostname [100];
     struct hostent *hp;

     if (myhostname [0] != '\0')
	  return (myhostname);

     gethostname (myhostname, sizeof (myhostname));

     hp = gethostbyname (myhostname);
     if (hp == 0)
	  return(myhostname);

     strcpy (myhostname, hp -> h_name);
     return (myhostname);
}

int namcmp(str1, str2)
register char *str1, *str2;
{
     register char c1,c2;

     while (*str1 && *str2) {
	  if (*str1 == *str2) {
		  str1++;
		  str2++;
	  } else if (isalpha (*str1)) {
	       c1 = *str1++;
	       c2 = *str2++;
	       if (islower (c1))
		    c1 = toupper (c1);
	       if (islower (c2))
		    c2 = toupper (c2);
	       if (c1 == c2)
		    continue;
	       return(1);
	  } else
	       return (1);
     }

     return (*str1 || *str2);
}

