/*
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/host.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/host.c,v 1.3 1987-04-11 00:05:55 srz Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 * host.c () -- Program to return the default hostname, internet style.  
 *		Placed here because UNIX is too brain-damaged to have this
 *		routine (gethostname is doesn't return unique names.  This
 *		caches the result.
 *
 *	$Log: not supported by cvs2svn $
 *
 */
#ifndef lint
static char *rcsid_host_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/host.c,v 1.3 1987-04-11 00:05:55 srz Exp $";
#endif lint

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

