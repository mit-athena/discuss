/*
 *
 * host.c () -- Program to return the default hostname, internet style.  
 *		Placed here because UNIX is too brain-damaged to have this
 *		routine (gethostname is doesn't return unique names.  This
 *		caches the result.
 *
 */

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
	       *str1++,*str2++;
	       continue;
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

