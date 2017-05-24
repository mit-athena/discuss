/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * parse () -- Routines to parse words.
 *
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define EAT_SPACE(x) \
     while (isspace(*x)) x++;

int
get_word(spp,tpp,possible_delims,delimp)
char **spp;
char **tpp;
char *possible_delims;
char *delimp;
{
     register char *s,*u;

     s = *spp;
     EAT_SPACE(s);

     *tpp = s;
     for (; *s; s++) {
	  for (u = possible_delims; *u; u++) {
	       if (*u == *s)
		    goto found_delim;
	  }
     }

     /* Found null before delim.  */
     *delimp = '\0';

     /* Get rid of trailing spaces */
     trim(*tpp);

     *spp = ++s;
     return(0);

found_delim:
     *delimp = *s;
     *s++ = '\0';
     trim(*tpp);
     *spp = s;
     return(0);
}

static
lowercase(s)
char *s;
{
     while (*s) {
	  if (isupper(*s))
	       *s = tolower(*s);
	  s++;
     }
     return;
}

trim(s)
char *s;
{
     int j;
     char *t;

     j = strlen(s);
     t = s + j - 1;
     while (t >= s && isspace(*t))
	  *t-- = '\0';
}
