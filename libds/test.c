/*
 *
 * test ()  Test out the dsname routines.
 *
 */

#include "../include/dsname.h"
#include <ctype.h>
#include <stdio.h>

#define NULL 0

main ()
{
     name_blk *nb,nblk;
     char *host, *pathname;
     int code;
     int num,i;
     
     get_mtg_location ("borax.lcs.mit.edu:34343343/usr/srz/first", &host, &pathname, &code);
     if (code != 0)
	  printf ("get_mtg_location failed, code = %d\n", code);
     
     expand_mtg_set ("LCS.MIT.EDU", "srz", "*", &nb, &num);
     if (nb == NULL)
	  printf ("expand mtg set failed.\n");
     else {
	  for (i = 0; i < num; i++, nb++) {
	       printf ("realm= %s, user= %s, unique= %s, attend= %d, last=%d\n",
		       nb ->realm,nb->user,nb->unique_id,nb->date_attended,nb->last);
	  }
     }

     get_mtg_unique_id ("LCS.MIT.EDU", "srz", "discuss", &nblk, &code);
     if (code != 0) {
	  printf ("get mtg unique_id failed\n");
     } else {
	  printf ("realm= %s, user= %s, unique= %s, attend= %d, last=%d\n",
		  nblk.realm,nblk.user,nblk.unique_id,nblk.date_attended,nblk.last);
     }
     return;
}

