/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * Status request for DISCUSS
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/status.c,v 1.15 1996-09-19 22:28:31 ghudson Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/status.c,v $
 * $Locker:  $
 *
 */
#ifndef lint
static char rcsid_discuss_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/status.c,v 1.15 1996-09-19 22:28:31 ghudson Exp $";
#endif lint

#include <stdio.h>
#include <ctype.h>
#include <ss/ss.h>
#include <discuss/discuss.h>
#include "config.h"
#include "globals.h"
#include <string.h>

static int sending_msg;
static char msg_buf[64];

static
add_msg(str)
     char *str;
{
     if (!sending_msg)
	  msg_buf[0] = '\0';
     else
	  strcat(msg_buf, "; ");
     strcat(msg_buf, str);
     sending_msg++;
}

static
put_msg()
{
     if (sending_msg) {
	  msg_buf[0] = toupper(msg_buf[0]);
	  printf(" (%s.)", msg_buf);
     }
     sending_msg = 0;
}

status(argc, argv)
	int argc;
	char **argv;
{
     sending_msg = 0;
     printf("Discuss version %s.\n", dsc_version);
     if (!dsc_public.attending) {
	  printf("No current meeting.\n");
	  return;
     }

     printf("Attending %s (%s) meeting.",
	    dsc_public.m_info.long_name,
	    strrchr(dsc_public.m_info.location, '/')+1);
     if (dsc_public.m_info.public_flag)
	  add_msg("public");
     if (acl_is_subset("c", dsc_public.m_info.access_modes)) 
	  add_msg("you are a chairman");
     if (!acl_is_subset("w", dsc_public.m_info.access_modes)) {
	  if (!acl_is_subset("a", dsc_public.m_info.access_modes)) 
	       add_msg("read only");
	  else
	       add_msg("reply only");
     } else if (!acl_is_subset("a", dsc_public.m_info.access_modes))
	  add_msg("no replies");
     put_msg();

     if (dsc_public.current == 0)
	  printf("\nNo current transaction selected; %d highest.\n",
		 dsc_public.m_info.last);
     else
	  printf("\nTransaction %d of %d.\n", dsc_public.current,
		 dsc_public.m_info.last);
}
