/*
 *
 * Status request for DISCUSS
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/status.c,v 1.2 1986-10-29 10:29:40 srz Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/status.c,v $
 * $Locker:  $
 *
 * Copyright (C) 1986 by the MIT Student Information Processing Board
 *
 * $Log: not supported by cvs2svn $
 * Revision 1.1  86/08/09  20:16:10  spook
 * Initial revision
 * 
 *
 */

#include <stdio.h>
#include "../include/ss.h"
#include "../include/interface.h"
#include "../include/config.h"
#include "globals.h"

status(sci_idx, argc, argv)
	int sci_idx;
	int argc;
	char **argv;
{
	printf("Discuss version %s\n", CURRENT_VERSION);
	if (!dsc_public.attending) {
		printf("No current meeting\n");
		return;
	}
	if (dsc_public.m_info.public_flag)
		printf("Attending %s (%s) meeting (public)\n",
		       dsc_public.m_info.long_name, rindex(dsc_public.m_info.location, '/')+1);
	else
		printf("Attending %s (%s) meeting\n",
		       dsc_public.m_info.long_name, rindex(dsc_public.m_info.location, '/')+1);
	if (dsc_public.current == 0) {
		printf("No current transaction selected; %d highest.\n",
		       dsc_public.m_info.last);
		return;
	}
	printf("Transaction %d of %d.\n", dsc_public.current, dsc_public.m_info.last);
	return;
}
