/*
 *
 * Status request for DISCUSS
 *
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/status.c,v 1.1 1986-08-09 20:16:10 spook Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/client/status.c,v $
 * $Locker:  $
 *
 * Copyright (C) 1986 by the MIT Student Information Processing Board
 *
 * $Log: not supported by cvs2svn $
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
	if (cur_mtg == (char *)NULL) {
		printf("No current meeting\n");
		return;
	}
	if (m_info.public_flag)
		printf("Attending %s (%s) meeting (public)\n",
		       m_info.long_name, rindex(m_info.location, '/')+1);
	else
		printf("Attending %s (%s) meeting\n",
		       m_info.long_name, rindex(m_info.location, '/')+1);
	if (cur_trans == -1) {
		printf("No current transaction selected; %d highest.\n",
		       m_info.last);
		return;
	}
	printf("Transaction %d of %d.\n", cur_trans, m_info.last);
	return;
}
