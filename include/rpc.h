/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * proc.h -- defines for RPC procedure declarations.
 *
 */

#define MAXARGS 20

#define INTTYPE 1
#define STRTYPE 2
#define BOOLTYPE 3
#define TFILETYPE 4

struct proc_table {
     int numargs;
     char argtype [MAXARGS];
};


/* defined procs */
#define ADD_TRN 1
#define GET_TRN_INFO 2
#define DELETE_TRN 3
#define RETRIEVE_TRN 4
#define CREATE_MTG 5
#define OLD_GET_MTG_INFO 6
#define START_MTG_INFO 7
#define NEXT_MTG_INFO 8
#define GET_TRN 9
#define REMOVE_MTG 10
#define UPDATED_MTG 11
#define GET_MTG_INFO 12
#define GET_ACL 13
#define GET_ACCESS 14
#define SET_ACCESS 15
#define DELETE_ACCESS 16
#define WHO_AM_I 17
#define GET_TRN_INFO2 18
#define GET_SERVER_VERSION 19
#define SET_TRN_FLAGS 20
#define ADD_TRN2 21
#define GET_TRN_INFO3 22

#define REPLY_TYPE 440
#define PROC_BASE 400
#define TFILE_BLK 500
#define KRB_TICKET 501
#define UNKNOWN_CALL 502
#define KRB_TICKET2 503
#define TICKET_REPLY 504

/* Server version numbers */
#define SERVER_0 0
#define SERVER_1 1
#define SERVER_2 2
#define SERVER_3 3

extern int rpc_err;

/* USP stuff, that the caller shouldn't know about */
#include <stdio.h>
#include "usp.h"

typedef USPStream *rpc_conversation;

rpc_conversation open_rpc();

#include "rpc_et.h"
