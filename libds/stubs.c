     /********************************************************************
      *                                                                  *
      *         Copyright (c) 1985 by                                    *
      *            Lewis Makepeace and Stan Zanarotti                    *
      *                                                                  *
      *                                                                  *
      *         All rights reserved.                                     *
      *                                                                  *
      ********************************************************************/

/*
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/stubs.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/stubs.c,v 1.13 1989-03-27 00:33:07 srz Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 * stubs.c -- These are stubs that handle the calling of routines.
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.12  89/01/29  17:17:59  srz
 * Added flag routines, and version check.
 * 
 * Revision 1.11  89/01/04  20:52:00  raeburn
 * Still fixing includes..
 * 
 * Revision 1.10  88/10/16  14:04:50  raeburn
 * Changed includes and type names.
 * 
 * Revision 1.9  87/10/06  02:36:45  spook
 * Fixed error detected by High C compiler.
 * 
 * Revision 1.8  87/07/20  20:57:47  srz
 * Changed name of whoami to dwhoami (too generic of a name)
 * 
 * Revision 1.7  87/07/08  02:00:16  wesommer
 * Added whoami(), dsc_whoami() (the server support was always there.
 * 
 * Revision 1.6  87/06/27  01:10:55  spook
 * *** empty log message ***
 * 
 * Revision 1.5  87/04/11  00:06:13  srz
 * Added RCS junk
 * 
 *
 */
#ifndef lint
static char rcsid_stubs_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/stubs.c,v 1.13 1989-03-27 00:33:07 srz Exp $";
#endif lint

/* Derived from CORE.PAS 06/21/86 by SRZ */

#include <discuss/interface.h>
#include "rpc.h"
#include <discuss/tfile.h>
#include <discuss/acl.h>

extern bool recvbool();
extern char *recvstr();
dsc_acl *recv_acl();

static char discuss[] = "discuss";

/*
 *
 * add_trn () --
 * adds a transaction to the current meeting, either as a reply or an
 * original transaction.  Returns an error code, and the transaction number
 * given to the transaction
 *
 */
add_trn (mtg_name, source_file, subject, reply_trn, result_trn, result)
char *mtg_name;
tfile source_file;
char *subject;
trn_nums reply_trn;		/* trn replying to;  0 if original */
trn_nums *result_trn;		/* trn number given to added trn */
int *result;
{
     startsend(ADD_TRN);
     if (rpc_err) { *result = rpc_err; return; }
     sendstr(mtg_name);
     sendfile(source_file);
     sendstr(subject);
     sendint(reply_trn);
     sendit(discuss);
     senddata(source_file);
     if (rpc_err) { *result = rpc_err; return; }
     recvreply ();
     if (rpc_err) { *result = rpc_err; return; }
     *result_trn = recvint();
     *result = recvint();
     if (rpc_err) { *result = rpc_err; return; }
     return;
}

/*
 *
 * get_trn_info () --
 * returns information about the given transaction in info, with an error
 * code as its return argument
 *
 */
get_trn_info (mtg_name, trn, info, result)
char *mtg_name;
trn_nums trn;
trn_info *info;
int *result;
{
     info -> subject = NULL;			/* Not allocated */
     info -> author = NULL;
     startsend(GET_TRN_INFO);
     if (rpc_err) { *result = rpc_err; return; }
     sendstr(mtg_name);
     sendint(trn);
     sendit(discuss);
     if (rpc_err) { *result = rpc_err; return; }
     recvreply();
     if (rpc_err) { *result = rpc_err; return; }
     recv_trn_info(info);
     *result = recvint();
     if (rpc_err) { *result = rpc_err; return; }
     return;
}

/*
 *
 * get_trn_info2 () --
 * returns information about the given transaction in info, with an error
 * code as its return argument
 *
 */
get_trn_info2 (mtg_name, trn, info, result)
char *mtg_name;
trn_nums trn;
trn_info2 *info;
int *result;
{
     info -> subject = NULL;			/* Not allocated */
     info -> author = NULL;
     startsend(GET_TRN_INFO2);
     if (rpc_err) { *result = rpc_err; return; }
     sendstr(mtg_name);
     sendint(trn);
     sendit(discuss);
     if (rpc_err) { *result = rpc_err; return; }
     recvreply();
     if (rpc_err) { *result = rpc_err; return; }
     recv_trn_info2(info);
     *result = recvint();
     if (rpc_err) { *result = rpc_err; return; }
     return;
}


/*
 *
 * delete_trn () --
 * deletes the given transaction from the current meeting.  Returns an
 * error code
 *
 */
delete_trn (mtg_name, trn, result)
char *mtg_name;
trn_nums trn;
int *result;
{
     startsend(DELETE_TRN);
     if (rpc_err) { *result = rpc_err; return; }
     sendstr(mtg_name);
     sendint(trn);
     sendit(discuss);
     if (rpc_err) { *result = rpc_err; return; }
     recvreply();
     if (rpc_err) { *result = rpc_err; return; }
     *result = recvint();
     if (rpc_err) { *result = rpc_err; return; }
     return;
}

/*
 *
 * retrieve_trn () --
 * retrieves a previously deleted transaction from the current meeting, if
 * possible.  trn must refer to a deleted transaction.  An error code is
 * returned
 *
 */
retrieve_trn (mtg_name, trn, result)
char *mtg_name;
trn_nums trn;
int *result;
{
     startsend(RETRIEVE_TRN);
     if (rpc_err) { *result = rpc_err; return; }
     sendstr (mtg_name);
     sendint (trn);
     sendit(discuss);
     if (rpc_err) { *result = rpc_err; return; }
     recvreply();
     if (rpc_err) { *result = rpc_err; return; }
     *result = recvint();
     if (rpc_err) { *result = rpc_err; return; }
     return;
}



/*
 *
 * create_mtg () --
 * Creates a new meeting with the given long_mtg name, where location is the
 * it's place in the hierarchy, and the long_mtg_name is its canonical name.
 * The chairman of the new meeting is the current user.
 *
 */
create_mtg (location, long_mtg_name, public, result)
char *location,*long_mtg_name;
bool public;
int *result;
{
     startsend(CREATE_MTG);
     if (rpc_err) { *result = rpc_err; return; }
     sendstr(location);
     sendstr(long_mtg_name);
     sendbool(public);
     sendit(discuss);
     if (rpc_err) { *result = rpc_err; return; }
     recvreply();
     if (rpc_err) { *result = rpc_err; return; }
     *result = recvint();
     if (rpc_err) { *result = rpc_err; return; }
     return;
}


/*
 *
 * old_get_mtg_info () --
 * returns information about the given meeting.  Return argument is an
 * error code
 *
 */
old_get_mtg_info (mtg_name, info, result)
char *mtg_name;
mtg_info *info;
int *result;
{
     info -> location = NULL;
     info -> long_name = NULL;
     info -> chairman = NULL;
     startsend(OLD_GET_MTG_INFO);
     if (rpc_err) { *result = rpc_err; return; }
     sendstr(mtg_name);
     sendit(discuss);
     if (rpc_err) { *result = rpc_err; return; }
     recvreply();
     if (rpc_err) { *result = rpc_err; return; }
     old_recv_mtg_info(info);
     *result = recvint();
     if (rpc_err) { *result = rpc_err; return; }
     return;
}
/*
 *
 * get_mtg_info () --
 * returns information about the given meeting.  Return argument is an
 * error code
 *
 */
get_mtg_info (mtg_name, info, result)
char *mtg_name;
mtg_info *info;
int *result;
{
     info -> location = NULL;
     info -> long_name = NULL;
     info -> chairman = NULL;
     info -> access_modes = NULL;
     startsend(GET_MTG_INFO);
     if (rpc_err) { *result = rpc_err; return; }
     sendstr(mtg_name);
     sendit(discuss);
     if (rpc_err) { *result = rpc_err; return; }
     recvreply();
     if (rpc_err) { *result = rpc_err; return; }
     recv_mtg_info(info);
     *result = recvint();
     if (rpc_err) { *result = rpc_err; return; }
     return;
}

/*
 *
 * get_trn () --
 * gets the given transaction, and feeds it through dest_file.  Returns an
 * error code
 *
 */
get_trn (mtg_name, trn, dest_file, result)
char *mtg_name;
trn_nums trn;
tfile dest_file;
int *result;
{
     startsend(GET_TRN);
     if (rpc_err) { *result = rpc_err; return; }
     sendstr(mtg_name);
     sendint(trn);
     sendfile(dest_file);
     sendit(discuss);
     if (rpc_err) { *result = rpc_err; return; }
     recvdata(dest_file);
     recvreply();
     if (rpc_err) { *result = rpc_err; return; }
     *result = recvint();
     if (rpc_err) { *result = rpc_err; return; }
     return;
}

/*
 *
 * remove_mtg () --
 * removes the given meeting from the meeting list.  This should
 * probably go ahead and physically remove the contents of the meeting
 * but that's not obvious if we have the access to do that.
 *
 */
remove_mtg (mtg_name, result)
char *mtg_name;
int *result;
{
     startsend(REMOVE_MTG);
     if (rpc_err) { *result = rpc_err; return; }
     sendstr(mtg_name);
     sendit(discuss);
     if (rpc_err) { *result = rpc_err; return; }
     recvreply();
     if (rpc_err) { *result = rpc_err; return; }
     *result = recvint();
     if (rpc_err) { *result = rpc_err; return; }
     return;
}
/*
 *
 * updated_mtg () -- Quick procedure to check if the meeting is updated
 *		     with respect to a given time and transaction number.
 *
 */
updated_mtg (mtg_name, date_attended, last, updated, result)
char *mtg_name;
int date_attended, last;
bool *updated;
int *result;
{
     startsend(UPDATED_MTG);
     if (rpc_err) { *result = rpc_err; return; }
     sendstr(mtg_name);
     sendint(date_attended);
     sendint(last);
     sendit(discuss);
     if (rpc_err) { *result = rpc_err; return; }
     recvreply();
     if (rpc_err) { *result = rpc_err; return; }
     *updated = recvbool();
     *result = recvint();
     if (rpc_err) { *result = rpc_err; return; }
     return;
}
#define rpccheck if (rpc_err) { *result = rpc_err; return; }
/*
 * get_acl () -- Get access control list.
 */
get_acl(mtg_name, result, list)
	char *mtg_name;
	int *result;		/* RETURN */
	dsc_acl **list;		/* RETURN */
{
	startsend(GET_ACL);
	rpccheck;
	sendstr(mtg_name);
	sendit(discuss);
	rpccheck;
	recvreply();
	if (rpc_err) { *result = rpc_err; return; }
	*result = recvint();
	*list = recv_acl();
	rpccheck;
	return;
}

get_access(mtg_name, princ_name, modes, result)
	char *mtg_name;
	char *princ_name;
	char **modes;		/* RETURN */
	int *result;		/* RETURN */
{
	*modes = NULL;     
	startsend(GET_ACCESS);
	rpccheck;
	sendstr(mtg_name);
	sendstr(princ_name);
	sendit(discuss);
	rpccheck;
	recvreply();
	if (rpc_err) { *result = rpc_err; return; }
	*modes = recvstr();
	*result = recvint();
	rpccheck;
	return;
}
set_access(mtg_name, princ_name, modes, result)
	char *mtg_name;
	char *princ_name;
	char *modes;
	int *result;		/* RETURN */
{
	startsend(SET_ACCESS);
	rpccheck;
	sendstr(mtg_name);
	sendstr(princ_name);
	sendstr(modes);
	sendit(discuss);
	rpccheck;
	recvreply();
	if (rpc_err) { *result = rpc_err; return; }
	*result = recvint();
	rpccheck;
	return;
}

delete_access(mtg_name, princ_name, result)
	char *mtg_name;
	char *princ_name;
	int *result;		/* RETURN */
{
	startsend(DELETE_ACCESS);
	rpccheck;
	sendstr(mtg_name);
	sendstr(princ_name);
	sendit(discuss);
	rpccheck;
	recvreply();
	if (rpc_err) { *result = rpc_err; return; }
	*result = recvint();
	rpccheck;
	return;
}

set_trn_flags(mtg_name, trn, flags, result)
char *mtg_name;
trn_nums trn;
int flags;
int *result;
{
     startsend(SET_TRN_FLAGS);
     if (rpc_err) { *result = rpc_err; return; }
     sendstr(mtg_name);
     sendint(trn);
     sendint(flags);
     sendit(discuss);
     if (rpc_err) { *result = rpc_err; return; }
     recvreply();
     if (rpc_err) { *result = rpc_err; return; }
     *result = recvint();
     if (rpc_err) { *result = rpc_err; return; }
     return;
}


dwhoami(ident, result)
	char **ident;
	int *result;
{
	*result = 0;
	*ident = NULL;
	startsend(WHO_AM_I);
	rpccheck;
	sendit(discuss);
	rpccheck;
	recvreply();
	if (rpc_err) { *result = rpc_err; return; }
	*ident = recvstr();
	rpccheck;
	return;
}

get_server_version()
{
        int vers_num,result;

        startsend(GET_SERVER_VERSION);
	if (rpc_err) return (SERVER_0);
	sendit(discuss);
	if (rpc_err) return (SERVER_0);
	recvreply();
	if (rpc_err) return (SERVER_0);
	vers_num = recvint();
	if (rpc_err) return (SERVER_0);
	result = recvint();
	if (rpc_err) return (SERVER_0);
	return(vers_num);
}

/*
 *
 * recv_trn_info -- recv a trn_info struct.
 *
 */
recv_trn_info(tinfo)
trn_info *tinfo;
{
     tinfo -> version = recvint ();
     tinfo -> current = recvint ();
     tinfo -> prev = recvint ();
     tinfo -> next = recvint ();
     tinfo -> pref = recvint ();
     tinfo -> nref = recvint ();
     tinfo -> fref = recvint ();
     tinfo -> lref = recvint ();
     tinfo -> chain_index = recvint ();
     tinfo -> date_entered = recvint ();
     tinfo -> num_lines = recvint ();
     tinfo -> num_chars = recvint ();
     tinfo -> subject = recvstr ();
     tinfo -> author = recvstr ();
}

/*
 *
 * recv_trn_info2 -- recv a trn_info2 struct.
 *
 */
recv_trn_info2(tinfo)
trn_info2 *tinfo;
{
     tinfo -> version = recvint ();
     tinfo -> current = recvint ();
     tinfo -> prev = recvint ();
     tinfo -> next = recvint ();
     tinfo -> pref = recvint ();
     tinfo -> nref = recvint ();
     tinfo -> fref = recvint ();
     tinfo -> lref = recvint ();
     tinfo -> chain_index = recvint ();
     tinfo -> date_entered = recvint ();
     tinfo -> num_lines = recvint ();
     tinfo -> num_chars = recvint ();
     tinfo -> subject = recvstr ();
     tinfo -> author = recvstr ();
     tinfo -> flags = recvint ();
}
    
/*
 *
 * old_recv_mtg_info -- Recv a mtg info struct.
 *
 */
old_recv_mtg_info(minfo)
mtg_info *minfo;
{
     minfo -> version = recvint ();
     minfo -> location = recvstr ();
     minfo -> long_name = recvstr ();
     minfo -> chairman = recvstr ();
     minfo -> first = recvint ();
     minfo -> last = recvint ();
     minfo -> lowest = recvint ();
     minfo -> highest = recvint ();
     minfo -> date_created = recvint ();
     minfo -> date_modified = recvint ();
     minfo -> public_flag = recvbool ();
}

/*
 *
 * recv_mtg_info -- Recv a mtg info struct.
 *
 */
recv_mtg_info(minfo)
mtg_info *minfo;
{
     minfo -> version = recvint ();
     minfo -> location = recvstr ();
     minfo -> long_name = recvstr ();
     minfo -> chairman = recvstr ();
     minfo -> first = recvint ();
     minfo -> last = recvint ();
     minfo -> lowest = recvint ();
     minfo -> highest = recvint ();
     minfo -> date_created = recvint ();
     minfo -> date_modified = recvint ();
     minfo -> public_flag = recvbool ();
     minfo -> access_modes = recvstr ();
}

dsc_acl *recv_acl()
{
        char *malloc();
	/* The following code would lose points in 6.170... */
	register dsc_acl *result = (dsc_acl *) malloc(sizeof(dsc_acl));
	register dsc_acl_entry *ae;
	register unsigned int len;
	len = recvint();
	result->acl_length = len;
	if (len > 1000) /* Bogus! */ {
		/*XXX Put some error checking in here */
	}
	result->acl_entries =
	    (dsc_acl_entry *) malloc(sizeof(dsc_acl_entry) * len);
	for (ae = result->acl_entries; len; --len, ++ae) {
		ae->modes = recvstr();	
		ae->principal = recvstr();
	}
	return result;
}
		
