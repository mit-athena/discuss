/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/interface.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/interface.c,v 1.4 1986-11-16 06:09:04 wesommer Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.3  86/11/11  17:00:20  wesommer
 * Replaced version of select_meeting which keeps connections open..
 * 
 */

#ifndef lint
static char *rcsid_interface_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/interface.c,v 1.4 1986-11-16 06:09:04 wesommer Exp $";
#endif lint

#include <stdio.h>
#include "../include/rpc.h"
#include "../include/tfile.h"
#include "../include/interface.h"
#include "../include/acl.h"

typedef struct _connection{
	struct _connection *next;
	struct _connection *prev;
	char *hostname;
	rpc_conversation rc;
} connection;

typedef struct _meeting {
	struct _meeting *next;
	struct _meeting *prev;
	char *unique_id;
	char *name;
	connection *cn;
} meeting;

extern char *malloc();
extern char *new_string();

static int initialized = 0;
static meeting *meeting_list = (meeting *)NULL;
meeting *cmtg = (meeting *) NULL;
static connection *conn_list = (connection *) NULL;
#define	mtg_name (cmtg->name)

#define	FREE(ptr) { if (ptr) free(ptr); }

static
select_meeting(mtg_uid, code_ptr)
	char *mtg_uid;
	int *code_ptr;
{
	char *host, *path;
	char *current_host;
	register meeting *mp;
	register connection *cp;
	extern int errno;

	*code_ptr = 0;
	if (!initialized) {
		init_rpc();
		meeting_list = (meeting *)NULL;
		initialized = 1;
	}
	/*
	 * Is the current meeting one which we've talked to recently?
	 */
	for (mp = meeting_list; mp; mp = mp->next) {
		if (mp->unique_id == mtg_uid 
		|| !strcmp(mp->unique_id, mtg_uid))
			break;
	}
	if (!mp) {
		get_mtg_location(mtg_uid, &host, &path, code_ptr);
		if (*code_ptr) return;

		if (!(mp = (meeting *)malloc(sizeof(meeting)))) {
			*code_ptr = errno;
			return;
		}

		/* find the conversation, if any.. */
		for (cp = conn_list; cp; cp=cp->next) {
			if (!strcmp(cp->hostname, host)) {
				mp->cn = cp;
				break;
			}
		}
		if(!cp) {
			if (!(cp = (connection *)malloc(sizeof(connection)))) {
				FREE(mp);
				*code_ptr = errno;
				return;
			}
			if(!(cp->rc = open_rpc(host, "discuss", code_ptr))) {
				FREE(mp);
				FREE(cp);
				return;
			}
			cp->hostname = new_string(host);
			cp->prev = NULL;
			cp->next = conn_list;
			if (conn_list) conn_list->prev = cp;
			conn_list = cp;
		} else {
			/* XXX warp cp to head of list */
		}
		mp->unique_id = new_string(mtg_uid);
		mp->name = path;
		mp->cn = cp;
		/* link 'em up.. */
		mp->next = meeting_list;
		mp->prev = NULL;
		if (meeting_list) meeting_list -> prev = mp;
		meeting_list = mp;

	} else {
		/*XXX should move mp to head of list.. but not yet */
	}
	set_rpc(mp->cn->rc);
	/* XXX Should sanity check that "rc" is a valid connection.. */
	cmtg = mp;
}

dsc_add_trn(mtg_uid, text, subject, reply_trn, result_trn, code_ptr)
	char *mtg_uid;
	tfile text;
	char *subject;
	trn_nums reply_trn, result_trn;
	int *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	add_trn(mtg_name, text, subject, reply_trn, result_trn, code_ptr);
}

dsc_get_trn_info(mtg_uid, trn, info, code_ptr)
	char *mtg_uid;
	trn_nums trn;
	trn_info *info;
	int *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	get_trn_info(mtg_name, trn, info, code_ptr);
}

dsc_delete_trn(mtg_uid, trn, code_ptr)
	char *mtg_uid;
	trn_nums trn;
	int *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	delete_trn(mtg_name, trn, code_ptr);
}

dsc_retrieve_trn(mtg_uid, trn, code_ptr)
	char *mtg_uid;
	trn_nums trn;
	int *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	retrieve_trn(mtg_name, trn, code_ptr);
}

char *
dsc_create_mtg(host, location, name, public, hidden, code_ptr)
	char *host, *location, *name;
	bool public, hidden;
	int *code_ptr;
{
	/* mumble */
	/* hand back mtg_uid */
}

dsc_get_mtg_info(mtg_uid, info, code_ptr)
	char *mtg_uid;
	mtg_info *info;
	int *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	get_mtg_info(mtg_name, info, code_ptr);
}


dsc_get_trn(mtg_uid, trn, dest, code_ptr)
	char *mtg_uid;
	trn_nums trn;
	tfile dest;
	int *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	get_trn(mtg_name, trn, dest, code_ptr);
}

dsc_remove_mtg(mtg_uid, code_ptr)
	char *mtg_uid;
	int *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	remove_mtg(mtg_name, code_ptr);
}

dsc_updated_mtg (mtg_uid, date_attended, last, updated, result)
char *mtg_uid;
int date_attended, last;
bool *updated;
int *result;
{
     select_meeting(mtg_uid, result);
     if (*result) return;
     updated_mtg(mtg_name, date_attended, last, updated, result);
}

dsc_get_acl (mtg_uid, result, list)
	char *mtg_uid;
	int *result;	
	Acl **list;
{	
	select_meeting(mtg_uid, result);
	if(*result) return;
	get_acl(mtg_name, result, list);
}

dsc_get_access (mtg_uid, princ, modes, result)
	char *mtg_uid;
	char *princ;
	char *modes;
	int *result;
{
	select_meeting(mtg_uid, result);
	if (*result) return;
	get_access(mtg_name, princ, result);
}

dsc_set_access (mtg_uid, princ, modes, result)
	char *mtg_uid;
	char *princ;
	char *modes;
	int *result;
{
	select_meeting(mtg_uid, result);
	if (*result) return;
	set_access(mtg_name, princ, modes, result);
}

dsc_delete_access (mtg_uid, princ, result)
	char *mtg_uid;
	char *princ;
	int *result;
{
	select_meeting(mtg_uid, result);
	if (*result) return;
	delete_access(mtg_name, princ, result);
}

/*
 *
 * new_string (s)  --   Routine to create a copy of the given string, using
 *		      	malloc.
 *
 */
char *new_string (s)
char *s;
{
     int len;
     char *newstr;

     len = strlen (s) + 1;
     newstr = malloc (len);
     strcpy (newstr, s);
     return (newstr);
}
