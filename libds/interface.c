/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/interface.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/interface.c,v 1.21 1989-02-25 18:33:17 raeburn Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 */

#ifndef lint
static char rcsid_interface_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/interface.c,v 1.21 1989-02-25 18:33:17 raeburn Exp $";
#endif lint

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <discuss/tfile.h>
#include <discuss/interface.h>
#include <discuss/acl.h>
#include <discuss/dsname.h>
#include "rpc.h"
#include <discuss/dsc_et.h>

/* for linked list of meetings */

#define same_str(s1,s2)		((s1==s2)||!strcmp(s1,s2))
/* note -- first must be (name_blk *), second must be (meeting *) */
#define	same_mtg(nbp,mtg)\
	(same_str(nbp->hostname,mtg->host)&&same_str(nbp->pathname,mtg->path))

typedef struct _meeting {
	struct _meeting *next;
	struct _meeting *prev;
	char *host, *path;
	char *module;
} meeting;

extern char *malloc();
extern int errno;

static meeting *meeting_list = (meeting *)NULL;
meeting *cmtg = (meeting *) NULL;
#define	mtg_name (cmtg->path)

#define	FREE(ptr) { if (ptr) free(ptr); }

static meeting *create_mblock(host, path, code_ptr)
	char *host, *path;
	int *code_ptr;
{
	register meeting *mp;
	static int initialized = 0;
	
	*code_ptr = 0;

	if (!initialized) {
		init_rpc();
		meeting_list = (meeting *)NULL;
		initialized = 1;
	}
	if (!(mp = (meeting *)malloc(sizeof (meeting)))) {
		*code_ptr = errno;
		return NULL;
	}

	mp->path = NULL;	/* initialize, for later cleanup */
	mp->host = NULL;	
	
	mp->module = malloc (strlen (host) + 9);
	if (!mp->module) goto out;
	
	strcpy (mp->module , "discuss@");
	strcpy (&(mp->module [8]), host);

	mp->host = malloc(strlen(host)+1);
	if (!mp->host) goto out;
	strcpy(mp->host, host);

	mp->path = malloc(strlen(path)+1);
	if (!mp->path) goto out;
	strcpy(mp->path, path);

	/* link 'em up.. */
	mp->next = meeting_list;
	mp->prev = NULL;
	if (meeting_list) meeting_list -> prev = mp;
	meeting_list = mp;
	return mp;
	
out:
	*code_ptr = errno;
	if (mp->host) free(mp->host);
	if (mp->path) free(mp->path);
	if (mp->module) free(mp->path);
	free(mp);
	return NULL;
}

static
select_meeting(nbp, code_ptr)
	name_blk *nbp;
	int *code_ptr;
{
	register meeting *mp;
	int fatal;

	*code_ptr = 0;

	/*
	 * Is the current meeting one which we've talked to recently?
	 */
	for (mp = meeting_list; mp; mp = mp->next) {
		if (same_mtg(nbp, mp))
			break;
	}
	if (!mp) {
		mp = create_mblock(nbp->hostname, nbp->pathname, code_ptr);
		if (*code_ptr)
			return;
	} else {
		/*XXX should move mp to head of list.. but not yet */
	}
	set_module (mp->module, &fatal, code_ptr);
	if (*code_ptr && !fatal) {
	    char buf[100];
	    sprintf(buf, "while connecting to %s", mp->module);
	    com_err ("discuss", *code_ptr, buf);
	    *code_ptr = 0;
	}
	cmtg = mp;
}

dsc_add_trn(nbp, text, subject, reply_trn, result_trn, code_ptr)
	name_blk *nbp;
	tfile text;
	char *subject;
	trn_nums reply_trn, *result_trn;
	int *code_ptr;
{
	select_meeting(nbp, code_ptr);
	if (*code_ptr) return;
	add_trn(mtg_name, text, subject, reply_trn, result_trn, code_ptr);
}

dsc_get_trn_info(nbp, trn, info, code_ptr)
	name_blk *nbp;
	trn_nums trn;
	trn_info *info;
	int *code_ptr;
{
	select_meeting(nbp, code_ptr);
	if (*code_ptr) return;
	get_trn_info(mtg_name, trn, info, code_ptr);
}

dsc_get_trn_info2(nbp, trn, info, code_ptr)
	name_blk *nbp;
	trn_nums trn;
	trn_info2 *info;
	int *code_ptr;
{
	select_meeting(nbp, code_ptr);
	if (*code_ptr) return;
	if (get_conv_server_version() >= SERVER_1)
	     get_trn_info2(mtg_name, trn, info, code_ptr);
	else {
	     get_trn_info(mtg_name, trn, (trn_info *) info, code_ptr);
	     info -> flags = 0;
	}
}

dsc_set_trn_flags(nbp, trn, flags, code_ptr)
	name_blk *nbp;
	trn_nums trn;
	int flags;
	int *code_ptr;
{
	select_meeting(nbp, code_ptr);
	if (*code_ptr) return;
	if (get_conv_server_version() >= SERVER_1)
	     set_trn_flags(mtg_name, trn, flags, code_ptr);
	else {
	     *code_ptr = NO_SUPPORT;
	}
}

dsc_delete_trn(nbp, trn, code_ptr)
	name_blk *nbp;
	trn_nums trn;
	int *code_ptr;
{
	select_meeting(nbp, code_ptr);
	if (*code_ptr) return;
	delete_trn(mtg_name, trn, code_ptr);
}

dsc_retrieve_trn(nbp, trn, code_ptr)
	name_blk *nbp;
	trn_nums trn;
	int *code_ptr;
{
	select_meeting(nbp, code_ptr);
	if (*code_ptr) return;
	retrieve_trn(mtg_name, trn, code_ptr);
}

dsc_create_mtg(host, location, name, public, hidden, code_ptr)
	char *host, *location, *name;
	bool public, hidden;
	int *code_ptr;
{
	register meeting *mp;
	int fatal;
	mp = create_mblock(host, location, code_ptr);
	if (*code_ptr) return;
	set_module(mp->module, &fatal, code_ptr);
	if (*code_ptr) return;
	
	create_mtg(location, name, public, code_ptr);
	return;
}

dsc_get_mtg_info(nbp, info, code_ptr)
	name_blk *nbp;
	mtg_info *info;
	int *code_ptr;
{
	select_meeting(nbp, code_ptr);
	if (*code_ptr) return;
	get_mtg_info(mtg_name, info, code_ptr);
}


dsc_get_trn(nbp, trn, dest, code_ptr)
	name_blk *nbp;
	trn_nums trn;
	tfile dest;
	int *code_ptr;
{
	select_meeting(nbp, code_ptr);
	if (*code_ptr) return;
	get_trn(mtg_name, trn, dest, code_ptr);
}

dsc_remove_mtg(nbp, code_ptr)
	name_blk *nbp;
	int *code_ptr;
{
	select_meeting(nbp, code_ptr);
	if (*code_ptr) return;
	remove_mtg(mtg_name, code_ptr);
}

dsc_updated_mtg (nbp, updated, result)
	name_blk *nbp;
	bool *updated;
	int *result;
{
	select_meeting(nbp, result);
	if (*result) return;
	updated_mtg(mtg_name, nbp->date_attended, nbp->last, updated, result);
}

dsc_get_acl (nbp, result, list)
	name_blk *nbp;
	int *result;	
	dsc_acl **list;
{	
	select_meeting(nbp, result);
	if(*result) return;
	get_acl(mtg_name, result, list);
}

dsc_get_access (nbp, princ, modes, result)
	name_blk *nbp;
	char *princ;
	char **modes;
	int *result;
{
	select_meeting(nbp, result);
	if (*result) return;
	get_access(mtg_name, princ, modes, result);
}

dsc_set_access (nbp, princ, modes, result)
	name_blk *nbp;
	char *princ;
	char *modes;
	int *result;
{
	select_meeting(nbp, result);
	if (*result) return;
	set_access(mtg_name, princ, modes, result);
}

dsc_delete_access (nbp, princ, result)
	name_blk *nbp;
	char *princ;
	int *result;
{
	select_meeting(nbp, result);
	if (*result) return;
	delete_access(mtg_name, princ, result);
}

dsc_whoami(nbp, ident, result)
	name_blk *nbp;
	char **ident;
	int *result;
{
	select_meeting(nbp, result);
	if (*result) return;
	dwhoami(ident, result);
}

void dsc_destroy_mtg_info (info)
	register mtg_info *info;
{
	if (info->chairman) {
		free(info->chairman);
		info->chairman = NULL;
	}
	if (info->access_modes) {
		free (info->access_modes);
		info->access_modes = NULL;
	}
}

void dsc_destroy_trn_info (info)
	register trn_info *info;
{
	if (info->author) {
		free (info->author);
		info->author = NULL;
	}
	if (info->subject) {
		free (info->subject);
		info->subject = NULL;
	}
}
