/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/interface.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/interface.c,v 1.12 1987-09-17 02:34:28 spook Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.11  87/07/20  20:56:26  srz
 * Changed name of whoami to dwhoami (too generic of a name)
 * 
 * Revision 1.10  87/07/08  01:59:44  wesommer
 * Added whoami(), dsc_whoami() (the server support was always there.
 * 
 * Revision 1.9  87/06/27  01:16:46  spook
 * *** empty log message ***
 * 
 * Revision 1.8  87/03/22  04:21:46  spook
 * *** empty log message ***
 * 
 * Revision 1.7  87/01/12  04:15:47  wesommer
 * Replaced an fprintf(stderr, ...) with a call to log_warning.
 * 
 * Revision 1.6  87/01/09  20:51:00  srz
 * Added modules to rpc mechanism.
 * 
 * Revision 1.5  86/11/22  03:34:52  wesommer
 * Corrected argument mismatch on get_access() call.
 * 
 * Revision 1.4  86/11/16  06:09:04  wesommer
 * Added dsc_get_acl, dsc_get_access, dsc_set_access, and dsc_delete_access.
 * 
 * Revision 1.3  86/11/11  17:00:20  wesommer
 * Replaced version of select_meeting which keeps connections open..
 * 
 */

#ifndef lint
static char *rcsid_interface_c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/interface.c,v 1.12 1987-09-17 02:34:28 spook Exp $";
#endif lint

#include <stdio.h>
#include <strings.h>
#include "tfile.h"
#include "interface.h"
#include "acl.h"
#include <errno.h>
#include "dsname.h"

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

static meeting *meeting_list = (meeting *)NULL;
meeting *cmtg = (meeting *) NULL;
#define	mtg_name (cmtg->path)

#define	FREE(ptr) { if (ptr) free(ptr); }

static
select_meeting(nbp, code_ptr)
	name_blk *nbp;
	int *code_ptr;
{
	register meeting *mp;
	extern int errno;
	static int initialized = 0;

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
		if (same_mtg(nbp, mp))
			break;
	}
	if (!mp) {
		if (!(mp = (meeting *)malloc(sizeof (meeting)))) {
		        *code_ptr = errno;
			return;
		}

		mp -> module = malloc (strlen (nbp->hostname) + 9);
		strcpy (mp -> module , "discuss@");
		strcpy (&(mp -> module [8]), nbp->hostname);

 		mp->host = malloc(strlen(nbp->hostname)+1);
		strcpy(mp->host, nbp->hostname);
		mp->path = malloc(strlen(nbp->pathname)+1);
		strcpy(mp->path, nbp->pathname);
		/* link 'em up.. */
		mp->next = meeting_list;
		mp->prev = NULL;
		if (meeting_list) meeting_list -> prev = mp;
		meeting_list = mp;

	} else {
		/*XXX should move mp to head of list.. but not yet */
	}
	set_module (mp->module, code_ptr);
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

char *
dsc_create_mtg(host, location, name, public, hidden, code_ptr)
	char *host, *location, *name;
	bool public, hidden;
	int *code_ptr;
{
	/* mumble */
	/* hand back mtg_uid */
	*code_ptr = EACCES;
	return((char *)NULL);
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
	Acl **list;
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
