#include <stdio.h>
#include "../include/rpc.h"
#include "../include/tfile.h"
#include "../include/interface.h"

typedef struct {
	struct meeting *next;
	char *unique_id;
	char *hostname;
	char *name;
	rpc_conversation rc;
} meeting;

extern char *malloc();

static int initialized = 0;
static meeting *meeting_list = (meeting *)NULL;
static meeting *cmtg = (meeting *)NULL;	/* current meeting */
#define	mtg_name (cmtg->name)

#define	FREE(ptr) { if (ptr) free(ptr); }

static
select_meeting(mtg_uid, code_ptr)
	char *mtg_uid;
	error_code *code_ptr;
{
	char *host, *path;
	char *current_host;

	*code_ptr = 0;
	if (!initialized) {
		init_rpc();
		cmtg = (meeting *)NULL;
		initialized = 1;
	}
	if (cmtg == (meeting *)NULL) {
		cmtg = (meeting *)malloc(sizeof(meeting));
		if (cmtg == (meeting *)NULL) {
			*code_ptr = ERRNO;
			return;
		}
		bzero(cmtg, sizeof(*cmtg));
	}
	else if (cmtg->unique_id == mtg_uid)
		return;
	else if (!strcmp(cmtg->unique_id, mtg_uid))
		return;
	/* we need to set the meeting */
#ifdef	DEBUG
	printf("interface.debug: Changing to meeting %s\n", mtg_uid);
#endif
	current_host = cmtg->hostname;
	FREE(cmtg->unique_id);
	FREE(cmtg->name);
	get_mtg_location(mtg_uid, &host, &path, code_ptr);
	if (*code_ptr) {
		FREE(host);
		FREE(cmtg->hostname);
		FREE(path);
		close_rpc(cmtg->rc);
		FREE(cmtg);
		cmtg = (meeting *)NULL;
		return;
	}
	cmtg->unique_id = malloc(strlen(mtg_uid)+1);
	strcpy(cmtg->unique_id, mtg_uid);
	cmtg->hostname = host;
	cmtg->name = path;
	if (!strcmp(current_host, host)) {
		FREE(current_host);
		return;
	}
	/* we need to set the host */
#ifdef	DEBUG
	printf("interface.debug: Changing host to %s\n", host);
#endif
	FREE(current_host);
	if (cmtg->rc)
		close_rpc(cmtg->rc);
	cmtg->rc = open_rpc(host, "discuss", code_ptr);
	if (cmtg -> rc == NULL) {
		FREE(host);
		FREE(path);
		FREE(cmtg);
		cmtg = (meeting *)NULL;
		return;
	}
	/* we don't need to grab info here; let the next call lose.... */
	*code_ptr = 0;					/* sigh */
}

dsc_add_trn(mtg_uid, text, subject, reply_trn, result_trn, code_ptr)
	char *mtg_uid;
	tfile text;
	char *subject;
	trn_nums reply_trn, result_trn;
	error_code *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	add_trn(mtg_name, text, subject, reply_trn, result_trn, code_ptr);
}

dsc_get_trn_info(mtg_uid, trn, info, code_ptr)
	char *mtg_uid;
	trn_nums trn;
	trn_info *info;
	error_code *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	get_trn_info(mtg_name, trn, info, code_ptr);
}

dsc_delete_trn(mtg_uid, trn, code_ptr)
	char *mtg_uid;
	trn_nums trn;
	error_code *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	delete_trn(mtg_name, trn, code_ptr);
}

dsc_retrieve_trn(mtg_uid, trn, code_ptr)
	char *mtg_uid;
	trn_nums trn;
	error_code *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	retrieve_trn(mtg_name, trn, code_ptr);
}

char *
dsc_create_mtg(host, location, name, public, hidden, code_ptr)
	char *host, *location, *name;
	bool public, hidden;
	error_code *code_ptr;
{
	/* mumble */
	/* hand back mtg_uid */
}

dsc_get_mtg_info(mtg_uid, info, code_ptr)
	char *mtg_uid;
	mtg_info *info;
	error_code *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	get_mtg_info(mtg_name, info, code_ptr);
}


dsc_get_trn(mtg_uid, trn, dest, code_ptr)
	char *mtg_uid;
	trn_nums trn;
	tfile dest;
	error_code *code_ptr;
{
	select_meeting(mtg_uid, code_ptr);
	if (*code_ptr) return;
	get_trn(mtg_name, trn, dest, code_ptr);
}

dsc_remove_mtg(mtg_uid, code_ptr)
	char *mtg_uid;
	error_code *code_ptr;
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
