/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/discuss/acl.h,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/discuss/acl.h,v 1.4 1987-03-17 02:21:50 srz Exp $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.3  86/11/22  05:51:47  spook
 * Included types.h, punted duplicate Bool type (use bool from types.h);
 * fixed up declarations of external routines.
 * 
 * Revision 1.2  86/11/17  01:23:26  spook
 * Removed include file acl_et.h, which has gone away.
 * 
 */

#include "types.h"

typedef struct _acl_entry {
	char *principal;	/* The principal */
	char *modes;		/* The allowed modes */
} acl_entry;

typedef struct _acl {
	int acl_length;
	acl_entry *acl_entries;	/* Members of list (gets realloc'ed) */
} Acl;

Acl *acl_read();
Acl *acl_create();
Acl *acl_copy();
bool acl_check();
extern int acl_add_access();
extern bool acl_delete_access();
extern bool acl_is_subset();
extern acl_replace_access();
char *acl_get_access();
char *acl_canon();
bool acl_write();
bool has_mtg_access();
bool has_trn_access();
