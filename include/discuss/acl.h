/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/discuss/acl.h,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/discuss/acl.h,v 1.1 1986-11-16 06:11:16 wesommer Exp $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 */


typedef struct _acl_entry {
	char *principal;	/* The principal */
	char *modes;		/* The allowed modes */
} acl_entry;

typedef struct _acl {
	int acl_length;
	acl_entry *acl_entries;	/* Members of list (gets realloc'ed) */
} Acl;

#ifdef FALSE
#define Bool bool
#else
typedef int Bool;
#define FALSE (0)
#define TRUE (!0)
#endif

Acl *acl_read();
Acl *acl_create();
Bool acl_check();
extern acl_add_access();
extern acl_delete_access();
extern acl_replace_access();
char *acl_get_access();
char *acl_canon();
acl_write();

#include "acl_et.h"
