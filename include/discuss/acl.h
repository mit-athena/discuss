/*
 *	$Id: acl.h,v 1.6 1999-01-22 23:09:49 ghudson Exp $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 */

typedef struct dsc_acl_entry {
	char *principal;	/* The principal */
	char *modes;		/* The allowed modes */
} dsc_acl_entry;

typedef struct dsc_acl {
	int acl_length;
	dsc_acl_entry *acl_entries; /* Members of list (gets realloc'ed) */
} dsc_acl;
