/*
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/discuss/acl.h,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/include/discuss/acl.h,v 1.5 1988-09-22 23:52:08 raeburn Exp $
 *
 *	Copyright (C) 1986 by the Student Information Processing Board
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.4  87/03/17  02:21:50  srz
 * Added acl_copy declaration
 * 
 * Revision 1.3  86/11/22  05:51:47  spook
 * Included types.h, punted duplicate Bool type (use bool from types.h);
 * fixed up declarations of external routines.
 * 
 * Revision 1.2  86/11/17  01:23:26  spook
 * Removed include file acl_et.h, which has gone away.
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
