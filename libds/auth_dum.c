/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/auth_dum.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/auth_dum.c,v 1.3 1989-06-03 00:19:30 srz Exp $
 *
 * auth_dum () -- Authentication procedure for non-kerberos sites.
 *		  Just returns an empty string.
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.2  87/04/11  00:05:35  srz
 * Added RCS junk
 * 
 */
#ifndef lint
static char *rcsid__c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/auth_dum.c,v 1.3 1989-06-03 00:19:30 srz Exp $";
#endif lint

get_authenticator (service_id, checksum, authp, authl, result)
char *service_id;
int checksum;
char **authp;
int *authl;
int *result;
{
     *authl = 0;
     *authp = "";
     *result = 0;
     return;
}
