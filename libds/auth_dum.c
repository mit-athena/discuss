/*
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/auth_dum.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/auth_dum.c,v 1.2 1987-04-11 00:05:35 srz Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 * auth_dum () -- Authentication procedure for non-kerberos sites.
 *		  Just returns an empty string.
 *
 *	$Log: not supported by cvs2svn $
 */
#ifndef lint
static char *rcsid__c = "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/auth_dum.c,v 1.2 1987-04-11 00:05:35 srz Exp $";
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
