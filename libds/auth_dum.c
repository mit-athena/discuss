/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 *	$Id: auth_dum.c,v 1.4 1999-01-22 23:09:55 ghudson Exp $
 *
 * auth_dum () -- Authentication procedure for non-kerberos sites.
 *		  Just returns an empty string.
 *
 */
#ifndef lint
static char *rcsid__c = "$Id: auth_dum.c,v 1.4 1999-01-22 23:09:55 ghudson Exp $";
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
