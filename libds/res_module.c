/*
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/res_module.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/res_module.c,v 1.7 1989-05-19 18:12:21 srz Exp $
 *
 *	Copyright (C) 1986 by the Massachusetts Institute of Technology
 *
 * resolve_module () --
 *	Can you say "Put all the configuration into one file?"  Can you
 *	say "Concentrated kludgery?"  I knew you could.  This procedure
 *	resolves a module name into port number, hostname, service; it
 *      is allowed to use any trick in the book -- it can depend on hostnames,
 *	have hard coded constants (hopefully recorded in config files, etc.).
 *	Note that if service name contains a '/' as the first character, then
 *	the remote function is executed as a subprocess.
 *
 *	$Log: not supported by cvs2svn $
 * Revision 1.6  89/05/19  17:05:18  raeburn
 * *** empty log message ***
 * 
 */

#ifndef lint
static char rcsid_res_module_c[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/libds/res_module.c,v 1.7 1989-05-19 18:12:21 srz Exp $";
#endif lint

#include "rpc_et.h"
#include "config.h"
#include "ansi.h"
#include <netdb.h>
#include <string.h>
#include <ctype.h>

#ifdef KERBEROS
#include "krb.h"
#ifndef MAX_K_NAME_SZ
/* @#$%^$ last minute changes by jtkohl */
#define krb_get_lrealm get_krbrlm
#endif
static void ExpandHost ();
#endif /* KERBEROS */

#ifndef SNAME_SZ
#define SNAME_SZ 30
#define REALM_SZ 30
#define MAX_HSTNM 60
#endif SNAME_SZ

#define NULL 0

char *local_host_name ();
const char *local_realm ();

static int service_port = 0;

void resolve_module (modname, port, hostp, servp, result)
    char *modname;		/* name to translate */
    int *port;			/* resultant port number */
    char **hostp;		/* ptr to hostname (static) */
    char **servp;		/* service_id */
    int *result;		/* std error code */
{
    static char service_id [SNAME_SZ+REALM_SZ];
    static char hostname [MAX_HSTNM];
    char realm [REALM_SZ];

    char *myhnamep = NULL;
    const char *realmp = NULL;
    struct servent *sp;
    struct hostent *hp;

    *hostp = NULL;
    *servp = NULL;
    *port = 0;
    *result = 0;

    /* The module name could be of the form "discuss@hostname", where
     * hostname is the host to contact.  If the hostname is omitted,
     * the current host is assumed */
    if (!strncmp (modname, "discuss", 7)) {
	if (modname [7] == '@') { /* got hostname */
	    myhnamep = &modname [8];
	    hp = gethostbyname (myhnamep); /* make it primary */
	    if (!hp) {
		extern int h_errno;
		int h = h_errno;
		switch (h) {
		case HOST_NOT_FOUND:
		    *result = RPC_HOST_UNKNOWN;
		    break;
		case TRY_AGAIN:
		    *result = RPC_NS_TIMEOUT;
		    break;
		case NO_RECOVERY:
		    *result = RPC_NS_ERROR;
		    break;
		case NO_ADDRESS:
		    *result = RPC_NO_ADDR;
		    break;
		default:
		    *result = RPC_NS_ERROR;
		}
		return;
	    }
	    strcpy (hostname, hp -> h_name);
	    myhnamep = hostname;
	} else if (modname [7] == '\0') { /* Just discuss - use current host */
	    myhnamep = local_host_name ();
	} else {
	    *result = RPC_MOD_UNKNOWN;
	    return;
	}
    }

#if 0
    /* or... the module could be of the form of disname@realm, where realm
     * is a kerberos realm.  If realm is not given, then the current realm
     * is assumed. */
    else if (!strncmp (modname, "disname", 7)) {
	if (modname [7] == '@') {		/* got realm */
	    realmp = &modname [8];
	} else if (modname [7] == '\0') {
	    /* Just disname - use current realm */
	    realmp = local_realm ();
	} else {
	    *result = RPC_MOD_UNKNOWN;
	    return;
	}

	/* got realm -- use our static lookup. */
	if (!strcmp (realmp, "LCS.MIT.EDU"))
	    myhnamep = "GRAPE-NEHI.LCS.MIT.EDU";
	else if (!strcmp (realmp, "ATHENA.MIT.EDU"))
	    myhnamep = "CHARON.MIT.EDU";
	else {
	    *result = RPC_REALM_UNKNOWN;
	    return;
	}
    }
#endif
    else {
	*result = RPC_MOD_UNKNOWN;
	return;
    }

    /* Now we have the host name, and all we have to do is create the
     * service id & port number.  If this is local, we use the subprocess,
     * for better authentication */
    if (!namcmp (myhnamep, local_host_name ())) {
	*port = 0;
	*servp = SERVER;
	*hostp = myhnamep;
	*result = 0;
	return;
    }

    /* otherwise, we have to generate the port number */
    if (service_port == 0) {
	sp = getservbyname (SERVICE_NAME, 0);
	if (!sp) {
	    *result = RPC_SERV_UNKNOWN;
	    return;
	}

	service_port = sp -> s_port;
    }

    *port = service_port;

    /* generate the service name, but concatenating "discuss.instance@realm"
     * desired realm. */
#ifndef KERBEROS
    strcpy (service_id, "discuss@");
    strcpy (&service_id[8], REALM);
#else
    strcpy (service_id, "discuss.");
    ExpandHost (myhnamep, &service_id[8], realm);
    strcat(service_id, "@");
    if (realmp)
	strcat (service_id, realmp);
    else
	strcat (service_id, realm);
#endif KERBEROS
    *hostp = myhnamep;
    *servp = service_id;
    *result = 0;
}

#ifdef KERBEROS
/*
 *
 * ExpandHost -- takes a user string alias for a host, and converts it
 *		 to the official Kerberos principal name, plus the realm
 *		 that it lies in.
 *
 *     Warning:  There are some heuristics here.
 *
 */

static void ExpandHost (primary_name, krb_host, krb_realm )
    char *primary_name,*krb_realm;
    char *krb_host;
{
    char *p,*sp=primary_name,*dp=krb_host;
    /*
     * The convention established by the Kerberos-authenticated
     * rcmd services (rlogin, rsh, rcp) is that the principal host
     * name is all lower case characters.  Therefore, we can get
     * this name from an alias by taking the official, fully
     * qualified hostname, stripping off the domain info (ie, take
     * everything up to but excluding the '.') and translating it
     * to lower case.  For example, if "menel" is an alias for
     * host officially named "menelaus" (in /etc/hosts), for the
     * host whose official name is "MENELAUS.MIT.EDU", the user
     * could give the command "menel echo foo" and we will resolve
     * it to "menelaus".
     */
    *krb_realm = '\0';		/* null for now */
    p = index( sp, '.' );
    if (p) {
	char *p1;

	strncpy(krb_realm,p+1,REALM_SZ);		/* Realm after '.' */
	krb_realm[REALM_SZ-1] = NULL;
	p1 = krb_realm;                           /* Upper case this */
	do {
	    if (islower(*p1))
		*p1=toupper(*p1);
	} while (*p1++);
    }
    /* lower case Kerberos host name */
    do {
	if (isupper(*sp)) *dp=tolower(*sp);
	else *dp = *sp;
    } while (dp++,*sp && (*sp++ != '.'));
    *(--dp) = NULL;

    /* heuristics */

    if (*krb_realm == '\0')
	strcpy (krb_realm, local_realm());
    if (!strcmp(krb_realm,"MIT.EDU"))
	strcpy(krb_realm,"ATHENA.MIT.EDU");
    return;
}
#endif

const char *local_realm ()
{
#ifdef KERBEROS
    static char realm [REALM_SZ] = "";

    if (realm [0] == '\0')
	krb_get_lrealm (realm, 1);

    return (realm);
#else KERBEROS
    return (REALM);
#endif KERBEROS
}
