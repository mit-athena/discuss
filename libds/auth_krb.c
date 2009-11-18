/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 *	$Id: auth_krb.c,v 1.13 2007-08-09 20:41:32 amb Exp $
 *
 * auth_krb () -- Authentication procedure for kerberos v5.  This contains the
 *		  standard authentication for kerberos v5, and fallback code
 *                for kerberos v4.
 *
 */
#ifndef lint
static char *rcsid_auth_krb_c =
    "$Id: auth_krb.c,v 1.13 2007-08-09 20:41:32 amb Exp $";
#endif /* lint */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#ifdef HAVE_KRB4
#include "krb.h"
#endif /* HAVE_KRB4 */
#ifdef HAVE_KRB5
#include "krb5.h"
#endif /* HAVE_KRB5 */
#include "discuss_err.h"

char *local_host_name ();

/*
 *
 * get_authenticator () -- Interface routine to get an authenticator over
 *			   the net.  Input is a service name (for kerberos,
 *			   this is in the form of service@REALM), optional
 *			   checksum.  We return a pointer to the authenticator,
 *			   its length, and a standard error code.
 *
 */
get_authenticator (service_id, checksum, authp, authl, result)
char *service_id;
int checksum;
char **authp;
int *authl;
int *result;
{
#ifdef HAVE_KRB5
     get_authenticator_krb5(service_id, checksum, authp, authl, result);
#elif HAVE_KRB4
     get_authenticator_krb4(service_id, checksum, authp, authl, result);
#else /* No Kerberos */
     *authl = 0;
     *authp = NULL;
     *result = DISC_NO_KRB;
#endif
}

#ifdef HAVE_KRB5
get_authenticator_krb5 (service_id, checksum, authp, authl, result)
char *service_id;
int checksum;
char **authp;
int *authl;
int *result;
{
     char *realmp,*instancep;
     char serv [80];
     int rem;
     krb5_data packet, inbuf;
     krb5_ccache ccdef;
     krb5_context context;
     krb5_auth_context auth_context = NULL;

     rem = krb5_init_context(&context);
     if (rem) {
         com_err("get_authenticator_krb5", rem, "while initializing krb5");
	 exit(1);
     }

#if !defined(__APPLE__) || !defined(__MACH__)
     initialize_krb5_error_table();
#endif

     realmp = strchr (service_id, '@');
     if (realmp == NULL || realmp - service_id >= sizeof (serv)) {
	  realmp = "";
	  strncpy (serv, service_id, sizeof (serv));
     } else {
	 memcpy (serv, service_id, realmp - service_id); /* copy to serv */
	 serv [realmp - service_id] = '\0';
	 realmp++;
     }

     /* look for service instance */
     instancep = strchr (serv, '/');
     if (instancep == NULL) {
	  instancep = "";
     } else {
	  *instancep++ = '\0';
     }

     inbuf.data = instancep;
     inbuf.length = strlen(instancep);

     rem = krb5_cc_default(context, &ccdef);
     if (rem) {
         com_err("get_authenticator_krb5", rem, "while getting default ccache");
	 exit(1);
     }

     rem = krb5_mk_req (context, &auth_context, 0, serv, instancep, &inbuf,
                        ccdef, &packet);
     if (rem) {
         com_err("get_authenticator_krb5", rem, "while preparing AP_REQ");
         *authl = 0;
         *authp = NULL;
         *result = rem;
     } else {
         *authl = packet.length;
         *authp = (char *)packet.data;
         *result = 0;
     }
}
#endif /* HAVE_KRB5 */

#ifdef HAVE_KRB4
get_authenticator_krb4 (service_id, checksum, authp, authl, result)
char *service_id;
int checksum;
char **authp;
int *authl;
int *result;
{
     char *realmp,*instancep;
     char serv [SNAME_SZ+INST_SZ];
     int rem;

     static KTEXT_ST ticket;

     initialize_krb_error_table();

     realmp = strchr (service_id, '@');
     if (realmp == NULL || realmp - service_id >= sizeof (serv)) {
          realmp = "";
          strncpy (serv, service_id, sizeof (serv));
     } else {
         memcpy (serv, service_id, realmp - service_id); /* copy to serv */
         serv [realmp - service_id] = '\0';
         realmp++;
     }

     /* look for service instance */
     instancep = strchr (serv, '.');
     if (instancep == NULL) {
          instancep = "";
     } else {
          *instancep++ = '\0';
     }

     rem = krb_mk_req (&ticket, serv, instancep, realmp, checksum);
     if (rem == KSUCCESS) {
	 *authl = ticket.length;
	 *authp = (char *) ticket.dat;
	 *result = 0;
     } else {
	 *authl = 0;
	 *authp = NULL;
	 *result = rem + ERROR_TABLE_BASE_krb;
     }
}
#endif /* HAVE_KRB4 */
