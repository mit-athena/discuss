
/*
 *
 *  rpcall.c -- Procedures to implement a simple (perhaps brain-asleep) RPC
 *	  	protocol over a TCP connection.
 *		This file handles the caller's side of the connection.
 *
 */

/* INCLUDES */

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#ifdef KERBEROS
#include <krb.h>
#endif
#include "../include/tfile.h"
#include "../include/rpc.h"
#include "../include/config.h"

/* DEFINES */

#define min(A, B) ((A) < (B) ? (A) : (B))
#define SUCCESS 1
#define ERROR   -1

/* EXTERNAL ROUTINES */

char *malloc();
extern int errno;

#ifdef KERBEROS
extern int krb_err_base;
#endif

int rpc_err;

/* STATIC VARIABLES */

/* argument list info */
static int procno;				/* procedure number */

/* connections & socket info */
static USPStream *us = NULL;

/*
 *
 * startsend()  -- Get ready for an RPC call.
 *
 */
startsend(whichproc)
int whichproc;
{
     procno = whichproc;
     if (us == NULL) {
	  rpc_err = RPC_NOT_INIT;
	  return;
     }
     USP_begin_block(us,PROC_BASE+procno);
     rpc_err = 0;

     return;
}

/*
 *
 * sendint(i)  -- Send an integer in an RPC call.
 *
 */
sendint(i)
int i;
{
     if (USP_put_long_integer(us, i) != SUCCESS) {
	  rpc_err = errno;
     }
}
/*
 *
 * sendshort(i)  -- Send a short integer in an RPC call.
 *
 */
sendshort(i)
short i;
{
     if (USP_put_integer(us, i) != SUCCESS) {
	  rpc_err = errno;
     }
}

/*
 *
 * sendstr(i)  -- Send a string in an RPC call.
 *
 */
sendstr(str)
char *str;
{
     if (us == NULL) {
	  rpc_err = RPC_NOT_INIT;
	  return;
     }
     if (USP_put_string(us, str) != SUCCESS) {
	  rpc_err = errno;
     }
}

/*
 *
 * sendbool(b)  -- Send a boolean in an RPC call.
 *
 */
sendbool(b)
unsigned short b;
{
     if (USP_put_boolean(us, b) != SUCCESS) {
	  rpc_err = errno;
     }
}


/*
 *
 * sendfile(tf)  -- Send a file in an RPC call.
 *
 */
sendfile(tf)
tfile tf;
{
     int tfs,j,numleft;
     char buffer[512];
     char *bptr;

     tfs = tfsize (tf);
     if (USP_put_long_integer(us, tfs) != SUCCESS) {
	  rpc_err = errno;
     }
}

/*
 *
 * sendit () -- Make the final call.
 *
 */
sendit(dest)
char *dest;
{
     if (USP_end_block(us) != SUCCESS) {
	  rpc_err = errno;
     }
     return;
}

/*
 *
 * init_rpc () -- Initialize the RPC mechanism
 *
 */
init_rpc ()
{
#ifdef SUBPROC
     int parent,sv[2],i;
#endif

     init_rpc_err_tbl();
     init_usp_err_tbl();
#ifdef SUBPROC
     if (socketpair(AF_UNIX,SOCK_STREAM,0,sv) < 0)
          panic ("can't do socket pair");

     parent = fork ();
     if (parent < 0)
          panic ("Can't fork");
     if (!parent) {				/* child's play */
	  dup2(sv[1],0);		     	/* child takes second one */

	  for (i = 3; i < 20; i++)
	       (void) close (i);
	  execl(SERVER, SERVER_NAME, 0);
	  panic ("Can't exec");
     } else {
	  (void) close (sv[1]);
	  us = USP_associate (sv[0]);
     }
#endif

#ifdef KERBEROS
     init_krb_err_tbl();
#endif
}

/*
 *
 * term_rpc -- Shutdown the rpc mechanism
 *
 */
term_rpc()
{
     USP_close_connection(us);
     return;
}

/*
 *
 * close_rpc () -- Close down a specific rpc conversation
 *
 */
close_rpc(rc)
rpc_conversation rc;
{
     USP_close_connection(rc);
     us = NULL;
     return;
}

/*
 *
 * set_rpc ()  -- Sets the current rpc conversation
 *
 */
set_rpc(rc)
rpc_conversation rc;
{
     us = rc;
}

/*
 *
 * open_rpc ()  -- Open the connection to the server
 *		   Returns an rpc conversation id.
 *
 */
rpc_conversation open_rpc (host, serv, code)
char *host, *serv;
int *code;
{
#ifndef SUBPROC
#ifdef KERBEROS
     char krb_realm[REALM_SZ];
     KTEXT_ST ticket;
     int rem,i;
     char phost[MAX_HSTNM];	/* principal hostname, for Kerberos only */
#endif
     rpc_conversation conv;

     *code = 0;
     if ((conv = USP_make_connection (host, serv)) == NULL) {
	  if (errno == 0)
	       *code = RPC_SERV_UNKNOWN;		/* sigh */
	  else 
	       *code = errno;
	  return(0);
     }

     us = conv;

#ifdef KERBEROS
     ExpandHost(host, phost, krb_realm);
     if (rem == KSUCCESS)
	  rem = mk_ap_req (&ticket, serv, phost, krb_realm, (u_long)0);
     if (rem == KSUCCESS) {			/* send ticket */
	  USP_begin_block(us,KRB_TICKET);
	  sendshort(ticket.length);
	  for (i = 0; i < ticket.length; i++) {
	       sendshort(ticket.dat[i]);
	  }
	  USP_end_block(us);
     }
     if (rem != KSUCCESS) {
	  USP_begin_block(us,KRB_TICKET);	/* send blank ticket */
	  sendshort(0);
	  USP_end_block(us);
	  *code = rem + krb_err_base;
     }
#endif

     return(conv);
#endif
}

/*
 *
 * recvreply ()  -- Routine to accept an RPC return.
 *
 */
recvreply ()
{
     USPCardinal bt;

     if (USP_rcv_blk(us, &bt) != SUCCESS) {
	  rpc_err = errno;
	  return;
     }

     if (bt != REPLY_TYPE)
	  rpc_err = RPC_PROTOCOL;

     return;

}

int recvint ()
{
     USPLong_integer li;

     if (USP_get_long_integer(us, &li) != SUCCESS) {
	  rpc_err = errno;
	  return(0);
     }

     return (li);
}

/*
 *
 * recvstr ()  -- Receive a string from an RPC call
 *
 */

char *recvstr ()
{
     USPString str;

     if (USP_get_string(us, &str) != SUCCESS) {
	  rpc_err = errno;
	  return("");
     }

     return (str);
}
unsigned short recvbool()
{
     USPBoolean flag;

     if (USP_get_boolean(us, &flag) != SUCCESS) {
	  rpc_err = errno;
	  return(0);
     }

     return (flag);
}

/*
 *
 * senddata () -- Routine to send the contents of a transaction file
 * 		  across the net.
 *
 */
senddata(tf)
tfile tf;
{
     int tfs,tocopy;
     char buffer[508];
     int result;

     topen (tf, "r", &result);
     USP_begin_block (us, TFILE_BLK);
     tfs = tfsize(tf);
     while (tfs > 0) {
	  tocopy = min (tfs, 508);
	  tread (tf, buffer, tocopy, &result);
	  if (result)
	       break;
	  USP_put_byte_block(us, buffer, tocopy);
	  tfs -= tocopy;
     }
     USP_end_block(us);
     tclose (tf, &result);
}

/*
 *
 * recvdata () -- Routine to receive a USP file.
 *
 */
recvdata(tf)
tfile tf;
{
     int tfs;
     char buffer[508];
     USPCardinal bt,actual;
     int result;
     
     if (USP_rcv_blk(us, &bt) != SUCCESS) {
	  rpc_err = errno;
	  return;
     }

     if (bt != TFILE_BLK) {
	  rpc_err = RPC_PROTOCOL;
	  return;
     }

     topen (tf, "w", &result);
     if (result) goto done;
     for (;;) {
	  if (USP_get_byte_block(us, buffer, 508, &actual) != SUCCESS) {
	       rpc_err = errno;
	       break;
	  }
	  if (actual == 0)
	       break;
	  twrite(tf, buffer, actual, &result);
	  if (result) break;
     }

     tclose (tf, &result);

 done:
     USP_flush_block(us);
}
/*
 *
 * panic -- just a printf
 *
 */
panic(str)
char *str;
{
     printf("panic: %s\n",str);
     perror("discuss");
     exit(1);
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
 *
 */


ExpandHost( alias, krb_host, krb_realm )
	char *alias,*krb_host,*krb_realm;

{
	struct hostent *h;
	char *p,*sp,*dp=krb_host;
	/*
	 * The convention established by the Kerberos-authenticated rcmd
	 * services (rlogin, rsh, rcp) is that the principal host name is
	 * all lower case characters.  Therefore, we can get this name from
	 * an alias by taking the official, fully qualified hostname, stripping off
	 * the domain info (ie, take everything up to but excluding the
	 * '.') and translating it to lower case.  For example, if "menel" is an
	 * alias for host officially named "menelaus" (in /etc/hosts), for 
	 * the host whose official name is "MENELAUS.MIT.EDU", the user could
	 * give the command "menel echo foo" and we will resolve it to "menelaus".
	 */
	*krb_realm = '\0';		/* null for now */
	if ( (h=gethostbyname(alias)) != (struct hostent *)NULL )
	     sp = h -> h_name;
	else
	     sp = alias;

	p = index( sp, '.' );
	if (p) {
	     char *p1;

	     strncpy(krb_realm,p+1,REALM_SZ);		/* Realm after '.' */
	     krb_realm[REALM_SZ-1] = NULL;
             p1 = krb_realm;                           /* Upper case this */
	     do {
		  if (islower(*p1)) *p1=toupper(*p1);
	     } while (*p1++);
	}
	/* lower case Kerberos host name */
	do {
	     if (isupper(*sp)) *dp=tolower(*sp);
	     else *dp = *sp;
	} while (*dp++,*sp && (*sp++ != '.'));
	*(--dp) = NULL;

	/* heuristics */
	if (*krb_realm == NULL)
	     get_krbrlm(krb_realm,1);
	if (!strcmp(krb_realm,"MIT.EDU"))
	     strcpy(krb_realm,"ATHENA.MIT.EDU");
	return;
}
#endif
