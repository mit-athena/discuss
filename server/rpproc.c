/*
 *
 *  rpproc.c -- Procedures to implement a simple (perhaps brain-asleep) RPC
 *		protocol over a TCP connection.
 *	     	This file handles the server's side of the connection.
 *
 */

#ifdef INETD
#define ASSOC 1
#endif

#ifdef SUBPROC
#define ASSOC 1
#endif

/* Includes */

#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <netinet/in.h>
#include <sys/stat.h>
#include <netdb.h>
#include <errno.h>
#include <pwd.h>
#include <strings.h>
#ifdef KERBEROS
#include <krb.h>
#endif
#include "../include/tfile.h"
#include "../include/rpc.h"
#include "../include/types.h"
#include "../include/config.h"

#define SUCCESS 1
#define ERROR   -1
#define min(a, b) (a < b ? a : b)

/* global */
char rpc_caller[50];

extern int numprocs;
extern struct proc_table procs [1];
extern char *malloc();
extern int errno;
short recvshort();
int rpc_err;
extern tfile net_tfile ();

/* Static variables */

/* connections & socket info */
static USPStream *us = NULL;

/*
 *
 * init_rpc () -- Initialize the RPC mechanism
 *
 */
init_rpc (service,code)
char *service;
int *code;
{
#ifdef INETD
     int d;
#endif
     int snew;					/* socket we're reading from */

#ifdef SUBPROC
     int uid;
     struct passwd *pwent;
#endif

#ifndef ASSOC
     struct protoent *pe;
     struct servent *se;
     struct sockaddr_in sai;
     int sock_len = sizeof (sai);
     int s;
#endif

#ifdef KERBEROS
     int fromlen,i;
     struct sockaddr_in from;
     char hostname[50];
     long hostaddr;
     char filename[50];
     char instance[INST_SZ];
     AUTH_DAT kdata;
     KTEXT_ST ticket;
     struct hostent *hp;
     USPCardinal bt;
#endif	  

     init_rpc_err_tbl();
     
#ifdef INETD
     d = open ("/dev/null", 2);
     dup2(d, 1);
     dup2(d, 2);
     close(d);
     if (geteuid() == 0)
	  panic ("Can't run as root.");		/* just in case setuid bit off */
#endif

#ifdef ASSOC
     /* safety check -- 0 better be a socket, not a pipe or file */
     {
	  if (isatty (0)) {
	       *code = RPC_NOT_SUBPROC;
	       return;
	  }
     }

     snew = 0;
     us = USP_associate (snew);
#else    
     /* to be added */
     setprotoent(0);			    /* get protocol information */
     pe = getprotobyname("tcp");
     setservent(0);			    /* get service information */
     
     if((se = getservbyname("discuss", "tcp")) == NULL) {
	  *code = RPC_SERV_UNKNOWN;
	  return;
     }
     sai.sin_addr.s_addr = INADDR_ANY;
     sai.sin_port = se->s_port;		    /* set up socket */
     if((s = socket(AF_INET, SOCK_STREAM, pe->p_proto)) < 0) {
	  *code = errno;
	  return;
     }
     if(bind(s, &sai, sizeof(sai))) {	    /* bind service name */
	  *code = errno;
	  return;
     }	
     listen(s, SOMAXCONN);		/* listen for connection */
     if((snew = accept(s, &sai, &sock_len)) < 0) { /* accept connection */
	  *code = errno;
	  return;
     }

     us = USP_associate (snew);
     if (us == NULL) {
	  *code = errno;
	  return;
     }
#endif

     strcpy (rpc_caller, "???");		/* safety drop */

#ifdef SUBPROC
     uid = getuid ();
     pwent = getpwuid(uid);
     if (pwent != 0) {
	  strcpy (rpc_caller, pwent -> pw_name);
     }
#endif
     strcat (rpc_caller, "@");
     strcat (rpc_caller, REALM);

#ifdef KERBEROS
     fromlen = sizeof (from);
     if (getpeername (snew, &from, &fromlen) < 0) {
	  *code = errno;
	  return;
     }
     if (fromlen == 0) {		/* no len, UNIX domain = me */
	  gethostname(hostname, sizeof(hostname));
	  hp = gethostbyname(hostname);
	  bcopy(hp -> h_addr, &hostaddr, 4);
     } else {
	  bcopy(&from.sin_addr, &hostaddr, 4);
     }

     if ((USP_rcv_blk(us, &bt) != SUCCESS) || bt != KRB_TICKET) {
	  *code = RPC_PROTOCOL;
	  return;
     }

     /* read authenticator off net */
     ticket.length = recvshort();
     if ((ticket.length<=0) || (ticket.length>MAX_KTXT_LEN)) {
	  goto punt_kerberos;
     }
     for (i=0; i<ticket.length; i++) {
	  ticket.dat[i] = recvshort();
     }

     /* make filename from service */
     strcpy (filename, "/usr/spool/");
     strcat (filename, service);
     strcat (filename, "/srvtab");

     strcpy(instance,"*");
     i = rd_ap_req(&ticket, service, instance, hostaddr, &kdata,filename);
     if (i == 0) {
	  strcpy(rpc_caller, kdata.pname);
	  if (kdata.pinst[0] != '\0') {
	       strcat(rpc_caller, ".");
	       strcat(rpc_caller, kdata.pinst);
	  }
	  strcat(rpc_caller, "@");
	  strcat(rpc_caller, kdata.prealm);
     }
     else {
	  goto punt_kerberos;
     }
punt_kerberos:
     USP_flush_block(us);
#endif
     *code = 0;
     return;
}

/*
 *
 * recvit ()  -- Routine to accept an RPC call.
 *
 */
recvit (code)
int *code;
{
     USPCardinal bt;
     int procno;

     if (USP_rcv_blk(us, &bt) != SUCCESS) {
	  if (errno = ECONNRESET) {		/* he went away, so do we */
	       *code = errno;
	  }
	  *code = errno;
	  return;
     }

     procno = bt - PROC_BASE;

     if (procno == 0) {
	  *code = RPC_PROTOCOL;
	  return;
     }
     if (procno > numprocs) {
	  USP_flush_block(us);
	  senddunno();
	  *code = 0;
	  return;
     }

     rpc_err = 0;
     dispatch (procno);
     *code = rpc_err;
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
short recvshort ()
{
     USPInteger li;

     if (USP_get_integer(us, &li) != SUCCESS) {
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

/*
 *
 * recvbool ()  -- Receive a boolean in an RPC call.
 *
 */
bool recvbool()
{
     USPBoolean flag;

     if (USP_get_boolean(us, &flag) != SUCCESS) {
	  rpc_err = errno;
	  return(0);
     }

     return ((bool)flag);
}

/*
 *
 * recvfile() -- Receive a file in an RPC call.
 *
 */
tfile recvfile ()
{
     USPLong_integer tfs;
     tfile tf;

     if (USP_get_long_integer(us, &tfs) != SUCCESS) {
	  rpc_err = errno;
	  return(0);
     }

     tf = net_tfile (tfs,us);

     return (tf);
}

/*
 *
 * startreply()  -- Get ready to send reply of an RPC call.
 *
 */
startreply()
{
     USP_begin_block(us,REPLY_TYPE);

     return;
}

/*
 *
 * sendint(i)  -- Send an integer in an RPC return.
 *
 */
sendint(i)
int i;
{
     if (USP_put_long_integer(us, i) != SUCCESS) {
	  rpc_err = errno + rpc_err_base;
     }
}

/*
 *
 * sendstr(i)  -- Send a string in an RPC return.
 *
 */
sendstr(str)
char *str;
{
     if (USP_put_string(us, str) != SUCCESS) {
	  rpc_err = rpc_err_base + errno;
	  return;
     }
}

/*
 *
 * sendbool(b)  -- Send a boolean in an RPC return.
 *
 */
sendbool(b)
bool b;
{
     if (USP_put_boolean(us, (USPBoolean)b) != SUCCESS) {
	  rpc_err = rpc_err_base + errno;
	  return;
     }
}

/*
 *
 * sendreply () -- Make the final call.
 *
 */
sendreply()
{
     if (USP_end_block(us) != SUCCESS) {
	  rpc_err = rpc_err_base + errno;
	  return;
     }
     return;
}

/*
 *
 * senddunno () -- Send a 'I don't know this call' reply
 *
 */
senddunno()
{
     USP_begin_block(us,UNKNOWN_CALL);
     if (USP_end_block(us) != SUCCESS) {
	  rpc_err = rpc_err_base + errno;
	  return;
     }
     return;
}
