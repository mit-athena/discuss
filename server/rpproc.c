/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 *  rpproc.c -- Procedures to implement a simple (perhaps brain-asleep) RPC
 *		protocol over a TCP connection.
 *	     	This file handles the server's side of the connection.
 *
 */

/*
 *
 *	$Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/server/rpproc.c,v $
 *	$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/server/rpproc.c,v 1.12 1996-09-19 22:32:25 ghudson Exp $
 *
 *	$Log: not supported by cvs2svn $
 *	Revision 1.11  1994/03/25 17:22:48  miki
 *	replaced bcopy with memmove for POSIX
 *
 * Revision 1.10  90/06/21  01:16:24  srz
 * Change the NOTTY of the controlling terminal to simply setting our
 * process group;  this is a better way of avoiding tty signals since the
 * open of /dev/tty can block in some circumstances.
 * 
 * Revision 1.9  89/06/03  00:43:37  srz
 * Added standard copyright notice.
 * 
 * Revision 1.8  89/01/29  17:16:57  srz
 * Add new kerberos ticket support.
 * 
 * Revision 1.7  89/01/29  13:38:23  srz
 * Ken's changes.
 * 
 * Revision 1.6  88/10/08  01:41:53  raeburn
 * Ensured that /dev/null file descriptor really is 1.
 * 
 * Revision 1.5  88/07/04  08:06:10  raeburn
 * Fixed names used in Kerberos library, to be compatible with new
 * library.
 * 
 * Revision 1.4  87/04/24  21:01:05  srz
 * Have subprocess close the tty, so that random signals can't get sent.
 * 
 *
 *
 */


#ifdef INETD
#define ASSOC 1
#endif

#ifdef SUBPROC
#define ASSOC 1
#endif

/* Includes */

#include <sys/ioctl.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <netinet/in.h>
#include <sys/stat.h>
#include <netdb.h>
#include <errno.h>
#include <pwd.h>
#include <string.h>
#ifdef KERBEROS
#include <krb.h>
#endif
#include <discuss/tfile.h>
#include "rpc.h"
#include <discuss/types.h>
#include "config.h"

#define SUCCESS 1
#define ERROR   -1
#define min(a, b) (a < b ? a : b)

/* global */
char rpc_caller[50];
static long hostaddr;

extern int numprocs;
extern struct proc_table procs [];
extern char *malloc();
extern int errno;
#ifdef KERBEROS
static char serv_name[20];
extern int krb_err_base;
#endif KERBEROS
short recvshort();
int rpc_err;
extern tfile net_tfile ();

/* Static variables */

#ifdef USPRPC
/* connections & socket info */
static USPStream *us = NULL;
#endif

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
    int snew;			/* socket we're reading from */
    
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
	panic ("Can't run as root."); /* just in case setuid bit off */
#endif
    
#ifdef ASSOC
    /* safety check -- 0 better be a socket, not a pipe or file */
    {
	if (isatty (0)) {
	    *code = RPC_NOT_SUBPROC;
	    return;
	}
    }

#ifdef SUBPROC
    {
	int s;
	for (s = 1; s < 10; s++)
	    (void) close (s);
    }
    {
	int fd;
	fd = open("/dev/null", 2);
	if (fd != 1) {
	    (void) dup2 (fd, 1);
	    (void) close (fd);
	}
	(void) dup2(1, 2);
    }
    {
	setpgrp(0, getpid());		/* So we don't get tty signals */
    }
#endif
    
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

    strcpy(serv_name, service);
    fromlen = sizeof (from);
    if (getpeername (snew, &from, &fromlen) < 0) {
	*code = errno;
	return;
    }
    if (fromlen == 0) {		/* no len, UNIX domain = me */
	gethostname(hostname, sizeof(hostname));
	hp = gethostbyname(hostname);
	memcpy(&hostaddr, hp -> h_addr,  4);
    } else {
	memcpy(&hostaddr, &from.sin_addr, 4);
    }

    
    if ((USP_rcv_blk(us, &bt) != SUCCESS) || (bt != KRB_TICKET &&
					      bt != KRB_TICKET2)) {
	*code = RPC_PROTOCOL;
	return;
    }
    
    handle_kerberos(bt,serv_name,hostaddr);
#endif
    *code = 0;
    return;
}

#ifdef KERBEROS
handle_kerberos(bt,service,haddr)
USPCardinal bt;
char *service;
long haddr;
{
    int i,result;
    char hostname[50];
    char filename[50];
    char instance[INST_SZ];
    AUTH_DAT kdata;
    KTEXT_ST ticket;

    strcpy (rpc_caller, "???@");		/* safety drop */
    strcat (rpc_caller, REALM);

    /* read authenticator off net */
    ticket.length = recvshort();
    if ((ticket.length<=0) || (ticket.length>MAX_KTXT_LEN)) {
	result = RPC_PROTOCOL;
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
    result = krb_rd_req (&ticket, service, instance, haddr, &kdata, filename);
    if (result == 0) {
	strcpy(rpc_caller, kdata.pname);
	if (kdata.pinst[0] != '\0') {
	    strcat(rpc_caller, ".");
	    strcat(rpc_caller, kdata.pinst);
	}
	strcat(rpc_caller, "@");
	strcat(rpc_caller, kdata.prealm);
    }
    else {
	result += krb_err_base;
	goto punt_kerberos;
    }
punt_kerberos:
    USP_flush_block(us);
    if (bt == KRB_TICKET2) {
	 USP_begin_block(us,TICKET_REPLY);
	 USP_put_long_integer(us, i);
	 USP_end_block(us);
    }
}
#endif KERBEROS

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

#ifdef KERBEROS
    if (bt == KRB_TICKET || bt == KRB_TICKET2) {
	 handle_kerberos(bt, serv_name, hostaddr);
	 *code = 0;
	 return;
    }
#endif KERBEROS

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
