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
 *	$Id: rpproc.c,v 1.23 2007-08-09 20:41:33 amb Exp $
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
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <sys/stat.h>
#include <netdb.h>
#include <errno.h>
#include <pwd.h>
#include <string.h>
#ifdef HAVE_KRB4
#include <krb.h>
#endif
#ifdef HAVE_KRB5
#include <krb5.h>
#endif
#include <discuss/tfile.h>
#include "rpc.h"
#include <discuss/types.h>
#include "config.h"

#define SUCCESS 1
#define ERROR   -1
#define min(a, b) (a < b ? a : b)

/* global */
#ifdef HAVE_KRB4
char rpc_caller[MAX_K_NAME_SZ + 1];
#else
char rpc_caller[50];
#endif
static long hostaddr;

extern int numprocs;
extern struct proc_table procs [];
#ifdef HAVE_KRB4
static char serv_name[20];
#endif /* HAVE_KRB4 */
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
    
#ifdef HAVE_KRB4
    int fromlen,i;
    struct sockaddr_in from;
    char hostname[50];
    struct hostent *hp;
    USPCardinal bt;
#endif	  

#if defined(__APPLE__) && defined(__MACH__)
    add_error_table(&et_rpc_error_table);
#else
    initialize_rpc_error_table();
#endif

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
    
    se = getservbyname(SERVICE_NAME, "tcp");
    sai.sin_addr.s_addr = INADDR_ANY;
    sai.sin_port = (se) ? se->s_port : htons(DISCUSS_FALLBACK_PORT);
					    /* set up socket */
    if((s = socket(AF_INET, SOCK_STREAM, pe->p_proto)) < 0) {
	*code = errno;
	return;
    }
    if(bind(s, (struct sockaddr *)&sai, sizeof(sai))) {	 /* bind service name */
	*code = errno;
	return;
    }	
    listen(s, SOMAXCONN);		/* listen for connection */
    if((snew = accept(s, (struct sockaddr *)&sai, &sock_len)) < 0) {
                                        /* accept connection */
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
    
#ifdef HAVE_KRB4

    strcpy(serv_name, service);
    fromlen = sizeof (from);
    if (getpeername (snew, (struct sockaddr *)&from, &fromlen) < 0) {
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

#ifdef HAVE_KRB4
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

#ifdef HAVE_KRB5
    char *envvar;
    krb5_context context;
    krb5_auth_context auth_context = NULL;
    krb5_data packet;
    krb5_principal sprinc;
    krb5_keytab keytab = NULL;
    krb5_ticket *processed_ticket = NULL;
#endif /* HAVE_KRB5 */

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
#ifdef HAVE_KRB5
    packet.length = ticket.length;
    packet.data = (krb5_pointer) ticket.dat;
    
    envvar = malloc(strlen(service) + 50);
    if (envvar) {
	sprintf(envvar, "KRB5_KTNAME=/var/spool/%s/krb5.keytab", service);
	putenv(envvar);
    }
#endif /* HAVE_KRB5 */
    /* make filename from service */
    strcpy (filename, "/var/spool/");
    strcat (filename, service);
    strcat (filename, "/srvtab");

    strcpy(instance, "*");

#ifdef HAVE_KRB5
    result = krb5_init_context(&context);
    if (result) {
        com_err(service, result, "while initializing krb5");
	goto punt_kerberos;
    }
    result = krb5_sname_to_principal(context, NULL, service, KRB5_NT_SRV_HST,
                                     &sprinc);
    if (result) {
        com_err(service, result, "while generating srv name %s", service);
	goto punt_kerberos;
    }
    result = krb5_rd_req(context, &auth_context, &packet, sprinc, keytab,
                         NULL, &processed_ticket);
    if (result == 0) {  /* It's a valid krb5 request */
        result = krb5_524_conv_principal(context,
	                                 processed_ticket->enc_part2->client,
					 kdata.pname, kdata.pinst,
					 kdata.prealm);
	if (result) {
	    com_err(service, result, "while converting principal to krb4");
	    goto punt_kerberos;
	}
    }
    else {  /* Let's try krb4 */
	/* First, log the krb5 error. */
	com_err(service, result, "while reading request");
#endif /* HAVE_KRB5 */
        result = krb_rd_req (&ticket, service, instance, haddr, &kdata,
	                     filename);
	if (result) {
	    result += ERROR_TABLE_BASE_krb;
	    goto punt_kerberos;
	}
#ifdef HAVE_KRB5
    }
#endif /* HAVE_KRB5 */

    strcpy(rpc_caller, kdata.pname);
    if (kdata.pinst[0] != '\0') {
        strcat(rpc_caller, ".");
	strcat(rpc_caller, kdata.pinst);
    }
    strcat(rpc_caller, "@");
    strcat(rpc_caller, kdata.prealm);

punt_kerberos:
    USP_flush_block(us);
    if (bt == KRB_TICKET2) {
	 USP_begin_block(us,TICKET_REPLY);
	 USP_put_long_integer(us, i);
	 USP_end_block(us);
    }
}
#endif /* HAVE_KRB4 */

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
	if (errno == ECONNRESET) {		/* he went away, so do we */
	    *code = errno;
	}
	*code = errno;
	return;
    }

#ifdef HAVE_KRB4
    if (bt == KRB_TICKET || bt == KRB_TICKET2) {
	 handle_kerberos(bt, serv_name, hostaddr);
	 *code = 0;
	 return;
    }
#endif /* HAVE_KRB4 */

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
	rpc_err = errno + ERROR_TABLE_BASE_rpc;
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
    if (str == NULL)
      str = "";
    if (USP_put_string(us, str) != SUCCESS) {
	rpc_err = ERROR_TABLE_BASE_rpc + errno;
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
	rpc_err = ERROR_TABLE_BASE_rpc + errno;
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
	rpc_err = ERROR_TABLE_BASE_rpc + errno;
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
	rpc_err = ERROR_TABLE_BASE_rpc + errno;
	return;
    }
    return;
}
