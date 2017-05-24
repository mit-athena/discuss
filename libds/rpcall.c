/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 *  rpcall.c -- Procedures to implement a simple (perhaps brain-asleep) RPC
 *	  	protocol over a TCP connection.
 *		This file handles the caller's side of the connection.
 *
 *	$Id: rpcall.c,v 1.25 2007-08-09 20:41:32 amb Exp $
 *
 */
#ifndef lint
static char rcsid_rpcall_c[] =
    "$Id: rpcall.c,v 1.25 2007-08-09 20:41:32 amb Exp $";
#endif /* lint */

/* INCLUDES */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#include <unistd.h>
#endif
#include <discuss/tfile.h>
#include "rpc.h"
#include "config.h"

#ifdef NOKERBEROS
#undef HAVE_KRB4
#undef HAVE_KRB5
#endif /* NOKERBEROS */

#ifdef HAVE_KRB5
#include "krb5.h"
#endif /* HAVE_KRB5 */
#include "rpc_et.h"

/* DEFINES */

#define min(A, B) ((A) < (B) ? (A) : (B))
#define SUCCESS 1
#define ERROR   -1

/* EXTERNAL ROUTINES */

int rpc_err;

/* static variables and functions */

/* panic -- just a printf */
static void panic(str)
    char *str;
{
    fprintf(stderr, "panic: %s\n", str);
    perror("discuss");
    exit(1);
}

/* argument list info */
static int procno;				/* procedure number */

/* connections & socket info */
static USPStream *us = NULL;

/*
 *
 * startsend()  -- Get ready for an RPC call.
 *
 */
void startsend(whichproc)
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
void sendint(i)
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
void sendshort(i)
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
void sendstr(str)
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
void sendbool(b)
    unsigned short b;
{
    if (USP_put_boolean(us, b) != SUCCESS) {
	rpc_err = errno;
    }
}


/*
 *
 * rpc_sendfile(tf)  -- Send a file in an RPC call.
 *
 */
void rpc_sendfile(tf)
    tfile tf;
{
    int tfs;

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
void sendit(dest)
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
void init_rpc ()
{
#if defined(__APPLE__) && defined(__MACH__)
    add_error_table(&et_rpc_error_table);
    add_error_table(&et_usp_error_table);
#else
    initialize_rpc_error_table();
    initialize_usp_error_table();
#endif
}

/*
 *
 * term_rpc -- Shutdown the rpc mechanism
 *
 */
void term_rpc()
{
    flush_convs ();
    return;
}

/*
 *
 * close_rpc () -- Close down a specific rpc conversation
 *
 */
void close_rpc(rc)
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
void set_rpc(rc)
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
rpc_conversation open_rpc (host, port_num, service_id, code)
    char *host;			/* hostname to connect to */
    int port_num;		/* port number to use */
    char *service_id;		/* authenticator service id */
    register int *code;		/* return code */
{
    int parent,sv[2];
    rpc_conversation conv;
    struct hostent *hp;
    int authl;
    register int i, s = -1;

    char *server_name,*authp;
    struct sockaddr_in address, peeraddr;
    int retval, flags, arglen;

    *code = 0;

    if (service_id [0] == '/') { /* authenticate using sub-process */
	if (socketpair(AF_UNIX,SOCK_STREAM,0,sv) < 0)
	    panic ("can't do socket pair");

	parent = fork ();
	if (parent < 0)
	    panic ("Can't fork");
	if (!parent) {		/* child's play */
	    dup2(sv[1],0);	/* child takes second one */

	    for (i = 3; i < 20; i++)
		(void) close (i);

	    server_name = strrchr (service_id, '/');
	    if (server_name == NULL)
		server_name = service_id;
	    else
		server_name++;
	    execl(service_id, server_name, 0);
	    {
		char buf[100];
		sprintf(buf, "Can't exec %s", service_id);
		panic (buf);
	    }
	} else {
	    (void) close (sv[1]);
	    (void) fcntl (sv[0], F_SETFD, 1);
	    us = USP_associate (sv[0]);
	    return(us);
	}
    }

    hp = gethostbyname(host);
    if (hp == NULL) {
	extern int h_errno;
	int h = h_errno;
	switch (h) {
	case HOST_NOT_FOUND:
	    *code = RPC_HOST_UNKNOWN;
	    break;
	case TRY_AGAIN:
	    *code = RPC_NS_TIMEOUT;
	    break;
	case NO_RECOVERY:
	    *code = RPC_NS_ERROR;
	    break;
	case NO_ADDRESS:
	    *code = RPC_NO_ADDR;
	    break;
	default:
	    *code = RPC_NS_ERROR;
	    break;
	}
	return(NULL);
    }

    /* since we already have our port number, the following code was
       stolen from USP to manually set up the connection.  Note that
       one benefit from using the primitive USP routine (USP_associate)
       is that we eliminate an extra host lookup, and we don't have
       to rely on USP's primitive error mechanism.  Yeah! */

    memset(&address, 0, sizeof(address));
    memcpy(&address.sin_addr, hp->h_addr, hp->h_length);
    address.sin_family = hp->h_addrtype;
    address.sin_port = port_num;
    if((s = socket(hp->h_addrtype, SOCK_STREAM, 0)) < 0)
	goto punt;

    /* attempt to connect to the remote host, timing out after a
       pre-configured period of time.. */
    flags = fcntl(s, F_GETFL);
    if (flags < 0)
	goto punt;

    /* put socket in non-blocking mode */
    if (fcntl(s, F_SETFL, flags | O_NONBLOCK) < 0)
	goto punt;

    retval = connect(s, (struct sockaddr *) &address, sizeof(address));
    /* if connect returns an error immediately, punt */
    if (retval < 0 && errno != EINPROGRESS)
	goto punt;

    /* if not connected immediately, wait until connected or timed out */
    if (retval < 0) {
	struct timeval conn_timeout;
	fd_set fds;
	char c;

	conn_timeout.tv_sec = CONN_TIMEOUT;
	conn_timeout.tv_usec = 0;

	FD_ZERO(&fds);
	FD_SET(s, &fds);

	retval = select(s + 1, NULL, &fds, NULL, &conn_timeout);

	/* if select returned an error, punt */
	if (retval < 0)
	    goto punt;

	/* if no response, set errno and punt */
	if (retval == 0) {
	    errno = ETIMEDOUT;
	    goto punt;
	}

	/* presumably got a response; check if the connection is open */
	arglen = sizeof(peeraddr);
	if (getpeername(s, (struct sockaddr *)&peeraddr, &arglen) < 0) {
	    /* not connected; find out why and punt */
	    arglen = sizeof(errno);
	    (void) getsockopt(s, SOL_SOCKET, SO_ERROR, (void *)&errno, &arglen);
	    goto punt;
	}
    }

    /* put socket back in blocking mode */
    if (fcntl(s, F_SETFL, flags) < 0)
	goto punt;

    (void) fcntl (s, F_SETFD, 1);
    conv = USP_associate (s);
    us = conv;
    if (!us)
	goto punt;

    get_authenticator(service_id, 0, &authp, &authl, code);
    if (! *code) {
	USP_begin_block(us,KRB_TICKET);
	sendshort(authl);
	for (i = 0; i < authl; i++) {
	    sendshort(*authp++);
	}
	USP_end_block(us);
#ifdef HAVE_KRB5
	/* Prior to server version 3, Kerberos 5 wasn't an available
	 * authentication method, so we need to send a Kerberos 4 ticket.
	 * Unfortunately, there's no way to query the server version sooner. */
        krb5_error_code kcode;
        krb5_principal princ;
        char name[30];
        char inst[100];
        char realm[30];
        krb5_context context;
	if (get_server_version() < SERVER_3) {
#ifdef HAVE_KRB4
	    kcode = krb5_init_context(&context);
	    if (kcode) {
	        com_err("discuss", kcode, "while initializing krb5");
		return(conv);
	    }
	    kcode = krb5_parse_name(context, service_id, &princ);
	    if (kcode) {
	        com_err("discuss", kcode, "while parsing krb5 name");
		return(conv);
	    }
	    kcode = krb5_524_conv_principal(context, princ, name, inst, realm);
	    if (kcode) {
	        com_err("discuss", kcode, "while converting k5 princ to k4");
		return(conv);
	    }
            strcpy(service_id, name);
	    if (*inst) {
	        strcat(service_id, ".");
		strcat(service_id, inst);
	    }
	    strcat(service_id, "@");
	    strcat(service_id, realm);
	    get_authenticator_krb4(service_id, 0, &authp, &authl, code);
	    if (! *code) {
	        USP_begin_block(us,KRB_TICKET);
		sendshort(authl);
		for (i = 0; i < authl; i++) {
		    sendshort(*authp++);
		}
		USP_end_block(us);
	    }
#else /* HAVE_KRB4 */
            com_err("discuss", RPC_SERVER_TOO_OLD, "while authenticating to discuss server");
#endif /* HAVE_KRB4 */
	}
#endif /* HAVE_KRB5 */
    } else {
	USP_begin_block(us,KRB_TICKET);	/* send blank ticket */
	sendshort(0);
	USP_end_block(us);
    }
    return(conv);
punt:
    if (s >= 0) close(s);
    *code = errno;
    return(NULL);
}

/*
 *
 * recvreply ()  -- Routine to accept an RPC return.
 *
 */
void recvreply ()
{
    USPCardinal bt;

    if (USP_rcv_blk(us, &bt) != SUCCESS) {
	rpc_err = errno;
	return;
    }

    if (bt != REPLY_TYPE) {
	if (bt == UNKNOWN_CALL)
	    rpc_err = RPC_UNIMPL_CALL;
	else
	    rpc_err = RPC_PROTOCOL;
	USP_flush_block(us);
    }
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
    /* the invoking code will call free()
     * on values returned from this function */
    str = malloc(1);
    str[0] = '\0';
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
void senddata(tf)
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
void recvdata(tf)
    tfile tf;
{
    char buffer[508];
    USPCardinal bt;
    unsigned actual;
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
