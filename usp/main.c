/* UNIX Unified Stream Protocol 

   Copyright 1986 by the Massachusetts Institute of Technology 
   See permission and disclaimer notice in file "notice.h" 

   First implementation, long ago in a galaxy far far away: Ted Kim
   Many bug fixes: Mark L. Lambert
   
   7/1/86 SRZ: added USP_make_connection and USP_associate calls.  
   	       USP_accept_connection now calls USP_associate.

   7/7/86 MLL: USP_accept_connection now takes fork flag and does not fork if
	       flag value is FALSE.  This is for running USP applications under
	       dbx.  
	       Each interface operation now clears errno before beginning

   7/8/86 SRZ: Get_from_net bug fixed--a 0 return from recv used to cause an
               infinite loop if the other end disappeared suddenly.  Now
	       errno is set to ECONNRESET and ERROR is returned...

   7/9/86 MLL: Cleaned up last of TK brain damage (I think) and fixed a bug
	       in put_onto_net that caused it to return random garbage instead
	       of SUCCESS/ERROR.  
	       Punted USP_accept_connection
	       Punted select/send/recv calls in favor of simple read and write
	       calls
	       Get_from_net does not return ERROR/ECONNRESET on 0 byte read
	       return (turns out read call is allowed to return 0 bytes if no
	       data happens to be available at the moment).  

   7/10/86 MLL: replaced single read/write socket in USPStream structure with
                a pair of FILE stream pointers, created via fdopen after a
		DUP call.  This allows buffered data sends with (presumably)
		fewer packet transmissions.  Replaced read/write with fread/
		fwrite
		Added USP_put/get_byte_block calls to get at raw USP block
		
   8/26/86 MLL: Implemented open-connection, connection-error, end, and
                end-reply blocks as per the USP spec.  Yuk.


   This library contains the following calls:

   Connection operations (main.c)

   (USPStream *) USP_make_connection((char *), (char *))
   (USPStream *) USP_associate((int))
   USP_close_connection(USPStream *))

   Block operations (block.c)

   USP_rcv_blk((USPStream *), (USPCardinal *))
   USP_begin_block((USPStream *), (USPCardinal))
   USP_end_block((USPStream *))
   USP_flush_block((USPStream *))
   Boolean USP_end_of_block_p((USPStream *))

   Input operations (get.c)

   USP_get_boolean((USPStream *), (USPBoolean *))
   USP_get_integer((USPStream *), (USPInteger *))
   USP_get_cardinal((USPStream *), (USPCardinal *))
   USP_get_long_integer((USPStream *), (USPLong_integer *))
   USP_get_long_cardinal((USPStream *), (USPLong_cardinal *))
   USP_get_string((USPStream *), (USPString *))
   USP_get_byte_block((USPStream *), (Byte *), (unsigned), (unsigned *))

   Output operations (put.c)

   USP_put_boolean((USPStream *), (USPBoolean))
   USP_put_integer((USPStream *), (USPInteger))
   USP_put_cardinal((USPStream *), (USPCardinal))
   USP_put_long_integer((USPStream *), (USPLong_integer))
   USP_put_long_cardinal((USPStream *), (USPLong_cardinal))
   USP_put_string((USPStream *), (USPString))
   USP_put_byte_block((USPStream *), (Byte *), (unsigned))

   Miscellaneous operations (block.c)

   (char *) usp_error((int))

*/

/*
 * $Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/usp/main.c,v 1.4 1996-09-19 22:35:40 ghudson Exp $
 * $Source: /afs/dev.mit.edu/source/repository/athena/bin/discuss/usp/main.c,v $
 * $Locker:  $
 */

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include "gen.h"
#include "usp.h"

static char rcsid[] =
    "$Header: /afs/dev.mit.edu/source/repository/athena/bin/discuss/usp/main.c,v 1.4 1996-09-19 22:35:40 ghudson Exp $";

/* connection operations */

/* make a USP connection to a given server */

USPStream *USP_make_connection(host, service)

char    *host;
char	*service;
{
     struct sockaddr_in address;
     struct servent     *svc_info;
     struct hostent     *host_info;
     int 		s, melen = 64;
     char               me[65];
     USPStream          *us;

     errno = 0;
     if(! (svc_info = getservbyname(service, "tcp"))) {
	 return(NULL);
     }
     if(! (host_info = gethostbyname(host))) {
	 return(NULL);
     }
     memset(&address, 0, sizeof(address));
     memcpy(&address.sin_addr, host_info->h_addr, host_info->h_length);
     address.sin_family = host_info->h_addrtype;
     address.sin_port = svc_info->s_port;
     if((s = socket(host_info->h_addrtype, SOCK_STREAM, 0)) == ERROR) {
	 return(NULL);
     }
     if(connect(s, (char *) &address, sizeof(address)) == ERROR) {
	 return(NULL);
     }
     if((us = USP_associate(s))) {

	 /* set up and transmit "connection-open" block */

	 if(USP_begin_block(us, CONNECTION_OPEN) == ERROR) {
	     USP_shutdown(us);
	     return(NULL);
	 }
	 if(USP_put_cardinal(us, GLOBAL_NAME) == ERROR) {
	     USP_shutdown(us);
	     return(NULL);
	 }
	 if(USP_put_string(us, host_info->h_name) == ERROR) {
	     USP_shutdown(us);
	     return(NULL);
	 }
	 if(USP_put_cardinal(us, GLOBAL_NAME) == ERROR) {
	     USP_shutdown(us);
	     return(NULL);
	 }	 
	 if(gethostname(me, &melen) == ERROR) {
	     USP_shutdown(us);
	     return(NULL);
	 }	 
	 if(USP_put_string(us, me) == ERROR) {
	     USP_shutdown(us);
	     return(NULL);
	 }
	 if(USP_put_string(us, svc_info->s_name) == ERROR) {
	     USP_shutdown(us);
	     return(NULL);
	 }
	 if(USP_end_block(us) == ERROR) {
	     USP_shutdown(us);
	     return(NULL);
	 }
     }
     return(us);
}

/* associate a USP connection with an already open socket */

USPStream *USP_associate(s)

int	s;
{
     USPStream  *us;
     int	write_desc;
     int	on = 1;

     /* set up unified stream */
     
     if(! (us = (USPStream *) calloc(1, sizeof(USPStream)))) {
	  return(NULL);
     }
     /* (don't care much if this fails) */
     (void) setsockopt (s, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof(on));
#if 0
     write_desc = dup(s);
#else
     write_desc = s;
#endif
     if(write_desc == ERROR) {
	 cfree((char *) us);
	 return(NULL);
     }
     if(! (us->us_read = fdopen(s, "r"))) {
	 cfree((char *) us);
	 return(NULL);
     }
     if(! (us->us_write = fdopen(write_desc, "w"))) {
	 cfree((char *) us);
	 return(NULL);
     }
     us->us_in_receiving_p = FALSE;
     us->us_out_sending_p = FALSE;
     return(us);
}

/* initiate the close of a unified stream connection */

USP_close_connection(us)

USPStream	   *us;
{
    int	status = SUCCESS;
    USPCardinal bt;

    errno = 0;

    /* start USP close sequence */


    if(USP_begin_block(us, CONNECTION_END) == ERROR) {
	status = ERROR;
    }
    if(USP_end_block(us) == ERROR) {
	status = ERROR;
    }
    
    /* wait for CONNECTION-END-REPLY block */

    if(USP_rcv_blk(us, &bt) == ERROR) {
	status = ERROR;
    }
    else if(bt != CONNECTION_END_REPLY) {
	status = ERROR;
    }
    else if(USP_begin_block(us, CONNECTION_END_REPLY) == ERROR) {
	status = ERROR;
    }
    else if(USP_end_block(us) == ERROR) {
	status = ERROR;
    }
    status = USP_shutdown(us);
    return(status);
}

USP_shutdown(us)

USPStream *us;
{
    int status = SUCCESS;

    if(fclose(us->us_read) == EOF) { 
	status = ERROR;
    }
    if(fclose(us->us_write) == EOF) { 
	status = ERROR;
    }
    cfree((char *) us);
    return(status);
}
