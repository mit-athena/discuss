/* UNIX Unified Stream Protocol 

   Copyright 1986 by the Massachusetts Institute of Technology 
   See permission and disclaimer notice in file "notice.h" 
*/

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include "gen.h"
#include "usp.h"
#include <netinet/in.h>

char *error_message();
/* USP block operations */

int usp_internal_errno = 0;
char usp_internal_string[256];

char *usp_setup_errlist[] = {
    "foreign name unknown",
    "service identifier unknown",
    "host down",
    "service not supported on host",
    "service not currently available",
    "protocol conversion not available"};

char *usp_during_errlist[] = {
    "transport failure",
    "transport timeout",
    "foreign client failure",
    "path transformation impossible"};

USP_rcv_blk(us, bt) 

USPStream   *us;
USPCardinal *bt;
{
    errno = 0;

    /* get block type */
    
    if(get_from_net(us, (char *) bt, sizeof(USPCardinal)) == ERROR) {
    	return(ERROR);
    }
    *bt = ntohs(*bt); 
    us->us_in_receiving_p = TRUE;
    us->us_nunread_sub_block_bytes = 0;
    if(get_sub_block_header(us) == ERROR) { 
	return(ERROR); 
    }

    /* sneak a look at the block type.  If CONNECTION-ERROR,
       intercept it, parse the reason and return an error to
       the client */

    if(*bt == CONNECTION_ERROR) {
	USPCardinal what_error, who_reported;
	USPString   info, module_name;
	char        what_string[256], who_string[65];
	
	errno = UEINTERNAL;
	if(USP_get_cardinal(us, &what_error) == ERROR) {
	    USP_flush_block(us);
	    strcpy(usp_internal_string, "CONNECTION-ERROR block read error");
	    return(ERROR);
	}
	switch(what_error) {
	  case CE_UNKNOWN:
	    strcpy(what_string, "unknown internal error");
	    break;
	  case CE_NAME_UNKNOWN:
	  case CE_SERVICE_UNKNOWN:
	  case CE_HOST_DOWN:
	  case CE_SERVICE_UNSUPPORTED:
	  case CE_SERVICE_UNAVAILABLE:
	  case CE_CONVERSION_UNAVAILABLE:
	    strcpy(what_string, usp_setup_errlist[what_error - CE_NAME_UNKNOWN]);
	    break;
	  case CE_TRANSPORT_FAILURE:
	  case CE_TRANSPORT_TIMEOUT:
	  case CE_CLIENT_FAILURE:
	  case CE_PATH_TRANSLATION:
	    strcpy(what_string, usp_during_errlist[what_error - CE_TRANSPORT_FAILURE]);
	    break;
	  default:
	    sprintf(what_string, "(unknown internal error code %u)", what_error);
	    break;
	}
	if(USP_get_string(us, &info) == ERROR) {
	    USP_flush_block(us);
	    strcpy(usp_internal_string, "CONNECTION-ERROR block read error");
	    return(ERROR);
	}
	if(USP_get_cardinal(us, &who_reported) == ERROR) {
	    USP_flush_block(us);
	    strcpy(usp_internal_string, "CONNECTION-ERROR block read error");
	    return(ERROR);
	}
	switch(who_reported) {
	  case FOREIGN_DETECT:
	    strcpy(who_string, "foreign client");
	    break;
	  case TRANSPORT_DETECT:
	    strcpy(who_string, "transport layer");
	    break;
	  case INTERMEDIATE_DETECT:
	    strcpy(who_string, "protocol converter");
	    break;
	  default:
	    strcpy(who_string, "[unknown]");
	}
	if(USP_get_string(us, &module_name) == ERROR) {
	    USP_flush_block(us);
	    strcpy(usp_internal_string, "CONNECTION-ERROR block read error");
	    return(ERROR);
	}
	sprintf(usp_internal_string, "Internal error \"%s\" (%s), detected by %s \"%s\"",
		what_string, info, who_string, module_name);
	cfree((char *) info);
	cfree((char *) module_name);
	return(ERROR);
    }

    /* if other end wishes to shut down, fine... */

    else if(*bt == CONNECTION_END) {
	if(USP_begin_block(us, CONNECTION_END_REPLY)) 
	  ;
	if(USP_end_block(us) == ERROR) 
	  ;
	return(CLOSED);
    }

    /* if other end is sending an OPEN block, ignore it and get the next
       block.  Note that we might recurse indefinitely if the other end
       never sends anything but CONNECTION-OPEN blocks... */

    else if(*bt == CONNECTION_OPEN) {
	USP_flush_block(us);
	return(USP_rcv_blk(us, bt));
    }
    return(SUCCESS);
}

USP_begin_block(us, bt) 

USPStream   *us;
USPCardinal bt;
{
    errno = 0;
    if(us->us_out_sending_p) {		/* already sending a block! */
	errno = UECBNOTENDED;
	return(ERROR);
    }
    bt = htons(bt); 
    if(put_onto_net(us, (char *) &bt, sizeof(USPCardinal), FALSE) == ERROR) {
	return(ERROR);
    }
    us->us_out_sending_p = TRUE;
    us->us_nsent_sub_block_bytes = 0;					
    return(SUCCESS);
}

/* end block and flush buffer */

USP_end_block(us) 

USPStream    *us;
{
    errno = 0;
    if(! us->us_out_sending_p) { 
	errno = UENOTSENDING;
	return(ERROR);
    }
    if(send_sub_block(us, TRUE) == ERROR) { /* force send of last sub-block */
    	return(ERROR);
    }
    us->us_out_sending_p = FALSE;
    return(SUCCESS);
}

/* skip to end of block (does nothing if already there) */

USP_flush_block(us) 

USPStream    *us;
{
    errno = 0;
    if(! us->us_in_receiving_p) {		/* already there? */
	return;
    }
    while(! us->us_last_sub_block_p) {	       
	if(skip_sub_block(us) == ERROR) {
	    return;
	}
	if(get_sub_block_header(us) == ERROR) {
	    return;
	}
    }

    /* and skip over the last sub-block */

    if(skip_sub_block(us) == ERROR) { 
	return;
    }
    us->us_in_receiving_p = FALSE;
}


Boolean USP_end_of_block_p(us)

USPStream	*us;
{
    return(((us->us_nunread_sub_block_bytes == 0) && us->us_last_sub_block_p) ?
	   TRUE : FALSE);
}



char *usp_error(errnum)

int errnum;
{
    if(errnum == UEINTERNAL) {
	return(usp_internal_string);
    }
    else {
	return(error_message(errnum));
    }
}
