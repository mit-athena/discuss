/* UNIX Unified Stream Protocol 

   Copyright 1986 by the Massachusetts Institute of Technology 
   See permission and disclaimer notice in file "notice.h" 
*/

#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include "gen.h"
#include "usp.h"
#include <netinet/in.h>

/* internal routines */

get_sub_block_header(us) 

USPStream    *us;
{
    USPCardinal sbl;			    /* sub-block length */

    /* get sub-block length (12 bits) + last-sub-block flag (4 bits) */
    
    if(get_from_net(us, (char *)&sbl, sizeof(USPCardinal)) == ERROR) {
	return(ERROR);
    }
    sbl = (USPCardinal) ntohs((u_short) sbl);
    if(sbl & EOB_FLAG) { 
	us->us_last_sub_block_p = TRUE;
    }
    else {
	us->us_last_sub_block_p = FALSE;
    }

    /* mask length to 12 bits less length of sub-block length field */

    us->us_nunread_sub_block_bytes = 
      (unsigned) ((sbl & 0x0FFF) - sizeof(USPCardinal));
    return(SUCCESS);
}

/* skip to end of sub-block */

skip_sub_block(us) 

USPStream    *us;
{
    Byte	buf[1024]; 

    if(us->us_nunread_sub_block_bytes == 0) {
	return(SUCCESS);
    }
    while(us->us_nunread_sub_block_bytes > 1024) {
	if(get_from_net(us, buf, 1024) == ERROR) {
	    return(ERROR);
	}
	us->us_nunread_sub_block_bytes -= 1024;
    }
    if(get_from_net(us, buf, us->us_nunread_sub_block_bytes) == ERROR) { 
	return(ERROR);
    }
    us->us_nunread_sub_block_bytes = 0;
    return(SUCCESS);
}

/* get <req> bytes from the net, putting the bytes in <buf> and returning
   the number of bytes actually read in <bytes-actually-read>.  Assumes
   the current sub_block has had its header read via get_sub_block_header */

get_from_sub_block(us, buf, req, bytes_actually_read) 

USPStream	*us;
Byte 	        *buf; 
unsigned        req; 
unsigned	*bytes_actually_read;
{
    /* read remainder of current sub-block plus whole sub-blocks until
       we have <req> bytes */

    *bytes_actually_read = 0;
    if(req == 0) {
	return(SUCCESS);
    }
    while (req > us->us_nunread_sub_block_bytes) {
	if(get_from_net(us, buf, us->us_nunread_sub_block_bytes) == ERROR) {
	    return(ERROR);
        }
	req -= us->us_nunread_sub_block_bytes;
	buf += us->us_nunread_sub_block_bytes;
	*bytes_actually_read += us->us_nunread_sub_block_bytes;
	if(us->us_last_sub_block_p) { 
	    us->us_nunread_sub_block_bytes = 0;
	    return(SUCCESS);	    /* block was shorter than we expected */
	}
	if(get_sub_block_header(us) == ERROR) { 
	    return(ERROR);
        }
    }
    if(get_from_net(us, buf, req) == ERROR) {	/* get rest of data */
	return(ERROR);
    }
    us->us_nunread_sub_block_bytes -= req; 
    *bytes_actually_read += req;
    return(SUCCESS);
}

get_from_net(us, buf, req) 

USPStream   *us;
Byte	    *buf;			/* buffer */
unsigned    req;			/* bytes requested */
{
    int bytes_read;

    while(req) {
	if((bytes_read = fread((char *) buf, sizeof(Byte), (int) req,
			       us->us_read)) == 0) {
            if(errno != 0) {
		return(ERROR);
	    }
	    else {
		errno = ECONNRESET;	/* end of file -- conn reset */
		return(ERROR);
	    }
	}
	req -= bytes_read;
	buf += bytes_read;
    }
    return(SUCCESS);
}

/* put the <len> bytes in <buf> into the current sub-block.  If the
   current sub-block fills up, send it and begin a new sub_block */

put_into_sub_block(us, buf, len) 

USPStream	*us;
Byte 	        *buf; 
unsigned        len;	
{
    unsigned avail = MAX_SUB_BLOCK_LENGTH - us->us_nsent_sub_block_bytes;

    while(len > avail) { 

	/* copy <avail> bytes of <buf> into <us_outbuf> and send <us_outbuf>
	   as a sub-block.  Thhen flush <us_outbuf> and put more bytes of
	   <buf> into it */

#ifdef POSIX
      memmove(us->us_outbuf + us->us_nsent_sub_block_bytes, buf, avail);
#else
	bcopy(buf, us->us_outbuf + us->us_nsent_sub_block_bytes, avail);
#endif
	len -= avail;
	buf += avail;
	us->us_nsent_sub_block_bytes += avail;
	if(send_sub_block(us, FALSE) == ERROR) {
	    return(ERROR);
        }
	avail = MAX_SUB_BLOCK_LENGTH; 
	us->us_nsent_sub_block_bytes = 0;	
    }

    /* put rest of <buf> into <us->us_outbuf> */

#ifdef POSIX
     memmove(us->us_outbuf + us->us_nsent_sub_block_bytes, buf, len);
#else
    bcopy(buf, us->us_outbuf + us->us_nsent_sub_block_bytes, len);
#endif
    us->us_nsent_sub_block_bytes += len;
    return(SUCCESS);
}

send_sub_block(us, last_p) 

USPStream  *us;
Boolean    last_p;		/* last sub-block flag */
{
    USPCardinal len = (USPCardinal) (us->us_nsent_sub_block_bytes + 
				     sizeof(USPCardinal));

    if(last_p) {		/* is this the last sub-block? */
	len |= EOB_FLAG;	/* then set last-sub-block flag */
    }
    len = (USPCardinal) htons((u_short) len);
    
    /* send length word */
    
    if(put_onto_net(us, (char *) &len, sizeof(USPCardinal), FALSE) == ERROR) {
	return(ERROR);
    }

    /* and send outbuf */

    if(put_onto_net(us, (char *) us->us_outbuf, us->us_nsent_sub_block_bytes, 
		    last_p) == ERROR) { 
	return(ERROR);
    }
    return(SUCCESS);
}

/* put bytes onto network */

put_onto_net(us, buf, len, send_p) 

USPStream	*us;
Byte	    	*buf;			/* buffer */
unsigned     	len;			/* buffer length */
Boolean	     	send_p;			/* if TRUE, force write */
{
    int	    bytes_sent;

    if((bytes_sent = fwrite((char *) buf, sizeof(Byte), (int) len,
			    us->us_write)) != len) {
	return(ERROR);
    }
    if(bytes_sent == 0 && errno != 0) {
	return(ERROR);
    }
    if(send_p) {
	fflush(us->us_write);
    }
    return(SUCCESS);
}
