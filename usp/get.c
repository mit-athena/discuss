/* UNIX Unified Stream Protocol 

   Copyright 1986 by the Massachusetts Institute of Technology 
   See permission and disclaimer notice in file "notice.h" 
*/

#include <sys/types.h>
#include <stdio.h>
#include "gen.h"
#include "usp.h"
#include <netinet/in.h>

/* input operations */

USP_get_boolean(us, bo)

USPStream   *us;
USPBoolean  *bo;
{
    unsigned	actual;

    errno = 0;
    if(! us->us_in_receiving_p) {
	errno = UENOTRCVING;
	return(ERROR);
    }
    if(get_from_sub_block(us, (char *) bo, sizeof(USPBoolean), 
			  &actual) == ERROR) {
	return(ERROR);
    }
    if(actual != sizeof(USPBoolean)) {
	errno = UEPREMEOB;
	return(ERROR);
    }
    *bo = (USPBoolean) ntohs((u_short) *bo);
    switch(*bo) {
      case TRUE:
      case FALSE:
	return(SUCCESS);
      default: 
	errno = UEBADATA;
	return(ERROR);
    }
}

USP_get_integer(us, ui) 

USPStream   *us;
USPInteger  *ui;
{
    unsigned	actual;

    errno = 0;
    if(! us->us_in_receiving_p) { 
	errno = UENOTRCVING;
	return(ERROR);
    }
    if(get_from_sub_block(us, (char *) ui, sizeof(USPInteger), 
			  &actual) == ERROR) {
	return(ERROR);
    }
    if(actual != sizeof(USPInteger)) {
	errno = UEPREMEOB;
	return(ERROR);
    }
    *ui = (USPInteger) ntohs((u_short) *ui);
    return(SUCCESS);
}

USP_get_cardinal(us, ca)

USPStream	*us;
USPCardinal     *ca;
{
    unsigned	actual;

    errno = 0;
    if(! us->us_in_receiving_p) { 
	errno = UENOTRCVING;
	return(ERROR);
    }
    if(get_from_sub_block(us, (char *) ca, sizeof(USPCardinal), 
			  &actual) == ERROR) {
    	return(ERROR);
    }
    if(actual != sizeof(USPCardinal)) {
	errno = UEPREMEOB;
	return(ERROR);
    }
    *ca = (USPCardinal) ntohs((u_short) *ca);
    return(SUCCESS);
}

USP_get_long_integer(us, li)

USPStream	*us;
USPLong_integer *li;
{
    unsigned	actual;

    errno = 0;
    if(! us->us_in_receiving_p) {
	errno = UENOTRCVING;
	return(ERROR);
    }
    if(get_from_sub_block(us, (char *) li, sizeof(USPLong_integer), 
			  &actual) == ERROR)  {
    	return(ERROR);
    }
    if(actual != sizeof(USPLong_integer)) {
	errno = UEPREMEOB;
	return(ERROR);
    }
    *li = (USPLong_integer) ntohl((u_long) *li); 
    return(SUCCESS);
}

USP_get_long_cardinal(us, lc)

USPStream        *us;
USPLong_cardinal *lc;
{
    unsigned	actual;

    errno = 0;
    if(! us->us_in_receiving_p) { 
	errno = UENOTRCVING;
	return(ERROR);
    }
    if(get_from_sub_block(us, (char *) lc, sizeof(USPLong_cardinal), 
			  &actual) == ERROR)  {
    	return(ERROR);
    }
    if(actual != sizeof(USPLong_cardinal)) {
	errno = UEPREMEOB;
	return(ERROR);
    }
    *lc = (USPLong_cardinal) ntohl((u_long) *lc);
    return(SUCCESS);
}

USP_get_string(us, str)

USPStream   *us;
USPString   *str;
{
    USPCardinal	sl;			/* string length */
    register char *sptr, c;
    register char *dptr;
    Boolean     oddp = FALSE;
    unsigned    actual;

    errno = 0;
    if(! us->us_in_receiving_p) {
	errno = UENOTRCVING; 
	return(ERROR);
    }
    if(get_from_sub_block(us, (char *)&sl, sizeof(USPCardinal), 
			  &actual) == ERROR) {
	return(ERROR);
    }
    if(actual != sizeof(USPCardinal)) {
	errno = UEPREMEOB;
	return(ERROR);
    }
    sl = ntohs(sl);
    if(sl & 1) oddp = TRUE;
    if(! (*str = (USPString) malloc(sl + 1))) {
	errno = UENOMEM;
	return(ERROR);
    }
    dptr = sptr = *str;

    if (get_from_sub_block (us, sptr, sl + oddp, &actual) == ERROR) {
	cfree (*str);
	*str = NULL;
	return (ERROR);
    }
    if (actual != sl + oddp) {
	errno = UEPREMEOB;
	return ERROR;
    }
    /* de-netasciify */

    while (actual > 0) {
	--actual;
	c = *sptr++;
	if(c == '\r' && actual > 0) {
	    --actual;
	    c = *sptr++;
	    if(c == '\012') {
		*dptr++ = '\n';
	    } else {
		*dptr++ = '\r';
	    }
	} else {
	    *dptr++ = c;
	}
    }
    *dptr = '\0';

    /* if length is odd, discard the last byte */
    return(SUCCESS);
}

/* this reads a raw block of bytes from a USP block.  For those of us who
   have no need of USP's data types... */

USP_get_byte_block(us, buf, len, actual)

USPStream	*us;
Byte		*buf;
unsigned	len;
unsigned	*actual;
{
    return(get_from_sub_block(us, buf, len, actual));
}
