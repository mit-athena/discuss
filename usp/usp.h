/* UNIX Unified Stream Protocol Header File 

   Copyright 1986 by the Massachusetts Institute of Technology 
   See permission and disclaimer notice in file "notice.h" 
 */ 

/* useful constants */

#define CLOSED		       99       /* connection ahs been closed */
#define EOB_FLAG               0x8000   /* end of block flag */
#define MAX_SUB_BLOCK_LENGTH   508	/* max sub-block length less hdr len */

#define CONNECTION_OPEN        10       /* USP connection-open block type */
#define CONNECTION_ERROR       20       /* USP connection-error block type */
#define CONNECTION_END         254      /* USP connection-end block type */
#define CONNECTION_END_REPLY   255      /* USP connection-end-reply block type */


#define FOREIGN_DETECT         1        /* error detected by foreign USP */
#define TRANSPORT_DETECT       2        /* error detected by transport layer */
#define INTERMEDIATE_DETECT    3        /* error detected by protocol converter */

/* USP internal error codes transmitted in CONNECTION_ERROR block */

#define CE_UNKNOWN             1

/* during connection establishment */

#define CE_NAME_UNKNOWN        10
#define CE_SERVICE_UNKNOWN     11
#define CE_HOST_DOWN           12
#define CE_SERVICE_UNSUPPORTED 13
#define CE_SERVICE_UNAVAILABLE 14
#define CE_CONVERSION_UNAVAILABLE 15

/* after connection establishment */


#define CE_TRANSPORT_FAILURE   20
#define CE_TRANSPORT_TIMEOUT   21
#define CE_CLIENT_FAILURE      22
#define CE_PATH_TRANSLATION    23

/* internal choice numbers for USP host names */

#define GLOBAL_NAME            0
#define PATH_NAME              1


/* USP-to-client error codes */

#include "usp_et.h"

/* USP data types */		   
		   
typedef unsigned short USPBoolean;
typedef short USPInteger;           
typedef unsigned short USPCardinal; 
typedef long USPLong_integer;       
typedef unsigned long USPLong_cardinal;
typedef char *USPString;

typedef struct {
    FILE	*us_read;
    FILE        *us_write;
    int	    	us_in_receiving_p;	    /* input: receiving block? */
    unsigned    us_last_sub_block_p;	    /* last sub-block flag */
    unsigned    us_nunread_sub_block_bytes; /* #bytes remaining in subblock */
    int         us_out_sending_p;	    /* output: sending block? */
    unsigned    us_nsent_sub_block_bytes;   /* buffer pointer */
    char    	us_outbuf[MAX_SUB_BLOCK_LENGTH];    /* output sub-block buf */
} USPStream;

USPStream     *USP_make_connection();
USPStream     *USP_associate();
char	      *usp_error();

