/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 * Hand-modified version -- do not delete.
 */

#define NULL 0
#ifdef __STDC__
#define NOARGS void
#else
#define NOARGS
#define const
#endif

extern const char * const krb_err_txt[256];
extern int init_error_table();

const int krb_err_base = 39525376;

int init_krb_err_tbl (NOARGS) {
    return(init_error_table(krb_err_txt, 39525376, 200));
}
