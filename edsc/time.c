/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 * time-formatting routine to provide shorter output than ctime()
 * since the extra verbosity isn't necessary
 */

static char time_buf[15] = "xx/xx/xx xx:xx";
/* ctime format is "Sun Sep 16 01:03:52 1973\0" */
/*                  0         1         2   2   */
/*                  0         0         0   4   */
/* output format is "mm/dd/yy hh:mm\0"          */
/*                   0         1                */

#include <stdio.h>
#include <time.h>

char *
short_time(time)
     long *time;
{
     register struct tm *now;

     now = localtime(time);
     time_buf[2] = '/';
     time_buf[5] = '/';
     time_buf[8] = ' ';
     time_buf[11] = ':';
     time_buf[14] = '\0';
#define	put(n,o) {register int i,j;i=n/10;j=n-10*i;time_buf[o]='0'+i;time_buf[o+1]='0'+j;}
     now->tm_mon++;
     put(now->tm_mon, 0);
     put(now->tm_mday, 3);
     put(now->tm_year, 6);
     put(now->tm_hour, 9);
     put(now->tm_min, 12);
     return(time_buf);
}
