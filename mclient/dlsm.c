/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */

/* 	This seems like a pretty useless program, if you ask me.  -srz */
main()
{
	execlp("discuss", "discuss", "-prompt", 
	       "discuss_list_meetings", "-request", "list_meetings", 
	       "-quit", 0);
	perror("discuss");
}
