/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 * Fudge over differences between K&R(I) and ANSI C.  We won't have
 * <stdc.h> available until BSD 4.4 or so...
 */

#if !defined(__STDC__) && !defined(volatile)
#define volatile
#define const
#define signed
#endif
