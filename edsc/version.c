/*
 *
 *	Copyright (C) 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * version.c --- contains the version strings
 *
 */

#include "./edsc.h"
#include "./version.h"

#ifdef EDSC_CACHE
#define PROTOCOL_VERSION "25"
#else
#define PROTOCOL_VERSION "24"
#endif

char	*edsc_protocol_version = PROTOCOL_VERSION;
char	*edsc_version_string = VERSION_STRING;
