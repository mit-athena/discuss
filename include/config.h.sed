/*
 *
 *	Copyright (C) 1988, 1989 by the Massachusetts Institute of Technology
 *    	Developed by the MIT Student Information Processing Board (SIPB).
 *    	For copying information, see the file mit-copyright.h in this release.
 *
 */
/*
 *
 * config.h  -- Configuration file for DISCUSS.  Fix this up with your
 *		site specific options.
 *
 */

/* Descriptions of definitions:

REALM:  This is the name of the local Kerberos realm.  This is used to
identify the administrative realm when using the "disserve" subprocess
server, and the identity of non-authenticated users "???@REALM".  Set
this to your local Kerberos realm if you have one, or a possible
Kerberos realm name (the current convention is to use a Internet
Domain name; e.g., "LCS.MIT.EDU").

INFO_DIR: This is the directory where the command-based Discuss client
finds its help files.  This would normally be under /usr/lib/discuss,
or /usr/local/lib/discuss.  The help files are in the distribution
under source/client/info; to install, copy/tar the directory into the
desired location.

SERVICE_NAME:  This is the service name used to find a port number.
The default is "discuss", port 2100/tcp.

SERVER:  This is the location of the discuss subprocess server.  When
a client program accesses meetings on the same machine, discuss bypasses the
network and creates a subprocess server.  Set this to the location of
the server program, where disserve will be installed.

SERVER_LOCAL: This is the location of a copy of the subprocess server which
is not a protected subsystem, but relies on filesystem protections instead.

SERVER_NAME:  The name of the server program (argv[0] of the subprocess).

DSPIPE:  This was used by dsmail.c to enter transactions into
meetings;  it is obsolete.

DSC_SETUP:  This name of the DSC_SETUP program.  This program is
invoked by the various clients when a user first runs Discuss, to
create an initial .meetings file. */


/* Server to exec (assumed to be in SERVERDIR) */
#ifndef SERVER
#define SERVER "/usr/athena/lib/disserve"
#endif
#ifndef SERVER_LOCAL
/* this one is path-searched if it doesn't begin with slash... */
#define SERVER_LOCAL "disserve-fs"
#endif
#ifndef SERVER_NAME
#define SERVER_NAME "disserve"
#endif

/* Info directory */
#ifndef INFO_DIR
#define INFO_DIR "/usr/athena/lib/discuss/info"
#endif

/* Name server / Kerberos realm */
#ifndef REALM
#define REALM  "ATHENA.MIT.EDU"
#endif

/* Service name */
#ifndef SERVICE_NAME
#define SERVICE_NAME "discuss"
#endif

/* Place where dsmail can find dspipe */
#ifndef DSPIPE
#define DSPIPE "/usr/local/dspipe"
#endif

/* dsc_setup command */
#ifndef DSC_SETUP
#define DSC_SETUP "dsc_setup"
#endif
