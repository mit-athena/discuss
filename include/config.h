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
#define SERVER "/usr/local/disserve"
#define SERVER_LOCAL "/usr/local/disserve-fs"
#define SERVER_NAME "disserve"

/* Info directory */
#define INFO_DIR "/usr/sipb/lib/discuss/info"

/* Name server / Kerberos realm */
#define REALM  "ATHENA.MIT.EDU"

/* Service name */
#define SERVICE_NAME "discuss"

/* Place where dsmail can find dspipe */
#define DSPIPE "/usr/local/dspipe"

/* dsc_setup command */
#define DSC_SETUP "dsc_setup"
