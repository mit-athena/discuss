#!/bin/sh
#

d=`pwd`
u=${USER-the_discuss_builder}
h=`hostname`
t=`date`

umask 002
/bin/echo "#define VERSION_STRING \"(${t}) ${u}@${h}:$d\"" >version.h
