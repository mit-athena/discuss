#!/bin/csh -f
# This takes a .disrc and creates a .meetings from it.
# Written by Mark Eichin 4/2/87 pending the installation of the new discuss.
#
# I wrote this in 15 minutes. If you want real meeting names, write
# a REAL program in C, or just fix them by hand with an editor.
#
# Modified same date Ken Raeburn to add 'echo' comments, and use
# homedir for files.
#
# Modified 4/8/87 Bill Sommerfeld to also add the long-form names to 
# the meeting (by calling a C program to do this..)
#
# usage: disrc2meetings
#

echo "  "
echo "  This shell script will create a .meetings file for you"
echo "  from a .disrc file if you do not have a .meetings file."
echo "  "

if ( ! -f ~/.disrc ) then
	echo "  You don't seem to have a .disrc file.  Run dsc_setup instead."
	exit
endif

if ( -f ~/.meetings) then
	echo "  You already have a meetings file.  Only adding long names.. "
else
	echo "  Creating .meetings file:"
	set noclobber
	sed < ~/.disrc >~/.meetings \
		's/\([^:]*\):\([^:]*\):\([^:]*\):\([^:]*\):[0-9]*\([^:]*\)/0:\3:\2:\4:\5:\1:/'

	echo "  Adding long names into .meetings file:"
endif

crmtgs
