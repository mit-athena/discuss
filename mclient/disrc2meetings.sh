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

#
# usage: disrc2meetings
#

echo "  "
echo "  This shell script will create a .meetings file for you"
echo "  from a .disrc file."
echo "  "
echo "  This new .meetings file will use only the secondary"
echo "  names of meetings; if you want to have the real names,"
echo "  you can add them by hand -- we haven't got a tool to do"
echo "  it yet, unfortunately..  The namelist is the last"
echo "  nonblank field, with commas between names."
echo "  "

set noclobber
cat ~/.disrc \
| awk -F: '{ print "0:"$3":"$2":"$4":"$5":"$5":" }' \
| sed "s.:[0-9]*/.:/." \
| sed 's&:[^:]*/\([^:]*\):$&:\1:&' > ~/.meetings
