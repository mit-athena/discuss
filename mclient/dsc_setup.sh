#!/bin/csh -f
# Set up a user for discuss.
# Writing this in C is overkill.
# True, but C-shell syntax is gross.

if ( -f ~/.meetings ) then
		if ($#argv == 0) echo "  You appear to have a .meetings file; you don't need to run this."
	exit
endif

if ( -f ~/.disrc ) then
	if ($#argv == 0) then
		echo "  You appear to have a .disrc file left over from the experimental"
		echo "  version of discuss."
		echo "  Converting your .disrc file to a .meetings file:"
	endif
	disrc2meetings
else
	if ($#argv == 0) echo "  Creating .meetings file:"
	cat >~/.meetings << _EOF_
0:0:0:charon.mit.edu:/usr/spool/discuss/new_meetings:New_meetings,new_meetings:
0:0:0:charon.mit.edu:/usr/spool/discuss/eve:Everybody,eve:
_EOF_
	if ($#argv == 0) echo "  done."
endif
