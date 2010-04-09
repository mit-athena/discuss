#!/bin/sh
# Set up a user for discuss.
# Writing this in C is overkill.
# True, but C-shell syntax is gross.
# So we rewrite it in Bourne shell

case "$@" in
    -q)
	quiet=0
	;;
    "")
	quiet=1
	;;
    *)
	echo "Usage: $0 [-q]\n"
	exit 1
	;;
esac


maybe_echo() {
    if [ $quiet -eq 1 ]; then
	echo "$@"
    fi
}

if [ -f ~/.meetings ]; then
    maybe_echo "  You appear to have a .meetings file; you don't need to run this."
    exit 0
fi

if [ -f ~/.disrc ]; then
    maybe_echo "  You appear to have a .disrc file left over from the experimental"
    maybe_echo "  version of discuss."
    maybe_echo "  Converting your .disrc file to a .meetings file:"
    disrc2meetings
else
    maybe_echo "  Creating .meetings file:"
    cat >~/.meetings << _EOF_
0:0:0:charon.mit.edu:/var/spool/discuss/new_meetings:New_meetings,new_meetings:
0:0:0:charon.mit.edu:/var/spool/discuss/eve:Everybody,eve:
_EOF_
    maybe_echo "  done."
fi
