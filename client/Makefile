LIBS= ../server/acl.o ../lib/libdsk.a ../ets/dsc_et.o ../lib/libusp.a \
	../lib/libcom_err.a libss.a ../lib/libkrb.a ../lib/libdes.a

CFLAGS=	-g -I../include
LFLAGS=	-uhv -I../include

OBJS=	discuss.o edit.o output.o trn_expr.o list.o trn_select.o \
	status.o new_trans.o print.o lsm.o addmtg.o list_acl.o \
	interrupt.o goto.o reply.o \
	cmds.o ../ets/discuss_err.o

SRCS=	discuss.c edit.c output.c trn_expr.c list.c trn_select.c \
	status.c new_trans.c print.c lsm.c addmtg.c list_acl.c \
	interrupt.c goto.c reply.c

.SUFFIXES:	.ct .h

.ct.o:	
	./make_commands $*.ct

.y.o:	
	-rm -f $*.c $*.o
	yacc $*.y
	mv y.tab.c $*.c
	cc $(CFLAGS) -c $*.c

.c.o:
	-rm -f $*.o
	cc $(CFLAGS) -c $*.c

#

all:	discuss tags

discuss: $(OBJS) $(LIBS)
	-rm -f discuss
	cc $(CFLAGS) -o discuss $(OBJS) $(LIBS)

tags:	TAGS

TAGS:	$(SRCS)

lint:	$(SRCS) ../server/acl.c ../libds/llib-lds.ln ../lib/llib-lcom_err.ln \
			llib-lss.ln
	lint $(LFLAGS) $(SRCS) ../lib/llib-lcom_err.ln llib-lss.ln \
			../server/acl.c ../libds/llib-lds.ln |\
		grep -v " unused in function " |\
		grep -v " set but not used in function " |\
		grep -v "possible pointer alignment "

clean:
	rm -f *.o *~ \#* *.bak
	rm -f discuss trn_expr.c y.tab.c TAGS

dist:	$(SRCS) Makefile make_commands libss.a trn_expr.y cmds.ct
	-mkdir ../dist/client
	cp -p $(SRCS) ../dist/client/
	cp -p Makefile ../dist/client/
	cp -p trn_expr.y cmds.ct ../dist/client/
	tar cf - info | (cd ../dist/client ; tar xfv - )
	ln -s ../include/ss.h ../dist/client/ss.h
	ln -s ../include/ss_err.h ../dist/client/ss_err.h
	ln -s ../ss/make_commands ../dist/client/make_commands
	ln -s ../lib/libss.a ../dist/client/

./discuss_err.h: discuss_err.h

# 'make depend' code
CFILES=	$(SRCS)
depend: ${CFILES}
	${CC} -M -I../include ${COPTS} ${CFILES} | \
	sed -e ':loop' \
	    -e 's/\.\.\/[^ /]*\/\.\./../' \
	    -e 't loop' | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ex - Makefile < eddep
	rm eddep makedep
#
# the last constant line in the makefile should be...
# DO NOT DELETE THIS LINE

discuss.o: discuss.c /usr/include/stdio.h /usr/include/sys/file.h
discuss.o: /usr/include/signal.h /usr/include/strings.h /usr/include/sys/wait.h
discuss.o: /usr/include/pwd.h ./ss.h ./ss_err.h ../include/tfile.h
discuss.o: ../include/interface.h ../include/types.h ../include/config.h
discuss.o: ../include/rpc.h /usr/include/stdio.h ../include/usp.h
discuss.o: ../include/usp_et.h ../include/rpc_et.h ../include/globals.h
discuss.o: ../include/dsname.h ../include/acl.h ../include/types.h
edit.o: edit.c /usr/include/stdio.h /usr/include/sys/file.h
edit.o: /usr/include/strings.h /usr/include/signal.h ./ss.h ./ss_err.h
edit.o: ../include/tfile.h ../include/interface.h ../include/types.h
edit.o: ../include/config.h ../include/discuss_err.h ../include/globals.h
edit.o: ../include/dsname.h /usr/include/sys/wait.h /usr/include/sys/types.h
edit.o: /usr/include/sys/stat.h /usr/include/sys/errno.h
output.o: output.c /usr/include/stdio.h /usr/include/sys/file.h
output.o: /usr/include/strings.h /usr/include/signal.h ./ss.h ./ss_err.h
output.o: ../include/tfile.h ../include/interface.h ../include/types.h
output.o: ../include/config.h ../include/discuss_err.h ../include/globals.h
output.o: ../include/dsname.h
trn_expr.o: trn_expr.c /usr/include/ctype.h ../include/interface.h
trn_expr.o: ../include/types.h ../include/discuss_err.h ../include/globals.h
trn_expr.o: ../include/dsname.h
list.o: list.c /usr/include/stdio.h /usr/include/strings.h ./ss.h ./ss_err.h
list.o: ../include/tfile.h ../include/interface.h ../include/types.h
list.o: ../include/config.h ../include/dsc_et.h ../include/globals.h
list.o: ../include/dsname.h
trn_select.o: trn_select.c /usr/include/stdio.h ../include/interface.h
trn_select.o: ../include/types.h ../include/discuss_err.h ../include/globals.h
trn_select.o: ../include/dsname.h
status.o: status.c /usr/include/stdio.h ./ss.h ./ss_err.h
status.o: ../include/interface.h ../include/types.h ../include/config.h
status.o: ../include/globals.h ../include/dsname.h ../include/acl.h
status.o: ../include/types.h
new_trans.o: new_trans.c /usr/include/stdio.h /usr/include/sys/file.h
new_trans.o: /usr/include/signal.h /usr/include/strings.h
new_trans.o: /usr/include/sys/wait.h ./ss.h ./ss_err.h ../include/tfile.h
new_trans.o: ../include/interface.h ../include/types.h ../include/config.h
new_trans.o: ../include/globals.h ../include/dsname.h ../include/acl.h
new_trans.o: ../include/types.h
print.o: print.c /usr/include/stdio.h /usr/include/errno.h
print.o: /usr/include/sys/file.h /usr/include/signal.h /usr/include/strings.h
print.o: /usr/include/sys/wait.h ./ss.h ./ss_err.h ../include/tfile.h
print.o: ../include/interface.h ../include/types.h ../include/config.h
print.o: ../include/dsc_et.h ../include/globals.h ../include/dsname.h
lsm.o: lsm.c /usr/include/strings.h /usr/include/stdio.h ../include/types.h
lsm.o: ../include/interface.h ../include/types.h ../include/globals.h
lsm.o: ../include/dsname.h
addmtg.o: addmtg.c /usr/include/strings.h /usr/include/stdio.h
addmtg.o: /usr/include/sys/file.h ../include/tfile.h ../include/types.h
addmtg.o: ../include/interface.h ../include/types.h ../include/globals.h
addmtg.o: ../include/dsname.h
list_acl.o: list_acl.c /usr/include/strings.h /usr/include/stdio.h
list_acl.o: ../include/types.h ../include/interface.h ../include/types.h
list_acl.o: ../include/acl.h ../include/types.h ../include/globals.h
list_acl.o: ../include/dsname.h
interrupt.o: interrupt.c /usr/include/signal.h
goto.o: goto.c /usr/include/stdio.h /usr/include/sys/file.h
goto.o: /usr/include/signal.h /usr/include/strings.h /usr/include/sys/wait.h
goto.o: ./ss.h ./ss_err.h ../include/tfile.h ../include/interface.h
goto.o: ../include/types.h ../include/config.h ../include/rpc.h
goto.o: /usr/include/stdio.h ../include/usp.h ../include/usp_et.h
goto.o: ../include/rpc_et.h ../include/globals.h ../include/dsname.h
goto.o: ../include/acl.h ../include/types.h
reply.o: reply.c /usr/include/stdio.h /usr/include/sys/file.h
reply.o: /usr/include/strings.h /usr/include/sys/wait.h ./ss.h ./ss_err.h
reply.o: ../include/tfile.h ../include/interface.h ../include/types.h
reply.o: ../include/config.h ../include/rpc.h /usr/include/stdio.h
reply.o: ../include/usp.h ../include/usp_et.h ../include/rpc_et.h
reply.o: ../include/globals.h ../include/dsname.h ../include/acl.h
reply.o: ../include/types.h
