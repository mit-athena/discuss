# libzephyr.a needed only for Zephyr notifications
# libkrb.a,libdes.a needed for Kerberos

LIBKRB=	../lib/libkrb.a ../lib/libdes.a

LIBS=	../server/acl.o ../lib/libdsk.a $(LIBKRB) \
	../lib/libusp.a ../ss/libss.a ../lib/libcom_err.a

INCLUDES= -I../include
CFLAGS=	-g -O ${INCLUDES}
LFLAGS=	-uhv ${INCLUDES}

OBJS=	discuss.o edit.o output.o trn_expr.o list.o trn_select.o \
	status.o new_trans.o print.o lsm.o addmtg.o list_acl.o \
	interrupt.o goto.o reply.o ckm.o anm.o \
	time.o data.o \
	cmds.o ../ets/discuss_err.o rn.o randrp.o catchup.o set.o
#	trn_spec.o nls.o

SRCS=	discuss.c edit.c output.c trn_expr.c list.c trn_select.c \
	status.c new_trans.c print.c lsm.c addmtg.c list_acl.c \
	interrupt.c goto.c reply.c ckm.c anm.c \
	time.c data.c rn.c randrp.c catchup.c set.c
#	trn_spec.c nls.c trn_spec.h

# defaults for options that should come from above.
DESTDIR= /usr/local

INFODIR= /usr/lib/discuss/info

.SUFFIXES:	.ct .h

.ct.c:	
	rm -f $*.o $*.c
	../ss/mk_cmds $*.ct

.y.o:	
	-rm -f $*.o y.tab.c y.tab.o
	$(YACC) $(YFLAGS) $<
	${CC} $(CFLAGS) -c y.tab.c
	mv y.tab.o $@

.c.o:
	-rm -f $*.o
	${CC} $(CFLAGS) -c $*.c

#

all:	discuss

discuss: $(OBJS) $(LIBS)
	-rm -f discuss
	${CC} $(CFLAGS) -o discuss $(OBJS) $(LIBS)

cmds.o : cmds.c
cmds.c : cmds.ct

tags:	TAGS

TAGS:	$(SRCS)
	-etags $(SRCS)

lint:	$(SRCS) ../server/acl.c ../libds/llib-lds.ln ../lib/llib-lcom_err.ln \
			llib-lss.ln
	lint $(LFLAGS) $(SRCS) ../lib/llib-lcom_err.ln llib-lss.ln \
			../server/acl.c ../libds/llib-lds.ln |\
		grep -v " unused in function " |\
		grep -v " set but not used in function " |\
		grep -v "possible pointer alignment "

install: all
	install -c discuss $(DESTDIR)/discuss
	rm -f info.tar
	rm -rf $(INFODIR)
	mkdir $(INFODIR)
	cd info; tar cf ../info.tar .
	mv -f info.tar $(INFODIR)/info.tar
	cd $(INFODIR); tar xf info.tar
	rm -f $(INFODIR)/info.tar

clean:
	rm -f *.o *~ \#* *.bak
	rm -f discuss trn_expr.c y.tab.c TAGS info.tar

dist:	$(SRCS) Makefile trn_expr.y cmds.ct
	-mkdir ../../dist/source/client
	cp -p $(SRCS) ../../dist/source/client/
	cp -p Makefile ../../dist/source/client/
	chmod 644 ../../dist/source/client/Makefile
	cp -p trn_expr.y cmds.ct ../../dist/source/client/
	rm -f info.tar
	tar cf info.tar info
	(cd ../../dist/source/client ; tar xf ../../../source/client/info.tar )
	ln -s ../include/ss.h ../../dist/source/client/ss.h
	ln -s ../include/ss_err.h ../../dist/source/client/ss_err.h
	ln -s ../ss/make_commands ../../dist/source/client/make_commands
	ln -s ../lib/libss.a ../../dist/source/client/libss.a

# 'make depend' code
depend: ${SRCS}
	${CC} -M ${CFLAGS} ${SRCS} > makedep
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	rm -f Makefile.bak
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm -f eddep makedep

undepend:
	rm -f Makefile.bak
	cp Makefile Makefile.bak
	echo '/^# DO NOT DELETE THIS LINE/+1,$$c' >eddep
	echo '#' >>eddep
	echo '.' >>eddep
	echo 'w' >>eddep
	ed - Makefile < eddep
	rm -f eddep makedep
	
#
# the last constant line in the makefile should be...
# DO NOT DELETE THIS LINE
discuss.o: discuss.c
discuss.o: /usr/include/stdio.h
discuss.o: /usr/include/sys/file.h
discuss.o: /usr/include/signal.h
discuss.o: /usr/include/string.h
discuss.o: /usr/include/strings.h
discuss.o: /usr/include/sys/wait.h
discuss.o: /usr/include/sys/signal.h
discuss.o: /usr/include/pwd.h
discuss.o: ../include/ss.h
discuss.o: ../include/ss/mit-sipb-copyright.h
discuss.o: ../include/ss/ss_err.h
discuss.o: ../include/discuss/discuss.h
discuss.o: ../include/discuss/mit-sipb-copyright.h
discuss.o: ../include/discuss/types.h
discuss.o: ../include/discuss/tfile.h
discuss.o: ../include/discuss/dsc_et.h
discuss.o: ../include/discuss/acl.h
discuss.o: ../include/discuss/interface.h
discuss.o: ../include/discuss/types.h
discuss.o: ../include/discuss/dsname.h
discuss.o: ../include/config.h
discuss.o: ../include/globals.h
discuss.o: ../include/discuss/dsname.h
discuss.o: ../include/discuss_err.h
edit.o: edit.c
edit.o: /usr/include/stdio.h
edit.o: /usr/include/sys/file.h
edit.o: /usr/include/string.h
edit.o: /usr/include/strings.h
edit.o: /usr/include/signal.h
edit.o: ../include/ss.h
edit.o: ../include/ss/mit-sipb-copyright.h
edit.o: ../include/ss/ss_err.h
edit.o: ../include/discuss/discuss.h
edit.o: ../include/discuss/mit-sipb-copyright.h
edit.o: ../include/discuss/types.h
edit.o: ../include/discuss/tfile.h
edit.o: ../include/discuss/dsc_et.h
edit.o: ../include/discuss/acl.h
edit.o: ../include/discuss/interface.h
edit.o: ../include/discuss/types.h
edit.o: ../include/discuss/dsname.h
edit.o: ../include/globals.h
edit.o: ../include/discuss/dsname.h
edit.o: ../include/discuss_err.h
edit.o: /usr/include/sys/wait.h
edit.o: /usr/include/sys/types.h
edit.o: /usr/include/sys/stat.h
edit.o: /usr/include/sys/errno.h
output.o: output.c
output.o: /usr/include/stdio.h
output.o: /usr/include/sys/file.h
output.o: /usr/include/string.h
output.o: /usr/include/strings.h
output.o: /usr/include/signal.h
output.o: ../include/ss.h
output.o: ../include/ss/mit-sipb-copyright.h
output.o: ../include/ss/ss_err.h
output.o: ../include/discuss/discuss.h
output.o: ../include/discuss/mit-sipb-copyright.h
output.o: ../include/discuss/types.h
output.o: ../include/discuss/tfile.h
output.o: ../include/discuss/dsc_et.h
output.o: ../include/discuss/acl.h
output.o: ../include/discuss/interface.h
output.o: ../include/discuss/types.h
output.o: ../include/discuss/dsname.h
output.o: ../include/config.h
output.o: ../include/globals.h
output.o: ../include/discuss/dsname.h
output.o: ../include/discuss_err.h
trn_expr.o: trn_expr.c
trn_expr.o: /usr/include/ctype.h
trn_expr.o: ../include/discuss/discuss.h
trn_expr.o: ../include/discuss/mit-sipb-copyright.h
trn_expr.o: ../include/discuss/types.h
trn_expr.o: ../include/discuss/tfile.h
trn_expr.o: ../include/discuss/dsc_et.h
trn_expr.o: ../include/discuss/acl.h
trn_expr.o: ../include/discuss/interface.h
trn_expr.o: ../include/discuss/types.h
trn_expr.o: ../include/discuss/dsname.h
trn_expr.o: ../include/globals.h
trn_expr.o: ../include/discuss/dsname.h
trn_expr.o: ../include/discuss_err.h
list.o: list.c
list.o: /usr/include/stdio.h
list.o: /usr/include/string.h
list.o: /usr/include/strings.h
list.o: /usr/include/sys/param.h
list.o: /usr/include/machine/machparam.h
list.o: /usr/include/signal.h
list.o: /usr/include/sys/types.h
list.o: ../include/ss/ss.h
list.o: ../include/ss/mit-sipb-copyright.h
list.o: ../include/ss/ss_err.h
list.o: ../include/config.h
list.o: ../include/discuss/discuss.h
list.o: ../include/discuss/mit-sipb-copyright.h
list.o: ../include/discuss/types.h
list.o: ../include/discuss/tfile.h
list.o: ../include/discuss/dsc_et.h
list.o: ../include/discuss/acl.h
list.o: ../include/discuss/interface.h
list.o: ../include/discuss/types.h
list.o: ../include/discuss/dsname.h
list.o: ../include/globals.h
list.o: ../include/discuss/dsname.h
list.o: ../include/discuss_err.h
trn_select.o: trn_select.c
trn_select.o: /usr/include/stdio.h
trn_select.o: ../include/discuss/discuss.h
trn_select.o: ../include/discuss/mit-sipb-copyright.h
trn_select.o: ../include/discuss/types.h
trn_select.o: ../include/discuss/tfile.h
trn_select.o: ../include/discuss/dsc_et.h
trn_select.o: ../include/discuss/acl.h
trn_select.o: ../include/discuss/interface.h
trn_select.o: ../include/discuss/types.h
trn_select.o: ../include/discuss/dsname.h
trn_select.o: ../include/globals.h
trn_select.o: ../include/discuss/dsname.h
trn_select.o: ../include/discuss_err.h
status.o: status.c
status.o: /usr/include/stdio.h
status.o: /usr/include/ctype.h
status.o: ../include/ss.h
status.o: ../include/ss/mit-sipb-copyright.h
status.o: ../include/ss/ss_err.h
status.o: ../include/discuss/discuss.h
status.o: ../include/discuss/mit-sipb-copyright.h
status.o: ../include/discuss/types.h
status.o: ../include/discuss/tfile.h
status.o: ../include/discuss/dsc_et.h
status.o: ../include/discuss/acl.h
status.o: ../include/discuss/interface.h
status.o: ../include/discuss/types.h
status.o: ../include/discuss/dsname.h
status.o: ../include/config.h
status.o: ../include/globals.h
status.o: ../include/discuss/dsname.h
status.o: ../include/discuss_err.h
status.o: /usr/include/string.h
status.o: /usr/include/strings.h
new_trans.o: new_trans.c
new_trans.o: /usr/include/stdio.h
new_trans.o: /usr/include/sys/file.h
new_trans.o: /usr/include/signal.h
new_trans.o: /usr/include/string.h
new_trans.o: /usr/include/strings.h
new_trans.o: /usr/include/sys/wait.h
new_trans.o: ../include/ss.h
new_trans.o: ../include/ss/mit-sipb-copyright.h
new_trans.o: ../include/ss/ss_err.h
new_trans.o: ../include/discuss/discuss.h
new_trans.o: ../include/discuss/mit-sipb-copyright.h
new_trans.o: ../include/discuss/types.h
new_trans.o: ../include/discuss/tfile.h
new_trans.o: ../include/discuss/dsc_et.h
new_trans.o: ../include/discuss/acl.h
new_trans.o: ../include/discuss/interface.h
new_trans.o: ../include/discuss/types.h
new_trans.o: ../include/discuss/dsname.h
new_trans.o: ../include/config.h
new_trans.o: ../include/globals.h
new_trans.o: ../include/discuss/dsname.h
new_trans.o: ../include/discuss_err.h
print.o: print.c
print.o: /usr/include/stdio.h
print.o: /usr/include/errno.h
print.o: /usr/include/sys/file.h
print.o: /usr/include/signal.h
print.o: /usr/include/string.h
print.o: /usr/include/strings.h
print.o: /usr/include/sys/wait.h
print.o: ../include/discuss/discuss.h
print.o: ../include/discuss/mit-sipb-copyright.h
print.o: ../include/discuss/types.h
print.o: ../include/discuss/tfile.h
print.o: ../include/discuss/dsc_et.h
print.o: ../include/discuss/acl.h
print.o: ../include/discuss/interface.h
print.o: ../include/discuss/types.h
print.o: ../include/discuss/dsname.h
print.o: ../include/ss.h
print.o: ../include/ss/mit-sipb-copyright.h
print.o: ../include/ss/ss_err.h
print.o: ../include/config.h
print.o: ../include/globals.h
print.o: ../include/discuss/dsname.h
print.o: ../include/discuss_err.h
lsm.o: lsm.c
lsm.o: /usr/include/string.h
lsm.o: /usr/include/strings.h
lsm.o: /usr/include/stdio.h
lsm.o: ../include/discuss/discuss.h
lsm.o: ../include/discuss/mit-sipb-copyright.h
lsm.o: ../include/discuss/types.h
lsm.o: ../include/discuss/tfile.h
lsm.o: ../include/discuss/dsc_et.h
lsm.o: ../include/discuss/acl.h
lsm.o: ../include/discuss/interface.h
lsm.o: ../include/discuss/types.h
lsm.o: ../include/discuss/dsname.h
lsm.o: ../include/globals.h
lsm.o: ../include/discuss/dsname.h
lsm.o: ../include/discuss_err.h
lsm.o: /usr/include/errno.h
lsm.o: ../include/ss.h
lsm.o: ../include/ss/mit-sipb-copyright.h
lsm.o: ../include/ss/ss_err.h
addmtg.o: addmtg.c
addmtg.o: /usr/include/string.h
addmtg.o: /usr/include/strings.h
addmtg.o: /usr/include/stdio.h
addmtg.o: /usr/include/netdb.h
addmtg.o: /usr/include/sys/file.h
addmtg.o: ../include/discuss/discuss.h
addmtg.o: ../include/discuss/mit-sipb-copyright.h
addmtg.o: ../include/discuss/types.h
addmtg.o: ../include/discuss/tfile.h
addmtg.o: ../include/discuss/dsc_et.h
addmtg.o: ../include/discuss/acl.h
addmtg.o: ../include/discuss/interface.h
addmtg.o: ../include/discuss/types.h
addmtg.o: ../include/discuss/dsname.h
addmtg.o: ../include/globals.h
addmtg.o: ../include/discuss/dsname.h
addmtg.o: ../include/discuss_err.h
addmtg.o: ../include/ss.h
addmtg.o: ../include/ss/mit-sipb-copyright.h
addmtg.o: ../include/ss/ss_err.h
list_acl.o: list_acl.c
list_acl.o: /usr/include/string.h
list_acl.o: /usr/include/strings.h
list_acl.o: /usr/include/stdio.h
list_acl.o: ../include/discuss/discuss.h
list_acl.o: ../include/discuss/mit-sipb-copyright.h
list_acl.o: ../include/discuss/types.h
list_acl.o: ../include/discuss/tfile.h
list_acl.o: ../include/discuss/dsc_et.h
list_acl.o: ../include/discuss/acl.h
list_acl.o: ../include/discuss/interface.h
list_acl.o: ../include/discuss/types.h
list_acl.o: ../include/discuss/dsname.h
list_acl.o: ../include/globals.h
list_acl.o: ../include/discuss/dsname.h
list_acl.o: ../include/discuss_err.h
interrupt.o: interrupt.c
interrupt.o: /usr/include/signal.h
goto.o: goto.c
goto.o: /usr/include/stdio.h
goto.o: /usr/include/sys/file.h
goto.o: /usr/include/signal.h
goto.o: /usr/include/string.h
goto.o: /usr/include/strings.h
goto.o: /usr/include/sys/wait.h
goto.o: /usr/include/ctype.h
goto.o: ../include/ss.h
goto.o: ../include/ss/mit-sipb-copyright.h
goto.o: ../include/ss/ss_err.h
goto.o: ../include/discuss/discuss.h
goto.o: ../include/discuss/mit-sipb-copyright.h
goto.o: ../include/discuss/types.h
goto.o: ../include/discuss/tfile.h
goto.o: ../include/discuss/dsc_et.h
goto.o: ../include/discuss/acl.h
goto.o: ../include/discuss/interface.h
goto.o: ../include/discuss/types.h
goto.o: ../include/discuss/dsname.h
goto.o: ../include/globals.h
goto.o: ../include/discuss/dsname.h
goto.o: ../include/discuss_err.h
reply.o: reply.c
reply.o: /usr/include/stdio.h
reply.o: /usr/include/sys/file.h
reply.o: /usr/include/strings.h
reply.o: /usr/include/sys/wait.h
reply.o: ../include/ss.h
reply.o: ../include/ss/mit-sipb-copyright.h
reply.o: ../include/ss/ss_err.h
reply.o: ../include/discuss/discuss.h
reply.o: ../include/discuss/mit-sipb-copyright.h
reply.o: ../include/discuss/types.h
reply.o: ../include/discuss/tfile.h
reply.o: ../include/discuss/dsc_et.h
reply.o: ../include/discuss/acl.h
reply.o: ../include/discuss/interface.h
reply.o: ../include/discuss/types.h
reply.o: ../include/discuss/dsname.h
reply.o: ../include/config.h
reply.o: ../include/globals.h
reply.o: ../include/discuss/dsname.h
reply.o: ../include/discuss_err.h
ckm.o: ckm.c
ckm.o: /usr/include/string.h
ckm.o: /usr/include/strings.h
ckm.o: /usr/include/stdio.h
ckm.o: ../include/discuss/discuss.h
ckm.o: ../include/discuss/mit-sipb-copyright.h
ckm.o: ../include/discuss/types.h
ckm.o: ../include/discuss/tfile.h
ckm.o: ../include/discuss/dsc_et.h
ckm.o: ../include/discuss/acl.h
ckm.o: ../include/discuss/interface.h
ckm.o: ../include/discuss/types.h
ckm.o: ../include/discuss/dsname.h
ckm.o: ../include/globals.h
ckm.o: ../include/discuss/dsname.h
ckm.o: ../include/discuss_err.h
anm.o: anm.c
anm.o: /usr/include/stdio.h
anm.o: /usr/include/sys/file.h
anm.o: ../include/ss.h
anm.o: ../include/ss/mit-sipb-copyright.h
anm.o: ../include/ss/ss_err.h
anm.o: ../include/discuss/discuss.h
anm.o: ../include/discuss/mit-sipb-copyright.h
anm.o: ../include/discuss/types.h
anm.o: ../include/discuss/tfile.h
anm.o: ../include/discuss/dsc_et.h
anm.o: ../include/discuss/acl.h
anm.o: ../include/discuss/interface.h
anm.o: ../include/discuss/types.h
anm.o: ../include/discuss/dsname.h
anm.o: ../include/config.h
anm.o: ../include/globals.h
anm.o: ../include/discuss/dsname.h
anm.o: ../include/discuss_err.h
time.o: time.c
time.o: /usr/include/stdio.h
time.o: /usr/include/time.h
data.o: data.c
data.o: /usr/include/stdio.h
data.o: ../include/discuss/discuss.h
data.o: ../include/discuss/mit-sipb-copyright.h
data.o: ../include/discuss/types.h
data.o: ../include/discuss/tfile.h
data.o: ../include/discuss/dsc_et.h
data.o: ../include/discuss/acl.h
data.o: ../include/discuss/interface.h
data.o: ../include/discuss/types.h
data.o: ../include/discuss/dsname.h
data.o: ../include/ss.h
data.o: ../include/ss/mit-sipb-copyright.h
data.o: ../include/ss/ss_err.h
data.o: ../include/globals.h
data.o: ../include/discuss/dsname.h
data.o: ../include/discuss_err.h
rn.o: rn.c
rn.o: ../include/discuss/discuss.h
rn.o: ../include/discuss/mit-sipb-copyright.h
rn.o: ../include/discuss/types.h
rn.o: ../include/discuss/tfile.h
rn.o: ../include/discuss/dsc_et.h
rn.o: ../include/discuss/acl.h
rn.o: ../include/discuss/interface.h
rn.o: ../include/discuss/types.h
rn.o: ../include/discuss/dsname.h
rn.o: ../include/globals.h
rn.o: ../include/discuss/dsname.h
rn.o: ../include/discuss_err.h
rn.o: /usr/include/stdio.h
rn.o: /usr/include/sys/types.h
rn.o: /usr/include/sys/file.h
rn.o: /usr/include/sys/ioctl.h
rn.o: /usr/include/sys/ttychars.h
rn.o: /usr/include/sys/ttydev.h
randrp.o: randrp.c
randrp.o: ../include/discuss/discuss.h
randrp.o: ../include/discuss/mit-sipb-copyright.h
randrp.o: ../include/discuss/types.h
randrp.o: ../include/discuss/tfile.h
randrp.o: ../include/discuss/dsc_et.h
randrp.o: ../include/discuss/acl.h
randrp.o: ../include/discuss/interface.h
randrp.o: ../include/discuss/types.h
randrp.o: ../include/discuss/dsname.h
randrp.o: ../include/globals.h
randrp.o: ../include/discuss/dsname.h
randrp.o: ../include/discuss_err.h
randrp.o: /usr/include/stdio.h
randrp.o: /usr/include/sys/time.h
randrp.o: /usr/include/time.h
catchup.o: catchup.c
catchup.o: /usr/include/stdio.h
catchup.o: /usr/include/string.h
catchup.o: /usr/include/strings.h
catchup.o: ../include/discuss/discuss.h
catchup.o: ../include/discuss/mit-sipb-copyright.h
catchup.o: ../include/discuss/types.h
catchup.o: ../include/discuss/tfile.h
catchup.o: ../include/discuss/dsc_et.h
catchup.o: ../include/discuss/acl.h
catchup.o: ../include/discuss/interface.h
catchup.o: ../include/discuss/types.h
catchup.o: ../include/discuss/dsname.h
catchup.o: ../include/ss.h
catchup.o: ../include/ss/mit-sipb-copyright.h
catchup.o: ../include/ss/ss_err.h
catchup.o: ../include/config.h
catchup.o: ../include/globals.h
catchup.o: ../include/discuss/dsname.h
catchup.o: ../include/discuss_err.h
set.o: set.c
set.o: /usr/include/stdio.h
set.o: /usr/include/string.h
set.o: /usr/include/strings.h
