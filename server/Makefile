OBJS=	disserve.o atom.o dispatch.o acl.o acl_core.o \
	../libds/tfile.o ../libds/tnet.o ../ets/dsc_et.o ../ets/rpc_et.o

SRCS=	disserve.c core.c coreutil.c atom.c dispatch.c acl.c rpproc.c \
	acl_core.c expunge.c recover.c glue.c

SERVERDIR= ..

KRBLIB=	-lkrb
DESLIB=	-ldes
# null these out if your site does not have Zephyr
ZEPHYRLIB=	-lzephyr
ZEPHYRDEF=	-DZEPHYR

INCLUDES=-I../include
DEBUG=-g -O
DEFS=-DUSPRPC $(ZEPHYRDEF)
CFLAGS=${DEBUG} ${INCLUDES} ${DEFS}
COMERR=../lib/libcom_err.a
LIBS=${ZEPHYRLIB} ../lib/libdsk.a ${KRBLIB} ${DESLIB} ../lib/libusp.a $(COMERR)

all:	disserve discussd debug recover expunge

disserve: $(OBJS) rpproc.nk.o core.o coreutil.o
	rm -f disserve
	${CC} $(CFLAGS) -o disserve $(OBJS) core.o coreutil.o rpproc.nk.o \
		$(LIBS)

servetest: ../client/discuss.o ../client/cmds.o ../client/libss.a \
		../client/discuss_utils.o core.o coreutil.o atom.o acl.o \
		../libds/tfile.o glue.o
	${CC} $(CFLAGS) -o servetest ../client/discuss.o ../client/cmds.o \
		../client/libss.a ../client/discuss_utils.o core.o coreutil.o \
		atom.o acl.o ../libds/tfile.o glue.o

recover: recover.o core.o coreutil.o atom.o acl.o ../libds/tunix.o \
		../libds/tfile.o glue.o ../ets/dsc_et.o $(COMERR)
	rm -f recover
	${CC} $(CFLAGS) -o recover recover.o core.o coreutil.o atom.o acl.o \
		glue.o $(LIBS)

expunge: expunge.o core.o coreutil.o atom.o acl.o acl_core.o \
		../libds/tunix.o ../libds/tfile.o glue.o ../ets/dsc_et.o \
		$(COMERR)
	rm -rf expunge
	${CC} $(CFLAGS) -o expunge expunge.o core.o coreutil.o atom.o acl.o \
		acl_core.o glue.o $(LIBS)

discussd: $(OBJS) rpproc.k.o core.o coreutil.o
	rm -f discussd
	${CC} $(CFLAGS) -o discussd $(OBJS) core.o coreutil.o rpproc.k.o \
		$(LIBS)

debug:	disdebug

disdebug: $(OBJS) rpproc.debug.o core.o coreutil.o
	rm -f disdebug
	${CC} $(CFLAGS) -o disdebug $(OBJS) core.o coreutil.o rpproc.debug.o \
		$(LIBS)

rpproc.k.o: rpproc.c
	rm -f rpproc.k.o rpproc.o
	${CC} $(CFLAGS) -DINETD -DKERBEROS -c rpproc.c
	mv rpproc.o rpproc.k.o

rpproc.nk.o: rpproc.c
	rm -f rpproc.o rpproc.nk.o
	${CC} $(CFLAGS) -DSUBPROC -c rpproc.c
	mv rpproc.o rpproc.nk.o

rpproc.debug.o: rpproc.c
	rm -f rpproc.debug.o rpproc.o
	${CC} $(CFLAGS) -DKERBEROS -c rpproc.c
	mv rpproc.o rpproc.debug.o

core.o: core.c
	rm -f core.o
	${CC} $(CFLAGS) -DKERBEROS -c core.c

coreutil.o: coreutil.c
	rm -f coreutil.o
	${CC} $(CFLAGS) -DKERBEROS -c coreutil.c

clean:
	rm -f *.o *~ \#* disserve recover expunge discussd disdebug

install: discussd disserve
	install -c discussd $(SERVERDIR)/discussd
	-/etc/chown discuss $(SERVERDIR)/discussd
	install -c -m 4755 disserve $(SERVERDIR)/disserve
	-/etc/chown discuss $(SERVERDIR)/disserve

dist:	$(SRCS) Makefile mtg.h
	-mkdir ../../dist/source/server
	-chmod g+w ../../dist/source/server
	cp -p $(SRCS) ../../dist/source/server/
	cp -p Makefile ../../dist/source/server/
	chmod 644 ../../dist/source/server/Makefile
	cp -p mtg.h ../../dist/source/server/

lint:	${SRCS}
	lint $(LFLAGS) $(SRCS)

.c.o:
	rm -rf $*.o
	${CC} $(CFLAGS) -c $*.c

# 'make depend' code
CFILES= ${SRCS}
COPTS=	${CFLAGS}
depend: 
	${CC} -M ${COPTS} ${CFILES} | \
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
	rm -f Makefile.bak
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep

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
# the last line in the makefile should be...
# DO NOT DELETE THIS LINE

disserve.o: disserve.c /usr/include/stdio.h /usr/include/syslog.h
core.o: core.c ../include/discuss/types.h ../include/discuss/dsc_et.h
core.o: ../include/discuss/interface.h ../include/discuss/types.h ./mtg.h
core.o: ../include/discuss/types.h ../include/discuss/tfile.h ../include/atom.h
core.o: ../include/discuss/acl.h ../include/internal.h /usr/include/errno.h
core.o: /usr/include/sys/file.h /usr/include/sys/types.h
core.o: /usr/include/sys/stat.h /usr/include/sys/param.h
core.o: /usr/include/machine/machparam.h /usr/include/signal.h
core.o: /usr/include/sys/types.h /usr/include/string.h /usr/include/strings.h
coreutil.o: coreutil.c ../include/discuss/types.h ../include/discuss/dsc_et.h
coreutil.o: ../include/atom.h ./mtg.h ../include/discuss/types.h
coreutil.o: ../include/discuss/tfile.h ../include/discuss/acl.h
coreutil.o: ../include/internal.h ../include/ansi.h
coreutil.o: /usr/include/zephyr/zephyr.h /usr/include/zephyr/mit-copyright.h
coreutil.o: /usr/include/zephyr/zephyr_err.h /usr/include/zephyr/zephyr_conf.h
coreutil.o: /usr/include/zephyr/mit-copyright.h /usr/include/errno.h
coreutil.o: /usr/include/sys/types.h /usr/include/netinet/in.h
coreutil.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/stdio.h
coreutil.o: /usr/include/errno.h /usr/include/sys/types.h /usr/include/netdb.h
coreutil.o: /usr/include/sys/file.h /usr/include/sys/stat.h
coreutil.o: /usr/include/strings.h
atom.o: atom.c ./../include/atom.h /usr/include/sys/types.h
atom.o: /usr/include/sys/stat.h /usr/include/sys/file.h
dispatch.o: dispatch.c ../include/rpc.h /usr/include/stdio.h ../include/usp.h
dispatch.o: ../include/usp_et.h ../include/rpc_et.h
dispatch.o: ../include/discuss/interface.h ../include/discuss/types.h
dispatch.o: ../include/discuss/tfile.h ../include/discuss/acl.h
dispatch.o: ../include/ansi.h
acl.o: acl.c /usr/include/stdio.h /usr/include/strings.h /usr/include/ctype.h
acl.o: ../include/discuss/discuss.h ../include/discuss/mit-sipb-copyright.h
acl.o: ../include/discuss/types.h ../include/discuss/tfile.h
acl.o: ../include/discuss/dsc_et.h ../include/discuss/acl.h
acl.o: ../include/discuss/interface.h ../include/discuss/types.h
acl.o: ../include/discuss/dsname.h ../include/internal.h
rpproc.o: rpproc.c /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
rpproc.o: /usr/include/sys/ttydev.h /usr/include/fcntl.h
rpproc.o: /usr/include/sys/types.h /usr/include/sys/socket.h
rpproc.o: /usr/include/stdio.h /usr/include/netinet/in.h
rpproc.o: /usr/include/sys/stat.h /usr/include/netdb.h /usr/include/errno.h
rpproc.o: /usr/include/pwd.h /usr/include/string.h /usr/include/strings.h
rpproc.o: ../include/discuss/tfile.h ../include/rpc.h /usr/include/stdio.h
rpproc.o: ../include/usp.h ../include/usp_et.h ../include/rpc_et.h
rpproc.o: ../include/discuss/types.h ./config.h
acl_core.o: acl_core.c ../include/discuss/types.h ../include/discuss/dsc_et.h
acl_core.o: ../include/discuss/acl.h ../include/internal.h
acl_core.o: /usr/include/sys/file.h /usr/include/errno.h /usr/include/stdio.h
acl_core.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
acl_core.o: /usr/include/signal.h /usr/include/sys/types.h
acl_core.o: /usr/include/string.h /usr/include/strings.h ../include/ansi.h
expunge.o: expunge.c /usr/include/stdio.h /usr/include/strings.h
expunge.o: /usr/include/sys/types.h /usr/include/sys/file.h
expunge.o: /usr/include/sys/stat.h ../include/discuss/types.h
expunge.o: ../include/discuss/dsc_et.h ../include/discuss/tfile.h
expunge.o: ../include/discuss/interface.h ../include/discuss/types.h
expunge.o: ../include/discuss/acl.h ./mtg.h ../include/discuss/types.h
recover.o: recover.c /usr/include/stdio.h /usr/include/strings.h
recover.o: /usr/include/sys/types.h /usr/include/sys/file.h
recover.o: /usr/include/sys/stat.h ../include/discuss/types.h
recover.o: ../include/discuss/dsc_et.h ../include/discuss/tfile.h
recover.o: ../include/discuss/interface.h ../include/discuss/types.h ./mtg.h
recover.o: ../include/discuss/types.h
glue.o: glue.c
