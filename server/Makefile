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

all:	disserve discussd debug recover expunge byteswap

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

byteswap: byteswap.o core.o coreutil.o atom.o acl.o acl_core.o \
		../libds/tunix.o ../libds/tfile.o glue.o ../ets/dsc_et.o \
		$(COMERR)
	rm -rf byteswap
	${CC} $(CFLAGS) -o byteswap byteswap.o core.o coreutil.o atom.o acl.o \
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

installclient: disserve
	install -c disserve $(DESTDIR)/disserve-fs


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
#
