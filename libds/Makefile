CFLAGS=	-g -O -I../include
LFLAGS=	-uhv -I../include -DKERBEROS -DCONNECT -DSUBPROC

SRCS=	tfile.c tunix.c tnet.c stubs.c dsname.c interface.c rpcall.c \
	conv_mgr.c host.c announce.c \
	res_module.c auth_krb.c dsc_enter.c auth_dum.c tmem.c

USPET=../ets/usp_et.o
RPCET=../ets/rpc_et.o
DSCET=../ets/dsc_et.o
ETS=$(USPET) $(RPCET)
OBJS=	tfile.o tunix.o tnet.o stubs.o dsname.o interface.o rpcall.o \
	conv_mgr.o host.o announce.o dsc_enter.o tmem.o
#	$(USPET) $(RPCET) $(DSCET)

KRBET=../ets/krb_et.o
KOBJS=	res_module.k.o auth_krb.o # ../ets/krb_et.o
NKOBJS=	res_module.nk.o auth_dum.o

all:	libds.a libdsk.a libds_p.a libdsk_p.a

libds.a:	$(OBJS) ${NKOBJS} $(USPET) $(ETS)
	ar cruv libds.a $(OBJS) ${NKOBJS} $(RPCET) $(USPET) $(DSCET)
	ranlib libds.a

libdsk.a:	$(OBJS) ${KOBJS} $(ETS) $(KRBET)
	ar cruv libdsk.a $(OBJS) ${KOBJS} $(RPCET) $(USPET) $(DSCET) $(KRBET)
	ranlib libdsk.a

libds_p.a:	${OBJS} ${NKOBJS} $(ETS)
	(cd profiled; \
	    ar cruv ../libds_p.a ${OBJS} ${NKOBJS} ../$(RPCET) ../$(USPET) ../$(DSCET) ; \
	    ranlib ../libds_p.a)

libdsk_p.a:	${OBJS} ${KOBJS} $(ETS) $(KRBET)
	(cd profiled; \
	    ar cruv ../libdsk_p.a ${OBJS} ${KOBJS} ../$(RPCET) ../$(USPET) ../$(KRBET) ../$(DSCET); \
	    ranlib ../libdsk_p.a)

lint:	llib-lds.ln

llib-lds.ln: $(SRCS)
	lint -Cds $(LFLAGS) $(SRCS)

# these should make profiled versions too
res_module.k.o: res_module.c
	rm -f res_module.k.o res_module.o profiled/res_module.k.o
	${CC} $(CFLAGS) -DKERBEROS -c -p res_module.c
	mv res_module.o profiled/res_module.k.o
	${CC} $(CFLAGS) -DKERBEROS -c res_module.c
	mv res_module.o res_module.k.o

res_module.nk.o: res_module.c
	rm -f res_module.nk.o res_module.o profiled/res_module.nk.o
	${CC} $(CFLAGS) -c -p res_module.c
	mv res_module.o profiled/res_module.nk.o
	${CC} ${CFLAGS} -c res_module.c
	mv res_module.o res_module.nk.o

install:

dist:	$(SRCS) Makefile
	-mkdir ../../dist/source/libds
	-mkdir ../../dist/source/libds/profiled
	cp -p $(SRCS) ../../dist/source/libds/
	cp -p Makefile ../../dist/source/libds/

.c.o:
	-rm -f $*.o profiled/$*.o
	${CC} -c -pg ${CFLAGS} $*.c
	mv -f $*.o profiled/$*.o
	${CC} -c ${CFLAGS} $*.c

clean:
	rm -rf *~ \#* *.o libds.a libdsk.a profiled/ llib-lds.ln

# 'make depend' code
depend: ${CFILES}
	${CC} -M ${CFLAGS} ${SRCS} > makedep
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
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
tfile.o: tfile.c
tfile.o: /usr/include/stdio.h
tfile.o: /usr/include/errno.h
tfile.o: ../include/discuss/tfile.h
tfile.o: /usr/include/sys/types.h
tfile.o: /usr/include/sys/stat.h
tunix.o: tunix.c
tunix.o: /usr/include/stdio.h
tunix.o: ../include/discuss/tfile.h
tunix.o: /usr/include/errno.h
tunix.o: /usr/include/sys/types.h
tunix.o: /usr/include/sys/stat.h
tnet.o: tnet.c
tnet.o: /usr/include/stdio.h
tnet.o: ../include/usp.h
tnet.o: ../include/usp_et.h
tnet.o: ../include/discuss/tfile.h
tnet.o: /usr/include/errno.h
stubs.o: stubs.c
stubs.o: ../include/discuss/interface.h
stubs.o: ../include/discuss/types.h
stubs.o: ../include/rpc.h
stubs.o: /usr/include/stdio.h
stubs.o: ../include/usp.h
stubs.o: ../include/usp_et.h
stubs.o: ../include/rpc_et.h
stubs.o: ../include/discuss/tfile.h
stubs.o: ../include/discuss/acl.h
dsname.o: dsname.c
dsname.o: /usr/include/stdio.h
dsname.o: /usr/include/strings.h
dsname.o: /usr/include/pwd.h
dsname.o: /usr/include/sys/file.h
dsname.o: /usr/include/sys/param.h
dsname.o: /usr/include/machine/machparam.h
dsname.o: /usr/include/signal.h
dsname.o: /usr/include/sys/types.h
dsname.o: /usr/include/errno.h
dsname.o: /usr/include/assert.h
dsname.o: ../include/discuss/dsname.h
dsname.o: ../include/discuss/dsc_et.h
dsname.o: ../include/ansi.h
interface.o: interface.c
interface.o: /usr/include/stdio.h
interface.o: /usr/include/string.h
interface.o: /usr/include/strings.h
interface.o: /usr/include/errno.h
interface.o: ../include/discuss/tfile.h
interface.o: ../include/discuss/interface.h
interface.o: ../include/discuss/types.h
interface.o: ../include/discuss/acl.h
interface.o: ../include/discuss/dsname.h
interface.o: ../include/rpc.h
interface.o: /usr/include/stdio.h
interface.o: ../include/usp.h
interface.o: ../include/usp_et.h
interface.o: ../include/rpc_et.h
interface.o: ../include/discuss/dsc_et.h
rpcall.o: rpcall.c
rpcall.o: /usr/include/sys/types.h
rpcall.o: /usr/include/stdio.h
rpcall.o: /usr/include/ctype.h
rpcall.o: /usr/include/strings.h
rpcall.o: /usr/include/sys/socket.h
rpcall.o: /usr/include/netinet/in.h
rpcall.o: /usr/include/netdb.h
rpcall.o: ../include/discuss/tfile.h
rpcall.o: ../include/rpc.h
rpcall.o: /usr/include/stdio.h
rpcall.o: ../include/usp.h
rpcall.o: ../include/usp_et.h
rpcall.o: ../include/rpc_et.h
rpcall.o: ../include/config.h
conv_mgr.o: conv_mgr.c
conv_mgr.o: /usr/include/errno.h
conv_mgr.o: /usr/include/string.h
conv_mgr.o: /usr/include/strings.h
conv_mgr.o: ../include/rpc.h
conv_mgr.o: /usr/include/stdio.h
conv_mgr.o: ../include/usp.h
conv_mgr.o: ../include/usp_et.h
conv_mgr.o: ../include/rpc_et.h
host.o: host.c
host.o: /usr/include/strings.h
host.o: /usr/include/netdb.h
host.o: /usr/include/ctype.h
announce.o: announce.c
announce.o: /usr/include/stdio.h
announce.o: /usr/include/sys/file.h
announce.o: ../include/discuss/tfile.h
announce.o: ../include/discuss/interface.h
announce.o: ../include/discuss/types.h
announce.o: ../include/discuss/dsname.h
announce.o: ../include/discuss/dsc_et.h
res_module.o: res_module.c
res_module.o: ../include/rpc_et.h
res_module.o: ../include/config.h
res_module.o: /usr/include/netdb.h
res_module.o: /usr/include/string.h
res_module.o: /usr/include/strings.h
res_module.o: /usr/include/ctype.h
auth_krb.o: auth_krb.c
auth_krb.o: /usr/include/strings.h
auth_krb.o: /usr/include/ctype.h
auth_krb.o: /usr/include/krb.h
auth_krb.o: ../include/mit-copyright.h
auth_krb.o: /usr/include/des.h
auth_krb.o: ../include/mit-copyright.h
auth_krb.o: /usr/include/des_conf.h
auth_krb.o: ../include/mit-copyright.h
dsc_enter.o: dsc_enter.c
dsc_enter.o: /usr/include/stdio.h
dsc_enter.o: /usr/include/strings.h
dsc_enter.o: /usr/include/ctype.h
dsc_enter.o: /usr/include/sys/file.h
dsc_enter.o: ../include/discuss/tfile.h
dsc_enter.o: ../include/discuss/types.h
auth_dum.o: auth_dum.c
tmem.o: tmem.c
tmem.o: /usr/include/errno.h
tmem.o: /usr/include/sys/types.h
tmem.o: /usr/include/sys/uio.h
tmem.o: ../include/discuss/tfile.h
