CFLAGS=	-g -I../include
LFLAGS=	-uhv -I../include -DKERBEROS -DCONNECT -DSUBPROC

SRCS=	tfile.c tunix.c tnet.c stubs.c dsname.c interface.c rpcall.c \
	conv_mgr.c host.c announce.c \
	res_module.c auth_krb.c dsc_enter.c auth_dum.c

OBJS=	tfile.o tunix.o tnet.o stubs.o dsname.o interface.o rpcall.o \
	conv_mgr.o host.o announce.o \
	../ets/usp_et.o ../ets/rpc_et.o dsc_enter.o

KOBJS=	res_module.k.o auth_krb.o ../ets/krb_et.o
NKOBJS=	res_module.nk.o auth_dum.o

all:	libds.a libdsk.a llib-lds.ln

libds.a:	$(OBJS) ${NKOBJS}
	ar cruv libds.a $(OBJS) ${NKOBJS}
	ranlib libds.a

libdsk.a:	$(OBJS) ${KOBJS}
	ar cruv libdsk.a $(OBJS) ${KOBJS}
	ranlib libdsk.a

libds_p.a:	${OBJS} ${NKOBJS}
	(cd profiled; ar cruv ../libds_p.a ${OBJS} ${NKOBJS} ; \
		ranlib ../libds_p.a)

libdsk_p.a:	${OBJS} ${KOBJS}
	(cd profiled; ar cruv ../libdsk_p.a ${OBJS} ${KOBJS} ; \
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
	-mkdir ../dist/libds
	-mkdir ../dist/libds/profiled
	cp -p $(SRCS) ../dist/libds/
	cp -p Makefile ../dist/libds/

.c.o:
	-rm -f $*.o profiled/$*.o
	${CC} -c -pg ${CFLAGS} $*.c
	mv -f $*.o profiled/$*.o
	${CC} -c ${CFLAGS} $*.c

clean:
	rm -rf *~ \#* *.o libds.a libdsk.a profiled/ llib-lds.ln

# 'make depend' code
depend: ${CFILES}
	${CC} -M ${CFLAGS} ${SRCS} | \
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
	ed - Makefile < eddep
	rm eddep makedep
#
# the last line in the makefile should be...
# DO NOT DELETE THIS LINE

tfile.o: tfile.c /usr/include/stdio.h /usr/include/errno.h ./../include/tfile.h
tfile.o: /usr/include/sys/types.h /usr/include/sys/stat.h
tunix.o: tunix.c /usr/include/stdio.h ./../include/tfile.h /usr/include/errno.h
tunix.o: /usr/include/sys/types.h /usr/include/sys/stat.h
tnet.o: tnet.c /usr/include/stdio.h ./../include/usp.h ./../include/usp_et.h
tnet.o: ./../include/tfile.h /usr/include/errno.h
stubs.o: stubs.c ./../include/interface.h ./../include/types.h
stubs.o: ./../include/rpc.h /usr/include/stdio.h ./../include/usp.h
stubs.o: ./../include/usp_et.h ./../include/rpc_et.h ./../include/tfile.h
stubs.o: ./../include/acl.h ./../include/types.h
dsname.o: dsname.c /usr/include/stdio.h /usr/include/strings.h
dsname.o: /usr/include/pwd.h /usr/include/sys/file.h /usr/include/sys/param.h
dsname.o: /usr/include/machine/machparam.h /usr/include/signal.h
dsname.o: /usr/include/sys/types.h /usr/include/errno.h ../include/config.h
dsname.o: ../include/dsname.h ../include/dsc_et.h
interface.o: interface.c /usr/include/stdio.h /usr/include/strings.h
interface.o: ../include/tfile.h ../include/interface.h ../include/types.h
interface.o: ../include/acl.h ../include/types.h /usr/include/errno.h
interface.o: ../include/dsname.h
rpcall.o: rpcall.c /usr/include/sys/types.h /usr/include/stdio.h
rpcall.o: /usr/include/ctype.h /usr/include/strings.h /usr/include/sys/socket.h
rpcall.o: /usr/include/netinet/in.h /usr/include/netdb.h ../include/tfile.h
rpcall.o: ../include/rpc.h /usr/include/stdio.h ../include/usp.h
rpcall.o: ../include/usp_et.h ../include/rpc_et.h ../include/config.h
conv_mgr.o: conv_mgr.c /usr/include/errno.h /usr/include/strings.h
conv_mgr.o: ../include/rpc.h /usr/include/stdio.h ../include/usp.h
conv_mgr.o: ../include/usp_et.h ../include/rpc_et.h
host.o: host.c /usr/include/strings.h /usr/include/netdb.h /usr/include/ctype.h
announce.o: announce.c /usr/include/stdio.h /usr/include/sys/file.h
announce.o: ../include/tfile.h ../include/interface.h ../include/types.h
announce.o: ../include/dsname.h ../include/dsc_et.h
res_module.o: res_module.c ../include/rpc_et.h ../include/config.h
res_module.o: /usr/include/netdb.h /usr/include/strings.h /usr/include/ctype.h
auth_krb.o: auth_krb.c /usr/include/strings.h /usr/include/ctype.h
auth_krb.o: /usr/include/krb.h ../include/des.h
