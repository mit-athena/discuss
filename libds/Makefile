CFLAGS=	-g -I../include
LFLAGS=	-uhv -I../include -DKERBEROS -DCONNECT -DSUBPROC

SRCS=	tfile.c tunix.c tnet.c stubs.c dsname.c interface.c rpcall.c \
	conv_mgr.c host.c res_module.c auth_krb.c

OBJS=	tfile.o tunix.o tnet.o stubs.o dsname.o interface.o rpcall.o \
	conv_mgr.o host.o ../ets/usp_et.o ../ets/rpc_et.o

all:	libds.a libdsk.a llib-lds.ln

libds.a:	$(OBJS) res_module.nk.o
	ar cruv libds.a $(OBJS) res_module.nk.o
	ranlib libds.a

libdsk.a:	$(OBJS) res_module.k.o auth_krb.o ../ets/krb_et.o
	ar cruv libdsk.a $(OBJS) res_module.k.o auth_krb.o ../ets/krb_et.o
	ranlib libdsk.a

llib-lds.ln: $(SRCS)
	lint -Cds $(LFLAGS) $(SRCS) |\
		grep -v "warning: possible pointer alignment problem" |\
		grep -v "warning: null effect"

res_module.k.o: res_module.c
	rm -f res_module.k.o res_module.o
	cc $(CFLAGS) -DKERBEROS -c res_module.c
	mv res_module.o res_module.k.o

res_module.nk.o: res_module.c
	rm -f res_module.nk.o res_module.o
	cc $(CFLAGS) -c res_module.c
	mv res_module.o res_module.nk.o

install:

dist:	$(SRCS) Makefile
	-mkdir ../dist/libds
	cp -p $(SRCS) ../dist/libds/
	cp -p Makefile ../dist/libds/

.c.o:
	rm -rf $*.o
	cc $(CFLAGS) -c $*.c

clean:
	rm -rf *~ \#* *.o libds.a libdsk.a

# 'make depend' code
CFILES= ${SRCS}
COPTS=	${CFLAGS}
depend: ${CFILES}
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
dsname.o: dsname.c /usr/include/strings.h /usr/include/ctype.h
dsname.o: /usr/include/errno.h /usr/include/pwd.h /usr/include/sys/file.h
dsname.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
dsname.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/stdio.h
dsname.o: ./../include/dsname.h
interface.o: interface.c /usr/include/stdio.h ./../include/tfile.h
interface.o: ./../include/interface.h ./../include/types.h ./../include/acl.h
interface.o: ./../include/types.h
rpcall.o: rpcall.c /usr/include/sys/types.h /usr/include/stdio.h
rpcall.o: /usr/include/ctype.h /usr/include/strings.h /usr/include/sys/socket.h
rpcall.o: /usr/include/netinet/in.h /usr/include/netdb.h ./../include/tfile.h
rpcall.o: ./../include/rpc.h /usr/include/stdio.h ./../include/usp.h
rpcall.o: ./../include/usp_et.h ./../include/rpc_et.h ./../include/config.h
conv_mgr.o: conv_mgr.c ../include/rpc.h /usr/include/stdio.h ../include/usp.h
conv_mgr.o: ../include/usp_et.h ../include/rpc_et.h
host.o: host.c /usr/include/netdb.h /usr/include/ctype.h
res_module.o: res_module.c ../include/rpc_et.h ../include/config.h
res_module.o: /usr/include/netdb.h /usr/include/ctype.h
auth_krb.o: auth_krb.c /usr/include/ctype.h /usr/include/krb.h
auth_krb.o: /usr/include/des.h
