KRB=	-lkrb -ldes
LIBS=	../lib/libdsk.a ../lib/libusp.a ../lib/libcom_err.a
LIBS1=	../lib/libds.a ../lib/libusp.a ../lib/libcom_err.a
CFLAGS=	-g -O -I../include -I..

SRCS=	pmtg.c mkds.c crmtgs.c create_mtg_dir.c \
	disrc2meetings.sh dsc_setup.sh dspipe.c dlsm.c dsmail.c

PROGS=	pmtg mkds crmtgs rmds create_mtg_dir dsmail dspipe

SCRIPTS= dsc_setup

DESTDIR= /usr/local

.c.o:	
	rm -f $*.o
	${CC} -c $(CFLAGS) $*.c

all:	$(PROGS)

clean:	
	rm -f $(PROGS) *.o *~ *.bak

install: all
	for prog in $(PROGS); do \
		install -c -s $$prog $(DESTDIR)/$$prog ; done
	for prog in $(SCRIPTS); do \
		install -c $$prog.sh $(DESTDIR)/$$prog ; done

pmtg:	pmtg.o $(LIBS) ../ets/dsc_et.o
	rm -f pmtg
	${CC} $(CFLAGS) -o pmtg pmtg.o ../ets/dsc_et.o $(LIBS) $(KRB)

dlsm:	dlsm.o
	rm -f dlsm
	${CC} $(CFLAGS) -o dlsm dlsm.o

dspipe:	dspipe.o $(LIBS) ../ets/dsc_et.o
	rm -f dspipe
	${CC} $(CFLAGS) -o dspipe dspipe.o ../ets/dsc_et.o $(LIBS) $(KRB)

dsmail:	dsmail.o $(LIBS1) ../ets/dsc_et.o
	rm -f dstmp
	${CC} $(CFLAGS) -o dstmp -g dsmail.o ../ets/dsc_et.o $(LIBS1) $(KRB)
	mv dstmp dsmail

crmtgs:	crmtgs.o $(LIBS) ../ets/dsc_et.o
	rm -f crmtgs
	${CC} $(CFLAGS) -o crmtgs -g crmtgs.o ../ets/dsc_et.o $(LIBS) $(KRB)

mkds:	mkds.o ../ets/dsc_et.o ../client/edit.o $(LIBS)
	rm -f mkds
	${CC} $(CFLAGS) -o mkds mkds.o \
		../client/edit.o ../ets/dsc_et.o $(LIBS) $(KRB)

rmds:	mkds
	rm -f rmds
	ln mkds rmds

create_mtg_dir: create_mtg_dir.o
	${CC} $(CFLAGS) -o create_mtg_dir create_mtg_dir.o

dist: $(SRCS) Makefile
	-mkdir ../../dist/source/mclient
	cp -p $(SRCS) ../../dist/source/mclient/
	cp -p Makefile ../../dist/source/mclient/
	chmod 644 ../../dist/source/mclient/Makefile

# 'make depend' code
CFILES=	pmtg.c dspipe.c mkds.c dlsm.c crmtgs.c
depend: 
	${CC} -M ${CFLAGS} ${COPTS} ${CFILES} | \
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
# the last constant line in the makefile should be...
# DO NOT DELETE THIS LINE

