LIBS=	../lib/libdsk.a ../lib/libusp.a ../lib/libkrb.a ../lib/libdes.a ../lib/libcom_err.a
CFLAGS=	-g -I../include

SRCS=	pmtg.c dspipe.c mkds.c dlsm.c crmtgs.c disrc2meetings.sh dsc_setup.sh

PROGS=	pmtg dspipe mkds dlsm crmtgs

SCRIPTS= disrc2meetings dsc_setup

.c.o:	
	rm -f $*.o
	cc -c $(CFLAGS) $*.c

all:	$(PROGS)

clean:	
	rm -f $(PROGS) *.o *~ *.bak

install: all
	for prog in $(PROGS); do \
		install -c -s $$prog $(DESTDIR)/bin/ ; done
	for prog in $(SCRIPTS); do \
		install -c $$prog.sh $(DESTDIR)/bin/$$prog ; done

pmtg:	pmtg.o $(LIBS) ../ets/dsc_et.o
	rm -f pmtg
	cc $(CFLAGS) -o pmtg pmtg.o ../ets/dsc_et.o $(LIBS)

dlsm:	dlsm.o
	rm -f dlsm
	cc $(CFLAGS) -o dlsm dlsm.o

dspipe:	dspipe.o $(LIBS) ../ets/dsc_et.o
	rm -f dspipe
	cc $(CFLAGS) -o dspipe dspipe.o ../ets/dsc_et.o $(LIBS)

dsmail:	dsmail.o $(LIBS) ../ets/dsc_et.o
	rm -f dstmp
	cc $(CFLAGS) -o dstmp -g dsmail.o ../ets/dsc_et.o $(LIBS)

crmtgs:	crmtgs.o $(LIBS) ../ets/dsc_et.o
	rm -f crmtgs
	cc $(CFLAGS) -o crmtgs -g crmtgs.o ../ets/dsc_et.o $(LIBS)

mkds:	mkds.o ../ets/dsc_et.o ../client/edit.o $(LIBS)
	rm -f mkds
	cc $(CFLAGS) -o mkds mkds.o \
		../client/edit.o ../ets/dsc_et.o $(LIBS)

rmds:	mkds
	rm -f rmds
	ln mkds rmds

dist: $(SRCS) Makefile
	-mkdir ../dist/mclient
	cp -p $(SRCS) ../dist/mclient/
	cp -p Makefile ../dist/mclient/

# 'make depend' code
CFILES=	pmtg.c dspipe.c mkds.c dlsm.c crmtgs.c
depend: 
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

