# Things you might want to put in ENV and LENV:
# -Dvoid=int		compilers that don't do void
# -DCHARBITS=0377	compilers that don't do unsigned char
# -DSTATIC=extern	compilers that don't like "static foo();" as forward decl
# -DSTRCSPN		library does not have strcspn()
# -Dstrchr=index	library does not have strchr()
# -DERRAVAIL		have utzoo-compatible error() function and friends

LENV= -I.
ENV= -I.

# Things you might want to put in PROF:
# -Dstatic='/* */'	make everything global so profiler can see it.
# -p			profiler
PROF=

CFLAGS= -O $(ENV) $(PROF)
LINTFLAGS= $(LENV)  -ha

SRCS=	regexp.c regsub.c
OBJS=	regexp.o regsub.o

all:	libregexp.a

libregexp.a: $(OBJS)
	ar cruv libregexp.a $(OBJS)
	ranlib libregexp.a

dist:   $(SRCS) Makefile
	-mkdir ../../dist/source/dsgrep/regexp
	-mkdir ../../dist/source/dsgrep/regexp/profiled
	cp -p $(SRCS) ../../dist/source/dsgrep/regexp/
	cp -p Makefile ../../dist/source/dsgrep/regexp/

.c.o:
	-rm -f $*.o
	${CC} -c ${CFLAGS} $*.c

clean:
	-rm -f *~ \#* *.o libregexp.a

# 'make depend' code
depend: ${SRCS}
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
