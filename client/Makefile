LIBS=	libds.a libusp.a
CFLAGS=	-g

all:	testrpc pmtg dspipe mkds discuss rmds tags

discuss: discuss.o cmds.o discuss_utils.o pager.o trn_expr.o libss.a $(LIBS)
	cc $(CFLAGS) -o discuss discuss.o cmds.o discuss_utils.o pager.o libss.a $(LIBS)

tags:	TAGS

TAGS:	discuss.c discuss_utils.c pager.c
	-etags discuss.c discuss_utils.c pager.c

testrpc:  testrpc.o libds.a
	cc $(CFLAGS) -o testrpc testrpc.o $(LIBS)

pmtg:	pmtg.o libds.a
	cc $(CFLAGS) -o pmtg pmtg.o $(LIBS)

dspipe:	dspipe.o libds.a
	cc $(CFLAGS) -o dspipe dspipe.o $(LIBS)

mkds:	mkds.o libds.a
	cc $(CFLAGS) -o mkds mkds.o $(LIBS)

rmds:	mkds
	rm -f rmds
	ln mkds rmds

clean:
	rm -f *.o *~ \#* testrpc pmtg dspipe mkds discuss rmds

.SUFFIXES:	.ct

.ct.o:
	make_commands $*.ct
	rm -f $*.o
	cc -R -O -c $*.c
	rm -f a.out
	ld -r -n -s $*.o
	mv a.out $*.o
	rm $*.c

.y.o:	
	yacc $*.y
	mv y.tab.c $*.c
	cc $(CFLAGS) -c $*.c
