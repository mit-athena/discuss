LIBS= ../lib/libdsk.a ../ets/dsc_et.o ../lib/libusp.a \
	../lib/libcom_err.a libss.a

CFLAGS=	-g

OBJS=	discuss.o discuss_utils.o pager.o trn_expr.o list.o trn_select.o \
	status.o new_trans.o print.o \
	cmds.o ../ets/discuss_err.o

SRCS=	discuss.c discuss_utils.c pager.c trn_expr.c list.c trn_select.c \
	status.c new_trans.c print.c

.SUFFIXES:	.ct .h .et

.ct.o:	
	make_commands $*.ct
	cc -R -O -c $*.c
	rm -f $*.c

.et.o:	# how to say that this regenerates .h files too?
	../lib/compile_et $*.et
	cc -R -O -c $*.c
	rm -f $*.c

.et.h:
	../lib/compile_et $*.et
	cc -R -O -c $*.c
	rm -f $*.c

.y.o:	
	yacc $*.y
	mv y.tab.c $*.c
	cc $(CFLAGS) -c $*.c

.c.o:
	rm -f $*.o
	cc $(CFLAGS) -c $*.c

#

all:	discuss tags

discuss: $(OBJS) $(LIBS)
	cc $(CFLAGS) -o discuss $(OBJS) $(LIBS) -lkrb -ldes

tags:	TAGS

TAGS:	$(SRCS)
	-etags $(SRCS)

lint:	$(SRCS)
	lint $(SRCS)

clean:
	rm -f *.o *~ \#* discuss

./discuss_err.h: discuss_err.h

# 'make depend' code
CFILES=	$(SRCS)
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
	cp Makefile Makefile.bak
	ex - Makefile < eddep
	rm eddep makedep
#
# the last constant line in the makefile should be...
# DO NOT DELETE THIS LINE

discuss.o: discuss.c /usr/include/stdio.h /usr/include/sys/file.h
discuss.o: /usr/include/signal.h /usr/include/strings.h /usr/include/sys/wait.h
discuss.o: ./../include/ss.h ./../include/ss_err.h ./../include/tfile.h
discuss.o: ./../include/interface.h ./../include/types.h ./../include/config.h
discuss.o: ./../include/dsname.h ./globals.h
discuss_utils.o: discuss_utils.c /usr/include/stdio.h /usr/include/sys/file.h
discuss_utils.o: /usr/include/strings.h /usr/include/signal.h ./../include/ss.h
discuss_utils.o: ./../include/ss_err.h ./../include/tfile.h
discuss_utils.o: ./../include/interface.h ./../include/types.h
discuss_utils.o: ./../include/config.h ./../include/discuss_err.h
discuss_utils.o: /usr/include/sys/wait.h
pager.o: pager.c /usr/include/stdio.h
trn_expr.o: trn_expr.c /usr/include/ctype.h ./../include/interface.h
trn_expr.o: ./../include/types.h ./../include/discuss_err.h
list.o: list.c /usr/include/stdio.h /usr/include/strings.h ./../include/ss.h
list.o: ./../include/ss_err.h ./../include/tfile.h ./../include/interface.h
list.o: ./../include/types.h ./../include/config.h ./../include/dsc_et.h
list.o: ./globals.h
trn_select.o: trn_select.c /usr/include/stdio.h ./../include/interface.h
trn_select.o: ./../include/types.h ./../include/discuss_err.h ./globals.h
status.o: status.c /usr/include/stdio.h ./../include/ss.h ./../include/ss_err.h
status.o: ./../include/interface.h ./../include/types.h ./../include/config.h
status.o: ./globals.h
new_trans.o: new_trans.c /usr/include/stdio.h /usr/include/sys/file.h
new_trans.o: /usr/include/signal.h /usr/include/strings.h
new_trans.o: /usr/include/sys/wait.h ./../include/ss.h ./../include/ss_err.h
new_trans.o: ./../include/tfile.h ./../include/interface.h ./../include/types.h
new_trans.o: ./../include/config.h ./globals.h
print.o: print.c /usr/include/stdio.h /usr/include/sys/file.h
print.o: /usr/include/signal.h /usr/include/strings.h /usr/include/sys/wait.h
print.o: ./../include/ss.h ./../include/ss_err.h ./../include/tfile.h
print.o: ./../include/interface.h ./../include/types.h ./../include/config.h
print.o: ./../include/dsc_et.h ./globals.h
