AC_INIT
AC_CONFIG_SRCDIR([client/discuss.c])

AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_PROG_MKDIR_P

lispdir='${datarootdir}/emacs/site-lisp'
AC_SUBST(lispdir)

dnl Checks for libraries.
ATHENA_KRB4
ATHENA_KRB5
ATHENA_REGEXP
ATHENA_ZEPHYR

PKG_CHECK_MODULES([SS], [ss])
AC_MSG_NOTICE([Adding ss CFLAGS ${SS_CFLAGS}])
CFLAGS="${SS_CFLAGS} ${CFLAGS}"
AC_MSG_NOTICE([Adding ss LDFLAGS ${SS_LIBS}])
LDFLAGS="${SS_LDFLAGS} ${LDFLAGS}"

PKG_CHECK_MODULES([COM_ERR], [com_err])
AC_MSG_NOTICE([Adding com_err CFLAGS ${COM_ERR_CFLAGS}])
CFLAGS="${COM_ERR_CFLAGS} ${CFLAGS}"
AC_MSG_NOTICE([Adding com_err LDFLAGS ${COM_ERR_LIBS}])
LDFLAGS="${COM_ERR_LDFLAGS} ${LDFLAGS}"

AC_SEARCH_LIBS([gethostbyname], [nsl])
AC_SEARCH_LIBS([socket], [socket])

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([fcntl.h unistd.h termios.h])

dnl Checks for library functions.
AC_CHECK_FUNCS([srand48 sigaction])

AC_CONFIG_FILES([Makefile client/Makefile dsgrep/Makefile edsc/Makefile ets/Makefile
                 include/Makefile libds/Makefile mclient/Makefile server/Makefile
                 usp/Makefile])
AC_OUTPUT
