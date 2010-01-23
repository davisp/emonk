dnl Licensed under the Apache License, Version 2.0 (the "License"); you may not
dnl use this file except in compliance with the License.  dnl You may obtain a
dnl copy of the License at
dnl
dnl   http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl Unless required by applicable law or agreed to in writing, software
dnl distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
dnl WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
dnl License for the specific language governing permissions and limitations
dnl under the License.

AC_INIT([LOCAL_PACKAGE_NAME], [LOCAL_VERSION], [], [LOCAL_PACKAGE_TARNAME])

AC_PREREQ([2.59])

AC_CONFIG_SRCDIR([CHANGES])
AC_CONFIG_AUX_DIR([build-aux])

AM_CONFIG_HEADER([config.h])

AM_INIT_AUTOMAKE([1.6.3 foreign])

AC_GNU_SOURCE
AC_ENABLE_SHARED
AC_DISABLE_STATIC

AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_LN_S

AC_MSG_CHECKING([for pthread_create in -lpthread])

original_LIBS="$LIBS"
LIBS="-lpthread $original_LIBS"

AC_TRY_LINK([#include<pthread.h>],
    [pthread_create((void *)0, (void *)0, (void *)0, (void *)0)],
    [pthread=yes], [pthread=no])

if test x${pthread} = xyes; then
    AC_MSG_RESULT([yes])
else
    LIBS="$original_LIBS"
    AC_MSG_RESULT([no])
fi

AC_ARG_WITH([erlang], [AC_HELP_STRING([--with-erlang=PATH],
    [set PATH to the Erlang include directory])], [
    ERLANG_FLAGS="-I$withval"
], [
    ERLANG_FLAGS="-I${libdir}/erlang/usr/include"
    ERLANG_FLAGS="$ERLANG_FLAGS -I/usr/lib/erlang/usr/include"
    ERLANG_FLAGS="$ERLANG_FLAGS -I/usr/local/lib/erlang/usr/include"
    ERLANG_FLAGS="$ERLANG_FLAGS -I/opt/local/lib/erlang/usr/include"
])

AC_ARG_WITH([js-include], [AC_HELP_STRING([--with-js-include=PATH],
    [set PATH to the SpiderMonkey include directory])], [
    JS_INCLUDE="$withval"
    JS_FLAGS="-I$JS_INCLUDE"
], [
    JS_FLAGS="-I/usr/include"
    JS_FLAGS="$JS_FLAGS -I/usr/include/js"
    JS_FLAGS="$JS_FLAGS -I/usr/include/mozjs"
    JS_FLAGS="$JS_FLAGS -I/usr/local/include"
    JS_FLAGS="$JS_FLAGS -I/opt/local/include"
    JS_FLAGS="$JS_FLAGS -I/usr/local/include/js"
    JS_FLAGS="$JS_FLAGS -I/opt/local/include/js"
])

AC_ARG_WITH([js-lib], [AC_HELP_STRING([--with-js-lib=PATH],
    [set PATH to the SpiderMonkey library directory])],
    [
    JS_LIB_DIR=$withval
    JS_LIB_FLAGS="-L$withval"
], [
    JS_LIB_DIR=
])
AC_SUBST(JS_LIB_DIR)

AC_ARG_VAR([ERLC_FLAGS], [general flags to prepend to ERLC_FLAGS])
AC_ARG_VAR([FLAGS], [general flags to prepend to LDFLAGS and CPPFLAGS])

LIB_FLAGS="$JS_LIB_FLAGS -L/usr/local/lib -L/opt/local/lib"
LIBS="$LIB_FLAGS $LIBS"

FLAGS="$LIB_FLAGS $ERLANG_FLAGS $JS_FLAGS -DXP_UNIX $FLAGS"
CPPFLAGS="$FLAGS $CPPFLAGS"
# manually linking libm is requred for FreeBSD 7.0
LDFLAGS="$FLAGS -lm $LDFLAGS"

AC_CHECK_LIB([mozjs], [JS_NewContext], [JS_LIB_BASE=mozjs], [
    AC_CHECK_LIB([js], [JS_NewContext], [JS_LIB_BASE=js], [
        AC_CHECK_LIB([js3250], [JS_NewContext], [JS_LIB_BASE=js3250], [
            AC_CHECK_LIB([js32], [JS_NewContext], [JS_LIB_BASE=js32], [
                AC_MSG_ERROR([Could not find the js library.

Is the Mozilla SpiderMonkey library installed?])])])])])

AC_SUBST(JS_LIB_BASE)
JSLIB=-l$JS_LIB_BASE

AC_CHECK_HEADER([jsapi.h], [], [
    AC_CHECK_HEADER([js/jsapi.h],
        [CPPFLAGS="$CPPFLAGS -I$JS_INCLUDE/js"],
        [
            AC_MSG_ERROR([Could not find the jsapi header.

Are the Mozilla SpiderMonkey headers installed?])
        ])])

AC_SUBST(JSLIB)

AC_LANG_PUSH(C)
OLD_CFLAGS="$CFLAGS"
CFLAGS="-Werror-implicit-function-declaration"
AC_COMPILE_IFELSE(
    [AC_LANG_PROGRAM(
        [[#include <jsapi.h>]],
        [[JS_SetOperationCallback(0, 0);]]
    )],
    AC_DEFINE([USE_JS_SETOPCB], [], [Use new JS_SetOperationCallback])
)
CFLAGS="$OLD_CFLAGS"
AC_LANG_POP(C)

case "$(uname -s)" in
  Linux)
    LIBS="$LIBS -lcrypt"
    CPPFLAGS="-D_XOPEN_SOURCE $CPPFLAGS"
    ;;
  FreeBSD)
    LIBS="$LIBS -lcrypt"
    ;;
  OpenBSD)
    LIBS="$LIBS -lcrypto"
  ;;
esac

AC_PATH_PROG([ERL], [erl])

if test x${ERL} = x; then
    AC_MSG_ERROR([Could not find the `erl' executable. Is Erlang installed?])
fi

erlang_version_error="The installed Erlang version is less than 5.6.5 (R12B05)."

version="`${ERL} -version 2>&1 | ${SED} "s/[[^0-9]]/ /g"`"

if test `echo $version | ${AWK} "{print \\$1}"` -lt 5; then
    AC_MSG_ERROR([$erlang_version_error])
fi

if test `echo $version | ${AWK} "{print \\$2}"` -lt 6; then
    AC_MSG_ERROR([$erlang_version_error])
fi

if test `echo $version | ${AWK} "{print \\$2}"` -eq 6; then
    if test `echo $version | ${AWK} "{print \\$3}"` -lt 5; then
        AC_MSG_ERROR([$erlang_version_error])
    fi
fi

AC_PATH_PROG([ERLC], [erlc])

if test x${ERLC} = x; then
    AC_MSG_ERROR([Could not find the `erlc' executable. Is Erlang installed?])
fi

AC_CHECK_HEADER([erl_driver.h], [], [
    AC_MSG_ERROR([Could not find the `erl_driver.h' header.

Are the Erlang headers installed? Use the `--with-erlang' option to specify the
path to the Erlang include directory.])])

AC_ARG_VAR([ERL], [path to the `erl' executable])
AC_ARG_VAR([ERLC], [path to the `erlc' executable])

AC_SUBST([package_author_name], ["LOCAL_PACKAGE_AUTHOR_NAME"])
AC_SUBST([package_author_address], ["LOCAL_PACKAGE_AUTHOR_ADDRESS"])
AC_SUBST([package_identifier], ["LOCAL_PACKAGE_IDENTIFIER"])
AC_SUBST([package_tarname], ["LOCAL_PACKAGE_TARNAME"])
AC_SUBST([package_name], ["LOCAL_PACKAGE_NAME"])

AC_SUBST([version], ["LOCAL_VERSION"])
AC_SUBST([version_major], ["LOCAL_VERSION_MAJOR"])
AC_SUBST([version_minor], ["LOCAL_VERSION_MINOR"])
AC_SUBST([version_revision], ["LOCAL_VERSION_REVISION"])
AC_SUBST([version_stage], ["LOCAL_VERSION_STAGE"])
AC_SUBST([version_release], ["LOCAL_VERSION_RELEASE"])

AC_SUBST([bug_uri], ["LOCAL_BUG_URI"])

AC_SUBST([localconfdir], [${sysconfdir}/${package_identifier}])
AC_SUBST([localdatadir], [${datadir}/${package_identifier}])
AC_SUBST([localdocdir], [${datadir}/doc/${package_identifier}])
AC_SUBST([locallibdir], [${libdir}/${package_identifier}])
AC_SUBST([localstatelibdir], [${localstatedir}/lib/${package_identifier}])
AC_SUBST([localstatelogdir], [${localstatedir}/log/${package_identifier}])
AC_SUBST([localstaterundir], [${localstatedir}/run/${package_identifier}])
AC_SUBST([locallibbindir], [${locallibdir}/bin])
AC_SUBST([localerlanglibdir], [${locallibdir}/erlang/lib])

# fix for older autotools that don't define "abs_top_YYY" by default
AC_SUBST(abs_top_srcdir)
AC_SUBST(abs_top_builddir)

AC_REVISION([LOCAL_VERSION])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([priv/Makefile])
AC_CONFIG_FILES([src/Makefile])
AC_CONFIG_FILES([src/emonk.app.tpl])

AC_OUTPUT

echo
echo "Run \`make && sudo make install' to install."
