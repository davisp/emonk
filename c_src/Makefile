# This BSD Makefile builds the dependencies (libjs and libnspr) needed by
# couchjs
OS!=	uname -s

CFLAGS=	-g -O2 -Wall
LDFLAGS=	-lstdc++
MAKE_ENV?=	-j3

TAR?=	tar
GUNZIP?=	gunzip
SMONKEY_VER=	1.8.5
SMONKEY_PKGVER=	185-1.0.0
NSPR_VER=	4.8
NSPR_CONFIGURE_ENV=
ARCH=
GNUMAKE?=	gmake

SYSTEM_DIR=	${.CURDIR}/system
LIB_DIR=	${SYSTEM_DIR}/lib
INC_DIR=	${SYSTEM_DIR}/include

JS_DIR=	${.CURDIR}/js-${SMONKEY_VER}
NSPR_DIR=	${.CURDIR}/nsprpub

# doubt there are some unix using pmake but...
.if (${OS} == Linux)
ARCH!=	arch
.endif

.if (${OS} == FreeBSD || ${OS} == OpenBSD || \
	${OS} == NetBSD || ${OS} == Darwin)
ARCH!=	uname -p
.endif

.if (${ARCH} == x86_64 || ${ARCH} == amd64)
NSPR_CONFIGURE_ENV+=	--enable-64bit
.endif

all: ${LIB_DIR}/libjs_static.a

${LIB_DIR}/libjs_static.a: ${LIB_DIR}/libnspr4.a
	${GUNZIP} -c js${SMONKEY_PKGVER}.tar.gz | ${TAR} xf -
	(cd ${JS_DIR}/js/src && \
	 patch -p0 -i ${.CURDIR}/patches/patch-configure && \
	 patch -p0 -i ${.CURDIR}/patches/patch-jsprf_cpp && \
	 CFLAGS="${CFLAGS}" LDFLAGS="${LDFLAGS}" \
	 CPPFLAGS="-DXP_UNIX -DJS_C_STRINGS_ARE_UTF8" \
	 ./configure --prefix=${SYSTEM_DIR} \
				    --disable-debug \
					--enable-optimize \
					--enable-static \
					--disable-shared-js \
					--disable-tests \
					--with-system-nspr \
					--with-nspr-prefix=${SYSTEM_DIR} && \
		${GNUMAKE} ${MAKE_ENV} all)
	mkdir $(INC_DIR)/js
	cp $(JS_DIR)/js/src/*.h $(INC_DIR)/js
	cp $(JS_DIR)/js/src/*.tbl $(INC_DIR)/js
	cp $(JS_DIR)/js/src/libjs_static.a $(LIB_DIR)
	rm -rf $(JS_DIR)/js/src/dist

${LIB_DIR}/libnspr4.a:
	${GUNZIP} -c nsprpub-${NSPR_VER}.tar.gz | ${TAR} xf -
	(cd ${NSPR_DIR} && \
	 ./configure --disable-debug --enable-optimize \
                     --prefix=${SYSTEM_DIR} ${NSPR_CONFIGURE_ENV} && \
         ${GNUMAKE} ${MAKE_ENV} all ; ${GNUMAKE} install)

clean:
	-rm -rf ${SYSTEM_DIR} ${NSPR_DIR} ${JS_DIR}
	-rm -rf *flymake*
