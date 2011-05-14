# This Makefile builds the dependencies (libjs and libnspr) needed by
# couchjs

uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
uname_M := $(shell sh -c 'uname -m 2>/dev/null || echo not')
uname_O := $(shell sh -c 'uname -o 2>/dev/null || echo not')
uname_R := $(shell sh -c 'uname -r 2>/dev/null || echo not')
uname_P := $(shell sh -c 'uname -p 2>/dev/null || echo not')
uname_V := $(shell sh -c 'uname -v 2>/dev/null || echo not')

CFLAGS = -g -O2 -Wall
LDFLAGS = -lstdc++
MAKE_ENV = -j 3

TAR	= tar
GUNZIP = gunzip
SMONKEY_VER	= 1.8.5
SMONKEY_PKGVER = 185-1.0.0
NSPR_VER = 4.8
NSPR_CONFIGURE_ENV =
ARCH =

SYSTEM_DIR = $(CURDIR)/system
LIB_DIR = $(SYSTEM_DIR)/lib
INC_DIR = $(SYSTEM_DIR)/include

JS_DIR = $(CURDIR)/js-$(SMONKEY_VER)
NSPR_DIR = $(CURDIR)/nsprpub

ifeq ($(uname_S),Linux)
	ARCH := $(shell sh -c 'arch 2>/dev/null || echo i386')
	ifeq ($(ARCH), x86_64)
		NSPR_CONFIGURE_ENV += --enable-64bit
	endif
endif

ifeq ($(uname_S), Darwin)

ARCH := $(shell sh -c 'getconf LONG_BIT 2>/dev/null || echo "32"')
	ifeq ($(ARCH), 64)
		NSPR_CONFIGURE_ENV += --enable-64bit
	endif
endif

ifeq ($(uname_S), FreeBSD)
	ARCH := $(shell sh -c 'uname -p 2>/dev/null || echo 32')
	ifeq ($(ARCH), amd64)
		NSPR_CONFIGURE_ENV += --enable-64bit
	endif
endif

ifeq ($(uname_S), OpenBSD)
	ARCH := $(shell sh -c 'uname -p 2>/dev/null || echo 32')
	ifeq ($(ARCH), x86_64)
		NSPR_CONFIGURE_ENV += --enable-64bit
	endif
endif

all: js

js: $(LIB_DIR)/libjs_static.a

$(LIB_DIR)/libjs_static.a: $(LIB_DIR)/libnspr4.a
	$(GUNZIP) -c js$(SMONKEY_PKGVER).tar.gz | $(TAR) xf -
	(cd $(JS_DIR)/js/src && \
	 CFLAGS="$(CFLAGS)" LDFLAGS="$(LDFLAGS)" \
	 CPPFLAGS="-DXP_UNIX -DJS_C_STRINGS_ARE_UTF8" \
	 ./configure --prefix=$(SYSTEM_DIR) \
				    --disable-debug \
					--enable-optimize \
					--enable-static \
					--disable-shared-js \
					--disable-tests \
					--with-system-nspr \
					--with-nspr-prefix=$(SYSTEM_DIR) && \
		$(MAKE) $(MAKE_ENV) all)
	@mkdir $(INC_DIR)/js
	@cp $(JS_DIR)/js/src/*.h $(INC_DIR)/js
	@cp $(JS_DIR)/js/src/*.tbl $(INC_DIR)/js
	@cp $(JS_DIR)/js/src/libjs_static.a $(LIB_DIR)
	@rm -rf $(JS_DIR)/js/src/dist

$(LIB_DIR)/libnspr4.a:
	$(GUNZIP) -c nsprpub-$(NSPR_VER).tar.gz | $(TAR) xf -
	(cd $(NSPR_DIR) && \
	 ./configure --disable-debug --enable-optimize \
                     --prefix=$(SYSTEM_DIR) $(NSPR_CONFIGURE_ENV) && \
         $(MAKE) $(MAKE_ENV) all ; $(MAKE) install)

clean:
	@rm -rf $(SYSTEM_DIR) $(NSPR_DIR) $(JS_DIR)
	@rm -rf *flymake*

.EXPORT_ALL_VARIABLES:
