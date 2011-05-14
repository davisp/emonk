#!/bin/sh

CURL_BIN=`which curl`

SRC_DIR=`pwd`/c_src

NSPR_URI="http://rcouch.refuge.io/dl/libs/nsprpub-4.8.tar.gz"
NSPR_TARGET="${SRC_DIR}/nsprpub-4.8.tar.gz"

JS_URI="http://rcouch.refuge.io/dl/libs/js185-1.0.0.tar.gz"
JS_TARGET="${SRC_DIR}/js185-1.0.0.tar.gz"

if ! test -f $NSPR_TARGET; then
    echo "==> Fetch ${NSPR_TARGET}"
    $CURL_BIN $NSPR_URI -o $NSPR_TARGET
fi

if ! test -f $JS_TARGET; then
    echo "==> Fetch ${JS_TARGET}"
    $CURL_BIN $JS_URI -o $JS_TARGET
fi
