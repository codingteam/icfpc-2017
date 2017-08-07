#!/bin/sh

set -e

TARBALL_NAME=icfpc-XX_add_our_code_here_XX.tar.gz

BASE_DIR=`pwd`

build(){
	cd "$BASE_DIR/src-scala" && sbt ';clean;deploy'
}

pack(){
	cd "$BASE_DIR/deploy" && tar -c * | gzip -9 > "$BASE_DIR/$TARBALL_NAME"
}

build && pack && echo "Tarball name: $TARBALL_NAME"

