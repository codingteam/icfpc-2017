#!/bin/sh

set -e

if test -z "$TARBALL_NAME"; then
  TARBALL_NAME=icfp-XXXXXXXX_add_our_code_here_XXXXXXXXX.tar.gz
fi
                        
BASE_DIR=`pwd`

build(){
	cd "$BASE_DIR/src-scala"
	sbt ';clean;deploy'
}

pack(){
	cd "$BASE_DIR/deploy"
	tar -c * | gzip -9 > "$BASE_DIR/$TARBALL_NAME"
}

build
pack
cd "$BASE_DIR"
echo "Tarball name: $TARBALL_NAME"
md5sum -b "$TARBALL_NAME"

