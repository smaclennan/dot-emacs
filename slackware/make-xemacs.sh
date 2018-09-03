#!/bin/sh

# Non-Slackware build script for XEmacs
# WARNING: does a make install, not a package!

PRGNAM=xemacs
VERSION=21.5.34

CWD=$(pwd)

if [ -z "$ARCH" ]; then
  case "$( uname -m )" in
    i?86) ARCH=i486 ;;
    arm*) ARCH=arm ;;
       *) ARCH=$( uname -m ) ;;
  esac
fi

if [ "$ARCH" = "i486" ]; then
    SLKCFLAGS="-O2 -march=i486 -mtune=i686"
    LIBDIRSUFFIX=""
elif [ "$ARCH" = "i686" ]; then
    SLKCFLAGS="-O2 -march=i686 -mtune=i686"
    LIBDIRSUFFIX=""
elif [ "$ARCH" = "x86_64" ]; then
    # XEmacs will not run compiled -fPIC
    SLKCFLAGS="-O2"
    LIBDIRSUFFIX="64"
fi

set -e

rm -rf $PRGNAM-$VERSION
tar xf $PRGNAM-$VERSION.tar.*
cd $PRGNAM-$VERSION

# The .elc files are built with mule. We must remove them for a non-mule build.
rm lisp/*.elc

# XEmacs is hardcoded to look in lib
if [ "$ARCH" = "x86_64" ]; then
  patch -p1 < $CWD/patches/lib64-beta.patch
fi

# Should work even with earlier compilers
patch -p1 < $CWD/patches/gcc5.3.0.patch

patch -p1 < $CWD/patches/select-warning.patch

# Thanks to Jerry James for letting me know about the
# --with-default-eol-detection option!

CFLAGS="$SLKCFLAGS" \
 ./configure \
 --prefix=/usr \
 --without-error-checking \
 --without-mule \
 --with-default-eol-detection \
 --with-xft \
 $ARCH-slackware-linux

make -j8
make install \
  prefix=$PKG/usr \
  mandir=$PKG/usr/man/man1 \
  libdir=$PKG/usr/lib${LIBDIRSUFFIX}

make datadir=$PKG/usr/share gzip-el
