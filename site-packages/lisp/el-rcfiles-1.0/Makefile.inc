### Makefile.inc --- Generic toplevel include makefile for Emacs Lisp libraries

## Copyright (C) 2011-2013 Didier Verna

## Author: Didier Verna <didier@didierverna.net>

# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.


### Commentary:


### Code:

# Path to your version of (X)Emacs:
EMACS := emacs

# Prefix for standard GNU Emacs installation with `make install'.
# Installation will use the following subdirectories:
# PREFIX/share/emacs/site-lisp/
# PREFIX/info/
# PREFIX/etc/
PREFIX := /usr/local

# Prefix and directory for package-style installation with `make install-pkg'
# (see the INSTALL file for more information).
# Installation will use the following subdirectories:
# PKG_DIR/lisp/<project>/
# PKG_DIR/info/
# PKG_DIR/etc/<project>/
PKG_PREFIX := $(PREFIX)
# GNU Emacs:
PKG_DIR := $(PKG_PREFIX)/share/emacs/local-packages
# XEmacs 21.4:
# PKG_DIR := $(PKG_PREFIX)/lib/xemacs/site-packages
# XEmacs 21.5:
# PKG_DIR := $(PKG_PREFIX)/share/xemacs/site-packages

# Set to t if you want a symlink installation
SYMLINK :=

# Programs for generating the documentation:
MAKEINFO := makeinfo
TEXI2DVI := texi2dvi
DVIPS    := dvips


## Local Variables:
## mode: makefile
## End:

### Makefile.inc ends here
