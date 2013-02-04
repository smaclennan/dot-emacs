# Copyright (C) 1998, 1999 William Perry.

# This file is part of XEmacs.

# XEmacs is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.

# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.


#
# This is slightly more complicated than would normally be the case,
# as this makefile has been tailored to work in the Emacs source tree.
# For samples of how to compile modules outside of the source tree
# (as would be the case if a user had downloaded a module and wanted
# to compile it for use within Emacs), see the samples in the sub-directory
# 'installed'.
#

DESTDIR=

SHELL=/bin/sh
RM=rm -f
#CC=../../lib-src/ellcc
CC=ellcc
LD=$(CC) --mode=link
MKINIT=$(CC) --mode=init

# XEmacs 21.4.22 x86
CFLAGS=-I/usr/lib/xemacs-21.4.22/i486-slackware-linux/include
MAKEDOC=/usr/lib/xemacs-21.4.22/i486-slackware-linux/make-docfile


SRCS=ocra.c
OBJS=$(SRCS:.c=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<

MODNAME=ocra
MODVER=1.0.0
MODTITLE="OCRA"

all: $(MODNAME).ell

distclean: clean

clean:
	$(RM) $(MODNAME).ell $(OBJS) ocra_i.o ocra_i.c

$(MODNAME).ell: $(OBJS) ocra_i.o
	$(LD) --mod-output=$@ $(OBJS) ocra_i.o -lcrypto

ocra_i.o: ocra_i.c
ocra_i.c: $(SRCS)
	ELLMAKEDOC=$(MAKEDOC) $(MKINIT) --mod-output=$@ \
	--mod-name=$(MODNAME) --mod-version=$(MODVER) \
	--mod-title=$(MODTITLE) $(SRCS)

