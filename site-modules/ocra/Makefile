DESTDIR=

SHELL=/bin/sh
RM=rm -f
#CC=../../lib-src/ellcc
CC=ellcc
LD=$(CC) --mode=link
MKINIT=$(CC) --mode=init

# XEmacs sends the output to stderr
VERSION=$(shell xemacs -batch -vanilla -eval '(message "%d.%d.%d" emacs-major-version emacs-minor-version emacs-patch-level)' 2>&1)

ARCH=$(shell uname -m)
ifneq ($(ARCH),x86_64)
ARCH=i486
endif

# XEmacs 21.4.22
export ELLMAKEDOC=$(shell ls /usr/lib/xemacs-$(VERSION)/$(ARCH)-*/make-docfile)


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
	$(MKINIT) --mod-output=$@ \
	--mod-name=$(MODNAME) --mod-version=$(MODVER) \
	--mod-title=$(MODTITLE) $(SRCS)

