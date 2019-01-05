.PHONY: all clean

include ./Rules.mk

ifneq ($(findstring xemacs,$(EMACS)),)
SPECIFIC = xemacs
else
SPECIFIC = emacs
endif

all:
	$(MAKE) -C lisp $(MFLAGS) all
	$(MAKE) -C misc $(MFLAGS) all

clean:
	find . -name "*.elc" -delete
	$(MAKE) -C lisp $(MFLAGS) clean
	$(MAKE) -C misc $(MFLAGS) clean
