.PHONY: all subdirs clean

ifeq ($(EMACS),)
BASE=$(shell basename $(PWD))
ifeq ($(BASE),.sxemacs)
EMACS := sxemacs
else ifeq ($(BASE),.xemacs)
EMACS := xemacs
else
EMACS := emacs
endif
endif

.el.elc:
	@echo Build $(EMACS) $< ...
	@$(EMACS) -batch -l init.el -f batch-byte-compile $<

all: subdirs init.elc $(patsubst %.el,%.elc,$(wildcard work.el))

subdirs:
ifeq ($(EMACS),emacs)
	make EMACS=$(EMACS) -C esp
endif
	make EMACS=$(EMACS) -C site-packages/lisp

clean:
	rm -f *.elc
	make -C site-packages/lisp clean
	make -C esp clean

clean-sam:
	make -C site-packages/lisp/sam clean
