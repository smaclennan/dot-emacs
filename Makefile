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

#all: subdirs init.elc $(patsubst %.el,%.elc,$(wildcard work.el))
all: subdirs

subdirs:
ifeq ($(EMACS),emacs)
	make EMACS=$(EMACS) -C esp
endif
	make EMACS=$(EMACS) -C site-packages/lisp

clean:
	@echo Clean $(EMACS) ...
	rm -f *.elc
	make -C site-packages/lisp EMACS=$(EMACS) clean
	make -C esp EMACS=$(EMACS) clean

clean-sam:
	make -C site-packages/lisp/sam clean
